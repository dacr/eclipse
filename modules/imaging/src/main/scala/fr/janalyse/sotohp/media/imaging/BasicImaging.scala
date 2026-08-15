package fr.janalyse.sotohp.media.imaging

// -----------------------------------------------------------------------------
// Local copy of sotohp's `modules/imaging` code (https://github.com/dacr/sotohp).
// The package and every pre-existing signature are kept unchanged so that the
// additions made here can be pushed back upstream without any breaking change.
// Additions are marked with an `ADDED` comment.
// -----------------------------------------------------------------------------

import java.awt.RenderingHints
import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import javax.imageio.{IIOImage, ImageIO, ImageWriteParam}
import scala.jdk.CollectionConverters.*
import scala.math.*
import scala.sys.process.*
import scala.util.Using

case object BasicImaging {

  def fileTypeFromName(filename: String): Option[String] =
    filename.lastIndexOf(".") match {
      case -1 => None
      case i  => Some(filename.substring(i + 1).toLowerCase).filterNot(_.isEmpty)
    }

  def fileTypeFromName(path: Path): Option[String] =
    fileTypeFromName(path.getFileName.toString)

  def resize(
    originalImage: BufferedImage,
    targetWidth: Int,
    targetHeight: Int
  ): BufferedImage = {
    val ratio     = min(1d * targetWidth / originalImage.getWidth, 1d * targetHeight / originalImage.getHeight)
    val newWidth  = floor(originalImage.getWidth * ratio).toInt
    val newHeight = floor(originalImage.getHeight * ratio).toInt
    val newImage  = new BufferedImage(newWidth, newHeight, BufferedImage.TYPE_INT_RGB)
    val graphics  = newImage.createGraphics
    try {
      graphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
      graphics.drawImage(originalImage, 0, 0, newWidth, newHeight, null)
      newImage
    } finally {
      graphics.dispose()
    }
  }

  def rotate(
    originalImage: BufferedImage,
    angleDegree: Double
  ): BufferedImage = {
    if (angleDegree == 0d) originalImage
    else {
      val angle = toRadians(angleDegree)
      val sin = abs(Math.sin(angle))
      val cos = abs(Math.cos(angle))
      val width = originalImage.getWidth.toDouble
      val height = originalImage.getHeight.toDouble
      val newWidth = floor(width * cos + height * sin)
      val newHeight = floor(height * cos + width * sin)
      val newImage = BufferedImage(newWidth.toInt, newHeight.toInt, BufferedImage.TYPE_INT_RGB)
      val graphics = newImage.createGraphics
      try {
        graphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
        graphics.translate((newWidth - width) / 2d, (newHeight - height) / 2d)
        graphics.rotate(angle, width / 2d, height / 2d)
        graphics.drawImage(originalImage, 0, 0, null)
        // graphics.setColor(Color.RED)
        // graphics.drawRect(0, 0, newWidth - 1, newHeight - 1)
        newImage
      } finally {
        graphics.dispose()
      }
    }
  }

  def mirror(originalImage: BufferedImage, horizontally: Boolean = true, vertically: Boolean = false): BufferedImage = {
    val width    = originalImage.getWidth
    val height   = originalImage.getHeight
    val newImage = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val graphics = newImage.createGraphics
    try {
      graphics.drawImage(
        originalImage,
        if (horizontally) width else 0,
        if (vertically) height else 0,
        if (horizontally) -width else width,
        if (vertically) -height else height,
        null
      )
      newImage
    } finally {
      graphics.dispose()
    }
  }

  def display(image: BufferedImage, title: String = "Image display"): javax.swing.JFrame = {
    import java.awt.FlowLayout
    import javax.swing.{ImageIcon, JFrame, JLabel, WindowConstants}

    val icon  = ImageIcon(image)
    val frame = JFrame()
    frame.setLayout(FlowLayout())
    frame.setSize(image.getWidth + 50, image.getHeight + 50)
    val label = JLabel()
    label.setIcon(icon)
    frame.add(label)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame
  }

  def load(input: Path): BufferedImage = {
    val fileType = fileTypeFromName(input).getOrElse("").toLowerCase
    if (fileType == "heif" || fileType == "heic") {
      loadWithImageMagick(input)
    } else if (PortablePixmap.extensions.contains(fileType)) {
      // ADDED : netpbm, the native output of the usual RAW converters, read without any library
      PortablePixmap.read(input)
    } else {
      val image = ImageIO.read(input.toFile)
      if (image == null) throw new RuntimeException(s"Unsupported input image format : $input") // TODO enhance error support
      image
    }
  }

  private def loadWithImageMagick(input: Path): BufferedImage = {
    val tempFile = Files.createTempFile("sotohp-heif-", ".jpg")
    try {
      val exitCode = Process(Seq("magick", input.toString, tempFile.toString)).!
      if (exitCode != 0) {
        throw new RuntimeException(s"Failed to convert HEIF image with ImageMagick : $input")
      }
      val image = ImageIO.read(tempFile.toFile)
      if (image == null) throw new RuntimeException(s"Failed to read converted HEIF image : $input")
      image
    } finally {
      Files.deleteIfExists(tempFile)
    }
  }

  def save(output: Path, image: BufferedImage, compressionLevel: Option[Double] = None): Unit = {
    val foundImageType   = fileTypeFromName(output)
    val foundImageWriter =
      foundImageType
        .flatMap(imageType => ImageIO.getImageWritersByFormatName(imageType).asScala.toList.headOption)

    foundImageWriter match {
      case Some(writer) =>
        val params       = writer.getDefaultWriteParam
        // ADDED : `canWriteCompressed` guard, writers such as the PNG one throw
        // an UnsupportedOperationException when given a compression quality.
        if (compressionLevel.isDefined && params.canWriteCompressed) {
          params.setCompressionMode(ImageWriteParam.MODE_EXPLICIT)
          params.setCompressionQuality(compressionLevel.get.toFloat)
        }
        val outputStream = ImageIO.createImageOutputStream(output.toFile)
        try { // Not using `Using` because of an error propagation issue
          writer.setOutput(outputStream)
          val outputImage = IIOImage(image, null, null)
          writer.write(null, outputImage, params)
          writer.dispose()
        } catch {
          case error: Exception =>
            output.toFile.delete()
            throw new RuntimeException(s"Error while saving face image to $output", error)
        } finally {
          outputStream.close()
        }

      case None => throw RuntimeException(s"Unsupported output image format : $output") // TODO enhance error support
    }
  }

  def reshapeImage(
    input: Path,
    output: Path,
    targetMaxSize: Int,
    rotateDegrees: Option[Double] = None,
    compressionLevel: Option[Double] = None
  ): (width: Int, height: Int) = {
    val originalImage = load(input)
    val ratio         = targetMaxSize.toDouble / math.max(originalImage.getWidth, originalImage.getHeight)
    val targetWidth   = (originalImage.getWidth() * ratio).toInt
    val targetHeight  = (originalImage.getHeight() * ratio).toInt
    val resizedImage  = resize(originalImage, targetWidth, targetHeight)
    val finalImage    =
      if (rotateDegrees.exists(_ != 0d))
        rotate(resizedImage, rotateDegrees.get)
      else resizedImage
    save(output, finalImage, compressionLevel)
    (width = finalImage.getWidth, height = finalImage.getHeight)
  }

  // ADDED : creates an empty image with the same color model kind as the given one.
  def emptyLike(originalImage: BufferedImage, width: Int, height: Int): BufferedImage = {
    val imageType =
      if (originalImage.getColorModel.hasAlpha) BufferedImage.TYPE_INT_ARGB
      else BufferedImage.TYPE_INT_RGB
    BufferedImage(width, height, imageType)
  }

  // ADDED : converts an image to the given BufferedImage type (TYPE_INT_RGB, TYPE_INT_ARGB, ...).
  def convertTo(originalImage: BufferedImage, imageType: Int): BufferedImage = {
    if (originalImage.getType == imageType) originalImage
    else {
      val converted = BufferedImage(originalImage.getWidth, originalImage.getHeight, imageType)
      val graphics  = converted.createGraphics
      try {
        graphics.drawImage(originalImage, 0, 0, null)
        converted
      } finally {
        graphics.dispose()
      }
    }
  }

  // ADDED : crops a region, the requested region may exceed the image bounds,
  // in that case the missing parts are left transparent/black.
  def crop(originalImage: BufferedImage, x: Int, y: Int, width: Int, height: Int): BufferedImage = {
    val cropped  = emptyLike(originalImage, width, height)
    val graphics = cropped.createGraphics
    try {
      graphics.drawImage(originalImage, -x, -y, null)
      cropped
    } finally {
      graphics.dispose()
    }
  }

  // ADDED : crops a square region centered on a sub-pixel accurate position, the
  // content is translated with a bicubic interpolation so that the given center
  // lands exactly at the center of the returned image. This is what makes stacking
  // or aligning several shots of a same subject possible without any jitter.
  def cropCenteredOn(
    originalImage: BufferedImage,
    centerX: Double,
    centerY: Double,
    size: Int
  ): BufferedImage = {
    val cropped  = emptyLike(originalImage, size, size)
    val graphics = cropped.createGraphics
    try {
      graphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
      graphics.translate(size / 2d - centerX, size / 2d - centerY)
      graphics.drawImage(originalImage, 0, 0, null)
      cropped
    } finally {
      graphics.dispose()
    }
  }

  // ADDED : scales an image by the given ratio, keeping its aspect ratio.
  def scaleBy(originalImage: BufferedImage, ratio: Double): BufferedImage = {
    val newWidth  = max(1, round(originalImage.getWidth * ratio).toInt)
    val newHeight = max(1, round(originalImage.getHeight * ratio).toInt)
    resize(originalImage, newWidth, newHeight)
  }

  // ADDED : downscales an image so that its largest side doesn't exceed the given
  // size, returns the image unchanged when it is already small enough.
  def fitWithin(originalImage: BufferedImage, maxSize: Int): BufferedImage = {
    val currentMaxSize = max(originalImage.getWidth, originalImage.getHeight)
    if (currentMaxSize <= maxSize) originalImage
    else scaleBy(originalImage, maxSize.toDouble / currentMaxSize)
  }

}
