package fr.janalyse.sotohp.media.imaging

import fr.janalyse.sotohp.media.imaging.CircleFitting.Circle

import java.awt.Color
import java.awt.image.BufferedImage

/** Blending of several images onto a single canvas.
  *
  * Everything needed to build a composite : extract a normalized tile around a detected subject,
  * give it a soft circular edge so that no square seam shows up, then blend it at the right place.
  */
object Compositing {

  enum BlendMode {

    /** Standard alpha blending, the last drawn tile wins */
    case Over

    /** Keeps the brightest of the two, ideal on a black sky : overlapping coronas add up nicely and
      * the background stays black whatever the tile edges look like
      */
    case Lighten

    /** Additive, gives a glow where tiles overlap */
    case Add

    /** Inverse multiply, softer than `Add`, never clips */
    case Screen
  }

  /** Square tile extracted around a subject, with its own geometry */
  final case class Tile(image: BufferedImage, discRadiusPixels: Double) {
    def size: Int          = image.getWidth
    def halfSize: Double   = image.getWidth / 2d
    def radiusRatio: Double = halfSize / discRadiusPixels
  }

  /** Extracts a square tile centered on the detected subject and rescales it so that the subject
    * always has the very same radius in pixels, whatever the frame it comes from.
    *
    * This is the step which cancels both the drift of the subject through the frame and any
    * re-framing done during the session.
    *
    * @param radiusFactor how much room is kept around the subject, 1.0 stops at the limb, 3.0 or
    *                     more is needed to keep the corona of a totality frame.
    */
  def extractTile(
    image: BufferedImage,
    disc: Circle,
    targetDiscRadiusPixels: Double,
    radiusFactor: Double = 1.6d
  ): Tile = {
    val sourceHalfSize = disc.radius * radiusFactor
    val sourceSize     = math.max(2, math.ceil(sourceHalfSize * 2d).toInt)
    val cropped        = BasicImaging.cropCenteredOn(image, disc.centerX, disc.centerY, sourceSize)
    val ratio          = targetDiscRadiusPixels / disc.radius
    val scaled         = if (math.abs(ratio - 1d) < 1e-3) cropped else BasicImaging.scaleBy(cropped, ratio)
    Tile(scaled, targetDiscRadiusPixels)
  }

  /** Radial alpha mask, fully opaque up to `innerRadius` then fading out to `outerRadius` */
  def radialMask(size: Int, innerRadius: Double, outerRadius: Double): Array[Float] = {
    val mask   = Array.ofDim[Float](size * size)
    val center = size / 2d
    val span   = math.max(1e-6d, outerRadius - innerRadius)
    var y      = 0
    while (y < size) {
      var x = 0
      while (x < size) {
        val distance = math.hypot(x + 0.5d - center, y + 0.5d - center)
        val alpha    =
          if (distance <= innerRadius) 1d
          else if (distance >= outerRadius) 0d
          else {
            val ratio = (distance - innerRadius) / span
            0.5d * (1d + math.cos(math.Pi * ratio)) // smooth cosine fade
          }
        mask(y * size + x) = alpha.toFloat
        x += 1
      }
      y += 1
    }
    mask
  }

  /** Drawing surface, plain 8 bits RGB : a composite can be huge (tens of thousands of pixels wide)
    * so an int backed image is the reasonable trade-off, tiles being already normalized before
    * being blended.
    */
  final class Canvas(val image: BufferedImage) {
    def width: Int  = image.getWidth
    def height: Int = image.getHeight

    /** Blends a tile at the given position, `x` and `y` being the top left corner of the tile */
    def draw(
      tile: BufferedImage,
      x: Int,
      y: Int,
      mode: BlendMode = BlendMode.Lighten,
      mask: Option[Array[Float]] = None,
      opacity: Double = 1d
    ): Unit = {
      val tileWidth  = tile.getWidth
      val tileHeight = tile.getHeight
      val sourceRow  = Array.ofDim[Int](tileWidth)
      val targetRow  = Array.ofDim[Int](tileWidth)
      var tileY      = math.max(0, -y)
      val lastTileY  = math.min(tileHeight, height - y)
      val firstX     = math.max(0, -x)
      val lastX      = math.min(tileWidth, width - x)
      val spanWidth  = lastX - firstX
      if (spanWidth > 0) {
        while (tileY < lastTileY) {
          tile.getRGB(firstX, tileY, spanWidth, 1, sourceRow, 0, spanWidth)
          image.getRGB(x + firstX, y + tileY, spanWidth, 1, targetRow, 0, spanWidth)
          var index = 0
          while (index < spanWidth) {
            val alpha = opacity * mask.map(values => values((tileY * tileWidth + firstX + index)).toDouble).getOrElse(1d)
            if (alpha > 0d) {
              targetRow(index) = blendPixel(sourceRow(index), targetRow(index), mode, alpha)
            }
            index += 1
          }
          image.setRGB(x + firstX, y + tileY, spanWidth, 1, targetRow, 0, spanWidth)
          tileY += 1
        }
      }
    }

    /** Fills a disc with a solid color, its edge faded over the last fraction of the radius.
      *
      * Blending keeps the brighter of two pixels, which is exactly what makes the square edge of a
      * tile vanish on a black sky - and exactly what makes a black subject vanish on anything else.
      * A body which emits nothing has to be *put* black rather than left black, as soon as there is
      * something behind it.
      *
      * @param featherRatio width of the fade at the edge, as a fraction of the radius
      */
    def fillDisc(centerX: Double, centerY: Double, radius: Double, color: Color, featherRatio: Double = 0.03d): Unit = {
      val packed   = color.getRGB
      val inner    = radius * (1d - math.max(0d, math.min(1d, featherRatio)))
      val span     = math.max(1e-6d, radius - inner)
      val firstX   = math.max(0, math.floor(centerX - radius).toInt)
      val lastX    = math.min(width - 1, math.ceil(centerX + radius).toInt)
      val firstY   = math.max(0, math.floor(centerY - radius).toInt)
      val lastY    = math.min(height - 1, math.ceil(centerY + radius).toInt)
      val rowWidth = lastX - firstX + 1
      if (rowWidth > 0) {
        val row = Array.ofDim[Int](rowWidth)
        var y   = firstY
        while (y <= lastY) {
          image.getRGB(firstX, y, rowWidth, 1, row, 0, rowWidth)
          var index = 0
          while (index < rowWidth) {
            val distance = math.hypot(firstX + index + 0.5d - centerX, y + 0.5d - centerY)
            if (distance < radius) {
              val alpha = if (distance <= inner) 1d else 1d - (distance - inner) / span
              row(index) = blendPixel(packed, row(index), BlendMode.Over, alpha)
            }
            index += 1
          }
          image.setRGB(firstX, y, rowWidth, 1, row, 0, rowWidth)
          y += 1
        }
      }
    }

    /** Redraws a whole image over the canvas through a projective map.
      *
      * The map goes the other way round, from canvas pixel to source pixel : every canvas pixel asks
      * where it comes from and helps itself, which is what leaves no hole whatever the map does to
      * the geometry. Canvas pixels whose source falls outside the image are left as they are.
      *
      * A projective map is exactly what relates two rectilinear views of the same distant scene, so
      * this is all that is needed to redraw a wide angle photograph in the geometry of another one -
      * with no trigonometry in the loop, nine multiplications per pixel.
      *
      * @param matrix 3x3 row major, taking canvas coordinates into source coordinates
      * @param brightness applied to the source pixels on the way, to push a background back
      */
    def paintProjective(source: BufferedImage, matrix: Array[Double], brightness: Double = 1d): Unit = {
      val sourceWidth  = source.getWidth
      val sourceHeight = source.getHeight
      val pixels       = Array.ofDim[Int](sourceWidth * sourceHeight)
      source.getRGB(0, 0, sourceWidth, sourceHeight, pixels, 0, sourceWidth)

      java.util.stream.IntStream
        .range(0, height)
        .parallel()
        .forEach { y =>
          val row     = Array.ofDim[Int](width)
          var changed = false
          image.getRGB(0, y, width, 1, row, 0, width)
          var x = 0
          while (x < width) {
            val depth = matrix(6) * x + matrix(7) * y + matrix(8)
            if (depth > 1e-12d) {
              val sourceX = (matrix(0) * x + matrix(1) * y + matrix(2)) / depth
              val sourceY = (matrix(3) * x + matrix(4) * y + matrix(5)) / depth
              if (sourceX >= 0d && sourceY >= 0d && sourceX <= sourceWidth - 1d && sourceY <= sourceHeight - 1d) {
                row(x) = sampleBilinear(pixels, sourceWidth, sourceX, sourceY, brightness)
                changed = true
              }
            }
            x += 1
          }
          if (changed) image.setRGB(0, y, width, 1, row, 0, width)
        }
    }

    /** Blends a tile so that its center lands on the given position */
    def drawCenteredOn(
      tile: BufferedImage,
      centerX: Double,
      centerY: Double,
      mode: BlendMode = BlendMode.Lighten,
      mask: Option[Array[Float]] = None,
      opacity: Double = 1d
    ): Unit =
      draw(
        tile = tile,
        x = math.round(centerX - tile.getWidth / 2d).toInt,
        y = math.round(centerY - tile.getHeight / 2d).toInt,
        mode = mode,
        mask = mask,
        opacity = opacity
      )
  }

  object Canvas {
    def create(width: Int, height: Int, background: Color = Color.BLACK): Canvas = {
      val image    = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
      val graphics = image.createGraphics
      try {
        graphics.setColor(background)
        graphics.fillRect(0, 0, width, height)
      } finally graphics.dispose()
      Canvas(image)
    }
  }

  /** Bilinear sample of a packed RGB image, the four neighbours weighted by the distance to each */
  private def sampleBilinear(pixels: Array[Int], width: Int, x: Double, y: Double, brightness: Double): Int = {
    val leftColumn = x.toInt
    val topRow     = y.toInt
    val rightColumn = math.min(leftColumn + 1, width - 1)
    val bottomRow   = math.min(topRow + 1, pixels.length / width - 1)
    val fractionX   = x - leftColumn
    val fractionY   = y - topRow
    val topLeft     = pixels(topRow * width + leftColumn)
    val topRight    = pixels(topRow * width + rightColumn)
    val bottomLeft  = pixels(bottomRow * width + leftColumn)
    val bottomRight = pixels(bottomRow * width + rightColumn)

    def channel(shift: Int): Int = {
      val top    = ((topLeft >> shift) & 0xff) * (1d - fractionX) + ((topRight >> shift) & 0xff) * fractionX
      val bottom = ((bottomLeft >> shift) & 0xff) * (1d - fractionX) + ((bottomRight >> shift) & 0xff) * fractionX
      val value  = (top * (1d - fractionY) + bottom * fractionY) * brightness
      math.max(0, math.min(255, math.round(value).toInt))
    }

    (channel(16) << 16) | (channel(8) << 8) | channel(0)
  }

  private def blendPixel(source: Int, target: Int, mode: BlendMode, alpha: Double): Int = {
    val sourceRed   = (source >> 16) & 0xff
    val sourceGreen = (source >> 8) & 0xff
    val sourceBlue  = source & 0xff
    val targetRed   = (target >> 16) & 0xff
    val targetGreen = (target >> 8) & 0xff
    val targetBlue  = target & 0xff
    val red         = blendChannel(sourceRed, targetRed, mode, alpha)
    val green       = blendChannel(sourceGreen, targetGreen, mode, alpha)
    val blue        = blendChannel(sourceBlue, targetBlue, mode, alpha)
    (red << 16) | (green << 8) | blue
  }

  private def blendChannel(source: Int, target: Int, mode: BlendMode, alpha: Double): Int = {
    val blended = mode match {
      case BlendMode.Over    => source.toDouble
      case BlendMode.Lighten => math.max(source, target).toDouble
      case BlendMode.Add     => math.min(255d, source.toDouble + target)
      case BlendMode.Screen  => 255d - (255d - source) * (255d - target) / 255d
    }
    val mixed   = target + alpha * (blended - target)
    math.max(0, math.min(255, math.round(mixed).toInt))
  }
}
