package fr.janalyse.sotohp.media.imaging

import java.awt.image.{BufferedImage, IndexColorModel}

/** Floating point image representations.
  *
  * `BufferedImage` is fine to load, resize and save images but it is a poor fit for measurements and
  * pixel level maths : the sample depth (8 bits, 16 bits, ...) and the band layout leak into every
  * algorithm. Those rasters normalize everything to `Float` values within [0..1] whatever the source
  * image is, so that all the processing code below stays simple and depth agnostic.
  *
  * Memory wise a raster costs 4 bytes per pixel and per channel, they are meant to be built either
  * on downscaled images (analysis) or on small crops (tiles), not on full resolution 45Mpix images.
  */
object Rasters {

  /** Rec.709 luminance weights */
  val LuminanceWeights: (Double, Double, Double) = (0.2126d, 0.7152d, 0.0722d)

  private def maxSampleValue(image: BufferedImage): Double = {
    val bits = image.getColorModel.getComponentSize(0)
    (math.pow(2d, bits.toDouble) - 1d).max(1d)
  }

  private def isIndexed(image: BufferedImage): Boolean =
    image.getColorModel.isInstanceOf[IndexColorModel]

  /** Reads one image row as normalized red/green/blue values within [0..1] */
  private def readRow(image: BufferedImage, y: Int, red: Array[Float], green: Array[Float], blue: Array[Float]): Unit = {
    val width = image.getWidth
    if (isIndexed(image)) {
      var x = 0
      while (x < width) {
        val rgb = image.getRGB(x, y)
        red(x) = ((rgb >> 16) & 0xff) / 255f
        green(x) = ((rgb >> 8) & 0xff) / 255f
        blue(x) = (rgb & 0xff) / 255f
        x += 1
      }
    } else {
      val raster    = image.getRaster
      val bandCount = raster.getNumBands
      val maxValue  = maxSampleValue(image)
      val samples   = raster.getPixels(0, y, width, 1, null.asInstanceOf[Array[Int]])
      var x         = 0
      while (x < width) {
        val base = x * bandCount
        if (bandCount >= 3) {
          red(x) = (samples(base) / maxValue).toFloat
          green(x) = (samples(base + 1) / maxValue).toFloat
          blue(x) = (samples(base + 2) / maxValue).toFloat
        } else {
          val gray = (samples(base) / maxValue).toFloat
          red(x) = gray
          green(x) = gray
          blue(x) = gray
        }
        x += 1
      }
    }
  }

  /** Single channel view of an image, used by every measurement algorithm */
  final case class GrayRaster(width: Int, height: Int, values: Array[Float]) {
    def size: Int = width * height

    def apply(x: Int, y: Int): Float = values(y * width + x)

    def at(x: Int, y: Int, outside: Float = 0f): Float =
      if (x < 0 || y < 0 || x >= width || y >= height) outside else values(y * width + x)

    def minimum: Float = {
      var found = Float.MaxValue
      var index = 0
      while (index < values.length) { if (values(index) < found) found = values(index); index += 1 }
      if (values.isEmpty) 0f else found
    }

    def maximum: Float = {
      var found = Float.MinValue
      var index = 0
      while (index < values.length) { if (values(index) > found) found = values(index); index += 1 }
      if (values.isEmpty) 0f else found
    }

    def mean: Double = {
      var total = 0d
      var index = 0
      while (index < values.length) { total += values(index); index += 1 }
      if (values.isEmpty) 0d else total / values.length
    }

    /** Cumulative histogram based percentile, avoids sorting millions of values */
    def percentile(ratio: Double, binCount: Int = 4096): Double = percentiles(List(ratio), binCount).head

    /** Several percentiles at once, sharing a single histogram pass */
    def percentiles(ratios: List[Double], binCount: Int = 4096): List[Double] = {
      if (values.isEmpty) ratios.map(_ => 0d)
      else {
        val low       = minimum.toDouble
        val high      = maximum.toDouble
        val span      = high - low
        if (span <= 0d) ratios.map(_ => low)
        else {
          val histogram = Array.ofDim[Int](binCount)
          var index     = 0
          while (index < values.length) {
            val bin = (((values(index) - low) / span) * (binCount - 1)).toInt
            histogram(math.max(0, math.min(binCount - 1, bin))) += 1
            index += 1
          }
          ratios.map { ratio =>
            val target    = math.max(0d, math.min(1d, ratio)) * values.length
            var bin       = 0
            var collected = 0L
            while (bin < binCount - 1 && collected + histogram(bin) < target) {
              collected += histogram(bin)
              bin += 1
            }
            low + span * bin / (binCount - 1)
          }
        }
      }
    }

    /** Binary mask of every pixel strictly brighter than the given level */
    def brighterThan(level: Double): BitMask = {
      val flags = Array.ofDim[Boolean](values.length)
      var index = 0
      var count = 0
      while (index < values.length) {
        val flag = values(index) > level
        flags(index) = flag
        if (flag) count += 1
        index += 1
      }
      BitMask(width, height, flags, count)
    }

    def toImage: BufferedImage = {
      val image = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
      var y     = 0
      while (y < height) {
        var x = 0
        while (x < width) {
          val level = math.max(0, math.min(255, (apply(x, y) * 255f).toInt))
          image.setRGB(x, y, (level << 16) | (level << 8) | level)
          x += 1
        }
        y += 1
      }
      image
    }
  }

  object GrayRaster {
    def fromImage(image: BufferedImage, weights: (Double, Double, Double) = LuminanceWeights): GrayRaster = {
      val width                          = image.getWidth
      val height                         = image.getHeight
      val values                         = Array.ofDim[Float](width * height)
      val red                            = Array.ofDim[Float](width)
      val green                          = Array.ofDim[Float](width)
      val blue                           = Array.ofDim[Float](width)
      val (redRatio, greenRatio, blueRatio) = weights
      var y                              = 0
      while (y < height) {
        readRow(image, y, red, green, blue)
        var x = 0
        while (x < width) {
          values(y * width + x) = (red(x) * redRatio + green(x) * greenRatio + blue(x) * blueRatio).toFloat
          x += 1
        }
        y += 1
      }
      GrayRaster(width, height, values)
    }
  }

  /** Boolean mask with a few geometric helpers */
  final case class BitMask(width: Int, height: Int, flags: Array[Boolean], count: Int) {
    def apply(x: Int, y: Int): Boolean =
      if (x < 0 || y < 0 || x >= width || y >= height) false else flags(y * width + x)

    def coverage: Double = if (flags.isEmpty) 0d else count.toDouble / flags.length

    /** Bounding box of the mask, as (minimumX, minimumY, maximumX, maximumY) */
    def bounds: Option[(Int, Int, Int, Int)] = {
      var minimumX = Int.MaxValue
      var minimumY = Int.MaxValue
      var maximumX = Int.MinValue
      var maximumY = Int.MinValue
      var y        = 0
      while (y < height) {
        var x = 0
        while (x < width) {
          if (flags(y * width + x)) {
            if (x < minimumX) minimumX = x
            if (x > maximumX) maximumX = x
            if (y < minimumY) minimumY = y
            if (y > maximumY) maximumY = y
          }
          x += 1
        }
        y += 1
      }
      if (maximumX < minimumX) None else Some((minimumX, minimumY, maximumX, maximumY))
    }

    /** How much of the frame the mask spans, horizontally and vertically.
      *
      * A round subject stays compact whatever its brightness : anything spanning the whole frame is
      * not the subject but the sky, the ground, or a flare.
      */
    def span: (Double, Double) =
      bounds match {
        case None                                          => (0d, 0d)
        case Some((minimumX, minimumY, maximumX, maximumY)) =>
          ((maximumX - minimumX + 1).toDouble / width, (maximumY - minimumY + 1).toDouble / height)
      }

    /** Center of gravity of the mask, beware : for a crescent shape this is NOT the disc center */
    def centroid: Option[(Double, Double)] = {
      var totalX = 0d
      var totalY = 0d
      var total  = 0L
      var y      = 0
      while (y < height) {
        var x = 0
        while (x < width) {
          if (flags(y * width + x)) { totalX += x; totalY += y; total += 1 }
          x += 1
        }
        y += 1
      }
      if (total == 0L) None else Some((totalX / total, totalY / total))
    }
  }

  /** Three channels floating point image, the working representation for tone and color operations */
  final case class RgbRaster(width: Int, height: Int, red: Array[Float], green: Array[Float], blue: Array[Float]) {
    def size: Int = width * height

    def luminance(index: Int): Float = {
      val (redRatio, greenRatio, blueRatio) = LuminanceWeights
      (red(index) * redRatio + green(index) * greenRatio + blue(index) * blueRatio).toFloat
    }

    def toGray: GrayRaster = {
      val values = Array.ofDim[Float](size)
      var index  = 0
      while (index < values.length) { values(index) = luminance(index); index += 1 }
      GrayRaster(width, height, values)
    }

    /** Applies the given function to every channel of every pixel */
    def mapChannels(transform: Float => Float): RgbRaster = {
      val newRed   = Array.ofDim[Float](size)
      val newGreen = Array.ofDim[Float](size)
      val newBlue  = Array.ofDim[Float](size)
      var index    = 0
      while (index < size) {
        newRed(index) = transform(red(index))
        newGreen(index) = transform(green(index))
        newBlue(index) = transform(blue(index))
        index += 1
      }
      RgbRaster(width, height, newRed, newGreen, newBlue)
    }

    def toImage: BufferedImage = {
      val image = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
      var y     = 0
      while (y < height) {
        var x = 0
        while (x < width) {
          val index = y * width + x
          val r     = clamp8(red(index))
          val g     = clamp8(green(index))
          val b     = clamp8(blue(index))
          image.setRGB(x, y, (r << 16) | (g << 8) | b)
          x += 1
        }
        y += 1
      }
      image
    }

    private def clamp8(value: Float): Int =
      math.max(0, math.min(255, math.round(value * 255f)))
  }

  object RgbRaster {
    def fromImage(image: BufferedImage): RgbRaster = {
      val width  = image.getWidth
      val height = image.getHeight
      val red    = Array.ofDim[Float](width * height)
      val green  = Array.ofDim[Float](width * height)
      val blue   = Array.ofDim[Float](width * height)
      val rowRed   = Array.ofDim[Float](width)
      val rowGreen = Array.ofDim[Float](width)
      val rowBlue  = Array.ofDim[Float](width)
      var y      = 0
      while (y < height) {
        readRow(image, y, rowRed, rowGreen, rowBlue)
        System.arraycopy(rowRed, 0, red, y * width, width)
        System.arraycopy(rowGreen, 0, green, y * width, width)
        System.arraycopy(rowBlue, 0, blue, y * width, width)
        y += 1
      }
      RgbRaster(width, height, red, green, blue)
    }
  }
}
