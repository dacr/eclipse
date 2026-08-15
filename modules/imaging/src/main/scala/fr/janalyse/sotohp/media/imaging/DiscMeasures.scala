package fr.janalyse.sotohp.media.imaging

import fr.janalyse.sotohp.media.imaging.CircleFitting.Circle
import fr.janalyse.sotohp.media.imaging.Rasters.{GrayRaster, RgbRaster}

/** Measurements taken inside a detected disc */
object DiscMeasures {

  /** Fraction of the disc area which is dark.
    *
    * On an eclipse frame this is the obscuration : the part of the solar disc hidden by the moon.
    * Measured on the image itself, no ephemeris of the moon needed, and it is exactly the quantity
    * one wants to spread evenly when building a sequence showing the eclipse progress.
    *
    * @param level luminance below which a pixel is considered as covered, when not given it is
    *              taken half way between the sky background and the disc brightness.
    */
  def obscuration(raster: GrayRaster, circle: Circle, level: Option[Double] = None): Double = {
    // the reference brightness has to be the photosphere one, taken on the whole frame : looking
    // for it inside the disc would collapse as soon as the disc is mostly covered, which is
    // precisely when the measurement matters most.
    val threshold  = level.getOrElse {
      val levels     = raster.percentiles(List(0.5d, 0.9999d))
      val background = levels.head
      val photosphere = levels.last
      (background + photosphere) / 2d
    }
    var covered    = 0L
    var total      = 0L
    val minimumX   = math.max(0, math.floor(circle.centerX - circle.radius).toInt)
    val maximumX   = math.min(raster.width - 1, math.ceil(circle.centerX + circle.radius).toInt)
    val minimumY   = math.max(0, math.floor(circle.centerY - circle.radius).toInt)
    val maximumY   = math.min(raster.height - 1, math.ceil(circle.centerY + circle.radius).toInt)
    var y          = minimumY
    while (y <= maximumY) {
      var x = minimumX
      while (x <= maximumX) {
        if (circle.distanceToCenter(x, y) <= circle.radius) {
          total += 1
          if (raster(x, y) <= threshold) covered += 1
        }
        x += 1
      }
      y += 1
    }
    if (total == 0L) 0d else covered.toDouble / total
  }

  /** Median luminance of the pixels inside the disc */
  def medianWithin(raster: GrayRaster, circle: Circle): Option[Double] = {
    val collected = collectWithin(raster, circle)
    if (collected.isEmpty) None
    else {
      val sorted = collected.sorted
      Some(sorted(sorted.length / 2).toDouble)
    }
  }

  /** Highest luminance found inside the disc, ignoring the hottest pixels */
  def highLevelWithin(raster: GrayRaster, circle: Circle, ratio: Double = 0.99d): Option[Double] = {
    val collected = collectWithin(raster, circle)
    if (collected.isEmpty) None
    else {
      val sorted = collected.sorted
      Some(sorted(math.min(sorted.length - 1, (sorted.length * ratio).toInt)).toDouble)
    }
  }

  /** Mean red/green/blue levels of the lit pixels inside the disc, the base of any color correction */
  def meanColorWithin(raster: RgbRaster, circle: Circle, minimumLevel: Double = 0.02d): Option[(Double, Double, Double)] = {
    var totalRed   = 0d
    var totalGreen = 0d
    var totalBlue  = 0d
    var count      = 0L
    val minimumX   = math.max(0, math.floor(circle.centerX - circle.radius).toInt)
    val maximumX   = math.min(raster.width - 1, math.ceil(circle.centerX + circle.radius).toInt)
    val minimumY   = math.max(0, math.floor(circle.centerY - circle.radius).toInt)
    val maximumY   = math.min(raster.height - 1, math.ceil(circle.centerY + circle.radius).toInt)
    var y          = minimumY
    while (y <= maximumY) {
      var x = minimumX
      while (x <= maximumX) {
        if (circle.distanceToCenter(x, y) <= circle.radius) {
          val index = y * raster.width + x
          if (raster.luminance(index) > minimumLevel) {
            totalRed += raster.red(index)
            totalGreen += raster.green(index)
            totalBlue += raster.blue(index)
            count += 1
          }
        }
        x += 1
      }
      y += 1
    }
    if (count == 0L) None else Some((totalRed / count, totalGreen / count, totalBlue / count))
  }

  /** How far the subject actually shines, in pixels from its center.
    *
    * The brightness is averaged over concentric rings, and the returned radius is the one where it
    * falls back into the sky level. On a totality frame this measures the extent of the recorded
    * corona - which is what a tile has to be large enough to hold, instead of being guessed.
    *
    * @param aboveSkyRatio how far above the sky level the signal must still be, as a fraction of
    *                      the whole dynamic range of the frame
    */
  def signalExtentRadius(
    raster: GrayRaster,
    circle: Circle,
    aboveSkyRatio: Double = 0.02d,
    maximumRadiusFactor: Double = 8d,
    ringCount: Int = 120
  ): Option[Double] = {
    val levels     = raster.percentiles(List(0.5d, 0.9999d))
    val sky        = levels.head
    val range      = levels.last - sky
    if (range <= 0d || circle.radius <= 0d) None
    else {
      val threshold     = sky + aboveSkyRatio * range
      val maximumRadius = circle.radius * maximumRadiusFactor
      val step          = maximumRadius / ringCount
      val totals        = Array.ofDim[Double](ringCount)
      val counts        = Array.ofDim[Int](ringCount)
      val minimumX      = math.max(0, math.floor(circle.centerX - maximumRadius).toInt)
      val maximumX      = math.min(raster.width - 1, math.ceil(circle.centerX + maximumRadius).toInt)
      val minimumY      = math.max(0, math.floor(circle.centerY - maximumRadius).toInt)
      val maximumY      = math.min(raster.height - 1, math.ceil(circle.centerY + maximumRadius).toInt)
      var y             = minimumY
      while (y <= maximumY) {
        var x = minimumX
        while (x <= maximumX) {
          val ring = (circle.distanceToCenter(x, y) / step).toInt
          if (ring < ringCount) { totals(ring) += raster(x, y); counts(ring) += 1 }
          x += 1
        }
        y += 1
      }
      // the outermost ring still above the threshold, scanning inwards so that a lonely bright
      // detail far away does not extend the measurement
      var ring  = ringCount - 1
      var found = -1
      while (ring >= 0 && found < 0) {
        if (counts(ring) > 0 && totals(ring) / counts(ring) > threshold) found = ring
        ring -= 1
      }
      if (found < 0) None else Some((found + 1) * step)
    }
  }

  /** Luminance percentiles of a ring drawn around the subject.
    *
    * Where `medianColorInRing` gives the sky level, this gives how much it wavers : the gap between
    * the median and a high percentile is the noise and the gradient of the sky put together, which
    * is exactly what has to be clipped away rather than amplified along with the subject.
    */
  def luminancePercentilesInRing(
    raster: RgbRaster,
    circle: Circle,
    innerRadius: Double,
    outerRadius: Double,
    ratios: List[Double]
  ): Option[List[Double]] = {
    val collected = Array.newBuilder[Float]
    val minimumX  = math.max(0, math.floor(circle.centerX - outerRadius).toInt)
    val maximumX  = math.min(raster.width - 1, math.ceil(circle.centerX + outerRadius).toInt)
    val minimumY  = math.max(0, math.floor(circle.centerY - outerRadius).toInt)
    val maximumY  = math.min(raster.height - 1, math.ceil(circle.centerY + outerRadius).toInt)
    var y         = minimumY
    while (y <= maximumY) {
      var x = minimumX
      while (x <= maximumX) {
        val distance = circle.distanceToCenter(x, y)
        if (distance >= innerRadius && distance <= outerRadius) collected += raster.luminance(y * raster.width + x)
        x += 1
      }
      y += 1
    }
    val values = collected.result()
    if (values.isEmpty) None
    else {
      val sorted = values.sorted
      Some(ratios.map(ratio => sorted(math.max(0, math.min(sorted.length - 1, (sorted.length * ratio).toInt))).toDouble))
    }
  }

  /** Median color of a ring drawn around the subject : the sky level, measured where the subject is
    * not. Subtracting it removes a twilight gradient, a light polluted sky or a filter flare, and
    * it is a median so a branch or a star crossing the ring does not disturb it.
    */
  def medianColorInRing(
    raster: RgbRaster,
    circle: Circle,
    innerRadius: Double,
    outerRadius: Double
  ): Option[(Double, Double, Double)] = {
    val red      = Array.newBuilder[Float]
    val green    = Array.newBuilder[Float]
    val blue     = Array.newBuilder[Float]
    val minimumX = math.max(0, math.floor(circle.centerX - outerRadius).toInt)
    val maximumX = math.min(raster.width - 1, math.ceil(circle.centerX + outerRadius).toInt)
    val minimumY = math.max(0, math.floor(circle.centerY - outerRadius).toInt)
    val maximumY = math.min(raster.height - 1, math.ceil(circle.centerY + outerRadius).toInt)
    var y        = minimumY
    while (y <= maximumY) {
      var x = minimumX
      while (x <= maximumX) {
        val distance = circle.distanceToCenter(x, y)
        if (distance >= innerRadius && distance <= outerRadius) {
          val index = y * raster.width + x
          red += raster.red(index)
          green += raster.green(index)
          blue += raster.blue(index)
        }
        x += 1
      }
      y += 1
    }
    val redValues   = red.result()
    val greenValues = green.result()
    val blueValues  = blue.result()
    if (redValues.isEmpty) None
    else {
      def median(values: Array[Float]): Double = {
        val sorted = values.sorted
        sorted(sorted.length / 2).toDouble
      }
      Some((median(redValues), median(greenValues), median(blueValues)))
    }
  }

  private def collectWithin(raster: GrayRaster, circle: Circle): Array[Float] = {
    val builder  = Array.newBuilder[Float]
    val minimumX = math.max(0, math.floor(circle.centerX - circle.radius).toInt)
    val maximumX = math.min(raster.width - 1, math.ceil(circle.centerX + circle.radius).toInt)
    val minimumY = math.max(0, math.floor(circle.centerY - circle.radius).toInt)
    val maximumY = math.min(raster.height - 1, math.ceil(circle.centerY + circle.radius).toInt)
    var y        = minimumY
    while (y <= maximumY) {
      var x = minimumX
      while (x <= maximumX) {
        if (circle.distanceToCenter(x, y) <= circle.radius) builder += raster(x, y)
        x += 1
      }
      y += 1
    }
    builder.result()
  }
}
