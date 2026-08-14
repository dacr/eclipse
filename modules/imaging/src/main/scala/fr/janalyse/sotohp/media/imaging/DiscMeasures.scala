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
