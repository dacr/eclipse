package fr.janalyse.sotohp.media.imaging

import fr.janalyse.sotohp.media.imaging.Rasters.RgbRaster

/** Merging several exposures of the same subject into one.
  *
  * A solar corona spans a range of brightness no single exposure can hold : the inner corona and
  * the prominences at the limb are thousands of times brighter than the outer streamers. Shooting a
  * bracket is the answer, and merging it is what turns that bracket into one picture showing both -
  * the long exposure bringing the reach, the short ones the detail that the long one burnt away.
  *
  * The principle is plain : a pixel value divided by the exposure that produced it gives the
  * brightness of the scene, the same number whatever the exposure - as long as the pixel is neither
  * clipped nor drowned in noise. Every exposure therefore votes, weighted by how well exposed that
  * particular pixel is, and the votes are averaged.
  */
object ExposureStack {

  /** One frame of the bracket.
    *
    * @param relativeExposure how much light the settings let in, in arbitrary but consistent units :
    *                         `time * iso / aperture²` does the job, only the ratios matter
    */
  final case class Exposure(raster: RgbRaster, relativeExposure: Double)

  final case class MergeConfig(
    /** the images are not linear, the converter applied a display curve : it is undone before the
      * arithmetic and put back afterwards, since dividing by an exposure only means something on
      * linear values
      */
    gamma: Double = 2.2d,
    /** above that, the pixel is clipped and says nothing but "at least this bright" */
    clippedAbove: Double = 0.95d,
    /** below that, it is mostly read noise */
    noiseBelow: Double = 0.02d
  )

  /** Merges a bracket into one raster holding relative scene brightness.
    *
    * The result is not meant to be displayed as it is : its values span whatever range the bracket
    * covered, which is the whole point. `display` brings it back to something visible.
    */
  def merge(exposures: Seq[Exposure], config: MergeConfig = MergeConfig()): Option[RgbRaster] = {
    val usable = exposures.filter(exposure => exposure.relativeExposure > 0d)
    if (usable.isEmpty) None
    else if (usable.sizeIs == 1) Some(usable.head.raster)
    else {
      val width  = usable.head.raster.width
      val height = usable.head.raster.height
      if (usable.exists(exposure => exposure.raster.width != width || exposure.raster.height != height)) None
      else {
        val size     = width * height
        val red      = Array.ofDim[Float](size)
        val green    = Array.ofDim[Float](size)
        val blue     = Array.ofDim[Float](size)
        // the exposures are brought back to the scale of the longest one, so that the result stays
        // in familiar territory rather than in arbitrary units
        val reference = usable.map(_.relativeExposure).max

        var index = 0
        while (index < size) {
          red(index) = mergeChannel(usable.map(exposure => (exposure.raster.red(index), exposure.relativeExposure)), reference, config)
          green(index) = mergeChannel(usable.map(exposure => (exposure.raster.green(index), exposure.relativeExposure)), reference, config)
          blue(index) = mergeChannel(usable.map(exposure => (exposure.raster.blue(index), exposure.relativeExposure)), reference, config)
          index += 1
        }
        Some(RgbRaster(width, height, red, green, blue))
      }
    }
  }

  private def mergeChannel(samples: Seq[(Float, Double)], reference: Double, config: MergeConfig): Float = {
    var weighted = 0d
    var weights  = 0d
    var best     = 0d
    var bestGap  = Double.MaxValue

    samples.foreach { case (value, exposure) =>
      val linear    = math.pow(math.max(0d, value.toDouble), config.gamma)
      val brightness = linear * reference / exposure
      val weight     = weightOf(value.toDouble, config)
      if (weight > 0d) { weighted += brightness * weight; weights += weight }
      // kept aside in case every exposure is clipped or drowned : the least bad one still answers
      val gap = math.abs(value.toDouble - 0.5d)
      if (gap < bestGap) { bestGap = gap; best = brightness }
    }

    val merged = if (weights > 0d) weighted / weights else best
    math.pow(math.max(0d, merged), 1d / config.gamma).toFloat
  }

  /** How much a pixel is worth listening to : a well exposed one speaks, a clipped or a dark one
    * does not. A plain triangular hat, which is enough and keeps the merge predictable.
    */
  private def weightOf(value: Double, config: MergeConfig): Double =
    if (value >= config.clippedAbove || value <= config.noiseBelow) 0d
    else 1d - math.abs(2d * value - 1d)

  /** Brings a merged raster back into the visible range.
    *
    * A corona needs a strong curve : its brightness falls by orders of magnitude from the limb
    * outwards, and a straight scaling would show either the inner corona or the outer streamers,
    * never both - which is exactly what merging the bracket was meant to avoid.
    */
  def display(merged: RgbRaster, highlightRatio: Double = 0.999d, gamma: Double = 2.6d): RgbRaster = {
    val gray  = merged.toGray
    val white = gray.percentile(highlightRatio)
    val black = gray.percentile(0.5d)
    ToneMapping.levels(merged, black, math.max(black + 1e-4d, white), gamma)
  }
}
