package fr.janalyse.sotohp.media.imaging

import fr.janalyse.sotohp.media.imaging.Rasters.RgbRaster

/** Tone and color adjustments working on floating point rasters.
  *
  * They are kept purely functional and unit-less (levels within [0..1]) so that they can be applied
  * the same way whatever the source depth is - which matters a lot when mixing frames shot through
  * a very dense solar filter with frames shot without any filter at all.
  */
object ToneMapping {

  /** Classic black point / white point / gamma stretch */
  def levels(raster: RgbRaster, blackPoint: Double, whitePoint: Double, gamma: Double = 1d): RgbRaster = {
    val span    = math.max(1e-6d, whitePoint - blackPoint)
    val inverse = 1d / math.max(1e-6d, gamma)
    raster.mapChannels { value =>
      val stretched = (value - blackPoint) / span
      val clamped   = math.max(0d, math.min(1d, stretched))
      math.pow(clamped, inverse).toFloat
    }
  }

  /** Automatic stretch based on the raster own luminance distribution */
  def autoLevels(raster: RgbRaster, lowRatio: Double = 0.001d, highRatio: Double = 0.999d, gamma: Double = 1d): RgbRaster = {
    val bounds = raster.toGray.percentiles(List(lowRatio, highRatio))
    levels(raster, bounds.head, bounds.last, gamma)
  }

  /** Multiplies every channel, the way an exposure compensation would */
  def scaleExposure(raster: RgbRaster, factor: Double): RgbRaster =
    raster.mapChannels(value => (value * factor).toFloat)

  /** Brings a reference level (typically the median brightness of the solar disc) onto a target
    * value. This is what makes a whole sequence look consistent despite exposure changes.
    */
  def normalizeReferenceLevel(raster: RgbRaster, referenceLevel: Double, targetLevel: Double): RgbRaster =
    if (referenceLevel <= 1e-6d) raster
    else scaleExposure(raster, targetLevel / referenceLevel)

  def gammaCorrect(raster: RgbRaster, gamma: Double): RgbRaster = {
    val inverse = 1d / math.max(1e-6d, gamma)
    raster.mapChannels(value => math.pow(math.max(0d, value), inverse).toFloat)
  }
}

/** Color balance operations */
object ColorBalance {

  /** Applies per channel gains */
  def applyGains(raster: RgbRaster, redGain: Double, greenGain: Double, blueGain: Double): RgbRaster = {
    val size     = raster.size
    val newRed   = Array.ofDim[Float](size)
    val newGreen = Array.ofDim[Float](size)
    val newBlue  = Array.ofDim[Float](size)
    var index    = 0
    while (index < size) {
      newRed(index) = (raster.red(index) * redGain).toFloat
      newGreen(index) = (raster.green(index) * greenGain).toFloat
      newBlue(index) = (raster.blue(index) * blueGain).toFloat
      index += 1
    }
    RgbRaster(raster.width, raster.height, newRed, newGreen, newBlue)
  }

  /** Neutralizes a color cast from a measured reference color.
    *
    * A very dense solar filter (ND100000 and beyond) gives a strong and filter dependent cast, this
    * removes it by making the measured reference neutral, then optionally re-tints the result to a
    * chosen color - a slightly warm sun usually looks better than a pure white one.
    */
  /** @param maximumGain a channel is never pushed further than that.
    *
    * The bound is not a detail. A sun a couple of degrees above the horizon is deeply reddened by
    * the atmosphere - its blue channel may hold almost nothing - and asking for a neutral disc then
    * means multiplying that channel by ten or twenty. What little signal there is gets amplified
    * along with the sky and the noise, and the frame comes out blue with a green crescent in it.
    * Past the bound, the correction is simply left incomplete : a low sun is meant to look warm.
    */
  def neutralizeFrom(
    raster: RgbRaster,
    reference: (Double, Double, Double),
    target: (Double, Double, Double) = (1d, 1d, 1d),
    maximumGain: Double = 2.5d
  ): RgbRaster = {
    val (referenceRed, referenceGreen, referenceBlue) = reference
    val (targetRed, targetGreen, targetBlue)          = target
    if (referenceRed <= 1e-6d || referenceGreen <= 1e-6d || referenceBlue <= 1e-6d) raster
    else {
      val referenceMean = (referenceRed + referenceGreen + referenceBlue) / 3d
      def bounded(gain: Double): Double = math.max(1d / maximumGain, math.min(maximumGain, gain))
      applyGains(
        raster,
        redGain = bounded(targetRed * referenceMean / referenceRed),
        greenGain = bounded(targetGreen * referenceMean / referenceGreen),
        blueGain = bounded(targetBlue * referenceMean / referenceBlue)
      )
    }
  }

  /** Removes a uniform background level (light pollution, sky glow, filter flare) */
  def removeBackground(raster: RgbRaster, level: Double): RgbRaster =
    raster.mapChannels(value => math.max(0d, value - level).toFloat)

  /** Removes a background level channel by channel : a twilight sky is neither neutral nor uniform
    * in color, subtracting a single level would leave a colored haze behind.
    */
  def removeBackground(raster: RgbRaster, levels: (Double, Double, Double)): RgbRaster = {
    val (redLevel, greenLevel, blueLevel) = levels
    val size     = raster.size
    val newRed   = Array.ofDim[Float](size)
    val newGreen = Array.ofDim[Float](size)
    val newBlue  = Array.ofDim[Float](size)
    var index    = 0
    while (index < size) {
      newRed(index) = math.max(0d, raster.red(index) - redLevel).toFloat
      newGreen(index) = math.max(0d, raster.green(index) - greenLevel).toFloat
      newBlue(index) = math.max(0d, raster.blue(index) - blueLevel).toFloat
      index += 1
    }
    RgbRaster(raster.width, raster.height, newRed, newGreen, newBlue)
  }
}
