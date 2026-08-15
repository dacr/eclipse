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

  /** Multiplies every channel, but never past the point where a channel would clip.
    *
    * Clipping a channel changes the color : brighten a deep red sun enough and its red channel
    * stops at the maximum while green and blue keep climbing, so the middle of the disc turns pale,
    * then white, then faintly blue - a sunset rendered as a snowball. Scaling the whole pixel by
    * whatever room its strongest channel has left keeps the ratios, hence the hue : the disc
    * saturates in brightness, as it must, but stays the color it was photographed.
    */
  def scaleExposurePreservingHue(raster: RgbRaster, factor: Double): RgbRaster = {
    val size     = raster.size
    val newRed   = Array.ofDim[Float](size)
    val newGreen = Array.ofDim[Float](size)
    val newBlue  = Array.ofDim[Float](size)
    var index    = 0
    while (index < size) {
      val red      = raster.red(index)
      val green    = raster.green(index)
      val blue     = raster.blue(index)
      val strongest = math.max(red, math.max(green, blue))
      val applied   = if (strongest * factor > 1d) 1d / strongest else factor
      newRed(index) = (red * applied).toFloat
      newGreen(index) = (green * applied).toFloat
      newBlue(index) = (blue * applied).toFloat
      index += 1
    }
    RgbRaster(raster.width, raster.height, newRed, newGreen, newBlue)
  }

  /** Brings a reference level (typically the median brightness of the solar disc) onto a target
    * value. This is what makes a whole sequence look consistent despite exposure changes.
    */
  def normalizeReferenceLevel(raster: RgbRaster, referenceLevel: Double, targetLevel: Double): RgbRaster =
    if (referenceLevel <= 1e-6d) raster
    else scaleExposurePreservingHue(raster, targetLevel / referenceLevel)

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

  /** Applies per channel gains, fading them out where the pixel is saturated.
    *
    * A blown highlight has no color left to correct. When the sun is photographed without its
    * filter, or a stop too long, the middle of the disc reaches the maximum in red and green while
    * blue stops short of it - not because the sun is that color there, but because two channels ran
    * out of room. Applying a cast correction to such a pixel - red down, blue up, as a warm frame
    * calls for - swaps the order of its channels and the disc comes out violet.
    *
    * So the correction fades away as a pixel approaches the maximum, and vanishes at it : what was
    * recorded as the brightest thing in the frame stays the brightest thing in the frame, with the
    * color it was given. Everything below the knee is corrected in full, which is the whole disc on
    * a properly exposed frame.
    */
  def applyGainsProtectingHighlights(
    raster: RgbRaster,
    redGain: Double,
    greenGain: Double,
    blueGain: Double,
    highlightKnee: Double = 0.9d
  ): RgbRaster = {
    val size     = raster.size
    val newRed   = Array.ofDim[Float](size)
    val newGreen = Array.ofDim[Float](size)
    val newBlue  = Array.ofDim[Float](size)
    val headroom = math.max(1e-6d, 1d - highlightKnee)
    var index    = 0
    while (index < size) {
      val red       = raster.red(index)
      val green     = raster.green(index)
      val blue      = raster.blue(index)
      val strongest = math.max(red, math.max(green, blue))
      val protection = math.max(0d, math.min(1d, (strongest - highlightKnee) / headroom))
      def faded(gain: Double): Double = gain * (1d - protection) + protection
      newRed(index) = (red * faded(redGain)).toFloat
      newGreen(index) = (green * faded(greenGain)).toFloat
      newBlue(index) = (blue * faded(blueGain)).toFloat
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
  /** @param highlightKnee level above which the correction fades out, 1 to correct everything.
    *
    * Worth setting below 1 only when the frame is known to hold blown highlights : their color was
    * not recorded, so correcting it invents one. See [[applyGainsProtectingHighlights]].
    */
  def neutralizeFrom(
    raster: RgbRaster,
    reference: (Double, Double, Double),
    target: (Double, Double, Double) = (1d, 1d, 1d),
    maximumGain: Double = 2.5d,
    highlightKnee: Double = 1d
  ): RgbRaster = {
    val (referenceRed, referenceGreen, referenceBlue) = reference
    val (targetRed, targetGreen, targetBlue)          = target
    if (referenceRed <= 1e-6d || referenceGreen <= 1e-6d || referenceBlue <= 1e-6d) raster
    else {
      val referenceMean = (referenceRed + referenceGreen + referenceBlue) / 3d
      def bounded(gain: Double): Double = math.max(1d / maximumGain, math.min(maximumGain, gain))
      applyGainsProtectingHighlights(
        raster,
        redGain = bounded(targetRed * referenceMean / referenceRed),
        greenGain = bounded(targetGreen * referenceMean / referenceGreen),
        blueGain = bounded(targetBlue * referenceMean / referenceBlue),
        highlightKnee = highlightKnee
      )
    }
  }

  /** Removes a uniform background level (light pollution, sky glow, filter flare) */
  def removeBackground(raster: RgbRaster, level: Double): RgbRaster =
    raster.mapChannels(value => math.max(0d, value - level).toFloat)

  /** Removes a background level without touching the color of what is left.
    *
    * Subtracting a level from each channel on its own changes the hue of everything dim : take the
    * deep red glow around a sun setting in the haze, subtract a level of the same order, and what
    * survives is whichever channel happened to be relatively less absorbed - blue. Brighten that
    * afterwards, as the brightness normalization does, and a red sun comes out lavender.
    *
    * So the level is taken off the luminance instead, and the three channels are scaled by the same
    * factor : anything at or below the background goes to black, anything above keeps its color and
    * loses exactly the background brightness.
    */
  def removeBackgroundPreservingHue(raster: RgbRaster, level: Double): RgbRaster = {
    val size     = raster.size
    val newRed   = Array.ofDim[Float](size)
    val newGreen = Array.ofDim[Float](size)
    val newBlue  = Array.ofDim[Float](size)
    var index    = 0
    while (index < size) {
      val luminance = raster.luminance(index)
      val factor    = if (luminance <= level) 0d else (luminance - level) / luminance
      newRed(index) = (raster.red(index) * factor).toFloat
      newGreen(index) = (raster.green(index) * factor).toFloat
      newBlue(index) = (raster.blue(index) * factor).toFloat
      index += 1
    }
    RgbRaster(raster.width, raster.height, newRed, newGreen, newBlue)
  }

  /** Removes a background level channel by channel : a twilight sky is neither neutral nor uniform
    * in color, subtracting a single level would leave a colored haze behind.
    *
    * Only worth it when what is left is bright compared with the background - the corona at
    * totality. On a dim subject use [[removeBackgroundPreservingHue]], which does not let the
    * subtraction decide the color.
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
