package fr.janalyse.sotohp.media.imaging

import fr.janalyse.sotohp.media.imaging.Rasters.RgbRaster

class ToneMappingTest extends munit.FunSuite {

  private def pixel(red: Float, green: Float, blue: Float): RgbRaster =
    RgbRaster(1, 1, Array(red), Array(green), Array(blue))

  private def hueOf(raster: RgbRaster): (Double, Double) = {
    val strongest = math.max(raster.red(0), math.max(raster.green(0), raster.blue(0)))
    (raster.green(0) / strongest, raster.blue(0) / strongest)
  }

  test("a scaling that fits below the maximum is a plain exposure change") {
    val scaled = ToneMapping.scaleExposurePreservingHue(pixel(0.2f, 0.1f, 0.05f), 2d)
    assertEqualsFloat(scaled.red(0), 0.4f, 1e-6f)
    assertEqualsFloat(scaled.green(0), 0.2f, 1e-6f)
    assertEqualsFloat(scaled.blue(0), 0.1f, 1e-6f)
  }

  test("a sunset disc brightened past the maximum keeps its color instead of turning white") {
    // what the last frames of a session look like : deep red, and dim enough that bringing them to
    // the level of the others means multiplying by a lot
    val sunset = pixel(0.30f, 0.09f, 0.03f)
    val (green, blue) = hueOf(sunset)

    val preserved = ToneMapping.scaleExposurePreservingHue(sunset, 8d)
    val (preservedGreen, preservedBlue) = hueOf(preserved)
    assertEqualsDouble(preservedGreen, green, 1e-6d)
    assertEqualsDouble(preservedBlue, blue, 1e-6d)
    assertEqualsFloat(preserved.red(0), 1f, 1e-6f)

    // the naive scaling is what turned that disc into a pale blob : red stops at the maximum while
    // the two other channels keep climbing, and the hue drifts towards white
    val clipped = ToneMapping.scaleExposure(sunset, 8d)
    val naiveGreen = math.min(1d, clipped.green(0)) / math.min(1d, clipped.red(0))
    assert(naiveGreen > green * 2d, f"clipping should wash the color out, $naiveGreen%.2f against $green%.2f")
  }

  test("a neutral highlight is left where it was") {
    val white = ToneMapping.scaleExposurePreservingHue(pixel(0.5f, 0.5f, 0.5f), 4d)
    assertEqualsFloat(white.red(0), 1f, 1e-6f)
    assertEqualsFloat(white.green(0), 1f, 1e-6f)
    assertEqualsFloat(white.blue(0), 1f, 1e-6f)
  }

  test("the disc reaches the level it was asked for as long as it can") {
    val disc       = pixel(0.10f, 0.09f, 0.08f)
    val normalized = ToneMapping.normalizeReferenceLevel(disc, 0.10d, 0.80d)
    assertEqualsFloat(normalized.red(0), 0.8f, 1e-6f)
  }
}
