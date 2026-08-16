package fr.janalyse.sotohp.media.imaging

import fr.janalyse.sotohp.media.imaging.Rasters.GrayRaster

class BrightestSpotTest extends munit.FunSuite {

  private def blank(width: Int, height: Int): Array[Float] = Array.ofDim[Float](width * height)

  /** A landscape at dusk : a gradient of sky, and whatever is drawn on it */
  private def dusk(width: Int, height: Int): Array[Float] = {
    val values = blank(width, height)
    var y      = 0
    while (y < height) {
      var x = 0
      while (x < width) {
        values(y * width + x) = (0.25f * y / height) + 0.05f
        x += 1
      }
      y += 1
    }
    values
  }

  private def paintDisc(values: Array[Float], width: Int, centerX: Double, centerY: Double, radius: Double, level: Float): Unit = {
    var y = 0
    while (y < values.length / width) {
      var x = 0
      while (x < width) {
        if (math.hypot(x - centerX, y - centerY) <= radius) values(y * width + x) = level
        x += 1
      }
      y += 1
    }
  }

  test("the sun of a landscape is found, whatever else is bright in it") {
    val width  = 600
    val height = 400
    val values = dusk(width, height)
    paintDisc(values, width, 410d, 150d, 12d, 1f)   // the sun
    paintDisc(values, width, 120d, 330d, 25d, 0.5f) // a bright roof
    values(37 * width + 12) = 0.98f                 // a hot pixel

    val spot = DiscMeasures.brightestSpot(GrayRaster(width, height, values)).get
    assertEqualsDouble(spot.centerX, 410d, 1.5d)
    assertEqualsDouble(spot.centerY, 150d, 1.5d)
    assertEqualsDouble(spot.radiusPixels, 12d, 1.5d)
  }

  test("an eclipsed sun is measured on its outline, where its center of gravity has already left") {
    // the moon eats the upper left of the disc, as it did at the end of that afternoon
    val width  = 300
    val height = 300
    val values = blank(width, height)
    paintDisc(values, width, 150d, 150d, 40d, 1f)
    paintDisc(values, width, 130d, 130d, 26d, 0f)

    val raster  = GrayRaster(width, height, values)
    val spot    = DiscMeasures.brightestSpot(raster).get
    val gravity = raster.brighterThan(0.5d).centroid.get

    val outlineError = math.hypot(spot.centerX - 150d, spot.centerY - 150d)
    val gravityError = math.hypot(gravity._1 - 150d, gravity._2 - 150d)
    assert(outlineError < 1.5d, f"the outline should still give the center, it is off by $outlineError%.1f px")
    assert(
      gravityError > 4d && gravityError > outlineError * 3d,
      f"the center of gravity should have drifted, it is off by $gravityError%.1f px only"
    )
  }

  test("a picture with nothing in it holds no spot") {
    val width  = 100
    val height = 100
    assert(DiscMeasures.brightestSpot(GrayRaster(width, height, blank(width, height))).isEmpty)
  }

  test("the largest group is the one that holds together, not the one drawn first") {
    val width  = 200
    val height = 100
    val values = blank(width, height)
    paintDisc(values, width, 30d, 50d, 6d, 1f)
    paintDisc(values, width, 150d, 50d, 20d, 1f)

    val group = GrayRaster(width, height, values).brighterThan(0.5d).largestGroup.get
    val (minimumX, _, maximumX, _) = group.bounds.get
    assert(minimumX > 120, s"the small disc should have been left out, the group starts at $minimumX")
    assert(maximumX < 180, s"and it should not reach beyond the large one, it ends at $maximumX")
  }
}
