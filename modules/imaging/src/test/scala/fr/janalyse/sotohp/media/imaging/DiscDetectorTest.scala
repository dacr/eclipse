package fr.janalyse.sotohp.media.imaging

import fr.janalyse.sotohp.media.imaging.CircleFitting.{Circle, Point}
import fr.janalyse.sotohp.media.imaging.Rasters.GrayRaster

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D, RenderingHints}

class DiscDetectorTest extends munit.FunSuite {

  /** Synthetic eclipse frame : a bright solar disc, partly hidden by a dark lunar disc */
  def eclipseFrame(
    width: Int = 1600,
    height: Int = 1200,
    sun: Circle = Circle(700d, 500d, 200d),
    moon: Option[Circle] = None
  ): BufferedImage = {
    val image    = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val graphics = image.createGraphics
    try {
      graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      graphics.setColor(Color(4, 3, 6)) // not perfectly black, as a real sky never is
      graphics.fillRect(0, 0, width, height)
      fill(graphics, sun, Color(255, 245, 230))
      moon.foreach(disc => fill(graphics, disc, Color(6, 5, 8)))
      image
    } finally graphics.dispose()
  }

  private def fill(graphics: Graphics2D, circle: Circle, color: Color): Unit = {
    graphics.setColor(color)
    graphics.fillOval(
      math.round(circle.centerX - circle.radius).toInt,
      math.round(circle.centerY - circle.radius).toInt,
      math.round(circle.radius * 2d).toInt,
      math.round(circle.radius * 2d).toInt
    )
  }

  test("the full solar disc is found with its exact position and size") {
    val sun   = Circle(700d, 500d, 200d)
    val image = eclipseFrame(sun = sun)
    val found = DiscDetector.detect(image).fold(error => fail(error), identity)
    assertEqualsDouble(found.circle.centerX, sun.centerX, 1.5d)
    assertEqualsDouble(found.circle.centerY, sun.centerY, 1.5d)
    assertEqualsDouble(found.circle.radius, sun.radius, 2d)
    assertEquals(found.kind, DiscDetector.DiscKind.Photosphere)
  }

  test("a partially eclipsed sun is still centered on the solar disc, not on the lit crescent") {
    val sun   = Circle(700d, 500d, 200d)
    val moon  = Circle(790d, 430d, 205d) // hides a good half of the disc
    val image = eclipseFrame(sun = sun, moon = Some(moon))
    val found = DiscDetector.detect(image).fold(error => fail(error), identity)
    assertEqualsDouble(found.circle.centerX, sun.centerX, 3d)
    assertEqualsDouble(found.circle.centerY, sun.centerY, 3d)
    assertEqualsDouble(found.circle.radius, sun.radius, 5d)
  }

  test("a thin crescent is recovered when the expected radius is known") {
    val sun    = Circle(700d, 500d, 200d)
    val moon   = Circle(760d, 460d, 205d)
    val image  = eclipseFrame(sun = sun, moon = Some(moon))
    val config = DiscDetector.DiscDetectorConfig(expectedRadiusPixels = Some(200d))
    val found  = DiscDetector.detect(image, config).fold(error => fail(error), identity)
    assertEqualsDouble(found.circle.centerX, sun.centerX, 3d)
    assertEqualsDouble(found.circle.centerY, sun.centerY, 3d)
    assertEqualsDouble(found.circle.radius, 200d, 3d)
    // the fit is left free as long as it stays plausible : the measured radius is its own, not the
    // one handed over, otherwise a disc that looks a little smaller than expected would have all of
    // its limb rejected as if it belonged to the moon
    assert(found.radialSpread < 2d, f"the limb points should sit on a circle, spread ${found.radialSpread}%.2f px")
  }

  test("a radius expected far too large does not throw the whole limb away") {
    val sun    = Circle(700d, 500d, 200d)
    val image  = eclipseFrame(sun = sun, moon = Some(Circle(790d, 430d, 205d)))
    // the session says 240 px where the frame shows 200, as a darker exposure would
    val config = DiscDetector.DiscDetectorConfig(expectedRadiusPixels = Some(240d))
    val found  = DiscDetector.detect(image, config).fold(error => fail(error), identity)
    assertEqualsDouble(found.circle.centerX, sun.centerX, 4d)
    assertEqualsDouble(found.circle.centerY, sun.centerY, 4d)
    assertEqualsDouble(found.circle.radius, 200d, 6d)
    assert(found.inlierCount > found.boundaryPointCount / 4, s"${found.inlierCount} inliers out of ${found.boundaryPointCount}")
  }

  test("obscuration measures how much of the solar disc is hidden") {
    val sun         = Circle(700d, 500d, 200d)
    val image       = eclipseFrame(sun = sun, moon = Some(Circle(700d, 500d - 200d, 200d))) // half a disc away
    val raster      = GrayRaster.fromImage(image)
    val obscuration = DiscMeasures.obscuration(raster, sun)
    // two circles of the same radius, centers one radius apart, overlap on ~39% of their area
    assertEqualsDouble(obscuration, 0.391d, 0.03d)
  }

  test("the moon is located during totality, from the corona alone") {
    val width    = 1200
    val height   = 900
    val image    = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val center   = (620d, 470d)
    var y        = 0
    while (y < height) {
      var x = 0
      while (x < width) {
        val distance = math.hypot(x - center._1, y - center._2)
        val level    = if (distance < 80d) 2d else math.max(2d, 120d * math.exp(-(distance - 80d) / 90d))
        val value    = math.min(255, level.toInt)
        image.setRGB(x, y, (value << 16) | (value << 8) | value)
        x += 1
      }
      y += 1
    }
    val config = DiscDetector.DiscDetectorConfig(expectedRadiusPixels = Some(80d))
    val found  = DiscDetector.detect(image, config).fold(error => fail(error), identity)
    assertEquals(found.kind, DiscDetector.DiscKind.Corona)
    assertEqualsDouble(found.circle.centerX, center._1, 6d)
    assertEqualsDouble(found.circle.centerY, center._2, 6d)
  }

  test("the sun is still found in a bright twilight sky, with foreground in the frame") {
    // what a totality frame taken at dusk really looks like : a sky gradient covering the whole
    // image, dark branches in a corner, and the subject barely brighter than its surroundings
    val width    = 1500
    val height   = 1000
    val image    = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val sun      = Circle(640d, 430d, 90d)
    val graphics = image.createGraphics
    try {
      var y = 0
      while (y < height) {
        // a bright sky, only a little darker than the subject : this is what makes a fixed
        // threshold useless, most of the frame sits above it
        val level = (225 - 60 * y / height).max(10)
        graphics.setColor(Color(level, level - 4, level + 6))
        graphics.drawLine(0, y, width, y)
        y += 1
      }
      graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      graphics.setColor(Color(20, 18, 16)) // a branch across the top of the frame
      graphics.fillRect(0, 0, width, 90)
      fill(graphics, sun, Color(252, 248, 240))
    } finally graphics.dispose()

    val found = DiscDetector.detect(image).fold(error => fail(error), identity)
    assertEqualsDouble(found.circle.centerX, sun.centerX, 3d)
    assertEqualsDouble(found.circle.centerY, sun.centerY, 3d)
    assertEqualsDouble(found.circle.radius, sun.radius, 4d)
  }

  test("the outer limb fit ignores the points lying inside the circle") {
    val truth  = Circle(120d, 90d, 60d)
    val limb   = (0 until 240).map { index =>
      val angle = math.Pi * index / 240d // half of the circle only
      Point(truth.centerX + 60d * math.cos(angle), truth.centerY + 60d * math.sin(angle))
    }
    val inside = (0 until 200).map { index =>
      val angle = math.Pi + math.Pi * index / 200d
      Point(truth.centerX + 25d * math.cos(angle), truth.centerY + 25d * math.sin(angle))
    }
    val (fitted, _) = CircleFitting
      .robustOuterFit(limb ++ inside, knownRadius = Some(60d))
      .getOrElse(fail("fit failed"))
    assertEqualsDouble(fitted.centerX, truth.centerX, 0.5d)
    assertEqualsDouble(fitted.centerY, truth.centerY, 0.5d)
  }
}
