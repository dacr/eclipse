package fr.janalyse.sotohp.media.imaging

import fr.janalyse.sotohp.media.imaging.CircleFitting.Circle

import java.awt.image.BufferedImage
import java.awt.{Color, RenderingHints}

/** Telling a visible limb from a corona is what decides whether a frame is a partial phase or a
  * totality one, and that decision drives the whole analysis : where it starts from, which frames
  * get their radius imposed, and what the composite finally shows.
  */
class LimbContrastTest extends munit.FunSuite {

  def frame(width: Int = 1200, height: Int = 900)(draw: java.awt.Graphics2D => Unit): BufferedImage = {
    val image    = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val graphics = image.createGraphics
    try {
      graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      graphics.setColor(Color(3, 2, 5))
      graphics.fillRect(0, 0, width, height)
      draw(graphics)
    } finally graphics.dispose()
    image
  }

  def fill(graphics: java.awt.Graphics2D, circle: Circle, color: Color): Unit = {
    graphics.setColor(color)
    graphics.fillOval(
      math.round(circle.centerX - circle.radius).toInt,
      math.round(circle.centerY - circle.radius).toInt,
      math.round(circle.radius * 2d).toInt,
      math.round(circle.radius * 2d).toInt
    )
  }

  test("a strongly exposed partial phase is not taken for a corona") {
    // an unfiltered partial phase : the disc is saturated, its limb is a clean step. Measuring the
    // step at the edge of the thresholded mask would land inside the saturated area and find
    // nothing, which used to turn such frames into totality ones.
    val sun   = Circle(600d, 450d, 180d)
    val image = frame() { graphics =>
      fill(graphics, sun, Color(255, 214, 40))
      fill(graphics, Circle(470d, 360d, 185d), Color(4, 3, 6))
    }
    val found = DiscDetector.detect(image).fold(error => fail(error), identity)
    assertEquals(found.kind, DiscDetector.DiscKind.Photosphere)
    assert(found.limbContrast > 0.6d, f"limb contrast ${found.limbContrast}%.2f, the limb is a clean step")
  }

  test("a corona has no limb to speak of") {
    val width  = 1200
    val height = 900
    val image  = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    var y      = 0
    while (y < height) {
      var x = 0
      while (x < width) {
        val distance = math.hypot(x - 620d, y - 470d)
        val level    = if (distance < 80d) 2d else math.max(2d, 120d * math.exp(-(distance - 80d) / 90d))
        val value    = math.min(255, level.toInt)
        image.setRGB(x, y, (value << 16) | (value << 8) | value)
        x += 1
      }
      y += 1
    }
    val found = DiscDetector.detect(image, DiscDetector.DiscDetectorConfig(expectedRadiusPixels = Some(80d)))
      .fold(error => fail(error), identity)
    assertEquals(found.kind, DiscDetector.DiscKind.Corona)
    assert(found.limbContrast < 0.3d, f"limb contrast ${found.limbContrast}%.2f, a corona fades away")
  }

  test("a filtered partial phase keeps a visible limb") {
    val sun   = Circle(700d, 500d, 200d)
    val image = frame(1600, 1200) { graphics =>
      fill(graphics, sun, Color(255, 245, 230))
      fill(graphics, Circle(790d, 430d, 205d), Color(6, 5, 8))
    }
    val found = DiscDetector.detect(image).fold(error => fail(error), identity)
    assertEquals(found.kind, DiscDetector.DiscKind.Photosphere)
    assert(found.limbContrast > 0.6d, f"limb contrast ${found.limbContrast}%.2f")
  }
}
