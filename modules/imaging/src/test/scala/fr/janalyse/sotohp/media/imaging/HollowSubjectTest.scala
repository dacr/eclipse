package fr.janalyse.sotohp.media.imaging

import java.awt.image.BufferedImage

/** Telling totality from a low sun.
  *
  * Both show a soft edge - a sun a couple of degrees above the horizon crosses so much atmosphere
  * that its limb genuinely blurs - so softness alone cannot decide. What only totality shows is a
  * dark middle : the moon. A whole hour of low sun frames of a real session was being taken for
  * totality until that was looked at.
  */
class HollowSubjectTest extends munit.FunSuite {

  /** A disc whose edge fades over `blur` pixels, optionally hollowed out by a dark middle */
  def disc(centerX: Double, centerY: Double, radius: Double, blur: Double, hollow: Boolean): BufferedImage = {
    val width  = 1200
    val height = 900
    val image  = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    var y      = 0
    while (y < height) {
      var x = 0
      while (x < width) {
        val distance = math.hypot(x - centerX, y - centerY)
        val edge     = math.max(0d, math.min(1d, (radius + blur - distance) / (2d * blur)))
        val middle   = if (hollow && distance < radius * 0.75d) 0.02d else 1d
        val level    = math.min(255, (235d * edge * middle + 4d).toInt)
        image.setRGB(x, y, (level << 16) | (level << 8) | level)
        x += 1
      }
      y += 1
    }
    image
  }

  test("a low sun with a blurred limb stays a partial phase") {
    val image = disc(600d, 450d, 170d, blur = 25d, hollow = false)
    val found = DiscDetector.detect(image).fold(error => fail(error), identity)
    assertEquals(found.kind, DiscDetector.DiscKind.Photosphere)
    assertEqualsDouble(found.circle.radius, 170d, 12d)
    // its edge really is soft, which is why softness alone could not have decided
    assert(found.limbContrast < 0.6d, f"limb sharpness ${found.limbContrast}%.2f should be low")
  }

  test("a ring of light around a dark middle is totality") {
    val image = disc(600d, 450d, 170d, blur = 25d, hollow = true)
    val found = DiscDetector.detect(image, DiscDetector.DiscDetectorConfig(expectedRadiusPixels = Some(170d)))
      .fold(error => fail(error), identity)
    assertEquals(found.kind, DiscDetector.DiscKind.Corona)
    assertEqualsDouble(found.circle.centerX, 600d, 10d)
    assertEqualsDouble(found.circle.centerY, 450d, 10d)
  }

  test("a sharp limb is a partial phase whatever its middle looks like") {
    val image = disc(600d, 450d, 170d, blur = 1d, hollow = false)
    val found = DiscDetector.detect(image).fold(error => fail(error), identity)
    assertEquals(found.kind, DiscDetector.DiscKind.Photosphere)
    assert(found.limbContrast > 0.6d, f"limb sharpness ${found.limbContrast}%.2f should be high")
  }
}
