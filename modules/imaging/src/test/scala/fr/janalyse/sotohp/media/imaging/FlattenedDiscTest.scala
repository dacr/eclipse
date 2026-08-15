package fr.janalyse.sotohp.media.imaging

import fr.janalyse.sotohp.media.imaging.CircleFitting.{Circle, Point}
import fr.janalyse.sotohp.media.imaging.EllipseFitting.Ellipse

import java.awt.image.BufferedImage
import java.awt.{Color, RenderingHints}

/** The sun near the horizon : squashed by refraction, and still to be measured */
class FlattenedDiscTest extends munit.FunSuite {

  private def outlineOf(ellipse: Ellipse, count: Int, noise: Double = 0d): Seq[Point] = {
    val random = scala.util.Random(42)
    (0 until count).map { index =>
      val angle = 2d * math.Pi * index / count
      val jitter = if (noise == 0d) 0d else (random.nextDouble() - 0.5d) * 2d * noise
      Point(
        ellipse.centerX + (ellipse.semiHorizontal + jitter) * math.cos(angle),
        ellipse.centerY + (ellipse.semiVertical + jitter) * math.sin(angle)
      )
    }
  }

  private def frameWith(
    width: Int = 1600,
    height: Int = 1200,
    ellipse: Ellipse = Ellipse(700d, 500d, 200d, 188d)
  ): BufferedImage = {
    val image    = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val graphics = image.createGraphics
    try {
      graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      graphics.setColor(Color(4, 3, 6))
      graphics.fillRect(0, 0, width, height)
      graphics.setColor(Color(255, 240, 210))
      graphics.fillOval(
        math.round(ellipse.centerX - ellipse.semiHorizontal).toInt,
        math.round(ellipse.centerY - ellipse.semiVertical).toInt,
        math.round(ellipse.semiHorizontal * 2d).toInt,
        math.round(ellipse.semiVertical * 2d).toInt
      )
      image
    } finally graphics.dispose()
  }

  test("an ellipse is recovered from points lying on it") {
    val truth = Ellipse(730.5d, 480.25d, 201.4d, 187.9d)
    val found = EllipseFitting.fit(outlineOf(truth, 200)).getOrElse(fail("nothing fitted"))
    assertEqualsDouble(found.centerX, truth.centerX, 0.05d)
    assertEqualsDouble(found.centerY, truth.centerY, 0.05d)
    assertEqualsDouble(found.semiHorizontal, truth.semiHorizontal, 0.05d)
    assertEqualsDouble(found.semiVertical, truth.semiVertical, 0.05d)
    assertEqualsDouble(found.flattening, truth.flattening, 0.001d)
  }

  test("a circle is fitted as a circle, without inventing a flattening") {
    val truth = Ellipse(700d, 500d, 200d, 200d)
    val found = EllipseFitting.fit(outlineOf(truth, 200, noise = 1d)).getOrElse(fail("nothing fitted"))
    assert(math.abs(found.flattening) < 0.01d, f"flattening ${found.flattening}%.4f should be zero")
  }

  test("the robust fit settles on the outer outline, ignoring the points inside it") {
    val truth   = Ellipse(700d, 500d, 200d, 186d)
    val limb    = outlineOf(truth, 300)
    val intruder = outlineOf(Ellipse(720d, 490d, 100d, 100d), 120) // an occulting body limb, well inside
    val seed    = Ellipse(truth.centerX + 8d, truth.centerY - 6d, 195d, 195d)
    val (found, kept) = EllipseFitting
      .robustOuterFit(scala.util.Random.shuffle(limb ++ intruder), seed)
      .getOrElse(fail("nothing fitted"))
    assertEqualsDouble(found.centerX, truth.centerX, 2d)
    assertEqualsDouble(found.centerY, truth.centerY, 2d)
    assertEqualsDouble(found.flattening, truth.flattening, 0.01d)
    assert(kept.sizeIs < limb.size + intruder.size, "the inner points should have been dropped")
  }

  test("a squashed sun is measured as squashed, and its horizontal size stays the true one") {
    val truth = Ellipse(700d, 500d, 200d, 188d) // 6 % flattening, what two degrees of altitude give
    val found = DiscDetector.detect(frameWith(ellipse = truth)).fold(error => fail(error), identity)
    assertEquals(found.kind, DiscDetector.DiscKind.Photosphere)
    assertEqualsDouble(found.circle.centerX, truth.centerX, 2d)
    assertEqualsDouble(found.circle.centerY, truth.centerY, 2d)
    assertEqualsDouble(found.circle.radius, truth.semiHorizontal, 3d)
    assertEqualsDouble(found.flattening, truth.flattening, 0.015d)
  }

  test("measuring a squashed sun as a circle is what condemned it, the ellipse clears it") {
    val truth  = Ellipse(700d, 500d, 200d, 188d)
    val image  = frameWith(ellipse = truth)
    val asCircle = DiscDetector
      .detect(image, DiscDetector.DiscDetectorConfig(maximumFlattening = 0d))
      .fold(error => fail(error), identity)
    val asEllipse = DiscDetector.detect(image).fold(error => fail(error), identity)

    // A circle cannot pass through a squashed outline, so it does the only other thing it can : it
    // discards the top and the bottom of the limb as if they belonged to an occulting body, and
    // keeps the horizontal band it can fit. That is what collapses the confidence of the frame -
    // it is computed from the share of the limb the fit could keep - and what emptied the end of a
    // real session. The ellipse keeps the whole outline instead.
    def kept(detection: DiscDetector.DiscDetection) =
      detection.inlierCount.toDouble / detection.boundaryPointCount

    assert(kept(asCircle) < 0.75d, f"a circle should have to drop the squashed limb, kept ${kept(asCircle)}%.2f")
    assert(kept(asEllipse) > 0.95d, f"the ellipse should keep the whole limb, kept ${kept(asEllipse)}%.2f")
    assertEqualsDouble(asEllipse.flattening, truth.flattening, 0.015d)
  }

  test("a round sun is left round, the extra parameter is not spent for nothing") {
    val truth = Circle(700d, 500d, 200d)
    val found = DiscDetector
      .detect(frameWith(ellipse = Ellipse(truth.centerX, truth.centerY, truth.radius, truth.radius)))
      .fold(error => fail(error), identity)
    assertEqualsDouble(found.flattening, 0d, 0.001d)
    assertEqualsDouble(found.circle.radius, truth.radius, 2d)
  }
}
