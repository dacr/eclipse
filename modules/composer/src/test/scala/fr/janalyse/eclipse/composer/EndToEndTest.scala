package fr.janalyse.eclipse.composer

import fr.janalyse.eclipse.astro.SolarEphemeris
import fr.janalyse.eclipse.frames.{FrameAnalysisConfig, FrameAnalyzer}
import fr.janalyse.eclipse.model.*
import fr.janalyse.sotohp.media.imaging.BasicImaging

import java.awt.image.BufferedImage
import java.awt.{Color, RenderingHints}
import java.nio.file.{Files, Path}
import java.time.{Duration, LocalDateTime, ZoneOffset}
import scala.util.Random

/** Whole pipeline check, on a synthetic session which mimics the real shooting conditions : a fixed
  * tripod (so the sun drifts through the frame), a few re-framings, one shot every 30 seconds, and
  * an eclipse going from first contact to totality and back.
  */
class EndToEndTest extends munit.FunSuite {

  val observer   = GeoPoint(43.6047d, 1.4442d, 150d)
  val start      = LocalDateTime.of(2026, 8, 12, 16, 30, 0).toInstant(ZoneOffset.UTC)
  val frameWidth = 1200
  val frameHeight = 800
  val sunRadius  = 70d

  /** Draws one frame of the session and returns where the solar disc really is */
  def drawFrame(directory: Path, index: Int, obscuration: Double, random: Random): (Path, (Double, Double)) = {
    // the sun drifts because nothing tracks it, and jumps whenever the frame gets re-centered
    val drift    = index * 3.5d
    val reframe  = if (index > 25) -80d else 0d
    val centerX  = 300d + drift + reframe + random.nextDouble() * 2d
    val centerY  = 260d + index * 1.7d + random.nextDouble() * 2d
    val image    = BufferedImage(frameWidth, frameHeight, BufferedImage.TYPE_INT_RGB)
    val graphics = image.createGraphics
    try {
      graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      graphics.setColor(Color(3, 2, 5))
      graphics.fillRect(0, 0, frameWidth, frameHeight)
      // the solar filter gives a strong color cast, the composer is supposed to cancel it
      // a filtered photosphere is coloured but not clipped : leaving a channel at 255 would give
      // the brightness normalization no room to work with, which no real frame does
      graphics.setColor(Color(220, 138, 78))
      graphics.fillOval(
        (centerX - sunRadius).toInt,
        (centerY - sunRadius).toInt,
        (sunRadius * 2).toInt,
        (sunRadius * 2).toInt
      )
      // the moon comes from the upper left, the distance between centers gives the obscuration
      val separation = (1d - obscuration) * 2d * sunRadius
      graphics.setColor(Color(4, 3, 6))
      graphics.fillOval(
        (centerX - separation * 0.8d - sunRadius).toInt,
        (centerY - separation * 0.6d - sunRadius).toInt,
        (sunRadius * 2).toInt,
        (sunRadius * 2).toInt
      )
      val path = directory.resolve(f"frame_$index%04d.png")
      BasicImaging.save(path, image)
      (path, (centerX, centerY))
    } finally graphics.dispose()
  }

  test("a whole session goes from files to a composite without overlapping discs") {
    val directory = Files.createTempDirectory("eclipse-session-")
    val cache     = directory.resolve("cache")
    try {
      val random = Random(42)
      val count  = 45
      val truths = (0 until count).map { index =>
        // obscuration grows up to 0.85 in the middle of the session, the photosphere always shows
        val ratio       = index.toDouble / (count - 1)
        val obscuration = 0.85d * (1d - math.abs(ratio - 0.5d) * 2d)
        val (path, center) = drawFrame(directory, index, obscuration, random)
        (index, path, center)
      }

      // --- measurement --------------------------------------------------------------------------
      val config = FrameAnalysisConfig(cacheDirectory = cache, observer = Some(observer), parallelism = 2)
      val frames = truths.toList.map { case (index, path, (centerX, centerY)) =>
        val measured = FrameAnalyzer.analyze(path, config)
        val disc     = measured.disc.getOrElse(fail(s"no disc found on $path : ${measured.issues.mkString(", ")}"))
        // the detected center must be the center of the solar disc, not the center of the crescent
        assertEqualsDouble(disc.centerX, centerX, 4d)
        assertEqualsDouble(disc.centerY, centerY, 4d)
        assertEqualsDouble(disc.radiusPixels, sunRadius, 4d)

        val instant = start.plus(Duration.ofSeconds(30L * index))
        measured.copy(
          metadata = measured.metadata.copy(shotAt = Some(instant.atOffset(ZoneOffset.UTC)), location = Some(observer)),
          sun = Some(SolarEphemeris.position(instant, observer))
        )
      }

      assert(frames.forall(_.isUsable), "every frame should be usable")
      assert(frames.flatMap(_.obscuration).max > 0.7d, "the eclipse should be well advanced in the middle of the session")

      // --- composition --------------------------------------------------------------------------
      val outcome = EclipseComposer
        .compose(
          frames,
          ComposeConfig(
            cacheDirectory = cache,
            render = RenderConfig(discRadiusPixels = 40d, marginPixels = 20)
          )
        )
        .fold(error => fail(error), identity)

      val report = outcome.result.report
      assert(report.drawnFrameCount == outcome.selection.keptCount, s"${report.drawnFrameCount} drawn, ${outcome.selection.keptCount} selected")
      assert(report.smallestGapPixels >= 0d, f"discs overlap by ${-report.smallestGapPixels}%.1f px")
      assert(report.warnings.isEmpty, report.warnings.mkString(", "))
      assertEquals(outcome.result.image.getWidth, report.width)

      // every placed frame must show its lit crescent on the composite - not necessarily at the
      // center of the tile, which is covered by the moon as soon as the eclipse is well advanced
      val composite = outcome.result.image
      outcome.result.placements.zipWithIndex.foreach { case (placement, index) =>
        val brightest = brightestWithin(composite, placement.x, placement.y, placement.discRadiusPixels)
        assert(brightest > 0.3d, f"placement $index shows nothing, brightest sample $brightest%.3f")
      }

      // and the sky between two neighbour tiles must have stayed untouched
      outcome.result.placements.sliding(2).foreach { case Seq(first, second) =>
        val gap = math.hypot(second.x - first.x, second.y - first.y) - (first.tileRadiusPixels + second.tileRadiusPixels)
        if (gap > 8d) {
          val middleX = (first.x + second.x) / 2d
          val middleY = (first.y + second.y) / 2d
          assert(
            luminance(composite, math.round(middleX).toInt, math.round(middleY).toInt) < 0.05d,
            "the sky between two frames should have stayed black"
          )
        }
      }
    } finally {
      Files.walk(directory).sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
    }
  }

  test("a thin crescent is drawn as bright as a full disc, not brighter") {
    val directory = Files.createTempDirectory("eclipse-levels-")
    val cache     = directory.resolve("cache")
    try {
      val random = Random(7)
      // two frames an hour apart, so that both survive the selection whatever the spacing rule
      val frames = List(0d, 0.95d).zipWithIndex.map { case (obscuration, index) =>
        val (path, _) = drawFrame(directory, index * 3, obscuration, random)
        val instant   = start.plus(Duration.ofSeconds(3600L * index))
        val measured  = FrameAnalyzer.analyze(path, FrameAnalysisConfig(cacheDirectory = cache, observer = Some(observer)))
        measured.copy(
          metadata = measured.metadata.copy(shotAt = Some(instant.atOffset(ZoneOffset.UTC)), location = Some(observer)),
          sun = Some(SolarEphemeris.position(instant, observer))
        )
      }

      val outcome = EclipseComposer
        .compose(frames, ComposeConfig(cacheDirectory = cache, render = RenderConfig(discRadiusPixels = 60d)))
        .fold(error => fail(error), identity)

      assertEquals(outcome.result.placements.size, 2)
      val levels = outcome.result.placements.map { placement =>
        brightestWithin(outcome.result.image, placement.x, placement.y, placement.discRadiusPixels)
      }
      assertEqualsDouble(levels(1), levels(0), 0.12d)
    } finally {
      Files.walk(directory).sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
    }
  }

  private def brightestWithin(image: BufferedImage, centerX: Double, centerY: Double, radius: Double): Double = {
    var brightest = 0d
    var angle     = 0d
    while (angle < 2 * math.Pi) {
      var distance = 0d
      while (distance <= radius) {
        val level = luminance(image, math.round(centerX + math.cos(angle) * distance).toInt, math.round(centerY + math.sin(angle) * distance).toInt)
        if (level > brightest) brightest = level
        distance += 2d
      }
      angle += math.Pi / 60d
    }
    brightest
  }

  private def luminance(image: BufferedImage, x: Int, y: Int): Double = {
    val rgb = image.getRGB(math.max(0, math.min(image.getWidth - 1, x)), math.max(0, math.min(image.getHeight - 1, y)))
    (((rgb >> 16) & 0xff) * 0.2126d + ((rgb >> 8) & 0xff) * 0.7152d + (rgb & 0xff) * 0.0722d) / 255d
  }
}
