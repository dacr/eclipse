package fr.janalyse.eclipse.composer

import fr.janalyse.eclipse.astro.SolarEphemeris
import fr.janalyse.eclipse.model.*

import java.nio.file.Paths
import java.time.{Duration, LocalDateTime, ZoneOffset}

class AutoTunerTest extends munit.FunSuite {

  val observer = GeoPoint(43.6047d, 1.4442d, 150d)

  /** A session shot at 200mm on a 45Mpix full frame body : 795 px/°, so a 420 px solar disc */
  def session(
    frameCount: Int = 200,
    plateScale: Double = 795d,
    imageWidth: Int = 8192,
    imageHeight: Int = 5464,
    totalityFrames: Int = 0,
    coronaExtentFactor: Double = 2.2d
  ): List[FrameAnalysis] = {
    val start = LocalDateTime.of(2026, 8, 12, 16, 30, 0).toInstant(ZoneOffset.UTC)
    (0 until frameCount).toList.map { index =>
      val instant     = start.plus(Duration.ofSeconds(30L * index))
      val sun         = SolarEphemeris.position(instant, observer)
      val radius      = sun.semiDiameterDegrees * plateScale
      val middle      = frameCount / 2
      val isTotality  = totalityFrames > 0 && math.abs(index - middle) < totalityFrames / 2
      val ratio       = index.toDouble / (frameCount - 1)
      val obscuration = if (isTotality) 1d else 0.98d * (1d - math.abs(ratio - 0.5d) * 2d)
      // the sun drifts through the frame, sometimes coming as close as 1.8 radius from a border
      val centerX     = 900d + (index % 40) * 60d
      val centerY     = 700d + (index % 25) * 40d
      FrameAnalysis(
        path = Paths.get(f"IMG_$index%04d.CR3"),
        metadata = ShotMetadata.empty.copy(
          shotAt = Some(instant.atOffset(ZoneOffset.UTC)),
          location = Some(observer),
          imageWidth = Some(imageWidth),
          imageHeight = Some(imageHeight)
        ),
        sun = Some(sun),
        disc = Some(
          MeasuredDisc(
            centerX = centerX,
            centerY = centerY,
            radiusPixels = radius,
            phase = if (isTotality) FramePhase.Totality else FramePhase.Partial,
            obscuration = Some(obscuration),
            fitResidualPixels = 0.4d,
            detectionConfidence = 0.9d,
            limbContrast = if (isTotality) 0.1d else 0.8d,
            signalRadiusPixels = if (isTotality) Some(radius * coronaExtentFactor) else None,
            roomFactor = Some(List(centerX, centerY, imageWidth - centerX, imageHeight - centerY).min / radius)
          )
        )
      )
    }
  }

  test("the plate scale is recovered from the measurements") {
    val tuning = AutoTuner.tune(session())
    assertEqualsDouble(tuning.plateScale.get.pixelsPerDegree, 795d, 1d)
    assert(tuning.explanations.exists(_.contains("plate scale")), tuning.explanations.mkString("\n"))
  }

  test("the tile size stays within what the frames can give") {
    val frames = session()
    val tuning = AutoTuner.tune(frames)
    val worstRoom = frames.flatMap(_.disc).flatMap(_.roomFactor).min
    assert(
      tuning.render.tileRadiusFactor <= math.max(1.15d, worstRoom) + 0.001d,
      s"tile factor ${tuning.render.tileRadiusFactor} exceeds the available room $worstRoom"
    )
    assert(tuning.render.tileRadiusFactor >= 1.15d)
  }

  test("the totality tile is sized on the corona actually recorded") {
    val tuning = AutoTuner.tune(session(totalityFrames = 8, coronaExtentFactor = 2.2d))
    assertEqualsDouble(tuning.render.totalityTileRadiusFactor, 2.42d, 0.3d)
    assert(tuning.render.totalityTileRadiusFactor > tuning.render.tileRadiusFactor)
  }

  test("the output scale never exceeds the sensor resolution") {
    // a short session : the whole path fits easily, so nothing forces a reduction
    val tuning = AutoTuner.tune(session(frameCount = 30))
    val discRadius = tuning.render.discRadiusPixels
    val measured   = session(frameCount = 30).head.disc.get.radiusPixels
    assert(discRadius <= measured * 1.01d, s"$discRadius should not exceed the measured $measured")
  }

  test("a long session gets scaled down to stay within the requested size") {
    val frames = session(frameCount = 330) // almost three hours
    val tuning = AutoTuner.tune(frames, AutoTuner.TuningIntent(maximumCanvasSide = 12000))
    val config = tuning.toComposeConfig(Paths.get("."))
    val placements = tuning.layout.place(
      FrameSelector.select(frames, tuning.selection).kept,
      LayoutConfig(EclipseComposer.pixelsPerDegree(frames, config), tuning.render.tileRadiusFactor, tuning.render.totalityTileRadiusFactor)
    )
    val width = placements.map(p => p.x + p.tileRadiusPixels).max - placements.map(p => p.x - p.tileRadiusPixels).min
    assert(width <= 12000d, f"$width%.0f px wide, 12000 px asked for")
    assert(width > 6000d, f"$width%.0f px wide, the composite should still use the room it was given")
    assert(tuning.explanations.exists(_.contains("resolution")), tuning.explanations.mkString("\n"))
  }

  test("the selection made by the tuner never lets two discs touch") {
    val frames    = session(totalityFrames = 6)
    val tuning    = AutoTuner.tune(frames)
    val selection = FrameSelector.select(frames, tuning.selection)
    selection.kept.sliding(2).foreach { case List(first, second) =>
      val distance = first.position.get.angularDistanceTo(second.position.get)
      val required = FrameSelector.tileRadiusDegrees(first, tuning.selection) + FrameSelector.tileRadiusDegrees(second, tuning.selection)
      assert(distance >= required * 0.999d, f"$distance%.4f° apart, $required%.4f° needed")
    }
  }

  test("a session without any usable frame falls back on the defaults") {
    val tuning = AutoTuner.tune(Nil)
    assertEquals(tuning.render.discRadiusPixels, RenderConfig().discRadiusPixels)
    assert(tuning.explanations.exists(_.contains("no usable frame")))
  }
}
