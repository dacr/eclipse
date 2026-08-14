package fr.janalyse.eclipse.composer

import fr.janalyse.eclipse.astro.SolarEphemeris
import fr.janalyse.eclipse.composer.FrameSelector.SelectionConfig
import fr.janalyse.eclipse.model.*

import java.nio.file.{Files, Paths}
import java.time.{Duration, Instant, LocalDateTime, ZoneOffset}

class ComposerTest extends munit.FunSuite {

  val observer = GeoPoint(43.6047d, 1.4442d, 150d) // Toulouse

  /** A session shot every 30 seconds, the way the real one was */
  def session(frameCount: Int = 240, cadence: Duration = Duration.ofSeconds(30)): List[FrameAnalysis] = {
    val start = LocalDateTime.of(2026, 8, 12, 16, 30, 0).toInstant(ZoneOffset.UTC)
    (0 until frameCount).toList.map { index =>
      val instant = start.plus(cadence.multipliedBy(index.toLong))
      val sun     = SolarEphemeris.position(instant, observer)
      // obscuration growing then decreasing, maximum in the middle of the session
      val ratio       = index.toDouble / (frameCount - 1)
      val obscuration = 1d - math.abs(ratio - 0.5d) * 2d
      FrameAnalysis(
        path = Paths.get(f"IMG_$index%04d.CR3"),
        metadata = ShotMetadata.empty.copy(shotAt = Some(instant.atOffset(ZoneOffset.UTC)), location = Some(observer)),
        sun = Some(sun),
        disc = Some(
          MeasuredDisc(
            centerX = 4096d,
            centerY = 2732d,
            radiusPixels = sun.semiDiameterDegrees * 800d,
            phase = if (obscuration > 0.995d) FramePhase.Totality else FramePhase.Partial,
            obscuration = Some(obscuration),
            fitResidualPixels = 0.4d,
            detectionConfidence = 0.9d
          )
        )
      )
    }
  }

  test("a 30 seconds cadence gives frames far too close to each other to be drawn as they are") {
    val frames    = session()
    val positions = frames.flatMap(_.position)
    val step      = positions.sliding(2).collect { case Seq(a, b) => a.angularDistanceTo(b) }.max
    val diameter  = frames.head.sun.get.diameterDegrees
    assert(step < diameter / 4d, s"the sun moves $step° between two shots, way less than its $diameter° diameter")
  }

  test("selection keeps the discs apart") {
    val frames    = session()
    val config    = SelectionConfig(separationFactor = 1.05d, tileRadiusFactor = 1.5d, totalityTileRadiusFactor = 3d)
    val selection = FrameSelector.select(frames, config)

    assert(selection.keptCount > 3, s"only ${selection.keptCount} frames kept")
    assert(selection.keptCount < frames.size / 4, s"${selection.keptCount} frames kept, the selection did nothing")

    selection.kept.sliding(2).foreach { case List(first, second) =>
      val distance = first.position.get.angularDistanceTo(second.position.get)
      val required = FrameSelector.tileRadiusDegrees(first, config) + FrameSelector.tileRadiusDegrees(second, config)
      assert(distance >= required * 0.999d, f"$distance%.4f° between two kept frames, $required%.4f° needed")
    }
  }

  test("the most eclipsed frame is always part of the selection") {
    val frames       = session()
    val mostEclipsed = frames.maxBy(_.obscuration.getOrElse(0d))
    val selection    = FrameSelector.select(frames)
    assert(selection.kept.exists(_.path == mostEclipsed.path))
  }

  test("selection by obscuration step spreads the eclipse progress evenly") {
    val kept = FrameSelector.selectByObscurationStep(session(), step = 0.1d)
    assert(kept.sizeIs >= 10, s"only ${kept.size} frames kept")
    kept.sliding(2).foreach { case List(first, second) =>
      val step = math.abs(second.obscuration.get - first.obscuration.get)
      assert(step >= 0.09d, f"$step%.3f step between two kept frames")
    }
  }

  test("the sky path layout draws tiles which never touch each other") {
    val frames     = session()
    val selection  = FrameSelector.select(frames)
    val placements = SkyPathLayout().place(selection.kept, LayoutConfig(pixelsPerDegree = 90d / 0.266d))
    assertEquals(placements.size, selection.keptCount)
    placements.sliding(2).foreach { case Seq(first, second) =>
      val distance = math.hypot(second.x - first.x, second.y - first.y)
      assert(
        distance >= first.tileRadiusPixels + second.tileRadiusPixels,
        f"tiles overlap : $distance%.1f px between centers, ${first.tileRadiusPixels + second.tileRadiusPixels}%.1f px needed"
      )
    }
  }

  test("the even spacing variant keeps the frames in order and evenly spread") {
    val frames     = session()
    val selection  = FrameSelector.select(frames)
    val placements = SkyPathLayout(evenSpacing = true).place(selection.kept, LayoutConfig(pixelsPerDegree = 90d / 0.266d))
    val steps      = placements.sliding(2).collect { case Seq(first, second) =>
      math.hypot(second.x - first.x, second.y - first.y)
    }.toList
    assertEqualsDouble(steps.max / steps.min, 1d, 0.05d)
  }

  test("the grid layout lines up the frames without overlap") {
    val frames     = session().take(20)
    val placements = GridLayout(columns = Some(5)).place(frames, LayoutConfig(pixelsPerDegree = 300d))
    assertEquals(placements.size, 20)
    val distinctRows = placements.map(_.y).distinct
    assertEquals(distinctRows.size, 4)
  }

  test("measurements survive a save and load round trip") {
    val frames = session(10)
    val file   = Files.createTempFile("eclipse-measurements-", ".csv")
    try {
      FrameStore.save(file, frames)
      val loaded = FrameStore.load(file).fold(error => fail(error), identity)
      assertEquals(loaded.size, frames.size)
      loaded.zip(frames).foreach { case (read, original) =>
        assertEquals(read.path, original.path)
        assertEquals(read.shotAt, original.shotAt)
        assertEquals(read.phase, original.phase)
        assertEqualsDouble(read.disc.get.radiusPixels, original.disc.get.radiusPixels, 1e-9d)
        assertEqualsDouble(read.obscuration.get, original.obscuration.get, 1e-9d)
        // sun positions are recomputed on load, they must match the original ones
        assertEqualsDouble(read.position.get.azimuthDegrees, original.position.get.azimuthDegrees, 1e-9d)
      }
    } finally Files.deleteIfExists(file)
  }

  test("frames without any GPS fix inherit the position of the session") {
    // the receiver had not locked yet at the beginning, and dropped its fix later on
    val frames = session(30).zipWithIndex.map { case (frame, index) =>
      if (index < 8 || (index > 20 && index < 24)) frame.copy(metadata = frame.metadata.copy(location = None))
      else frame
    }
    val file   = Files.createTempFile("eclipse-partial-gps-", ".csv")
    try {
      FrameStore.save(file, frames)
      val (loaded, consolidation) = FrameStore.loadSession(file).fold(error => fail(error), identity)

      assertEquals(consolidation.fixCount, 19)
      assertEquals(consolidation.missingCount, 11)
      assert(loaded.forall(_.isUsable), s"${loaded.count(!_.isUsable)} frames stayed unusable")
      assert(loaded.forall(_.metadata.location.contains(observer)), "every frame should share the session position")
      // and the composition works on the whole session, not only on the frames that had a fix
      assertEquals(FrameSelector.select(loaded).candidateCount, 30)
    } finally Files.deleteIfExists(file)
  }

  test("a session with no GPS at all can still be placed from the given position") {
    val frames = session(10).map(frame => frame.copy(metadata = frame.metadata.copy(location = None)))
    val file   = Files.createTempFile("eclipse-no-gps-", ".csv")
    try {
      FrameStore.save(file, frames)
      val withoutPosition = FrameStore.loadSession(file).fold(error => fail(error), identity)
      assert(withoutPosition._1.forall(!_.isUsable), "without any position nothing can be placed")
      assert(withoutPosition._2.location.isEmpty)

      val withPosition = FrameStore.loadSession(file, Some(observer)).fold(error => fail(error), identity)
      assert(withPosition._1.forall(_.isUsable), "the given position should rescue the whole session")
      assert(withPosition._2.fromFallback)
    } finally Files.deleteIfExists(file)
  }

  test("the session summary reports what matters") {
    val summary = EclipseComposer.summary(session())
    assert(summary.contains("frames"), summary)
    assert(summary.contains("median cadence"), summary)
    assert(summary.contains("sky path length"), summary)
    assert(summary.contains("maximum obscuration"), summary)
  }
}
