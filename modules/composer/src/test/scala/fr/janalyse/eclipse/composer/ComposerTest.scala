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

  test("no tile is drawn where the scenery already shows the sun") {
    val frames    = session()
    val plain     = SelectionConfig(separationFactor = 1.05d, tileRadiusFactor = 1.5d, totalityTileRadiusFactor = 3d)
    // the wide angle frame was taken at the very end of the session, so its sun stands where the
    // last frames of the sequence would have landed
    val landscape = frames.last.sun.get
    val config    = plain.copy(occupied = List(FrameSelector.OccupiedSky(landscape.apparent, landscape.semiDiameterDegrees)))

    val withScenery = FrameSelector.select(frames, config)
    assert(withScenery.rejectedForScenery > 0, "the frames landing on that sun should have been left out")
    assert(withScenery.keptCount < FrameSelector.select(frames, plain).keptCount, "one tile fewer at least")

    val required = (FrameSelector.tileRadiusDegrees(frames.last, config) +
      landscape.semiDiameterDegrees * config.tileRadiusFactor) * config.separationFactor
    withScenery.kept.foreach { frame =>
      val distance = frame.position.get.angularDistanceTo(landscape.apparent)
      assert(distance >= required * 0.999d, f"a tile lands $distance%.4f° from the sun of the scenery, $required%.4f° needed")
    }
  }

  test("without a scenery nothing is left out for it") {
    val selection = FrameSelector.select(session(), SelectionConfig())
    assertEquals(selection.rejectedForScenery, 0)
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

  test("the maximum of the eclipse is always part of the selection") {
    val frames    = session()
    val maximum   = FrameSelector.representativeMaximum(frames.filter(_.isUsable))
    val selection = FrameSelector.select(frames)
    assert(selection.kept.exists(_.path == maximum.path), "the frame standing for the maximum was dropped")
    // and it really is at the maximum, not merely somewhere in the selection
    assert(maximum.obscuration.exists(_ > 0.99d), s"${maximum.name} is not at the maximum of the eclipse")
  }

  test("the frame standing for the maximum is taken from the middle of totality") {
    // totality is a continuous stretch, and every one of its frames reads as fully obscured : the
    // first one is a diamond ring, the middle one is the corona
    // a real session shoots a burst during those two minutes, and its frames all read as fully
    // obscured - the crescents flanking totality do too, which is what makes the middle the only
    // safe pick
    val plain   = session()
    val middle  = plain.size / 2
    val frames  = plain.zipWithIndex.map { case (frame, index) =>
      if (index >= middle - 4 && index <= middle + 4)
        frame.copy(disc = frame.disc.map(_.copy(phase = FramePhase.Totality, obscuration = Some(1d))))
      else frame
    }
    val totality = frames.filter(_.phase == FramePhase.Totality).sortBy(_.instant.map(_.toEpochMilli).getOrElse(0L))
    assertEquals(totality.size, 9)
    val maximum  = FrameSelector.representativeMaximum(frames.filter(_.isUsable))
    assertEquals(maximum.path, totality(totality.size / 2).path)
  }

  test("a balanced selection keeps as many frames before the maximum as after it") {
    // a session is rarely symmetric : this one is cut short after the maximum, as a sunset would
    val frames  = session(240).take(160)
    val maximum = FrameSelector.representativeMaximum(frames.filter(_.isUsable)).instant.get

    val plain = FrameSelector.select(frames)
    val before = plain.kept.count(_.instant.exists(_.isBefore(maximum)))
    val after  = plain.kept.count(_.instant.exists(_.isAfter(maximum)))
    assert(before != after, s"this session should be lopsided, got $before before and $after after")

    val even = FrameSelector.select(frames, SelectionConfig(balanced = true))
    assertEquals(
      even.kept.count(_.instant.exists(_.isBefore(maximum))),
      even.kept.count(_.instant.exists(_.isAfter(maximum)))
    )
    assertEquals(even.keptCount, math.min(before, after) * 2 + 1)
  }

  test("balancing trims the frames furthest from the maximum, keeping the closest ones") {
    val frames  = session(240).take(160)
    val maximum = FrameSelector.representativeMaximum(frames.filter(_.isUsable)).instant.get
    val even    = FrameSelector.select(frames, SelectionConfig(balanced = true))
    val plain   = FrameSelector.select(frames)

    // whatever is dropped sits at the ends, never in the middle of the sequence
    val keptInstants = even.kept.flatMap(_.instant)
    val span         = (keptInstants.min, keptInstants.max)
    plain.kept.flatMap(_.instant).filterNot(keptInstants.contains).foreach { dropped =>
      assert(dropped.isBefore(span._1) || dropped.isAfter(span._2), s"$dropped was dropped from the middle")
    }
    assert(even.kept.exists(_.instant.contains(maximum)), "the maximum itself must stay")
  }

  test("the number of frames per side can be capped outright") {
    val frames = session()
    val capped = FrameSelector.select(frames, SelectionConfig(framesPerSide = Some(3)))
    assertEquals(capped.keptCount, 7) // three on each side, plus the maximum
  }

  test("a totality burst is handed over whole to its placement") {
    // the burst : several exposures within seconds of one another, as a real totality is shot
    val plain  = session()
    val middle = plain.size / 2
    val frames = plain.zipWithIndex.map { case (frame, index) =>
      if (index >= middle - 3 && index <= middle + 3)
        // a real burst brackets its exposures : that is the whole point of shooting one
        frame.copy(
          disc = frame.disc.map(_.copy(phase = FramePhase.Totality, obscuration = Some(1d))),
          metadata = frame.metadata.copy(
            exposureTimeSeconds = Some(0.004d * math.pow(2d, (index - middle + 3).toDouble)),
            aperture = Some(8d),
            isoSensitivity = Some(100d)
          )
        )
      else frame
    }
    val selection  = FrameSelector.select(frames)
    val placements = SkyPathLayout().place(selection.kept, LayoutConfig(pixelsPerDegree = 300d))
    val enriched   = EclipseComposer.withTotalityBursts(placements, frames, withinSeconds = 300L)

    val totalityPlacement = enriched.find(_.frame.phase == FramePhase.Totality)
    assert(totalityPlacement.isDefined, "the maximum should have been placed")
    assertEquals(totalityPlacement.get.stack.size, 7)
    // and the partial phases are left alone : there is nothing to merge there
    assert(enriched.filter(_.frame.phase != FramePhase.Totality).forall(_.stack.isEmpty))
  }

  test("only one frame per exposure level is merged, the closest in time") {
    // a real bracket : repeats at the same settings, and two ways of reaching the same exposure
    val start = LocalDateTime.of(2026, 8, 12, 18, 30, 0).toInstant(ZoneOffset.UTC)
    def shot(seconds: Long, time: Double, aperture: Double) = FrameAnalysis(
      path = Paths.get(f"IMG_$seconds%04d.CR3"),
      metadata = ShotMetadata.empty.copy(
        shotAt = Some(start.plusSeconds(seconds).atOffset(ZoneOffset.UTC)),
        exposureTimeSeconds = Some(time),
        aperture = Some(aperture),
        isoSensitivity = Some(100d)
      ),
      sun = None,
      disc = None
    )

    val burst = List(
      shot(0, 0.4d, 8d),      // three repeats of the long exposure
      shot(4, 0.4d, 8d),
      shot(8, 0.4d, 8d),
      shot(20, 0.008d, 8d),   // the short one
      shot(24, 0.05d, 2.8d)   // same exposure as 0.4s at f/8, by another route
    )
    val reference = start.plusSeconds(8)
    val chosen    = EclipseComposer.oneFramePerExposure(burst, reference)

    // two levels only : the long one - whichever aperture reached it - and the short one
    assertEquals(chosen.size, 2)
    val exposures = chosen.flatMap(_.metadata.exposureValue).map(value => math.round(value * 3d)).distinct
    assertEquals(exposures.size, 2)
    // and among the repeats, the one taken closest to the reference instant
    assert(chosen.exists(_.name == "IMG_0008.CR3"), chosen.map(_.name).mkString(", "))
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
