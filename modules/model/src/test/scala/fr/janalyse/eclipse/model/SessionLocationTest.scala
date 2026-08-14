package fr.janalyse.eclipse.model

class SessionLocationTest extends munit.FunSuite {

  val place = GeoPoint(43.6047d, 1.4442d, 150d)

  /** A fix wandering by a few meters around the real position, as a receiver always does */
  def wandering(index: Int): GeoPoint = {
    val jitter = math.sin(index.toDouble) * 0.00005d // about 5 m
    GeoPoint(place.latitudeDegrees + jitter, place.longitudeDegrees - jitter, place.altitudeMeters + jitter * 100000d)
  }

  def session(withFix: Int, withoutFix: Int): List[ShotMetadata] =
    (0 until withFix).toList.map(index => ShotMetadata.empty.copy(location = Some(wandering(index)))) ++
      List.fill(withoutFix)(ShotMetadata.empty)

  test("the position of the session is the median of the fixes") {
    val consolidated = SessionLocation.consolidate(session(withFix = 50, withoutFix = 0))
    val found        = consolidated.location.getOrElse(fail("no position"))
    assert(SessionLocation.distanceMeters(found, place) < 6d, s"$found is too far from $place")
    assertEquals(consolidated.fixCount, 50)
    assertEquals(consolidated.missingCount, 0)
    assert(consolidated.spreadMeters < 15d, f"spread ${consolidated.spreadMeters}%.1f m")
    assert(!consolidated.looksMoved())
  }

  test("frames without a fix inherit the position of the session") {
    val consolidated = SessionLocation.consolidate(session(withFix = 20, withoutFix = 180))
    assertEquals(consolidated.fixCount, 20)
    assertEquals(consolidated.missingCount, 180)
    assert(consolidated.location.isDefined, "twenty fixes are enough to place the whole session")
    assert(!consolidated.fromFallback)
  }

  test("a single fix in the whole session is enough") {
    val consolidated = SessionLocation.consolidate(session(withFix = 1, withoutFix = 300))
    assertEquals(consolidated.location, Some(wandering(0)))
    assertEquals(consolidated.spreadMeters, 0d)
  }

  test("one wrong fix does not drag the session position away") {
    val wrong        = ShotMetadata.empty.copy(location = Some(GeoPoint(48.85d, 2.35d, 35d))) // Paris
    val consolidated = SessionLocation.consolidate(session(withFix = 40, withoutFix = 0) :+ wrong)
    val found        = consolidated.location.getOrElse(fail("no position"))
    // the median ignores it, but the spread reports it
    assert(SessionLocation.distanceMeters(found, place) < 10d, s"$found got dragged towards the wrong fix")
    assert(consolidated.looksMoved(), "such a spread should be reported")
  }

  test("without any fix the configured position is used") {
    val consolidated = SessionLocation.consolidate(session(withFix = 0, withoutFix = 100), Some(place))
    assertEquals(consolidated.location, Some(place))
    assert(consolidated.fromFallback)
    assertEquals(consolidated.missingCount, 100)
  }

  test("without any fix and without configured position, there is nothing to do") {
    val consolidated = SessionLocation.consolidate(session(withFix = 0, withoutFix = 10))
    assertEquals(consolidated.location, None)
    assert(consolidated.describe.contains("no position at all"), consolidated.describe)
  }

  test("distances are the real ones") {
    // Toulouse to Paris, about 590 km
    val distance = SessionLocation.distanceMeters(place, GeoPoint(48.8566d, 2.3522d))
    assertEqualsDouble(distance / 1000d, 590d, 15d)
    assertEqualsDouble(SessionLocation.distanceMeters(place, place), 0d, 1e-9d)
  }
}
