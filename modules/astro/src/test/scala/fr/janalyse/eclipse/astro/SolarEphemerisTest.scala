package fr.janalyse.eclipse.astro

import fr.janalyse.eclipse.model.{AtmosphericConditions, GeoPoint, HorizontalCoordinates}

import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset}

class SolarEphemerisTest extends munit.FunSuite {

  val greenwich = GeoPoint(51.4778d, -0.0015d, 47d)

  test("julian day of the J2000 epoch") {
    val instant = LocalDateTime.of(2000, 1, 1, 12, 0, 0).toInstant(ZoneOffset.UTC)
    assertEqualsDouble(SolarEphemeris.julianDay(instant), 2451545d, 1e-6d)
  }

  test("greenwich mean sidereal time, Meeus example 12.a") {
    // 1987 April 10 at 0h UT : theta0 = 13h10m46.3668s = 197.693195 degrees
    val instant = LocalDateTime.of(1987, 4, 10, 0, 0, 0).toInstant(ZoneOffset.UTC)
    val sidereal = SolarEphemeris.greenwichMeanSiderealTime(SolarEphemeris.julianDay(instant))
    assertEqualsDouble(sidereal, 197.693195d, 1e-4d)
  }

  test("apparent position of the sun, Meeus example 25.b") {
    // 1992 October 13 at 0h TD : right ascension 13h13m31s, declination -7°47'
    val instant  = LocalDateTime.of(1992, 10, 13, 0, 0, 0).toInstant(ZoneOffset.UTC)
    val position = SolarEphemeris.position(instant, GeoPoint(0d, 0d), deltaTSeconds = 0d)
    assertEqualsDouble(position.equatorial.rightAscensionDegrees, 198.38083d, 0.02d)
    assertEqualsDouble(position.equatorial.declinationDegrees, -7.78507d, 0.02d)
    assertEqualsDouble(position.distanceAstronomicalUnits, 0.99760775d, 1e-4d)
  }

  test("the sun looks bigger in january than in july") {
    def semiDiameter(date: LocalDate) =
      SolarEphemeris.position(date.atTime(12, 0).toInstant(ZoneOffset.UTC), greenwich).semiDiameterDegrees

    val january = semiDiameter(LocalDate.of(2026, 1, 3))
    val july    = semiDiameter(LocalDate.of(2026, 7, 5))
    assert(january > july, s"$january should be greater than $july")
    assertEqualsDouble(january, 0.2712d, 0.001d)
    assertEqualsDouble(july, 0.2623d, 0.001d)
  }

  test("highest sun of the year at greenwich, on the june solstice") {
    val date      = LocalDate.of(2026, 6, 21)
    val positions = (0 until 24 * 60 by 2).map { minutes =>
      val instant = date.atStartOfDay.plusMinutes(minutes.toLong).toInstant(ZoneOffset.UTC)
      SolarEphemeris.position(instant, greenwich)
    }
    val highest = positions.maxBy(_.geometric.altitudeDegrees)
    // 90 - latitude + obliquity = 61.96 degrees
    assertEqualsDouble(highest.geometric.altitudeDegrees, 61.96d, 0.2d)
    assertEqualsDouble(highest.apparent.azimuthDegrees, 180d, 1d)
  }

  test("refraction lifts the sun near the horizon and vanishes overhead") {
    // standard conditions of the Bennett formula : 1010 hPa and 10°C
    val standard = AtmosphericConditions(1010d, 10d)
    assertEqualsDouble(Refraction.correctionDegrees(0d, standard), 0.4830d, 0.002d)
    assertEqualsDouble(Refraction.correctionDegrees(5d, standard), 0.1612d, 0.002d)
    assertEqualsDouble(Refraction.correctionDegrees(45d, standard), 0.0169d, 0.001d)
    assert(Refraction.correctionDegrees(89d, standard) < 0.001d)
    // near the horizon the sun is lifted by about its own diameter, which cannot be ignored when
    // placing frames of a sunset eclipse
    assert(Refraction.correctionDegrees(0d, standard) > 0.4d)
    // a warmer or a thinner atmosphere refracts less
    assert(Refraction.correctionDegrees(5d, AtmosphericConditions(1010d, 30d)) < Refraction.correctionDegrees(5d, standard))
    assert(Refraction.correctionDegrees(5d, AtmosphericConditions(900d, 10d)) < Refraction.correctionDegrees(5d, standard))
  }

  test("refraction squashes the disc, and only near the horizon") {
    // this is what the elliptical fit of the detector is expected to find on the last frames of a
    // sunset eclipse, so it is worth having the physics written down next to it
    assert(Refraction.flattening(45d) < 0.002d, "high in the sky the sun stays round")
    assertEqualsDouble(Refraction.flattening(5d), 0.024d, 0.006d)
    assertEqualsDouble(Refraction.flattening(2d), 0.065d, 0.015d)
    assert(Refraction.flattening(0d) > 0.12d, "on the horizon the squashing is plain to see")
    // and it only ever grows as the sun goes down
    val altitudes = List(30d, 20d, 10d, 5d, 3d, 2d, 1d, 0d)
    assertEquals(altitudes.map(Refraction.flattening(_)), altitudes.map(Refraction.flattening(_)).sorted)
  }

  test("angular distance between two directions of the sky") {
    val zenith = HorizontalCoordinates(0d, 90d)
    val south  = HorizontalCoordinates(180d, 30d)
    assertEqualsDouble(zenith.angularDistanceTo(south), 60d, 1e-6d)
    assertEqualsDouble(
      HorizontalCoordinates(180d, 30d).angularDistanceTo(HorizontalCoordinates(180d, 31d)),
      1d,
      1e-6d
    )
  }
}
