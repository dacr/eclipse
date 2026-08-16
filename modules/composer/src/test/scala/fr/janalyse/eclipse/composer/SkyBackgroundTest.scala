package fr.janalyse.eclipse.composer

import fr.janalyse.eclipse.astro.{SkyFrame, SolarEphemeris}
import fr.janalyse.eclipse.model.{AtmosphericConditions, GeoPoint, HorizontalCoordinates}

import java.awt.image.BufferedImage
import java.nio.file.Paths
import java.time.Instant

class SkyBackgroundTest extends munit.FunSuite {

  private val observer = GeoPoint(41.9583d, -4.7735d, 825d)
  private val image    = BufferedImage(6000, 4000, BufferedImage.TYPE_INT_RGB)
  private val scale    = 84.43d

  private def request(sessionSpan: Option[(Instant, Instant)] = None) =
    SkyBackground.Request(
      path = Paths.get("landscape.jpg"),
      cacheDirectory = Paths.get("target/test-cache"),
      observer = Some(observer),
      atmosphere = AtmosphericConditions(),
      sessionSpan = sessionSpan
    )

  private def pixelOf(background: SkyBackground, sky: HorizontalCoordinates): (Double, Double) = {
    val (x, y) = background.frame.project(sky).get
    (background.centerX + x * background.pixelsPerRadian, background.centerY - y * background.pixelsPerRadian)
  }

  test("a camera is aimed by the one star it recorded") {
    val sun      = HorizontalCoordinates(288.45d, 2.01d)
    val sunPixel = (3237.5d, 2099.5d)
    val frame    = SkyBackground.pointingFrom(sun, sunPixel, image, scale, rollDegrees = 0d)

    val background = SkyBackground(image, frame, scale)
    val (x, y)     = pixelOf(background, sun)
    assertEqualsDouble(x, sunPixel._1, 1e-6d)
    assertEqualsDouble(y, sunPixel._2, 1e-6d)

    // a level camera whose sun sits below the middle of the frame is aimed above the sun
    assert(frame.pointing.altitudeDegrees > sun.altitudeDegrees)
    assert(frame.pointing.azimuthDegrees < sun.azimuthDegrees)
  }

  test("a rolled camera is aimed just as well") {
    val sun      = HorizontalCoordinates(288.45d, 2.01d)
    val sunPixel = (2100d, 900d)
    val frame    = SkyBackground.pointingFrom(sun, sunPixel, image, scale, rollDegrees = 6.5d)

    val (x, y) = pixelOf(SkyBackground(image, frame, scale), sun)
    assertEqualsDouble(x, sunPixel._1, 1e-6d)
    assertEqualsDouble(y, sunPixel._2, 1e-6d)
  }

  test("the horizon lands where a level camera says it should") {
    val sun        = HorizontalCoordinates(288.45d, 2.01d)
    val frame      = SkyBackground.pointingFrom(sun, (3237.5d, 2099.5d), image, scale, rollDegrees = 0d)
    val background = SkyBackground(image, frame, scale)
    val row        = background.horizonRow.get

    // the sun stands two degrees above the horizon, so the horizon runs two degrees below it
    val degreesBelowTheSun = (row - 2099.5d) / scale
    assertEqualsDouble(degreesBelowTheSun, 2.01d, 0.05d)
  }

  test("a clock left on another time zone is put right by whole hours") {
    // the wall clock was right, the offset was not : the picture claims an instant an hour later
    val truth    = Instant.parse("2026-08-12T19:09:16Z")
    val declared = truth.plusSeconds(3600L)
    val session  = (Instant.parse("2026-08-12T17:00:35Z"), Instant.parse("2026-08-12T19:08:47Z"))
    val notes    = List.newBuilder[String]

    val found = SkyBackground.resolveInstant(declared, observer, request(Some(session)), notes)
    assertEquals(found, truth)
    assert(notes.result().exists(_.contains("corrected by -1 h")), notes.result().mkString(", "))
  }

  test("the smallest correction wins, not the one that lands deepest in the session") {
    // two hours back would fall right in the middle of the session, and would be wrong : the sun
    // would then stand ten degrees high, which is not what a picture of a sunset shows
    val declared = Instant.parse("2026-08-12T20:09:16Z")
    val session  = (Instant.parse("2026-08-12T17:00:35Z"), Instant.parse("2026-08-12T19:08:47Z"))
    val notes    = List.newBuilder[String]

    val found     = SkyBackground.resolveInstant(declared, observer, request(Some(session)), notes)
    val altitude  = SolarEphemeris.position(found, observer, AtmosphericConditions()).apparent.altitudeDegrees
    assertEquals(found, Instant.parse("2026-08-12T19:09:16Z"))
    assert(altitude > 0d && altitude < 4d, f"the sun should be about to set, it stands at $altitude%.2f°")
  }

  test("a clock which agrees with the session is left alone") {
    val declared = Instant.parse("2026-08-12T18:30:00Z")
    val session  = (Instant.parse("2026-08-12T17:00:35Z"), Instant.parse("2026-08-12T19:08:47Z"))
    val notes    = List.newBuilder[String]

    assertEquals(SkyBackground.resolveInstant(declared, observer, request(Some(session)), notes), declared)
    assertEquals(notes.result(), Nil)
  }

  test("the covered area stays inside what the picture holds") {
    val frame      = SkyBackground.pointingFrom(HorizontalCoordinates(288.45d, 2.01d), (3237.5d, 2099.5d), image, scale, 0d)
    val background = SkyBackground(image, frame, scale)
    val composite  = SkyFrame(HorizontalCoordinates(277d, 13.5d))
    val pixelsPerRadian = 600d * 180d / math.Pi

    val (left, top, right, bottom) = background.coveredArea(composite, pixelsPerRadian).get
    assert(left < right && top < bottom)

    // every corner of that area has to fall on a pixel the picture actually has
    List((left, top), (right, top), (left, bottom), (right, bottom)).foreach { case (x, y) =>
      val sky      = composite.skyAt(x / pixelsPerRadian, -y / pixelsPerRadian)
      val (px, py) = pixelOf(background, sky)
      assert(
        px >= -1d && py >= -1d && px <= background.width + 1d && py <= background.height + 1d,
        f"the area reaches ${px}%.0f,${py}%.0f, outside a ${background.width}x${background.height} picture"
      )
    }
  }
}
