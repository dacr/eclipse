package fr.janalyse.eclipse.astro

import fr.janalyse.eclipse.model.HorizontalCoordinates

class TangentPlaneProjectionTest extends munit.FunSuite {

  val center     = HorizontalCoordinates(200d, 40d)
  val projection = TangentPlaneProjection(center)

  test("the tangency point falls at the origin") {
    val (x, y) = projection.project(center).getOrElse(fail("projection failed"))
    assertEqualsDouble(x, 0d, 1e-12d)
    assertEqualsDouble(y, 0d, 1e-12d)
  }

  test("a small offset in altitude projects at the tangent of the angle") {
    val (x, y) = projection.project(HorizontalCoordinates(200d, 41d)).getOrElse(fail("projection failed"))
    assertEqualsDouble(x, 0d, 1e-9d)
    assertEqualsDouble(y, math.tan(math.toRadians(1d)), 1e-9d)
  }

  test("increasing azimuths go to the right") {
    val (x, _) = projection.project(HorizontalCoordinates(202d, 40d)).getOrElse(fail("projection failed"))
    assert(x > 0d, s"$x should be positive")
  }

  test("projecting then unprojecting gives the starting direction back") {
    val positions = List(
      HorizontalCoordinates(200d, 40d),
      HorizontalCoordinates(215d, 35d),
      HorizontalCoordinates(180d, 55d),
      HorizontalCoordinates(230d, 10d)
    )
    positions.foreach { position =>
      val (x, y) = projection.project(position).getOrElse(fail(s"projection failed for $position"))
      val back   = projection.unproject(x, y)
      assertEqualsDouble(back.azimuthDegrees, position.azimuthDegrees, 1e-6d)
      assertEqualsDouble(back.altitudeDegrees, position.altitudeDegrees, 1e-6d)
    }
  }

  test("the mean direction sits between the given ones") {
    // vector average, so the highest direction weighs a little less on the azimuth
    val mean = TangentPlaneProjection.meanDirection(
      List(HorizontalCoordinates(170d, 30d), HorizontalCoordinates(190d, 40d))
    )
    assertEqualsDouble(mean.azimuthDegrees, 179.4d, 0.2d)
    assertEqualsDouble(mean.altitudeDegrees, 35.4d, 0.2d)

    val symmetric = TangentPlaneProjection.meanDirection(
      List(HorizontalCoordinates(170d, 30d), HorizontalCoordinates(190d, 30d))
    )
    assertEqualsDouble(symmetric.azimuthDegrees, 180d, 1e-6d)
  }

  test("nothing can be projected from behind the observer") {
    assertEquals(projection.project(HorizontalCoordinates(20d, 40d)), None)
  }
}
