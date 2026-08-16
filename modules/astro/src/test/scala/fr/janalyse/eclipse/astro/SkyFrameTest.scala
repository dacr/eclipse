package fr.janalyse.eclipse.astro

import fr.janalyse.eclipse.model.HorizontalCoordinates

class SkyFrameTest extends munit.FunSuite {

  private val samples = for {
    x <- List(-0.6d, -0.2d, 0d, 0.15d, 0.5d)
    y <- List(-0.4d, -0.1d, 0d, 0.25d, 0.45d)
  } yield (x, y)

  /** The 3x3 map applied by hand, so that the test checks the matrix and not a helper */
  private def through(matrix: Array[Double], x: Double, y: Double): Option[(Double, Double)] = {
    val depth = matrix(6) * x + matrix(7) * y + matrix(8)
    Option.when(depth > 1e-12d)(
      ((matrix(0) * x + matrix(1) * y + matrix(2)) / depth, (matrix(3) * x + matrix(4) * y + matrix(5)) / depth)
    )
  }

  test("a direction projected and read back is the direction it was") {
    val frame  = SkyFrame(HorizontalCoordinates(285d, 12d))
    val sky    = HorizontalCoordinates(262d, 24d)
    val (x, y) = frame.project(sky).get
    val back   = frame.skyAt(x, y)
    assertEqualsDouble(back.azimuthDegrees, sky.azimuthDegrees, 1e-9d)
    assertEqualsDouble(back.altitudeDegrees, sky.altitudeDegrees, 1e-9d)
  }

  test("nothing behind the camera gets projected") {
    val frame = SkyFrame(HorizontalCoordinates(0d, 0d))
    assert(frame.project(HorizontalCoordinates(180d, 0d)).isEmpty)
    assert(frame.project(HorizontalCoordinates(91d, 0d)).isEmpty)
    assert(frame.project(HorizontalCoordinates(80d, 0d)).isDefined)
  }

  test("the map between two tangent planes is the round trip through the sky, exactly") {
    // two cameras of the same session : one aimed at the sequence, one at the landscape, ten degrees
    // apart and not squared with one another
    val sequence  = SkyFrame(HorizontalCoordinates(277d, 13.5d))
    val landscape = SkyFrame(HorizontalCoordinates(285.6d, 3.2d))
    val matrix    = sequence.tangentMapTo(landscape)

    samples.foreach { case (x, y) =>
      val throughTheSky = landscape.project(sequence.directionAt(x, y))
      val throughMatrix = through(matrix, x, y)
      assertEquals(throughMatrix.isDefined, throughTheSky.isDefined, s"disagreement about ($x,$y)")
      (throughTheSky zip throughMatrix).foreach { case ((expectedX, expectedY), (foundX, foundY)) =>
        assertEqualsDouble(foundX, expectedX, 1e-12d)
        assertEqualsDouble(foundY, expectedY, 1e-12d)
      }
    }
  }

  test("a frame maps onto itself as the identity, roll included") {
    val frame  = SkyFrame(HorizontalCoordinates(120d, 40d), rollDegrees = 7d)
    val matrix = frame.tangentMapTo(frame)
    samples.foreach { case (x, y) =>
      val (foundX, foundY) = through(matrix, x, y).get
      assertEqualsDouble(foundX, x, 1e-12d)
      assertEqualsDouble(foundY, y, 1e-12d)
    }
  }

  test("a roll tilts the horizon down towards the right of the picture") {
    val level      = SkyFrame(HorizontalCoordinates(270d, 5d))
    val rolled     = SkyFrame(HorizontalCoordinates(270d, 5d), rollDegrees = 10d)
    val toTheRight = HorizontalCoordinates(280d, 0d)

    val (levelX, levelY)   = level.project(toTheRight).get
    val (rolledX, rolledY) = rolled.project(toTheRight).get
    assert(rolledX > 0d && levelX > 0d, "the point is to the right either way")
    assert(rolledY < levelY, f"a rolled camera should push it down, $rolledY%.4f against $levelY%.4f")
  }

  test("the projection still answers what the spherical trigonometry answered") {
    val projection = TangentPlaneProjection(HorizontalCoordinates(180d, 45d))
    val (x, y)     = projection.project(HorizontalCoordinates(190d, 50d)).get
    assertEqualsDouble(x, 0.11282733d, 1e-7d)
    assertEqualsDouble(y, 0.09507926d, 1e-7d)
    val back       = projection.unproject(x, y)
    assertEqualsDouble(back.azimuthDegrees, 190d, 1e-9d)
    assertEqualsDouble(back.altitudeDegrees, 50d, 1e-9d)
  }
}
