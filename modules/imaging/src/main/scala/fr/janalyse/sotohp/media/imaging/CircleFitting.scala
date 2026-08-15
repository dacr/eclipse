package fr.janalyse.sotohp.media.imaging

/** Circle fitting helpers.
  *
  * Used to recover the exact position and size of a round subject (the sun, the moon, a planet)
  * within a frame. Two fits are provided :
  *   - an algebraic one (Kåsa) which is direct and fast but needs points spread over the circle,
  *   - a geometric one with a known radius, which only looks for the center and is far more robust
  *     when only an arc of the circle is visible - exactly what a partially eclipsed sun gives.
  */
object CircleFitting {

  final case class Circle(centerX: Double, centerY: Double, radius: Double) {
    def distanceToCenter(x: Double, y: Double): Double = math.hypot(x - centerX, y - centerY)

    /** Signed distance to the circle outline, negative when the point lies inside */
    def residual(x: Double, y: Double): Double = distanceToCenter(x, y) - radius

    def scaled(factor: Double): Circle = Circle(centerX * factor, centerY * factor, radius * factor)

    def withRadius(newRadius: Double): Circle = copy(radius = newRadius)
  }

  final case class Point(x: Double, y: Double)

  /** Direct algebraic circle fit (Kåsa), minimizes the algebraic distance over all given points */
  def algebraicFit(points: Seq[Point]): Option[Circle] = {
    if (points.sizeIs < 3) None
    else {
      var sumX  = 0d; var sumY = 0d; var sumXX = 0d; var sumYY = 0d; var sumXY = 0d
      var sumXZ = 0d; var sumYZ = 0d; var sumZ = 0d
      points.foreach { point =>
        val squared = point.x * point.x + point.y * point.y
        sumX += point.x; sumY += point.y
        sumXX += point.x * point.x; sumYY += point.y * point.y; sumXY += point.x * point.y
        sumXZ += point.x * squared; sumYZ += point.y * squared; sumZ += squared
      }
      val count = points.size.toDouble
      // normal equations of  z + D.x + E.y + F = 0
      val matrix = Array(
        Array(sumXX, sumXY, sumX),
        Array(sumXY, sumYY, sumY),
        Array(sumX, sumY, count)
      )
      val vector = Array(-sumXZ, -sumYZ, -sumZ)
      LinearAlgebra.solve(matrix, vector).flatMap { case Array(d, e, f) =>
        val centerX      = -d / 2d
        val centerY      = -e / 2d
        val radiusSquare = centerX * centerX + centerY * centerY - f
        if (radiusSquare <= 0d) None else Some(Circle(centerX, centerY, math.sqrt(radiusSquare)))
      }
    }
  }

  /** Geometric fit with a fixed radius : only the center is searched for.
    *
    * Fixed point iteration of the least squares problem `min sum((|p - c| - r)^2)`, it converges in
    * a handful of iterations and stays stable even when the points only cover a small arc.
    */
  def fitCenterWithKnownRadius(points: Seq[Point], radius: Double, start: (Double, Double), iterations: Int = 50): Option[Circle] = {
    if (points.isEmpty || radius <= 0d) None
    else {
      var centerX = start._1
      var centerY = start._2
      var step    = 0
      while (step < iterations) {
        var meanX      = 0d
        var meanY      = 0d
        var directionX = 0d
        var directionY = 0d
        points.foreach { point =>
          val deltaX   = point.x - centerX
          val deltaY   = point.y - centerY
          val distance = math.hypot(deltaX, deltaY)
          meanX += point.x
          meanY += point.y
          if (distance > 1e-9) {
            directionX += deltaX / distance
            directionY += deltaY / distance
          }
        }
        val count   = points.size.toDouble
        val newX    = meanX / count - radius * directionX / count
        val newY    = meanY / count - radius * directionY / count
        val moved   = math.hypot(newX - centerX, newY - centerY)
        centerX = newX
        centerY = newY
        step = if (moved < 1e-6) iterations else step + 1
      }
      Some(Circle(centerX, centerY, radius))
    }
  }

  /** Outline oriented robust fit.
    *
    * A partially eclipsed sun gives two families of boundary points : the solar limb (on the circle
    * we are looking for) and the lunar limb (inside it). Points falling clearly inside the current
    * estimate are dropped, iteration after iteration, so that the fit converges onto the outer arc.
    *
    * @param knownRadius when known (from the ephemeris and the plate scale) the radius is enforced,
    *                    which makes the whole thing far more reliable on thin crescents.
    */
  def robustOuterFit(
    points: Seq[Point],
    knownRadius: Option[Double] = None,
    iterations: Int = 8,
    rejectionRatio: Double = 0.03,
    minimumInlierRatio: Double = 0.15
  ): Option[(Circle, Seq[Point])] = {
    // the fit is started on the outer half of the points only : starting from all of them would let
    // the inner arc - the occulting body limb - drag the first estimate far away from the truth,
    // and the rejection would then throw away the wrong family of points.
    val meanX      = points.map(_.x).sum / points.size.max(1)
    val meanY      = points.map(_.y).sum / points.size.max(1)
    val distances  = points.map(point => math.hypot(point.x - meanX, point.y - meanY)).sorted
    val medianDistance = if (distances.isEmpty) 0d else distances(distances.size / 2)
    val outerPoints    = points.filter(point => math.hypot(point.x - meanX, point.y - meanY) >= medianDistance)
    val startingPoints = if (outerPoints.sizeIs >= 3) outerPoints else points

    val initial = knownRadius match {
      case Some(radius) => fitCenterWithKnownRadius(startingPoints, radius, (meanX, meanY))
      case None         => algebraicFit(startingPoints)
    }
    initial.flatMap { start =>
      var circle    = start
      var remaining = points
      var step      = 0
      var failed    = false
      while (step < iterations && !failed) {
        val tolerance = rejectionRatio * circle.radius
        val kept      = remaining.filter(point => circle.residual(point.x, point.y) > -tolerance)
        if (kept.size < (points.size * minimumInlierRatio).toInt.max(3)) {
          step = iterations
        } else {
          remaining = kept
          val refitted = knownRadius match {
            case Some(radius) => fitCenterWithKnownRadius(kept, radius, (circle.centerX, circle.centerY))
            case None         => algebraicFit(kept)
          }
          refitted match {
            case Some(better) => circle = better; step += 1
            case None         => failed = true
          }
        }
      }
      if (failed) None else Some(circle -> remaining)
    }
  }

  /** Root mean square distance of the points to the circle outline */
  def residualRms(circle: Circle, points: Seq[Point]): Double =
    if (points.isEmpty) 0d
    else math.sqrt(points.map(point => math.pow(circle.residual(point.x, point.y), 2)).sum / points.size)

  /** How far from a circle the points are, whatever its radius : the standard deviation of their
    * distances to the center.
    *
    * This is what tells a good fit from a bad one when the radius is imposed. The apparent radius
    * of the sun does move a little from one frame to the next - a darker exposure cuts the limb
    * darkening earlier - and judging a fit by its distance to the imposed radius would condemn a
    * perfectly centered measurement. What matters is that the points lie on *a* circle.
    */
  def radialSpread(circle: Circle, points: Seq[Point]): Double =
    if (points.sizeIs < 2) 0d
    else {
      val distances = points.map(point => circle.distanceToCenter(point.x, point.y))
      val mean      = distances.sum / distances.size
      math.sqrt(distances.map(distance => math.pow(distance - mean, 2)).sum / distances.size)
    }
}
