package fr.janalyse.sotohp.media.imaging

import fr.janalyse.sotohp.media.imaging.CircleFitting.{Circle, Point}

/** Fitting an axis aligned ellipse to a set of outline points.
  *
  * Near the horizon the sun stops being round : the atmosphere refracts its lower limb more than
  * its upper one, and the disc is squashed vertically - by some six percent of its diameter at two
  * degrees of altitude, half that at five. A circle cannot follow that shape, and trying leaves a
  * residual large enough to condemn a perfectly good measurement.
  *
  * The squashing is vertical, so an ellipse with axes along the frame is enough as long as the
  * camera was level - which a tripod usually is. That restriction is what makes the fit a plain
  * least squares problem rather than an eigenvalue one : with the cross term dropped, the conic
  * `x² + C·y² + D·x + E·y + F = 0` is linear in its four remaining coefficients.
  */
object EllipseFitting {

  /** An ellipse whose axes are those of the frame, hence described by its two half widths */
  final case class Ellipse(centerX: Double, centerY: Double, semiHorizontal: Double, semiVertical: Double) {

    /** How much the shape is squashed vertically, 0 for a circle, negative if it is stretched */
    def flattening: Double = if (semiHorizontal <= 0d) 0d else 1d - semiVertical / semiHorizontal

    def meanRadius: Double = (semiHorizontal + semiVertical) / 2d

    /** Distance to the outline, in pixels, positive outside.
      *
      * An approximation - the exact distance to an ellipse has no closed form - but a good one for
      * the mild flattening at hand, and it is only ever used to judge a fit.
      */
    def residual(x: Double, y: Double): Double = {
      val normalized = math.hypot((x - centerX) / semiHorizontal, (y - centerY) / semiVertical)
      (normalized - 1d) * meanRadius
    }

    def scaled(factor: Double): Ellipse =
      Ellipse(centerX * factor, centerY * factor, semiHorizontal * factor, semiVertical * factor)

    /** The circle the rest of the pipeline works with : same center, and the horizontal half width
      * as radius - that axis is the one refraction leaves alone, so it is the true angular size.
      */
    def asCircle: Circle = Circle(centerX, centerY, semiHorizontal)
  }

  /** Least squares fit of an axis aligned ellipse, `None` when the points do not describe one */
  def fit(points: Seq[Point]): Option[Ellipse] = {
    if (points.sizeIs < 5) None
    else {
      // the fit is done around the centroid : with raw pixel coordinates the normal equations mix
      // terms of wildly different magnitudes and lose most of their significant digits
      val shiftX  = points.map(_.x).sum / points.size
      val shiftY  = points.map(_.y).sum / points.size
      val shifted = points.map(point => Point(point.x - shiftX, point.y - shiftY))

      // x² + C y² + D x + E y + F = 0, solved for (C, D, E, F) against -x²
      val terms  = shifted.map(point => Array(point.y * point.y, point.x, point.y, 1d))
      val target = shifted.map(point => -point.x * point.x)

      val matrix = Array.fill(4, 4)(0d)
      val vector = Array.fill(4)(0d)
      terms.zip(target).foreach { case (row, value) =>
        var i = 0
        while (i < 4) {
          var j = 0
          while (j < 4) { matrix(i)(j) += row(i) * row(j); j += 1 }
          vector(i) += row(i) * value
          i += 1
        }
      }

      LinearAlgebra.solve(matrix, vector).flatMap { case Array(c, d, e, f) =>
        if (c <= 1e-9d) None // a hyperbola or a parabola, not the shape of a sun
        else {
          val centerX      = -d / 2d
          val centerY      = -e / (2d * c)
          val squaredWidth = centerX * centerX + c * centerY * centerY - f
          if (squaredWidth <= 0d) None
          else {
            val semiHorizontal = math.sqrt(squaredWidth)
            val semiVertical   = math.sqrt(squaredWidth / c)
            Option.when(semiHorizontal > 0d && semiVertical > 0d)(
              Ellipse(centerX + shiftX, centerY + shiftY, semiHorizontal, semiVertical)
            )
          }
        }
      }
    }
  }

  /** Outline oriented robust fit, the ellipse counterpart of [[CircleFitting.robustOuterFit]].
    *
    * The same two families of points are met here : the limb of the body being measured, and the
    * limb of whatever occults it. Points falling clearly inside the current estimate are dropped,
    * iteration after iteration, so that the fit settles on the outer outline.
    *
    * The seed matters more than for a circle. Four free parameters fitted to an arc can wander off
    * into any elongated shape that happens to pass through it, so the fit is started from the
    * circle found beforehand and the rejection is done against that starting shape first.
    */
  def robustOuterFit(
    points: Seq[Point],
    seed: Ellipse,
    iterations: Int = 6,
    rejectionRatio: Double = 0.03,
    seedRejectionRatio: Double = 0.3,
    minimumInlierRatio: Double = 0.15
  ): Option[(Ellipse, Seq[Point])] = {
    var ellipse   = seed
    var remaining = points
    var step      = 0
    var failed    = false
    while (step < iterations && !failed) {
      // the seed is a circle, so the very points that carry the flattening - those at the top and
      // at the bottom of the outline - lie inside it, by exactly as much as the disc is squashed.
      // Rejecting them on the first pass, as if they belonged to an occulting body, would throw
      // away the only evidence there is that the shape is not round. The first pass is therefore
      // forgiving, and the tolerance closes in once a shape has actually been fitted.
      val tolerance = (if (step == 0) seedRejectionRatio else rejectionRatio) * ellipse.meanRadius
      val kept      = remaining.filter(point => ellipse.residual(point.x, point.y) > -tolerance)
      if (kept.size < (points.size * minimumInlierRatio).toInt.max(5)) step = iterations
      else {
        remaining = kept
        fit(kept) match {
          case Some(better) => ellipse = better; step += 1
          case None         => failed = true
        }
      }
    }
    Option.when(!failed && remaining.sizeIs >= 5)(ellipse -> remaining)
  }

  /** Root mean square distance of the points to the outline */
  def residualRms(ellipse: Ellipse, points: Seq[Point]): Double =
    if (points.isEmpty) 0d
    else math.sqrt(points.map(point => math.pow(ellipse.residual(point.x, point.y), 2d)).sum / points.size)

  /** How far from the fitted outline the points scatter, the counterpart of
    * [[CircleFitting.radialSpread]] : the standard deviation of their residuals, which for a circle
    * is exactly the standard deviation of their distances to the center.
    */
  def radialSpread(ellipse: Ellipse, points: Seq[Point]): Double =
    if (points.sizeIs < 2) 0d
    else {
      val residuals = points.map(point => ellipse.residual(point.x, point.y))
      val mean      = residuals.sum / residuals.size
      math.sqrt(residuals.map(residual => math.pow(residual - mean, 2d)).sum / residuals.size)
    }

  /** How wide a range of directions the points cover around a center, from 0 to 1.
    *
    * Four free parameters need the whole outline to be pinned down ; an arc leaves the shape free
    * to elongate along the missing directions. This is the guard against that.
    */
  def angularCoverage(points: Seq[Point], centerX: Double, centerY: Double, sectors: Int = 36): Double =
    if (points.isEmpty) 0d
    else {
      val touched = points.map { point =>
        val angle = math.atan2(point.y - centerY, point.x - centerX)
        ((angle + 2d * math.Pi) % (2d * math.Pi) / (2d * math.Pi) * sectors).toInt.min(sectors - 1)
      }.toSet
      touched.size.toDouble / sectors
    }
}
