package fr.janalyse.eclipse.astro

import fr.janalyse.eclipse.model.HorizontalCoordinates

/** Gnomonic (tangent plane) projection of the sky onto a flat picture.
  *
  * This is exactly what a rectilinear lens does : the sky is projected onto the sensor plane
  * through the optical center. Laying out a composite with this projection gives an image which
  * looks like a single wide angle shot of the whole event - straight lines stay straight, and the
  * curvature of the solar path is the real one, not a decorative arc.
  *
  * Coordinates are returned in tangent units (the tangent of the angular distance to the center),
  * multiply by the number of pixels per radian to get pixels.
  *
  * @param center direction the virtual camera is pointed at, and tangency point of the projection
  */
final case class TangentPlaneProjection(center: HorizontalCoordinates) {

  /** The projection is the geometry of a camera pointed at the center, level and unrolled */
  val frame: SkyFrame = SkyFrame(center)

  /** Projects a direction of the sky, `x` grows towards increasing azimuths (to the right when
    * facing the center) and `y` grows upwards. Returns `None` for anything more than 90° away.
    */
  def project(coordinates: HorizontalCoordinates): Option[(Double, Double)] = frame.project(coordinates)

  /** Inverse projection, mostly useful for tests and for annotating a composite */
  def unproject(x: Double, y: Double): HorizontalCoordinates =
    if (x * x + y * y < 1e-24d) center else frame.skyAt(x, y)

  /** Angular size, in tangent units, of something seen `degrees` wide at the given position : away
    * from the tangency point the gnomonic projection stretches the field, this accounts for it.
    */
  def scaleAt(coordinates: HorizontalCoordinates): Double = {
    val distanceDegrees = center.angularDistanceTo(coordinates)
    val cosine          = math.cos(math.toRadians(distanceDegrees))
    if (cosine <= 1e-6d) 1d else 1d / (cosine * cosine)
  }
}

object TangentPlaneProjection {

  /** Mean direction of a set of sky positions, computed as a vector average : the natural place to
    * point the virtual camera at when projecting a whole session.
    */
  def meanDirection(positions: Seq[HorizontalCoordinates]): HorizontalCoordinates = {
    if (positions.isEmpty) HorizontalCoordinates(180d, 45d)
    else {
      var x = 0d
      var y = 0d
      var z = 0d
      positions.foreach { position =>
        val altitude = math.toRadians(position.altitudeDegrees)
        val azimuth  = math.toRadians(position.azimuthDegrees)
        x += math.cos(altitude) * math.cos(azimuth)
        y += math.cos(altitude) * math.sin(azimuth)
        z += math.sin(altitude)
      }
      val count     = positions.size
      val meanX     = x / count
      val meanY     = y / count
      val meanZ     = z / count
      val horizontal = math.hypot(meanX, meanY)
      HorizontalCoordinates(
        azimuthDegrees = SolarEphemeris.normalizeDegrees(math.toDegrees(math.atan2(meanY, meanX))),
        altitudeDegrees = math.toDegrees(math.atan2(meanZ, horizontal))
      )
    }
  }
}
