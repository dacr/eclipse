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
  private val centerAltitude = math.toRadians(center.altitudeDegrees)
  private val centerAzimuth  = math.toRadians(center.azimuthDegrees)

  /** Projects a direction of the sky, `x` grows towards increasing azimuths (to the right when
    * facing the center) and `y` grows upwards. Returns `None` for anything more than 90° away.
    */
  def project(coordinates: HorizontalCoordinates): Option[(Double, Double)] = {
    val altitude     = math.toRadians(coordinates.altitudeDegrees)
    val deltaAzimuth = math.toRadians(coordinates.azimuthDegrees) - centerAzimuth
    val cosDistance  =
      math.sin(centerAltitude) * math.sin(altitude) +
        math.cos(centerAltitude) * math.cos(altitude) * math.cos(deltaAzimuth)
    if (cosDistance <= 1e-6d) None
    else {
      val x = math.cos(altitude) * math.sin(deltaAzimuth) / cosDistance
      val y = (math.cos(centerAltitude) * math.sin(altitude) -
        math.sin(centerAltitude) * math.cos(altitude) * math.cos(deltaAzimuth)) / cosDistance
      Some((x, y))
    }
  }

  /** Inverse projection, mostly useful for tests and for annotating a composite */
  def unproject(x: Double, y: Double): HorizontalCoordinates = {
    val distance = math.sqrt(x * x + y * y)
    if (distance < 1e-12d) center
    else {
      val angle    = math.atan(distance)
      val altitude = math.asin(
        math.cos(angle) * math.sin(centerAltitude) + y * math.sin(angle) * math.cos(centerAltitude) / distance
      )
      val azimuth  = centerAzimuth + math.atan2(
        x * math.sin(angle),
        distance * math.cos(centerAltitude) * math.cos(angle) - y * math.sin(centerAltitude) * math.sin(angle)
      )
      HorizontalCoordinates(SolarEphemeris.normalizeDegrees(math.toDegrees(azimuth)), math.toDegrees(altitude))
    }
  }

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
