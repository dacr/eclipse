package fr.janalyse.eclipse.astro

import fr.janalyse.eclipse.model.HorizontalCoordinates

/** A direction in space, in the frame where `x` points north, `y` east and `z` up.
  *
  * Angles are convenient to read and hopeless to compute with : every composition of two rotations
  * turns into a page of spherical trigonometry. Three numbers and a dot product say the same thing
  * and compose by themselves.
  */
final case class Vec3(x: Double, y: Double, z: Double) {
  def dot(other: Vec3): Double  = x * other.x + y * other.y + z * other.z
  def +(other: Vec3): Vec3      = Vec3(x + other.x, y + other.y, z + other.z)
  def *(factor: Double): Vec3   = Vec3(x * factor, y * factor, z * factor)
  def norm: Double              = math.sqrt(x * x + y * y + z * z)

  def normalized: Vec3 = {
    val length = norm
    if (length <= 1e-12d) this else this * (1d / length)
  }

  /** Back to an azimuth and an altitude */
  def toHorizontal: HorizontalCoordinates = {
    val length = norm
    if (length <= 1e-12d) HorizontalCoordinates(0d, 90d)
    else
      HorizontalCoordinates(
        azimuthDegrees = SolarEphemeris.normalizeDegrees(math.toDegrees(math.atan2(y, x))),
        altitudeDegrees = math.toDegrees(math.asin(math.max(-1d, math.min(1d, z / length))))
      )
  }
}

object Vec3 {

  /** Unit vector towards a direction of the sky */
  def of(coordinates: HorizontalCoordinates): Vec3 = {
    val altitude = math.toRadians(coordinates.altitudeDegrees)
    val azimuth  = math.toRadians(coordinates.azimuthDegrees)
    Vec3(
      x = math.cos(altitude) * math.cos(azimuth),
      y = math.cos(altitude) * math.sin(azimuth),
      z = math.sin(altitude)
    )
  }
}

/** How a rectilinear camera is oriented : where it points, and how it is rolled around that axis.
  *
  * This is the geometry of a gnomonic projection expressed as an orthonormal basis rather than as
  * formulas. Written that way, two things which are painful in spherical trigonometry become
  * immediate :
  *
  *   - a rolled camera is just a rotation of two basis vectors, no formula changes ;
  *   - going from the tangent plane of one camera to the tangent plane of another is a single 3x3
  *     projective map, exactly, because both planes are central projections of the same sphere.
  *
  * That second point is what lets a wide angle photograph of the whole landscape be redrawn in the
  * geometry of a composite : one matrix, nine multiplications per pixel, no trigonometry and no
  * approximation.
  *
  * @param rollDegrees rotation of the camera around its axis, positive when the horizon runs down
  *                    towards the right of the picture.
  */
final case class SkyFrame(pointing: HorizontalCoordinates, rollDegrees: Double = 0d) {

  /** Where the optical axis points */
  val forward: Vec3 = Vec3.of(pointing)

  // the natural basis of the tangent plane : towards increasing azimuths, and towards increasing
  // altitudes. Both are unit vectors and both are perpendicular to the axis.
  private val azimuthal = {
    val azimuth = math.toRadians(pointing.azimuthDegrees)
    Vec3(-math.sin(azimuth), math.cos(azimuth), 0d)
  }

  private val vertical = {
    val altitude = math.toRadians(pointing.altitudeDegrees)
    val azimuth  = math.toRadians(pointing.azimuthDegrees)
    Vec3(
      -math.sin(altitude) * math.cos(azimuth),
      -math.sin(altitude) * math.sin(azimuth),
      math.cos(altitude)
    )
  }

  private val roll = math.toRadians(rollDegrees)

  /** Image axis growing to the right of the picture */
  val right: Vec3 = azimuthal * math.cos(roll) + vertical * math.sin(roll)

  /** Image axis growing towards the top of the picture */
  val up: Vec3 = azimuthal * -math.sin(roll) + vertical * math.cos(roll)

  /** Direction seen at the given tangent plane coordinates */
  def directionAt(x: Double, y: Double): Vec3 = (forward + right * x + up * y).normalized

  /** Where a point of the tangent plane falls in the sky */
  def skyAt(x: Double, y: Double): HorizontalCoordinates = directionAt(x, y).toHorizontal

  /** Tangent plane coordinates of a direction, `None` for anything at 90° or more from the axis */
  def project(direction: Vec3): Option[(Double, Double)] = {
    val depth = direction.dot(forward)
    if (depth <= 1e-6d) None else Some((direction.dot(right) / depth, direction.dot(up) / depth))
  }

  def project(coordinates: HorizontalCoordinates): Option[(Double, Double)] = project(Vec3.of(coordinates))

  /** The 3x3 projective map taking tangent plane coordinates of this frame into those of `other`.
    *
    * Row major, applied to `(x, y, 1)` and divided by the third result : the plain homography of a
    * plane to plane perspective change. Exact for any angle between the two axes.
    */
  def tangentMapTo(other: SkyFrame): Array[Double] =
    Array(
      other.right.dot(right), other.right.dot(up), other.right.dot(forward),
      other.up.dot(right), other.up.dot(up), other.up.dot(forward),
      other.forward.dot(right), other.forward.dot(up), other.forward.dot(forward)
    )
}
