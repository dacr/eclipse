package fr.janalyse.eclipse.model

import java.nio.file.Path
import java.time.{Instant, OffsetDateTime}

/** Observer position on earth */
final case class GeoPoint(
  latitudeDegrees: Double,
  longitudeDegrees: Double,
  altitudeMeters: Double = 0d
) {
  override def toString: String = f"$latitudeDegrees%.6f,$longitudeDegrees%.6f (${altitudeMeters}%.0fm)"
}

/** Where to look at : azimuth counted from north, clockwise, and altitude above the horizon */
final case class HorizontalCoordinates(azimuthDegrees: Double, altitudeDegrees: Double) {

  /** Angular distance to another direction of the sky, in degrees */
  def angularDistanceTo(other: HorizontalCoordinates): Double = {
    val thisAltitude  = math.toRadians(altitudeDegrees)
    val otherAltitude = math.toRadians(other.altitudeDegrees)
    val deltaAzimuth  = math.toRadians(other.azimuthDegrees - azimuthDegrees)
    val cosine        =
      math.sin(thisAltitude) * math.sin(otherAltitude) +
        math.cos(thisAltitude) * math.cos(otherAltitude) * math.cos(deltaAzimuth)
    math.toDegrees(math.acos(math.max(-1d, math.min(1d, cosine))))
  }

  override def toString: String = f"az=$azimuthDegrees%.3f° alt=$altitudeDegrees%.3f°"
}

/** Where a star is on the celestial sphere */
final case class EquatorialCoordinates(rightAscensionDegrees: Double, declinationDegrees: Double)

/** Atmospheric conditions, they drive the refraction correction near the horizon */
final case class AtmosphericConditions(pressureHectoPascals: Double = 1010d, temperatureCelsius: Double = 15d)

/** Everything worth knowing about the sun at a given instant and from a given place.
  *
  * `apparent` is refraction corrected : this is where the sun *looks like* it is, hence where the
  * camera did record it, and therefore the position to use when laying out a composite.
  */
final case class SunPosition(
  instant: Instant,
  geometric: HorizontalCoordinates,
  apparent: HorizontalCoordinates,
  equatorial: EquatorialCoordinates,
  distanceAstronomicalUnits: Double,
  semiDiameterDegrees: Double,
  parallacticAngleDegrees: Double
) {
  def diameterDegrees: Double = semiDiameterDegrees * 2d
}

/** What the camera recorded, as read from the EXIF metadata */
final case class ShotMetadata(
  shotAt: Option[OffsetDateTime],
  location: Option[GeoPoint],
  cameraName: Option[String],
  lensName: Option[String],
  focalLengthMillimeters: Option[Double],
  aperture: Option[Double],
  exposureTimeSeconds: Option[Double],
  isoSensitivity: Option[Double],
  imageWidth: Option[Int],
  imageHeight: Option[Int]
)

object ShotMetadata {
  val empty: ShotMetadata = ShotMetadata(None, None, None, None, None, None, None, None, None, None)
}

extension (metadata: ShotMetadata) {

  /** Exposure value brought back to 100 ISO.
    *
    * The single most telling number of an eclipse session : shooting through a ND1000000 filter and
    * shooting the corona bare lens are about twenty stops apart, so this alone tells which frames
    * were taken during totality, with no image analysis at all.
    */
  def exposureValue: Option[Double] =
    for {
      aperture <- metadata.aperture
      time     <- metadata.exposureTimeSeconds
      if aperture > 0d && time > 0d
    } yield {
      val sensitivity = metadata.isoSensitivity.getOrElse(100d).max(1d)
      math.log(aperture * aperture / time) / math.log(2d) - math.log(sensitivity / 100d) / math.log(2d)
    }
}

/** How the solar disc shows up on a given frame */
enum FramePhase {

  /** The photosphere is visible, filtered shot, from first to last contact */
  case Partial

  /** Totality : no photosphere left, corona and prominences only */
  case Totality

  /** Could not be decided */
  case Unknown
}

/** The solar disc as measured on the frame itself, in full resolution pixel coordinates */
final case class MeasuredDisc(
  centerX: Double,
  centerY: Double,
  radiusPixels: Double,
  phase: FramePhase,
  obscuration: Option[Double],
  fitResidualPixels: Double,
  detectionConfidence: Double,
  /** brightness step measured across the outer edge, high when the photosphere limb is visible */
  limbContrast: Double = 0d
)

/** Image scale, the bridge between the sky and the sensor */
final case class PlateScale(pixelsPerDegree: Double) {
  def pixelsFor(degrees: Double): Double = degrees * pixelsPerDegree
  def degreesFor(pixels: Double): Double = pixels / pixelsPerDegree

  /** Focal length a full frame camera would need to cover the given field, only meant to be
    * printed in a report : it tells which lens would have caught the whole composite in one shot.
    */
  def equivalentFullFrameFocalLength(fieldWidthDegrees: Double): Double =
    18d / math.tan(math.toRadians(math.max(0.001d, fieldWidthDegrees)) / 2d)
}

object PlateScale {

  /** Plate scale computed from a measured disc and the expected apparent size of the sun */
  def fromDisc(radiusPixels: Double, semiDiameterDegrees: Double): PlateScale =
    PlateScale(radiusPixels / semiDiameterDegrees)

  /** Plate scale computed from the optics : focal length and pixel pitch */
  def fromOptics(focalLengthMillimeters: Double, pixelPitchMicrometers: Double): PlateScale =
    PlateScale(math.toRadians(1d) * focalLengthMillimeters * 1000d / pixelPitchMicrometers)
}

/** One frame of the session, with everything measured on it */
final case class FrameAnalysis(
  path: Path,
  metadata: ShotMetadata,
  sun: Option[SunPosition],
  disc: Option[MeasuredDisc],
  issues: List[String] = Nil
) {
  def name: String                      = path.getFileName.toString
  def shotAt: Option[OffsetDateTime]    = metadata.shotAt
  def instant: Option[Instant]          = metadata.shotAt.map(_.toInstant)
  def isUsable: Boolean                 = sun.isDefined && disc.isDefined
  def phase: FramePhase                 = disc.map(_.phase).getOrElse(FramePhase.Unknown)
  def obscuration: Option[Double]       = disc.flatMap(_.obscuration)
  def position: Option[HorizontalCoordinates] = sun.map(_.apparent)
}
