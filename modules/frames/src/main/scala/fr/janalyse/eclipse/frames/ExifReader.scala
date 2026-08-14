package fr.janalyse.eclipse.frames

import com.drew.imaging.ImageMetadataReader
import com.drew.metadata.Metadata as DrewMetadata
import com.drew.metadata.exif.{ExifDirectoryBase, ExifIFD0Directory, ExifSubIFDDirectory, GpsDirectory}
import fr.janalyse.eclipse.model.{GeoPoint, ShotMetadata}

import java.nio.file.Path
import java.time.format.DateTimeFormatter
import java.time.{Duration, Instant, LocalDateTime, OffsetDateTime, ZoneOffset}
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

/** EXIF metadata extraction.
  *
  * Same approach as sotohp's `OriginalBuilder` (same library, same tag handling), reduced to what a
  * sequence of shots of the sky needs, plus one addition which matters a lot here : when the camera
  * did not record the time zone but did record a GPS fix, the UTC offset is recovered by comparing
  * the local shooting time with the GPS UTC time. Getting the absolute time right is what makes the
  * computed sun positions trustworthy.
  */
object ExifReader {

  private val exifDateTimeFormat = DateTimeFormatter.ofPattern("yyyy:MM:dd HH:mm:ss")

  def read(path: Path): Either[String, ShotMetadata] =
    Try(ImageMetadataReader.readMetadata(path.toFile)) match {
      case Failure(error)    => Left(s"unable to read metadata from $path : ${error.getMessage}")
      case Success(metadata) => Right(extract(metadata))
    }

  def extract(metadata: DrewMetadata): ShotMetadata = {
    val exif    = Option(metadata.getFirstDirectoryOfType(classOf[ExifIFD0Directory]))
    val exifSub = Option(metadata.getFirstDirectoryOfType(classOf[ExifSubIFDDirectory]))
    val gps     = Option(metadata.getFirstDirectoryOfType(classOf[GpsDirectory]))

    ShotMetadata(
      shotAt = shootDateTime(exifSub, gps),
      location = location(gps),
      cameraName = cameraName(exif),
      lensName = exifSub.flatMap(directory => Option(directory.getString(ExifDirectoryBase.TAG_LENS_MODEL))).map(_.trim).filter(_.nonEmpty),
      focalLengthMillimeters = doubleTag(exifSub, ExifDirectoryBase.TAG_FOCAL_LENGTH),
      aperture = doubleTag(exifSub, ExifDirectoryBase.TAG_FNUMBER),
      exposureTimeSeconds = doubleTag(exifSub, ExifDirectoryBase.TAG_EXPOSURE_TIME),
      isoSensitivity = doubleTag(exifSub, ExifDirectoryBase.TAG_ISO_EQUIVALENT),
      imageWidth = intTag(exifSub, ExifDirectoryBase.TAG_EXIF_IMAGE_WIDTH),
      imageHeight = intTag(exifSub, ExifDirectoryBase.TAG_EXIF_IMAGE_HEIGHT),
      pixelPitchMicrometers = pixelPitch(exifSub.orElse(exif))
    )
  }

  /** Sensor pixel pitch, from the focal plane resolution the camera recorded.
    *
    * Together with the focal length it gives the plate scale of the setup - hence the expected size
    * of the solar disc in pixels - without having looked at a single image.
    */
  private def pixelPitch(directory: Option[com.drew.metadata.Directory]): Option[Double] =
    for {
      found      <- directory
      if found.containsTag(ExifDirectoryBase.TAG_FOCAL_PLANE_X_RESOLUTION)
      resolution <- Try(found.getDouble(ExifDirectoryBase.TAG_FOCAL_PLANE_X_RESOLUTION)).toOption
      if resolution > 0d
      unit        = Try(found.getInt(ExifDirectoryBase.TAG_FOCAL_PLANE_RESOLUTION_UNIT)).getOrElse(2)
      micrometers = unit match {
                      case 3 => 10000d // centimeter
                      case 4 => 1000d  // millimeter
                      case 5 => 1d     // micrometer
                      case _ => 25400d // inch, the usual one
                    }
    } yield micrometers / resolution

  /** Shooting instant, with the best time zone information available */
  private def shootDateTime(exifSub: Option[ExifSubIFDDirectory], gps: Option[GpsDirectory]): Option[OffsetDateTime] = {
    for {
      directory <- exifSub
      raw       <- Option(directory.getString(ExifDirectoryBase.TAG_DATETIME_ORIGINAL))
                     .filterNot(_.startsWith("0000:00:00"))
                     .filterNot(_.contains(": "))
      local     <- Try(LocalDateTime.parse(raw.trim, exifDateTimeFormat)).toOption
    } yield {
      val subSecond   = Option(directory.getString(ExifDirectoryBase.TAG_SUBSECOND_TIME_ORIGINAL))
                          .flatMap(value => Try(value.trim.take(3).padTo(3, '0').toInt).toOption)
                          .getOrElse(0)
      val withSubSecond = local.withNano(subSecond * 1000000)
      val offset        = declaredOffset(directory).orElse(offsetFromGps(withSubSecond, gps)).getOrElse(ZoneOffset.UTC)
      withSubSecond.atOffset(offset)
    }
  }

  private def declaredOffset(directory: ExifSubIFDDirectory): Option[ZoneOffset] =
    Option(directory.getString(ExifDirectoryBase.TAG_TIME_ZONE_ORIGINAL))
      .map(_.trim)
      .flatMap(value => Try(ZoneOffset.of(value)).toOption)

  /** The GPS time stamp is UTC : comparing it with the local shooting time gives the offset which
    * was in use, rounded to the nearest quarter of an hour.
    */
  private def offsetFromGps(local: LocalDateTime, gps: Option[GpsDirectory]): Option[ZoneOffset] =
    for {
      directory <- gps
      gpsDate   <- Option(directory.getGpsDate)
      utc        = Instant.ofEpochMilli(gpsDate.getTime).atOffset(ZoneOffset.UTC).toLocalDateTime
      seconds    = Duration.between(utc, local).getSeconds
      if math.abs(seconds) <= 18 * 3600
      rounded    = math.round(seconds / 900d) * 900
      offset    <- Try(ZoneOffset.ofTotalSeconds(rounded.toInt)).toOption
    } yield offset

  private def location(gps: Option[GpsDirectory]): Option[GeoPoint] =
    for {
      directory   <- gps
      geoLocation <- Option(directory.getGeoLocation)
      if !geoLocation.isZero
    } yield GeoPoint(
      latitudeDegrees = geoLocation.getLatitude,
      longitudeDegrees = geoLocation.getLongitude,
      altitudeMeters = if (directory.containsTag(GpsDirectory.TAG_ALTITUDE)) {
        Try(directory.getDouble(GpsDirectory.TAG_ALTITUDE)).getOrElse(0d)
      } else 0d
    )

  private def cameraName(exif: Option[ExifIFD0Directory]): Option[String] =
    for {
      directory <- exif
      maker      = Option(directory.getString(ExifDirectoryBase.TAG_MAKE)).map(_.trim).filter(_.nonEmpty)
      model      = Option(directory.getString(ExifDirectoryBase.TAG_MODEL)).map(_.trim).filter(_.nonEmpty)
      name      <- maker
                     .flatMap(value => model.map(other => s"$value/${other.replaceAll(s"(?i)^$value\\s*", "")}"))
                     .orElse(model)
    } yield name

  private def doubleTag(directory: Option[ExifSubIFDDirectory], tag: Int): Option[Double] =
    directory.filter(_.containsTag(tag)).flatMap(found => Try(found.getDouble(tag)).toOption)

  private def intTag(directory: Option[ExifSubIFDDirectory], tag: Int): Option[Int] =
    directory.filter(_.containsTag(tag)).flatMap(found => Try(found.getInt(tag)).toOption)

  /** Every tag of every directory, handy while exploring a new camera output */
  def allTags(metadata: DrewMetadata): Map[String, String] =
    metadata.getDirectories.asScala
      .flatMap(_.getTags.asScala)
      .filter(tag => tag.hasTagName && tag.getDescription != null)
      .map(tag => s"${tag.getDirectoryName}/${tag.getTagName}" -> tag.getDescription)
      .toMap
}
