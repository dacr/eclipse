package fr.janalyse.eclipse.composer

import fr.janalyse.eclipse.astro.SolarEphemeris
import fr.janalyse.eclipse.model.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.OffsetDateTime
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** Measurements persistence.
  *
  * Analyzing a session (RAW decoding included) is by far the longest step, while trying layouts and
  * renderings is where the back and forth happens. Measurements are therefore saved as a plain CSV
  * file, easy to re-read, easy to inspect, easy to fix by hand when one frame went wrong.
  *
  * Sun positions are not stored : they are recomputed from the instant and the position, which
  * keeps the file small and always consistent with the ephemeris code.
  */
object FrameStore {

  private val header = List(
    "path",
    "shotAt",
    "latitude",
    "longitude",
    "altitude",
    "camera",
    "lens",
    "focalLength",
    "aperture",
    "exposureTime",
    "iso",
    "imageWidth",
    "imageHeight",
    "discCenterX",
    "discCenterY",
    "discRadius",
    "phase",
    "obscuration",
    "fitResidual",
    "confidence",
    "limbContrast",
    "signalRadius",
    "roomFactor",
    "issues"
  )

  def toCsv(frames: Seq[FrameAnalysis]): String = {
    val lines = frames.map { frame =>
      val metadata = frame.metadata
      val disc     = frame.disc
      List(
        frame.path.toString,
        metadata.shotAt.map(_.toString).getOrElse(""),
        metadata.location.map(_.latitudeDegrees.toString).getOrElse(""),
        metadata.location.map(_.longitudeDegrees.toString).getOrElse(""),
        metadata.location.map(_.altitudeMeters.toString).getOrElse(""),
        metadata.cameraName.getOrElse(""),
        metadata.lensName.getOrElse(""),
        metadata.focalLengthMillimeters.map(_.toString).getOrElse(""),
        metadata.aperture.map(_.toString).getOrElse(""),
        metadata.exposureTimeSeconds.map(_.toString).getOrElse(""),
        metadata.isoSensitivity.map(_.toString).getOrElse(""),
        metadata.imageWidth.map(_.toString).getOrElse(""),
        metadata.imageHeight.map(_.toString).getOrElse(""),
        disc.map(_.centerX.toString).getOrElse(""),
        disc.map(_.centerY.toString).getOrElse(""),
        disc.map(_.radiusPixels.toString).getOrElse(""),
        disc.map(_.phase.toString).getOrElse(""),
        disc.flatMap(_.obscuration).map(_.toString).getOrElse(""),
        disc.map(_.fitResidualPixels.toString).getOrElse(""),
        disc.map(_.detectionConfidence.toString).getOrElse(""),
        disc.map(_.limbContrast.toString).getOrElse(""),
        disc.flatMap(_.signalRadiusPixels).map(_.toString).getOrElse(""),
        disc.flatMap(_.roomFactor).map(_.toString).getOrElse(""),
        frame.issues.mkString(" | ")
      ).map(escape).mkString(";")
    }
    (header.mkString(";") +: lines).mkString("\n") + "\n"
  }

  def save(path: Path, frames: Seq[FrameAnalysis]): Unit = {
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.write(path, toCsv(frames).getBytes(StandardCharsets.UTF_8))
  }

  def load(
    path: Path,
    observer: Option[GeoPoint] = None,
    conditions: AtmosphericConditions = AtmosphericConditions()
  ): Either[String, List[FrameAnalysis]] =
    Try {
      val lines = Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toList
      lines match {
        case Nil            => Left(s"empty measurements file : $path")
        case _ :: dataLines =>
          Right(dataLines.filter(_.trim.nonEmpty).map(line => fromCsvLine(line, observer, conditions)))
      }
    }.toEither.left.map(error => s"unable to read $path : ${error.getMessage}").flatten

  private def fromCsvLine(line: String, observer: Option[GeoPoint], conditions: AtmosphericConditions): FrameAnalysis = {
    val columns  = split(line)
    def at(index: Int): Option[String] = columns.lift(index).map(_.trim).filter(_.nonEmpty)
    def number(index: Int): Option[Double] = at(index).flatMap(value => Try(value.toDouble).toOption)

    val location = for {
      latitude  <- number(2)
      longitude <- number(3)
    } yield GeoPoint(latitude, longitude, number(4).getOrElse(0d))

    val metadata = ShotMetadata(
      shotAt = at(1).flatMap(value => Try(OffsetDateTime.parse(value)).toOption),
      location = location.orElse(observer),
      cameraName = at(5),
      lensName = at(6),
      focalLengthMillimeters = number(7),
      aperture = number(8),
      exposureTimeSeconds = number(9),
      isoSensitivity = number(10),
      imageWidth = number(11).map(_.toInt),
      imageHeight = number(12).map(_.toInt)
    )

    val disc = for {
      centerX <- number(13)
      centerY <- number(14)
      radius  <- number(15)
    } yield MeasuredDisc(
      centerX = centerX,
      centerY = centerY,
      radiusPixels = radius,
      phase = at(16).flatMap(value => Try(FramePhase.valueOf(value)).toOption).getOrElse(FramePhase.Unknown),
      obscuration = number(17),
      fitResidualPixels = number(18).getOrElse(0d),
      detectionConfidence = number(19).getOrElse(1d),
      limbContrast = number(20).getOrElse(0d),
      signalRadiusPixels = number(21),
      roomFactor = number(22)
    )

    val sun = for {
      shotAt   <- metadata.shotAt
      position <- metadata.location
    } yield SolarEphemeris.position(shotAt.toInstant, position, conditions)

    FrameAnalysis(
      path = Paths.get(at(0).getOrElse("")),
      metadata = metadata,
      sun = sun,
      disc = disc,
      issues = at(23).map(_.split(""" \| """).toList).getOrElse(Nil)
    )
  }

  private def escape(value: String): String = {
    val cleaned = value.replace("\n", " ").replace("\r", " ")
    if (cleaned.contains(";") || cleaned.contains("\"")) "\"" + cleaned.replace("\"", "\"\"") + "\""
    else cleaned
  }

  private def split(line: String): Vector[String] = {
    val columns = Vector.newBuilder[String]
    val current = StringBuilder()
    var quoted  = false
    var index   = 0
    while (index < line.length) {
      val character = line.charAt(index)
      if (quoted) {
        if (character == '"') {
          if (index + 1 < line.length && line.charAt(index + 1) == '"') { current.append('"'); index += 1 }
          else quoted = false
        } else current.append(character)
      } else {
        if (character == '"') quoted = true
        else if (character == ';') { columns += current.result(); current.clear() }
        else current.append(character)
      }
      index += 1
    }
    columns += current.result()
    columns.result()
  }
}
