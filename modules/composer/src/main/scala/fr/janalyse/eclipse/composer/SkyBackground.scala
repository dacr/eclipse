package fr.janalyse.eclipse.composer

import fr.janalyse.eclipse.astro.{SkyFrame, SolarEphemeris}
import fr.janalyse.eclipse.frames.{ExifReader, Shot}
import fr.janalyse.eclipse.model.*
import fr.janalyse.sotohp.media.imaging.Rasters.GrayRaster
import fr.janalyse.sotohp.media.imaging.{DiscMeasures, LinearAlgebra, RawDecoder}

import java.awt.image.BufferedImage
import java.nio.file.Path
import java.time.{Duration, Instant}

/** A wide angle photograph used as the scenery behind a composite.
  *
  * A sequence of long lens shots of the sun says nothing about where the sun was : each frame is a
  * disc on a black sky, and the composite places them by computation alone. A single wide shot of
  * the same sky, taken from the same place, brings back everything the long lens could not hold -
  * the horizon, the trees, the plain, the colour of the air - and it is enough to place it once for
  * the whole geometry to follow, because the sun in it is the same sun.
  *
  * The frame is therefore anchored on its own sun : knowing where the sun was in the sky at that
  * instant, and where it landed on that picture, fixes the direction the camera was pointed at.
  * Nothing else needs to be known about how the picture was taken - only the plate scale, which the
  * focal length and the pixel pitch give away, and the roll, which is zero for a camera held level.
  *
  * @param frame           where the camera pointed, and how it was rolled
  * @param pixelsPerDegree plate scale of that picture, at the middle of it
  * @param brightness      applied when drawing, to push the scenery back behind the subject
  */
final case class SkyBackground(
  image: BufferedImage,
  frame: SkyFrame,
  pixelsPerDegree: Double,
  brightness: Double = 1d
) {
  def width: Int  = image.getWidth
  def height: Int = image.getHeight

  /** The optical axis goes through the middle of the picture */
  def centerX: Double = width / 2d
  def centerY: Double = height / 2d

  def pixelsPerRadian: Double = pixelsPerDegree * 180d / math.Pi

  def fieldWidthDegrees: Double  = 2d * math.toDegrees(math.atan(centerX / pixelsPerRadian))
  def fieldHeightDegrees: Double = 2d * math.toDegrees(math.atan(centerY / pixelsPerRadian))

  /** Tangent plane coordinates of this camera, turned into its own pixels */
  def tangentToPixel: Array[Double] =
    LinearAlgebra.affine3x3(pixelsPerRadian, -pixelsPerRadian, centerX, centerY)

  /** Where a pixel of this picture points in the sky */
  def skyAt(x: Double, y: Double): HorizontalCoordinates =
    frame.skyAt((x - centerX) / pixelsPerRadian, (centerY - y) / pixelsPerRadian)

  /** The row of the picture where the true horizon runs, at the middle of it.
    *
    * The one number that checks the whole placement without any faith being asked for : the sun,
    * the clock, the plate scale and the roll all come together into a line the picture itself shows.
    * It should land right where the ground meets the sky, a fraction of a degree above it in fact,
    * since a viewpoint standing above the surroundings sees its horizon a little lower than level.
    */
  def horizonRow: Option[Double] =
    frame
      .project(HorizontalCoordinates(frame.pointing.azimuthDegrees, 0d))
      .map { case (_, y) => centerY - y * pixelsPerRadian }

  /** The map taking canvas pixels straight to pixels of this picture.
    *
    * Three changes of coordinates chained into a single matrix : canvas pixels to the tangent plane
    * of the composite, that plane to the tangent plane of this camera - the projective part, and the
    * only one which is not a mere scaling - then that plane to the pixels of this picture.
    */
  def canvasMatrix(compositeFrame: SkyFrame, canvasToTangentPlane: Array[Double]): Array[Double] =
    LinearAlgebra.multiply3x3(
      tangentToPixel,
      LinearAlgebra.multiply3x3(compositeFrame.tangentMapTo(frame), canvasToTangentPlane)
    )

  /** The area of the composite this picture is sure to cover, in the coordinates the placements use :
    * `x` growing right, `y` growing down, in pixels at the given scale, as (left, top, right, bottom).
    *
    * Two pictures of the same sky taken from the same place are never squared with one another - the
    * map from one to the other turns a rectangle into a slightly tilted quadrilateral - so the
    * rectangle which *contains* that quadrilateral has empty corners, and growing a canvas onto it
    * would frame a black wedge along the edge. What is measured here is therefore the rectangle
    * which *fits inside* it : each border of the picture is walked, and the one that bites furthest
    * into the frame sets the limit on its side.
    */
  def coveredArea(compositeFrame: SkyFrame, pixelsPerRadian: Double, samples: Int = 128): Option[(Double, Double, Double, Double)] = {
    val steps = math.max(2, samples / 4)
    val along = (0 to steps).map(_.toDouble / steps)

    def edge(pixels: Seq[(Double, Double)]): Seq[(Double, Double)] = pixels.flatMap { case (x, y) =>
      compositeFrame
        .project(frame.directionAt((x - centerX) / this.pixelsPerRadian, (centerY - y) / this.pixelsPerRadian))
        .map { case (x, y) => (x * pixelsPerRadian, -y * pixelsPerRadian) }
    }

    val top    = edge(along.map(ratio => (ratio * (width - 1), 0d)))
    val bottom = edge(along.map(ratio => (ratio * (width - 1), height - 1d)))
    val left   = edge(along.map(ratio => (0d, ratio * (height - 1))))
    val right  = edge(along.map(ratio => (width - 1d, ratio * (height - 1))))

    for {
      leftMost   <- left.map(_._1).maxOption
      topMost    <- top.map(_._2).maxOption
      rightMost  <- right.map(_._1).minOption
      bottomMost <- bottom.map(_._2).minOption
      if leftMost < rightMost && topMost < bottomMost
    } yield (leftMost, topMost, rightMost, bottomMost)
  }
}

object SkyBackground {

  /** What is known, or asked for, before the background frame is read */
  final case class Request(
    path: Path,
    cacheDirectory: Path,
    observer: Option[GeoPoint] = None,
    atmosphere: AtmosphericConditions = AtmosphericConditions(),
    /** when the rest of the session was shot, used to put a wrong camera clock right */
    sessionSpan: Option[(Instant, Instant)] = None,
    rollDegrees: Double = 0d,
    brightness: Double = 1d,
    /** where the sun sits on that picture, when it is not to be looked for */
    sunPixel: Option[(Double, Double)] = None,
    /** plate scale of that picture, when the optics did not say enough */
    pixelsPerDegree: Option[Double] = None,
    preferRawPixels: Boolean = true,
    rawDecode: RawDecoder.RawDecodeConfig = RawDecoder.RawDecodeConfig()
  )

  /** A placed background, and the account of how it was placed - every number of which is worth
    * checking by eye on the result
    */
  final case class Loaded(background: SkyBackground, sun: SunPosition, sunPixel: (Double, Double), report: List[String])

  def load(request: Request): Either[String, Loaded] = {
    val shot                = Shot.around(request.path)
    val (metadata, _)       = ExifReader.readAll(shot.metadataSources)
    val notes               = List.newBuilder[String]

    for {
      image    <- decode(shot, request)
      declared <- metadata.shotAt.map(_.toInstant).toRight(s"${request.path} : no shooting date in the metadata")
      observer <- request.observer.orElse(metadata.location).toRight(s"${request.path} : no observer position, give one with --observer")
      instant   = resolveInstant(declared, observer, request, notes)
      sun       = SolarEphemeris.position(instant, observer, request.atmosphere)
      scale    <- plateScale(metadata, image, request).toRight(
                    s"${request.path} : the plate scale of that picture is unknown, the focal length or the sensor pitch is missing from its metadata, give it with --background-scale"
                  )
      sunPixel <- request.sunPixel.orElse(findSun(image)).toRight(
                    s"${request.path} : the sun could not be found on that picture, point at it with --background-sun x,y"
                  )
    } yield {
      val frame = pointingFrom(sun.apparent, sunPixel, image, scale, request.rollDegrees)
      val found = SkyBackground(image, frame, scale, request.brightness)
      notes += f"background sun found at ${sunPixel._1}%.0f,${sunPixel._2}%.0f, where it stood at ${sun.apparent}"
      notes += f"background scale ${scale}%.1f px/° (solar disc ${scale * 0.53d}%.0f px), field ${found.fieldWidthDegrees}%.1f° x ${found.fieldHeightDegrees}%.1f°"
      notes += f"background aimed at ${frame.pointing}, roll ${request.rollDegrees}%.1f°"
      found.horizonRow.foreach(row =>
        notes += f"background horizon runs at y=${row}%.0f of ${found.height} : look at the picture, the ground should start just below it"
      )
      Loaded(found, sun, sunPixel, notes.result())
    }
  }

  /** Where the camera was pointed, deduced from one known star and its place on the picture.
    *
    * The unknown is the direction of the axis, and what is known is where a given direction of the
    * sky landed relative to it. Rather than inverting that relation in spherical trigonometry, the
    * axis is walked to its place : it starts at the sun and, at each step, the error between where
    * the sun should project and where it does is read as a displacement of the axis in its own
    * tangent plane. The projection being smooth and the error small from the very first step, a
    * handful of steps land far below the pixel.
    */
  def pointingFrom(
    sun: HorizontalCoordinates,
    sunPixel: (Double, Double),
    image: BufferedImage,
    pixelsPerDegree: Double,
    rollDegrees: Double,
    steps: Int = 8
  ): SkyFrame = {
    val pixelsPerRadian = pixelsPerDegree * 180d / math.Pi
    val targetX         = (sunPixel._1 - image.getWidth / 2d) / pixelsPerRadian
    val targetY         = (image.getHeight / 2d - sunPixel._2) / pixelsPerRadian
    var frame           = SkyFrame(sun, rollDegrees)
    var step            = 0
    while (step < steps) {
      frame = frame.project(sun) match {
        case Some((x, y)) => SkyFrame(frame.skyAt(x - targetX, y - targetY), rollDegrees)
        case None         => frame
      }
      step += 1
    }
    frame
  }

  /** The sun on a wide angle frame : the brightest compact thing there is */
  def findSun(image: BufferedImage): Option[(Double, Double)] =
    DiscMeasures.brightestSpot(GrayRaster.fromImage(image)).map(spot => (spot.centerX, spot.centerY))

  /** The instant that picture was taken, a wrong time zone put right.
    *
    * A camera whose clock was left on another time zone records the right wall clock and the wrong
    * offset, which is a whole number of hours away from the truth - and an hour, for a sun a couple
    * of degrees above the horizon, is the difference between broad daylight and the middle of the
    * night. Two facts settle it : the sun is visible on the picture, so it was above the horizon,
    * and the picture belongs to the session, so it was taken around it. Only whole hours are ever
    * considered, since that is the only error a time zone can make.
    */
  def resolveInstant(
    declared: Instant,
    observer: GeoPoint,
    request: Request,
    notes: scala.collection.mutable.Builder[String, List[String]]
  ): Instant = {
    def altitudeAt(instant: Instant): Double =
      SolarEphemeris.position(instant, observer, request.atmosphere).apparent.altitudeDegrees

    def distanceToSession(instant: Instant): Long =
      request.sessionSpan match {
        case None                 => 0L
        case Some((first, last))  =>
          if (instant.isBefore(first)) Duration.between(instant, first).getSeconds
          else if (instant.isAfter(last)) Duration.between(last, instant).getSeconds
          else 0L
      }

    val shifted = (-12 to 12).map(hours => (hours, declared.plusSeconds(hours * 3600L)))
    val lit     = shifted.filter { case (_, instant) => altitudeAt(instant) > -0.5d }

    // the smallest correction which fits the evidence, not the best fitting one : an hour off is an
    // hour off, and a session that ends at sunset would happily welcome a picture taken two hours
    // earlier - in the middle of it, and in broad daylight.
    val chosen = lit
      .filter { case (_, instant) => distanceToSession(instant) <= 3600L }
      .minByOption { case (hours, _) => math.abs(hours) }
      .orElse(lit.minByOption { case (hours, _) => math.abs(hours) })

    chosen match {
      case Some((0, _))           => declared
      case Some((hours, instant)) =>
        notes += f"background clock corrected by $hours%+d h : as declared the sun stood ${altitudeAt(declared)}%.1f° above the horizon, which is not what that picture shows"
        instant
      case None                   =>
        notes += "background clock left as declared, no whole hour correction puts the sun above the horizon"
        declared
    }
  }

  /** Plate scale of the background frame, from its optics, corrected for what the decoder gave.
    *
    * The pixel pitch is that of the sensor, so the scale it yields holds for the native resolution
    * of the camera : a RAW decoder which hands back a slightly different size - they all crop or
    * keep the borders as they see fit - would otherwise shift the scale by that ratio.
    */
  private def plateScale(metadata: ShotMetadata, image: BufferedImage, request: Request): Option[Double] =
    request.pixelsPerDegree.orElse {
      metadata.opticalPlateScale.map { scale =>
        val ratio = metadata.imageWidth.filter(_ > 0).map(image.getWidth.toDouble / _).getOrElse(1d)
        scale.pixelsPerDegree * ratio
      }
    }

  private def decode(shot: Shot, request: Request): Either[String, BufferedImage] =
    shot
      .pixelSources(request.preferRawPixels)
      .foldLeft(Left(s"${request.path} : no readable image"): Either[String, BufferedImage]) { (found, candidate) =>
        found.orElse(RawDecoder.load(candidate, request.cacheDirectory, request.rawDecode))
      }

  /** Everything a session says about where and when it happened, to place a background with */
  def sessionContext(frames: Seq[FrameAnalysis]): (Option[GeoPoint], Option[(Instant, Instant)]) = {
    val instants = frames.flatMap(_.instant).sorted
    val span     = for {
      first <- instants.headOption
      last  <- instants.lastOption
    } yield (first, last)
    (frames.flatMap(_.metadata.location).headOption, span)
  }
}
