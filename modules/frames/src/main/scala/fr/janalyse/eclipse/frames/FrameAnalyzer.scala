package fr.janalyse.eclipse.frames

import fr.janalyse.eclipse.astro.SolarEphemeris
import fr.janalyse.eclipse.model.*
import fr.janalyse.sotohp.media.imaging.CircleFitting.Circle
import fr.janalyse.sotohp.media.imaging.DiscDetector.{DiscDetection, DiscDetectorConfig, DiscKind}
import fr.janalyse.sotohp.media.imaging.Rasters.GrayRaster
import fr.janalyse.sotohp.media.imaging.{BasicImaging, DiscDetector, DiscMeasures, RawDecoder}

import java.awt.image.BufferedImage

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, TimeUnit}
import scala.util.{Failure, Success, Try}

final case class FrameAnalysisConfig(
  cacheDirectory: Path = Paths.get(".eclipse-cache"),
  rawDecode: RawDecoder.RawDecodeConfig = RawDecoder.RawDecodeConfig(),
  detector: DiscDetectorConfig = DiscDetectorConfig(),
  atmosphere: AtmosphericConditions = AtmosphericConditions(),
  /** used when the frames carry no GPS position */
  observer: Option[GeoPoint] = None,
  /** when known, the expected disc radius is enforced during the fit */
  plateScale: Option[PlateScale] = None,
  /** below that radius in the analysis copy, the analysis is redone on a larger one */
  minimumAnalysisDiscRadiusPixels: Double = 80d,
  /** upper bound of the adaptive analysis resolution, memory wise */
  maximumAnalysisSize: Int = 4000,
  /** use the exposure metadata to tell the unfiltered frames - the totality ones - apart */
  phaseFromExposure: Boolean = true,
  /** how many stops below the session median an exposure has to be to mean "filter removed" */
  unfilteredExposureDrop: Double = 5d,
  parallelism: Int = 2
)

/** Turns image files into measured frames : metadata, sun position, solar disc, obscuration.
  *
  * Every measurement is done frame by frame and nothing is kept in memory but the results, so a
  * session of several hundreds of 45Mpix RAW files can be processed on an ordinary machine.
  */
object FrameAnalyzer {

  def analyze(path: Path, config: FrameAnalysisConfig = FrameAnalysisConfig()): FrameAnalysis = {
    val issues   = List.newBuilder[String]
    val metadata = ExifReader.read(path) match {
      case Right(found) => found
      case Left(error)  => issues += error; ShotMetadata.empty
    }

    val location = metadata.location.orElse(config.observer)
    if (location.isEmpty) issues += "no GPS position, neither in the metadata nor in the configuration"
    if (metadata.shotAt.isEmpty) issues += "no shooting date found in the metadata"

    val sun = for {
      shotAt   <- metadata.shotAt
      observer <- location
    } yield SolarEphemeris.position(shotAt.toInstant, observer, config.atmosphere)

    var dimensions: Option[(Int, Int)] = None

    val disc = RawDecoder.load(path, config.cacheDirectory, config.rawDecode) match {
      case Left(error)  =>
        issues += error
        None
      case Right(image) =>
        dimensions = Some((image.getWidth, image.getHeight))
        measure(image, config, sun) match {
          case Left(error) =>
            issues += s"disc detection failed : $error"
            None
          case Right(disc) => Some(disc)
        }
    }

    // the metadata of a RAW file does not always carry the dimensions of the decoded image, and the
    // automatic tuning needs them to know how much room there is around the sun in each frame
    val withDimensions = metadata.copy(
      imageWidth = metadata.imageWidth.orElse(dimensions.map(_._1)),
      imageHeight = metadata.imageHeight.orElse(dimensions.map(_._2))
    )

    FrameAnalysis(path = path, metadata = withDimensions, sun = sun, disc = disc, issues = issues.result())
  }

  /** Analyzes a whole session.
    *
    * Two passes : the first one measures the discs freely, which gives the plate scale of the setup
    * (pixels per degree), the second one goes back to the frames whose fit looks doubtful - deep
    * partial phases and totality - and constrains their radius to the expected one.
    */
  def analyzeAll(
    paths: Seq[Path],
    config: FrameAnalysisConfig = FrameAnalysisConfig(),
    onProgress: (Int, Int, FrameAnalysis) => Unit = (_, _, _) => ()
  ): (List[FrameAnalysis], Option[PlateScale]) = {
    Files.createDirectories(config.cacheDirectory)
    val firstPass  = withExposurePhases(runAll(paths, config, onProgress), config)
    val plateScale = config.plateScale.orElse(estimatePlateScale(firstPass))
    plateScale match {
      case None        => (firstPass, None)
      case Some(scale) =>
        val refinedConfig = config.copy(plateScale = Some(scale))
        val toRefine      = firstPass.filter(frame => needsRefinement(frame, scale)).map(_.path).toSet
        if (toRefine.isEmpty) (firstPass, plateScale)
        else {
          val refined  = withExposurePhases(runAll(paths.filter(toRefine.contains), refinedConfig, onProgress), config)
            .map(frame => frame.path -> frame)
            .toMap
          val combined = firstPass.map(frame => refined.getOrElse(frame.path, frame))
          (combined, plateScale)
        }
    }
  }

  /** Marks as totality the frames shot with the filter removed.
    *
    * The exposure tells it far better than the image does : an eclipse session has two very
    * distinct exposure families, and everything far below the main one was shot bare lens. A frame
    * showing an unmistakable limb - the diamond ring, the first seconds of the partial phase after
    * third contact - keeps its measured phase, since its geometry was correctly fitted.
    */
  def withExposurePhases(frames: List[FrameAnalysis], config: FrameAnalysisConfig): List[FrameAnalysis] = {
    if (!config.phaseFromExposure) frames
    else {
      val exposureValues = frames.flatMap(_.metadata.exposureValue).sorted
      if (exposureValues.sizeIs < 3) frames
      else {
        val median = exposureValues(exposureValues.size / 2)
        frames.map { frame =>
          val unfiltered = frame.metadata.exposureValue.exists(_ <= median - config.unfilteredExposureDrop)
          frame.disc match {
            case Some(disc) if unfiltered && disc.limbContrast < 0.6d =>
              frame.copy(disc = Some(disc.copy(phase = FramePhase.Totality, obscuration = Some(1d))))
            case _                                                    => frame
          }
        }
      }
    }
  }

  /** Pixels per degree of the setup, from the frames where the solar limb is best visible */
  def estimatePlateScale(frames: Seq[FrameAnalysis]): Option[PlateScale] = {
    val samples = frames.flatMap { frame =>
      for {
        disc <- frame.disc
        sun  <- frame.sun
        if disc.phase == FramePhase.Partial
        if disc.obscuration.forall(_ < 0.6d)
        if disc.detectionConfidence > 0.5d
      } yield disc.radiusPixels / sun.semiDiameterDegrees
    }
    if (samples.isEmpty) None
    else {
      val sorted = samples.sorted
      Some(PlateScale(sorted(sorted.size / 2)))
    }
  }

  private def needsRefinement(frame: FrameAnalysis, plateScale: PlateScale): Boolean =
    frame.disc match {
      case None       => true
      case Some(disc) =>
        val expected = frame.sun.map(sun => plateScale.pixelsFor(sun.semiDiameterDegrees))
        disc.phase == FramePhase.Totality ||
        disc.detectionConfidence < 0.5d ||
        expected.exists(value => math.abs(disc.radiusPixels - value) / value > 0.05d)
    }

  private def expectedRadiusPixels(config: FrameAnalysisConfig, sun: Option[SunPosition]): Option[Double] =
    for {
      scale    <- config.plateScale
      position <- sun
    } yield scale.pixelsFor(position.semiDiameterDegrees)

  /** Measures the solar disc, adapting the analysis resolution to the size of the subject.
    *
    * The analysis is normally done on a downscaled copy, which is plenty when the sun fills a good
    * part of the frame. On a wider shot the disc would end up a few dozen pixels wide and the
    * measurement would get coarse, so the analysis is simply redone on a larger copy.
    */
  private def measure(image: BufferedImage, config: FrameAnalysisConfig, sun: Option[SunPosition]): Either[String, MeasuredDisc] = {
    def attempt(analysisMaxSize: Int): Either[String, (DiscDetection, GrayRaster, Double)] = {
      val analysed       = BasicImaging.fitWithin(image, analysisMaxSize)
      val scale          = image.getWidth.toDouble / analysed.getWidth
      val raster         = GrayRaster.fromImage(analysed)
      val detectorConfig = config.detector.copy(
        analysisMaxSize = analysisMaxSize,
        expectedRadiusPixels = expectedRadiusPixels(config, sun).map(_ / scale)
      )
      DiscDetector.detectOnRaster(raster, detectorConfig).map(detection => (detection, raster, scale))
    }

    attempt(config.detector.analysisMaxSize)
      .flatMap { first =>
        val (detection, _, scale) = first
        val wanted                = config.minimumAnalysisDiscRadiusPixels
        if (detection.circle.radius >= wanted || scale <= 1.0001d) Right(first)
        else {
          val enlarged = math.min(
            math.min(config.maximumAnalysisSize, math.max(image.getWidth, image.getHeight)),
            (config.detector.analysisMaxSize * math.min(scale, wanted / math.max(1d, detection.circle.radius))).toInt
          )
          if (enlarged <= config.detector.analysisMaxSize) Right(first)
          else attempt(enlarged).orElse(Right(first))
        }
      }
      .map { case (detection, raster, scale) =>
        measuredDisc(detection, raster, scale, image.getWidth, image.getHeight)
      }
  }

  private def measuredDisc(
    detection: DiscDetection,
    raster: GrayRaster,
    scale: Double,
    imageWidth: Int,
    imageHeight: Int
  ): MeasuredDisc = {
    val phase       = detection.kind match {
      case DiscKind.Photosphere => FramePhase.Partial
      case DiscKind.Corona      => FramePhase.Totality
    }
    val obscuration = phase match {
      // the detector threshold is reused, so that what counts as covered here is exactly what the
      // detector considered as unlit when it looked for the limb
      case FramePhase.Partial => Some(DiscMeasures.obscuration(raster, detection.circle, Some(detection.thresholdLevel)))
      case _                  => Some(1d)
    }
    val centerX     = detection.circle.centerX * scale
    val centerY     = detection.circle.centerY * scale
    val radius      = detection.circle.radius * scale
    val signalRadius = DiscMeasures.signalExtentRadius(raster, detection.circle).map(_ * scale)
    val room         =
      if (radius <= 0d) None
      else Some(List(centerX, centerY, imageWidth - centerX, imageHeight - centerY).min / radius)

    MeasuredDisc(
      centerX = centerX,
      centerY = centerY,
      radiusPixels = radius,
      phase = phase,
      obscuration = obscuration,
      fitResidualPixels = detection.residualRms * scale,
      detectionConfidence = confidenceOf(detection),
      limbContrast = detection.limbContrast,
      signalRadiusPixels = signalRadius,
      roomFactor = room
    )
  }

  private def confidenceOf(detection: DiscDetection): Double =
    detection.kind match {
      case DiscKind.Corona      => 0.4d
      case DiscKind.Photosphere =>
        val inlierRatio  = if (detection.boundaryPointCount == 0) 0d else detection.inlierCount.toDouble / detection.boundaryPointCount
        val residualRatio = if (detection.circle.radius <= 0d) 1d else detection.residualRms / detection.circle.radius
        math.max(0d, math.min(1d, inlierRatio * (1d - math.min(1d, residualRatio * 20d))))
    }

  private def runAll(
    paths: Seq[Path],
    config: FrameAnalysisConfig,
    onProgress: (Int, Int, FrameAnalysis) => Unit
  ): List[FrameAnalysis] = {
    val total    = paths.size
    val counter  = AtomicInteger(0)
    val executor = Executors.newFixedThreadPool(math.max(1, config.parallelism))
    try {
      val futures = paths.map { path =>
        path -> executor.submit[FrameAnalysis] { () =>
          val analysis = Try(analyze(path, config)) match {
            case Success(found) => found
            case Failure(error) =>
              FrameAnalysis(path, ShotMetadata.empty, None, None, List(s"analysis failed : ${error.getMessage}"))
          }
          onProgress(counter.incrementAndGet(), total, analysis)
          analysis
        }
      }
      futures.map { case (_, future) => future.get() }.toList
    } finally {
      executor.shutdown()
      executor.awaitTermination(1L, TimeUnit.MINUTES)
    }
  }
}
