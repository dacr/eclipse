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
  /** position to use for the frames that carry no GPS fix, and for the whole session when
    * `consolidateLocation` is on
    */
  observer: Option[GeoPoint] = None,
  /** every frame of a session comes from the same place : one position is consolidated over the
    * whole session and used for all of them, which fills the frames without a fix and irons out
    * the GPS wandering
    */
  consolidateLocation: Boolean = true,
  /** when known, the expected disc radius is enforced during the fit */
  plateScale: Option[PlateScale] = None,
  /** below that radius in the analysis copy, the analysis is redone on a larger one */
  minimumAnalysisDiscRadiusPixels: Double = 80d,
  /** upper bound of the adaptive analysis resolution, memory wise */
  maximumAnalysisSize: Int = 4000,
  /** measure and draw from the RAW file when a shot also exists as a JPEG, provided a converter is
    * installed - the JPEG is used otherwise, and does the job perfectly well
    */
  preferRawPixels: Boolean = true,
  /** use the exposure metadata to tell the unfiltered frames - the totality ones - apart */
  phaseFromExposure: Boolean = true,
  /** how many stops below the session median an exposure has to be to mean "filter removed" :
    * a very dense filter and a bare lens on the corona are only a few stops apart
    */
  unfilteredExposureDrop: Double = 2.5d,
  parallelism: Int = 2
)

/** What a whole session gave : the measured frames, and what was worked out about the session
  * itself along the way.
  *
  * @param anchorIndex where the analysis started from, in the chronological order of the frames
  */
final case class SessionAnalysis(
  frames: List[FrameAnalysis],
  plateScale: Option[PlateScale],
  location: SessionLocation.Consolidation,
  anchorIndex: Int
) {
  def anchor: Option[FrameAnalysis] = frames.lift(anchorIndex)
}

/** Turns image files into measured frames : metadata, sun position, solar disc, obscuration.
  *
  * Every measurement is done frame by frame and nothing is kept in memory but the results, so a
  * session of several hundreds of 45Mpix RAW files can be processed on an ordinary machine.
  */
object FrameAnalyzer {

  /** What is known of the subject before looking at a frame, propagated from the previous one */
  final case class FramePrior(centerX: Double, centerY: Double, radiusPixels: Double)

  def analyze(
    path: Path,
    config: FrameAnalysisConfig,
    prior: Option[FramePrior]
  ): FrameAnalysis = analyzeShot(Shot.of(path), config, prior)

  def analyze(path: Path, config: FrameAnalysisConfig): FrameAnalysis = analyzeShot(Shot.of(path), config, None)

  def analyze(path: Path): FrameAnalysis = analyzeShot(Shot.of(path), FrameAnalysisConfig(), None)

  /** Measures one shot : its metadata comes from all of its files, its pixels from the best one */
  def analyzeShot(
    shot: Shot,
    config: FrameAnalysisConfig = FrameAnalysisConfig(),
    prior: Option[FramePrior] = None
  ): FrameAnalysis = {
    val issues                     = List.newBuilder[String]
    val (metadata, metadataIssues) = ExifReader.readAll(shot.metadataSources)
    // when a shot is written as RAW+JPEG, one of the two often refuses to be read - a recent RAW
    // container is not understood by every reader. That is only worth reporting if the other file
    // did not make up for it : a shot that knows when and where it was taken has no problem.
    if (metadata.shotAt.isEmpty || metadata.location.isEmpty) metadataIssues.foreach(issues += _)
    val candidates                 = shot.pixelSources(config.preferRawPixels && rawDecodingAvailable(config))

    // the session position, when there is one, is preferred over the fix of this very frame : the
    // camera did not move, so the consolidated position is the more trustworthy of the two
    val location =
      if (config.consolidateLocation) config.observer.orElse(metadata.location)
      else metadata.location.orElse(config.observer)
    if (location.isEmpty) issues += "no GPS position, neither in the metadata nor in the configuration"
    if (metadata.shotAt.isEmpty) issues += "no shooting date found in the metadata"

    val sun = for {
      shotAt   <- metadata.shotAt
      observer <- location
    } yield SolarEphemeris.position(shotAt.toInstant, observer, config.atmosphere)

    // the preferred file is tried first, and the others are there in case it cannot be read at all
    val loaded = candidates.foldLeft(Option.empty[(Path, java.awt.image.BufferedImage)]) { (found, candidate) =>
      found.orElse {
        RawDecoder.load(candidate, config.cacheDirectory, config.rawDecode) match {
          case Right(image) =>
            if (candidate != candidates.head) issues += s"${candidates.head.getFileName} could not be read, ${candidate.getFileName} used instead"
            Some((candidate, image))
          case Left(error)  =>
            issues += error
            None
        }
      }
    }

    val path = loaded.map(_._1).getOrElse(candidates.head)
    val disc = loaded.flatMap { case (_, image) =>
      measure(image, config, sun, prior) match {
        case Left(error) =>
          issues += s"disc detection failed : $error"
          None
        case Right(disc) => Some(disc)
      }
    }

    // the dimensions are those of the image actually measured : a shot may also exist as a smaller
    // JPEG, whose EXIF dimensions would make the room around the sun look different than it is
    val dimensions     = loaded.map { case (_, image) => (image.getWidth, image.getHeight) }
    val withDimensions = metadata.copy(
      imageWidth = dimensions.map(_._1).orElse(metadata.imageWidth),
      imageHeight = dimensions.map(_._2).orElse(metadata.imageHeight),
      location = location
    )

    FrameAnalysis(path = path, metadata = withDimensions, sun = sun, disc = disc, issues = issues.result())
  }

  /** Analyzes a whole session, starting from totality and working outwards.
    *
    * The order matters. A frame taken during totality is the anchor : it is the reference moment of
    * the whole session, and it is also the hardest frame to measure, since there is no photosphere
    * left to fit. It is found beforehand from the exposure metadata alone - the unfiltered frames
    * stand a few stops apart from the rest of the session - so no image has to be looked at first.
    *
    * From that anchor the session is walked in both directions, each frame handing over what it
    * measured to the next one : one shot every 30 seconds means the sun barely moved in between, so
    * the previous position is an excellent starting point, and the thin crescents around totality
    * are then measured with the whole geometry already known. A re-framing simply invalidates the
    * hint, which is then ignored.
    *
    * RAW decoding runs ahead in the background : it is the expensive part, it is cached, and it does
    * not depend on the measurements.
    */
  def analyzeAll(
    paths: Seq[Path],
    config: FrameAnalysisConfig = FrameAnalysisConfig(),
    onProgress: (Int, Int, FrameAnalysis) => Unit = (_, _, _) => ()
  ): SessionAnalysis = {
    Files.createDirectories(config.cacheDirectory)
    // a shot written as RAW+JPEG is one shot, not two : the files are grouped by name first
    val shots = Shot.group(paths)
    if (shots.isEmpty) SessionAnalysis(Nil, None, SessionLocation.consolidate(Nil, config.observer), 0)
    else {
      val metadata    = readAllMetadata(shots, config)
      val ordered     = metadata.sortBy { case (_, found) => found.shotAt.map(_.toInstant.toEpochMilli).getOrElse(0L) }
      val anchorIndex = anchorOf(ordered.map(_._2), config)
      val seedScale   = config.plateScale.orElse(ordered.flatMap(_._2.opticalPlateScale).headOption)

      // one position for the whole session : the frames without a fix are no longer lost, and the
      // ones with a fix stop wandering by a few meters from one shot to the next
      val consolidation = SessionLocation.consolidate(ordered.map(_._2), config.observer)
      val located       =
        if (config.consolidateLocation) config.copy(observer = consolidation.location)
        else config

      val prefetcher = startPrefetching(ordered.map(_._1), anchorIndex, located)
      val firstPass  =
        try walkFromAnchor(ordered.map(_._1), anchorIndex, seedScale, located, onProgress)
        finally prefetcher.shutdownNow()

      val measured   = withExposurePhases(firstPass.toList, located)
      val plateScale = located.plateScale.orElse(estimatePlateScale(measured)).orElse(seedScale)

      val frames = plateScale match {
        case None        => measured
        case Some(scale) =>
          // one last look at the frames whose fit stayed doubtful, this time with the plate scale
          // of the session and with the neighbours already measured
          val refinedConfig = located.copy(plateScale = Some(scale))
          val byPath        = ordered.map { case (shot, _) => shot.pixelSource(config.preferRawPixels && rawDecodingAvailable(config)) -> shot }.toMap
          val refined       = measured.zipWithIndex.map { case (frame, index) =>
            if (!needsRefinement(frame, scale)) frame
            else {
              val prior = neighbourPrior(measured, index)
              val again = byPath.get(frame.path).map(shot => analyzeShot(shot, refinedConfig, prior)).getOrElse(frame)
              if (again.disc.isDefined) again else frame
            }
          }
          withExposurePhases(refined, located)
      }

      SessionAnalysis(frames, plateScale, consolidation, anchorIndex)
    }
  }

  /** The frames shot with the filter removed, told apart by their exposure alone.
    *
    * Beware of the intuition here : the filter does not make the exposure settings extreme, it makes
    * them ordinary. A ND1000000 turns the sun into something one shoots at 1/125 f/8 100 ISO, while
    * the corona, bare lens, needs a much longer exposure - so the totality frames sit a few stops
    * *below* the exposure value the rest of the session settles on, not twenty.
    *
    * A few stops of difference is not much, so two guards are added : the unfiltered frames can only
    * be a minority of the session, and the exposure alone never decides anything on its own - the
    * image always has the last word when it shows an unmistakable limb.
    */
  def unfilteredIndices(metadata: Seq[ShotMetadata], config: FrameAnalysisConfig): Seq[Int] = {
    val exposureValues = metadata.flatMap(_.exposureValue).sorted
    if (!config.phaseFromExposure || exposureValues.sizeIs < 3) Nil
    else {
      val median     = exposureValues(exposureValues.size / 2)
      val candidates = metadata.zipWithIndex.collect {
        case (found, index) if found.exposureValue.exists(_ <= median - config.unfilteredExposureDrop) => index
      }
      // totality lasts minutes, a session lasts hours : anything else means the exposure varied for
      // some other reason - the sun getting low, a cloud, a change of mind
      if (candidates.size > metadata.size * 0.4d) Nil else candidates
    }
  }

  /** The frame the whole analysis starts from : one taken during totality.
    *
    * Totality is a single continuous stretch of a couple of minutes, so the longest run of
    * consecutive unfiltered frames is taken, and its middle : that lands well inside totality
    * rather than on a diamond ring. Without usable exposure metadata, the middle of the session is
    * used instead - the maximum of an eclipse is rarely far from it.
    */
  def anchorOf(metadata: Seq[ShotMetadata], config: FrameAnalysisConfig): Int =
    longestRun(unfilteredIndices(metadata, config)) match {
      case Some(run) => run(run.size / 2)
      case None      => math.max(0, metadata.size / 2)
    }

  private def longestRun(indices: Seq[Int]): Option[Vector[Int]] = {
    val runs    = Vector.newBuilder[Vector[Int]]
    var current = Vector.empty[Int]
    indices.sorted.foreach { index =>
      if (current.isEmpty || index == current.last + 1) current = current :+ index
      else { runs += current; current = Vector(index) }
    }
    if (current.nonEmpty) runs += current
    runs.result().maxByOption(_.size)
  }

  /** Walks the session outwards from the anchor, both directions at once, handing the measured
    * position of each frame over to the next one.
    */
  private def walkFromAnchor(
    shots: Vector[Shot],
    anchorIndex: Int,
    seedScale: Option[PlateScale],
    config: FrameAnalysisConfig,
    onProgress: (Int, Int, FrameAnalysis) => Unit
  ): Vector[FrameAnalysis] = {
    val results  = new Array[FrameAnalysis](shots.size)
    val counter  = AtomicInteger(0)
    val samples  = scala.collection.mutable.ArrayBuffer.empty[Double]
    val total    = shots.size

    def currentScale: Option[PlateScale] = samples.synchronized {
      if (samples.sizeIs >= 5) {
        val sorted = samples.sorted
        Some(PlateScale(sorted(sorted.size / 2)))
      } else seedScale
    }

    def record(frame: FrameAnalysis): Unit =
      for {
        disc <- frame.disc
        sun  <- frame.sun
        if disc.phase == FramePhase.Partial && disc.detectionConfidence > 0.5d && disc.obscuration.forall(_ < 0.6d)
      } samples.synchronized { samples += disc.radiusPixels / sun.semiDiameterDegrees }

    def measureAt(index: Int, prior: Option[FramePrior]): FrameAnalysis = {
      val frame = Try(analyzeShot(shots(index), config.copy(plateScale = currentScale), prior)) match {
        case Success(found) => found
        case Failure(error) =>
          FrameAnalysis(shots(index).files.head, ShotMetadata.empty, None, None, List(s"analysis failed : ${error.getMessage}"))
      }
      results(index) = frame
      record(frame)
      onProgress(counter.incrementAndGet(), total, frame)
      frame
    }

    val anchor = measureAt(anchorIndex, None)

    def walk(step: Int): Unit = {
      var prior = priorOf(anchor)
      var index = anchorIndex + step
      while (index >= 0 && index < shots.size) {
        val frame = measureAt(index, prior)
        prior = priorOf(frame).orElse(prior)
        index += step
      }
    }

    // the two directions do not share anything but the plate scale samples, so they run together
    val executor = Executors.newFixedThreadPool(2)
    try {
      val forward  = executor.submit[Unit](() => walk(1))
      val backward = executor.submit[Unit](() => walk(-1))
      forward.get()
      backward.get()
    } finally {
      executor.shutdown()
      executor.awaitTermination(1L, TimeUnit.MINUTES)
    }
    results.toVector
  }

  private def priorOf(frame: FrameAnalysis): Option[FramePrior] =
    frame.disc.map(disc => FramePrior(disc.centerX, disc.centerY, disc.radiusPixels))

  /** The measurement of the closest already measured neighbour of a frame */
  private def neighbourPrior(frames: Seq[FrameAnalysis], index: Int): Option[FramePrior] = {
    val before = frames.take(index).reverse.view.flatMap(priorOf).headOption
    val after  = frames.drop(index + 1).view.flatMap(priorOf).headOption
    before.orElse(after)
  }

  /** Metadata of every frame, read in parallel : no image is decoded here, it is only a few
    * kilobytes read per file, and it is what decides where the analysis starts from.
    */
  private def readAllMetadata(shots: Seq[Shot], config: FrameAnalysisConfig): Vector[(Shot, ShotMetadata)] = {
    val executor = Executors.newFixedThreadPool(math.max(1, config.parallelism))
    try {
      val futures = shots.map(shot => shot -> executor.submit[ShotMetadata](() => ExifReader.readAll(shot.metadataSources)._1))
      futures.map { case (shot, future) => shot -> future.get() }.toVector
    } finally {
      executor.shutdown()
      executor.awaitTermination(1L, TimeUnit.MINUTES)
    }
  }

  /** Decodes the RAW files ahead of the measurements, in the walk order, to keep the cache warm */
  private def startPrefetching(shots: Vector[Shot], anchorIndex: Int, config: FrameAnalysisConfig) = {
    val executor = Executors.newFixedThreadPool(math.max(1, config.parallelism))
    val preferRaw = config.preferRawPixels && rawDecodingAvailable(config)
    val order     = anchorIndex +: (1 until shots.size)
      .flatMap(offset => List(anchorIndex + offset, anchorIndex - offset))
      .filter(index => index >= 0 && index < shots.size)
    order.foreach { index =>
      val path = shots(index).pixelSource(preferRaw)
      if (RawDecoder.isRawFile(path)) {
        executor.submit(new Runnable {
          def run(): Unit = Try(RawDecoder.decode(path, config.cacheDirectory, config.rawDecode))
        })
      }
    }
    executor
  }

  /** Whether a RAW converter is installed, computed once : without one, the JPEG of a shot is used */
  private lazy val availableTools = scala.collection.mutable.Map.empty[String, Boolean]

  private def rawDecodingAvailable(config: FrameAnalysisConfig): Boolean =
    availableTools.synchronized {
      availableTools.getOrElseUpdate(config.rawDecode.signature, RawDecoder.availableTools(config.rawDecode).nonEmpty)
    }

  /** The exposure no longer decides the phase : the detector has the image in front of it, and a
    * dark middle inside a ring of light is a far better sign of totality than a low exposure value,
    * which a low sun without its filter shows just as well. What the exposure is still good for is
    * saying where to start the analysis from.
    */
  def withExposurePhases(frames: List[FrameAnalysis], config: FrameAnalysisConfig): List[FrameAnalysis] = frames

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
  private def measure(
    image: BufferedImage,
    config: FrameAnalysisConfig,
    sun: Option[SunPosition],
    prior: Option[FramePrior]
  ): Either[String, MeasuredDisc] = {
    def attempt(analysisMaxSize: Int): Either[String, (DiscDetection, GrayRaster, Double)] = {
      val analysed       = BasicImaging.fitWithin(image, analysisMaxSize)
      val scale          = image.getWidth.toDouble / analysed.getWidth
      val raster         = GrayRaster.fromImage(analysed)
      val detectorConfig = config.detector.copy(
        analysisMaxSize = analysisMaxSize,
        expectedRadiusPixels = expectedRadiusPixels(config, sun).orElse(prior.map(_.radiusPixels)).map(_ / scale),
        expectedCenter = prior.map(found => (found.centerX / scale, found.centerY / scale))
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
      roomFactor = room,
      flattening = detection.flattening
    )
  }

  private def confidenceOf(detection: DiscDetection): Double =
    detection.kind match {
      case DiscKind.Corona      => 0.4d
      case DiscKind.Photosphere =>
        // the spread of the points around their own circle, not their distance to the radius the
        // session imposes : a frame whose disc looks a little smaller - a darker exposure cuts the
        // limb darkening earlier - is measured just as well, and its center is what gets used
        val inlierRatio = if (detection.boundaryPointCount == 0) 0d else detection.inlierCount.toDouble / detection.boundaryPointCount
        val spreadRatio = if (detection.circle.radius <= 0d) 1d else detection.radialSpread / detection.circle.radius
        math.max(0d, math.min(1d, inlierRatio * (1d - math.min(1d, spreadRatio * 20d))))
    }

}
