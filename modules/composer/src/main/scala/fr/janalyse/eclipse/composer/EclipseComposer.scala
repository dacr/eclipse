package fr.janalyse.eclipse.composer

import fr.janalyse.eclipse.composer.FrameSelector.{Selection, SelectionConfig}
import fr.janalyse.eclipse.model.{FrameAnalysis, FramePhase, exposureValue}
import fr.janalyse.sotohp.media.imaging.RawDecoder

import java.nio.file.Path
import java.time.Duration
import java.time.format.DateTimeFormatter

final case class ComposeConfig(
  cacheDirectory: Path,
  selection: SelectionConfig = SelectionConfig(),
  layout: CompositeLayout = SkyPathLayout(),
  render: RenderConfig = RenderConfig(),
  rawDecode: RawDecoder.RawDecodeConfig = RawDecoder.RawDecodeConfig()
)

final case class ComposeOutcome(
  result: CompositeResult,
  selection: Selection,
  placements: Vector[Placement]
)

/** The whole thing, from measured frames to the final picture */
object EclipseComposer {

  def compose(
    frames: Seq[FrameAnalysis],
    config: ComposeConfig,
    onProgress: (Int, Int) => Unit = (_, _) => ()
  ): Either[String, ComposeOutcome] = {
    val selection = FrameSelector.select(frames, config.selection)
    if (selection.kept.isEmpty) Left(s"no usable frame left after selection\n${diagnose(frames)}")
    else {
      val layoutConfig = LayoutConfig(
        pixelsPerDegree = pixelsPerDegree(frames, config),
        tileRadiusFactor = config.render.tileRadiusFactor,
        totalityTileRadiusFactor = config.render.totalityTileRadiusFactor
      )
      val placed       = config.layout.place(selection.kept, layoutConfig)
      val placements   = if (config.render.stackTotality) withTotalityBursts(placed, frames) else placed
      if (placements.isEmpty) Left("the layout could not place any frame")
      else
        CompositeRenderer
          .render(
            placements,
            config.render,
            config.cacheDirectory,
            config.rawDecode,
            onProgress,
            config.layout.skyFrame(selection.kept)
          )
          .map(result => ComposeOutcome(result, selection, placements))
    }
  }

  /** Hands each totality placement the whole burst it belongs to.
    *
    * During totality one does not take a picture, one takes a bracket : the long exposure reaches
    * the outer corona, the short ones keep the prominences the long one burnt away. Choosing among
    * them means giving up half of what was recorded, so the burst is passed on whole and merged at
    * drawing time.
    *
    * A burst is recognized by time alone : frames of the same totality, taken within seconds of one
    * another. Nothing else in a session looks like that.
    */
  def withTotalityBursts(
    placements: Vector[Placement],
    frames: Seq[FrameAnalysis],
    withinSeconds: Long = 90L
  ): Vector[Placement] = {
    val totality = frames.filter(frame => frame.phase == FramePhase.Totality && frame.isUsable)
    placements.map { placement =>
      if (placement.frame.phase != FramePhase.Totality) placement
      else
        placement.frame.instant match {
          case None          => placement
          case Some(instant) =>
            val burst = totality
              .filter(_.instant.exists(other => math.abs(Duration.between(instant, other).getSeconds) <= withinSeconds))
              .sortBy(_.instant.map(_.toEpochMilli).getOrElse(0L))
              .toList
            val chosen = oneFramePerExposure(burst, instant)
            if (chosen.sizeIs > 1) placement.copy(stack = chosen) else placement
        }
    }
  }

  /** Keeps one frame per exposure level, the one closest in time to the reference.
    *
    * A burst holds repeats - three frames at the same shutter speed, then two at another - and they
    * bring no dynamic range, only a little less noise. Worse, keeping them all widens the span of
    * time being merged, and time is precisely what hurts here : the moon moves in front of the sun,
    * so the further apart two frames are, the less their coronas line up. Keeping one frame per
    * exposure, and the most central one at that, gives the same range over a shorter moment.
    *
    * Levels are told apart at a third of a stop, which is finer than anyone brackets.
    */
  def oneFramePerExposure(burst: List[FrameAnalysis], reference: java.time.Instant): List[FrameAnalysis] =
    burst
      .groupBy(frame => frame.metadata.exposureValue.map(value => math.round(value * 3d)).getOrElse(Long.MinValue))
      .values
      .flatMap { sameExposure =>
        sameExposure.minByOption(_.instant.map(other => math.abs(Duration.between(reference, other).getSeconds)).getOrElse(Long.MaxValue))
      }
      .toList
      .sortBy(_.instant.map(_.toEpochMilli).getOrElse(0L))

  /** Output scale : the requested disc radius fixes how many pixels a degree of sky is worth */
  def pixelsPerDegree(frames: Seq[FrameAnalysis], config: ComposeConfig): Double = {
    val semiDiameters = frames.flatMap(_.sun.map(_.semiDiameterDegrees)).sorted
    val semiDiameter  = if (semiDiameters.isEmpty) 0.266d else semiDiameters(semiDiameters.size / 2)
    config.render.discRadiusPixels / semiDiameter
  }

  /** Human readable summary of a measured session, printed before deciding anything */
  def summary(frames: Seq[FrameAnalysis]): String = {
    val instantFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val usable        = frames.filter(_.isUsable)
    val instants      = frames.flatMap(_.instant).sorted
    val positions     = usable.flatMap(_.position)
    val lines         = List.newBuilder[String]

    lines += s"frames                : ${frames.size} (${usable.size} usable)"
    frames.count(_.phase == FramePhase.Totality) match {
      case 0     => lines += "totality              : none detected"
      case count => lines += s"totality              : $count frames"
    }
    if (instants.nonEmpty) {
      val first    = instants.head
      val last     = instants.last
      val duration = Duration.between(first, last)
      lines += s"first shot (UTC)      : ${instantFormat.format(first.atOffset(java.time.ZoneOffset.UTC))}"
      lines += s"last shot  (UTC)      : ${instantFormat.format(last.atOffset(java.time.ZoneOffset.UTC))}"
      lines += f"duration              : ${duration.toMinutes}%d min"
      if (instants.sizeIs > 1) {
        val gaps = instants.sliding(2).collect { case Seq(a, b) => Duration.between(a, b).toMillis / 1000d }.toList
        lines += f"median cadence        : ${gaps.sorted.apply(gaps.size / 2)}%.1f s"
      }
    }
    usable.flatMap(_.metadata.location).headOption.foreach(location => lines += s"observer              : $location")
    usable.flatMap(_.metadata.cameraName).headOption.foreach(camera => lines += s"camera                : $camera")
    usable.flatMap(_.metadata.focalLengthMillimeters).headOption.foreach(focal => lines += f"focal length          : $focal%.0f mm")
    if (positions.nonEmpty) {
      lines += f"sun altitude          : ${positions.map(_.altitudeDegrees).min}%.2f° to ${positions.map(_.altitudeDegrees).max}%.2f°"
      lines += f"sun azimuth           : ${positions.map(_.azimuthDegrees).min}%.2f° to ${positions.map(_.azimuthDegrees).max}%.2f°"
      lines += f"sky path length       : ${pathLengthDegrees(usable)}%.2f°"
    }
    usable.flatMap(_.obscuration).maxOption.foreach(value => lines += f"maximum obscuration   : ${value * 100}%.1f %%")
    plateScaleOf(usable).foreach(scale => lines += f"plate scale           : $scale%.1f px/° (solar disc ~ ${scale * 0.53d}%.0f px)")

    val failures = frames.filterNot(_.isUsable)
    if (failures.nonEmpty) {
      lines += ""
      lines += s"${failures.size} frames could not be measured :"
      failures.take(10).foreach(frame => lines += s"  ${frame.name} : ${frame.issues.mkString(", ")}")
      if (failures.sizeIs > 10) lines += s"  ... and ${failures.size - 10} more"
    }
    lines.result().mkString("\n")
  }

  /** Why the frames cannot be used, counted cause by cause.
    *
    * "no usable frame" is a dead end for whoever reads it : a frame is unusable because it has no
    * date, or no position, or because its disc could not be measured, and each of those has its own
    * cure. So they are counted separately, with a few examples and the issues met along the way.
    */
  def diagnose(frames: Seq[FrameAnalysis]): String = {
    val lines      = List.newBuilder[String]
    val unusable   = frames.filterNot(_.isUsable)
    val noDate     = frames.count(_.shotAt.isEmpty)
    val noPosition = frames.count(_.metadata.location.isEmpty)
    val noDisc     = frames.count(_.disc.isEmpty)
    val doubtful   = frames.count(_.disc.exists(_.detectionConfidence < 0.2d))

    lines += s"${frames.size} frames read, ${frames.count(_.isUsable)} usable"
    if (noDate > 0) lines += s"  $noDate without a shooting date  -> the sun position cannot be computed"
    if (noPosition > 0) lines += s"  $noPosition without a position       -> give one with --observer lat,lon"
    if (noDisc > 0) lines += s"  $noDisc without a measured disc  -> the sun was not found on the image"
    if (doubtful > 0) lines += s"  $doubtful measured but doubtful    -> dropped by the selection, see --min-confidence"

    val issues = frames.flatMap(_.issues).filter(_.nonEmpty).groupBy(identity).view.mapValues(_.size).toList.sortBy(-_._2)
    if (issues.nonEmpty) {
      lines += "what the analysis reported :"
      issues.take(5).foreach { case (issue, count) => lines += s"  ${count}x $issue" }
    }
    if (unusable.nonEmpty) {
      lines += "for instance :"
      unusable.take(3).foreach { frame =>
        val what = List(
          Option.when(frame.shotAt.isEmpty)("no date"),
          Option.when(frame.metadata.location.isEmpty)("no position"),
          Option.when(frame.disc.isEmpty)("no disc")
        ).flatten.mkString(", ")
        lines += s"  ${frame.name} : $what"
      }
    }
    lines.result().mkString("\n")
  }

  /** Total angular length of the path followed by the sun during the session */
  def pathLengthDegrees(frames: Seq[FrameAnalysis]): Double = {
    val positions = frames.sortBy(_.instant.map(_.toEpochMilli).getOrElse(0L)).flatMap(_.position)
    positions.sliding(2).collect { case Seq(first, second) => first.angularDistanceTo(second) }.sum
  }

  private def plateScaleOf(frames: Seq[FrameAnalysis]): Option[Double] = {
    val samples = frames.flatMap { frame =>
      for {
        disc <- frame.disc
        sun  <- frame.sun
        if disc.phase == FramePhase.Partial
      } yield disc.radiusPixels / sun.semiDiameterDegrees
    }.sorted
    if (samples.isEmpty) None else Some(samples(samples.size / 2))
  }
}
