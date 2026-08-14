package fr.janalyse.eclipse.composer

import fr.janalyse.eclipse.composer.FrameSelector.{Selection, SelectionConfig}
import fr.janalyse.eclipse.model.{FrameAnalysis, FramePhase}
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
    if (selection.kept.isEmpty) Left("no usable frame left after selection")
    else {
      val layoutConfig = LayoutConfig(
        pixelsPerDegree = pixelsPerDegree(frames, config),
        tileRadiusFactor = config.render.tileRadiusFactor,
        totalityTileRadiusFactor = config.render.totalityTileRadiusFactor
      )
      val placements   = config.layout.place(selection.kept, layoutConfig)
      if (placements.isEmpty) Left("the layout could not place any frame")
      else
        CompositeRenderer
          .render(placements, config.render, config.cacheDirectory, config.rawDecode, onProgress)
          .map(result => ComposeOutcome(result, selection, placements))
    }
  }

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
