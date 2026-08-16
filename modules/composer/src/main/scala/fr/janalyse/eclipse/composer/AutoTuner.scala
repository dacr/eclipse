package fr.janalyse.eclipse.composer

import fr.janalyse.eclipse.composer.FrameSelector.SelectionConfig
import fr.janalyse.eclipse.model.{FrameAnalysis, FramePhase, PlateScale}

import java.nio.file.Path

/** Automatic choice of the composition parameters.
  *
  * Everything needed is already measured on the frames themselves, so there is little reason to ask
  * for it : how big the sun is on the sensor, how much room there is around it before hitting the
  * frame border, how far the corona reaches, how long the path across the sky is. The tuner turns
  * those measurements into a complete configuration, and says out loud what it decided and why.
  *
  * Anything the user did specify explicitly is meant to win over what is decided here.
  */
object AutoTuner {

  final case class TuningIntent(
    /** the composite will not exceed that many pixels */
    maximumCanvasPixels: Long = 200000000L,
    /** nor that many pixels on a side, some viewers and printers choke beyond */
    maximumCanvasSide: Int = 24000,
    /** below that, a solar disc becomes a dot */
    minimumDiscRadiusPixels: Double = 20d,
    /** how much air is left between two neighbour tiles */
    separationFactor: Double = 1.05d,
    /** the constraints the user already asked for : the tuning has to work on the very selection
      * that will be drawn, otherwise the output scale is computed over a wider sky than the one
      * kept, and the composite comes out smaller than it could have been
      */
    balanced: Boolean = false,
    framesPerSide: Option[Int] = None,
    /** sky kept around the sequence beyond the frames themselves, to show a background : the scale
      * has to be chosen for the canvas which will actually be drawn, not for the sequence alone
      */
    extraFieldDegrees: Double = 0d,
    /** suns the background already shows, which no tile may be drawn on top of */
    occupied: List[FrameSelector.OccupiedSky] = Nil
  )

  final case class Tuning(
    selection: SelectionConfig,
    layout: CompositeLayout,
    render: RenderConfig,
    plateScale: Option[PlateScale],
    explanations: List[String]
  ) {
    def toComposeConfig(cacheDirectory: Path): ComposeConfig =
      ComposeConfig(cacheDirectory = cacheDirectory, selection = selection, layout = layout, render = render)
  }

  def tune(frames: Seq[FrameAnalysis], intent: TuningIntent = TuningIntent()): Tuning = {
    val explanations = List.newBuilder[String]
    val usable       = frames.filter(_.isUsable)

    if (usable.isEmpty) {
      explanations += "no usable frame, falling back on the default settings"
      Tuning(SelectionConfig(), SkyPathLayout(), RenderConfig(), None, explanations.result())
    } else {
      val plateScale = measuredPlateScale(usable)
      plateScale.foreach(scale => explanations += f"plate scale measured at ${scale.pixelsPerDegree}%.0f px/°, solar disc ${scale.pixelsPerDegree * 0.53d}%.0f px on the sensor")

      // --- how much room a tile may take, frame border wise -----------------------------------
      val partialRoom  = quantile(usable.filter(_.phase != FramePhase.Totality).flatMap(_.disc).flatMap(_.roomFactor), 0.1d)
      val totalityRoom = quantile(usable.filter(_.phase == FramePhase.Totality).flatMap(_.disc).flatMap(_.roomFactor), 0.1d)

      // beyond the limb a filtered frame holds nothing but black sky : the tile only needs enough
      // margin for its edge to fade out. Keeping more would waste canvas and, worse, force a wider
      // spacing between frames - hence fewer of them in the final picture.
      val wantedPartialTile = 1.35d
      val tileRadiusFactor  = partialRoom match {
        case Some(room) if room * 0.95d < wantedPartialTile =>
          val chosen = clamp(room * 0.95d, 1.1d, wantedPartialTile)
          explanations += f"tile radius reduced to ${chosen}%.2f disc radius, the sun comes as close as ${room}%.2f radius from a frame border"
          chosen
        case _                                              =>
          explanations += f"tile radius set to $wantedPartialTile%.2f disc radius, just enough for the edges to fade out"
          wantedPartialTile
      }

      // --- how far the corona actually reaches -------------------------------------------------
      val totalityFrames = usable.filter(_.phase == FramePhase.Totality)
      val coronaExtent   = quantile(
        totalityFrames.flatMap(frame => frame.disc.flatMap(disc => disc.signalRadiusPixels.map(_ / disc.radiusPixels))),
        0.9d
      )
      val totalityTileRadiusFactor = (coronaExtent, totalityRoom) match {
        case (Some(extent), room) =>
          val limit  = room.map(_ * 0.95d).getOrElse(4d)
          val chosen = clamp(math.min(extent * 1.1d, limit), 1.2d, 5d)
          explanations += f"totality tile radius set to ${chosen}%.2f disc radius (recorded corona reaches ${extent}%.2f radius)"
          chosen
        case (None, _) if totalityFrames.nonEmpty =>
          explanations += "totality tile radius left at its default, the corona extent could not be measured"
          3d
        case _                                    => 3d
      }

      // --- selection, then the output scale it implies ------------------------------------------
      val selection = SelectionConfig(
        separationFactor = intent.separationFactor,
        tileRadiusFactor = tileRadiusFactor,
        totalityTileRadiusFactor = totalityTileRadiusFactor,
        balanced = intent.balanced,
        framesPerSide = intent.framesPerSide,
        occupied = intent.occupied
      )
      val selected  = FrameSelector.select(usable, selection)
      explanations += s"${selected.keptCount} frames kept out of ${usable.size}, spaced so that no two discs touch"
      if (selected.rejectedForScenery > 0)
        explanations += s"${selected.rejectedForScenery} frames left undrawn, the background already shows the sun where they would have landed"

      val layout =
        if (EclipseComposer.pathLengthDegrees(usable) > 2d) SkyPathLayout()
        else {
          explanations += "the sun barely moved during the session, laying the frames out as a grid rather than along their path"
          GridLayout()
        }

      // the layout is run once at one pixel per degree : the result is the extent of the composite
      // expressed in degrees, from which the largest usable scale is derived
      val probe      = layout.place(selected.kept, LayoutConfig(1d, tileRadiusFactor, totalityTileRadiusFactor))
      val extra      = math.max(0d, intent.extraFieldDegrees) * 2d
      val spanWidth  = math.max(1e-6d, probe.map(p => p.x + p.tileRadiusPixels).max - probe.map(p => p.x - p.tileRadiusPixels).min) + extra
      val spanHeight = math.max(1e-6d, probe.map(p => p.y + p.tileRadiusPixels).max - probe.map(p => p.y - p.tileRadiusPixels).min) + extra
      if (extra > 0d) explanations += f"${extra}%.1f° of sky added around the sequence to make room for the background"

      val nativeScale  = plateScale.map(_.pixelsPerDegree).getOrElse(800d)
      val sideLimit    = intent.maximumCanvasSide / math.max(spanWidth, spanHeight)
      val surfaceLimit = math.sqrt(intent.maximumCanvasPixels.toDouble / (spanWidth * spanHeight))
      val scale        = List(nativeScale, sideLimit, surfaceLimit).min

      val semiDiameter = medianSemiDiameter(usable)
      val discRadius   = math.max(intent.minimumDiscRadiusPixels, scale * semiDiameter)

      if (scale >= nativeScale) explanations += f"drawn at the full sensor resolution, ${discRadius * 2}%.0f px per solar disc"
      else
        explanations += f"drawn at ${scale / nativeScale * 100}%.0f%% of the sensor resolution, ${discRadius * 2}%.0f px per solar disc, otherwise the composite would reach ${spanWidth * nativeScale}%.0f x ${spanHeight * nativeScale}%.0f px"

      explanations += f"composite will be about ${spanWidth * scale}%.0f x ${spanHeight * scale}%.0f px for a ${spanWidth}%.1f° x ${spanHeight}%.1f° field"

      val render = RenderConfig(
        discRadiusPixels = discRadius,
        tileRadiusFactor = tileRadiusFactor,
        totalityTileRadiusFactor = totalityTileRadiusFactor,
        marginPixels = math.max(40, (math.min(spanWidth, spanHeight) * scale * 0.03d).toInt),
        annotateTimes = false
      )

      Tuning(selection, layout, render, plateScale, explanations.result())
    }
  }

  private def measuredPlateScale(frames: Seq[FrameAnalysis]): Option[PlateScale] = {
    val samples = frames.flatMap { frame =>
      for {
        disc <- frame.disc
        sun  <- frame.sun
        if disc.phase == FramePhase.Partial
      } yield disc.radiusPixels / sun.semiDiameterDegrees
    }
    quantile(samples, 0.5d).map(PlateScale.apply)
  }

  private def medianSemiDiameter(frames: Seq[FrameAnalysis]): Double =
    quantile(frames.flatMap(_.sun.map(_.semiDiameterDegrees)), 0.5d).getOrElse(0.266d)

  private def quantile(values: Seq[Double], ratio: Double): Option[Double] = {
    val sorted = values.sorted
    if (sorted.isEmpty) None
    else Some(sorted(math.max(0, math.min(sorted.size - 1, (sorted.size * ratio).toInt))))
  }

  private def clamp(value: Double, low: Double, high: Double): Double = math.max(low, math.min(high, value))
}
