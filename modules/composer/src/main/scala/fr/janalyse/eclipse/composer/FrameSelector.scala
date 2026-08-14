package fr.janalyse.eclipse.composer

import fr.janalyse.eclipse.model.{FrameAnalysis, FramePhase}

/** Which frames end up in the composite.
  *
  * A shot every 30 seconds means the sun moves about a eighth of its own diameter between two
  * frames : drawing them all would give a continuous smear. The selection keeps the frames far
  * enough from each other for the discs never to touch, which is what makes both the motion and the
  * progress of the eclipse readable at the same time.
  */
object FrameSelector {

  /** Where the selection starts from, it is the only frame guaranteed to be kept */
  enum Anchor {
    case First
    case Last
    case MaximumEclipse
  }

  final case class SelectionConfig(
    /** minimum gap between two neighbour tiles, 1.0 means tiles just touching, 1.1 leaves 10% air */
    separationFactor: Double = 1.05d,
    /** how much room a tile takes around the solar disc, in disc radius units */
    tileRadiusFactor: Double = 1.5d,
    /** totality frames deserve more room, the corona spreads far beyond the disc */
    totalityTileRadiusFactor: Double = 3d,
    anchor: Anchor = Anchor.MaximumEclipse,
    /** always keep the most eclipsed frame, whatever the separation rule says */
    keepMaximumEclipse: Boolean = true,
    /** drop frames whose measurement is doubtful */
    minimumConfidence: Double = 0.2d
  )

  final case class Selection(
    kept: List[FrameAnalysis],
    candidateCount: Int,
    rejectedForOverlap: Int,
    rejectedForQuality: Int
  ) {
    def keptCount: Int = kept.size
  }

  /** Angular radius, in degrees, of the tile drawn for a frame */
  def tileRadiusDegrees(frame: FrameAnalysis, config: SelectionConfig): Double = {
    val factor = frame.phase match {
      case FramePhase.Totality => config.totalityTileRadiusFactor
      case _                   => config.tileRadiusFactor
    }
    frame.sun.map(_.semiDiameterDegrees * factor).getOrElse(0.266d * factor)
  }

  def select(frames: Seq[FrameAnalysis], config: SelectionConfig = SelectionConfig()): Selection = {
    val usable = frames
      .filter(_.isUsable)
      .filter(_.disc.exists(_.detectionConfidence >= config.minimumConfidence))
      .sortBy(_.instant.map(_.toEpochMilli).getOrElse(0L))
      .toVector

    val rejectedForQuality = frames.size - usable.size
    if (usable.isEmpty) Selection(Nil, frames.size, 0, rejectedForQuality)
    else {
      val anchorIndex = config.anchor match {
        case Anchor.First          => 0
        case Anchor.Last           => usable.size - 1
        case Anchor.MaximumEclipse =>
          usable.zipWithIndex.maxBy { case (frame, _) => frame.obscuration.getOrElse(0d) }._2
      }

      // the anchor first, then both directions, so that the most eclipsed frame is always centered
      // on the real maximum and the neighbours spread symmetrically around it
      val forward  = walk(usable, anchorIndex, 1, config)
      val backward = walk(usable, anchorIndex, -1, config)
      val kept     = (backward.reverse ++ List(usable(anchorIndex)) ++ forward)

      val withMaximum =
        if (!config.keepMaximumEclipse) kept
        else {
          val mostEclipsed = usable.maxBy(_.obscuration.getOrElse(0d))
          if (kept.exists(_.path == mostEclipsed.path)) kept
          else (kept :+ mostEclipsed).sortBy(_.instant.map(_.toEpochMilli).getOrElse(0L))
        }

      Selection(
        kept = withMaximum.toList,
        candidateCount = frames.size,
        rejectedForOverlap = usable.size - withMaximum.size,
        rejectedForQuality = rejectedForQuality
      )
    }
  }

  /** Greedy walk : from the anchor, keep the first frame far enough from the last kept one */
  private def walk(frames: Vector[FrameAnalysis], from: Int, step: Int, config: SelectionConfig): List[FrameAnalysis] = {
    val kept    = List.newBuilder[FrameAnalysis]
    var last    = frames(from)
    var index   = from + step
    while (index >= 0 && index < frames.size) {
      val candidate = frames(index)
      val distance  = for {
        lastPosition      <- last.position
        candidatePosition <- candidate.position
      } yield lastPosition.angularDistanceTo(candidatePosition)
      val required  = (tileRadiusDegrees(last, config) + tileRadiusDegrees(candidate, config)) * config.separationFactor
      if (distance.exists(_ >= required)) {
        kept += candidate
        last = candidate
      }
      index += step
    }
    kept.result()
  }

  /** Selection driven by the eclipse progress rather than by the sun motion : frames are picked so
    * that the obscuration grows by a regular step. Handy for a contact sheet like layout, where the
    * positions do not matter but the evolution does.
    */
  def selectByObscurationStep(frames: Seq[FrameAnalysis], step: Double = 0.05d): List[FrameAnalysis] = {
    val usable = frames.filter(_.isUsable).sortBy(_.instant.map(_.toEpochMilli).getOrElse(0L))
    val kept   = List.newBuilder[FrameAnalysis]
    var last   = Double.NaN
    usable.foreach { frame =>
      val obscuration = frame.obscuration.getOrElse(0d)
      if (last.isNaN || math.abs(obscuration - last) >= step) {
        kept += frame
        last = obscuration
      }
    }
    kept.result()
  }
}
