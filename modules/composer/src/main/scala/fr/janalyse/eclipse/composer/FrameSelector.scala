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
    /** keep as many frames before the maximum as after it.
      *
      * A session is rarely symmetric - this one starts an hour and a half before totality and stops
      * at sunset - so the walk naturally brings back more frames on one side. Balancing trims the
      * longer side to the length of the shorter one, which makes the composite read as a sequence
      * built around its middle rather than one drifting off to a side.
      */
    balanced: Boolean = false,
    /** hard limit on the number of frames kept on each side of the maximum */
    framesPerSide: Option[Int] = None,
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
      // The maximum of the eclipse is the anchor, not an afterthought : starting the walk there is
      // what puts it in the selection while the spacing rule keeps holding. Adding it afterwards
      // would drop it right next to a neighbour and break the one promise this whole thing makes.
      val anchorIndex = config.anchor match {
        case Anchor.First          => 0
        case Anchor.Last           => usable.size - 1
        case Anchor.MaximumEclipse =>
          val maximum = representativeMaximum(usable)
          math.max(0, usable.indexWhere(_.path == maximum.path))
      }

      // the anchor first, then both directions, so that the neighbours spread symmetrically around it
      val forward  = walk(usable, anchorIndex, 1, config)
      val backward = walk(usable, anchorIndex, -1, config)

      // both walks come away from the anchor, so trimming their tails drops the frames furthest
      // from the maximum - the ones the sequence can most afford to lose
      val perSide  = config.framesPerSide
        .orElse(Option.when(config.balanced)(math.min(forward.size, backward.size)))
        .getOrElse(Int.MaxValue)
      val kept     = backward.take(perSide).reverse ++ List(usable(anchorIndex)) ++ forward.take(perSide)

      Selection(
        kept = kept.toList,
        candidateCount = frames.size,
        rejectedForOverlap = usable.size - kept.size,
        rejectedForQuality = rejectedForQuality
      )
    }
  }

  /** The one frame that stands for the maximum of the eclipse.
    *
    * Every totality frame reads as fully obscured, so picking the most obscured one just returns
    * whichever came first - and the frames flanking totality, where the crescent is a hair thin,
    * read the same way. Totality is a continuous stretch of a minute or two, often shot as a burst,
    * so the longest run of consecutive totality frames is the real thing, and its middle is the
    * safest place to stand : not a diamond ring at either end.
    */
  def representativeMaximum(frames: Seq[FrameAnalysis]): FrameAnalysis = {
    val ordered = frames.sortBy(_.instant.map(_.toEpochMilli).getOrElse(0L))
    val runs    = ordered.zipWithIndex
      .filter { case (frame, _) => frame.phase == FramePhase.Totality }
      .foldLeft(List.empty[Vector[FrameAnalysis]]) { case (accumulated, (frame, index)) =>
        accumulated match {
          case head :: tail if ordered.indexOf(head.last) == index - 1 => (head :+ frame) :: tail
          case _                                                       => Vector(frame) :: accumulated
        }
      }
    runs.maxByOption(_.size) match {
      case Some(run) if run.nonEmpty => run(run.size / 2)
      case _                         => ordered.maxBy(_.obscuration.getOrElse(0d))
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
