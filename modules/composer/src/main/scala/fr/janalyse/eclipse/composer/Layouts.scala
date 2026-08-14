package fr.janalyse.eclipse.composer

import fr.janalyse.eclipse.astro.TangentPlaneProjection
import fr.janalyse.eclipse.model.{FrameAnalysis, FramePhase, HorizontalCoordinates}
import fr.janalyse.sotohp.media.imaging.LinearAlgebra

/** Where each selected frame lands on the composite */
final case class Placement(
  frame: FrameAnalysis,
  x: Double,
  y: Double,
  discRadiusPixels: Double,
  tileRadiusPixels: Double
)

final case class LayoutConfig(
  /** output scale, everything derives from it */
  pixelsPerDegree: Double,
  tileRadiusFactor: Double = 1.5d,
  totalityTileRadiusFactor: Double = 3d,
  /** grid and timeline layouts only : spacing between tiles, in tile diameter units */
  spacingFactor: Double = 1.1d,
  columns: Option[Int] = None
)

sealed trait CompositeLayout {
  def name: String
  def place(frames: Seq[FrameAnalysis], config: LayoutConfig): Vector[Placement]

  protected def discRadiusPixels(frame: FrameAnalysis, config: LayoutConfig): Double =
    frame.sun.map(_.semiDiameterDegrees).getOrElse(0.266d) * config.pixelsPerDegree

  protected def tileRadiusPixels(frame: FrameAnalysis, config: LayoutConfig): Double = {
    val factor = frame.phase match {
      case FramePhase.Totality => config.totalityTileRadiusFactor
      case _                   => config.tileRadiusFactor
    }
    discRadiusPixels(frame, config) * factor
  }
}

/** The real thing : each frame is placed where the sun actually was in the sky.
  *
  * The sky is projected the very same way a rectilinear lens would project it, so the result looks
  * like a single wide angle shot of the whole eclipse : the curve of the solar path, its slope, the
  * spacing between two shots, everything is geometrically true.
  *
  * @param evenSpacing redistributes the frames evenly along the fitted path, keeping the real shape
  *                    of the trajectory but ironing out the irregular gaps left by the selection.
  * @param center      direction the virtual camera points at, defaults to the mean sun position.
  */
final case class SkyPathLayout(
  evenSpacing: Boolean = false,
  center: Option[HorizontalCoordinates] = None
) extends CompositeLayout {
  val name = "sky-path"

  def place(frames: Seq[FrameAnalysis], config: LayoutConfig): Vector[Placement] = {
    val positions  = frames.flatMap(_.position)
    if (positions.isEmpty) Vector.empty
    else {
      val projection      = TangentPlaneProjection(center.getOrElse(TangentPlaneProjection.meanDirection(positions)))
      val pixelsPerRadian = config.pixelsPerDegree * 180d / math.Pi
      val projected       = frames.toVector.flatMap { frame =>
        frame.position
          .flatMap(projection.project)
          .map { case (x, y) =>
            Placement(
              frame = frame,
              x = x * pixelsPerRadian,
              y = -y * pixelsPerRadian, // screen coordinates grow downwards
              discRadiusPixels = discRadiusPixels(frame, config),
              tileRadiusPixels = tileRadiusPixels(frame, config)
            )
          }
      }
      if (!evenSpacing || projected.sizeIs < 3) projected else redistribute(projected)
    }
  }

  /** Keeps the shape of the path (a quadratic fit of the projected positions) but spreads the
    * frames at a constant step along it - a constant step in *arc length*, not in abscissa, so
    * that a steep or curved path stays evenly spaced.
    */
  private def redistribute(placements: Vector[Placement]): Vector[Placement] = {
    val points     = placements.map(placement => (placement.x, placement.y))
    val horizontal = points.map(_._1).max - points.map(_._1).min
    val vertical   = points.map(_._2).max - points.map(_._2).min
    val alongX     = horizontal >= vertical
    val samples    = if (alongX) points else points.map(_.swap)

    LinearAlgebra.polynomialFit(samples, 2) match {
      case None               => placements
      case Some(coefficients) =>
        val first      = samples.map(_._1).min
        val last       = samples.map(_._1).max
        val sampleCount = 2000
        val curve      = (0 to sampleCount).map { index =>
          val abscissa = first + (last - first) * index / sampleCount
          (abscissa, LinearAlgebra.evaluatePolynomial(coefficients, abscissa))
        }
        val steps      = curve.zip(curve.tail).map { case (previous, current) =>
          math.hypot(current._1 - previous._1, current._2 - previous._2)
        }
        val lengths    = steps.scanLeft(0d)(_ + _).toVector
        val totalLength = lengths.last
        placements.zipWithIndex.map { case (placement, index) =>
          val target   = totalLength * index / (placements.size - 1)
          val position = lengths.indexWhere(_ >= target) match {
            case -1    => curve.last
            case found => curve(math.min(found, curve.size - 1))
          }
          if (alongX) placement.copy(x = position._1, y = position._2)
          else placement.copy(x = position._2, y = position._1)
        }
    }
  }
}

/** Chronological strip : the frames are aligned on a straight line, evenly spaced.
  *
  * The sky geometry is dropped, only the evolution of the disc is shown. Useful as a companion
  * image, or when the session has too many gaps for the real path to look good.
  *
  * @param slopeDegrees inclination of the strip, 0 for a horizontal one.
  */
final case class TimelineLayout(slopeDegrees: Double = 0d) extends CompositeLayout {
  val name = "timeline"

  def place(frames: Seq[FrameAnalysis], config: LayoutConfig): Vector[Placement] = {
    val ordered = frames.toVector.sortBy(_.instant.map(_.toEpochMilli).getOrElse(0L))
    val step    = ordered.map(frame => tileRadiusPixels(frame, config)).maxOption.getOrElse(1d) * 2d * config.spacingFactor
    val slope   = math.toRadians(slopeDegrees)
    ordered.zipWithIndex.map { case (frame, index) =>
      Placement(
        frame = frame,
        x = index * step * math.cos(slope),
        y = index * step * math.sin(slope),
        discRadiusPixels = discRadiusPixels(frame, config),
        tileRadiusPixels = tileRadiusPixels(frame, config)
      )
    }
  }
}

/** Contact sheet : rows and columns, chronological order, to show the evolution alone */
final case class GridLayout(columns: Option[Int] = None) extends CompositeLayout {
  val name = "grid"

  def place(frames: Seq[FrameAnalysis], config: LayoutConfig): Vector[Placement] = {
    val ordered     = frames.toVector.sortBy(_.instant.map(_.toEpochMilli).getOrElse(0L))
    val columnCount = columns
      .orElse(config.columns)
      .getOrElse(math.max(1, math.ceil(math.sqrt(ordered.size.toDouble)).toInt))
    val step        = ordered.map(frame => tileRadiusPixels(frame, config)).maxOption.getOrElse(1d) * 2d * config.spacingFactor
    ordered.zipWithIndex.map { case (frame, index) =>
      Placement(
        frame = frame,
        x = (index % columnCount) * step,
        y = (index / columnCount) * step,
        discRadiusPixels = discRadiusPixels(frame, config),
        tileRadiusPixels = tileRadiusPixels(frame, config)
      )
    }
  }
}
