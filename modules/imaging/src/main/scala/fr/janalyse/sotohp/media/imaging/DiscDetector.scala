package fr.janalyse.sotohp.media.imaging

import fr.janalyse.sotohp.media.imaging.CircleFitting.{Circle, Point}
import fr.janalyse.sotohp.media.imaging.Rasters.{BitMask, GrayRaster}

import java.awt.image.BufferedImage

/** Detection of a bright round subject within a frame.
  *
  * Written for eclipse sequences but usable for any moon/sun/planet shot : it gives back the
  * sub-pixel center and the radius of the subject, which is what allows frames shot on a static
  * tripod - hence with the subject drifting through the frame, and possibly re-framed by hand
  * between shots - to be aligned afterwards.
  *
  * Two situations are handled, told apart by the contrast of the outer edge, which is exactly what
  * separates them physically :
  *   - `Photosphere` : the solar (or lunar) limb is visible, possibly as a thin crescent. The edge
  *     is a step, the brightness falls to the sky level within a pixel or two. The outer limb is
  *     fitted, ignoring the outline of the occulting body.
  *   - `Corona` : nothing but the corona is left (totality). The brightness fades away smoothly
  *     over hundreds of pixels, there is no limb to fit, so the center comes from the brightness
  *     distribution - which is symmetric around the moon - and the radius from the ephemeris.
  */
object DiscDetector {

  final case class DiscDetectorConfig(
    analysisMaxSize: Int = 1600,
    rayCount: Int = 720,
    thresholdRatio: Double = 0.35,
    minimumCoverage: Double = 0.000005,
    maximumCoverage: Double = 0.5,
    /** contrast across the edge, in dynamic range units, above which a limb is considered visible */
    limbContrastThreshold: Double = 0.3,
    /** distance at which the edge contrast is measured, in pixels on each side of the edge */
    limbContrastSpan: Double = 4d,
    expectedRadiusPixels: Option[Double] = None,
    /** forces the interpretation, typically from the exposure metadata */
    kindHint: Option[DiscKind] = None,
    fitIterations: Int = 8,
    minimumInlierRatio: Double = 0.15
  )

  enum DiscKind {
    case Photosphere, Corona
  }

  final case class DiscDetection(
    circle: Circle,
    kind: DiscKind,
    boundaryPointCount: Int,
    inlierCount: Int,
    residualRms: Double,
    thresholdLevel: Double,
    limbContrast: Double,
    coverage: Double
  )

  /** Detects the subject on a full resolution image.
    *
    * The analysis itself is done on a downscaled copy (memory and cpu friendly on 45Mpix files),
    * results are scaled back to the full resolution coordinate system.
    */
  def detect(image: BufferedImage, config: DiscDetectorConfig = DiscDetectorConfig()): Either[String, DiscDetection] = {
    val analysed = BasicImaging.fitWithin(image, config.analysisMaxSize)
    val scale    = image.getWidth.toDouble / analysed.getWidth
    val expected = config.expectedRadiusPixels.map(_ / scale)
    detectOnRaster(GrayRaster.fromImage(analysed), config.copy(expectedRadiusPixels = expected))
      .map(detection => detection.copy(circle = detection.circle.scaled(scale)))
  }

  /** Detects the subject on an already prepared luminance raster */
  def detectOnRaster(raster: GrayRaster, config: DiscDetectorConfig): Either[String, DiscDetection] = {
    val levels     = raster.percentiles(List(0.5d, 0.9999d))
    val background = levels.head
    val peak       = levels.last
    val range      = peak - background
    if (range < 1e-4d) Left("uniform frame, no subject found")
    else {
      val threshold = background + config.thresholdRatio * range
      val mask      = raster.brighterThan(threshold)
      if (mask.coverage < config.minimumCoverage) Left(f"subject too small or too faint (coverage ${mask.coverage}%.8f)")
      else if (mask.coverage > config.maximumCoverage) Left(f"subject too large, frame is probably overexposed (coverage ${mask.coverage}%.4f)")
      else {
        mask.centroid match {
          case None           => Left("no lit pixel found")
          case Some((cx, cy)) =>
            val samples  = scanRays(raster, mask, cx, cy, range, config)
            val contrast = medianContrast(samples)
            val kind     = config.kindHint.getOrElse {
              if (samples.nonEmpty && contrast >= config.limbContrastThreshold) DiscKind.Photosphere
              else DiscKind.Corona
            }
            kind match {
              case DiscKind.Photosphere => fitPhotosphere(raster, mask, samples, threshold, contrast, range, config)
              case DiscKind.Corona      => fitCorona(raster, mask, threshold, contrast, config)
            }
        }
      }
    }
  }

  /** One sample per ray : where the lit area ends, and how sharp that edge is */
  private final case class EdgeSample(point: Point, contrast: Double)

  /** Outer limb fit : the boundary points found along the rays are fitted with a robust circle fit
    * which keeps the outer arc only, then everything is done a second time from the center just
    * found - on a crescent the first rays all leave from a badly off-center origin.
    */
  private def fitPhotosphere(
    raster: GrayRaster,
    mask: BitMask,
    samples: Seq[EdgeSample],
    threshold: Double,
    contrast: Double,
    range: Double,
    config: DiscDetectorConfig
  ): Either[String, DiscDetection] = {
    def fitFrom(points: Seq[Point]) =
      if (points.sizeIs < 8) None
      else
        CircleFitting
          .robustOuterFit(
            points = points,
            knownRadius = config.expectedRadiusPixels,
            iterations = config.fitIterations,
            minimumInlierRatio = config.minimumInlierRatio
          )
          .map { case (circle, inliers) => (circle, inliers, points.size) }

    val first  = fitFrom(samples.map(_.point))
    val second = first.flatMap { case (circle, _, _) =>
      fitFrom(scanRays(raster, mask, circle.centerX, circle.centerY, range, config).map(_.point))
    }

    second.orElse(first) match {
      case None                             => Left("outer limb fit failed")
      case Some((circle, inliers, sampled)) =>
        Right(
          DiscDetection(
            circle = circle,
            kind = DiscKind.Photosphere,
            boundaryPointCount = sampled,
            inlierCount = inliers.size,
            residualRms = CircleFitting.residualRms(circle, inliers),
            thresholdLevel = threshold,
            limbContrast = contrast,
            coverage = mask.coverage
          )
        )
    }
  }

  /** Totality : the corona is roughly symmetric around the moon, its brightness weighted centroid
    * gives a usable center. The radius cannot be measured, it is taken from the expected one.
    */
  private def fitCorona(
    raster: GrayRaster,
    mask: BitMask,
    threshold: Double,
    contrast: Double,
    config: DiscDetectorConfig
  ): Either[String, DiscDetection] = {
    var totalX = 0d
    var totalY = 0d
    var total  = 0d
    var y      = 0
    while (y < raster.height) {
      var x = 0
      while (x < raster.width) {
        val level = raster(x, y) - threshold
        if (level > 0f) { totalX += x * level; totalY += y * level; total += level }
        x += 1
      }
      y += 1
    }
    if (total <= 0d) Left("no corona signal found")
    else {
      val radius = config.expectedRadiusPixels.getOrElse(math.sqrt(mask.count / math.Pi))
      Right(
        DiscDetection(
          circle = Circle(totalX / total, totalY / total, radius),
          kind = DiscKind.Corona,
          boundaryPointCount = 0,
          inlierCount = mask.count,
          residualRms = 0d,
          thresholdLevel = threshold,
          limbContrast = contrast,
          coverage = mask.coverage
        )
      )
    }
  }

  /** Casts rays from the given origin, and reports for each of them the farthest lit sample found
    * along with the brightness step measured across that edge.
    */
  private def scanRays(
    raster: GrayRaster,
    mask: BitMask,
    originX: Double,
    originY: Double,
    range: Double,
    config: DiscDetectorConfig
  ): Seq[EdgeSample] = {
    val maximumRadius = List(originX, originY, mask.width - originX, mask.height - originY).max
    (0 until config.rayCount).flatMap { rayIndex =>
      val angle       = 2d * math.Pi * rayIndex / config.rayCount
      val directionX  = math.cos(angle)
      val directionY  = math.sin(angle)
      var radius      = 0d
      var lastLit     = -1d
      var consecutive = 0
      while (radius < maximumRadius) {
        val x = originX + directionX * radius
        val y = originY + directionY * radius
        if (mask(math.round(x).toInt, math.round(y).toInt)) {
          consecutive += 1
          if (consecutive >= 3) lastLit = radius // ignores isolated hot pixels
        } else consecutive = 0
        radius += 0.5d
      }
      if (lastLit < 0d) None
      else {
        val span    = config.limbContrastSpan
        val inside  = sample(raster, originX + directionX * (lastLit - span), originY + directionY * (lastLit - span))
        val outside = sample(raster, originX + directionX * (lastLit + span), originY + directionY * (lastLit + span))
        Some(
          EdgeSample(
            point = Point(originX + directionX * lastLit, originY + directionY * lastLit),
            contrast = if (range <= 0d) 0d else (inside - outside) / range
          )
        )
      }
    }
  }

  private def medianContrast(samples: Seq[EdgeSample]): Double =
    if (samples.isEmpty) 0d
    else {
      val sorted = samples.map(_.contrast).sorted
      sorted(sorted.size / 2)
    }

  private def sample(raster: GrayRaster, x: Double, y: Double): Double =
    raster.at(math.round(x).toInt, math.round(y).toInt, 0f).toDouble
}
