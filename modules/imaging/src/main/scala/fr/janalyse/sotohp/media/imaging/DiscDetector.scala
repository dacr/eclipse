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
    /** how much of the frame the subject may span before being taken for sky or landscape */
    maximumSpanRatio: Double = 0.8,
    /** how much of the brightness fall must happen right at the edge for a limb to be seen there */
    limbContrastThreshold: Double = 0.6,
    /** distance at which the edge contrast is measured, in pixels on each side of the edge */
    limbContrastSpan: Double = 4d,
    expectedRadiusPixels: Option[Double] = None,
    /** how far the freely measured radius may stray from the expected one before it is imposed */
    radiusTolerance: Double = 0.25d,
    /** the most a disc may be squashed before the shape is taken for a mistake rather than for
      * refraction : six percent is what two degrees of altitude give, twice that is already below
      * the horizon
      */
    maximumFlattening: Double = 0.25d,
    /** how much of the outline the boundary points must cover before an ellipse - four free
      * parameters instead of three - may be fitted to them
      */
    minimumAngularCoverage: Double = 0.55d,
    /** where the subject is expected to be, typically propagated from the previous frame of a
      * sequence : it is used as the origin of the rays and to focus the corona centroid, and it is
      * silently ignored when it does not land on the subject - a re-framing, a cloud, a mistake
      */
    expectedCenter: Option[(Double, Double)] = None,
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
    /** how far from a circle the boundary points are, whatever its radius */
    radialSpread: Double,
    thresholdLevel: Double,
    limbContrast: Double,
    coverage: Double,
    /** how much the disc is squashed vertically, 0 when it was measured as a circle */
    flattening: Double = 0d
  )

  /** Detects the subject on a full resolution image.
    *
    * The analysis itself is done on a downscaled copy (memory and cpu friendly on 45Mpix files),
    * results are scaled back to the full resolution coordinate system.
    */
  def detect(image: BufferedImage, config: DiscDetectorConfig = DiscDetectorConfig()): Either[String, DiscDetection] = {
    val analysed = BasicImaging.fitWithin(image, config.analysisMaxSize)
    val scale    = image.getWidth.toDouble / analysed.getWidth
    detectOnRaster(
      GrayRaster.fromImage(analysed),
      config.copy(
        expectedRadiusPixels = config.expectedRadiusPixels.map(_ / scale),
        expectedCenter = config.expectedCenter.map { case (x, y) => (x / scale, y / scale) }
      )
    ).map(detection => detection.copy(circle = detection.circle.scaled(scale)))
  }

  /** Detects the subject on an already prepared luminance raster */
  def detectOnRaster(raster: GrayRaster, config: DiscDetectorConfig): Either[String, DiscDetection] = {
    val levels     = raster.percentiles(List(0.5d, 0.9999d))
    val background = levels.head
    val peak       = levels.last
    val range      = peak - background
    if (range < 1e-4d) Left("uniform frame, no subject found")
    else {
      // The threshold is not fixed : a twilight sky, a bright horizon or a landscape in the frame
      // would put far too much of the image above any preset level, while an underexposed corona
      // would put too little.
      //
      // The levels are tried from the lowest up, and the first one that isolates something compact
      // wins - the lowest threshold is the one that catches the whole subject. Starting from the
      // top instead would stop at the first thing that stands out, and on a frame whose disc is
      // dim but whose limb is burnt, that first thing is the bright rim alone : a subject five
      // times too small, with no limb around it, taken for a corona.
      val ratios   = (config.thresholdRatio +: List(0.05d, 0.1d, 0.2d, 0.5d, 0.65d, 0.8d, 0.9d, 0.96d)).distinct.sorted
      val attempts = ratios.view.map { ratio =>
        val level = background + ratio * range
        (level, raster.brighterThan(level))
      }
      val found    = attempts.find { case (_, mask) =>
        val (horizontal, vertical) = mask.span()
        mask.coverage >= config.minimumCoverage &&
        mask.coverage <= config.maximumCoverage &&
        horizontal <= config.maximumSpanRatio &&
        vertical <= config.maximumSpanRatio &&
        plausibleSize(mask, config)
      }

      found match {
        case None =>
          val (_, first) = attempts.head
          val (horizontal, vertical) = first.span()
          if (first.coverage < config.minimumCoverage)
            Left(f"subject too small or too faint (coverage ${first.coverage}%.8f, ${ratios.size} thresholds tried)")
          else
            Left(
              f"nothing compact stands out of the frame (coverage ${first.coverage}%.4f, " +
                f"spanning ${horizontal * 100}%.0f%% x ${vertical * 100}%.0f%% of it, ${ratios.size} thresholds tried)"
            )

        case Some((locatingLevel, locatingMask)) =>
        // Two thresholds, because they answer two different questions. The low one above found
        // *where* the subject is, halo and faint parts included - that is what it is for. Measuring
        // an edge on it would be wrong : it follows the outer haze rather than the limb, and the
        // points scatter. The edge of a body sits at half height between it and its background, so
        // now that the subject is located, its own level is read and the limb measured there.
        val (threshold, mask) =
          raster.medianAbove(locatingLevel) match {
            case Some(subjectLevel) if subjectLevel > locatingLevel =>
              val halfHeight = (background + subjectLevel) / 2d
              val refined    = raster.brighterThan(halfHeight)
              if (refined.count > 0 && refined.centroid.isDefined) (halfHeight, refined)
              else (locatingLevel, locatingMask)
            case _                                                  => (locatingLevel, locatingMask)
          }

        mask.centroid match {
          case None           => Left("no lit pixel found")
          case Some(centroid) =>
            val (cx, cy) = trustedCenter(config, centroid, raster)
            val samples  = scanRays(raster, mask, cx, cy, range, config)

            // The limb is looked for first, and the answer is judged afterwards. Deciding
            // beforehand meant deciding on the edge of the thresholded mask, which sits well inside
            // a saturated disc and shows no step there : a sharp limb then looked like a corona.
            // Fitting first costs one pass and lets the contrast be measured where it means
            // something - across the circle that was found.
            def corona() = fitCorona(raster, mask, threshold, (cx, cy), config)

            config.kindHint match {
              case Some(DiscKind.Corona) => corona()
              case hint                   =>
                fitPhotosphere(raster, mask, samples, threshold, range, config) match {
                  case Right(detection)
                      if hint.contains(DiscKind.Photosphere) ||
                        detection.limbContrast >= config.limbContrastThreshold ||
                        !hollow(raster, detection.circle) =>
                    Right(detection)
                  case _ => corona()
                }
            }
        }
      }
    }
  }

  /** Whether the subject is a ring of light around a dark middle, rather than a filled disc.
    *
    * This is what totality looks like, and nothing else does : the moon sits in the middle and it
    * is black. The distinction matters because a soft limb is not enough to tell - a sun a couple
    * of degrees above the horizon crosses so much atmosphere that its edge genuinely blurs, and a
    * whole hour of such frames was being taken for totality on that ground alone. Its middle,
    * however, is as bright as ever.
    */
  private def hollow(raster: GrayRaster, circle: Circle, ringCount: Int = 20): Boolean = {
    // the shape of the radial profile decides, not a ratio between two chosen radii : the fitted
    // circle may well be smaller than the dark middle itself, and comparing an inner disc with an
    // outer ring would then compare darkness with darkness. A filled disc peaks at its center, a
    // ring peaks away from it, whatever the scale.
    val reach  = circle.radius * 1.5d
    val step   = reach / ringCount
    val totals = Array.ofDim[Double](ringCount)
    val counts = Array.ofDim[Int](ringCount)
    var y      = math.max(0, (circle.centerY - reach).toInt)
    val lastY  = math.min(raster.height - 1, (circle.centerY + reach).toInt)
    while (y <= lastY) {
      var x     = math.max(0, (circle.centerX - reach).toInt)
      val lastX = math.min(raster.width - 1, (circle.centerX + reach).toInt)
      while (x <= lastX) {
        val ring = (circle.distanceToCenter(x, y) / step).toInt
        if (ring < ringCount) { totals(ring) += raster(x, y); counts(ring) += 1 }
        x += 1
      }
      y += 1
    }

    val profile = Array.tabulate(ringCount)(index => if (counts(index) == 0) 0d else totals(index) / counts(index))
    val peak    = profile.indices.maxBy(profile.apply)
    // the brightest ring has to sit clearly away from the center, and the center to be much darker
    peak * step > circle.radius * 0.2d && profile(0) < profile(peak) * 0.5d
  }

  /** Whether what stands out could be the subject, size wise.
    *
    * Only ever checked when the expected radius is known, and generously : a crescent covers much
    * less area than a full disc, so this rules out the gross mistakes - a bright rim mistaken for
    * the whole sun - not the fine ones.
    */
  private def plausibleSize(mask: BitMask, config: DiscDetectorConfig): Boolean =
    config.expectedRadiusPixels.forall { expected =>
      val equivalentRadius = math.sqrt(mask.count / math.Pi)
      equivalentRadius >= expected * 0.25d && equivalentRadius <= expected * 1.6d
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
    range: Double,
    config: DiscDetectorConfig
  ): Either[String, DiscDetection] = {
    def fitFrom(points: Seq[Point], knownRadius: Option[Double]) =
      if (points.sizeIs < 8) None
      else
        CircleFitting
          .robustOuterFit(
            points = points,
            knownRadius = knownRadius,
            iterations = config.fitIterations,
            minimumInlierRatio = config.minimumInlierRatio
          )
          .map { case (circle, inliers) => (circle, inliers, points) }

    /** The expected radius is a safety net, never a straitjacket.
      *
      * Imposing it looks tempting since the session knows its plate scale, but the apparent radius
      * of the disc breathes a little from one exposure to the next. Imposing a radius larger than
      * the one the frame actually shows puts every single limb point *inside* the circle, they all
      * get rejected as if they belonged to the moon, and the fit ends up on whatever noise is left.
      * So the free fit has the first word, and the expected radius only steps in when that fit
      * comes back with something implausible - which is what happens on the thinnest crescents.
      */
    def fitBoth(points: Seq[Point]) = {
      val free = fitFrom(points, None)
      (free, config.expectedRadiusPixels) match {
        case (Some((circle, _, _)), Some(expected)) if math.abs(circle.radius - expected) / expected > config.radiusTolerance =>
          fitFrom(points, Some(expected)).orElse(free)
        case (None, Some(expected))                                                                                          =>
          fitFrom(points, Some(expected))
        case _                                                                                                               => free
      }
    }

    val first  = fitBoth(samples.map(_.point))
    val second = first.flatMap { case (circle, _, _) =>
      fitBoth(scanRays(raster, mask, circle.centerX, circle.centerY, range, config).map(_.point))
    }

    second.orElse(first) match {
      case None                              => Left("outer limb fit failed")
      case Some((circle, inliers, sampled)) =>
        val (shape, shapeInliers, flattening) = squashed(circle, inliers, sampled, config)
        Right(
          DiscDetection(
            circle = shape,
            kind = DiscKind.Photosphere,
            boundaryPointCount = sampled.size,
            inlierCount = shapeInliers.size,
            residualRms = residualOf(shape, shapeInliers, flattening),
            radialSpread = spreadOf(shape, shapeInliers, flattening),
            thresholdLevel = threshold,
            limbContrast = limbContrastAcross(raster, shape, shapeInliers, range, config),
            coverage = mask.coverage,
            flattening = flattening
          )
        )
    }
  }

  /** Refits the limb as an ellipse when the disc turns out not to be round.
    *
    * A sun a couple of degrees above the horizon is squashed by refraction, and a circle simply
    * cannot pass through such an outline : it splits the difference, leaves a residual of several
    * percent of the radius, and the frame gets thrown out for a bad measurement although its limb
    * was perfectly visible. That is what emptied the end of a real session, the last twenty minutes
    * of it, and left a hole in the composite.
    *
    * The ellipse is only kept when it earns its extra parameter : the outline has to be covered
    * widely enough to pin four of them down, the shape has to be squashed the way the atmosphere
    * squashes - vertically, and by a plausible amount - and the points have to scatter clearly less
    * around it than around the circle. Otherwise the circle stands, which is what happens for the
    * whole first part of a session.
    */
  private def squashed(
    circle: Circle,
    inliers: Seq[Point],
    sampled: Seq[Point],
    config: DiscDetectorConfig
  ): (Circle, Seq[Point], Double) = {
    val unchanged = (circle, inliers, 0d)
    if (config.maximumFlattening <= 0d) unchanged
    else {
      // the ellipse is fitted against every boundary point, not against the inliers the circle kept
      // : those are precisely the points a circle could pass through, so the squashed part of the
      // outline has already been dropped from them
      val seed = EllipseFitting.Ellipse(circle.centerX, circle.centerY, circle.radius, circle.radius)
      EllipseFitting
        .robustOuterFit(
          points = sampled,
          seed = seed,
          rejectionRatio = 0.03d,
          seedRejectionRatio = 0.03d + config.maximumFlattening,
          minimumInlierRatio = config.minimumInlierRatio
        )
        .filter { case (ellipse, points) =>
          // the two shapes are judged on the same points, otherwise the circle would be flattered
          // by having been allowed to choose which ones it had to pass through
          val circleSpread  = CircleFitting.radialSpread(circle, points)
          val ellipseSpread = EllipseFitting.radialSpread(ellipse, points)
          val sizeShift     = math.abs(ellipse.semiHorizontal - circle.radius) / circle.radius
          ellipse.flattening > 0.01d &&
          ellipse.flattening <= config.maximumFlattening &&
          sizeShift <= 0.3d &&
          ellipseSpread < circleSpread * 0.8d &&
          EllipseFitting.angularCoverage(points, ellipse.centerX, ellipse.centerY) >= config.minimumAngularCoverage
        }
        .map { case (ellipse, points) => (ellipse.asCircle, points, ellipse.flattening) }
        .getOrElse(unchanged)
    }
  }

  /** The measured shape as an ellipse again - the detection only carries a circle and how squashed
    * it is, which is all the rest of the pipeline needs, but judging the fit needs the shape back
    */
  private def shapeOf(circle: Circle, flattening: Double): EllipseFitting.Ellipse =
    EllipseFitting.Ellipse(circle.centerX, circle.centerY, circle.radius, circle.radius * (1d - flattening))

  private def residualOf(circle: Circle, points: Seq[Point], flattening: Double): Double =
    if (flattening <= 0d) CircleFitting.residualRms(circle, points)
    else EllipseFitting.residualRms(shapeOf(circle, flattening), points)

  private def spreadOf(circle: Circle, points: Seq[Point], flattening: Double): Double =
    if (flattening <= 0d) CircleFitting.radialSpread(circle, points)
    else EllipseFitting.radialSpread(shapeOf(circle, flattening), points)

  /** How abruptly the brightness falls across the fitted edge, between 0 and 1.
    *
    * What separates a limb from a corona is not how much the brightness drops but how *fast* : the
    * solar limb falls to the sky within a pixel or two, a corona fades over hundreds. So the drop
    * measured close to the edge is compared with the drop measured far from it. On a step, both are
    * the same and the ratio is close to one ; on a corona, most of the fall happens further out and
    * the ratio stays low.
    *
    * Judging on the amplitude instead - as this did - fails twice over. At the edge of the
    * thresholded mask, which on a saturated disc sits well inside it, there is no step to be seen ;
    * and normalizing by the dynamic range of the whole frame makes a perfectly sharp but dim limb
    * look flat next to a burnt out rim. Both mistakes turned unfiltered partial phases of a real
    * session into totality frames.
    */
  private def limbContrastAcross(
    raster: GrayRaster,
    circle: Circle,
    inliers: Seq[Point],
    range: Double,
    config: DiscDetectorConfig
  ): Double = {
    if (inliers.isEmpty || range <= 0d) 0d
    else {
      val near      = config.limbContrastSpan
      val far       = near * 4d
      val sharpness = inliers.flatMap { point =>
        // the edge is where the point is, not where the radius says : on a disc squashed by
        // refraction those two differ by several percent near the top and the bottom of the limb,
        // enough to measure the sky instead of the edge
        val edge       = circle.distanceToCenter(point.x, point.y)
        val angle      = math.atan2(point.y - circle.centerY, point.x - circle.centerX)
        val directionX = math.cos(angle)
        val directionY = math.sin(angle)
        def at(distance: Double) =
          sample(raster, circle.centerX + directionX * distance, circle.centerY + directionY * distance)

        val nearDrop = at(edge - near) - at(edge + near)
        val farDrop  = at(edge - far) - at(edge + far)
        // an edge worth judging has to drop at all, and by something above the noise
        Option.when(farDrop > range * 0.02d)(math.max(0d, math.min(1d, nearDrop / farDrop)))
      }.sorted

      if (sharpness.isEmpty) 0d else sharpness(sharpness.size / 2)
    }
  }

  /** Totality : the corona is roughly symmetric around the moon, its brightness weighted centroid
    * gives a usable center. The radius cannot be measured, it is taken from the expected one.
    */
  private def fitCorona(
    raster: GrayRaster,
    mask: BitMask,
    threshold: Double,
    origin: (Double, Double),
    config: DiscDetectorConfig
  ): Either[String, DiscDetection] = {
    // the weighting is kept around the expected position when there is one : a diamond ring, a
    // reflection or a bright planet elsewhere in the frame would otherwise drag the center away
    val window = config.expectedCenter.map(_ => config.expectedRadiusPixels.getOrElse(mask.width / 8d) * 4d)
    var totalX = 0d
    var totalY = 0d
    var total  = 0d
    var y      = 0
    while (y < raster.height) {
      var x = 0
      while (x < raster.width) {
        val inside = window.forall(radius => math.hypot(x - origin._1, y - origin._2) <= radius)
        val level  = raster(x, y) - threshold
        if (inside && level > 0f) { totalX += x * level; totalY += y * level; total += level }
        x += 1
      }
      y += 1
    }
    if (total <= 0d) Left("no corona signal found")
    else {
      val weighted = (totalX / total, totalY / total)
      val fallback = config.expectedRadiusPixels.getOrElse(math.sqrt(mask.count / math.Pi))

      // The brightness weighted center is only a starting point : how far the corona reaches
      // depends on the exposure, so on a bracket that center drifts from one frame to the next and
      // the frames no longer stack. The edge of the moon does not drift - it is a sharp, dark disc
      // - so it is measured, and that is what the frames are aligned on.
      moonEdge(raster, weighted, fallback, config) match {
        case Some((circle, points)) =>
          Right(
            DiscDetection(
              circle = circle,
              kind = DiscKind.Corona,
              boundaryPointCount = points.size,
              inlierCount = points.size,
              residualRms = CircleFitting.residualRms(circle, points),
              radialSpread = CircleFitting.radialSpread(circle, points),
              thresholdLevel = threshold,
              limbContrast = 0d, // no photosphere limb : that is precisely why this branch was taken
              coverage = mask.coverage
            )
          )

        case None =>
          Right(
            DiscDetection(
              circle = Circle(weighted._1, weighted._2, fallback),
              kind = DiscKind.Corona,
              boundaryPointCount = 0,
              inlierCount = mask.count,
              residualRms = 0d,
              radialSpread = 0d,
              thresholdLevel = threshold,
              limbContrast = 0d,
              coverage = mask.coverage
            )
          )
      }
    }
  }

  /** The dark disc of the moon, measured from inside the corona.
    *
    * Rays leave the middle and stop where the brightness first rises to half way between the dark
    * middle and the brightest ring around it. That crossing is the lunar edge, and it sits at the
    * same place whatever exposure the frame was given - which is what makes a bracket stackable.
    */
  private def moonEdge(
    raster: GrayRaster,
    origin: (Double, Double),
    expectedRadius: Double,
    config: DiscDetectorConfig
  ): Option[(Circle, Seq[Point])] = {
    val reach = expectedRadius * 1.6d
    val step  = math.max(0.5d, reach / 200d)

    def levelAround(from: Double, to: Double): Double = {
      val samples = for {
        index <- 0 until 360
        angle  = math.Pi * 2d * index / 360d
        radius <- Iterator.iterate(from)(_ + step).takeWhile(_ <= to).toSeq
      } yield sample(raster, origin._1 + math.cos(angle) * radius, origin._2 + math.sin(angle) * radius)
      if (samples.isEmpty) 0d else samples.sum / samples.size
    }

    val middle = levelAround(0d, expectedRadius * 0.4d)
    val ring   = levelAround(expectedRadius * 1.05d, expectedRadius * 1.3d)
    if (ring - middle < 1e-4d) None
    else {
      val halfHeight = (middle + ring) / 2d
      val points     = (0 until config.rayCount).flatMap { index =>
        val angle      = math.Pi * 2d * index / config.rayCount
        val directionX = math.cos(angle)
        val directionY = math.sin(angle)
        var radius     = expectedRadius * 0.3d
        var crossing   = -1d
        while (radius <= reach && crossing < 0d) {
          if (sample(raster, origin._1 + directionX * radius, origin._2 + directionY * radius) >= halfHeight) crossing = radius
          radius += step
        }
        Option.when(crossing > 0d)(Point(origin._1 + directionX * crossing, origin._2 + directionY * crossing))
      }

      if (points.sizeIs < 16) None
      else
        CircleFitting
          .algebraicFit(points)
          .filter(circle => circle.radius > expectedRadius * 0.4d && circle.radius < expectedRadius * 1.6d)
          .map(circle => (circle, points))
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
    // A ray reports the farthest lit sample it meets, which is what lets it find the limb through
    // the gaps a thin crescent leaves. Near the horizon that generosity backfires : the haze around
    // the sun is lit too, hundreds of pixels out, and the ray stops there instead of on the limb.
    // The fit then comes back with a sun twice its ephemeris size, gets refused for that, and the
    // frame is lost. So when the expected size is known the rays are not allowed to look further
    // than a disc could possibly reach - counting from an origin which, on a crescent, sits on the
    // occulting body rather than on the sun.
    val reach         = List(originX, originY, mask.width - originX, mask.height - originY).max
    val maximumRadius = config.expectedRadiusPixels.fold(reach)(expected => math.min(reach, expected * 2.5d))
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

  /** Where to cast the rays from : the expected position when it is plausible, the center of
    * gravity of the lit pixels otherwise.
    *
    * The expected position does not have to fall on a lit pixel - on a crescent it lands on the
    * moon, which is exactly where rays should start from. It is only refused when it is too far
    * away from what the frame shows, which is what happens after a re-framing.
    */
  private def trustedCenter(
    config: DiscDetectorConfig,
    centroid: (Double, Double),
    raster: GrayRaster
  ): (Double, Double) =
    config.expectedCenter match {
      case None                 => centroid
      case Some((x, y))         =>
        val reach   = config.expectedRadiusPixels.map(_ * 3d).getOrElse(math.hypot(raster.width, raster.height) / 4d)
        val inFrame = x >= 0 && y >= 0 && x < raster.width && y < raster.height
        if (inFrame && math.hypot(x - centroid._1, y - centroid._2) <= reach) (x, y) else centroid
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
