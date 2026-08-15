package fr.janalyse.eclipse.composer

import fr.janalyse.eclipse.model.{FrameAnalysis, FramePhase, PlateScale}
import fr.janalyse.sotohp.media.imaging.CircleFitting.Circle
import fr.janalyse.sotohp.media.imaging.Compositing.{BlendMode, Canvas}
import fr.janalyse.sotohp.media.imaging.Rasters.RgbRaster
import fr.janalyse.sotohp.media.imaging.{ColorBalance, Compositing, DiscMeasures, ExposureStack, RawDecoder, ToneMapping}

import java.awt.image.BufferedImage
import java.awt.{Color, Font, RenderingHints}
import java.nio.file.Path
import java.time.format.DateTimeFormatter
import java.time.{Duration, ZoneOffset}
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

final case class RenderConfig(
  /** radius of the solar disc in the composite, this sets the scale of everything */
  discRadiusPixels: Double = 90d,
  tileRadiusFactor: Double = 1.5d,
  totalityTileRadiusFactor: Double = 3d,
  blendMode: BlendMode = BlendMode.Lighten,
  /** width of the fade at the tile edge, in tile radius units */
  featherRatio: Double = 0.25d,
  background: Color = Color.BLACK,
  marginPixels: Int = 80,
  /** merges the whole bracket of a totality burst into one tile, rather than choosing one of them */
  stackTotality: Boolean = true,
  /** measures the sky level around the subject and subtracts it, tile per tile */
  subtractSkyBackground: Boolean = true,
  /** cancels the color cast of the solar filter */
  neutralizeColorCast: Boolean = true,
  /** color given to the solar disc once neutralized, slightly warm by default */
  discColor: (Double, Double, Double) = (1d, 0.94d, 0.86d),
  /** brings every disc to the same brightness, whatever the exposure was */
  normalizeBrightness: Boolean = true,
  targetDiscLevel: Double = 0.80d,
  /** totality frames are stretched on their own, the corona spans a huge dynamic range */
  totalityGamma: Double = 2.2d,
  totalityHighlightRatio: Double = 0.9995d,
  annotateTimes: Boolean = false,
  caption: Option[String] = None,
  captionZoneId: ZoneOffset = ZoneOffset.UTC,
  /** safety net, a composite bigger than that gets scaled down */
  maximumCanvasPixels: Long = 250000000L
)

final case class CompositeReport(
  width: Int,
  height: Int,
  drawnFrameCount: Int,
  /** how many tiles were merged from a bracket rather than drawn from a single exposure */
  stackedFrameCount: Int,
  pixelsPerDegree: Double,
  fieldWidthDegrees: Double,
  fieldHeightDegrees: Double,
  equivalentFullFrameFocalLengthMillimeters: Double,
  smallestGapPixels: Double,
  scaleReduction: Double,
  warnings: List[String]
) {
  def overlaps: Boolean = smallestGapPixels < 0d
}

/** @param placements where each frame finally landed, in canvas coordinates : handy to annotate
  *                   the composite afterwards, or to build an interactive map of it
  */
final case class CompositeResult(
  image: BufferedImage,
  report: CompositeReport,
  placements: Vector[Placement]
)

/** Draws the composite : one tile per selected frame, blended at its own place */
object CompositeRenderer {

  private val timeFormat = DateTimeFormatter.ofPattern("HH:mm:ss")

  def render(
    placements: Seq[Placement],
    config: RenderConfig = RenderConfig(),
    cacheDirectory: Path,
    rawDecode: RawDecoder.RawDecodeConfig = RawDecoder.RawDecodeConfig(),
    onProgress: (Int, Int) => Unit = (_, _) => ()
  ): Either[String, CompositeResult] = {
    if (placements.isEmpty) Left("nothing to draw")
    else {
      val warnings = List.newBuilder[String]

      // --- canvas geometry --------------------------------------------------------------------
      val minimumX = placements.map(placement => placement.x - placement.tileRadiusPixels).min
      val maximumX = placements.map(placement => placement.x + placement.tileRadiusPixels).max
      val minimumY = placements.map(placement => placement.y - placement.tileRadiusPixels).min
      val maximumY = placements.map(placement => placement.y + placement.tileRadiusPixels).max
      val rawWidth  = maximumX - minimumX + 2 * config.marginPixels
      val rawHeight = maximumY - minimumY + 2 * config.marginPixels

      val reduction = {
        val pixels = rawWidth * rawHeight
        if (pixels <= config.maximumCanvasPixels) 1d
        else {
          val factor = math.sqrt(config.maximumCanvasPixels / pixels)
          warnings += f"composite scaled down by ${factor}%.3f to stay under ${config.maximumCanvasPixels} pixels"
          factor
        }
      }

      val scaled = placements.map { placement =>
        placement.copy(
          x = (placement.x - minimumX + config.marginPixels) * reduction,
          y = (placement.y - minimumY + config.marginPixels) * reduction,
          discRadiusPixels = placement.discRadiusPixels * reduction,
          tileRadiusPixels = placement.tileRadiusPixels * reduction
        )
      }

      val width  = math.max(1, math.ceil(rawWidth * reduction).toInt)
      val height = math.max(1, math.ceil(rawHeight * reduction).toInt)
      val canvas = Canvas.create(width, height, config.background)
      val masks  = mutable.Map.empty[(Int, Int, Int), Array[Float]]

      // --- tiles ------------------------------------------------------------------------------
      var drawn = 0
      var stacked = 0
      scaled.zipWithIndex.foreach { case (placement, index) =>
        onProgress(index + 1, scaled.size)
        val frame = placement.frame
        val drawnTile = for {
          disc  <- frame.disc.toRight(s"${frame.name} : no measured disc")
          image <- RawDecoder.load(frame.path, cacheDirectory, rawDecode)
        } yield {
          val factor = frame.phase match {
            case FramePhase.Totality => config.totalityTileRadiusFactor
            case _                   => config.tileRadiusFactor
          }
          val tile   = Compositing.extractTile(
            image = image,
            disc = Circle(disc.centerX, disc.centerY, disc.radiusPixels),
            targetDiscRadiusPixels = placement.discRadiusPixels,
            radiusFactor = factor
          )
          val merged = if (placement.stack.sizeIs > 1) stackedTile(placement, factor, cacheDirectory, rawDecode, warnings) else None
          if (merged.isDefined) stacked += 1
          val ready  = merged.getOrElse(adjust(tile.image, placement.discRadiusPixels, frame.phase, config))
          val mask   = maskFor(masks, ready.getWidth, placement.discRadiusPixels, frame.phase, config)
          canvas.drawCenteredOn(ready, placement.x, placement.y, config.blendMode, Some(mask))
          if (config.annotateTimes) annotateTime(canvas, placement, config)
          ()
        }
        drawnTile match {
          case Left(error) => warnings += error
          case Right(_)    => drawn += 1
        }
      }

      config.caption.foreach(text => drawCaption(canvas, text, config))

      // --- report -----------------------------------------------------------------------------
      val pixelsPerDegree = scaled
        .flatMap(placement => placement.frame.sun.map(sun => placement.discRadiusPixels / sun.semiDiameterDegrees))
        .headOption
        .getOrElse(config.discRadiusPixels / 0.266d)

      val gaps = scaled
        .sliding(2)
        .collect { case Seq(first, second) =>
          math.hypot(second.x - first.x, second.y - first.y) - (first.tileRadiusPixels + second.tileRadiusPixels)
        }
        .toList
      val smallestGap = gaps.minOption.getOrElse(0d)
      if (smallestGap < 0d) warnings += f"tiles overlap by ${-smallestGap}%.0f pixels, increase the separation factor"

      val fieldWidth  = width / pixelsPerDegree
      val fieldHeight = height / pixelsPerDegree

      Right(
        CompositeResult(
          image = canvas.image,
          placements = scaled.toVector,
          report = CompositeReport(
            width = width,
            height = height,
            drawnFrameCount = drawn,
            stackedFrameCount = stacked,
            pixelsPerDegree = pixelsPerDegree,
            fieldWidthDegrees = fieldWidth,
            fieldHeightDegrees = fieldHeight,
            equivalentFullFrameFocalLengthMillimeters = PlateScale(pixelsPerDegree).equivalentFullFrameFocalLength(fieldWidth),
            smallestGapPixels = smallestGap,
            scaleReduction = reduction,
            warnings = warnings.result()
          )
        )
      )
    }
  }

  /** Merges a totality bracket into a single tile.
    *
    * Each exposure is cut out around *its own* measured disc, so the frames land on top of one
    * another whatever the sun was doing in the frame - the alignment is free, it comes from the
    * measurement that was made anyway. They are then merged on the strength of their exposure
    * values, and stretched hard : a corona falls off by orders of magnitude from the limb outwards.
    */
  private def stackedTile(
    placement: Placement,
    radiusFactor: Double,
    cacheDirectory: Path,
    rawDecode: RawDecoder.RawDecodeConfig,
    warnings: mutable.Builder[String, List[String]]
  ): Option[java.awt.image.BufferedImage] = {
    // One radius for the whole burst. The moon does not change size in the half minute a bracket
    // takes, so the small differences between the measured radii are measurement artefacts - the
    // bright inner corona eats into the edge on the long exposures. Scaling each frame by its own
    // radius would stretch them differently and leave a coloured seam around the moon.
    val radii        = placement.stack.flatMap(_.disc).map(_.radiusPixels).sorted
    val commonRadius = if (radii.isEmpty) 0d else radii(radii.size / 2)

    val exposures = placement.stack.flatMap { frame =>
      for {
        disc     <- frame.disc
        exposure <- relativeExposure(frame)
        if commonRadius > 0d
        image    <- RawDecoder.load(frame.path, cacheDirectory, rawDecode).toOption
      } yield {
        val tile = Compositing.extractTile(
          image = image,
          disc = Circle(disc.centerX, disc.centerY, commonRadius),
          targetDiscRadiusPixels = placement.discRadiusPixels,
          radiusFactor = radiusFactor
        )
        ExposureStack.Exposure(RgbRaster.fromImage(tile.image), exposure)
      }
    }

    if (exposures.sizeIs < 2) {
      warnings += s"${placement.frame.name} : the totality burst could not be merged, a single exposure was usable"
      None
    } else {
      val spread = exposures.map(_.relativeExposure).max / exposures.map(_.relativeExposure).min
      ExposureStack.merge(exposures).map { merged =>
        warnings += f"${placement.frame.name} : ${exposures.size} exposures merged, spanning ${math.log(spread) / math.log(2d)}%.1f stops"
        blackenMoon(ExposureStack.display(merged), placement.discRadiusPixels).toImage
      }
    }
  }

  /** Puts the moon back to black at the middle of a merged tile.
    *
    * The moon moves in front of the sun - about fifteen pixels over the half minute a bracket takes
    * - so frames stacked on the lunar edge no longer line up on the corona, which then spills a
    * bright crescent over the dark disc. Whatever ends up inside that disc is an artefact one way
    * or another : the moon emits nothing. The fade stops just short of the edge so that the
    * prominences, which stand right outside it, are left untouched.
    */
  private def blackenMoon(tile: RgbRaster, moonRadius: Double, feather: Double = 0.03d): RgbRaster = {
    val centerX = tile.width / 2d
    val centerY = tile.height / 2d
    val inner   = moonRadius * (1d - feather)
    val red     = tile.red.clone()
    val green   = tile.green.clone()
    val blue    = tile.blue.clone()
    var y       = 0
    while (y < tile.height) {
      var x = 0
      while (x < tile.width) {
        val distance = math.hypot(x + 0.5d - centerX, y + 0.5d - centerY)
        if (distance < moonRadius) {
          val keep  = if (distance <= inner) 0f else ((distance - inner) / (moonRadius - inner)).toFloat
          val index = y * tile.width + x
          red(index) = red(index) * keep
          green(index) = green(index) * keep
          blue(index) = blue(index) * keep
        }
        x += 1
      }
      y += 1
    }
    RgbRaster(tile.width, tile.height, red, green, blue)
  }

  /** How much light the settings let in, in units that only matter relative to one another */
  private def relativeExposure(frame: FrameAnalysis): Option[Double] =
    for {
      time     <- frame.metadata.exposureTimeSeconds
      aperture  = frame.metadata.aperture.getOrElse(8d)
      if time > 0d && aperture > 0d
    } yield time * frame.metadata.isoSensitivity.getOrElse(100d) / (aperture * aperture)

  /** Color cast removal and brightness normalization, so that frames shot through a very dense
    * filter and frames shot without any filter at all can live in the same picture.
    */
  private def adjust(tile: BufferedImage, discRadiusPixels: Double, phase: FramePhase, config: RenderConfig): BufferedImage = {
    val loaded   = RgbRaster.fromImage(tile)
    val halfSize = tile.getWidth / 2d
    val disc     = Circle(halfSize, halfSize, discRadiusPixels * 0.85d)

    // the sky is not black : at a total eclipse it is a twilight sky, with a gradient and a color
    // of its own. Measured on a ring drawn where the subject is not, and subtracted, so that the
    // tiles do not show up as bright patches on the composite.
    val raster =
      if (!config.subtractSkyBackground) loaded
      else {
        val innerRadius = phase match {
          case FramePhase.Totality => halfSize * 0.85d // the corona spreads far, stay at the very edge
          case _                   => math.min(halfSize * 0.85d, discRadiusPixels * 1.3d)
        }
        DiscMeasures
          .medianColorInRing(loaded, Circle(halfSize, halfSize, 0d), innerRadius, halfSize)
          .map(levels => ColorBalance.removeBackground(loaded, levels))
          .getOrElse(loaded)
      }

    phase match {
      case FramePhase.Totality =>
        val gray      = raster.toGray
        val bounds    = gray.percentiles(List(0.5d, config.totalityHighlightRatio))
        val stretched = ToneMapping.levels(raster, bounds.head, bounds.last, config.totalityGamma)
        stretched.toImage

      case _ =>
        // Subtracting the sky level leaves its grain and its gradient behind, and the brightness
        // normalization that follows would multiply those along with the disc - the more so as the
        // sun gets low and dim. Anything still within the wavering of the sky is therefore clipped
        // away first : past the limb, a filtered frame holds nothing worth keeping anyway.
        val cleaned =
          if (!config.subtractSkyBackground) raster
          else
            DiscMeasures
              .luminancePercentilesInRing(raster, Circle(halfSize, halfSize, 0d), discRadiusPixels * 1.15d, halfSize, List(0.98d))
              .map(levels => ColorBalance.removeBackground(raster, levels.head))
              .getOrElse(raster)

        // The reference is the brightness of the photosphere itself, read on the lit pixels alone.
        // Over the disc area it would collapse as the moon covers it, and over the whole tile it
        // would depend on how much of that tile the crescent happens to fill : either way a thin
        // crescent ends up stretched until it saturates, coming out white next to its neighbours.
        val gray             = cleaned.toGray
        val litLevel         = math.max(0.01d, gray.percentile(0.9999d) * 0.5d)
        val photosphereLevel = gray.medianAbove(litLevel).getOrElse(gray.percentile(0.999d))

        val neutralized =
          if (!config.neutralizeColorCast) cleaned
          else
            DiscMeasures
              .meanColorWithin(cleaned, disc, litLevel)
              .map(reference => ColorBalance.neutralizeFrom(cleaned, reference, config.discColor))
              .getOrElse(cleaned)

        val normalized =
          if (!config.normalizeBrightness) neutralized
          else {
            val corrected = neutralized.toGray
            val reference = corrected.medianAbove(litLevel).getOrElse(corrected.percentile(0.999d))
            ToneMapping.normalizeReferenceLevel(neutralized, reference, config.targetDiscLevel)
          }
        normalized.toImage
    }
  }

  private def maskFor(
    cache: mutable.Map[(Int, Int, Int), Array[Float]],
    size: Int,
    discRadiusPixels: Double,
    phase: FramePhase,
    config: RenderConfig
  ): Array[Float] = {
    val halfSize = size / 2d
    val (inner, outer) = phase match {
      // the tile of a totality frame is sized on the corona actually recorded, so the fade has to
      // stay at its very edge : starting it half way in would rub out the outer corona, which is
      // the one thing those frames are there for
      case FramePhase.Totality => (halfSize * (1d - config.featherRatio), halfSize)
      case _                   =>
        val edge = math.min(halfSize, discRadiusPixels * 1.04d)
        (edge, math.min(halfSize, edge * (1d + config.featherRatio)))
    }
    cache.getOrElseUpdate(
      (size, math.round(inner).toInt, math.round(outer).toInt),
      Compositing.radialMask(size, inner, outer)
    )
  }

  private def annotateTime(canvas: Canvas, placement: Placement, config: RenderConfig): Unit = {
    placement.frame.shotAt.foreach { shotAt =>
      val graphics = canvas.image.createGraphics
      try {
        graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
        val fontSize = math.max(9, (placement.discRadiusPixels / 4d).toInt)
        graphics.setFont(Font(Font.SANS_SERIF, Font.PLAIN, fontSize))
        graphics.setColor(Color(190, 190, 190))
        val text  = shotAt.withOffsetSameInstant(config.captionZoneId).format(timeFormat)
        val width = graphics.getFontMetrics.stringWidth(text)
        graphics.drawString(
          text,
          (placement.x - width / 2d).toInt,
          (placement.y + placement.tileRadiusPixels + fontSize * 1.2d).toInt
        )
      } finally graphics.dispose()
    }
  }

  private def drawCaption(canvas: Canvas, caption: String, config: RenderConfig): Unit = {
    val graphics = canvas.image.createGraphics
    try {
      graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
      val fontSize = math.max(12, canvas.width / 120)
      graphics.setFont(Font(Font.SANS_SERIF, Font.PLAIN, fontSize))
      graphics.setColor(Color(210, 210, 210))
      caption.split("\n").zipWithIndex.foreach { case (line, index) =>
        graphics.drawString(
          line,
          config.marginPixels / 2,
          canvas.height - config.marginPixels / 2 - (caption.count(_ == '\n') - index) * (fontSize + 4)
        )
      }
    } finally graphics.dispose()
  }

  /** Handy when reporting : how long a session lasted */
  def duration(placements: Seq[Placement]): Option[Duration] =
    for {
      first <- placements.flatMap(_.frame.instant).minOption
      last  <- placements.flatMap(_.frame.instant).maxOption
    } yield Duration.between(first, last)
}
