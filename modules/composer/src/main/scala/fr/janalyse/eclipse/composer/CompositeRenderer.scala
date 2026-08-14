package fr.janalyse.eclipse.composer

import fr.janalyse.eclipse.model.{FramePhase, PlateScale}
import fr.janalyse.sotohp.media.imaging.CircleFitting.Circle
import fr.janalyse.sotohp.media.imaging.Compositing.{BlendMode, Canvas}
import fr.janalyse.sotohp.media.imaging.Rasters.RgbRaster
import fr.janalyse.sotohp.media.imaging.{ColorBalance, Compositing, DiscMeasures, RawDecoder, ToneMapping}

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
          val ready  = adjust(tile.image, placement.discRadiusPixels, frame.phase, config)
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
        // The reference is the brightness of the photosphere itself, taken as a high percentile of
        // the whole tile. Measuring it over the disc area would collapse as the moon covers it :
        // a thin crescent would then be stretched until it saturates, and would come out white or
        // oddly tinted while its neighbours stay correct.
        val photosphereLevel = raster.toGray.percentile(0.999d)
        val litLevel         = math.max(0.01d, photosphereLevel * 0.5d)

        val neutralized =
          if (!config.neutralizeColorCast) raster
          else
            DiscMeasures
              .meanColorWithin(raster, disc, litLevel)
              .map(reference => ColorBalance.neutralizeFrom(raster, reference, config.discColor))
              .getOrElse(raster)

        val normalized =
          if (!config.normalizeBrightness) neutralized
          else {
            val reference = neutralized.toGray.percentile(0.999d)
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
      case FramePhase.Totality => (halfSize * (1d - config.featherRatio * 2d), halfSize)
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
