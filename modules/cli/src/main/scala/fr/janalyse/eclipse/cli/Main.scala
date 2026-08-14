package fr.janalyse.eclipse.cli

import fr.janalyse.eclipse.composer.*
import fr.janalyse.eclipse.composer.FrameSelector.SelectionConfig
import fr.janalyse.eclipse.frames.{FrameAnalysisConfig, FrameAnalyzer}
import fr.janalyse.eclipse.model.{AtmosphericConditions, FrameAnalysis, GeoPoint}
import fr.janalyse.sotohp.media.imaging.Compositing.BlendMode
import fr.janalyse.sotohp.media.imaging.{BasicImaging, DiscDetector, RawDecoder}

import java.nio.file.{FileVisitOption, Files, Path, Paths}
import java.time.{Duration, ZoneOffset}
import scala.jdk.CollectionConverters.*
import scala.util.Try

object Main {

  private val usage =
    """eclipse - build a composite picture out of a solar eclipse sequence
      |
      |usage :
      |  analyze <files or directories...>   measures every frame and writes the measurements file
      |  plan    <files or measurements.csv> says what would be drawn, without drawing anything
      |  compose <files or measurements.csv> builds the composite picture
      |
      |common options :
      |  --out <path>              output file (default : measurements.csv / composite.png)
      |  --cache <dir>             where decoded RAW files are kept (default : .eclipse-cache)
      |  --observer <lat,lon[,alt]> observer position, when the frames carry no GPS data
      |  --parallelism <n>         number of frames analyzed at once (default : 2)
      |  --pressure <hPa>          atmospheric pressure, for the refraction (default : 1010)
      |  --temperature <°C>        temperature, for the refraction (default : 15)
      |
      |composition options :
      |  every setting below is worked out from the measurements when it is not given
      |  --no-auto                 keeps the plain defaults instead of the measured settings
      |  --max-pixels <n>          largest composite to produce (default : 200000000)
      |  --max-side <n>            largest composite side, in pixels (default : 24000)
      |  --layout <name>           sky-path, sky-path-even, timeline, grid
      |  --disc-radius <px>        radius of the solar disc in the composite
      |  --separation <factor>     gap between neighbour tiles, 1.0 = tiles touching (default : 1.05)
      |  --tile-factor <factor>    room kept around the disc
      |  --totality-factor <f>     room kept around a totality frame
      |  --blend <mode>            lighten (default), over, add, screen
      |  --columns <n>             grid layout only
      |  --annotate                writes the time under each frame
      |  --caption <text>          caption drawn at the bottom of the picture
      |  --no-sky-fix              keeps the sky background of each frame
      |  --no-color-fix            keeps the original color cast of the filter
      |  --no-brightness-fix       keeps the original brightness of each frame
      |  --quality <0..1>          jpeg compression level when the output is a jpeg
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    args.toList match {
      case command :: rest if Set("analyze", "plan", "compose").contains(command) =>
        val options = Options.parse(rest)
        val outcome = command match {
          case "analyze" => analyze(options)
          case "plan"    => plan(options)
          case _         => compose(options)
        }
        outcome match {
          case Left(error) => Console.err.println(s"error : $error"); sys.exit(1)
          case Right(text) => println(text)
        }
      case _                                                                      =>
        println(usage)
        sys.exit(if (args.isEmpty) 0 else 1)
    }
  }

  // ---------------------------------------------------------------------------------------------

  private def analyze(options: Options): Either[String, String] = {
    for {
      inputs <- imageInputs(options)
      _      <- Either.cond(inputs.nonEmpty, (), "no image found")
    } yield {
      val started        = System.currentTimeMillis()
      val (frames, scale) = FrameAnalyzer.analyzeAll(
        inputs,
        analysisConfig(options),
        (done, total, frame) => Console.err.println(f"[$done%4d/$total%4d] ${frame.name} ${describe(frame)}")
      )
      val output         = options.path("out").getOrElse(Paths.get("measurements.csv"))
      FrameStore.save(output, frames)
      val elapsed        = Duration.ofMillis(System.currentTimeMillis() - started)
      List(
        EclipseComposer.summary(frames),
        scale.map(value => f"plate scale (fitted)  : ${value.pixelsPerDegree}%.1f px/°").getOrElse(""),
        "",
        s"measurements written to $output (${elapsed.toMinutes} min ${elapsed.toSecondsPart} s)"
      ).filter(_.nonEmpty).mkString("\n")
    }
  }

  private def plan(options: Options): Either[String, String] = {
    for {
      frames <- loadFrames(options)
      tuned   = tuning(options, frames)
      config  = composeConfig(options, tuned)
    } yield {
      val selection       = FrameSelector.select(frames, config.selection)
      val pixelsPerDegree = EclipseComposer.pixelsPerDegree(frames, config)
      val placements      = config.layout.place(
        selection.kept,
        LayoutConfig(pixelsPerDegree, config.render.tileRadiusFactor, config.render.totalityTileRadiusFactor)
      )
      val width           = if (placements.isEmpty) 0d else placements.map(p => p.x + p.tileRadiusPixels).max - placements.map(p => p.x - p.tileRadiusPixels).min
      val height          = if (placements.isEmpty) 0d else placements.map(p => p.y + p.tileRadiusPixels).max - placements.map(p => p.y - p.tileRadiusPixels).min
      val gaps            = placements
        .sliding(2)
        .collect { case Seq(first, second) =>
          math.hypot(second.x - first.x, second.y - first.y) - (first.tileRadiusPixels + second.tileRadiusPixels)
        }
        .toList
      val cadence         = selection.kept
        .flatMap(_.instant)
        .sorted
        .sliding(2)
        .collect { case Seq(first, second) => Duration.between(first, second).getSeconds }
        .toList

      List(
        EclipseComposer.summary(frames),
        "",
        tuned.explanations.map(explanation => s"automatic             : $explanation").mkString("\n"),
        "",
        s"layout                : ${config.layout.name}",
        s"selected frames       : ${selection.keptCount} of ${selection.candidateCount}",
        f"output scale          : $pixelsPerDegree%.0f px/° (solar disc ${2 * config.render.discRadiusPixels}%.0f px)",
        f"composite size        : ${width + 2 * config.render.marginPixels}%.0f x ${height + 2 * config.render.marginPixels}%.0f px",
        f"field covered         : ${width / pixelsPerDegree}%.2f° x ${height / pixelsPerDegree}%.2f°",
        gaps.minOption.map(value => f"smallest gap          : $value%.0f px ${if (value < 0) "(OVERLAP)" else ""}").getOrElse(""),
        cadence.minOption.map(value => s"kept frames every     : ${value}s to ${cadence.max}s").getOrElse("")
      ).filter(_.nonEmpty).mkString("\n")
    }
  }

  private def compose(options: Options): Either[String, String] = {
    for {
      frames  <- loadFrames(options)
      tuned    = tuning(options, frames)
      config   = composeConfig(options, tuned)
      _        = tuned.explanations.foreach(explanation => Console.err.println(s"automatic : $explanation"))
      outcome <- EclipseComposer.compose(
                   frames,
                   config,
                   (done, total) => Console.err.println(f"[$done%4d/$total%4d] drawing")
                 )
    } yield {
      val output = options.path("out").getOrElse(Paths.get("composite.png"))
      Option(output.getParent).foreach(Files.createDirectories(_))
      BasicImaging.save(output, outcome.result.image, options.double("quality"))
      val report = outcome.result.report
      List(
        s"layout                : ${config.layout.name}",
        s"frames drawn          : ${report.drawnFrameCount} of ${outcome.selection.candidateCount}",
        s"composite             : ${report.width} x ${report.height} px",
        f"field covered         : ${report.fieldWidthDegrees}%.2f° x ${report.fieldHeightDegrees}%.2f°",
        f"equivalent lens       : ${report.equivalentFullFrameFocalLengthMillimeters}%.0f mm on 24x36",
        f"smallest gap          : ${report.smallestGapPixels}%.0f px",
        report.warnings.map(warning => s"warning               : $warning").mkString("\n"),
        "",
        s"composite written to $output"
      ).filter(_.nonEmpty).mkString("\n")
    }
  }

  // ---------------------------------------------------------------------------------------------

  private def describe(frame: FrameAnalysis): String =
    frame.disc match {
      case None       => s"failed (${frame.issues.mkString(", ")})"
      case Some(disc) =>
        f"${disc.phase} r=${disc.radiusPixels}%.1fpx obscuration=${disc.obscuration.getOrElse(0d) * 100}%.1f%% confidence=${disc.detectionConfidence}%.2f"
    }

  private def analysisConfig(options: Options): FrameAnalysisConfig =
    FrameAnalysisConfig(
      cacheDirectory = options.path("cache").getOrElse(Paths.get(".eclipse-cache")),
      rawDecode = RawDecoder.RawDecodeConfig(),
      detector = DiscDetector.DiscDetectorConfig(),
      atmosphere = AtmosphericConditions(
        pressureHectoPascals = options.double("pressure").getOrElse(1010d),
        temperatureCelsius = options.double("temperature").getOrElse(15d)
      ),
      observer = options.observer,
      parallelism = options.int("parallelism").getOrElse(2)
    )

  /** Everything is measured on the frames, then whatever was asked for explicitly takes over */
  private def tuning(options: Options, frames: Seq[FrameAnalysis]): AutoTuner.Tuning =
    if (options.flag("no-auto"))
      AutoTuner.Tuning(SelectionConfig(), SkyPathLayout(), RenderConfig(), None, List("automatic tuning disabled"))
    else
      AutoTuner.tune(
        frames,
        AutoTuner.TuningIntent(
          maximumCanvasPixels = options.double("max-pixels").map(_.toLong).getOrElse(200000000L),
          maximumCanvasSide = options.int("max-side").getOrElse(24000),
          separationFactor = options.double("separation").getOrElse(1.05d)
        )
      )

  private def composeConfig(options: Options, tuned: AutoTuner.Tuning): ComposeConfig =
    ComposeConfig(
      cacheDirectory = options.path("cache").getOrElse(Paths.get(".eclipse-cache")),
      selection = SelectionConfig(
        separationFactor = options.double("separation").getOrElse(tuned.selection.separationFactor),
        tileRadiusFactor = options.double("tile-factor").getOrElse(tuned.selection.tileRadiusFactor),
        totalityTileRadiusFactor = options.double("totality-factor").getOrElse(tuned.selection.totalityTileRadiusFactor)
      ),
      layout = options.value("layout") match {
        case Some("sky-path-even") => SkyPathLayout(evenSpacing = true)
        case Some("timeline")      => TimelineLayout()
        case Some("grid")          => GridLayout(options.int("columns"))
        case Some("sky-path")      => SkyPathLayout()
        case _                     => tuned.layout
      },
      render = RenderConfig(
        discRadiusPixels = options.double("disc-radius").getOrElse(tuned.render.discRadiusPixels),
        tileRadiusFactor = options.double("tile-factor").getOrElse(tuned.render.tileRadiusFactor),
        totalityTileRadiusFactor = options.double("totality-factor").getOrElse(tuned.render.totalityTileRadiusFactor),
        marginPixels = options.int("margin").getOrElse(tuned.render.marginPixels),
        blendMode = options.value("blend").getOrElse("lighten") match {
          case "over"   => BlendMode.Over
          case "add"    => BlendMode.Add
          case "screen" => BlendMode.Screen
          case _        => BlendMode.Lighten
        },
        subtractSkyBackground = !options.flag("no-sky-fix"),
        neutralizeColorCast = !options.flag("no-color-fix"),
        normalizeBrightness = !options.flag("no-brightness-fix"),
        annotateTimes = options.flag("annotate"),
        caption = options.value("caption"),
        captionZoneId = ZoneOffset.UTC
      )
    )

  /** Frames either come from a measurements file, or are measured on the fly */
  private def loadFrames(options: Options): Either[String, List[FrameAnalysis]] =
    options.inputs match {
      case single :: Nil if single.toString.endsWith(".csv") =>
        FrameStore.load(single, options.observer)
      case _                                                 =>
        imageInputs(options).map { inputs =>
          FrameAnalyzer
            .analyzeAll(
              inputs,
              analysisConfig(options),
              (done, total, frame) => Console.err.println(f"[$done%4d/$total%4d] ${frame.name} ${describe(frame)}")
            )
            ._1
        }
    }

  private val supportedExtensions =
    RawDecoder.rawFileExtensions ++ Set("jpg", "jpeg", "png", "tif", "tiff", "heic", "heif")

  /** Expands the given inputs into the list of images to work on.
    *
    * Directories are walked through, symbolic links are followed - pointing the tool at a link
    * named `photos-eclipse` is the expected way of using it - and extensions are matched
    * regardless of their case, `.CR3` and `.cr3` alike.
    */
  private def imageInputs(options: Options): Either[String, List[Path]] = {
    val missing = options.inputs.filterNot(Files.exists(_))
    if (options.inputs.isEmpty) Left("no input given, name a directory or a list of files")
    else if (missing.nonEmpty)
      Left(
        s"${missing.map(_.toAbsolutePath).mkString(", ")} does not exist " +
          s"(paths are resolved from ${Paths.get("").toAbsolutePath})"
      )
    else {
      val expanded = options.inputs.flatMap { input =>
        if (!Files.isDirectory(input)) List(input)
        else
          Try {
            val stream = Files.walk(input, 4, FileVisitOption.FOLLOW_LINKS)
            try stream.iterator().asScala.toList
            finally stream.close()
          }.getOrElse(Nil)
      }
      val files    = expanded.filter(Files.isRegularFile(_))
      val images   = files
        .filter(path => BasicImaging.fileTypeFromName(path).exists(supportedExtensions.contains))
        .sortBy(_.getFileName.toString)

      if (images.nonEmpty) Right(images)
      else {
        val seen = files.flatMap(path => BasicImaging.fileTypeFromName(path)).distinct.sorted
        Left(
          s"no supported image found in ${options.inputs.map(_.toAbsolutePath).mkString(", ")} : " +
            s"${files.size} files seen" +
            (if (seen.isEmpty) "" else s", extensions ${seen.mkString(", ")}") +
            s". Supported : ${supportedExtensions.toList.sorted.mkString(", ")}"
        )
      }
    }
  }

  // ---------------------------------------------------------------------------------------------

  private final case class Options(inputs: List[Path], values: Map[String, String], flags: Set[String]) {
    def value(name: String): Option[String]  = values.get(name)
    def double(name: String): Option[Double] = values.get(name).flatMap(text => Try(text.toDouble).toOption)
    def int(name: String): Option[Int]       = values.get(name).flatMap(text => Try(text.toInt).toOption)
    def path(name: String): Option[Path]     = values.get(name).map(Paths.get(_))
    def flag(name: String): Boolean          = flags.contains(name)

    def observer: Option[GeoPoint] =
      values.get("observer").flatMap { text =>
        text.split(",").map(_.trim).toList match {
          case latitude :: longitude :: rest =>
            for {
              parsedLatitude  <- Try(latitude.toDouble).toOption
              parsedLongitude <- Try(longitude.toDouble).toOption
            } yield GeoPoint(parsedLatitude, parsedLongitude, rest.headOption.flatMap(value => Try(value.toDouble).toOption).getOrElse(0d))
          case _                             => None
        }
      }
  }

  private object Options {
    private val valuedOptions = Set(
      "out", "cache", "observer", "parallelism", "pressure", "temperature",
      "layout", "disc-radius", "separation", "tile-factor", "totality-factor",
      "blend", "columns", "caption", "quality", "max-pixels", "max-side", "margin"
    )

    def parse(arguments: List[String]): Options = {
      val inputs = List.newBuilder[Path]
      val values = Map.newBuilder[String, String]
      val flags  = Set.newBuilder[String]
      var rest   = arguments
      while (rest.nonEmpty) {
        rest match {
          case option :: value :: tail if option.startsWith("--") && valuedOptions.contains(option.drop(2)) =>
            values += option.drop(2) -> value
            rest = tail
          case option :: tail if option.startsWith("--")                                                    =>
            flags += option.drop(2)
            rest = tail
          case input :: tail                                                                                =>
            inputs += Paths.get(input)
            rest = tail
          case Nil                                                                                          =>
            rest = Nil
        }
      }
      Options(inputs.result(), values.result(), flags.result())
    }
  }
}
