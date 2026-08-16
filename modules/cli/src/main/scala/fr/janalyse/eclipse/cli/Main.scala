package fr.janalyse.eclipse.cli

import fr.janalyse.eclipse.astro.Refraction
import fr.janalyse.eclipse.composer.*
import fr.janalyse.eclipse.composer.FrameSelector.SelectionConfig
import fr.janalyse.eclipse.frames.{FrameAnalysisConfig, FrameAnalyzer, Shot}
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
      |  inspect <files or directories...>   draws what the detection found, to be looked at
      |  plan    <files or measurements.csv> says what would be drawn, without drawing anything
      |  compose <files or measurements.csv> builds the composite picture
      |
      |common options :
      |  --out <path>              output file (default : measurements.csv / composite.png)
      |  --cache <dir>             where decoded RAW files are kept (default : .eclipse-cache)
      |  --observer <lat,lon[,alt]> observer position, needed only when NO frame carries a fix :
      |                            a single fix in the whole session places all the others
      |  --parallelism <n>         number of frames analyzed at once (default : 2)
      |  --prefer-jpeg             measures and draws from the JPEG of a RAW+JPEG pair
      |  --pressure <hPa>          atmospheric pressure, for the refraction (default : 1010)
      |  --temperature <°C>        temperature, for the refraction (default : 15)
      |
      |composition options :
      |  every setting below is worked out from the measurements when it is not given
      |  --no-auto                 keeps the plain defaults instead of the measured settings
      |  --min-confidence <0..1>   how sure a measurement has to be to be drawn (default : 0.2)
      |  --balanced                keeps as many frames before the maximum as after it
      |  --frames-per-side <n>     hard limit on the number of frames on each side of the maximum
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
      |
      |background options :
      |  a wide angle shot of the same sky, from the same place, drawn behind the sequence and
      |  placed by its own sun - the landscape the long lens could not hold
      |  --background <file>       the wide angle frame to use as scenery
      |  --background-margin <deg> sky kept around the sequence to show it (default : 4°)
      |  --background-brightness <0..1>  dims it, so that the suns stay the subject
      |  --background-roll <deg>   camera roll, positive when its horizon runs down to the right
      |  --background-sun <x,y>    where the sun is on it, when it cannot be found by itself
      |  --background-scale <px/°> its plate scale, when its metadata does not give the optics
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    args.toList match {
      case command :: rest if Set("analyze", "plan", "compose", "inspect").contains(command) =>
        val options = Options.parse(rest)
        val outcome = command match {
          case "analyze" => analyze(options)
          case "plan"    => plan(options)
          case "inspect" => inspect(options)
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
      val started  = System.currentTimeMillis()
      val shots    = Shot.group(inputs)
      Console.err.println(
        s"${inputs.size} files, ${shots.size} shots" +
          (if (shots.count(_.hasBoth) > 0) s" (${shots.count(_.hasBoth)} as RAW+JPEG pairs)" else "")
      )
      val session  = FrameAnalyzer.analyzeAll(
        inputs,
        analysisConfig(options),
        (done, total, frame) => Console.err.println(f"[$done%4d/$total%4d] ${frame.name} ${describe(frame)}")
      )
      val output   = options.path("out").getOrElse(Paths.get("measurements.csv"))
      FrameStore.save(output, session.frames)
      val elapsed  = Duration.ofMillis(System.currentTimeMillis() - started)
      List(
        EclipseComposer.summary(session.frames),
        s"shots                 : ${shots.size} from ${inputs.size} files" +
          (if (shots.exists(_.hasBoth)) s", ${shots.count(_.hasBoth)} of them written as RAW+JPEG" else ""),
        s"position              : ${session.location.describe}",
        if (session.location.looksMoved()) f"warning               : the fixes are spread over ${session.location.spreadMeters}%.0f m, was the camera moved ?" else "",
        session.anchor.map(frame => s"analysis started from : ${frame.name}").getOrElse(""),
        session.plateScale.map(value => f"plate scale (fitted)  : ${value.pixelsPerDegree}%.1f px/°").getOrElse(""),
        "",
        s"measurements written to $output (${elapsed.toMinutes} min ${elapsed.toSecondsPart} s)"
      ).filter(_.nonEmpty).mkString("\n")
    }
  }

  /** Draws what the detection found on top of the frames, to be looked at.
    *
    * Numbers only say so much : a fit whose residual is large may be a disc cut by a branch, a sun
    * touching the frame border, or a genuinely bad measurement, and the eye tells them apart in a
    * second where a report cannot.
    */
  private def inspect(options: Options): Either[String, String] = {
    for {
      inputs <- imageInputs(options)
    } yield {
      val directory = options.path("out").getOrElse(Paths.get("out/inspect"))
      Files.createDirectories(directory)
      val config    = analysisConfig(options)
      val shots     = Shot.group(inputs)
      val lines     = shots.map { shot =>
        val frame = FrameAnalyzer.analyzeShot(shot, config)
        frame.disc match {
          case None       => s"${shot.name} : ${frame.issues.mkString(", ")}"
          case Some(disc) =>
            val drawn = RawDecoder
              .load(frame.path, config.cacheDirectory, config.rawDecode)
              .map { image =>
                val marked   = BasicImaging.convertTo(image, java.awt.image.BufferedImage.TYPE_INT_RGB)
                val graphics = marked.createGraphics
                try {
                  // the outline is drawn as it was measured : squashed when refraction squashed it,
                  // so that what the detector found can be compared with what the frame shows
                  val verticalRadius = disc.radiusPixels * (1d - disc.flattening)
                  graphics.setStroke(java.awt.BasicStroke(math.max(2f, disc.radiusPixels.toFloat / 60f)))
                  graphics.setColor(java.awt.Color.GREEN)
                  graphics.drawOval(
                    (disc.centerX - disc.radiusPixels).toInt,
                    (disc.centerY - verticalRadius).toInt,
                    (disc.radiusPixels * 2).toInt,
                    (verticalRadius * 2).toInt
                  )
                  val arm = (disc.radiusPixels / 4).toInt
                  graphics.setColor(java.awt.Color.RED)
                  graphics.drawLine(disc.centerX.toInt - arm, disc.centerY.toInt, disc.centerX.toInt + arm, disc.centerY.toInt)
                  graphics.drawLine(disc.centerX.toInt, disc.centerY.toInt - arm, disc.centerX.toInt, disc.centerY.toInt + arm)
                } finally graphics.dispose()
                val output = directory.resolve(s"${shot.name}-detection.png")
                BasicImaging.save(output, BasicImaging.fitWithin(marked, options.int("inspect-size").getOrElse(1400)))
                output
              }
            val expected = frame.sun
              .map(sun => Refraction.flattening(sun.geometric.altitudeDegrees, sun.semiDiameterDegrees))
              .map(value => f" (refraction predicts ${value * 100}%.1f%%)")
              .getOrElse("")
            f"${shot.name} : ${disc.phase} center=(${disc.centerX}%.1f,${disc.centerY}%.1f) r=${disc.radiusPixels}%.1f " +
              f"residual=${disc.fitResidualPixels}%.2fpx confidence=${disc.detectionConfidence}%.2f " +
              f"limbContrast=${disc.limbContrast}%.2f room=${disc.roomFactor.getOrElse(0d)}%.2f " +
              f"flattening=${disc.flattening * 100}%.1f%%$expected " +
              drawn.map(path => s"-> $path").getOrElse("(image not drawn)")
        }
      }
      lines.mkString("\n")
    }
  }

  private def plan(options: Options): Either[String, String] = {
    for {
      frames     <- loadFrames(options)
      background <- backgroundOf(options, frames)
      tuned       = tuning(options, frames, background)
      config      = composeConfig(options, tuned, background.map(_.background))
    } yield {
      val selection       = FrameSelector.select(frames, config.selection)
      val pixelsPerDegree = EclipseComposer.pixelsPerDegree(frames, config)
      val placements      = config.layout.place(
        selection.kept,
        LayoutConfig(pixelsPerDegree, config.render.tileRadiusFactor, config.render.totalityTileRadiusFactor)
      )
      // the very same reckoning the renderer does, background included : what is announced here is
      // what will come out
      val canvas          = Option.when(placements.nonEmpty)(
        CompositeRenderer.canvasPlan(placements, config.render, config.layout.skyFrame(selection.kept))
      )
      val width           = canvas.map(_.width(0)).getOrElse(0d)
      val height          = canvas.map(_.height(0)).getOrElse(0d)
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
        if (frames.exists(_.isUsable)) "" else "\n" + EclipseComposer.diagnose(frames),
        "",
        tuned.explanations.map(explanation => s"automatic             : $explanation").mkString("\n"),
        "",
        s"layout                : ${config.layout.name}",
        s"selected frames       : ${selection.keptCount} of ${selection.candidateCount}" +
          (selection.kept.count(_.phase == fr.janalyse.eclipse.model.FramePhase.Totality) match {
            case 0     => ", none of them during totality"
            case count => s", $count of them during totality"
          }),
        FrameSelector.representativeMaximum(frames.filter(_.isUsable)).instant match {
          case None          => ""
          case Some(maximum) =>
            val before = selection.kept.count(_.instant.exists(_.isBefore(maximum)))
            val after  = selection.kept.count(_.instant.exists(_.isAfter(maximum)))
            s"around the maximum    : $before before, $after after"
        },
        f"output scale          : $pixelsPerDegree%.0f px/° (solar disc ${2 * config.render.discRadiusPixels}%.0f px)",
        f"composite size        : ${width + 2 * config.render.marginPixels}%.0f x ${height + 2 * config.render.marginPixels}%.0f px",
        f"field covered         : ${width / pixelsPerDegree}%.2f° x ${height / pixelsPerDegree}%.2f°",
        gaps.minOption.map(value => f"smallest gap          : $value%.0f px ${if (value < 0) "(OVERLAP)" else ""}").getOrElse(""),
        cadence.minOption.map(value => s"kept frames every     : ${value}s to ${cadence.max}s").getOrElse(""),
        background.map(_.report.map(line => s"background            : $line").mkString("\n")).getOrElse(""),
        canvas.toList.flatMap(_.notes).map(note => s"note                  : $note").mkString("\n")
      ).filter(_.nonEmpty).mkString("\n")
    }
  }

  private def compose(options: Options): Either[String, String] = {
    for {
      frames     <- loadFrames(options)
      background <- backgroundOf(options, frames)
      _           = background.foreach(_.report.foreach(line => Console.err.println(s"background : $line")))
      tuned       = tuning(options, frames, background)
      config      = composeConfig(options, tuned, background.map(_.background))
      _           = tuned.explanations.foreach(explanation => Console.err.println(s"automatic : $explanation"))
      outcome    <- EclipseComposer.compose(
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
        if (report.stackedFrameCount > 0) s"exposure brackets     : ${report.stackedFrameCount} tile(s) merged from several exposures" else "",
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

  private def cacheDirectory(options: Options): Path = options.path("cache").getOrElse(Paths.get(".eclipse-cache"))

  private def atmosphere(options: Options): AtmosphericConditions =
    AtmosphericConditions(
      pressureHectoPascals = options.double("pressure").getOrElse(1010d),
      temperatureCelsius = options.double("temperature").getOrElse(15d)
    )

  private def analysisConfig(options: Options): FrameAnalysisConfig =
    FrameAnalysisConfig(
      cacheDirectory = cacheDirectory(options),
      rawDecode = RawDecoder.RawDecodeConfig(),
      detector = DiscDetector.DiscDetectorConfig(),
      atmosphere = atmosphere(options),
      observer = options.observer,
      preferRawPixels = !options.flag("prefer-jpeg"),
      parallelism = options.int("parallelism").getOrElse(2)
    )

  private def backgroundMarginDegrees(options: Options): Double = options.double("background-margin").getOrElse(4d)

  /** Reads the wide angle frame given as scenery, and works out where it was aimed.
    *
    * The session it belongs to hands it what its own metadata does not carry : where the camera
    * stood - a second body rarely has a GPS fix, and it did not move anyway - and when the session
    * happened, which is what a clock left on the wrong time zone is put right against.
    */
  private def backgroundOf(options: Options, frames: Seq[FrameAnalysis]): Either[String, Option[SkyBackground.Loaded]] =
    options.path("background") match {
      case None       => Right(None)
      case Some(path) =>
        val (sessionObserver, sessionSpan) = SkyBackground.sessionContext(frames)
        SkyBackground
          .load(
            SkyBackground.Request(
              path = path,
              cacheDirectory = cacheDirectory(options),
              observer = options.observer.orElse(sessionObserver),
              atmosphere = atmosphere(options),
              sessionSpan = sessionSpan,
              rollDegrees = options.double("background-roll").getOrElse(0d),
              brightness = options.double("background-brightness").getOrElse(1d),
              sunPixel = options.pair("background-sun"),
              pixelsPerDegree = options.double("background-scale"),
              preferRawPixels = !options.flag("prefer-jpeg")
            )
          )
          .map(Some(_))
    }

  /** Everything is measured on the frames, then whatever was asked for explicitly takes over */
  private def tuning(options: Options, frames: Seq[FrameAnalysis], background: Option[SkyBackground.Loaded]): AutoTuner.Tuning =
    if (options.flag("no-auto"))
      AutoTuner.Tuning(SelectionConfig(), SkyPathLayout(), RenderConfig(), None, List("automatic tuning disabled"))
    else
      AutoTuner.tune(
        frames,
        AutoTuner.TuningIntent(
          maximumCanvasPixels = options.double("max-pixels").map(_.toLong).getOrElse(200000000L),
          maximumCanvasSide = options.int("max-side").getOrElse(24000),
          separationFactor = options.double("separation").getOrElse(1.05d),
          balanced = options.flag("balanced") || options.int("frames-per-side").isDefined,
          framesPerSide = options.int("frames-per-side"),
          extraFieldDegrees = if (background.isDefined) backgroundMarginDegrees(options) else 0d
        )
      )

  private def composeConfig(options: Options, tuned: AutoTuner.Tuning, background: Option[SkyBackground]): ComposeConfig =
    ComposeConfig(
      cacheDirectory = cacheDirectory(options),
      selection = SelectionConfig(
        separationFactor = options.double("separation").getOrElse(tuned.selection.separationFactor),
        tileRadiusFactor = options.double("tile-factor").getOrElse(tuned.selection.tileRadiusFactor),
        totalityTileRadiusFactor = options.double("totality-factor").getOrElse(tuned.selection.totalityTileRadiusFactor),
        minimumConfidence = options.double("min-confidence").getOrElse(tuned.selection.minimumConfidence),
        balanced = options.flag("balanced") || options.int("frames-per-side").isDefined,
        framesPerSide = options.int("frames-per-side")
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
        captionZoneId = ZoneOffset.UTC,
        background = background,
        backgroundMarginDegrees = backgroundMarginDegrees(options)
      )
    )

  /** Frames either come from a measurements file, or are measured on the fly */
  private def loadFrames(options: Options): Either[String, List[FrameAnalysis]] =
    options.inputs match {
      case single :: Nil if single.toString.endsWith(".csv") =>
        FrameStore.loadSession(single, options.observer).map { case (frames, consolidation) =>
          Console.err.println(s"position  : ${consolidation.describe}")
          frames
        }
      case _                                                 =>
        imageInputs(options).map { inputs =>
          FrameAnalyzer
            .analyzeAll(
              inputs,
              analysisConfig(options),
              (done, total, frame) => Console.err.println(f"[$done%4d/$total%4d] ${frame.name} ${describe(frame)}")
            )
            .frames
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

    /** A pair of numbers given as `x,y` */
    def pair(name: String): Option[(Double, Double)] =
      values.get(name).flatMap { text =>
        text.split(",").map(_.trim).toList match {
          case first :: second :: _ =>
            for {
              parsedFirst  <- Try(first.toDouble).toOption
              parsedSecond <- Try(second.toDouble).toOption
            } yield (parsedFirst, parsedSecond)
          case _                    => None
        }
      }

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
      "blend", "columns", "caption", "quality", "max-pixels", "max-side", "margin", "min-confidence",
      "frames-per-side", "inspect-size",
      "background", "background-margin", "background-brightness", "background-roll",
      "background-sun", "background-scale"
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
