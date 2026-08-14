package fr.janalyse.sotohp.media.imaging

import java.awt.image.BufferedImage
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.sys.process.*
import scala.util.{Failure, Success, Try}

/** RAW files decoding through an external converter.
  *
  * Java has no RAW support at all, so this delegates to whichever converter is installed, the same
  * way `BasicImaging` already delegates HEIF decoding to ImageMagick. Results are cached, decoding a
  * few hundred 45Mpix RAW files is by far the most expensive step of any processing session.
  *
  * Whatever the converter, the point is to always use the *same* rendering for every frame of a
  * sequence : fixed white balance, no automatic brightness. An automatic per-file rendering would
  * silently change the photometry from one frame to the next and ruin any comparison.
  */
object RawDecoder {

  val rawFileExtensions: Set[String] =
    Set("cr3", "cr2", "crw", "nef", "nrw", "arw", "srf", "sr2", "raf", "orf", "rw2", "pef", "srw", "dng", "raw", "rwl", "iiq", "3fr")

  enum RawTool(val executable: String) {
    case LibRaw       extends RawTool("dcraw_emu")
    case Darktable    extends RawTool("darktable-cli")
    case RawTherapee  extends RawTool("rawtherapee-cli")
    case ImageMagick  extends RawTool("magick")
  }

  final case class RawDecodeConfig(
    preferredTools: List[RawTool] = List(RawTool.LibRaw, RawTool.Darktable, RawTool.RawTherapee, RawTool.ImageMagick),
    outputExtension: String = "tiff",
    cameraWhiteBalance: Boolean = true,
    automaticBrightness: Boolean = false,
    linearOutput: Boolean = false,
    sixteenBits: Boolean = true,
    halfSize: Boolean = false,
    interpolationQuality: Int = 3,
    extraArguments: List[String] = Nil
  ) {
    def signature: String = {
      val fields = List(
        outputExtension,
        cameraWhiteBalance.toString,
        automaticBrightness.toString,
        linearOutput.toString,
        sixteenBits.toString,
        halfSize.toString,
        interpolationQuality.toString,
        extraArguments.mkString(",")
      )
      Integer.toHexString(fields.mkString("|").hashCode)
    }
  }

  def isRawFile(path: Path): Boolean =
    BasicImaging.fileTypeFromName(path).exists(rawFileExtensions.contains)

  def isAvailable(tool: RawTool): Boolean =
    Try(Process(Seq("which", tool.executable)).!(ProcessLogger(_ => (), _ => ())) == 0).getOrElse(false)

  def availableTools(config: RawDecodeConfig = RawDecodeConfig()): List[RawTool] =
    config.preferredTools.filter(isAvailable)

  /** Decodes a RAW file into an image file usable by `BasicImaging.load`, result is cached */
  def decode(
    input: Path,
    cacheDirectory: Path,
    config: RawDecodeConfig = RawDecodeConfig()
  ): Either[String, Path] = {
    availableTools(config).headOption match {
      case None       =>
        Left(
          s"no RAW converter found, install one of : ${config.preferredTools.map(_.executable).mkString(", ")}"
        )
      case Some(tool) =>
        val baseName = input.getFileName.toString.replaceAll("""\.[^.]+$""", "")
        val key      = Integer.toHexString(s"${input.toAbsolutePath}|${Try(Files.size(input)).getOrElse(0L)}".hashCode)
        val output   = cacheDirectory.resolve(s"$baseName-$key-${config.signature}.${config.outputExtension}")
        if (Files.exists(output) && Try(Files.size(output)).getOrElse(0L) > 0L) Right(output)
        else {
          Files.createDirectories(cacheDirectory)
          runTool(tool, input, output, config)
        }
    }
  }

  /** Loads any image, RAW or not, decoding and caching on the fly when needed */
  def load(
    input: Path,
    cacheDirectory: Path,
    config: RawDecodeConfig = RawDecodeConfig()
  ): Either[String, BufferedImage] =
    if (!isRawFile(input)) Try(BasicImaging.load(input)).toEither.left.map(_.getMessage)
    else decode(input, cacheDirectory, config).flatMap(path => Try(BasicImaging.load(path)).toEither.left.map(_.getMessage))

  private def runTool(tool: RawTool, input: Path, output: Path, config: RawDecodeConfig): Either[String, Path] = {
    val logger = ProcessLogger(_ => (), _ => ())
    tool match {
      case RawTool.LibRaw =>
        // dcraw_emu writes its result next to the file it is given, a symbolic link inside a
        // temporary directory keeps the source directory untouched.
        val workDirectory = Files.createTempDirectory("sotohp-raw-")
        val linked        = workDirectory.resolve(input.getFileName.toString)
        val produced      = workDirectory.resolve(s"${input.getFileName.toString}.${if (config.outputExtension == "tiff") "tiff" else "ppm"}")
        val arguments     = List.newBuilder[String]
        arguments += tool.executable
        if (config.outputExtension == "tiff") arguments += "-T"
        if (config.sixteenBits) arguments += (if (config.linearOutput) "-4" else "-6")
        if (config.cameraWhiteBalance) arguments += "-w"
        if (!config.automaticBrightness) arguments += "-W"
        if (config.halfSize) arguments += "-h"
        arguments ++= List("-q", config.interpolationQuality.toString)
        arguments ++= config.extraArguments
        arguments += linked.toString
        try {
          Files.createSymbolicLink(linked, input.toAbsolutePath)
          val exitCode = Process(arguments.result()).!(logger)
          if (exitCode != 0) Left(s"${tool.executable} failed with exit code $exitCode on $input")
          else if (!Files.exists(produced)) Left(s"${tool.executable} produced no output for $input")
          else {
            Files.move(produced, output, StandardCopyOption.REPLACE_EXISTING)
            Right(output)
          }
        } catch {
          case error: Exception => Left(s"${tool.executable} failed on $input : ${error.getMessage}")
        } finally {
          Try(Files.deleteIfExists(linked))
          Try(Files.deleteIfExists(produced))
          Try(Files.deleteIfExists(workDirectory))
        }

      case RawTool.Darktable =>
        val arguments = List(tool.executable, input.toString, output.toString, "--core", "--disable-opencl") ++ config.extraArguments
        execute(arguments, output, tool, input, logger)

      case RawTool.RawTherapee =>
        val arguments = List(tool.executable, "-o", output.toString, "-t", "-Y", "-c", input.toString) ++ config.extraArguments
        execute(arguments, output, tool, input, logger)

      case RawTool.ImageMagick =>
        val arguments = List(tool.executable, input.toString) ++ config.extraArguments ++ List(output.toString)
        execute(arguments, output, tool, input, logger)
    }
  }

  private def execute(arguments: List[String], output: Path, tool: RawTool, input: Path, logger: ProcessLogger): Either[String, Path] =
    Try(Process(arguments).!(logger)) match {
      case Failure(error)                       => Left(s"${tool.executable} failed on $input : ${error.getMessage}")
      case Success(exitCode) if exitCode != 0    => Left(s"${tool.executable} failed with exit code $exitCode on $input")
      case Success(_) if !Files.exists(output)   => Left(s"${tool.executable} produced no output for $input")
      case Success(_)                            => Right(output)
    }
}
