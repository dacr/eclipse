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

  /** @param nativeOutputExtension the format the tool writes best, and which reads back reliably :
    *                              `dcraw_emu` does write TIFF files, but ones the JVM TIFF reader
    *                              refuses with `Data segment out of stream`, while its netpbm output
    *                              is trivial to read and carries the same sixteen bits
    */
  enum RawTool(val executable: String, val nativeOutputExtension: String) {
    case LibRaw      extends RawTool("dcraw_emu", "ppm")
    case Darktable   extends RawTool("darktable-cli", "png")
    case RawTherapee extends RawTool("rawtherapee-cli", "png")
    case ImageMagick extends RawTool("magick", "png")
  }

  final case class RawDecodeConfig(
    preferredTools: List[RawTool] = List(RawTool.LibRaw, RawTool.Darktable, RawTool.RawTherapee, RawTool.ImageMagick),
    /** left to the tool when not given */
    outputExtension: Option[String] = None,
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
        outputExtension.getOrElse("native"),
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
        val baseName  = input.getFileName.toString.replaceAll("""\.[^.]+$""", "")
        val key       = Integer.toHexString(s"${input.toAbsolutePath}|${Try(Files.size(input)).getOrElse(0L)}".hashCode)
        val extension = config.outputExtension.getOrElse(tool.nativeOutputExtension)
        val output    = cacheDirectory.resolve(s"$baseName-$key-${config.signature}.$extension")
        if (isReadyInCache(output)) Right(output)
        else {
          Files.createDirectories(cacheDirectory)
          // decoding the very same file twice at once is pure waste : whoever gets there first does
          // the work, the others wait for it and pick up the result
          lockFor(output).synchronized {
            if (isReadyInCache(output)) Right(output)
            else runTool(tool, input, output, config)
          }
        }
    }
  }

  private def isReadyInCache(output: Path): Boolean =
    Files.exists(output) && Try(Files.size(output)).getOrElse(0L) > 0L

  private val decodingLocks = scala.collection.mutable.Map.empty[String, AnyRef]

  private def lockFor(output: Path): AnyRef =
    decodingLocks.synchronized(decodingLocks.getOrElseUpdate(output.toString, new Object()))

  /** Loads any image, RAW or not, decoding and caching on the fly when needed */
  def load(
    input: Path,
    cacheDirectory: Path,
    config: RawDecodeConfig = RawDecodeConfig()
  ): Either[String, BufferedImage] =
    if (!isRawFile(input)) Try(BasicImaging.load(input)).toEither.left.map(_.getMessage)
    else decode(input, cacheDirectory, config).flatMap(path => Try(BasicImaging.load(path)).toEither.left.map(_.getMessage))

  /** Runs the converter in a working directory of its own, then publishes the result in one go.
    *
    * The work never happens at the final place : a decoding takes seconds and writes hundreds of
    * megabytes, and something else - the frame being measured right now, a prefetch running ahead -
    * may well be reading the cache meanwhile. The file only appears once complete, by a rename
    * within the same directory, which the file system guarantees to be atomic.
    */
  private def runTool(tool: RawTool, input: Path, output: Path, config: RawDecodeConfig): Either[String, Path] = {
    val logger        = ProcessLogger(_ => (), _ => ())
    val workDirectory = Files.createTempDirectory(output.getParent, "decoding-")
    val produced      = workDirectory.resolve(s"decoded.${config.outputExtension.getOrElse(tool.nativeOutputExtension)}")

    val outcome =
      try {
        tool match {
          case RawTool.LibRaw =>
            // dcraw_emu writes its result next to the file it is given, so it is given a symbolic
            // link inside the working directory, which keeps the photo directory untouched
            val linked    = workDirectory.resolve(input.getFileName.toString)
            val arguments = List.newBuilder[String]
            arguments += tool.executable
            if (config.outputExtension.contains("tiff") || config.outputExtension.contains("tif")) arguments += "-T"
            if (config.sixteenBits) arguments += (if (config.linearOutput) "-4" else "-6")
            if (config.cameraWhiteBalance) arguments += "-w"
            if (!config.automaticBrightness) arguments += "-W"
            if (config.halfSize) arguments += "-h"
            arguments ++= List("-q", config.interpolationQuality.toString)
            arguments ++= config.extraArguments
            arguments += linked.toString

            Files.createSymbolicLink(linked, input.toAbsolutePath)
            val exitCode = Process(arguments.result()).!(logger)
            // the output name is not guessed : depending on the version, dcraw_emu either replaces
            // or appends the extension, and the case of the original one is not always kept.
            // Whatever it did, the only file it could have created is the one that was not there.
            if (exitCode != 0) Left(s"${tool.executable} failed with exit code $exitCode on $input")
            else
              producedFileIn(workDirectory, linked) match {
                case None       => Left(s"${tool.executable} produced no output for $input")
                case Some(file) => Right(file)
              }

          case RawTool.Darktable =>
            execute(
              List(tool.executable, input.toString, produced.toString, "--core", "--disable-opencl") ++ config.extraArguments,
              produced,
              tool,
              input,
              logger
            )

          case RawTool.RawTherapee =>
            // -n asks for PNG, -b16 for sixteen bits per channel : compressed, and read back by
            // anything, where the TIFF path is exactly the one that let us down
            execute(
              List(tool.executable, "-o", produced.toString, "-n", if (config.sixteenBits) "-b16" else "-b8", "-Y", "-c", input.toString) ++ config.extraArguments,
              produced,
              tool,
              input,
              logger
            )

          case RawTool.ImageMagick =>
            execute(
              List(tool.executable, input.toString) ++ config.extraArguments ++ List(produced.toString),
              produced,
              tool,
              input,
              logger
            )
        }
      } catch {
        case error: Exception => Left(s"${tool.executable} failed on $input : ${error.getMessage}")
      }

    val published = outcome.flatMap { file =>
      Try(Files.move(file, output, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)).toEither
        .map(_ => output)
        .left
        .map(error => s"unable to publish the decoding of $input : ${error.getMessage}")
    }
    cleanUp(workDirectory)
    published
  }

  private def cleanUp(directory: Path): Unit = {
    val stream = Try(Files.list(directory))
    stream.foreach { found =>
      try found.iterator().forEachRemaining(path => Try(Files.deleteIfExists(path)).getOrElse(false))
      finally found.close()
    }
    Try(Files.deleteIfExists(directory))
  }

  /** The file a converter left in its working directory, whatever name it chose for it */
  private def producedFileIn(directory: Path, ignored: Path): Option[Path] = {
    val stream = Files.list(directory)
    try {
      val found = stream
        .filter(path => !path.equals(ignored) && Files.isRegularFile(path))
        .sorted()
        .findFirst()
      if (found.isPresent) Some(found.get) else None
    } finally stream.close()
  }

  private def execute(arguments: List[String], produced: Path, tool: RawTool, input: Path, logger: ProcessLogger): Either[String, Path] =
    Try(Process(arguments).!(logger)) match {
      case Failure(error)                      => Left(s"${tool.executable} failed on $input : ${error.getMessage}")
      case Success(exitCode) if exitCode != 0  => Left(s"${tool.executable} failed with exit code $exitCode on $input")
      case Success(_) if !Files.exists(produced) => Left(s"${tool.executable} produced no output for $input")
      case Success(_)                          => Right(produced)
    }
}
