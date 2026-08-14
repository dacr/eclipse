package fr.janalyse.eclipse.composer

import fr.janalyse.eclipse.astro.SolarEphemeris
import fr.janalyse.eclipse.frames.{FrameAnalysisConfig, FrameAnalyzer}
import fr.janalyse.eclipse.model.*
import fr.janalyse.sotohp.media.imaging.BasicImaging

import java.awt.image.BufferedImage
import java.awt.{Color, RenderingHints}
import java.nio.file.{Files, Path, Paths}
import java.time.{Duration, LocalDateTime, ZoneOffset}
import scala.util.Random

/** Writes a synthetic eclipse session to a directory : the frames themselves, and the measurements
  * file that goes with them. Handy to try layouts and renderings without hauling a hundred RAW
  * files around, and to check the command line from end to end.
  *
  * `sbt "composer/Test/runMain fr.janalyse.eclipse.composer.SampleSessionGenerator /tmp/session"`
  */
object SampleSessionGenerator {

  val observer    = GeoPoint(43.6047d, 1.4442d, 150d)
  val frameWidth  = 1600
  val frameHeight = 1000
  val sunRadius   = 90d

  def main(args: Array[String]): Unit = {
    val directory  = Paths.get(args.headOption.getOrElse("/tmp/eclipse-sample-session"))
    val frameCount = args.lift(1).map(_.toInt).getOrElse(120)
    Files.createDirectories(directory)

    val random = Random(1)
    val start  = LocalDateTime.of(2026, 8, 12, 16, 30, 0).toInstant(ZoneOffset.UTC)
    val paths  = (0 until frameCount).map { index =>
      val ratio       = index.toDouble / (frameCount - 1)
      val obscuration = math.min(0.97d, 0.97d * (1d - math.abs(ratio - 0.5d) * 2d) * 1.4d)
      write(directory, index, obscuration, random)
    }

    val config = FrameAnalysisConfig(
      cacheDirectory = directory.resolve("cache"),
      observer = Some(observer),
      parallelism = 4
    )
    val frames = paths.zipWithIndex.toList.map { case (path, index) =>
      val instant  = start.plus(Duration.ofSeconds(30L * index))
      val measured = FrameAnalyzer.analyze(path, config)
      measured.copy(
        metadata = measured.metadata.copy(shotAt = Some(instant.atOffset(ZoneOffset.UTC)), location = Some(observer)),
        sun = Some(SolarEphemeris.position(instant, observer))
      )
    }

    val measurements = directory.resolve("measurements.csv")
    FrameStore.save(measurements, frames)
    println(EclipseComposer.summary(frames))
    println()
    println(s"$frameCount frames and their measurements written to $directory")
  }

  private def write(directory: Path, index: Int, obscuration: Double, random: Random): Path = {
    val centerX  = 320d + index * 4.2d + (if (index > 70) -260d else 0d) + random.nextDouble() * 3d
    val centerY  = 300d + index * 2.1d + random.nextDouble() * 3d
    val image    = BufferedImage(frameWidth, frameHeight, BufferedImage.TYPE_INT_RGB)
    val graphics = image.createGraphics
    try {
      graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      graphics.setColor(Color(3, 2, 5))
      graphics.fillRect(0, 0, frameWidth, frameHeight)
      graphics.setColor(Color(255, 158, 88)) // the color cast a very dense solar filter gives
      graphics.fillOval((centerX - sunRadius).toInt, (centerY - sunRadius).toInt, (sunRadius * 2).toInt, (sunRadius * 2).toInt)
      val separation = (1d - obscuration) * 2d * sunRadius
      graphics.setColor(Color(4, 3, 6))
      graphics.fillOval(
        (centerX - separation * 0.85d - sunRadius).toInt,
        (centerY - separation * 0.53d - sunRadius).toInt,
        (sunRadius * 2).toInt,
        (sunRadius * 2).toInt
      )
    } finally graphics.dispose()
    val path = directory.resolve(f"IMG_$index%04d.PNG")
    BasicImaging.save(path, image)
    path
  }
}
