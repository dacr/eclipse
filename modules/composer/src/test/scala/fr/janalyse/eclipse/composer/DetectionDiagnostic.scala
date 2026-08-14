package fr.janalyse.eclipse.composer

import fr.janalyse.sotohp.media.imaging.CircleFitting.Circle
import fr.janalyse.sotohp.media.imaging.{BasicImaging, DiscDetector}

import java.awt.image.BufferedImage
import java.awt.{Color, RenderingHints}

object DetectionDiagnostic {
  def main(args: Array[String]): Unit = {
    val sunRadius = 70d
    List(0d, 0.1d, 0.23d, 0.5d, 0.75d, 0.85d).foreach { obscuration =>
      val centerX  = 322.5d
      val centerY  = 270d
      val image    = BufferedImage(1200, 800, BufferedImage.TYPE_INT_RGB)
      val graphics = image.createGraphics
      graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      graphics.setColor(Color(3, 2, 5))
      graphics.fillRect(0, 0, 1200, 800)
      graphics.setColor(Color(255, 160, 90))
      graphics.fillOval((centerX - sunRadius).toInt, (centerY - sunRadius).toInt, (sunRadius * 2).toInt, (sunRadius * 2).toInt)
      val separation = (1d - obscuration) * 2d * sunRadius
      graphics.setColor(Color(4, 3, 6))
      graphics.fillOval(
        (centerX - separation * 0.8d - sunRadius).toInt,
        (centerY - separation * 0.6d - sunRadius).toInt,
        (sunRadius * 2).toInt,
        (sunRadius * 2).toInt
      )
      graphics.dispose()

      val free       = DiscDetector.detect(image)
      val constrained = DiscDetector.detect(image, DiscDetector.DiscDetectorConfig(expectedRadiusPixels = Some(sunRadius)))
      println(f"obscuration=$obscuration%.2f")
      println(s"  free        : ${describe(free, centerX, centerY, sunRadius)}")
      println(s"  constrained : ${describe(constrained, centerX, centerY, sunRadius)}")
    }
  }

  private def describe(result: Either[String, DiscDetector.DiscDetection], x: Double, y: Double, radius: Double): String =
    result match {
      case Left(error)      => s"failed : $error"
      case Right(detection) =>
        val circle = detection.circle
        f"center=(${circle.centerX}%.2f,${circle.centerY}%.2f) r=${circle.radius}%.2f " +
          f"errorX=${circle.centerX - x}%+.2f errorY=${circle.centerY - y}%+.2f errorR=${circle.radius - radius}%+.2f " +
          f"points=${detection.boundaryPointCount} inliers=${detection.inlierCount} rms=${detection.residualRms}%.2f"
    }
}
