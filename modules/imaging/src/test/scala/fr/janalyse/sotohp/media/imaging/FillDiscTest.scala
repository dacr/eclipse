package fr.janalyse.sotohp.media.imaging

import fr.janalyse.sotohp.media.imaging.Compositing.{BlendMode, Canvas}

import java.awt.Color
import java.awt.image.BufferedImage

class FillDiscTest extends munit.FunSuite {

  private def luminanceAt(canvas: Canvas, x: Int, y: Int): Int = canvas.image.getRGB(x, y) & 0xff

  private def skyCanvas(size: Int, level: Int): Canvas = Canvas.create(size, size, Color(level, level, level))

  test("a black disc really is black, whatever was behind it") {
    val canvas = skyCanvas(200, 140)
    canvas.fillDisc(100d, 100d, 30d, Color.BLACK)

    assertEquals(luminanceAt(canvas, 100, 100), 0, "the middle should be black")
    assertEquals(luminanceAt(canvas, 100, 80), 0, "and so should the inside of the edge")
    assertEquals(luminanceAt(canvas, 100, 160), 140, "while the sky outside is untouched")
  }

  test("its edge fades rather than cutting") {
    val canvas = skyCanvas(200, 200)
    canvas.fillDisc(100d, 100d, 40d, Color.BLACK, featherRatio = 0.25d)

    val justInside = luminanceAt(canvas, 100, 100 - 38)
    assert(justInside > 0 && justInside < 200, s"the edge should be part way, it reads $justInside")
    assertEquals(luminanceAt(canvas, 100, 100 - 25), 0, "well inside is fully black")
  }

  test("a moon blended the usual way would let the sky through, which is the whole point") {
    val size  = 120
    val sky   = skyCanvas(size, 150)
    val moon  = BufferedImage(size, size, BufferedImage.TYPE_INT_RGB) // all black, as the moon is

    sky.draw(moon, 0, 0, BlendMode.Lighten)
    assertEquals(luminanceAt(sky, 60, 60), 150, "lighten keeps the brighter of the two, so the sky wins")

    sky.fillDisc(60d, 60d, 40d, Color.BLACK)
    sky.draw(moon, 0, 0, BlendMode.Lighten)
    assertEquals(luminanceAt(sky, 60, 60), 0, "put black first and it stays black")
  }
}
