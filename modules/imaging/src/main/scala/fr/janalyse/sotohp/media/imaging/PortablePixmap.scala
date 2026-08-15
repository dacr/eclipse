package fr.janalyse.sotohp.media.imaging

import java.awt.image.{BufferedImage, ComponentColorModel, DataBuffer, DataBufferUShort, Raster, WritableRaster}
import java.awt.color.ColorSpace
import java.awt.Point
import java.io.{BufferedInputStream, DataInputStream, EOFException, FileInputStream, InputStream}
import java.nio.file.Path
import scala.util.{Try, Using}

/** Netpbm images (PPM and PGM), read without any library.
  *
  * Every RAW converter can write this format, and it has one decisive quality : there is nothing to
  * interpret. A magic number, three integers, then the samples, big endian, and that is all.
  *
  * That matters more than it sounds. The TIFF files written by `dcraw_emu` are refused by the TIFF
  * reader bundled with the JVM - `Data segment out of stream` - so the whole decoding chain used to
  * end on an unreadable cache. Reading the converter's native format removes the middleman, and the
  * sixteen bits per channel are kept along the way.
  */
object PortablePixmap {

  val extensions: Set[String] = Set("ppm", "pgm", "pnm")

  def isPortablePixmap(path: Path): Boolean =
    BasicImaging.fileTypeFromName(path).exists(extensions.contains)

  def read(input: Path): BufferedImage =
    Using.resource(DataInputStream(BufferedInputStream(FileInputStream(input.toFile), 1 << 20))) { stream =>
      val magic = readToken(stream)
      val channels = magic match {
        case "P6" => 3
        case "P5" => 1
        case other => throw RuntimeException(s"unsupported netpbm format $other in $input, only binary P5 and P6 are read")
      }
      val width    = readToken(stream).toInt
      val height   = readToken(stream).toInt
      val maximum  = readToken(stream).toInt
      if (width <= 0 || height <= 0) throw RuntimeException(s"meaningless dimensions ${width}x$height in $input")
      if (maximum <= 0 || maximum > 65535) throw RuntimeException(s"unsupported maximum sample value $maximum in $input")

      if (maximum <= 255) readEightBits(stream, width, height, channels)
      else readSixteenBits(stream, width, height, channels)
    }

  private def readEightBits(stream: DataInputStream, width: Int, height: Int, channels: Int): BufferedImage = {
    val image = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val row   = Array.ofDim[Byte](width * channels)
    val pixels = Array.ofDim[Int](width)
    var y     = 0
    while (y < height) {
      stream.readFully(row)
      var x = 0
      while (x < width) {
        val base  = x * channels
        val red   = row(base) & 0xff
        val green = if (channels == 3) row(base + 1) & 0xff else red
        val blue  = if (channels == 3) row(base + 2) & 0xff else red
        pixels(x) = (red << 16) | (green << 8) | blue
        x += 1
      }
      image.setRGB(0, y, width, 1, pixels, 0, width)
      y += 1
    }
    image
  }

  /** Sixteen bits per channel are kept as they are : the whole dynamic range of the sensor is
    * exactly what a corona needs, and every measurement here works on normalized values anyway.
    */
  private def readSixteenBits(stream: DataInputStream, width: Int, height: Int, channels: Int): BufferedImage = {
    val samples = Array.ofDim[Short](width * height * 3)
    val row     = Array.ofDim[Byte](width * channels * 2)
    var y       = 0
    while (y < height) {
      stream.readFully(row)
      var x = 0
      while (x < width) {
        val base  = x * channels * 2
        val red   = ((row(base) & 0xff) << 8) | (row(base + 1) & 0xff)
        val green = if (channels == 3) ((row(base + 2) & 0xff) << 8) | (row(base + 3) & 0xff) else red
        val blue  = if (channels == 3) ((row(base + 4) & 0xff) << 8) | (row(base + 5) & 0xff) else red
        val index = (y * width + x) * 3
        samples(index) = red.toShort
        samples(index + 1) = green.toShort
        samples(index + 2) = blue.toShort
        x += 1
      }
      y += 1
    }
    val buffer      = DataBufferUShort(samples, samples.length)
    val raster      = Raster.createInterleavedRaster(buffer, width, height, width * 3, 3, Array(0, 1, 2), Point(0, 0))
    val colorModel  = ComponentColorModel(
      ColorSpace.getInstance(ColorSpace.CS_sRGB),
      false,
      false,
      java.awt.Transparency.OPAQUE,
      DataBuffer.TYPE_USHORT
    )
    BufferedImage(colorModel, raster.asInstanceOf[WritableRaster], false, null)
  }

  /** Reads one whitespace separated token, skipping the `#` comments netpbm allows */
  private def readToken(stream: InputStream): String = {
    val token   = StringBuilder()
    var current = stream.read()
    var done    = false
    while (!done) {
      if (current == -1) throw EOFException("truncated netpbm header")
      else if (current == '#') {
        while (current != '\n' && current != -1) current = stream.read()
      } else if (current == ' ' || current == '\t' || current == '\n' || current == '\r') {
        if (token.nonEmpty) done = true else current = stream.read()
      } else {
        token.append(current.toChar)
        current = stream.read()
      }
    }
    token.result()
  }
}
