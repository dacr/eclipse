package fr.janalyse.sotohp.media.imaging

import java.io.{DataOutputStream, FileOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class PortablePixmapTest extends munit.FunSuite {

  /** Writes a binary netpbm file the way a RAW converter does */
  def write(path: Path, magic: String, width: Int, height: Int, maximum: Int, samples: Seq[Int], comment: Boolean = false): Path = {
    val stream = DataOutputStream(FileOutputStream(path.toFile))
    try {
      val header = if (comment) s"$magic\n# made by dcraw_emu\n$width $height\n$maximum\n" else s"$magic\n$width $height\n$maximum\n"
      stream.write(header.getBytes(StandardCharsets.US_ASCII))
      samples.foreach { value =>
        if (maximum <= 255) stream.writeByte(value)
        else { stream.writeByte((value >> 8) & 0xff); stream.writeByte(value & 0xff) } // big endian, as netpbm says
      }
      path
    } finally stream.close()
  }

  test("an eight bits colour pixmap is read back as it was written") {
    val file = Files.createTempFile("pixmap-", ".ppm")
    try {
      // two pixels wide, one high : pure red then mid grey
      write(file, "P6", 2, 1, 255, Seq(255, 0, 0, 128, 128, 128))
      val image = PortablePixmap.read(file)
      assertEquals(image.getWidth, 2)
      assertEquals(image.getHeight, 1)
      assertEquals(image.getRGB(0, 0) & 0xffffff, 0xff0000)
      assertEquals(image.getRGB(1, 0) & 0xffffff, 0x808080)
    } finally Files.deleteIfExists(file)
  }

  test("a sixteen bits pixmap keeps its whole dynamic range") {
    val file = Files.createTempFile("pixmap-", ".ppm")
    try {
      write(file, "P6", 3, 1, 65535, Seq(65535, 0, 0, 0, 32768, 0, 1000, 1000, 1000))
      val image  = PortablePixmap.read(file)
      val raster = Rasters.RgbRaster.fromImage(image)
      assertEquals(image.getColorModel.getComponentSize(0), 16)
      assertEqualsFloat(raster.red(0), 1f, 0.001f)
      assertEqualsFloat(raster.green(1), 0.5f, 0.001f)
      // a level of 1000 out of 65535 would be lost in the rounding of an eight bits reading
      assertEqualsFloat(raster.red(2), 1000f / 65535f, 0.0001f)
      assert(raster.red(2) > 0f, "a faint corona must not be rounded away")
    } finally Files.deleteIfExists(file)
  }

  test("a grey pixmap is read as a neutral colour image") {
    val file = Files.createTempFile("pixmap-", ".pgm")
    try {
      write(file, "P5", 2, 2, 255, Seq(0, 64, 128, 255))
      val image = PortablePixmap.read(file)
      assertEquals(image.getWidth, 2)
      assertEquals(image.getHeight, 2)
      assertEquals(image.getRGB(1, 1) & 0xffffff, 0xffffff)
      assertEquals(image.getRGB(0, 0) & 0xffffff, 0x000000)
    } finally Files.deleteIfExists(file)
  }

  test("comments in the header are ignored, as the format allows them anywhere") {
    val file = Files.createTempFile("pixmap-", ".ppm")
    try {
      write(file, "P6", 1, 1, 255, Seq(10, 20, 30), comment = true)
      val image = PortablePixmap.read(file)
      assertEquals(image.getRGB(0, 0) & 0xffffff, 0x0a141e)
    } finally Files.deleteIfExists(file)
  }

  test("the loader recognizes netpbm files by their extension") {
    val file = Files.createTempFile("pixmap-", ".ppm")
    try {
      write(file, "P6", 4, 4, 255, Seq.fill(4 * 4 * 3)(200))
      // this is the path the whole pipeline takes, and the one the JVM TIFF reader could not follow
      val image = BasicImaging.load(file)
      assertEquals(image.getWidth, 4)
      assertEquals(image.getHeight, 4)
      assert(PortablePixmap.isPortablePixmap(file))
      assert(!PortablePixmap.isPortablePixmap(java.nio.file.Paths.get("IMG_0001.CR3")))
    } finally Files.deleteIfExists(file)
  }

  test("a truncated file is reported rather than silently half read") {
    val file = Files.createTempFile("pixmap-", ".ppm")
    try {
      val stream = DataOutputStream(FileOutputStream(file.toFile))
      try {
        stream.write("P6\n10 10\n255\n".getBytes(StandardCharsets.US_ASCII))
        stream.write(Array.fill[Byte](30)(0)) // one row instead of ten
      } finally stream.close()
      intercept[java.io.EOFException](PortablePixmap.read(file))
    } finally Files.deleteIfExists(file)
  }
}
