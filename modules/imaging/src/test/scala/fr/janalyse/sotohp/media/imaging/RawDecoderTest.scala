package fr.janalyse.sotohp.media.imaging

import java.nio.file.Paths

class RawDecoderTest extends munit.FunSuite {

  test("RAW files are recognized whatever the case of their extension") {
    val names = List(
      "IMG_0001.CR3",
      "IMG_0001.cr3",
      "IMG_0001.Cr3",
      "IMG_0001.cR3",
      "DSC_0001.NEF",
      "P1000001.rw2",
      "IMG.0001.CR3", // dots in the name itself
      "a.very.long.name.ARW"
    )
    names.foreach { name =>
      assert(RawDecoder.isRawFile(Paths.get(name)), s"$name should be seen as a RAW file")
    }
  }

  test("non RAW files are left alone, whatever the case of their extension") {
    val names = List("photo.JPG", "photo.jpeg", "photo.PNG", "photo.tif", "noextension", "archive.CR3.zip")
    names.foreach { name =>
      assert(!RawDecoder.isRawFile(Paths.get(name)), s"$name should not be seen as a RAW file")
    }
  }

  test("the file type is always reported in lower case") {
    assertEquals(BasicImaging.fileTypeFromName("IMG_0001.CR3"), Some("cr3"))
    assertEquals(BasicImaging.fileTypeFromName(Paths.get("/photos/IMG_0001.JPG")), Some("jpg"))
    assertEquals(BasicImaging.fileTypeFromName("noextension"), None)
    assertEquals(BasicImaging.fileTypeFromName("trailingdot."), None)
  }

  test("the decoding cache key does not depend on the case of the extension") {
    val upperCase = RawDecoder.RawDecodeConfig()
    assertEquals(upperCase.signature, RawDecoder.RawDecodeConfig().signature)
    assert(RawDecoder.RawDecodeConfig(halfSize = true).signature != upperCase.signature)
  }

  test("each converter writes the format it does best, and that reads back") {
    // the TIFF written by dcraw_emu is refused by the TIFF reader of the JVM, its netpbm is not
    assertEquals(RawDecoder.RawTool.LibRaw.nativeOutputExtension, "ppm")
    assert(PortablePixmap.extensions.contains(RawDecoder.RawTool.LibRaw.nativeOutputExtension))
    // no converter is asked for TIFF any more, that is the format the JVM reader stumbles on
    assertEquals(RawDecoder.RawTool.Darktable.nativeOutputExtension, "png")
    assertEquals(RawDecoder.RawTool.RawTherapee.nativeOutputExtension, "png")
    assertEquals(RawDecoder.RawTool.ImageMagick.nativeOutputExtension, "png")
    assert(!RawDecoder.RawTool.values.exists(_.nativeOutputExtension.startsWith("tif")))
  }

  test("asking for another output format changes the cache key") {
    val native = RawDecoder.RawDecodeConfig()
    val tiff   = RawDecoder.RawDecodeConfig(outputExtension = Some("tiff"))
    assert(native.signature != tiff.signature, "both would otherwise share the same cached files")
  }

  test("a missing converter is reported as such rather than crashing") {
    val result = RawDecoder.decode(
      Paths.get("/nowhere/IMG_0001.CR3"),
      Paths.get("/tmp/eclipse-cache-test"),
      RawDecoder.RawDecodeConfig(preferredTools = Nil)
    )
    assert(result.isLeft, result.toString)
    assert(result.left.exists(_.contains("no RAW converter")), result.toString)
  }
}
