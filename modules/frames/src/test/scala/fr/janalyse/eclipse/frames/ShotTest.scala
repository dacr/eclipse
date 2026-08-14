package fr.janalyse.eclipse.frames

import fr.janalyse.eclipse.model.*

import java.nio.file.Paths
import java.time.{LocalDateTime, ZoneOffset}

class ShotTest extends munit.FunSuite {

  /** A camera set to RAW+JPEG : two files per shot, same name, different extension */
  def pairs(count: Int, directory: String = "/photos"): List[java.nio.file.Path] =
    (0 until count).toList.flatMap { index =>
      List(Paths.get(f"$directory/IMG_$index%04d.CR3"), Paths.get(f"$directory/IMG_$index%04d.JPG"))
    }

  test("a RAW and its JPEG are one shot, not two") {
    val shots = Shot.group(pairs(120))
    assertEquals(shots.size, 120)
    assert(shots.forall(_.hasBoth), "every shot should have both of its files")
    assert(shots.forall(_.files.sizeIs == 2))
  }

  test("the pixels come from the RAW, the metadata from both, the JPEG first") {
    val shot = Shot.group(pairs(1)).head
    assertEquals(shot.pixelSource(preferRaw = true).getFileName.toString, "IMG_0000.CR3")
    assertEquals(shot.pixelSource(preferRaw = false).getFileName.toString, "IMG_0000.JPG")
    // the JPEG is asked first : it is the one that reliably carries the date and the position
    assertEquals(shot.metadataSources.map(_.getFileName.toString), List("IMG_0000.JPG", "IMG_0000.CR3"))
  }

  test("the extension case does not matter for the grouping") {
    val files = List("IMG_0001.CR3", "IMG_0001.jpg", "IMG_0002.cr3", "IMG_0002.JPG").map(name => Paths.get(s"/photos/$name"))
    val shots = Shot.group(files)
    assertEquals(shots.size, 2)
    assert(shots.forall(_.hasBoth))
  }

  test("shots sharing a name in different directories stay apart") {
    val shots = Shot.group(pairs(2, "/photos/morning") ++ pairs(2, "/photos/afternoon"))
    assertEquals(shots.size, 4)
  }

  test("a lonely file is a shot of its own") {
    val shots = Shot.group(List(Paths.get("/photos/IMG_0001.CR3"), Paths.get("/photos/IMG_0002.JPG")))
    assertEquals(shots.size, 2)
    assert(!shots.exists(_.hasBoth))
    assertEquals(shots.head.pixelSource(preferRaw = true).getFileName.toString, "IMG_0001.CR3")
    // no RAW here, so the JPEG is used whatever the preference says
    assertEquals(shots.last.pixelSource(preferRaw = true).getFileName.toString, "IMG_0002.JPG")
  }

  test("shot names ignore the extension, dots in the name included") {
    assertEquals(Shot.of(Paths.get("/photos/IMG_0001.CR3")).name, "IMG_0001")
    assertEquals(Shot.of(Paths.get("/photos/eclipse.2026.08.12.CR3")).name, "eclipse.2026.08.12")
    assertEquals(Shot.of(Paths.get("/photos/noextension")).name, "noextension")
  }

  test("what one file misses, the other provides") {
    // the JPEG knows when and where, the RAW knows the optics : a shot needs all of it
    val fromJpeg = ShotMetadata.empty.copy(
      shotAt = Some(LocalDateTime.of(2026, 8, 12, 18, 30, 0).atOffset(ZoneOffset.UTC)),
      location = Some(GeoPoint(43.6d, 1.44d, 150d))
    )
    val fromRaw  = ShotMetadata.empty.copy(
      focalLengthMillimeters = Some(200d),
      pixelPitchMicrometers = Some(4.39d),
      aperture = Some(8d)
    )
    val merged   = fromJpeg.completedWith(fromRaw)
    assertEquals(merged.shotAt, fromJpeg.shotAt)
    assertEquals(merged.location, fromJpeg.location)
    assertEquals(merged.focalLengthMillimeters, Some(200d))
    assert(merged.opticalPlateScale.isDefined, "the optics of the RAW completed the dates of the JPEG")
  }

  test("the first reading wins on the fields both files carry") {
    val first  = ShotMetadata.empty.copy(isoSensitivity = Some(100d), aperture = Some(8d))
    val second = ShotMetadata.empty.copy(isoSensitivity = Some(400d), exposureTimeSeconds = Some(0.001d))
    val merged = first.completedWith(second)
    assertEquals(merged.isoSensitivity, Some(100d))
    assertEquals(merged.exposureTimeSeconds, Some(0.001d))
  }
}
