package fr.janalyse.eclipse.frames

import fr.janalyse.eclipse.model.*

import java.time.{LocalDateTime, ZoneOffset}

class AnchorTest extends munit.FunSuite {

  val config = FrameAnalysisConfig()

  /** A session where the filter is removed in the middle : the exposure drops by some 20 stops */
  def session(frameCount: Int = 200, totalityFrom: Int = 95, totalityTo: Int = 105): List[ShotMetadata] = {
    val start = LocalDateTime.of(2026, 8, 12, 18, 0, 0)
    (0 until frameCount).toList.map { index =>
      val filtered = index < totalityFrom || index > totalityTo
      ShotMetadata.empty.copy(
        shotAt = Some(start.plusSeconds(30L * index).atOffset(ZoneOffset.UTC)),
        aperture = Some(8d),
        // the filter does not make the settings extreme, it makes them ordinary : 1/125 at 100 ISO
        // on the filtered sun, while the corona bare lens needs 1/60 at 400 ISO
        exposureTimeSeconds = Some(if (filtered) 1d / 125d else 1d / 60d),
        isoSensitivity = Some(if (filtered) 100d else 400d)
      )
    }
  }

  test("the exposure alone tells the unfiltered frames apart") {
    val metadata = session()
    val filtered = metadata.head.exposureValue.getOrElse(fail("no exposure value"))
    val bare     = metadata(100).exposureValue.getOrElse(fail("no exposure value"))
    // the unfiltered frames sit below the others, by a few stops only
    assert(filtered - bare > 2.5d, f"only ${filtered - bare}%.1f stops between the two exposures")
    assert(filtered - bare < 10d, f"${filtered - bare}%.1f stops apart, that is not what a session looks like")
  }

  test("the analysis starts from a frame taken during totality") {
    val anchor = FrameAnalyzer.anchorOf(session(), config)
    assert(anchor >= 95 && anchor <= 105, s"anchor $anchor is not one of the totality frames")
    // and it is taken well inside totality, not at one of its edges where a diamond ring sits
    assert(anchor > 95 && anchor < 105, s"anchor $anchor sits at the very edge of totality")
  }

  test("the anchor follows the totality, wherever it happens in the session") {
    val early = FrameAnalyzer.anchorOf(session(totalityFrom = 20, totalityTo = 26), config)
    assert(early >= 20 && early <= 26, s"anchor $early is not one of the totality frames")
    val late  = FrameAnalyzer.anchorOf(session(totalityFrom = 170, totalityTo = 178), config)
    assert(late >= 170 && late <= 178, s"anchor $late is not one of the totality frames")
  }

  test("without totality the analysis starts from the middle of the session") {
    val metadata = session(frameCount = 40, totalityFrom = 999, totalityTo = 999)
    assertEquals(FrameAnalyzer.anchorOf(metadata, config), 20)
  }

  test("without any exposure metadata the analysis still starts from the middle") {
    val metadata = List.fill(51)(ShotMetadata.empty)
    assertEquals(FrameAnalyzer.anchorOf(metadata, config), 25)
  }

  test("the exposure driven detection can be turned off") {
    val metadata = session()
    assertEquals(FrameAnalyzer.anchorOf(metadata, config.copy(phaseFromExposure = false)), 100)
  }

  test("the plate scale is known from the optics before any image is looked at") {
    // 200mm on a 45Mpix full frame body : 4.39 µm pixels, so about 795 px per degree
    val metadata = ShotMetadata.empty.copy(
      focalLengthMillimeters = Some(200d),
      pixelPitchMicrometers = Some(4.39d)
    )
    val scale = metadata.opticalPlateScale.getOrElse(fail("no optical plate scale"))
    assertEqualsDouble(scale.pixelsPerDegree, 795d, 5d)
    // which is a 420 px solar disc, the expected size to look for
    assertEqualsDouble(scale.pixelsFor(0.53d), 421d, 5d)
  }
}
