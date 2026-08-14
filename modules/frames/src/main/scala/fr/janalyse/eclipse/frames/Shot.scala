package fr.janalyse.eclipse.frames

import fr.janalyse.sotohp.media.imaging.{BasicImaging, RawDecoder}

import java.nio.file.Path

/** One shot, with every file that represents it.
  *
  * A camera set to RAW+JPEG writes two files per shot, `IMG_1234.CR3` and `IMG_1234.JPG`. They are
  * the same photograph and must be treated as one : counted once, measured once, drawn once.
  *
  * They are not interchangeable though :
  *   - the metadata is complete in the JPEG, while a recent RAW container often hides its GPS
  *     position and even its shooting date from most readers, so both are read and merged ;
  *   - the pixels are better in the RAW, so that is what gets measured and drawn - unless no RAW
  *     converter is installed, in which case the JPEG does the job perfectly well.
  */
final case class Shot(key: String, name: String, files: List[Path]) {

  def rawFiles: List[Path] = files.filter(RawDecoder.isRawFile)

  def renderedFiles: List[Path] = files.filterNot(RawDecoder.isRawFile)

  def hasBoth: Boolean = rawFiles.nonEmpty && renderedFiles.nonEmpty

  /** The file to measure and to draw from */
  def pixelSource(preferRaw: Boolean): Path =
    if (preferRaw) rawFiles.headOption.orElse(renderedFiles.headOption).getOrElse(files.head)
    else renderedFiles.headOption.orElse(rawFiles.headOption).getOrElse(files.head)

  /** The files to read the metadata from, the most talkative first : whatever the pixels come from,
    * a JPEG written by the camera is the most reliable source of date and position.
    */
  def metadataSources: List[Path] = renderedFiles ++ rawFiles
}

object Shot {

  /** Groups files into shots : same directory, same name, whatever the extension and its case */
  def group(paths: Seq[Path]): List[Shot] = {
    paths
      .groupBy(keyOf)
      .map { case (key, grouped) =>
        val ordered = grouped.sortBy(_.getFileName.toString).toList
        Shot(key = key, name = baseNameOf(ordered.head), files = ordered)
      }
      .toList
      .sortBy(_.key)
  }

  /** A shot made of a single file, for the times when there is nothing to group */
  def of(path: Path): Shot = Shot(keyOf(path), baseNameOf(path), List(path))

  private def keyOf(path: Path): String = {
    val directory = Option(path.getParent).map(_.toString).getOrElse("")
    s"$directory/${baseNameOf(path).toLowerCase}"
  }

  private def baseNameOf(path: Path): String = {
    val name = path.getFileName.toString
    BasicImaging.fileTypeFromName(name) match {
      case Some(extension) => name.dropRight(extension.length + 1)
      case None            => name
    }
  }
}
