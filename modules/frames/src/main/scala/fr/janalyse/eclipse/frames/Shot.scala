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
  def pixelSource(preferRaw: Boolean): Path = pixelSources(preferRaw).head

  /** Every file the pixels could be taken from, the preferred one first.
    *
    * A shot written as RAW+JPEG has a spare : if the RAW cannot be decoded, or if what the decoder
    * wrote cannot be read back, the JPEG the camera put beside it holds the very same photograph.
    * Giving up on a frame while another copy of it sits there would be absurd.
    */
  def pixelSources(preferRaw: Boolean): List[Path] = {
    val ordered = if (preferRaw) rawFiles ++ renderedFiles else renderedFiles ++ rawFiles
    if (ordered.isEmpty) files else ordered
  }

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

  /** The whole shot a single file belongs to, its siblings looked up beside it.
    *
    * Naming one half of a RAW+JPEG pair is meant to bring the other half along : whichever of
    * `IMG_1234.CR3` and `IMG_1234.JPG` is given, the pixels come from the RAW and the metadata from
    * both. Falls back to the file alone when the directory cannot be read.
    */
  def around(path: Path): Shot = {
    val siblings = Option(path.getParent).map(_.toFile).flatMap(directory => Option(directory.listFiles())).toList.flatten
    val same     = siblings.map(_.toPath).filter(candidate => keyOf(candidate) == keyOf(path))
    group(if (same.isEmpty) List(path) else same).headOption.getOrElse(of(path))
  }

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
