package fr.janalyse.eclipse.model

/** One position for the whole session.
  *
  * A sequence of an eclipse is shot from a tripod that nobody moves, so every frame shares the same
  * observing position. That single fact is worth exploiting : the frames which carry no GPS fix -
  * the receiver had not locked yet, the camera was switched to another card, the fix was dropped -
  * simply inherit the position of the others, and the position itself is taken as the median of all
  * the fixes, which irons out the usual GPS wandering.
  *
  * The consequence matters : one frame without a fix is no longer a frame that cannot be placed.
  */
object SessionLocation {

  final case class Consolidation(
    location: Option[GeoPoint],
    /** how many frames carried a usable fix */
    fixCount: Int,
    /** how many did not, and were given the session position */
    missingCount: Int,
    /** how far the farthest fix sits from the consolidated position */
    spreadMeters: Double,
    /** true when no frame carried a fix and the given fallback had to be used */
    fromFallback: Boolean
  ) {
    def total: Int = fixCount + missingCount

    /** Fixes scattered over more than a few dozen meters mean the camera moved, or that some fixes
      * are wrong : either way the single position assumption deserves to be questioned.
      */
    def looksMoved(toleranceMeters: Double = 200d): Boolean = spreadMeters > toleranceMeters

    def describe: String =
      location match {
        case None           => s"no position at all, neither in the $total frames nor in the configuration"
        case Some(position) =>
          val origin =
            if (fromFallback) "given in the configuration"
            else f"median of $fixCount fixes, spread ${spreadMeters}%.0f m"
          val filled = if (missingCount == 0) "" else s", $missingCount frames without a fix given that position"
          s"$position ($origin$filled)"
      }
  }

  /** Consolidates the position of a session, the fallback being used only when no frame has a fix */
  def consolidate(metadata: Seq[ShotMetadata], fallback: Option[GeoPoint] = None): Consolidation = {
    val fixes   = metadata.flatMap(_.location)
    val missing = metadata.size - fixes.size
    if (fixes.isEmpty)
      Consolidation(fallback, 0, metadata.size, 0d, fromFallback = fallback.isDefined)
    else {
      val consolidated = GeoPoint(
        latitudeDegrees = median(fixes.map(_.latitudeDegrees)),
        longitudeDegrees = median(fixes.map(_.longitudeDegrees)),
        altitudeMeters = median(fixes.map(_.altitudeMeters))
      )
      Consolidation(
        location = Some(consolidated),
        fixCount = fixes.size,
        missingCount = missing,
        spreadMeters = fixes.map(fix => distanceMeters(fix, consolidated)).maxOption.getOrElse(0d),
        fromFallback = false
      )
    }
  }

  /** Great circle distance between two points on earth, in meters */
  def distanceMeters(from: GeoPoint, to: GeoPoint): Double = {
    val earthRadius = 6371000d
    val fromLatitude = math.toRadians(from.latitudeDegrees)
    val toLatitude   = math.toRadians(to.latitudeDegrees)
    val deltaLatitude  = math.toRadians(to.latitudeDegrees - from.latitudeDegrees)
    val deltaLongitude = math.toRadians(to.longitudeDegrees - from.longitudeDegrees)
    val a =
      math.pow(math.sin(deltaLatitude / 2d), 2d) +
        math.cos(fromLatitude) * math.cos(toLatitude) * math.pow(math.sin(deltaLongitude / 2d), 2d)
    2d * earthRadius * math.atan2(math.sqrt(a), math.sqrt(1d - a))
  }

  private def median(values: Seq[Double]): Double = {
    val sorted = values.sorted
    if (sorted.isEmpty) 0d
    else if (sorted.size % 2 == 1) sorted(sorted.size / 2)
    else (sorted(sorted.size / 2 - 1) + sorted(sorted.size / 2)) / 2d
  }
}
