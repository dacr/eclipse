package fr.janalyse.eclipse.astro

import fr.janalyse.eclipse.model.*

import java.time.Instant

/** Apparent position and size of the sun, for a given instant and a given place on earth.
  *
  * Implementation of the low accuracy solar position of Jean Meeus, *Astronomical Algorithms*
  * (chapters 12, 13, 25) : better than 0.01°, which is a fiftieth of the solar diameter. Far more
  * than enough to lay out a composite, since the disc itself is measured on each frame anyway.
  *
  * The result also carries the apparent (refracted) position : this is the direction the light
  * actually came from, hence where the sun really was recorded on the sensor - it matters, close to
  * the horizon refraction lifts the sun by up to its own diameter.
  */
object SolarEphemeris {

  /** TT - UT1 in seconds, roughly 69s during the 2020's, exposed for the picky ones */
  val defaultDeltaTSeconds: Double = 69.5d

  /** Julian day of an instant, from the UTC time scale */
  def julianDay(instant: Instant): Double =
    instant.toEpochMilli / 86400000d + 2440587.5d

  def position(
    instant: Instant,
    observer: GeoPoint,
    conditions: AtmosphericConditions = AtmosphericConditions(),
    deltaTSeconds: Double = defaultDeltaTSeconds
  ): SunPosition = {
    val julianDayUT       = julianDay(instant)
    val julianDayTerrestrial = julianDayUT + deltaTSeconds / 86400d
    val centuries         = (julianDayTerrestrial - 2451545d) / 36525d

    // --- geometric position of the sun on the ecliptic --------------------------------------
    val meanLongitude = normalizeDegrees(280.46646d + 36000.76983d * centuries + 0.0003032d * centuries * centuries)
    val meanAnomaly   = normalizeDegrees(357.52911d + 35999.05029d * centuries - 0.0001537d * centuries * centuries)
    val eccentricity  = 0.016708634d - 0.000042037d * centuries - 0.0000001267d * centuries * centuries
    val meanAnomalyRadians = math.toRadians(meanAnomaly)
    val equationOfCenter   =
      (1.914602d - 0.004817d * centuries - 0.000014d * centuries * centuries) * math.sin(meanAnomalyRadians) +
        (0.019993d - 0.000101d * centuries) * math.sin(2d * meanAnomalyRadians) +
        0.000289d * math.sin(3d * meanAnomalyRadians)
    val trueLongitude = meanLongitude + equationOfCenter
    val trueAnomaly   = math.toRadians(meanAnomaly + equationOfCenter)
    val radiusVector  = 1.000001018d * (1d - eccentricity * eccentricity) / (1d + eccentricity * math.cos(trueAnomaly))

    // --- apparent position : nutation in longitude and aberration ---------------------------
    val ascendingNode      = math.toRadians(125.04d - 1934.136d * centuries)
    val apparentLongitude  = trueLongitude - 0.00569d - 0.00478d * math.sin(ascendingNode)
    val meanObliquity      =
      23.439291111d - 0.0130041667d * centuries - 1.6389e-7d * centuries * centuries + 5.036e-7d * centuries * centuries * centuries
    val obliquity          = math.toRadians(meanObliquity + 0.00256d * math.cos(ascendingNode))
    val apparentLongitudeRadians = math.toRadians(apparentLongitude)

    val rightAscension = normalizeDegrees(
      math.toDegrees(
        math.atan2(math.cos(obliquity) * math.sin(apparentLongitudeRadians), math.cos(apparentLongitudeRadians))
      )
    )
    val declination    = math.toDegrees(math.asin(math.sin(obliquity) * math.sin(apparentLongitudeRadians)))

    // --- from the celestial sphere to the local sky -----------------------------------------
    val siderealTime = greenwichMeanSiderealTime(julianDayUT)
    val hourAngle    = math.toRadians(normalizeDegrees(siderealTime + observer.longitudeDegrees - rightAscension))
    val latitude     = math.toRadians(observer.latitudeDegrees)
    val declinationRadians = math.toRadians(declination)

    val geometricAltitude = math.toDegrees(
      math.asin(
        math.sin(latitude) * math.sin(declinationRadians) +
          math.cos(latitude) * math.cos(declinationRadians) * math.cos(hourAngle)
      )
    )
    val azimuth           = normalizeDegrees(
      180d + math.toDegrees(
        math.atan2(
          math.sin(hourAngle),
          math.cos(hourAngle) * math.sin(latitude) - math.tan(declinationRadians) * math.cos(latitude)
        )
      )
    )

    // --- topocentric correction, the observer is not at the center of the earth --------------
    val horizontalParallax = 8.794d / 3600d / radiusVector
    val topocentricAltitude = geometricAltitude - horizontalParallax * math.cos(math.toRadians(geometricAltitude))

    val refraction      = Refraction.correctionDegrees(topocentricAltitude, conditions)
    val apparentAltitude = topocentricAltitude + refraction

    val parallacticAngle = math.toDegrees(
      math.atan2(
        math.sin(hourAngle),
        math.tan(latitude) * math.cos(declinationRadians) - math.sin(declinationRadians) * math.cos(hourAngle)
      )
    )

    SunPosition(
      instant = instant,
      geometric = HorizontalCoordinates(azimuth, topocentricAltitude),
      apparent = HorizontalCoordinates(azimuth, apparentAltitude),
      equatorial = EquatorialCoordinates(rightAscension, declination),
      distanceAstronomicalUnits = radiusVector,
      semiDiameterDegrees = 959.63d / 3600d / radiusVector,
      parallacticAngleDegrees = parallacticAngle
    )
  }

  /** Greenwich mean sidereal time in degrees, Meeus (12.4) */
  def greenwichMeanSiderealTime(julianDayUT: Double): Double = {
    val centuries = (julianDayUT - 2451545d) / 36525d
    normalizeDegrees(
      280.46061837d + 360.98564736629d * (julianDayUT - 2451545d) +
        0.000387933d * centuries * centuries -
        centuries * centuries * centuries / 38710000d
    )
  }

  def normalizeDegrees(degrees: Double): Double = {
    val remainder = degrees % 360d
    if (remainder < 0d) remainder + 360d else remainder
  }
}

/** Atmospheric refraction near the horizon */
object Refraction {

  /** Bennett formula, refraction to add to the true altitude to get the apparent one, in degrees.
    *
    * At 45° above the horizon this is about 0.016°, at 5° about 0.17°, and at the horizon roughly
    * half a degree - as much as the solar diameter itself.
    */
  def correctionDegrees(trueAltitudeDegrees: Double, conditions: AtmosphericConditions = AtmosphericConditions()): Double = {
    // the formula is only meaningful down to the horizon, below it the altitude is clamped so that
    // the correction stays bounded and monotonic instead of blowing up - a sun that low is anyway
    // hidden by the landscape long before geometry becomes the limiting factor.
    val altitude   = math.max(trueAltitudeDegrees, -0.9d)
    val arcMinutes = 1.02d / math.tan(math.toRadians(altitude + 10.3d / (altitude + 5.11d)))
    val factor     =
      (conditions.pressureHectoPascals / 1010d) * (283d / (273d + conditions.temperatureCelsius))
    arcMinutes * factor / 60d
  }

  /** How much a disc of the given size is squashed at the given altitude, 0 when round.
    *
    * Refraction lifts the lower limb of the sun more than its upper one, so the disc loses height
    * without losing width : about 6 % of its diameter two degrees above the horizon, 2 % at five,
    * and a good third of it when it touches the horizon. This is the shape a low sun really has,
    * and what the elliptical fit of the detector is expected to find - which makes the measurement
    * checkable against physics rather than against a tolerance.
    *
    * @param trueAltitudeDegrees altitude of the disc center, before refraction
    */
  def flattening(
    trueAltitudeDegrees: Double,
    semiDiameterDegrees: Double = 0.266d,
    conditions: AtmosphericConditions = AtmosphericConditions()
  ): Double =
    if (semiDiameterDegrees <= 0d) 0d
    else {
      val lower = correctionDegrees(trueAltitudeDegrees - semiDiameterDegrees, conditions)
      val upper = correctionDegrees(trueAltitudeDegrees + semiDiameterDegrees, conditions)
      math.max(0d, (lower - upper) / (2d * semiDiameterDegrees))
    }
}
