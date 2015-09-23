/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package edu.dartmouth;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.oi.AzEl;
import fr.jmmc.aspro.util.AngleUtils;
import fr.jmmc.jmal.ALX;
import net.jafama.FastMath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class uses JSkyCalc to perform several observation computations (target position with elevation and azimuth ...)
 *
 * @author bourgesl
 */
public final class AstroSkyCalcObservation {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(AstroSkyCalcObservation.class.getName());

    /* members */
    /** cached log debug enabled */
    private final boolean isLogDebug = logger.isDebugEnabled();
    /** site location (package visibility) */
    private Site site;
    /** rise/set altitude taking into account the observatory altitude */
    private double rise_set_alt;
    /** cosinus of site latitude */
    private double cosLat = 0d;
    /** sinus of site latitude */
    private double sinLat = 0d;
    /** target info */
    private Observation observation = null;

    /**
     * Public Constructor
     */
    public AstroSkyCalcObservation() {
        // no-op
    }

    /**
     * Reset the current observation
     */
    public void reset() {
        this.observation = null;
    }

    /**
     * Define the observation site
     * @param sc Astro Sky Calc instance to get site and date information
     */
    public void defineSite(final AstroSkyCalc sc) {
        // copy site info :
        this.site = sc.site;

        // pre compute cosLat / sinLat used intensively by getTargetPosition():
        final double latRad = this.site.lat.radians();
        this.cosLat = FastMath.cos(latRad);
        this.sinLat = FastMath.sin(latRad);
        
        // approx means you can't use a negative elevation for the obs to
        // compensate for higher terrain.
        // depression of the horizon in degrees
        final double horiz = Const.DEG_IN_RADIAN * Math.sqrt(2d * sc.site.elevhoriz / (1000d * Const.EARTHRAD_IN_KM));
        this.rise_set_alt = -(0.83d + horiz);        
    }

    /**
     * Define a target by its RA/dec coordinates in degrees
     * and return its precessed coordinates for the given date
     * @param jd julian date used to precess the target
     * @param ra right ascension (deg)
     * @param dec declination (deg)
     * @param pmRa proper motion in RA given divided by cos(DE) in mas/yr
     * @param pmDec proper motion in DE in mas/yr
     * @return double[] containing precessed ra (dec hours) and dec (deg) for the given jd date
     */
    public double[] defineTarget(final double jd, final double ra, final double dec, final Double pmRa, final Double pmDec) {

        // Proper motion handling:
        final Celest target;

        if (pmRa != null && pmDec != null) {
            final double years = (jd - Const.J2000) * Const.DAY_IN_YEAR;

            if (isLogDebug) {
                logger.debug("Target PM[RA/DEC]: {}, {} mas/yr - years = {}", pmRa, pmDec, years);
            }

            // pmRA is given in RA*cos(DE) cf ASCC (Proper Motion in RA*cos(DE)):

            // RAJ2000_ep2000 "RAJ2000+(2000-1991.25)*pmRA/cos(DEJ2000*PI/180)/1000/3600"
            final double deltaRa = years * (pmRa / FastMath.cos(FastMath.toRadians(dec))) * 1e-3d * ALX.ARCSEC_IN_DEGREES;

            // DEJ2000_ep2000 "DEJ2000+(2000-1991.25)*pmDE/1000/3600"        
            final double deltaDec = years * pmDec * 1e-3d * ALX.ARCSEC_IN_DEGREES;

            if (isLogDebug) {
                logger.debug("Target delta[RA/DEC]: {} {} arcsec", deltaRa * ALX.DEG_IN_ARCSEC, deltaDec * ALX.DEG_IN_ARCSEC);
            }

            // RA (decimal hours), DEC (degrees)
            target = new Celest(AngleUtils.deg2hours(ra + deltaRa), dec + deltaDec, AsproConstants.EPOCH_J2000);
        } else {
            // RA (decimal hours), DEC (degrees)
            target = new Celest(AngleUtils.deg2hours(ra), dec, AsproConstants.EPOCH_J2000);
        }

        if (isLogDebug) {
            logger.debug("Target [RA/DEC/EPOCH]: {} {}",
                    target.alpha.roundedRAString(3, ":"),
                    target.delta.roundedDecString(3, ":"));
        }

        // define jd as lst0 to precess the target:
        final WhenWhere ww = new WhenWhere(jd, this.site);

        // note: observation is now precessed to (jd)
        // it has a minor impact on coordinates (few arcsec per year):
        this.observation = new Observation(ww, target, false);

        if (isLogDebug) {
            logger.debug("Target [RA/DEC/EPOCH] precessed: {} {}",
                    this.observation.current.alpha.roundedRAString(3, ":"),
                    this.observation.current.delta.roundedDecString(3, ":"));
        }

        return new double[]{this.observation.current.alpha.value, this.observation.current.delta.value};
    }

    /**
     * Return the current target position (azimuth / elevation) in degrees and hour angle
     * @param cosDec cosinus of target declination
     * @param sinDec sinus of target declination
     * @param jd julian date
     * @param position target position: azimuth (0 to north) / elevation in degrees
     * @return hour angle
     */
    public double getTargetPosition(final double cosDec, final double sinDec, final double jd, final AzEl position) {
        getTargetPosition(cosDec, sinDec, jd);
        position.setAzEl(this.observation.azimuth, this.observation.altitude);
        return this.observation.ha.value;
    }

    /**
     * Compute the current target position (azimuth / elevation) in degrees
     * @param cosDec cosinus of target declination
     * @param sinDec sinus of target declination
     * @param jd julian date
     */
    private void getTargetPosition(final double cosDec, final double sinDec, final double jd) {
        this.observation.w.changeWhen(jd);

        // avoid computing precessed coordinates:
        this.observation.computeSkyFast(this.cosLat, this.sinLat, cosDec, sinDec);
    }

    /**
     * Return the moon separation in degrees of the current target at the given julian date
     * @param cosDec cosinus of target declination
     * @param sinDec sinus of target declination
     * @param jd julian date
     * @return moon separation in degrees or +INFINITY if moon is not visible
     */
    public double getMoonSeparation(final double cosDec, final double sinDec, final double jd) {
        getTargetPosition(cosDec, sinDec, jd);
        return getMoonSeparation();
    }

    /**
     * Return the moon separation in degrees of the current observation
     *
     * Note: Must be called after getTargetPosition() as the target position is not computed here
     *
     * @return moon separation in degrees or +INFINITY if moon is not visible
     */
    private double getMoonSeparation() {
        // Compute moon position and distances :
        this.observation.computeMoonSeparation();

        if (this.observation.w.altmoon < rise_set_alt) {
            if (isLogDebug) {
                logger.debug("moon set ? jd {} - moon altitude = {}", this.observation.w.when.jd, this.observation.w.altmoon);
            }
            return Double.POSITIVE_INFINITY;
        }
        
        // observation.moonobj gives the angular distance with moon in degrees
        final double moonSeparation = this.observation.moonobj;

        if (isLogDebug) {
            logger.debug("jd {} - moon distance = {}", this.observation.w.when.jd, moonSeparation);
            AstroSkyCalc.dumpWhen(this.observation.w, "Target");
            logger.debug("az / alt : {} {}", this.observation.azimuth, this.observation.altitude);
        }

        return moonSeparation;
    }

    /**
     * Log the minimum and maximum elevation for the current target / site
     */
    private void getTargetMinMaxAlt() {
        final double[] minmax = Spherical.min_max_alt(this.site.lat.value, this.observation.current.delta.value);

        // degrees :
        if (isLogDebug) {
            logger.debug("min/max alt = {} / {}", minmax[0], minmax[1]);
        }
    }

    /**
     * Computes the hour angle corresponding to the given elevation for the current target
     * @param dec target declination (corrected for the given julian date)
     * @param minElev min elevation (deg)
     * @return hour angle (dec hours) or -1 if the target never reaches this elevation
     */
    public double getHAForElevation(final double dec, final double minElev) {
        if (isLogDebug) {
            getTargetMinMaxAlt();
        }

        final double ha = Spherical.ha_alt_Inf(dec, this.site.lat.value, minElev);

        if (ha == Double.NEGATIVE_INFINITY) {
            // never rise (target is never over the min elevation) :
            return -1d;
        }

        if (ha == Double.POSITIVE_INFINITY) {
            // always rise (target is always over the min elevation) :
            return 12d;
        }

        if (isLogDebug) {
            logger.debug("ha = {}", ha);
        }

        return ha;
    }

    // static methods :
    /**
     * Return a string representation for RA (hms) and DEC (dms) with 3 digits
     * @param ra right ascension in deg
     * @param dec declination in deg
     * @return string containing 'RA (hms) DEC (dms)'
     */
    public static String asString(final double ra, final double dec) {
        final String[] raDec = toString(ra, 3, dec, 3);
        return raDec[0] + " " + raDec[1];
    }

    /**
     * Return a string representation for RA (hms) and DEC (dms) with 3 digits
     * @param ra right ascension in deg
     * @param dec declination in deg
     * @return string[] containing RA (hms) and DEC (dms)
     */
    public static String[] toString(final double ra, final double dec) {
        return toString(ra, 3, dec, 3);
    }

    /**
     * Return a string representation for RA (hms) and DEC (dms) with the given precision (number of digits)
     * @param ra right ascension in deg
     * @param raDigits ra digits
     * @param dec declination in deg
     * @param decDigits ra digits
     * @return string[] containing RA (hms) and DEC (dms)
     */
    public static String[] toString(final double ra, final int raDigits, final double dec, final int decDigits) {
        // RA (decimal hours), DEC (degrees)
        final Celest target = new Celest(AngleUtils.deg2hours(ra), dec, AsproConstants.EPOCH_J2000);

        return new String[]{
            target.alpha.roundedRAString(raDigits, ":"),
            target.delta.roundedDecString(decDigits, ":")
        };
    }
}
