/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AstroSkyCalcObservation.java,v 1.4 2011-04-26 13:02:20 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.3  2011/04/22 15:37:49  bourgesl
 * JSkyCalc refactoring
 *
 * Revision 1.2  2010/09/15 13:51:47  bourgesl
 * comments explaining how to get moon angular distance
 *
 * Revision 1.1  2010/06/25 14:12:38  bourgesl
 * methods from AstoSkyCalc related to targets moved in AstoSkyCalcObservation
 *
 *
 ******************************************************************************/
package edu.dartmouth;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.oi.AzEl;
import fr.jmmc.aspro.util.AngleUtils;
import java.util.logging.Level;

/**
 * This class uses JSkyCalc to perform several observation computations (target position with elevation and azimuth ...)
 *
 * @author bourgesl
 */
public final class AstroSkyCalcObservation {

  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(AstroSkyCalcObservation.class.getName());

  /* members */
  /** site location (package visibility) */
  private Site site;
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
  }

  /**
   * Define a target by its RA/dec coordinates in degrees
   * and return its precessed coordinates for the given date
   * @param jd julian date used to precess the target
   * @param ra right ascension (deg)
   * @param dec declination (deg)
   * @return double[] containing precessed ra (dec hours) and dec (deg) for the given jd date
   */
  public double[] defineTarget(final double jd, final double ra, final double dec) {

    // RA (decimal hours), DEC (degrees)
    final Celest target = new Celest(AngleUtils.deg2hours(ra), dec, AsproConstants.EPOCH_J2000);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Target [RA/DEC/EPOCH] :" + target.alpha.roundedRAString(3, ":") + " " + target.delta.roundedDecString(3, ":"));
    }

    // define jd as lst0 to precess the target:
    final WhenWhere ww = new WhenWhere(jd, this.site);

    // note: observation is now precessed to (jd)
    // it has a minor impact on coordinates (few arcsec per year):
    this.observation = new Observation(ww, target);

//    logger.severe("Target [RA/DEC/EPOCH] CENTER :" + this.observation.current.alpha.roundedRAString(3, ":") + " " + this.observation.current.delta.roundedDecString(3, ":"));

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Target [RA/DEC/EPOCH] precessed :" + this.observation.current.alpha.roundedRAString(3, ":") + " " + this.observation.current.delta.roundedDecString(3, ":"));
    }

    return new double[]{this.observation.current.alpha.value, this.observation.current.delta.value};
  }

  /**
   * Return the current target position (azimuth / elevation) in degrees
   * @param jd julian date
   * @param position target position: azimuth (0 to north) / elevation in degrees
   */
  public void getTargetPosition(final double jd, final AzEl position) {
    this.observation.w.changeWhen(jd);

    // avoid computing precessed coordinates:
    this.observation.computeSky(false);

//    logger.severe("Target [RA/DEC/EPOCH] :" + this.observation.current.alpha.roundedRAString(3, ":") + " " + this.observation.current.delta.roundedDecString(3, ":"));

    // TODO : check angular distance between target and moon :
    /*
     * // Check moon distance :
     * this.observation.computeSunMoon();
     *
     * observation.moonobj gives the angular distance with moon in degrees
     *
     * logger.severe("jd " + jd + " moon distance = " + this.observation.moonobj);
     *
     * if (logger.isLoggable(Level.FINE)) {
     *   AstroSkyCalc.dumpWhen(this.observation.w, "Target");
     *   logger.fine("az|alt   : " + this.observation.azimuth + " " + this.observation.altitude);
     * }
     */

    position.setAzEl(this.observation.azimuth, this.observation.altitude);
  }

  /**
   * Log the minimum and maximum elevation for the current target / site
   */
  private void getTargetMinMaxAlt() {
    final double[] minmax = Spherical.min_max_alt(this.site.lat.value, this.observation.current.delta.value);

    // degrees :
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("min/max alt = " + minmax[0] + " / " + minmax[1]);
    }
  }

  /**
   * Computes the hour angle corresponding to the given elevation for the current target
   * @param dec target declination (corrected for the given julian date)
   * @param minElev min elevation (deg)
   * @return hour angle (dec hours) or -1 if the target never reaches this elevation
   */
  public double getHAForElevation(final double dec, final double minElev) {

    if (logger.isLoggable(Level.INFO)) {
      getTargetMinMaxAlt();
    }

    final double ha = Spherical.ha_alt(dec, this.site.lat.value, minElev);

    if (ha == -1000d) {
      // never rise (target is never over the min elevation) :
      return -1d;
    }

    if (ha == 1000d) {
      // always rise (target is always over the min elevation) :
      return 12d;
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("ha = " + ha);
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
