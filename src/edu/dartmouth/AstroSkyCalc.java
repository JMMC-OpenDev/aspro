/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AstroSkyCalc.java,v 1.17 2010-04-02 09:20:25 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.16  2010/01/14 15:23:01  bourgesl
 * changed logger's class name
 *
 * Revision 1.15  2010/01/08 16:51:18  bourgesl
 * initial uv coverage
 *
 */
package edu.dartmouth;

import edu.dartmouth.SunAlmanachTime.SunAlmanachType;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.oi.AzEl;
import fr.jmmc.aspro.model.oi.LonLatAlt;
import fr.jmmc.aspro.util.AngleUtils;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.TreeSet;
import java.util.logging.Level;

/**
 * This class uses JSkyCalc to perform several astronomical computations (LST time, sun ephemerids, target position with elevation and azimuth ...)
 * 
 * @author bourgesl
 */
public class AstroSkyCalc {

  /** Class Name */
  private static final String className_ = "edu.dartmouth.AstroSkyCalc";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** time ratio : lst to jd */
  public static double LST_TO_JD = 24d * Const.SID_RATE;

  /* members */
  /** site location */
  private Site site;
  /** time / site info */
  private WhenWhere when;
  /** target info */
  private Observation observation;

  /**
   * Public Constructor
   */
  public AstroSkyCalc() {
    // nothing to do
  }

  /**
   * Define the observation site from the longitude, latitude and altitude coordinates (geographic)
   * @param name site name
   * @param position longitude (rad), latitude (rad) and altitude (m) coordinates
   */
  public void defineSite(final String name, final LonLatAlt position) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Site Long : " + Math.toDegrees(position.getLongitude()));
      logger.fine("Site Lat  : " + Math.toDegrees(position.getLatitude()));
    }

    // note : the given longitude is hours west in jSkyCalc :
    this.site = new Site(name,
            -AngleUtils.rad2hours(position.getLongitude()),
            Math.toDegrees(position.getLatitude()),
            position.getAltitude());

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Site dump : " + this.site.name +
              "\ntz offset : " + this.site.stdz +
              "\nlongitude : " + this.site.longit.RoundedLongitString(1, ":", true) +
              "\nlatitude  : " + this.site.lat.RoundedDecString(0, ":"));
    }
  }

  /**
   * Define the observation date with a gregorian date (lenient) in UTC at 00:00
   * @param year year
   * @param month month from [1-12]
   * @param day day in [1-31]
   */
  public void defineDate(final int year, final int month, final int day) {
    // yyyy mm dd hh:mm
    final String dateTime = year + " " + month + " " + day + " 00:00";

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("dateTime : " + dateTime);
    }

    // UTC time :
    final InstantInTime time = new InstantInTime(dateTime, this.site.stdz, this.site.use_dst, true);

    this.when = new WhenWhere(time, this.site);

    if (logger.isLoggable(Level.FINE)) {
      dumpWhen(this.when, "When");
    }
  }

  /**
   * Find the jd time value corresponding to the lst = 0 for the current date
   *
   * Note : accurate +/-12 h within the given date
   *
   * @return jd corresponding to the lst = 0 for the current date
   */
  public double findLst0() {
    return findLst0(this.when.when.jd);
  }

  /**
   * Find the jd time value corresponding to the lst = 0 for the given jd date
   *
   * Note : accurate +/-12 h within the given date
   *
   * @param jd julian date
   *
   * @return jd corresponding to the lst = 0 for the given jd date
   */
  public double findLst0(final double jd) {

    final WhenWhere ww = new WhenWhere(jd, this.site);

    // decimal hours :
    double error = ww.sidereal;
    int n = 0;

    while (error > 1e-3 && n < 9) {

      if (error > 12d) {
        error = 24d - error;
      } else {
        error *= -1d;
      }

      ww.ChangeWhen(ww.when.jd + (error / 24.d));

//    dumpWhen(this.when, "When");

      // next pass :
      error = ww.sidereal;
      n++;
    }

    if (logger.isLoggable(Level.FINE)) {
      dumpWhen(ww, "LST=0");
    }

    return ww.when.jd;
  }

  /**
   * Convert a julian date to a gregorian date (precise up to the second) in LST or UTC
   * @param jd julian date
   * @param useLst flag to select LST (true) or UTC conversion (false)
   * @return Date object
   */
  public Date toDate(final double jd, final boolean useLst) {
    final WhenWhere ww = new WhenWhere(jd, this.site);

    final InstantInTime t = ww.when;

    // The default TimeZone is already set to GMT :
    final Calendar cal = new GregorianCalendar();
    if (useLst) {
      // ignore the date info as the LST has only time :
      cal.set(Calendar.HOUR_OF_DAY, ww.siderealobj.sex.hour);
      cal.set(Calendar.MINUTE, ww.siderealobj.sex.minute);
      cal.set(Calendar.SECOND, (int) Math.round(ww.siderealobj.sex.second));
    } else {
      /* note : month is in range [0;11] in java Calendar */
      cal.set(t.UTDate.year, t.UTDate.month - 1, t.UTDate.day, t.UTDate.timeofday.hour, t.UTDate.timeofday.minute, (int) Math.round(t.UTDate.timeofday.second));
    }
    // fix milliseconds to 0 to be able to compare date instances :
    cal.set(Calendar.MILLISECOND, 0);

    return cal.getTime();
  }

  /**
   * Log the time info (UTC + sideral time)
   * @param ww WhenWhere instance
   * @param label message to log
   */
  private void dumpWhen(final WhenWhere ww, final String label) {
    if (logger.isLoggable(Level.FINE)) {
      final InstantInTime t = ww.when;
      logger.fine(label + " dump : " + t.jd +
              "\nUT : " + t.UTDate.day +
              "/" + t.UTDate.month +
              "/" + t.UTDate.year +
              " " + t.UTDate.timeofday.hour +
              ":" + t.UTDate.timeofday.minute +
              ":" + t.UTDate.timeofday.second +
              "\nlst : " + ww.siderealobj.RoundedRAString(3, ":"));
    }
  }

  /**
   * Return the list of Sun events arround LST [0;24] +- 12h
   * @param jdLst0 julian date corresponding to LST = 0
   * @return list of Sun events
   */
  public List<SunAlmanachTime> findSunRiseSet(final double jdLst0) {
    // unique sorted JD time stamps :
    final TreeSet<SunAlmanachTime> ts = new TreeSet<SunAlmanachTime>();

    addAlmanach(ts, jdLst0 - 1d);
    addAlmanach(ts, jdLst0);
    addAlmanach(ts, jdLst0 + 1d);
    addAlmanach(ts, jdLst0 + 2d);

    final List<SunAlmanachTime> sorted = new ArrayList<SunAlmanachTime>(ts);

    // return events in LST [0;24] +/- 12h :
    final double jd0 = jdLst0 - 0.5d;
    final double jd1 = jdLst0 + 1.5d;

    // find indexes inside the lst range [jd0;jd1] :
    int i0 = -1;
    int i1 = -1;
    SunAlmanachTime st;

    for (int i = 0, len = sorted.size(); i < len; i++) {
      st = sorted.get(i);
      /*
      if (logger.isLoggable(Level.FINE)) {
      dumpWhen(new WhenWhere(st.getJd(), this.site), st.getType().name());
      }
       */
      if (st.getJd() >= jd0) {
        if (i0 == -1) {
          // include previous event :
          i0 = i - 1;
        }
      }

      if (st.getJd() > jd1) {
        // include next event :
        i1 = i;
        break;
      }
    }

    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("filtered sun events :");
    }

    final List<SunAlmanachTime> result = new ArrayList<SunAlmanachTime>();

    for (int i = i0; i <= i1; i++) {
      st = sorted.get(i);

      if (logger.isLoggable(Level.FINEST)) {
        dumpWhen(new WhenWhere(st.getJd(), this.site), st.getType().name());
      }

      result.add(st);
    }

    return result;
  }

  /**
   * Add the sun almanach info as SunAlmanachTime objects for the given date
   * @param ts set to store the SunAlmanachTime objects
   * @param jd julian date
   */
  private void addAlmanach(final TreeSet<SunAlmanachTime> ts, final double jd) {
    final NightlyAlmanac na = new NightlyAlmanac(new WhenWhere(jd, this.site));

    ts.add(new SunAlmanachTime(na.morningTwilight.when.jd, SunAlmanachType.SunTwlRise));
    ts.add(new SunAlmanachTime(na.sunrise.when.jd, SunAlmanachType.SunRise));
    ts.add(new SunAlmanachTime(na.sunset.when.jd, SunAlmanachType.SunSet));
    ts.add(new SunAlmanachTime(na.eveningTwilight.when.jd, SunAlmanachType.SunTwlSet));
  }

  /**
   * Define a target by its RA/dec coordinates in degrees
   * @param jdLst0 julian date corresponding to LST = 0
   * @param ra right ascension (deg)
   * @param dec declination (deg)
   * @return double[] containing precessed ra (dec hours) and dec (deg) for the given jd date
   */
  public double[] defineTarget(final double jdLst0, final double ra, final double dec) {

    // RA (decimal hours), DEC (degrees)
    final Celest target = new Celest(AngleUtils.deg2hours(ra), dec, AsproConstants.EPOCH_J2000);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Target [RA/DEC/EPOCH] :" + target.Alpha.RoundedRAString(3, ":") + " " + target.Delta.RoundedDecString(3, ":"));
    }

    // define jd :
    this.when.ChangeWhen(jdLst0);

    this.observation = new Observation(this.when, target);

    return new double[]{this.observation.current.Alpha.value, this.observation.current.Delta.value};
  }

  /**
   * Return the current target position (azimuth / elevation) in degrees
   * @param jd julian date
   * @return azimuth (0 to north) / elevation in degrees
   */
  public AzEl getTargetPosition(final double jd) {

    this.observation.w.ChangeWhen(jd);
    this.observation.ComputeSky();

    /*
    if (logger.isLoggable(Level.FINE)) {
    dumpWhen(this.observation.w, "Target");
    logger.fine("az|alt   : " + this.observation.azimuth + " " + this.observation.altitude);
    }
     */
    return new AzEl(this.observation.azimuth, this.observation.altitude);
  }

  /**
   * Log the minimum and maximum elevation for the current target / site
   */
  private void getTargetMinMaxAlt() {
    final double[] minmax = Spherical.min_max_alt(this.site.lat.value, this.observation.current.Delta.value);

    // degrees :
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("min/max alt = " + minmax[0] + " / " + minmax[1]);
    }
  }

  /**
   * Computes the hour angle corresponding to the given elevation for the current target
   * @param dec target declination (corrected for the given julian date)
   * @param minElev min elevation (rad)
   * @return hour angle (dec hours) or -1 if the target never reaches this elevation
   */
  public double getHAForElevation(final double dec, final double minElev) {

    if (logger.isLoggable(Level.INFO)) {
      getTargetMinMaxAlt();
    }

    final double ha = Spherical.ha_alt(dec, this.site.lat.value, Math.toDegrees(minElev));

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
   * Return a string representation for RA (hms) and DEC (dms)
   * @param ra right ascension in deg
   * @param dec declination in deg
   * @return string[] containing RA (hms) and DEC (dms)
   */
  public static String[] toString(final double ra, final double dec) {

    // RA (decimal hours), DEC (degrees)
    final Celest target = new Celest(AngleUtils.deg2hours(ra), dec, AsproConstants.EPOCH_J2000);

    return new String[]{
              target.Alpha.RoundedRAString(3, ":"),
              target.Delta.RoundedDecString(3, ":")
            };
  }

  /**
   * Convert lst hours to jd hours
   * @param lst dec hours in LST
   * @return dec hours in JD
   */
  public static double lst2jd(final double lst) {
    return lst / AstroSkyCalc.LST_TO_JD;
  }

  /**
   * Convert jd hours to lst hours
   * @param jd dec hours
   * @return dec hours in LST
   */
  public static double jd2lst(final double jd) {
    return jd * AstroSkyCalc.LST_TO_JD;
  }
}
