/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AstroSkyCalc.java,v 1.23 2010-06-28 12:26:17 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.22  2010/06/25 15:14:54  bourgesl
 * added toCalendar method to get calendar instance instead of date
 *
 * Revision 1.21  2010/06/25 14:12:16  bourgesl
 * code cleanup
 * methods related to targets moved in AstoSkyCalcObservation
 * Added methods to convert HA to JD
 *
 * Revision 1.20  2010/06/17 10:02:51  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.19  2010/05/26 15:28:09  bourgesl
 * added method toString(ra, raDigits, dec, decDigits) to choose the number of digits
 *
 * Revision 1.18  2010/04/02 14:39:19  bourgesl
 * elevation in degrees instead of rad
 *
 * Revision 1.17  2010/04/02 09:20:25  bourgesl
 * updated javadoc
 * added toString(ra / dec in degrees) conversion
 *
 * Revision 1.16  2010/01/14 15:23:01  bourgesl
 * changed logger's class name
 *
 * Revision 1.15  2010/01/08 16:51:18  bourgesl
 * initial uv coverage
 *
 */
package edu.dartmouth;

import edu.dartmouth.SunAlmanachTime.SunAlmanachType;
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
 * This class uses JSkyCalc to perform several astronomical computations (sun ephemerids, LST/UTC time, JD time ...)
 * 
 * @author bourgesl
 */
public final class AstroSkyCalc {

  /** Class Name */
  private static final String className_ = "edu.dartmouth.AstroSkyCalc";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className_);
  /** time ratio : lst to jd */
  public final static double LST_TO_JD = 24d * Const.SID_RATE;
  /** one Day in LST expressed in JD day */
  public final static double LST_DAY_IN_JD = AstroSkyCalc.lst2jd(24d);
  /** Modified Juliean day reference */
  public final static double MJD_REF = 2400000.5d;
  /** 1 second in decimal hour */
  private final static double SEC_IN_DEC_HOUR = 1d / 3600d;

  /* members */
  /** site location */
  Site site;
  /** jd corresponding to LST=00:00:00 for the observation date */
  private double jdLst0;

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
    // stdz = longitude (dec hours) is also the GMT offset (UTC)
    this.site = new Site(name,
            -AngleUtils.rad2hours(position.getLongitude()),
            Math.toDegrees(position.getLatitude()),
            position.getAltitude());

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Site dump : " + this.site.name
              + "\ntz offset : " + this.site.stdz
              + "\nlongitude : " + this.site.longit.RoundedLongitString(1, ":", true)
              + "\nlatitude  : " + this.site.lat.RoundedDecString(0, ":"));
    }
  }

  /**
   * Define the observation date with a gregorian date (lenient) in UTC at 00:00
   * and return the jd time value corresponding to LST=00:00:00 for the current date
   * @param year year
   * @param month month from [1-12]
   * @param day day in [1-31]
   * @return jd corresponding to LST=00:00:00 for the current date
   */
  public double defineDate(final int year, final int month, final int day) {
    // yyyy mm dd hh:mm
    final String dateTime = year + " " + month + " " + day + " 00:00";

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("dateTime : " + dateTime);
    }

    // Define date as an UTC time :
    final InstantInTime instant = new InstantInTime(dateTime, this.site.stdz, this.site.use_dst, true);

    final WhenWhere ww = new WhenWhere(instant, this.site);

    if (logger.isLoggable(Level.FINE)) {
      dumpWhen(ww, "When");
    }

    // Find the julian date corresponding to the LST origin LST=00:00:00 for the given date :
    this.jdLst0 = findJdForLst0(ww.when.jd);

    return this.jdLst0;
  }

  /**
   * Return the jd time value corresponding to LST=00:00:00 for the current date
   * @see #findJdForLst0(double)
   *
   * @return jd corresponding to LST=00:00:00 for the current date
   */
  public double getJdForLst0() {
    return this.jdLst0;
  }

  /**
   * Find the jd time value corresponding to LST=00:00:00 arround the given jd date
   *
   * Note : accurate +/-12 h within the given date
   *
   * @param jd julian date
   *
   * @return jd corresponding to LST=00:00:00 for the given jd date
   */
  public double findJdForLst0(final double jd) {

    final WhenWhere ww = new WhenWhere(jd, this.site);

    // decimal hours :
    double error = ww.sidereal;
    int n = 0;

    while (error > SEC_IN_DEC_HOUR && n < 9) {

      if (error > 12d) {
        error = 24d - error;
      } else {
        error *= -1d;
      }

      ww.ChangeWhen(ww.when.jd + (error / 24d));

//    dumpWhen(this.when, "When");

      // next pass :
      error = ww.sidereal;
      n++;
    }

    if (logger.isLoggable(Level.FINE)) {
      dumpWhen(ww, "LST=00:00:00");
    }

    return ww.when.jd;
  }

  /**
   * Convert a julian date to a gregorian date (precise up to the second) in LST
   * @param jd julian date
   * @return Date object
   */
  public Date toDateLST(final double jd) {
    return toDate(jd, true);
  }

  /**
   * Convert a julian date to a gregorian date (precise up to the second) in LST or UTC
   * @param jd julian date
   * @param useLst flag to select LST (true) or UTC conversion (false)
   * @return Date object
   */
  public Date toDate(final double jd, final boolean useLst) {
    return toCalendar(jd, useLst).getTime();
  }

  /**
   * Convert a julian date to a gregorian calendar (precise up to the second) in LST or UTC
   * @param jd julian date
   * @param useLst flag to select LST (true) or UTC conversion (false)
   * @return Calendar object
   */
  public Calendar toCalendar(final double jd, final boolean useLst) {
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
      cal.set(t.UTDate.year, t.UTDate.month - 1, t.UTDate.day,
              t.UTDate.timeofday.hour, t.UTDate.timeofday.minute, (int) Math.round(t.UTDate.timeofday.second));
    }
    // fix milliseconds to 0 to be able to compare date instances :
    cal.set(Calendar.MILLISECOND, 0);

    return cal;
  }

  /**
   * Log the time info (UTC + sideral time)
   * @param ww WhenWhere instance
   * @param label message to log
   */
  private void dumpWhen(final WhenWhere ww, final String label) {
    if (logger.isLoggable(Level.FINE)) {
      final InstantInTime t = ww.when;
      logger.fine(label + " dump : " + t.jd
              + "\nUT : " + t.UTDate.day
              + "/" + t.UTDate.month
              + "/" + t.UTDate.year
              + " " + t.UTDate.timeofday.hour
              + ":" + t.UTDate.timeofday.minute
              + ":" + t.UTDate.timeofday.second
              + "\nlst : " + ww.siderealobj.RoundedRAString(3, ":"));
    }
  }

  /**
   * Return the list of Sun events arround LST [0;24] +- 12h
   * @param jdLst0 julian date corresponding to LST=00:00:00 for the observation date
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

//      if (logger.isLoggable(Level.FINE)) {
//        dumpWhen(new WhenWhere(st.getJd(), this.site), st.getType().name());
//      }

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
    final WhenWhere ww = new WhenWhere(jd, this.site);

    final NightlyAlmanac na = new NightlyAlmanac(ww);

    ts.add(new SunAlmanachTime(na.morningTwilight.when.jd, SunAlmanachType.SunTwlRise));
    ts.add(new SunAlmanachTime(na.sunrise.when.jd, SunAlmanachType.SunRise));
    ts.add(new SunAlmanachTime(na.sunset.when.jd, SunAlmanachType.SunSet));
    ts.add(new SunAlmanachTime(na.eveningTwilight.when.jd, SunAlmanachType.SunTwlSet));
  }

  /**
   * Convert a decimal hour angle in Julian day
   * @param ha decimal hour angle
   * @param precRA precessed target right ascension in decimal hours
   * @return JD value
   */
  public double convertHAToJD(final double ha, final double precRA) {
    final double lst = precRA + ha;

    // apply the sideral / solar ratio :
    return this.jdLst0 + AstroSkyCalc.lst2jd(lst);
  }

  /**
   * Convert a Julian day in decimal hour angle
   * Note : HA is not resticted to [-12;12]
   * @param jd date time
   * @param precRA precessed target right ascension in decimal hours
   * @return ha decimal hour angle
   */
  public double convertJDToHA(final double jd, final double precRA) {
    // apply the sideral / solar ratio :
    final double lst = AstroSkyCalc.jd2lst(jd - this.jdLst0);

    return lst - precRA;
  }

  // static methods :
  /**
   * Convert lst hours to jd hours
   * @param lst dec hours in LST
   * @return dec hours in JD
   */
  public static double lst2jd(final double lst) {
    return lst / LST_TO_JD;
  }

  /**
   * Convert jd hours to lst hours
   * @param jd dec hours
   * @return dec hours in LST
   */
  public static double jd2lst(final double jd) {
    return jd * LST_TO_JD;
  }

  /**
   * Return the modified julian day from the given julian day
   * @param jd julian day
   * @return modified julian day
   */
  public static double mjd(final double jd) {
    return jd - MJD_REF;
  }
}
