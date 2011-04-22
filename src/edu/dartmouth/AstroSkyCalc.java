/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AstroSkyCalc.java,v 1.27 2011-04-22 15:37:33 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.26  2010/10/01 15:25:56  bourgesl
 * fixed bug in findLst0 [Requires 'lower' < 'upper'] :
 * precision is better (<1ms) and LST must be < 1s
 *
 * Revision 1.25  2010/09/15 13:51:47  bourgesl
 * comments explaining how to get moon angular distance
 *
 * Revision 1.24  2010/07/22 12:32:22  bourgesl
 * added moon rise/set and moon illumination fraction
 *
 * Revision 1.23  2010/06/28 12:26:17  bourgesl
 * added mjd(jd) to get modified julian day
 *
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

import edu.dartmouth.AstroAlmanacTime.AlmanacType;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.oi.LonLatAlt;
import fr.jmmc.aspro.util.AngleUtils;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;

/**
 * This class uses JSkyCalc to perform several astronomical computations (sun ephemerids, LST/UTC time, JD time ...)
 * 
 * @author bourgesl
 */
public final class AstroSkyCalc {

  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(AstroSkyCalc.class.getName());
  /** time ratio : lst to jd */
  public final static double LST_TO_JD = 24d * Const.SID_RATE;
  /** one Day in LST expressed in JD day */
  public final static double LST_DAY_IN_JD = 1d / Const.SID_RATE;
  /** Modified Juliean day reference */
  public final static double MJD_REF = 2400000.5d;
  /** 1 second in decimal hour */
  private final static double SEC_IN_DEC_HOUR = 1d / 3600d;
  /** millisecond in decimal hour */
  private final static double MSEC_IN_DEC_HOUR = SEC_IN_DEC_HOUR / 1000d;
  /** minimal precision value in decimal hour */
  private final static double PREC_IN_DEC_HOUR = MSEC_IN_DEC_HOUR / 10d;

  /* members */
  /** site location */
  Site site;
  /** jd corresponding to LT=12:00:00 for the observation date */
  private double jdLt12;
  /** jd corresponding to LT=24:00:00 for the observation date */
  private double jdMidnight;
  /** jd corresponding to LST=00:00:00 for the observation date */
  private double jdLst0;
  /** date correspondign to LST=00:00:00 */
  private Calendar dateLst0;

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
   * Define the observation date with a gregorian date (lenient)
   * and return the jd time value corresponding to LST=00:00:00 for the current date
   * @param year year
   * @param month month from [1-12]
   * @param day day in [1-31]
   * @return jd corresponding to LST=00:00:00 for the current date night
   */
  public double defineDate(final int year, final int month, final int day) {
    // yyyy mm dd hh:mm
    final String dateTime = year + " " + month + " " + day + " 12:00";

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("dateTime : " + dateTime);
    }

    // Define date as 12:00 (local time) :
    final WhenWhere ww = new WhenWhere(new InstantInTime(dateTime, this.site.stdz, this.site.use_dst, false),
            this.site);

    if (logger.isLoggable(Level.FINE)) {
      AstroSkyCalc.dumpWhen(ww, "LocalTime at 12:00");
    }

    // Define the julian date corresponding to LT=12:00:00 for the given date :
    this.jdLt12 = ww.when.jd;

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("jdUt12 = " + this.jdLt12);
    }

    // midnight (local time) for the given date to locate LST origin arround:
    this.jdMidnight = this.jdLt12 + 0.5d;

    // Find the julian date corresponding to the LST origin LST=00:00:00 arround the given date :
    final InstantInTime lst0 = findLst0(jdMidnight);

    this.jdLst0 = lst0.jd;

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("jdLst0 = " + this.jdLst0);
    }

    // define the date at LST origin to have it when converting LST time in java dates (date + time):
    this.dateLst0 = new GregorianCalendar(lst0.UTDate.year, lst0.UTDate.month - 1, lst0.UTDate.day);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("dateLst0 = " + this.dateLst0.getTime());
    }

    return this.jdLst0;
  }

  /**
   * Return the jd time value corresponding to LT=12:00:00 for the current date
   *
   * @return jd corresponding to LT=12:00:00 for the current date
   */
  public double getJdLt12() {
    return this.jdLt12;
  }

  /**
   * Return the jd time value corresponding to LT=24:00:00 for the observation date
   *
   * @return jd corresponding to LT=24:00:00 for the observation date
   */
  public double getJdMidnight() {
    return this.jdMidnight;
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
    return findLst0(jd).jd;
  }

  /**
   * Find when LST=00:00:00 arround the given jd date
   *
   * Note : accurate +/-12 h within the given date
   *
   * @param jd julian date
   *
   * @return instant corresponding to LST=00:00:00 for the given jd date
   */
  private InstantInTime findLst0(final double jd) {

    final WhenWhere ww = new WhenWhere(jd, this.site, false);

    double error;
    double sign;

    // decimal hours :
    double lst;

    for (int n = 0;; n++) {
      lst = ww.sidereal;

      if (lst > 12d) {
        error = 24d - lst;
        sign = 1d;
      } else {
        error = lst;
        sign = -1d;
      }

      // test if error is less than 1ms, then exit loop :
      // absolute lst value must be positive i.e. inside [0;1s] :
      if (error < MSEC_IN_DEC_HOUR && lst < SEC_IN_DEC_HOUR || n > 10) {
        break;
      }

      // avoid too small step (double precision limit) :
      if (error < PREC_IN_DEC_HOUR) {
        error = PREC_IN_DEC_HOUR;
      }

      // adjust jd :
      ww.ChangeWhen(ww.when.jd + sign * error / 24d);

//      dumpWhen(ww, "When");
    }

    if (logger.isLoggable(Level.FINE)) {
      AstroSkyCalc.dumpWhen(ww, "LST=00:00:00");
    }

    return ww.when;
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
    final Calendar cal = toCalendar(jd, useLst);
    return cal.getTime();
  }

  /**
   * Convert a julian date to a gregorian calendar (precise up to the second) in LST or UTC
   * @param jd julian date
   * @param useLst flag to select LST (true) or UTC conversion (false)
   * @return Calendar object
   */
  public Calendar toCalendar(final double jd, final boolean useLst) {
    final WhenWhere ww = new WhenWhere(jd, this.site, false);

    /* notes :
     * - month is in range [0;11] in java Calendar
     * - milliseconds are set to 0 to be able to compare date instances
     */

    // The default TimeZone is already set to GMT :
    final Calendar calendar;
    if (useLst) {
      final RA sidereal = ww.siderealobj;

      // use the observation date as the LST has only time :
      calendar = new GregorianCalendar(this.dateLst0.get(Calendar.YEAR), this.dateLst0.get(Calendar.MONTH), this.dateLst0.get(Calendar.DAY_OF_MONTH),
              sidereal.sex.hour, sidereal.sex.minute, (int) Math.round(sidereal.sex.second));

      double days = (jd - this.jdLst0) * Const.SID_RATE;
//      logger.severe("days=" + days);
      int ndays = 0;
      if (days > 0d) {
        // Add 1ms (precision):
        days += MSEC_IN_DEC_HOUR;
        if (days > 1d) {
          ndays = (int) Math.floor(days);
        }
      } else if (days < 0d) {
        ndays = -1;
        // Add 1ms (precision):
        days += MSEC_IN_DEC_HOUR;
        if (days < -1d) {
          ndays += (int) Math.ceil(days);
        }
      }
      if (ndays != 0) {
//        logger.severe("ndays=" + ndays);
        calendar.add(Calendar.DAY_OF_YEAR, ndays);
      }

    } else {
      final GenericCalDat utDate = ww.when.UTDate;

      calendar = new GregorianCalendar(utDate.year, utDate.month - 1, utDate.day,
              utDate.timeofday.hour, utDate.timeofday.minute, (int) Math.round(utDate.timeofday.second));
    }

    return calendar;
  }

  /**
   * Log the time info (UTC + sideral time)
   * @param ww WhenWhere instance
   * @param label message to log
   */
  static void dumpWhen(final WhenWhere ww, final String label) {
    dumpWhen(Level.FINE, ww, label);
  }

  /**
   * Log the time info (UTC + sideral time)
   * @param logLevel log level (FINE...)
   * @param ww WhenWhere instance
   * @param label message to log
   */
  static void dumpWhen(final Level logLevel, final WhenWhere ww, final String label) {
    if (logger.isLoggable(logLevel)) {
      final InstantInTime t = ww.when;
      logger.log(logLevel, label + " dump : " + t.jd
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
   * Return the almanac for the proper night (arround midnight).
   * Moreover, sun times are duplicated and translated to -1Day, +1Day and +2Day
   * to be able to have night limits among LST[-12;+36]
   * @return almanac
   */
  public AstroAlmanac getAlmanac() {
    // Get the almanac for the proper night (arround midnight):
    final AstroAlmanac almanacNight = new AstroAlmanac();

    // MIDNIGHT :
    addAlmanacTimes(this.jdMidnight, almanacNight);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("almanacNight: " + almanacNight);
    }

    // Notes :
    // 1. Lst0 is arround midnight (+/- 12h):
    // 2. targets RA are in range [0;24]
    // 3. HA are in range [-12;12]
    // LST frame is in range [-12;36]
    // For midnight : in range [-24;48]

    // Translates sun times (-1,1,2 days) only:
    final AstroAlmanac almanac = new AstroAlmanac();

    // copy midnight & moon times:
    almanac.getMidnights().addAll(almanacNight.getMidnights());

    Set<AstroAlmanacTime> refTimes;

    // Hack on moon to have still fake moon ranges (rise / set):
    // findMoonRiseSet returns ranges if (rise / set) couples are present!

    // note: will be helpful as moon distance check is implemented to avoid calculations when moon is set !

    // MOON:
    refTimes = almanacNight.getMoonTimes();
    final Set<AstroAlmanacTime> moonTimes = almanac.getMoonTimes();
    // MIDNIGHT - 1DAY :
    AstroAlmanac.translate(-LST_DAY_IN_JD, refTimes, moonTimes);
    // MIDNIGHT :
    moonTimes.addAll(refTimes);
    // MIDNIGHT + 1DAY :
    AstroAlmanac.translate(+LST_DAY_IN_JD, refTimes, moonTimes);

    // SUN:
    refTimes = almanacNight.getSunTimes();
    final Set<AstroAlmanacTime> sunTimes = almanac.getSunTimes();

    // MIDNIGHT - 1DAY :
    AstroAlmanac.translate(-LST_DAY_IN_JD, refTimes, sunTimes);
    // MIDNIGHT :
    sunTimes.addAll(refTimes);
    // MIDNIGHT + 1DAY :
    AstroAlmanac.translate(+LST_DAY_IN_JD, refTimes, sunTimes);
    // MIDNIGHT + 2DAY :
    AstroAlmanac.translate(2d * LST_DAY_IN_JD, refTimes, sunTimes);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("almanac: " + almanac);
    }

    return almanac;
  }

  /**
   * Find the next midnight following jdUt12 in the given almanac instance
   * @param almanac almanac instance
   * @return julian date corresponding to midnight for the observation date
   */
  public double findMidnight(final AstroAlmanac almanac) {
    double jdMid = 0d;

    AstroAlmanacTime midnight;
    for (final Iterator<AstroAlmanacTime> it = almanac.getMidnights().iterator(); it.hasNext();) {
      midnight = it.next();

      if (logger.isLoggable(Level.FINEST)) {
        AstroSkyCalc.dumpWhen(new WhenWhere(midnight.getJd(), this.site, false), midnight.getType().name());
      }

      if (midnight.getJd() >= this.jdLt12) {
        jdMid = midnight.getJd();
        break;
      }
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("jdMidnight : " + jdMid);
    }
//    logger.severe("jdMidnight : " + jdMid);
//    logger.severe("jdLt24     : " + this.jdMidnight);

//    logger.severe("diff       : " + Math.abs(jdMid - this.jdMidnight));

    return jdMid;
  }

  /**
   * Return the jd ranges when moon is observable in range [jdLower; jdUpper]
   * @param almanac almanac instance
   * @param jdLower lower jd
   * @param jdUpper upper jd
   * @return list of jd ranges when moon is over the horizon
   */
  public List<Range> findMoonRiseSet(final AstroAlmanac almanac, final double jdLower, final double jdUpper) {

    // TODO : refactor that code to translates times into intervals inside a given valid range [jdLower; jdUpper]

    final List<AstroAlmanacTime> sorted = new ArrayList<AstroAlmanacTime>(almanac.getMoonTimes());
    final List<Range> ranges = new ArrayList<Range>(2);

    AstroAlmanacTime moonFrom, moonTo;
    double jdFrom, jdTo;

    for (int i = 0, size = sorted.size() - 1; i < size; i++) {
      moonFrom = sorted.get(i);

      if (moonFrom.getType() == AstroAlmanacTime.AlmanacType.MoonRise) {
        moonTo = sorted.get(i + 1);

        jdFrom = moonFrom.getJd();
        jdTo = moonTo.getJd();

        // Keep intervals that are inside or overlapping the range [jdLower; jdUpper] range :
        if ((jdFrom >= jdLower && jdFrom <= jdUpper) || (jdTo >= jdLower && jdTo <= jdUpper)) {

          if (logger.isLoggable(Level.FINE)) {
            logger.fine("Range[" + jdFrom + " - " + jdTo + "]");
          }

          // adjust range limits :
          if (jdFrom < jdLower) {
            jdFrom = jdLower;
          }
          if (jdTo > jdUpper) {
            jdTo = jdUpper;
          }

          ranges.add(new Range(jdFrom, jdTo));
        }
      }
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("moon ranges : " + ranges);
    }

    return ranges;
  }

  /**
   * Return the maximum of the moon illumination fraction for the given jd ranges
   * @param moonRanges jd ranges
   * @return maximum of the moon illumination fraction
   */
  public double getMaxMoonIllum(final List<Range> moonRanges) {
    double maxIllum = 0d;
    double jdMin, jdMax, jdMid;

    for (Range range : moonRanges) {
      jdMin = range.getMin();
      jdMax = range.getMax();
      jdMid = (jdMin + jdMax) / 2d;

      maxIllum = Math.max(maxIllum, moonIllum(jdMin));
      maxIllum = Math.max(maxIllum, moonIllum(jdMid));
      maxIllum = Math.max(maxIllum, moonIllum(jdMax));
    }
    return maxIllum;
  }

  /**
   * Return the moon illumination fraction for the given julian date
   * @param jd julian date
   * @return moon illumination fraction
   */
  private double moonIllum(final double jd) {
    final WhenWhere wwMoon = new WhenWhere(jd, this.site);
    wwMoon.ComputeSunMoon();

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("moon zenith = " + toDateLST(jd));
      logger.fine("alt illum = " + wwMoon.altmoon);
      logger.fine("moon illum = " + wwMoon.moonillum);
    }

    return wwMoon.moonillum;
  }

  /**
   * Return the list of Sun events in range [jdLower; jdUpper] +/- 12h
   * @param almanac almanac instance
   * @param jdLower lower jd
   * @param jdUpper upper jd
   * @return list of Sun events
   */
  public List<AstroAlmanacTime> findSunRiseSet(final AstroAlmanac almanac, final double jdLower, final double jdUpper) {

    // TODO : refactor that code to translates times into intervals inside a given valid range [jdLower; jdUpper]

    final double jd0 = jdLower - 0.5d * LST_DAY_IN_JD;
    final double jd1 = jdUpper + 0.5d * LST_DAY_IN_JD;

    final List<AstroAlmanacTime> sorted = new ArrayList<AstroAlmanacTime>(almanac.getSunTimes());

    // find indexes inside the lst range [jd0;jd1] :
    int i0 = -1;
    int i1 = -1;
    AstroAlmanacTime sunTime;

    for (int i = 0, len = sorted.size(); i < len; i++) {
      sunTime = sorted.get(i);

//      if (logger.isLoggable(Level.FINE)) {
//        dumpWhen(new WhenWhere(st.getJd(), this.site), st.getType().name());
//      }

      if (sunTime.getJd() >= jd0) {
        if (i0 == -1) {
          // include previous event :
          i0 = i - 1;
        }
      }

      if (sunTime.getJd() > jd1) {
        // include next event :
        i1 = i;
        break;
      }
    }

    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("filtered sun events :");
    }

    final List<AstroAlmanacTime> result = new ArrayList<AstroAlmanacTime>();

    for (int i = i0; i <= i1; i++) {
      sunTime = sorted.get(i);

      if (logger.isLoggable(Level.FINEST)) {
        AstroSkyCalc.dumpWhen(new WhenWhere(sunTime.getJd(), this.site), sunTime.getType().name());
      }

      result.add(sunTime);
    }

    return result;
  }

  /**
   * Add the sun and moon information as AstroAlmanacTime objects for the given date
   * @param jd julian date
   * @param almanac almanac instance set to store the AstroAlmanacTime objects
   */
  private void addAlmanacTimes(final double jd, final AstroAlmanac almanac) {
    final WhenWhere ww = new WhenWhere(jd, this.site);

    final NightlyAlmanac na = new NightlyAlmanac(ww);

    // add midnight:
    almanac.getMidnights().add(new AstroAlmanacTime(na.midnight.when.jd, AlmanacType.Midnight));

    // add sun times:
    final Set<AstroAlmanacTime> sunTimes = almanac.getSunTimes();

    sunTimes.add(new AstroAlmanacTime(na.morningTwilight18.when.jd, AlmanacType.SunTwl18Rise));
    sunTimes.add(new AstroAlmanacTime(na.morningTwilight12.when.jd, AlmanacType.SunTwl12Rise));
    sunTimes.add(new AstroAlmanacTime(na.morningTwilight06.when.jd, AlmanacType.SunTwl06Rise));
    sunTimes.add(new AstroAlmanacTime(na.sunrise.when.jd, AlmanacType.SunRise));
    sunTimes.add(new AstroAlmanacTime(na.sunset.when.jd, AlmanacType.SunSet));
    sunTimes.add(new AstroAlmanacTime(na.eveningTwilight06.when.jd, AlmanacType.SunTwl06Set));
    sunTimes.add(new AstroAlmanacTime(na.eveningTwilight12.when.jd, AlmanacType.SunTwl12Set));
    sunTimes.add(new AstroAlmanacTime(na.eveningTwilight18.when.jd, AlmanacType.SunTwl18Set));

    // add moon times:
    final Set<AstroAlmanacTime> moonTimes = almanac.getMoonTimes();

    moonTimes.add(new AstroAlmanacTime(na.moonrise.when.jd, AlmanacType.MoonRise));
    moonTimes.add(new AstroAlmanacTime(na.moonset.when.jd, AlmanacType.MoonSet));
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
