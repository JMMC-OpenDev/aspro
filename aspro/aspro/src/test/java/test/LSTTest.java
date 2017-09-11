/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package test;

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.model.TimeRef;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.ObservabilityContext;
import fr.jmmc.aspro.model.observability.DateTimeInterval;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerDescription;

import fr.jmmc.jmcs.util.NumberUtils;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class tests the AstroSkyCalc class to locate LST [0;24] range for every day between 1/1/1980 and 31/12/2049
 * @author bourgesl
 */
public final class LSTTest {

  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(LSTTest.class.getName());

  /**
   * Forbidden constructor
   */
  private LSTTest() {
    super();
  }

  /**
   * Test
   * @param args unused
   */
  public static void main(String[] args) {


    // Set the default locale to en-US locale (for Numerical Fields "." ",")
    Locale.setDefault(Locale.US);

    // Set the default timezone to GMT to handle properly the date in UTC :
    TimeZone.setDefault(TimeZone.getTimeZone("GMT"));

    // Preload configuration:
    ConfigurationManager.getInstance();

    // LST:
    // VLTI tests:
    test("VLTI Period 88", true, false, false);
    test("VLTI Period 88", true, false, true);
    test("VLTI Period 88", true, true, false);
    test("VLTI Period 88", true, true, true);

    // CHARA tests:
    test("CHARA", true, false, false);
    test("CHARA", true, false, true);
    test("CHARA", true, true, false);
    test("CHARA", true, true, true);

    // UTC:
    // VLTI tests:
    test("VLTI Period 88", false, false, false);
    test("VLTI Period 88", false, false, true);
    test("VLTI Period 88", false, true, false);
    test("VLTI Period 88", false, true, true);

    // CHARA tests:
    test("CHARA", false, false, false);
    test("CHARA", false, false, true);
    test("CHARA", false, true, false);
    test("CHARA", false, true, true);
  }

  /**
   * Test JD/LST or UTC date conversions
   * @param interferometerConfiguration interferometer configuration in "CHARA" / "VLTI Period 88"
   * @param useLst indicates if the timestamps are expressed in LST or in UTC
   * @param useNightLimit flag to enable the observability restriction due to the night
   * @param doCenterMidnight flag to center the plot arround midnight
   */
  public static void test(final String interferometerConfiguration, final boolean useLst,
          final boolean useNightLimit, final boolean doCenterMidnight) {

    logger.info("test[interferometer=" + interferometerConfiguration
            + ", useLst=" + useLst
            + ", useNightLimit=" + useNightLimit
            + ", doCenterMidnight=" + doCenterMidnight
            + "] start");

    final ConfigurationManager cm = ConfigurationManager.getInstance();

//    final InterferometerConfiguration intConf = cm.getInterferometerConfiguration("VLTI Period 87");
    final InterferometerConfiguration intConf = cm.getInterferometerConfiguration(interferometerConfiguration);

    final InterferometerDescription interferometer = intConf.getInterferometer();


    /** sky calc instance */
    final AstroSkyCalc sc = new AstroSkyCalc();

    // define site :
    sc.defineSite(interferometer.getName(), interferometer.getPosSph(), "GMT");

    // day expressed in milliseconds (depends on LST or JD day length) :
    final double dayLength = ((useLst) ? 1d : AstroSkyCalc.LST_DAY_IN_JD) * 24d * 3600d * 1000d;

    // 2 seconds of error (2 times rounding error on second field):
    final double dayLenError = 2 * 1000d;

//    logger.error("dayLength = " + dayLength);

    // HA range [-12;12]:
    final Range haRangeLimits = new Range(AsproConstants.HA_MIN, AsproConstants.HA_MAX);
    Range jdRange;
    final List<DateTimeInterval> visible = new ArrayList<DateTimeInterval>();
    DateTimeInterval interval;

    /*
    final int year = 1980;
    final int month = 1;
    final int day = 1;
     */
    final int year = 1980;
    final int month = 1;
    final int day = 1;

    final int lastYear = 2050;

    final Calendar cal = new GregorianCalendar(year, month - 1, day);
    cal.setLenient(false);

    double jdLower, jdUpper, jdMidnight;
    Date dateMin, dateMax;
    double len;
    
    final ObservabilityContext ctx = new ObservabilityContext(15); // 6T

    try {
      do {

//      logger.error("---------------------------------------\ndate = " + cal.getTime());

        // find the julian date corresponding to the LST origin LST=00:00:00 for the given date :
        jdLower = sc.defineDate(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH) + 1, cal.get(Calendar.DAY_OF_MONTH));

        // find the julian date corresponding to LST=00:00:00 next day:
        jdUpper = sc.findJdForLst0(jdLower + AstroSkyCalc.LST_DAY_IN_JD);

        final TimeRef timeRef = (useLst) ? TimeRef.LST : TimeRef.UTC;
        dateMin = sc.toDate(jdLower, timeRef);
        dateMax = sc.toDate(jdUpper, timeRef);

        if (dateMax.before(dateMin)) {
          throw new RuntimeException("upper[" + dateMax + "] before lower[" + dateMin + "]");
        }
        len = dateMax.getTime() - dateMin.getTime();

        // 1s error:
        if (!NumberUtils.equals(len, dayLength, dayLenError)) {
          throw new RuntimeException("Invalid date length : upper[" + dateMax + "] - lower[" + dateMin + "] = " + len);
        }

        /*
        // find the julian date corresponding to LST=00:00:00 previous day:
        final double jdLower1D = sc.findJdForLst0(jdLower - AstroSkyCalc.LST_DAY_IN_JD);
        
        final Date dateMin1D = sc.toDate(jdLower1D, useLst);
        
        logger.error("date min-1 = " + dateMin1D);
        
        len = dateMin.getTime() - dateMin1D.getTime();
        
        // 1s error:
        if (!NumberUtils.equals(len, dayLength, dayLenError)) {
        throw new RuntimeException("Invalid date length : upper[" + dateMin + "] - lower[" + dateMin1D + "] = " + len);
        }
         */

        // 1 - Find the day / twlight / night zones :
        if (useNightLimit) {

          if (doCenterMidnight) {
            jdMidnight = sc.getJdMidnight();

//          logger.error("jdMidnight = " + jdMidnight);

            // adjust the jd bounds :
            jdLower = jdMidnight - AstroSkyCalc.HALF_LST_DAY_IN_JD;
            jdUpper = jdMidnight + AstroSkyCalc.HALF_LST_DAY_IN_JD;

//          logger.error("jdLower = " + jdLower);
//          logger.error("jdUpper = " + jdUpper);

            dateMin = sc.toDate(jdLower, timeRef);
            dateMax = sc.toDate(jdUpper, timeRef);

//          logger.error("date min = " + dateMin);
//          logger.error("date max = " + dateMax);

            if (dateMax.before(dateMin)) {
              throw new RuntimeException("upper[" + dateMax + "] before lower[" + dateMin + "]");
            }
            len = dateMax.getTime() - dateMin.getTime();

            // 1s error:
            if (!NumberUtils.equals(len, dayLength, dayLenError)) {
              throw new RuntimeException("Invalid date length : upper[" + dateMax + "] - lower[" + dateMin + "] = " + len);
            }
          }
        }

        // Test fake targets :
        // fake target at RA=00:00:
        jdRange = sc.convertHAToJDRange(haRangeLimits, 0d, ctx);

        visible.clear();
        convertRangeToDateInterval(jdRange, visible, dateMin, dateMax, jdLower, jdUpper, useLst, sc);

        if (visible.size() > 1) {
          // merge contiguous date ranges :
          DateTimeInterval.merge(visible);
        }

        // checks:
        if (visible.size() != 1) {
          throw new RuntimeException("Invalid date intervals (size) : " + visible);
        }
        interval = visible.get(0);

        if (!interval.getStartDate().equals(dateMin)) {
          throw new RuntimeException("Invalid start date : " + interval.getStartDate() + " <> " + dateMin);
        }
        if (!interval.getEndDate().equals(dateMax)) {
          throw new RuntimeException("Invalid end   date : " + interval.getEndDate() + " <> " + dateMax);
        }

        // fake target at RA=12:00:
        jdRange = sc.convertHAToJDRange(haRangeLimits, 12d, ctx);

        visible.clear();
        convertRangeToDateInterval(jdRange, visible, dateMin, dateMax, jdLower, jdUpper, useLst, sc);

        if (visible.size() > 1) {
          // merge contiguous date ranges :
          DateTimeInterval.merge(visible);
        }

        // checks:
        if (visible.size() != 1) {
          throw new RuntimeException("Invalid date intervals (size) : " + visible);
        }
        interval = visible.get(0);

        if (!interval.getStartDate().equals(dateMin)) {
          throw new RuntimeException("Invalid start date : " + interval.getStartDate() + " <> " + dateMin);
        }
        if (!interval.getEndDate().equals(dateMax)) {
          throw new RuntimeException("Invalid end   date : " + interval.getEndDate() + " <> " + dateMax);
        }

        // fake target at RA=24:00:
        jdRange = sc.convertHAToJDRange(haRangeLimits, 24d, ctx);

        visible.clear();
        convertRangeToDateInterval(jdRange, visible, dateMin, dateMax, jdLower, jdUpper, useLst, sc);

        if (visible.size() > 1) {
          // merge contiguous date ranges :
          DateTimeInterval.merge(visible);
        }

        // checks:
        if (visible.size() != 1) {
          throw new RuntimeException("Invalid date intervals (size) : " + visible);
        }
        interval = visible.get(0);

        if (!interval.getStartDate().equals(dateMin)) {
          throw new RuntimeException("Invalid start date : " + interval.getStartDate() + " <> " + dateMin);
        }
        if (!interval.getEndDate().equals(dateMax)) {
          throw new RuntimeException("Invalid end   date : " + interval.getEndDate() + " <> " + dateMax);
        }


        // next day:
        cal.add(Calendar.DAY_OF_MONTH, 1);

      } while (cal.get(Calendar.YEAR) < lastYear);

    } catch (RuntimeException re) {
      logger.error("test[interferometer=" + interferometerConfiguration
              + ", useLst=" + useLst
              + ", useNightLimit=" + useNightLimit
              + ", doCenterMidnight=" + doCenterMidnight
              + "] failed for date : " + cal.getTime(), re);

      System.exit(1);
    }
  }

  /**
   * Convert a JD range to a date interval with respect for the LST range [0;24]
   * Note : due to HA limit [+/-12h], the converted JD / Date ranges
   * can have a discontinuity on the date axis !
   *
   * @param rangeJD range to convert
   * @param intervals interval list where new date intervals will be added
   */
  private static void convertRangeToDateInterval(final Range rangeJD, final List<DateTimeInterval> intervals,
          /* missing members */
          final Date dateMin, final Date dateMax,
          final double jdLower, final double jdUpper,
          final boolean useLst, final AstroSkyCalc sc) {
    final double jdStart = rangeJD.getMin();
    final double jdEnd = rangeJD.getMax();

    if (jdStart >= jdLower) {

      if (jdEnd <= jdUpper) {

        // single interval [jdStart;jdEnd]
        intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart, dateMin, dateMax, jdLower, jdUpper, useLst, sc),
                jdToDateInDateRange(jdEnd, dateMin, dateMax, jdLower, jdUpper, useLst, sc)));


      } else {

        if (jdStart > jdUpper) {
          // two points over LST 24 :

          // single interval [jdStart - day;jdEnd - day]
          intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart - AstroSkyCalc.LST_DAY_IN_JD, dateMin, dateMax, jdLower, jdUpper, useLst, sc),
                  jdToDateInDateRange(jdEnd - AstroSkyCalc.LST_DAY_IN_JD, dateMin, dateMax, jdLower, jdUpper, useLst, sc)));

        } else {
          // end occurs after LST 24 :

          // interval [jdStart;jdLst24]
          intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart, dateMin, dateMax, jdLower, jdUpper, useLst, sc),
                  dateMax));

          // add the second interval [jdLst0;jdEnd - day]
          intervals.add(new DateTimeInterval(dateMin,
                  jdToDateInDateRange(jdEnd - AstroSkyCalc.LST_DAY_IN_JD, dateMin, dateMax, jdLower, jdUpper, useLst, sc)));
        }
      }

    } else {
      // start occurs before LST 0h :

      if (jdEnd < jdLower) {
        // two points before LST 0h :

        // single interval [jdStart + day;jdEnd + day]
        intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart + AstroSkyCalc.LST_DAY_IN_JD, dateMin, dateMax, jdLower, jdUpper, useLst, sc),
                jdToDateInDateRange(jdEnd + AstroSkyCalc.LST_DAY_IN_JD, dateMin, dateMax, jdLower, jdUpper, useLst, sc)));

      } else {
        // interval [jdLst0;jdEnd]
        intervals.add(new DateTimeInterval(dateMin,
                jdToDateInDateRange(jdEnd, dateMin, dateMax, jdLower, jdUpper, useLst, sc)));

        // add the second interval [jdStart + day;jdLst24]
        intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart + AstroSkyCalc.LST_DAY_IN_JD, dateMin, dateMax, jdLower, jdUpper, useLst, sc),
                dateMax));
      }
    }
  }

  /**
   * Convert a JD value to a Date Object (LST or UTC)
   * within range [jdLst0;jdLst24]]<=>[DateMin;DateMax]
   * @see #useLST
   * @see #jdToDate(double)
   * @param jd julian day
   * @return Date Object (LST or UTC)
   */
  private static final Date jdToDateInDateRange(final double jd,
          /* missing members */
          final Date dateMin, final Date dateMax,
          final double jdLower, final double jdUpper,
          final boolean useLst, final AstroSkyCalc sc) {
    // adjust range limits :
    if (jd <= jdLower) {
      return dateMin;
    } else {
      if (jd >= jdUpper) {
        return dateMax;
      }
      return jdToDate(jd, useLst, sc);
    }
  }

  /**
   * Convert a JD value to a Date Object (LST or UTC)
   * @see #useLST
   * @param jd julian day
   * @return Date Object (LST or UTC)
   */
  private static Date jdToDate(final double jd,
          /* missing members */
          final boolean useLst, final AstroSkyCalc sc) {
    return sc.toDate(jd, (useLst) ? TimeRef.LST : TimeRef.UTC);
  }
}
