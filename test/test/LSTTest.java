/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: LSTTest.java,v 1.2 2011-04-22 15:33:34 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/10/01 15:21:09  bourgesl
 * new test case for find LST0 bug (date lower > date upper)
 *
 */
package test;

import edu.dartmouth.AstroAlmanac;
import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.util.NumberUtils;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;

/**
 * This class tests the AstroSkyCalc class to locate LST [0;24] range for every day between 1/1/1980 and 31/12/2049
 * @author bourgesl
 */
public final class LSTTest {

  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(LSTTest.class.getName());

  /**
   * Private constructor
   */
  private LSTTest() {
    super();
  }

  public static final void main(String[] args) {

    // interferometer configuration in "CHARA" / "VLTI Period 88" :
    final String interferometerConfiguration = /* "CHARA" */ /* */ "VLTI Period 88";

    // indicates if the timestamps are expressed in LST or in UTC:
    final boolean useLst = true;

    // flag to enable the observability restriction due to the night:
    final boolean useNightLimit = true;

    // flag to center the plot arround midnight:
    final boolean doCenterMidnight = true;




    // Set the default locale to en-US locale (for Numerical Fields "." ",")
    Locale.setDefault(Locale.US);

    // Set the default timezone to GMT to handle properly the date in UTC :
    TimeZone.setDefault(TimeZone.getTimeZone("GMT"));

    final ConfigurationManager cm = ConfigurationManager.getInstance();

//    final InterferometerConfiguration intConf = cm.getInterferometerConfiguration("VLTI Period 87");
    final InterferometerConfiguration intConf = cm.getInterferometerConfiguration(interferometerConfiguration);

    final InterferometerDescription interferometer = intConf.getInterferometer();


    /** sky calc instance */
    final AstroSkyCalc sc = new AstroSkyCalc();

    // define site :
    sc.defineSite(interferometer.getName(), interferometer.getPosSph());

    // day expressed in milliseconds (depends on LST or JD day length) :
    final double dayLength = ((useLst) ? 1d : AstroSkyCalc.LST_DAY_IN_JD) * 24d * 3600d * 1000d;

    // 2 seconds of error (2 times rounding error on second field):
    final double dayLenError = 2 * 1000d;

//    logger.severe("dayLength = " + dayLength);

    /*
    final int year = 1980;
    final int month = 10;
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
    AstroAlmanac almanac;
    double len;

    do {

//      logger.severe("---------------------------------------\ndate = " + cal.getTime());

      // find the julian date corresponding to the LST origin LST=00:00:00 for the given date :
      jdLower = sc.defineDate(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH) + 1, cal.get(Calendar.DAY_OF_MONTH));

      // check jdLst0 > jdUt12:
      len = sc.getJdForLst0() - sc.getJdLt12();
      if (len < 0d) {
        throw new RuntimeException("jdLst0 [" + sc.getJdForLst0() + "]  is before jdLt12[" + sc.getJdLt12() + "]");
      }
      if (len > 1d) {
        throw new RuntimeException("jdLst0 is next day [" + sc.getJdForLst0() + "]  - jdLt12[" + sc.getJdLt12() + "]");
      }

      // find the julian date corresponding to LST=00:00:00 next day:
      jdUpper = sc.findJdForLst0(jdLower + AstroSkyCalc.LST_DAY_IN_JD);

      dateMin = sc.toDate(jdLower, useLst);
      dateMax = sc.toDate(jdUpper, useLst);

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

      logger.severe("date min-1 = " + dateMin1D);

      len = dateMin.getTime() - dateMin1D.getTime();

      // 1s error:
      if (!NumberUtils.equals(len, dayLength, dayLenError)) {
      throw new RuntimeException("Invalid date length : upper[" + dateMin + "] - lower[" + dateMin1D + "] = " + len);
      }
       */

      // 1 - Find the day / twlight / night zones :
      if (useNightLimit) {
        //  sun rise/set with twilight : see NightlyAlmanac
        almanac = sc.getAlmanac();

        if (doCenterMidnight) {
          jdMidnight = sc.findMidnight(almanac);

//          logger.severe("jdMidnight = " + jdMidnight);

          // check jdLst0 > jdUt12:
          if (jdMidnight < sc.getJdLt12()) {
            throw new RuntimeException("jdMidnight [" + jdMidnight + "]  is before jdLt12[" + sc.getJdLt12() + "]");
          }

          // adjust the jd bounds :
          jdLower = jdMidnight - AstroSkyCalc.lst2jd(12d);
          jdUpper = jdMidnight + AstroSkyCalc.lst2jd(12d);

//          logger.severe("jdLower = " + jdLower);
//          logger.severe("jdUpper = " + jdUpper);

          dateMin = sc.toDate(jdLower, useLst);
          dateMax = sc.toDate(jdUpper, useLst);

//          logger.severe("date min = " + dateMin);
//          logger.severe("date max = " + dateMax);

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


      // next day:
      cal.add(Calendar.DAY_OF_MONTH, 1);

    } while (cal.get(Calendar.YEAR) < lastYear);

  }
}
