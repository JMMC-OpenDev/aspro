/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: LSTTest.java,v 1.1 2010-10-01 15:21:09 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package test;

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import java.util.Calendar;
import java.util.Date;

/**
 *
 * @author bourgesl
 */
public class LSTTest {

  public static final void main(String[] args) {

    final ConfigurationManager cm = ConfigurationManager.getInstance();

//    final InterferometerConfiguration intConf = cm.getInterferometerConfiguration("VLTI Period 87");
    final InterferometerConfiguration intConf = cm.getInterferometerConfiguration("CHARA");

    final InterferometerDescription interferometer = intConf.getInterferometer();


    /** sky calc instance */
    final AstroSkyCalc sc = new AstroSkyCalc();

    // define site :
    sc.defineSite(interferometer.getName(), interferometer.getPosSph());


    final Calendar cal = Calendar.getInstance();
    cal.set(1980, 1, 1);
    cal.setLenient(false);

    do {

      System.out.println("date = " + cal.getTime());

      // find the julian date corresponding to the LST origin LST=00:00:00 for the given date :
      final double jdLst0 = sc.defineDate(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH));

      // warning : in LST, remove 1s to avoid 00:00:00 :
      final double jdLst24 = sc.findJdForLst0(jdLst0 + 1d) - 1d / 86400d;

      final Date dateMin = sc.toDate(jdLst0, true);
      final Date dateMax = sc.toDate(jdLst24, true);

      final double len = dateMax.getTime() - dateMin.getTime();

      if (dateMax.before(dateMin)) {
        throw new RuntimeException("upper["+dateMax+"] before lower["+dateMin+"]");
      }
      if (len < 8.63E7) {
        throw new RuntimeException("Invalid date length : upper["+dateMax+"] - lower["+dateMin+"]");
      }

      cal.add(Calendar.DAY_OF_MONTH, 1);

    } while (cal.get(Calendar.YEAR) < 2050);

  }

}
