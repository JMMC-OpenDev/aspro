/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityService.java,v 1.4 2009-10-30 16:25:42 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.3  2009/10/27 16:47:17  bourgesl
 * fixed bug on month conversion
 *
 * Revision 1.2  2009/10/22 15:51:18  bourgesl
 * added comments on targets
 *
 * Revision 1.1  2009/10/22 15:47:22  bourgesl
 * beginning of observability computation with jSkyCalc
 *
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.service;

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import java.util.logging.Level;
import javax.xml.datatype.XMLGregorianCalendar;

/**
 * This service determines the observability of a list of targets given an observation setting
 *
 * @author bourgesl
 */
public class ObservabilityService {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.service.ObservabilityService";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /**
   * Main operation
   *
   * @param observation
   * @return undefined
   */
  public static Object calcObservability(final ObservationSetting observation) {
    logger.severe("start : " + observation);

    long start = System.nanoTime();

    try {

    final AstroSkyCalc sc = new AstroSkyCalc();

    final InterferometerConfiguration ic = observation.getInterferometerConfiguration().getInterferometerConfiguration();

    sc.defineSite(ic.getName(), ic.getInterferometer().getPosSph());

    final XMLGregorianCalendar cal = observation.getWhen().getDate();

    double jdMin, jdMax;
/*
    for (int i = 1; i <=12; i++) {

      logger.severe("test month : " + i);

      sc.defineDate(cal.getYear(), i, cal.getDay());

      jdLst0 = sc.findLst0();
    }
*/
    sc.defineDate(cal.getYear(), cal.getMonth(), cal.getDay());

    // 0 - Trouver l'origine LST=0 par rapport a la date donnee :
    jdMin = sc.findLst0();
    jdMax = jdMin + 1d;

    // 10 minutes :
    final double jdStep = (1d/6d) / 24d;

    // 1 - Trouver la nuit : sun rise/set with twilight : see NightlyAlmanac
    // TODO : convert in standard timestep data :
    sc.findSunRiseSet();


    // indicateur pour cette date mais ne pas se limiter a la nuit et utiliser le domaine LST [0;24h]

    // 2 - Pour chaque source, etudier sa progression en altitude (degrees) pour voir ensuite si cet angle est possible avec les telescopes ...


    if (observation.getTargets().isEmpty()) {
      logger.fine("No target defined !");
    } else {
      double jd;

      for (Target target : observation.getTargets()) {
        sc.defineTarget(target.getRA(), target.getDEC());

        jd = jdMin;

        while (jd < jdMax) {

          sc.getTargetPosition(jd);

          //
          jd += jdStep;

        }

      }
    }



    } catch (RuntimeException re) {
      logger.log(Level.SEVERE, "calcObservability failure :", re);
    }

    logger.severe("calcObservability : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");

    return null;
  }
}
