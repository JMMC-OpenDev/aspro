/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityService.java,v 1.2 2009-10-22 15:51:18 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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

    try {

    final AstroSkyCalc sc = new AstroSkyCalc();

    final InterferometerConfiguration ic = observation.getInterferometerConfiguration().getInterferometerConfiguration();

    sc.defineSite(ic.getName(), ic.getInterferometer().getPosSph());

    final XMLGregorianCalendar cal = observation.getWhen().getDate();
    sc.defineDate(cal.getYear(), cal.getMonth(), cal.getDay());

    // 1 - Trouver la nuit : sun rise/set with twilight : see NightlyAlmanac

    // 2 - Pour chaque source, etudier sa progression en altitude (degrees) pour voir ensuite si cet angle est possible avec les telescopes ...



    } catch (RuntimeException re) {
      logger.log(Level.SEVERE, "calcObservability failure :", re);
    }

    return null;
  }
}
