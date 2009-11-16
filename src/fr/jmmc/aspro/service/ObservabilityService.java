/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityService.java,v 1.7 2009-11-16 14:47:46 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.6  2009/11/05 12:59:39  bourgesl
 * first simple source observability (only min elevation condition)
 *
 * Revision 1.5  2009/11/03 16:57:56  bourgesl
 * added observability plot with LST/UTC support containing only day/night/twilight zones
 *
 * Revision 1.4  2009/10/30 16:25:42  bourgesl
 * added sun twilight times + get target position
 *
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
import edu.dartmouth.SunAlmanachTime;
import fr.jmmc.aspro.model.DateTimeInterval;
import fr.jmmc.aspro.model.ObservabilityData;
import fr.jmmc.aspro.model.StarObservability;
import fr.jmmc.aspro.model.SunTimeInterval;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
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
   * Main operation to determine the source observability for a given interferometer configuration
   *
   * @param observation observation settings
   * @param useLst true indicates to return date/time values in LST, false to use UTC reference
   * @param minElev minimum elevation
   * @return ObservabilityData container
   */
  public static ObservabilityData calcObservability(final ObservationSetting observation, final boolean useLST, final double minElev) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("start : " + observation);
    }
    // Get the current thread to check if the computation is interrupted :
    final Thread currentThread = Thread.currentThread();

    final ObservabilityData data = new ObservabilityData();

    final long start = System.nanoTime();

    try {

      final AstroSkyCalc sc = new AstroSkyCalc();

      final InterferometerConfiguration ic = observation.getInterferometerConfiguration().getInterferometerConfiguration();

      sc.defineSite(ic.getName(), ic.getInterferometer().getPosSph());

      // Get chosen stations / switchyard / 



      final XMLGregorianCalendar cal = observation.getWhen().getDate();

      sc.defineDate(cal.getYear(), cal.getMonth(), cal.getDay());

      // fast interrupt :
      if (currentThread.isInterrupted()) {
        return null;
      }

      // 0 - Trouver l'origine LST=0 par rapport a la date donnee :
      final double jdMin = sc.findLst0();
      // warning : in LST, remove 1s to avoid 00:00:00 :
      final double jdMax = sc.findLst0(jdMin + 1d) - 1d / 86400d;

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("jd   min = " + jdMin);
        logger.fine("jd   max = " + jdMax);
      }

      data.setDateMin(sc.toDate(jdMin, useLST));
      data.setDateMax(sc.toDate(jdMax, useLST));

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("date min = " + data.getDateMin());
        logger.fine("date max = " + data.getDateMax());
      }

      // fast interrupt :
      if (currentThread.isInterrupted()) {
        return null;
      }

      // 1 - Trouver la nuit : sun rise/set with twilight : see NightlyAlmanac
      // indicateur pour cette date mais ne pas se limiter a la nuit et utiliser le domaine LST [0;24h]

      final List<SunAlmanachTime> sunEvents = sc.findSunRiseSet(jdMin, jdMax);
      data.setSunIntervals(convertSunAlmanach(sc, sunEvents, useLST));

      // fast interrupt :
      if (currentThread.isInterrupted()) {
        return null;
      }

      // 2 - Pour chaque source, etudier sa progression en altitude (degrees) pour voir ensuite si cet angle est possible avec les telescopes ...

      if (observation.getTargets().isEmpty()) {
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("No target defined.");
        }
      } else {

        final double minElevRad = Math.toRadians(minElev);

        final List<StarObservability> starVis = data.getStarVisibilities();

        StarObservability starObs;
        List<DateTimeInterval> intervals;

/*
        DateTimeInterval interval;

        AzAlt azAlt;
        boolean last = false;
        boolean overHorizon = false;
        boolean visible = false;

        double jdi1 = 0;
        double jdi2 = 0;

        double jd;

        // 1 minutes :
        final double jdStep = (1d / 3600d) / 24d;
*/

        for (Target target : observation.getTargets()) {

          sc.defineTarget(target.getRA(), target.getDEC());

          starObs = new StarObservability(target.getName());
          starVis.add(starObs);

          intervals = starObs.getVisible();

          // fast interrupt :
          if (currentThread.isInterrupted()) {
            return null;
          }

          findTargetObservability(sc, intervals, minElevRad, jdMin, jdMax, useLST);
/*
          jd = jdMin;

          last = false;
          overHorizon = false;
          visible = false;
          interval = new DateTimeInterval();

          while (jd < jdMax) {

            // fast interrupt :
            if (currentThread.isInterrupted()) {
              return null;
            }

            azAlt = sc.getTargetPosition(jd);

            overHorizon = azAlt.getAltitude() > minElevRad;

            if (overHorizon) {
              // Check Shadowing for every stations
              // Base lines : vector
              // Check delay lines / switchyard / pops ?

              visible = true;
            } else {
              visible = false;
            }

            // manage intervals :
            if (visible) {
              if (!last) {
                last = true;
                // start point
                jdi1 = jd;
                interval.setStartDate(sc.toDate(jd, useLST));
              }
            } else {
              if (last) {
                last = false;
                // end point
                jdi2 = jd;

                logger.severe("interval center LST = " + sc.toDate(jdi1 + (jdi2 - jdi1) / 2d, true));

                interval.setEndDate(sc.toDate(jd, useLST));
                intervals.add(interval);
                interval = new DateTimeInterval();
              }
            }

            //
            jd += jdStep;
          }

          // close last interval if open :
          if (interval.getStartDate() != null) {
            interval.setEndDate(sc.toDate(jdMax, useLST));
            intervals.add(interval);
          }

          */
        } // for Target

        // dump star visibilities :
        if (logger.isLoggable(Level.INFO)) {
          logger.info("star visibilities : ");

          for (StarObservability so : starVis) {
            logger.info(so.toString());
          }
        }

      }

    } catch (RuntimeException re) {
      logger.log(Level.SEVERE, "calcObservability failure :", re);
    }

    if (logger.isLoggable(Level.INFO)) {
      logger.info("calcObservability : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");
    }

    return data;
  }

  private static List<SunTimeInterval> convertSunAlmanach(final AstroSkyCalc sc, final List<SunAlmanachTime> sunEvents, final boolean useLST) {
    List<SunTimeInterval> result = null;

    if (sunEvents != null && !sunEvents.isEmpty()) {
      final int nbInterval = sunEvents.size() - 1;
      result = new ArrayList<SunTimeInterval>(nbInterval);

      SunAlmanachTime stFrom, stTo;
      Date from, to;
      SunTimeInterval.SunType type;

      for (int i = 0; i < nbInterval; i++) {
        stFrom = sunEvents.get(i);
        stTo = sunEvents.get(i + 1);

        from = sc.toDate(stFrom.getJd(), useLST);
        to = sc.toDate(stTo.getJd(), useLST);

        switch (stFrom.getType()) {
          case SunRise:
            type = SunTimeInterval.SunType.Day;
            break;
          case SunSet:
            type = SunTimeInterval.SunType.Twilight;
            break;
          default:
          case SunTwlRise:
            type = SunTimeInterval.SunType.Twilight;
            break;
          case SunTwlSet:
            type = SunTimeInterval.SunType.Night;
        }

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("SunInterval[" + from + " - " + to + "] : " + type);
        }

        result.add(new SunTimeInterval(from, to, type));
      }
    }

    return result;
  }

  private static void findTargetObservability(final AstroSkyCalc sc, final List<DateTimeInterval> intervals,
          final double minElevRad, final double jdMin, final double jdMax, final boolean useLST) {

    DateTimeInterval interval;

    /*
     * Find lst interval corresponding to the rise / set of the target :
     */
    final double[] jdInterval = sc.getTimeIntervalForAltitude(jdMin, minElevRad);
    if (jdInterval != null) {
      final double jdRise = jdInterval[0];
      final double jdSet = jdInterval[1];

      if (jdRise > jdMin) {

        if (jdSet < jdMax) {

          // single interval [jdRise;jdSet]
          interval = new DateTimeInterval();
          interval.setStartDate(sc.toDate(jdRise, useLST));
          interval.setEndDate(sc.toDate(jdSet, useLST));
          intervals.add(interval);

        } else {

          // interval [jdRise;jdMax]
          interval = new DateTimeInterval();
          interval.setStartDate(sc.toDate(jdRise, useLST));
          interval.setEndDate(sc.toDate(jdMax, useLST));
          intervals.add(interval);

          // add the second interval [jdMin;jdSet - 1]

          interval = new DateTimeInterval();
          interval.setStartDate(sc.toDate(jdMin, useLST));
          interval.setEndDate(sc.toDate(jdSet - 1d, useLST));
          intervals.add(interval);
        }

      } else {
        // rise occurs before jd0 :

        // interval [jdMin;jdSet]
        interval = new DateTimeInterval();
        interval.setStartDate(sc.toDate(jdMin, useLST));
        interval.setEndDate(sc.toDate(jdSet, useLST));
        intervals.add(interval);

        // add the second interval [jdRise + 1;jdMax]

        interval = new DateTimeInterval();
        interval.setStartDate(sc.toDate(jdRise + 1d, useLST));
        interval.setEndDate(sc.toDate(jdMax, useLST));
        intervals.add(interval);
      }
    }

  }
}
