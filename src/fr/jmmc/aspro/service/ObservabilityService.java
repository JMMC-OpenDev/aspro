/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityService.java,v 1.37 2010-02-08 17:00:35 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.36  2010/01/22 13:16:44  bourgesl
 * added star observability type to change bar colors easily
 *
 * Revision 1.35  2010/01/20 16:18:38  bourgesl
 * observation form refactoring
 *
 * Revision 1.34  2010/01/20 09:56:13  bourgesl
 * removed Pop identifiers with the target name
 * added log of the best PoPs collection for a given list of targets
 *
 * Revision 1.33  2010/01/15 13:51:27  bourgesl
 * illegalStateException if any transient field is undefined
 *
 * Revision 1.32  2010/01/12 16:54:19  bourgesl
 * added PoPs in title + several changes on charts
 *
 * Revision 1.31  2010/01/08 16:51:17  bourgesl
 * initial uv coverage
 *
 * Revision 1.30  2010/01/04 16:57:00  bourgesl
 * modified best PoPs algorithm to take into account the night limits
 *
 * Revision 1.29  2009/12/18 14:49:31  bourgesl
 * fixed NPE
 *
 * Revision 1.28  2009/12/18 11:52:02  bourgesl
 * added Pops Finder Compatible Mode for a list of targets
 *
 * Revision 1.27  2009/12/16 16:47:24  bourgesl
 * comments
 *
 * Revision 1.26  2009/12/16 16:05:51  bourgesl
 * refactoring
 *
 * Revision 1.25  2009/12/16 08:44:40  bourgesl
 * fixed NPE if station fixed offset is undefined (VLTI)
 *
 * Revision 1.24  2009/12/15 16:35:26  bourgesl
 * user PoPs config + CHARA switchyard
 *
 * Revision 1.23  2009/12/11 16:37:32  bourgesl
 * added Pop field in observation form
 *
 * Revision 1.22  2009/12/11 15:15:42  bourgesl
 * log info the list of best PoP combinations
 *
 * Revision 1.21  2009/12/10 17:08:53  bourgesl
 * generate all pops combinations without any restrictions
 *
 * Revision 1.20  2009/12/08 14:54:18  bourgesl
 * fixed concurrentModification on target iteration
 *
 * Revision 1.19  2009/12/07 15:18:00  bourgesl
 * Load observation action now refreshes the observation form completely
 *
 * Revision 1.18  2009/12/02 17:23:51  bourgesl
 * fixed several bugs on pop finder + refactoring
 *
 * Revision 1.17  2009/12/01 17:14:45  bourgesl
 * first try to add the pop configuration finder
 *
 * Revision 1.16  2009/11/27 16:38:17  bourgesl
 * added minElev to GUI + fixed horizon profiles
 *
 * Revision 1.15  2009/11/27 10:13:19  bourgesl
 * fixed LST day/night intervals
 * fixed NPE on computation cancellation
 *
 * Revision 1.14  2009/11/26 17:04:11  bourgesl
 * added observability plots options (night/detail / UTC/LST)
 * added base line limits
 *
 * Revision 1.13  2009/11/25 17:14:32  bourgesl
 * fixed bugs on HA limits + merge JD intervals
 *
 * Revision 1.12  2009/11/24 17:27:12  bourgesl
 * first attempt to merge ranges
 *
 * Revision 1.11  2009/11/24 15:12:09  bourgesl
 * first step to handle delay line limits
 *
 * Revision 1.10  2009/11/23 16:49:17  bourgesl
 * added horizonService to check horizon profiles (VLTI)
 *
 * Revision 1.9  2009/11/20 16:55:47  bourgesl
 * Added Beam / Delay Line definition
 * ObservabilityService is stateless to simplify coding
 *
 * Revision 1.8  2009/11/17 17:00:28  bourgesl
 * chosen instrument configuration propagated to observation
 *
 * Revision 1.7  2009/11/16 14:47:46  bourgesl
 * determine the hour angle for a target over a min elevation to get the simple observability
 *
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
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.observability.DateTimeInterval;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.observability.PopCombination;
import fr.jmmc.aspro.model.observability.GroupedPopObservabilityData;
import fr.jmmc.aspro.model.observability.PopObservabilityData;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.observability.StarObservabilityData;
import fr.jmmc.aspro.model.observability.SunTimeInterval;
import fr.jmmc.aspro.model.oi.AzEl;
import fr.jmmc.aspro.model.oi.Channel;
import fr.jmmc.aspro.model.oi.ChannelLink;
import fr.jmmc.aspro.model.oi.DelayLine;
import fr.jmmc.aspro.model.oi.FocalInstrument;
import fr.jmmc.aspro.model.oi.FocalInstrumentConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Pop;
import fr.jmmc.aspro.model.oi.PopLink;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.StationLinks;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.service.HorizonService.Profile;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
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

  /* members */

  /* output */
  /** observability data */
  private ObservabilityData data = new ObservabilityData();

  /* inputs */
  /** observation settings */
  private final ObservationSetting observation;
  /** indicates if the timestamps are expressed in LST or in UTC */
  private final boolean useLST;
  /** flag to find baseline limits */
  private final boolean doBaseLineLimits;
  /** flag to produce detailed output with all BL / horizon / rise intervals per target */
  private final boolean doDetails;

  /* internal */
  /** Get the current thread to check if the computation is interrupted */
  private final Thread currentThread = Thread.currentThread();
  /** sky calc instance */
  private final AstroSkyCalc sc = new AstroSkyCalc();
  /** jd corresponding to LST=0 for the observation date */
  private double jdLst0;
  /** jd corresponding to LST=23:59:59 for the observation date */
  private double jdLst24;
  /** flag to enable the observability restriction due to the night */
  private boolean useNightLimit;
  /** Night ranges defined in julian day */
  private List<Range> nightLimits = null;
  /** minimum of elevation to observe any target (rad) */
  private double minElev;
  /** interferometer description */
  private InterferometerDescription interferometer = null;
  /** focal instrument description */
  private FocalInstrument instrument = null;
  /** flag to indicate that a station has an horizon profile */
  private boolean hasHorizon = false;
  /** flag to indicate that pops are used */
  private boolean hasPops = false;
  /** beam list */
  private List<Beam> beams = null;
  /** base line list */
  private List<BaseLine> baseLines = new ArrayList<BaseLine>();
  /** W ranges corresponding to the base line list */
  private List<Range> wRanges = new ArrayList<Range>();
  /** list of Pop combinations with pop delays per baseline */
  private List<PopCombination> popCombinations = null;

  /**
   * Constructor.
   * Note : This service is statefull so it can not be reused by several calls.
   *
   * @param observation observation settings
   * @param useNightLimit flag to enable the observability restriction due to the night
   * @param useLST indicates if the timestamps are expressed in LST or in UTC
   * @param doDetails flag to produce detailed output with all BL / horizon / rise intervals per target
   */
  public ObservabilityService(final ObservationSetting observation,
                              final boolean useLST, final boolean doDetails, final boolean doBaseLineLimits) {
    // Inputs :
    this.observation = observation;
    this.useLST = useLST;
    this.doDetails = doDetails;
    this.doBaseLineLimits = doBaseLineLimits;
  }

  /**
   * Main operation to determine the source observability for a given interferometer configuration
   *
   * @return ObservabilityData container
   */
  public ObservabilityData compute() {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("compute : " + this.observation);
    }

    // Start the computations :
    final long start = System.nanoTime();

    try {
      // Get interferometer / instrument :
      prepareObservation();

      // Define target list :
      final List<Target> targets;
      if (this.doBaseLineLimits) {
        targets = generateTargetsForBaseLineLimits();
      } else {
        // copy the list to avoid concurrent modification during iteration :
        targets = new ArrayList<Target>(this.observation.getTargets());
      }

      // define site :
      this.sc.defineSite(this.interferometer.getName(), this.interferometer.getPosSph());

      // define date :
      final XMLGregorianCalendar cal = this.observation.getWhen().getDate();
      this.sc.defineDate(cal.getYear(), cal.getMonth(), cal.getDay());

      // fast interrupt :
      if (this.currentThread.isInterrupted()) {
        return null;
      }

      // Find the julian date corresponding to the LST origin LST=0h for the given date :
      this.jdLst0 = this.sc.findLst0();
      // warning : in LST, remove 1s to avoid 00:00:00 :
      this.jdLst24 = this.sc.findLst0(this.jdLst0 + 1d) - 1d / 86400d;

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("jd   min = " + this.jdLst0);
        logger.fine("jd   max = " + this.jdLst24);
      }

      this.data.setDateMin(jdToDate(this.jdLst0));
      this.data.setDateMax(jdToDate(this.jdLst24));

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("date min = " + this.data.getDateMin());
        logger.fine("date max = " + this.data.getDateMax());
      }

      // fast interrupt :
      if (this.currentThread.isInterrupted()) {
        return null;
      }

      // 1 - Find the day / twlight / night zones :
      if (this.useNightLimit) {
        //  sun rise/set with twilight : see NightlyAlmanac

        // Use the LST range [0;24h] +- 12h to have valid night ranges to merge with target ranges :
        final List<SunAlmanachTime> sunEvents = this.sc.findSunRiseSet(this.jdLst0);

        processSunAlmanach(sunEvents);
      }

      // fast interrupt :
      if (this.currentThread.isInterrupted()) {
        return null;
      }

      // 2 - Observability per target :

      if (targets == null || targets.isEmpty()) {
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("No target defined.");
        }
      } else {

        // Prepare the beams (station / channel / delay line) :
        prepareBeams();

        // Prepare the base line (XYZ vector, wRange) :
        prepareBaseLines();

        // Prepare the pops :
        if (this.hasPops) {
          preparePopCombinations();
        }

        // Find the observability intervals for the target list :
        findObservability(targets);

        // fast interrupt :
        if (this.currentThread.isInterrupted()) {
          return null;
        }

        // dump star visibilities :
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("Star observability intervals : ");

          for (StarObservabilityData so : this.data.getStarVisibilities()) {
            logger.fine(so.toString());
          }
        }

      }

    } catch (IllegalStateException ise) {
      if (logger.isLoggable(Level.WARNING)) {
        logger.log(Level.WARNING, "invalid observation :", ise);
        logger.log(Level.WARNING, "observation : " + ObservationManager.toString(this.observation));
      }
      // clear invalid data :
      this.data = null;
    } catch (RuntimeException re) {
      if (logger.isLoggable(Level.SEVERE)) {
        logger.log(Level.SEVERE, "compute failure :", re);
        logger.log(Level.SEVERE, "observation : " + ObservationManager.toString(this.observation));
      }
      // clear invalid data :
      this.data = null;
    }

    if (logger.isLoggable(Level.INFO)) {
      logger.info("compute : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");
    }

    return this.data;
  }

  /**
   * Find the observability ranges for the complete list of targets
   * @param targets target list
   */
  private void findObservability(final List<Target> targets) {

    // PoPs : Compatible Mode if no user defined Pop Combination :
    if (this.hasPops && targets.size() > 1 && this.popCombinations.size() > 1) {
      // Objective : find the pop combination that maximize the observability of the complete list of target

      final PopCombination bestPopCombination = findCompatiblePoPs(targets);

      if (bestPopCombination != null) {
        this.data.setBestPops(bestPopCombination);

        // use the user defined PoPs configuration :
        this.popCombinations.clear();
        this.popCombinations.add(bestPopCombination);
      }
    }

    for (Target target : targets) {

      // fast interrupt :
      if (this.currentThread.isInterrupted()) {
        return;
      }

      findTargetObservability(target);

    } // for Target

  }

  private PopCombination findCompatiblePoPs(final List<Target> targets) {

    PopCombination bestPopCombination = null;

    final Map<String, GroupedPopObservabilityData> popMap = new HashMap<String, GroupedPopObservabilityData>();

    final int nTargets = targets.size();

    int nObsTarget = 0;

    // pop data per target
    List<PopObservabilityData> targetPopDataList = null;

    GroupedPopObservabilityData popMergeData = null;
    List<PopObservabilityData> flatPopDataList = null;
    String key;

    for (Target target : targets) {

      // fast interrupt :
      if (this.currentThread.isInterrupted()) {
        return null;
      }

      targetPopDataList = findPoPsForTargetObservability(target);

      // targetPopDataList can be empty if the target never rises or is incompatible (skip) :
      if (targetPopDataList != null && !targetPopDataList.isEmpty()) {
        nObsTarget++;
        // rearrange results :

        for (PopObservabilityData popData : targetPopDataList) {
          key = popData.getPopCombination().getIdentifier();

          popMergeData = popMap.get(key);

          if (popMergeData == null) {
            flatPopDataList = new ArrayList<PopObservabilityData>(nTargets);
            popMergeData = new GroupedPopObservabilityData(popData.getPopCombination(), flatPopDataList);
            popMap.put(key, popMergeData);
          } else {
            flatPopDataList = popMergeData.getPopDataList();
          }

          // add result :
          flatPopDataList.add(popData);
        }
      }

    } // for Target

    // Convert the map to a list :
    final List<GroupedPopObservabilityData> popMergeList = new ArrayList<GroupedPopObservabilityData>(popMap.values());

    popMap.clear();

    // merged results per PoP combination :
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Complete GroupedPopData : ");
      for (GroupedPopObservabilityData pm : popMergeList) {
        logger.fine(pm.toString());
      }
    }

    // find the maximum count of observable targets per PoPs combination :
    // This avoids to have no result at all if all targets can not be observed
    // with the same PoPs combination
    int maxObsTarget = 0;
    for (GroupedPopObservabilityData pm : popMergeList) {
      if (pm.getPopDataList().size() > maxObsTarget) {
        maxObsTarget = pm.getPopDataList().size();
      }
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Observable targets : max / total = " + maxObsTarget + " / " + nObsTarget);
    }

    // filter to keep only results for all valid targets :
    for (final Iterator<GroupedPopObservabilityData> it = popMergeList.iterator(); it.hasNext();) {
      popMergeData = it.next();
      if (popMergeData.getPopDataList().size() != maxObsTarget) {
        it.remove();
      }
    }

    if (popMergeList.isEmpty()) {
      if (logger.isLoggable(Level.INFO)) {
        logger.info("Impossible to find a PoPs combination compatible with all targets !");
      }
    } else {

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("Filtered GroupedPopData : ");
        for (GroupedPopObservabilityData pm : popMergeList) {
          logger.fine(pm.toString());
        }
      }

      // estimator to maximize observability for all observable targets :
      for (GroupedPopObservabilityData pm : popMergeList) {
        pm.estimateData();
      }

      // Sort pop merge data according to its estimator :
      Collections.sort(popMergeList);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("Sorted GroupedPopData with estimation : ");
        for (GroupedPopObservabilityData pm : popMergeList) {
          logger.fine(pm.toString());
        }
      }

      // maximum length for this Pop Combination :
      final int end = popMergeList.size() - 1;

      final GroupedPopObservabilityData popBestData = popMergeList.get(end);

      if (popMergeList.size() > 1) {
        GroupedPopObservabilityData pm;

        // find all equivalent pop combinations :
        final List<GroupedPopObservabilityData> bestPoPs = new ArrayList<GroupedPopObservabilityData>();
        for (int i = end; i >= 0; i--) {
          pm = popMergeList.get(i);
          if (popBestData.getEstimation() == pm.getEstimation()) {
            bestPoPs.add(pm);
          } else {
            break;
          }
        }
        if (logger.isLoggable(Level.INFO)) {
          logger.info("best PoPs : " + bestPoPs);
        }
      } else {
        if (logger.isLoggable(Level.INFO)) {
          logger.info("best PoPs : " + popBestData);
        }
      }

      bestPopCombination = popBestData.getPopCombination();
    }

    return bestPopCombination;
  }

  /**
   * Return the list of Pop Observability Data i.e. for every PoP combination,
   * give the intervals (hour angles) for all base lines compatible with wMin < w(h) < wMax,
   * wMin and wMax are given by wRanges.
   *
   * @param target target to use
   */
  private List<PopObservabilityData> findPoPsForTargetObservability(final Target target) {

    // Target coordinates precessed to jd and to get az/alt positions from JSkyCalc :
    final double[] raDec = this.sc.defineTarget(this.jdLst0, target.getRA(), target.getDEC());

    // precessed target right ascension in decimal hours :
    final double precRA = raDec[0];

    // precessed target declination in degrees :
    final double precDEC = raDec[1];

    // Find LST range corresponding to the rise / set of the target :
    final double haElev = this.sc.getHAForElevation(precDEC, this.minElev);

    // For all PoP combinations : find the HA interval merged with the HA Rise/set interval
    // list of observability data associated to a pop combination :
    List<PopObservabilityData> popDataList = null;

    // target rise :
    if (haElev > 0d) {
      // rise/set range :
      final Range rangeHARiseSet = new Range(-haElev, haElev);

      final List<Range> rangesTarget = new ArrayList<Range>(4);
      rangesTarget.add(rangeHARiseSet);

      if (this.useNightLimit) {
        final List<Range> haNightLimits = new ArrayList<Range>(2);

        Range rangeHA = null;
        for (Range rangeJD : this.nightLimits) {
          rangeHA = convertJDToHARange(rangeJD, precRA);
          if (rangeHA != null) {
            haNightLimits.add(rangeHA);
          }
        }

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("HA night limits = " + haNightLimits);
        }
        rangesTarget.addAll(haNightLimits);
      }

      popDataList = getPopObservabilityData(target.getName(), Math.toRadians(precDEC), rangesTarget);

    } else {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("Target never rise : " + target);
      }
    }
    return popDataList;
  }

  /**
   * Finds the observability ranges for the given target
   * @param target target to use
   */
  private void findTargetObservability(final Target target) {

    final StarObservabilityData starObs = new StarObservabilityData(target.getName(), StarObservabilityData.TYPE_STAR);
    // add the result to have also unobservable targets :
    this.data.getStarVisibilities().add(starObs);

    final StarData starData = new StarData(target.getName());
    this.data.addStarData(starData);

    // Target coordinates precessed to jd and to get az/alt positions from JSkyCalc :
    final double[] raDec = this.sc.defineTarget(this.jdLst0, target.getRA(), target.getDEC());

    // precessed target right ascension in decimal hours :
    final double precRA = raDec[0];

    // precessed target declination in degrees :
    final double precDEC = raDec[1];

    // Find LST range corresponding to the rise / set of the target :
    final double haElev = this.sc.getHAForElevation(precDEC, this.minElev);

    // update Star Data :
    starData.setPrecRA(precRA);
    starData.setPrecDEC(precDEC);
    starData.setHaElev(haElev);

    // target rise :
    if (haElev > 0d) {
      // rise/set range :
      final Range rangeHARiseSet = new Range(-haElev, haElev);

      // HA intervals for every base line :
      List<List<Range>> rangesHABaseLines;

      if (this.hasPops) {
        // handle here all possible combinations for POPs :
        // keep only the POPs that maximize the DL+rise intersection ...

        final List<Range> rangesTarget = new ArrayList<Range>(4);
        rangesTarget.add(rangeHARiseSet);

        if (this.useNightLimit) {
          final List<Range> haNightLimits = new ArrayList<Range>(2);

          Range rangeHA = null;
          for (Range rangeJD : this.nightLimits) {
            rangeHA = convertJDToHARange(rangeJD, precRA);
            if (rangeHA != null) {
              haNightLimits.add(rangeHA);
            }
          }

          if (logger.isLoggable(Level.FINE)) {
            logger.fine("HA night limits = " + haNightLimits);
          }
          rangesTarget.addAll(haNightLimits);
        }

        rangesHABaseLines = findHAIntervalsWithPops(Math.toRadians(precDEC), rangesTarget, starObs);

      } else {
        // Get intervals (HA) compatible with all base lines :
        rangesHABaseLines = DelayLineService.findHAIntervals(Math.toRadians(precDEC), this.baseLines, this.wRanges);
      }

      // rangesHABaseLines can be null if the thread was interrupted :
      // fast interrupt :
      if (this.currentThread.isInterrupted()) {
        return;
      }

      // convert HA range to JD range :
      final Range rangeJDRiseSet = convertHAToJDRange(rangeHARiseSet, precRA);

      // For now : only VLTI has horizon profiles :
      List<Range> rangesJDHz = null;
      if (this.hasHorizon) {
        // check horizon profiles inside rise/set range :
        rangesJDHz = checkHorizonProfile(rangeJDRiseSet);

        // fast interrupt :
        if (this.currentThread.isInterrupted()) {
          return;
        }
      }

      // observable ranges (jd) :
      final List<Range> obsRanges = new ArrayList<Range>();

      if (this.doDetails) {
        // Get the current target name :
        final String prefix = starObs.getName() + " ";

        // Add Rise/Set :
        final StarObservabilityData soRiseSet = new StarObservabilityData(target.getName() + " Rise/Set", StarObservabilityData.TYPE_RISE_SET);
        this.data.getStarVisibilities().add(soRiseSet);

        convertRangeToDateInterval(rangeJDRiseSet, soRiseSet.getVisible());

        if (rangesJDHz != null) {
          // Add Horizon :
          final StarObservabilityData soHz = new StarObservabilityData(target.getName() + " Horizon", StarObservabilityData.TYPE_HORIZON);
          this.data.getStarVisibilities().add(soHz);

          for (Range range : rangesJDHz) {
            convertRangeToDateInterval(range, soHz.getVisible());
          }
        }

        // Add ranges per BL :
        if (!rangesHABaseLines.isEmpty()) {
          BaseLine baseLine;
          List<Range> ranges;
          StarObservabilityData soBl;
          for (int i = 0, size = this.baseLines.size(); i < size; i++) {
            baseLine = this.baseLines.get(i);
            ranges = rangesHABaseLines.get(i);

            if (ranges != null) {
              for (Range range : ranges) {
                obsRanges.add(convertHAToJDRange(range, precRA));
              }
            }
            if (logger.isLoggable(Level.FINE)) {
              logger.fine("baseLine : " + baseLine);
              logger.fine("JD ranges  : " + obsRanges);
            }

            soBl = new StarObservabilityData(prefix + baseLine.getName(), StarObservabilityData.TYPE_BASE_LINE + i);
            this.data.getStarVisibilities().add(soBl);

            for (Range range : obsRanges) {
              convertRangeToDateInterval(range, soBl.getVisible());
            }

            if (logger.isLoggable(Level.FINE)) {
              logger.fine("Date ranges  : " + soBl.getVisible());
            }

            obsRanges.clear();
          }
        }
      }

      // Merge then all JD intervals :

      // nValid = nBL [dl] + 1 [rise or horizon] + 1 if night limits
      int nValid = this.baseLines.size() + 1;

      // flatten and convert HA ranges to JD range :
      for (List<Range> ranges : rangesHABaseLines) {
        if (ranges != null) {
          for (Range range : ranges) {
            obsRanges.add(convertHAToJDRange(range, precRA));
          }
        }
      }

      if (rangesJDHz != null) {
        obsRanges.addAll(rangesJDHz);
      } else {
        // Check Shadowing for every stations ?

        obsRanges.add(rangeJDRiseSet);
      }

      // Intersect with night limits :
      if (this.useNightLimit) {
        obsRanges.addAll(this.nightLimits);
        nValid++;
      }

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("obsRanges : " + obsRanges);
      }

      // finally : merge intervals :
      final List<Range> finalRanges = Range.mergeRanges(obsRanges, nValid);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("finalRanges : " + finalRanges);
      }

      // store merge result as date intervals :
      if (finalRanges != null) {
        for (Range range : finalRanges) {
          convertRangeToDateInterval(range, starObs.getVisible());
          /*
           * know bug : due to HA limit [+/-12h], the converted JD / Date ranges
           * can have a discontinuity on the LST/UTC axis !
           */
        }

        // update Star Data :
        final List<Range> haObsRanges = new ArrayList<Range>(2);

        Range rangeHA = null;
        for (Range rangeJD : finalRanges) {
          rangeHA = convertJDToHARange(rangeJD, precRA);
          if (rangeHA != null) {
            haObsRanges.add(rangeHA);
          }
        }

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("HA observability = " + haObsRanges);
        }
        starData.setObsRangesHA(haObsRanges);
      }

    } else {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("Target never rise : " + target);
      }
    }
  }

  /**
   * Return the intervals (hour angles) for all base lines compatible with wMin < w(h) < wMax,
   * wMin and wMax are given by wRanges.
   * 
   * This method is similar to DelayLineService.findHAIntervals(...) but it takes into account the PoPs i.e.
   * it finds the best PoP combination to maximize the HA interval (delay line + rise/set)
   *
   * @param dec target declination (rad)
   * @param rangesTarget HA ranges for target rise/set and night limits
   * @param starObs star observability bean to set the final PoP combination
   * @return intervals (hour angles) or null if thread interrupted
   */
  public List<List<Range>> findHAIntervalsWithPops(final double dec, final List<Range> rangesTarget, final StarObservabilityData starObs) {

    // First Pass :
    // For all PoP combinations : find the HA interval merged with the HA Rise/set interval
    // list of observability data associated to a pop combination :
    final List<PopObservabilityData> popDataList = getPopObservabilityData(starObs.getName(), dec, rangesTarget);

    // Current pop observability :
    PopObservabilityData popData;

    // Find the PoP combination that gives the longest HA interval :
    if (popDataList != null && !popDataList.isEmpty()) {
      // Sort pop observability data according to max length :
      Collections.sort(popDataList);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("Sorted PopData : " + popDataList);
      }

      // maximum length for this Pop Combination :
      final int end = popDataList.size() - 1;

      final PopObservabilityData popBestData = popDataList.get(end);

      if (this.popCombinations.size() > 1) {
        // find all equivalent pop combinations :
        final List<PopObservabilityData> bestPoPs = new ArrayList<PopObservabilityData>();
        for (int i = end; i >= 0; i--) {
          popData = popDataList.get(i);
          if (popBestData.getMaxLength() == popData.getMaxLength()) {
            bestPoPs.add(popData);
          } else {
            break;
          }
        }

        if (logger.isLoggable(Level.INFO)) {
          logger.info("best PoPs : " + bestPoPs);
        }
      }

      if (this.data.getBestPops() == null) {
        this.data.setBestPops(popBestData.getPopCombination());
      }

      return popBestData.getRangesBL();
    }
    return Collections.emptyList();
  }

  /**
   * Return the list of Pop Observability Data i.e. for every PoP combination,
   * give the intervals (hour angles) for all base lines compatible with wMin < w(h) < wMax,
   * wMin and wMax are given by wRanges.
   *
   * This method is similar to DelayLineService.findHAIntervals(...) but it takes into account the PoPs i.e.
   * it finds the best PoP combination to maximize the HA interval (delay line + rise/set)
   *
   * @param targetName name of the target
   * @param dec target declination (rad)
   * @param rangesTarget HA ranges for target rise/set and night limits
   * @return intervals (hour angles) or null if thread interrupted
   */
  public List<PopObservabilityData> getPopObservabilityData(final String targetName, final double dec, final List<Range> rangesTarget) {

    // For all PoP combinations : find the HA interval merged with the HA Rise/set interval
    // list of observability data associated to a pop combination :
    final List<PopObservabilityData> popDataList = new ArrayList<PopObservabilityData>();

    // Current pop observability :
    PopObservabilityData popData;

    // list of HA ranges for all base lines :
    final List<List<Range>> rangesBL = new ArrayList<List<Range>>();

    // Current list of HA ranges for a base line :
    List<Range> ranges;

    // Temporary lists to merge HA ranges with Rise/set range :
    final List<Range> flatRanges = new ArrayList<Range>();

    // w range using the pop offset for a given base line :
    final Range wRangeWithOffset = new Range();

    final int sizeCb = this.popCombinations.size();
    final int sizeBL = this.baseLines.size();

    BaseLine bl;
    Range wRange;
    Double offset;

    PopCombination popComb;
    List<Double> popOffsets;

    // flag to skip DL evaluation when the target is not observable for at least one DL
    boolean skip = false;

    // For every Pop Combination :
    for (int k = 0; k < sizeCb; k++) {
      popComb = this.popCombinations.get(k);
      popOffsets = popComb.getPopOffsets();

      skip = false;

      // For every Base Line :
      for (int i = 0; i < sizeBL; i++) {
        bl = this.baseLines.get(i);
        wRange = this.wRanges.get(i);
        offset = popOffsets.get(i);

        // adjust w range with the current pop combination's Offset :
        wRangeWithOffset.setMin(wRange.getMin() + offset.doubleValue());
        wRangeWithOffset.setMax(wRange.getMax() + offset.doubleValue());

        // fast interrupt :
        if (this.currentThread.isInterrupted()) {
          return null;
        }

        ranges = DelayLineService.findHAIntervalsForBaseLine(dec, bl, wRangeWithOffset);

        if (ranges.isEmpty()) {
          // this base line is incompatible with that W range :
          skip = true;
          break;
        } else {
          rangesBL.add(ranges);
        }
      }

      if (!skip) {
        popData = new PopObservabilityData(targetName, popComb, new ArrayList<List<Range>>(rangesBL));

        flatRanges.addAll(rangesTarget);

        // note : rangesTarget contains both rise/set intervals + night limits in HA

        // merge the baseline ranges with the target intervals :
        popData.computeMaxLength(sizeBL + 1 + ((this.useNightLimit) ? 1 : 0), flatRanges);

        if (popData.getMaxLength() > 0d) {
          // skip pop solutions outside Rise/Set HA range :
          popDataList.add(popData);
        }
      }

      rangesBL.clear();
    }

    return popDataList;
  }

  /**
   * Check the horizon profiles for all stations given the target rise/set range (JD)
   * @param jdRiseSet target rise/set range (JD)
   * @return list of observable ranges (no obstruction) or null if thread interrupted
   */
  private List<Range> checkHorizonProfile(final Range jdRiseSet) {
    // output :
    final List<Range> ranges = new ArrayList<Range>();

    // Prepare profiles :
    final HorizonService hs = HorizonService.getInstance();

    final List<Profile> profiles = new ArrayList<Profile>(this.beams.size());

    for (Beam b : this.beams) {
      profiles.add(hs.getProfile(this.interferometer.getName(), b.getStation()));
    }

    // 1 minutes :
    final double jdStep = (1d / 60d) / 24d;

    final double jdMin = jdRiseSet.getMin();
    final double jdMax = jdRiseSet.getMax();

    AzEl azEl;
    boolean visible = false;
    boolean last = false;

    Range range = new Range();

    for (double jd = jdMin; jd < jdMax; jd += jdStep) {

      // fast interrupt :
      if (this.currentThread.isInterrupted()) {
        return null;
      }

      azEl = this.sc.getTargetPosition(jd);

      // For every beam (station) :
      // check if there is no horizon obstruction :
      visible = true;

      for (Profile p : profiles) {
        if (!hs.checkProfile(p, azEl.getAzimuth(), azEl.getElevation())) {
          /*
          if (logger.isLoggable(Level.FINE)) {
          logger.fine("Target hidden by horizon profile = " + p.getName() + " [" +
          azEl.getAzimuth() + ", " + azEl.getElevation() + "]");
          }
           */
          visible = false;
          break;
        }
      }

      // manage intervals :
      if (visible) {
        if (!last) {
          last = true;
          // start point
          range.setMin(jd);
        }
      } else {
        if (last) {
          last = false;
          // end point
          range.setMax(jd);
          ranges.add(range);
          range = new Range();
        }
      }
    }

    // close last interval if opened :
    if (range.getMin() > 0d) {
      range.setMax(jdMax);
      ranges.add(range);
    }

    return ranges;
  }

  /**
   * Use the observation to define the interferometer and instrument configurations and check if the interferometer has PoPs
   * @throws IllegalStateException if the interferometer configuration or instrument configuration is undefined
   */
  private void prepareObservation() throws IllegalStateException {
    this.minElev = Math.toRadians(this.observation.getInterferometerConfiguration().getMinElevation());

    if (doBaseLineLimits) {
      // ignore night limits :
      this.useNightLimit = false;
    } else {
      this.useNightLimit = this.observation.getWhen().isNightRestriction();
    }

    final InterferometerConfiguration intConf = this.observation.getInterferometerConfiguration().getInterferometerConfiguration();

    this.interferometer = intConf.getInterferometer();

    final FocalInstrumentConfiguration insConf = this.observation.getInstrumentConfiguration().getInstrumentConfiguration();

    this.instrument = insConf.getFocalInstrument();

    // check Pops :
    this.hasPops = !this.interferometer.getPops().isEmpty();

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("interferometer = " + this.interferometer.getName());
      logger.fine("instrument     = " + this.instrument.getName());
    }
  }

  /**
   * Define the beams from the user station list, associates a channel to the beam (first channel not used) and a delay line.
   * Computes the fixed offset for the beam from the station fixed offset and the interferometer switchyard
   * @throws IllegalStateException if the station list is undefined
   */
  private void prepareBeams() {
    // Get chosen stations :
    final List<Station> stations = this.observation.getInstrumentConfiguration().getStationList();
    if (stations == null) {
      throw new IllegalStateException("prepareBeams : the station list is null !");
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("stations = " + stations);
    }

    final int nBeams = stations.size();

    this.beams = new ArrayList<Beam>(nBeams);

    // Beams are defined in the same ordering than stations :
    for (Station s : stations) {
      this.beams.add(new Beam(s));

      if (s.getHorizon() != null && !s.getHorizon().getPoints().isEmpty()) {
        this.hasHorizon = true;
      }
    }

    // Get channels :
    final List<Channel> channels = this.interferometer.getChannels();

    // Get delay Lines :
    final List<DelayLine> delayLines = this.interferometer.getDelayLines();

    final int nDelayLines = delayLines.size();

    // Has switchyard ?

    if (!channels.isEmpty() && this.interferometer.getSwitchyard() != null) {
      // Case Interferometer with a switchyard (VLTI and CHARA) :

      // used channels :
      final HashSet<Channel> channelSet = new HashSet<Channel>();

      // find an available channel for every station :

      StationLinks sl;
      for (Beam b : this.beams) {

        // for each station, get the possible channels in the switchyard configuration :
        sl = ConfigurationManager.getInstance().getStationLinks(this.interferometer, b.getStation());

        for (ChannelLink cl : sl.getChannelLinks()) {

          if (!channelSet.contains(cl.getChannel())) {
            channelSet.add(cl.getChannel());

            // optical path = switchyard + station fixed offset
            b.setChannel(cl.getChannel());
            b.addOpticalLength(cl.getOpticalLength());

            // fixed offset (CHARA) :
            if (b.getStation().getDelayLineFixedOffset() != null) {
              b.addOpticalLength(b.getStation().getDelayLineFixedOffset());
            }

            if (logger.isLoggable(Level.FINE)) {
              logger.fine("station = " + b.getStation() + ", Channel = " + b.getChannel().getName());
              logger.fine("switchyard = " + cl.getOpticalLength() + ", fixed = " + b.getStation().getDelayLineFixedOffset());
              logger.fine("total = " + b.getOpticalLength());
            }
            break;
          }
        }
        if (b.getChannel() == null) {
          throw new IllegalStateException("Impossible to associate a channel to every station among [" + stations + "].");
        }
      }

      // Associate a delay line to the beam :
      // Simple association between DL / Channel = DL_n linked to Channel_n

      Channel ch;
      int chPos;
      for (Beam b : this.beams) {
        ch = b.getChannel();

        chPos = channels.indexOf(ch);

        if (chPos < nDelayLines) {
          b.setDelayLine(delayLines.get(chPos));
        } else {
          // Invalid Channel : no delay line ?
          // To be checked 
          throw new IllegalStateException("Impossible to associate a delay line to the beam [" + b + "].");
        }

      }

    } else {
      // Simpler interferometer : no channel definition nor switchyard :

      // Simple association between DL / Station = DL_n linked to Station_n
      // Use the fixed offset of every station :
      int i = 0;
      for (Beam b : this.beams) {

        if (i < nDelayLines) {
          b.setDelayLine(delayLines.get(i));
          if (b.getStation().getDelayLineFixedOffset() == null) {
            throw new IllegalStateException("Missing fixed offset for station [" + b.getStation() + "].");
          }
          b.addOpticalLength(b.getStation().getDelayLineFixedOffset());
        } else {
          // no delay line ?
          throw new IllegalStateException("Impossible to associate a delay line to the beam [" + b + "].");
        }

        i++;
      }

    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Beams = " + this.beams);
    }
  }

  /**
   * Generate all PoP combinations and offsets for the given number of beams
   * @param pops list of pops for the interferometer
   */
  private void preparePopCombinations() {

    final int nBeams = this.beams.size();

    // Get chosen PoPs :
    final List<Pop> userPoPs = this.observation.getInstrumentConfiguration().getPopList();

    if (userPoPs == null) {
      // Generate all PoP combinations for 3T or 4T :
      final List<Pop> pops = this.interferometer.getPops();

      final int n = pops.size();
      final int p = nBeams;

      List<Pop> comb;

      // TODO : find a generic way to get all combinations for any number of beams
      if (p == 2) {
        this.popCombinations = new ArrayList<PopCombination>(n * n);

        for (int i = 0; i < n; i++) {
          for (int j = 0; j < n; j++) {
            comb = new ArrayList<Pop>(p);
            comb.add(pops.get(i));
            comb.add(pops.get(j));
            this.popCombinations.add(new PopCombination(comb));
          }
        }

      } else if (p == 3) {
        this.popCombinations = new ArrayList<PopCombination>(n * n * n);

        for (int i = 0; i < n; i++) {
          for (int j = 0; j < n; j++) {
            for (int k = 0; k < n; k++) {
              comb = new ArrayList<Pop>(p);
              comb.add(pops.get(i));
              comb.add(pops.get(j));
              comb.add(pops.get(k));
              this.popCombinations.add(new PopCombination(comb));
            }
          }
        }
      } else if (p == 4) {
        this.popCombinations = new ArrayList<PopCombination>(n * n * n * n);

        for (int i = 0; i < n; i++) {
          for (int j = 0; j < n; j++) {
            for (int k = 0; k < n; k++) {
              for (int l = 0; l < n; l++) {
                comb = new ArrayList<Pop>(p);
                comb.add(pops.get(i));
                comb.add(pops.get(j));
                comb.add(pops.get(k));
                comb.add(pops.get(l));
                this.popCombinations.add(new PopCombination(comb));
              }
            }
          }
        }
      } else {
        // case with 2 or 6 telescopes :
        throw new UnsupportedOperationException("This number of stations is not supported with PoPs !");
      }
    } else {
      // use the user defined PoPs configuration :
      this.popCombinations = new ArrayList<PopCombination>(1);
      this.popCombinations.add(new PopCombination(userPoPs));
    }

    Beam b1, b2;
    Pop p1, p2;

    List<Pop> popList;
    List<Double> popOffsetList;
    double t;

    for (PopCombination popComb : this.popCombinations) {
      popList = popComb.getPopList();

      popOffsetList = new ArrayList<Double>(this.baseLines.size());

      // use indices to get Pop associated to the station :
      for (int i = 0; i < nBeams; i++) {
        for (int j = i + 1; j < nBeams; j++) {
          b1 = this.beams.get(i);
          p1 = popList.get(i);

          b2 = this.beams.get(j);
          p2 = popList.get(j);

          // Note : the beams are in the same order as the stations :

          // optical path difference = difference of pops delays :
          t = getPopOpticalLength(b1.getStation(), p1) - getPopOpticalLength(b2.getStation(), p2);

          popOffsetList.add(Double.valueOf(t));
        }
      }
      popComb.setPopOffsets(popOffsetList);
    }
  }

  private double getPopOpticalLength(final Station station, final Pop pop) {
    for (PopLink pl : station.getPopLinks()) {
      if (pl.getPop().equals(pop)) {
        return pl.getOpticalLength();
      }
    }
    throw new IllegalStateException("No pop value for couple (" + station.getName() + " - " + pop + ") !");
  }

  /**
   * Define the base lines using the predefined beams and computes the range [W min; W max]
   * corresponding to the fixed optical path difference +- the delay line throw
   */
  private void prepareBaseLines() {

    final int nBeams = this.beams.size();

    Beam b1, b2;

    double x, y, z, t, wMin, wMax;
    for (int i = 0; i < nBeams; i++) {
      for (int j = i + 1; j < nBeams; j++) {
        b1 = this.beams.get(i);
        b2 = this.beams.get(j);

        x = b1.getStation().getRelativePosition().getPosX() - b2.getStation().getRelativePosition().getPosX();
        y = b1.getStation().getRelativePosition().getPosY() - b2.getStation().getRelativePosition().getPosY();
        z = b1.getStation().getRelativePosition().getPosZ() - b2.getStation().getRelativePosition().getPosZ();

        // a Base line defines only the geometry :
        this.baseLines.add(new BaseLine(b1, b2, x, y, z));

        t = b1.getOpticalLength() - b2.getOpticalLength();

        // 2 DL for 2 telescopes => double throw :
        // note : for now, all DLs are equivalent (same throw) :

        wMin = t - b2.getDelayLine().getMaximumThrow();
        wMax = t + b1.getDelayLine().getMaximumThrow();

        // the range contains the w delay limits for the corresponding base line :
        this.wRanges.add(new Range(wMin, wMax));
      }
    }

    this.data.setBaseLines(this.baseLines);
  }

  /**
   * Process the sun time stamps to have both night limits in the LST range [0;24] +/- 12h
   * and all intervals (day/night/twilight) in the LST range [0;24]
   * @param sunEvents jd sun events in the LST range [0;24] +/- 12h
   */
  private void processSunAlmanach(final List<SunAlmanachTime> sunEvents) {
    List<SunTimeInterval> intervals = null;

    if (sunEvents != null && !sunEvents.isEmpty()) {
      final int nbInterval = sunEvents.size() - 1;
      intervals = new ArrayList<SunTimeInterval>(nbInterval);

      SunAlmanachTime stFrom, stTo;
      double jdFrom, jdTo;
      Date from, to;
      SunTimeInterval.SunType type = null;

      for (int i = 0; i < nbInterval; i++) {
        stFrom = sunEvents.get(i);
        stTo = sunEvents.get(i + 1);

        jdFrom = stFrom.getJd();
        jdTo = stTo.getJd();

        switch (stFrom.getType()) {
          case SunRise:
            type = SunTimeInterval.SunType.Day;
            break;
          case SunSet:
            type = SunTimeInterval.SunType.Twilight;
            break;
          case SunTwlRise:
            type = SunTimeInterval.SunType.Twilight;
            break;
          case SunTwlSet:
            type = SunTimeInterval.SunType.Night;
            break;
          default:
        }

        if (type == SunTimeInterval.SunType.Night) {
          if (this.nightLimits == null) {
            this.nightLimits = new ArrayList<Range>(2);
          }
          this.nightLimits.add(new Range(jdFrom, jdTo));
        }

        // Keep intervals that are inside or overlapping the LST [0;24] range :
        if ((jdFrom >= this.jdLst0 && jdFrom <= this.jdLst24) || (jdTo >= this.jdLst0 && jdTo <= this.jdLst24)) {

          if (logger.isLoggable(Level.FINE)) {
            logger.fine("Range[" + jdFrom + " - " + jdTo + "] : " + type);
          }

          // adjust range limits :
          if (jdFrom < this.jdLst0) {
            jdFrom = this.jdLst0;
          }
          if (jdTo > this.jdLst24) {
            jdTo = this.jdLst24;
          }

          from = jdToDate(jdFrom);
          to = jdToDate(jdTo);

          if (logger.isLoggable(Level.FINE)) {
            logger.fine("SunInterval[" + from + " - " + to + "] : " + type);
          }

          intervals.add(new SunTimeInterval(from, to, type));
        }
      }

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("nightLimits : " + this.nightLimits);
      }

    }

    this.data.setSunIntervals(intervals);
  }

  /**
   * Convert an HA range to a JD range
   * @param rangeHA given in hour angle (dec hours)
   * @param lstOffset right ascension (dec hours)
   * @return JD range
   */
  private Range convertHAToJDRange(final Range rangeHA, final double lstOffset) {

    final double ha1 = rangeHA.getMin();
    final double ha2 = rangeHA.getMax();

    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("ha1 = " + ha1);
      logger.finest("ha2 = " + ha2);
    }

    final double lst1 = lstOffset + ha1;
    final double lst2 = lstOffset + ha2;

    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("lst1 = " + lst1 + " h");
      logger.finest("lst2 = " + lst2 + " h");
    }

    // apply the sideral / solar ratio :
    final double jd1 = this.jdLst0 + AstroSkyCalc.lst2jd(lst1);
    final double jd2 = this.jdLst0 + AstroSkyCalc.lst2jd(lst2);

    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("jd1 = " + this.sc.toDate(jd1, true));
      logger.finest("jd2 = " + this.sc.toDate(jd2, true));
    }

    return new Range(jd1, jd2);
  }

  /**
   * Convert a JD range to an HA range but keep only ranges with an HA in [-12;12]
   * @param rangeJD given in hour angle (dec hours)
   * @param lstOffset right ascension (dec hours)
   * @return JD range
   */
  private Range convertJDToHARange(final Range rangeJD, final double lstOffset) {

    final double jd1 = rangeJD.getMin();
    final double jd2 = rangeJD.getMax();

    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("jd1 = " + this.sc.toDate(jd1, true));
      logger.finest("jd2 = " + this.sc.toDate(jd2, true));
    }

    // apply the sideral / solar ratio :
    final double lst1 = AstroSkyCalc.jd2lst(jd1 - this.jdLst0);
    final double lst2 = AstroSkyCalc.jd2lst(jd2 - this.jdLst0);

    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("lst1 = " + lst1 + " h");
      logger.finest("lst2 = " + lst2 + " h");
    }

    double ha1 = lst1 - lstOffset;
    double ha2 = lst2 - lstOffset;

    if (ha1 < -12d) {
      ha1 = -12d;
    }
    if (ha1 > 12d) {
      // invalid range :
      return null;
    }
    if (ha2 < -12d) {
      // invalid range :
      return null;
    }
    if (ha2 > 12d) {
      ha2 = 12d;
    }

    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("ha1 = " + ha1 + " h");
      logger.finest("ha2 = " + ha2 + " h");
    }

    return new Range(ha1, ha2);
  }

  /**
   * Convert a JD range to a date interval with respect for the LST range [0;24]
   * Know bug : due to HA limit [+/-12h], the converted JD / Date ranges
   * can have a discontinuity on the LST/UTC axis !
   *
   * @param rangeJD range to convert
   * @param intervals interval list where new date intervals will be added
   */
  private void convertRangeToDateInterval(final Range rangeJD, final List<DateTimeInterval> intervals) {
    // one Day in LST is different than one Day in JD :
    final double day = AstroSkyCalc.lst2jd(24d);

    DateTimeInterval interval;

    final double jdStart = rangeJD.getMin();
    final double jdEnd = rangeJD.getMax();

    if (jdStart >= this.jdLst0) {

      if (jdEnd <= this.jdLst24) {

        // single interval [jdStart;jdEnd]
        interval = new DateTimeInterval();
        interval.setStartDate(jdToDate(jdStart));
        interval.setEndDate(jdToDate(jdEnd));
        intervals.add(interval);

      } else {

        if (jdStart > this.jdLst24) {
          // two points over LST 24 :

          // single interval [jdStart - day;jdEnd - day]
          interval = new DateTimeInterval();
          interval.setStartDate(jdToDate(jdStart - day));
          interval.setEndDate(jdToDate(jdEnd - day));
          intervals.add(interval);

        } else {
          // end occurs after LST 24 :

          // interval [jdStart;jdLst24]
          interval = new DateTimeInterval();
          interval.setStartDate(jdToDate(jdStart));
          interval.setEndDate(jdToDate(this.jdLst24));
          intervals.add(interval);

          // add the second interval [jdLst0;jdEnd - day]

          interval = new DateTimeInterval();
          interval.setStartDate(jdToDate(this.jdLst0));
          interval.setEndDate(jdToDate(jdEnd - day));
          intervals.add(interval);
        }
      }

    } else {
      // start occurs before LST 0h :

      if (jdEnd < this.jdLst0) {
        // two points before LST 0h :

        // single interval [jdStart + day;jdEnd + day]
        interval = new DateTimeInterval();
        interval.setStartDate(jdToDate(jdStart + day));
        interval.setEndDate(jdToDate(jdEnd + day));
        intervals.add(interval);

      } else {

        // interval [jdLst0;jdEnd]
        interval = new DateTimeInterval();
        interval.setStartDate(jdToDate(this.jdLst0));
        interval.setEndDate(jdToDate(jdEnd));
        intervals.add(interval);

        // add the second interval [jdStart + day;jdLst24]

        interval = new DateTimeInterval();
        interval.setStartDate(jdToDate(jdStart + day));
        interval.setEndDate(jdToDate(this.jdLst24));
        intervals.add(interval);
      }
    }

  }

  private Date jdToDate(final double jd) {
    return this.sc.toDate(jd, this.useLST);
  }

  /**
   * Generate a target list for the base line limits (every 5 deg)
   * @return target list
   */
  private List<Target> generateTargetsForBaseLineLimits() {

    final double obsLat = Math.toDegrees(this.interferometer.getPosSph().getLatitude());

    final double minElevDeg = Math.toDegrees(this.minElev);

    int decMin = 5 * (int) Math.round((obsLat - 90d + minElevDeg) / 5d);
    decMin = Math.max(decMin, -90);

    int decMax = 5 * (int) Math.round((obsLat + 90 - minElevDeg) / 5d);
    decMax = Math.min(decMax, 90);

    final List<Target> targets = new ArrayList<Target>();
    Target t;
    for (int i = decMax; i > decMin; i -= 5) {
      t = new Target();
      // delta = n (deg)
      t.setName("\u0394 = " + Integer.toString(i));
      // 12:00:00
      t.setRA(180d);
      t.setDEC(i);
      t.setEQUINOX(AsproConstants.EPOCH_J2000);

      targets.add(t);
    }
    return targets;
  }
}
