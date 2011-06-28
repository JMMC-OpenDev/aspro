/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import edu.dartmouth.AstroAlmanac;
import edu.dartmouth.AstroSkyCalc;
import edu.dartmouth.AstroSkyCalcObservation;
import edu.dartmouth.AstroAlmanacTime;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.observability.DateTimeInterval;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.observability.PopCombination;
import fr.jmmc.aspro.model.observability.GroupedPopObservabilityData;
import fr.jmmc.aspro.model.observability.PopObservabilityData;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.observability.ElevationDate;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.observability.StarObservabilityData;
import fr.jmmc.aspro.model.observability.SunTimeInterval;
import fr.jmmc.aspro.model.observability.SunTimeInterval.SunType;
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
import fr.jmmc.aspro.util.CombUtils;
import fr.jmmc.aspro.util.TestUtils;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.logging.Level;
import javax.xml.datatype.XMLGregorianCalendar;

/**
 * This service determines the observability of a list of targets given an observation setting
 *
 * @author bourgesl
 */
public final class ObservabilityService {

  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          ObservabilityService.class.getName());
  /** flag to slow down the service to detect concurrency problems */
  private final static boolean DEBUG_SLOW_SERVICE = false;

  /* members */

  /* output */
  /** observability data */
  private final ObservabilityData data;

  /* inputs */
  /** observation settings used  (read-only copy of the modifiable observation) */
  private final ObservationSetting observation;
  /** indicates if the timestamps are expressed in LST or in UTC */
  private final boolean useLST;
  /** flag to find baseline limits */
  private final boolean doBaseLineLimits;
  /** flag to produce detailed output with all BL / horizon / rise intervals per target */
  private final boolean doDetailedOutput;
  /** flag to center the plot arround midnight */
  private final boolean doCenterMidnight;
  /** twilight considered as night limit */
  private final SunType twilightNightLimit;

  /* internal */
  /** Get the current thread to check if the computation is interrupted */
  private final Thread currentThread = Thread.currentThread();
  /** sky calc instance */
  private final AstroSkyCalc sc = new AstroSkyCalc();
  /** observation sky calc instance */
  private final AstroSkyCalcObservation sco = new AstroSkyCalcObservation();
  /** lower jd arround the observation date */
  private double jdLower;
  /** upper jd arround the observation date */
  private double jdUpper;
  /** flag to enable the observability restriction due to the night */
  private boolean useNightLimit;
  /** Night ranges defined in julian day */
  private List<Range> nightLimits = null;
  /** minimum of elevation to observe any target (deg) */
  private double minElev = Double.NaN;
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
  private List<BaseLine> baseLines = null;
  /** W ranges corresponding to the base line list */
  private List<Range> wRanges = null;
  /** list of Pop combinations with pop delays per baseline */
  private List<PopCombination> popCombinations = null;
  /** flag to disable the observability restriction due to the night */
  private boolean ignoreUseNightLimit = false;

  /**
   * Constructor.
   * Note : This service is statefull so it can not be reused by several calls.
   *
   * @param observation observation settings
   * @param useLST indicates if the timestamps are expressed in LST or in UTC
   * @param doDetailedOutput flag to produce detailed output with all BL / horizon / rise intervals per target
   * @param doBaseLineLimits flag to find base line limits
   * @param doCenterMidnight flag to center JD range arround midnight
   * @param twilightNightLimit twilight considered as night limit
   */
  public ObservabilityService(final ObservationSetting observation,
                              final boolean useLST, final boolean doDetailedOutput, final boolean doBaseLineLimits,
                              final boolean doCenterMidnight, final SunType twilightNightLimit) {
    // Inputs :
    this.observation = observation;
    this.useLST = useLST;
    this.doDetailedOutput = doDetailedOutput;
    this.doBaseLineLimits = doBaseLineLimits;
    this.doCenterMidnight = doCenterMidnight;
    this.twilightNightLimit = twilightNightLimit;

    // create the observability data corresponding to the observation version :
    this.data = new ObservabilityData(observation.getVersion(), useLST, doDetailedOutput, doBaseLineLimits, doCenterMidnight, twilightNightLimit);
  }

  /**
   * Specific Constructor to prepare VLTI Observing blocks (LST)
   * Note : This service is statefull so it can not be reused by several calls.
   *
   * @param observation observation settings
   */
  public ObservabilityService(final ObservationSetting observation) {
    this(observation, false);
  }

  /**
   * Specific Constructor to prepare Observing blocks (LST) using astronomical night (-18 deg) ignoring night restrictions (OB VEGA only)
   * Note : This service is statefull so it can not be reused by several calls.
   *
   * @param observation observation settings
   * @param ignoreNightLimits true to disable the observability restriction due to the night
   */
  public ObservabilityService(final ObservationSetting observation, final boolean ignoreNightLimits) {
    // use LST without using night center:
    this(observation, true, false, false, false, SunType.Night);

    // ignore night restrictions :
    this.ignoreUseNightLimit = ignoreNightLimits;
  }

  /**
   * Main operation to determine the source observability for a given interferometer configuration
   *
   * @return ObservabilityData container
   */
  public ObservabilityData compute() {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("\n\n--------------------------------------------------------------------------------\n\n");
      logger.fine("compute : " + this.observation);
    }

    // Start the computations :
    final long start = System.nanoTime();

    // Get interferometer / instrument :
    prepareObservation();

    // Define target list :
    final List<Target> targets;
    if (this.doBaseLineLimits) {
      targets = generateTargetsForBaseLineLimits();
    } else {
      targets = this.observation.getTargets();
    }

    // define the targets anyway :
    this.data.setTargets(targets);

    // define site :
    this.sc.defineSite(this.interferometer.getName(), this.interferometer.getPosSph());
    this.sco.defineSite(this.sc);

    // define observation date :
    // find the appropriate night (LST range):
    this.defineObservationRange();

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

      // dump star visibilities :
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("Star observability intervals : ");

        for (List<StarObservabilityData> soList : this.data.getMapStarVisibilities().values()) {
          for (StarObservabilityData so : soList) {
            logger.fine(so.toString());
          }
        }
      }
    }

    // fast interrupt :
    if (this.currentThread.isInterrupted()) {
      return null;
    }

    if (DEBUG_SLOW_SERVICE) {
      TestUtils.busyWait(2000l);

      if (this.currentThread.isInterrupted()) {
        return null;
      }
    }

    if (logger.isLoggable(Level.INFO)) {
      logger.info("compute : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");
    }

    return this.data;
  }

  /**
   * Define the observation date and determine the jd bounds (lst0 - lst24 or arround midnight)
   */
  private void defineObservationRange() {

    this.data.setDateCalc(this.sc);

    // define date :
    final XMLGregorianCalendar cal = this.observation.getWhen().getDate();

    // find the julian date corresponding to the LST origin LST=00:00:00 for the given date :
    this.jdLower = this.sc.defineDate(cal.getYear(), cal.getMonth(), cal.getDay());

    // find the julian date corresponding to LST=00:00:00 next day:
    this.jdUpper = this.sc.findJdForLst0(this.jdLower + AstroSkyCalc.LST_DAY_IN_JD);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("jdLst0 = " + this.jdLower);
      logger.fine("jdLst24 = " + this.jdUpper);
    }

    this.data.setDateMin(jdToDate(this.jdLower));
    this.data.setDateMax(jdToDate(this.jdUpper));

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("date min = " + this.data.getDateMin());
      logger.fine("date max = " + this.data.getDateMax());
    }

    // fast interrupt :
    if (this.currentThread.isInterrupted()) {
      return;
    }

    // 1 - Find the day / twlight / night zones :
    if (this.useNightLimit) {
      // initialize night limits anyway:
      this.nightLimits = new ArrayList<Range>(2);

      if (this.doCenterMidnight) {
        final double jdMidnight = this.sc.getJdMidnight();

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("jdMidnight = " + jdMidnight);
        }

        // adjust the jd bounds :
        this.jdLower = jdMidnight - AstroSkyCalc.HALF_LST_DAY_IN_JD;
        this.jdUpper = jdMidnight + AstroSkyCalc.HALF_LST_DAY_IN_JD;

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("jdLower = " + this.jdLower);
          logger.fine("jdUpper = " + this.jdUpper);
        }

        this.data.setDateMin(jdToDate(this.jdLower));
        this.data.setDateMax(jdToDate(this.jdUpper));

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("date min = " + this.data.getDateMin());
          logger.fine("date max = " + this.data.getDateMax());
        }
      }

      //  sun rise/set with twilight : see NightlyAlmanac
      final AstroAlmanac almanac = this.sc.getAlmanac();

      // get sun times arround midnight (-1 day; + 2 days):
      final List<AstroAlmanacTime> sunTimes = new ArrayList<AstroAlmanacTime>(almanac.getSunTimes());

      // Extract night limits in [LST0 -12; LST0 + 36] and sun intervals in range [jdLower; jdUpper]:
      processSunAlmanach(sunTimes);

      // moon rise/set :
      final List<Range> moonRanges = this.sc.findMoonRiseSet(almanac, this.jdLower, this.jdUpper);

      // Moon filters:
      // 1-use ranges in  [LST0 -12; LST0 + 36]:
      // 2- moon Illum in jdLower / jdUpper !

      final double moonIllum = this.sc.getMaxMoonIllum(moonRanges);

      this.data.setMoonIllumPercent(100d * moonIllum);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("moon illum   : " + moonIllum);
      }
    }
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

  /**
   * Find the best Pop combination for the given list of targets
   * @param targets list of targets
   * @return best Pop combination or null if none exists
   */
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
   * @return list of Pop observability data
   */
  private List<PopObservabilityData> findPoPsForTargetObservability(final Target target) {

    // get Target coordinates precessed to jd :
    final double[] raDec = this.sco.defineTarget(jdCenter(), target.getRADeg(), target.getDECDeg());

    // precessed target right ascension in decimal hours :
    final double precRA = raDec[0];

    // precessed target declination in degrees :
    final double precDEC = raDec[1];

    // Find LST range corresponding to the rise / set of the target :
    final double haElev = this.sco.getHAForElevation(precDEC, this.minElev);

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
          rangeHA = this.sc.convertJDToHARange(rangeJD, precRA);
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

    // reset current target :
    this.sco.reset();
    return popDataList;
  }

  /**
   * Finds the observability ranges for the given target
   * @param target target to use
   */
  private void findTargetObservability(final Target target) {

    final String targetName = target.getName();

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("targetName = " + targetName);
    }

    final int listSize = (this.doDetailedOutput) ? (3 + this.baseLines.size()) : 1;
    final List<StarObservabilityData> starVisList = new ArrayList<StarObservabilityData>(listSize);
    this.data.addStarVisibilities(targetName, starVisList);

    final StarObservabilityData starObs = new StarObservabilityData(targetName, StarObservabilityData.TYPE_STAR);
    // add the result to have also unobservable targets :
    starVisList.add(starObs);

    final StarData starData = new StarData(target.getName());
    this.data.addStarData(starData);

    // get Target coordinates precessed to jd and define target to get later az/alt positions from JSkyCalc :
    final double[] raDec = this.sco.defineTarget(jdCenter(), target.getRADeg(), target.getDECDeg());

    // precessed target right ascension in decimal hours :
    final double precRA = raDec[0];

    // precessed target declination in degrees :
    final double precDEC = raDec[1];

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("target[" + target.getName() + "] "
              + AstroSkyCalcObservation.asString(target.getRADeg(), target.getDECDeg())
              + " - precessed: " + AstroSkyCalcObservation.asString(15d * precRA, precDEC));
    }

    // define transit date (HA = 0) :
    starObs.setTransitDate(convertJDToDate(this.sc.convertHAToJD(0d, precRA)));

    // Find LST range corresponding to the rise / set of the target :
    final double haElev = this.sco.getHAForElevation(precDEC, this.minElev);

    // update Star Data :
    starData.setPrecRA(precRA);
    starData.setPrecDEC(precDEC);
    starData.setHaElev(haElev);

    // target rise :
    if (haElev > 0d) {
      // rise/set range :
      final Range rangeHARiseSet = new Range(-haElev, haElev);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("rangeHARiseSet = " + rangeHARiseSet);
      }

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
            rangeHA = this.sc.convertJDToHARange(rangeJD, precRA);
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

      // convert HA range to JD range in range [LST0 - 12; LST0 + 12]
      final Range rangeJDRiseSet = this.sc.convertHAToJDRange(rangeHARiseSet, precRA);

      // For now : only VLTI has horizon profiles :
      List<Range> rangesJDHorizon = null;
      if (this.hasHorizon) {
        // check horizon profiles inside rise/set range :
        rangesJDHorizon = checkHorizonProfile(rangeJDRiseSet);

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("rangesJDHz : " + rangesJDHorizon);
        }

        // fast interrupt :
        if (this.currentThread.isInterrupted()) {
          return;
        }
      }

      // observable ranges (jd) :
      final List<Range> obsRanges = new ArrayList<Range>();

      if (this.doDetailedOutput) {

        // Add Rise/Set :
        final StarObservabilityData soRiseSet = new StarObservabilityData(targetName, "Rise/Set", StarObservabilityData.TYPE_RISE_SET);
        starVisList.add(soRiseSet);

        convertRangeToDateInterval(rangeJDRiseSet, soRiseSet.getVisible());

        if (rangesJDHorizon != null) {
          // Add Horizon :
          final StarObservabilityData soHz = new StarObservabilityData(targetName, "Horizon", StarObservabilityData.TYPE_HORIZON);
          starVisList.add(soHz);

          for (Range range : rangesJDHorizon) {
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
                obsRanges.add(this.sc.convertHAToJDRange(range, precRA));
              }
            }
            if (logger.isLoggable(Level.FINE)) {
              logger.fine("baseLine : " + baseLine);
              logger.fine("JD ranges  : " + obsRanges);
            }

            soBl = new StarObservabilityData(targetName, baseLine.getName(), StarObservabilityData.TYPE_BASE_LINE + i);
            starVisList.add(soBl);

            // convert JD ranges to date ranges :
            for (Range range : obsRanges) {
              convertRangeToDateInterval(range, soBl.getVisible());
            }
            if (soBl.getVisible().size() > 1) {
              // merge contiguous date ranges :
              DateTimeInterval.merge(soBl.getVisible());
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
            obsRanges.add(this.sc.convertHAToJDRange(range, precRA));
          }
        }
      }

      if (rangesJDHorizon != null) {
        obsRanges.addAll(rangesJDHorizon);
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
      final List<Range> finalRanges = Range.intersectRanges(obsRanges, nValid);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("finalRanges : " + finalRanges);
      }

      // store merge result as date intervals :
      if (finalRanges != null) {
        // elevation marks for the current target :
        findElevations(starObs, finalRanges, precRA, precDEC);

        // convert JD ranges to date ranges :
        for (Range range : finalRanges) {
          convertRangeToDateInterval(range, starObs.getVisible());
        }
        if (starObs.getVisible().size() > 1) {
          // merge contiguous date ranges :
          DateTimeInterval.merge(starObs.getVisible());
        }

        // update Star Data :
        final List<Range> haObsRanges = new ArrayList<Range>(2);

        Range rangeHA = null;
        for (Range rangeJD : finalRanges) {
          rangeHA = this.sc.convertJDToHARange(rangeJD, precRA);
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

    // reset current target :
    this.sco.reset();
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
  private List<List<Range>> findHAIntervalsWithPops(final double dec, final List<Range> rangesTarget, final StarObservabilityData starObs) {

    // First Pass :
    // For all PoP combinations : find the HA interval merged with the HA Rise/set interval
    // list of observability data associated to a pop combination :
    final List<PopObservabilityData> popDataList = getPopObservabilityData(starObs.getTargetName(), dec, rangesTarget);

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
  private List<PopObservabilityData> getPopObservabilityData(final String targetName, final double dec, final List<Range> rangesTarget) {
    final double cosDec = Math.cos(dec);
    final double sinDec = Math.sin(dec);

    final int sizeCb = this.popCombinations.size();
    final int sizeBL = this.baseLines.size();

    // For all PoP combinations : find the HA interval merged with the HA Rise/set interval
    // list of observability data associated to a pop combination :
    final List<PopObservabilityData> popDataList = new ArrayList<PopObservabilityData>();

    // Current pop observability :
    PopObservabilityData popData;

    // list of HA ranges for all base lines :
    final List<List<Range>> rangesBL = new ArrayList<List<Range>>(sizeBL);

    // Current list of HA ranges for a base line :
    List<Range> ranges;

    // Temporary lists to merge HA ranges with Rise/set range :
    final List<Range> flatRanges = new ArrayList<Range>();

    // w range using the pop offset for a given base line :
    final Range wRangeWithOffset = new Range();

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

        ranges = DelayLineService.findHAIntervalsForBaseLine(cosDec, sinDec, bl, wRangeWithOffset);

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
    final List<Range> ranges = new ArrayList<Range>(3);

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

    final AzEl azEl = new AzEl();
    boolean visible = false;
    boolean last = false;

    Range range = new Range();

    for (double jd = jdMin; jd < jdMax; jd += jdStep) {

      this.sco.getTargetPosition(jd, azEl);

      // For every beam (station) :
      // check if there is no horizon obstruction :
      visible = true;

      for (final Profile p : profiles) {
        if (!hs.checkProfile(p, azEl.getAzimuth(), azEl.getElevation())) {
          visible = false;

//          if (logger.isLoggable(Level.FINE)) {
//            logger.fine("Target hidden by horizon profile = " + p.getName() + " [" + azEl.getAzimuth() + ", " + azEl.getElevation() + "]");
//          }

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
   */
  private void prepareObservation() {
    // If min elevation was defined in constructor, skip observation's value :
    if (Double.isNaN(this.minElev)) {
      this.minElev = this.observation.getInterferometerConfiguration().getMinElevation();
    }

    this.useNightLimit = this.observation.getWhen().isNightRestriction();

    if (this.doBaseLineLimits || this.ignoreUseNightLimit) {
      // ignore night limits :
      this.useNightLimit = false;
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
  private void prepareBeams() throws IllegalStateException {
    // Get chosen stations :
    final List<Station> stations = this.observation.getInstrumentConfiguration().getStationList();
    if (stations == null || stations.isEmpty()) {
      throw new IllegalStateException("prepareBeams : the station list is empty !");
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("stations = " + stations);
    }

    this.data.setStationNames(this.observation.getInstrumentConfiguration().getStations());

    final int nBeams = stations.size();

    // find the optional channels associated to the stations in the instrument configuration :
    // CHARA : predefined channel per station for a specific base line :
    final List<Channel> relatedChannels = ConfigurationManager.getInstance().getInstrumentConfigurationChannels(
            this.observation.getInterferometerConfiguration().getName(),
            this.observation.getInstrumentConfiguration().getName(),
            this.observation.getInstrumentConfiguration().getStations());

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("relatedChannels = " + relatedChannels);
    }

    final int nRelChannels = relatedChannels.size();
    final boolean useRelatedChannels = nRelChannels > 0;

    if (useRelatedChannels && nBeams != nRelChannels) {
      throw new IllegalStateException("prepareBeams : the number of associated channels does not match the station list : " + stations + " <> " + relatedChannels);
    }

    this.beams = new ArrayList<Beam>(nBeams);

    // Beams are defined in the same ordering than stations :
    for (int i = 0; i < nBeams; i++) {
      Station station = stations.get(i);
      Beam beam = new Beam(station);

      // predefined Channel (CHARA) :
      if (useRelatedChannels && i < nRelChannels) {
        beam.setChannel(relatedChannels.get(i));
      }

      this.beams.add(beam);

      if (station.getHorizon() != null && !station.getHorizon().getPoints().isEmpty()) {
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

      // 2 cases ;
      // CHARA : predefined channel per station for a specific base line
      // VLTI : find an available channel for every station

      // used channels :
      final HashSet<Channel> channelSet = new HashSet<Channel>(nBeams);

      StationLinks sl;
      for (Beam b : this.beams) {

        // for each station, get the possible channels in the switchyard configuration :
        sl = ConfigurationManager.getInstance().getStationLinks(this.interferometer, b.getStation());

        // VLTI : find an available channel for every station :
        if (!useRelatedChannels) {
          for (ChannelLink cl : sl.getChannelLinks()) {

            if (!channelSet.contains(cl.getChannel())) {
              channelSet.add(cl.getChannel());

              // use this channel for the beam :
              b.setChannel(cl.getChannel());
              break;
            }
          }
        }
        if (b.getChannel() == null) {
          throw new IllegalStateException("Unable to associate a channel to every station [" + stations + "].");
        }

        // Use the channel link corresponding to the beam channel :
        for (ChannelLink cl : sl.getChannelLinks()) {

          if (cl.getChannel().equals(b.getChannel())) {
            // optical path = switchyard + station fixed offset
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
      } // for loop on beams

      // Associate a delay line to the beam :
      
      // Use any Delay line available except if the delay line has a prefered station (CHARA E1 shorter than others)

      // used delay lines :
      final HashSet<DelayLine> dlSet = new HashSet<DelayLine>(nBeams);

      for (Beam b : this.beams) {
        
        // first find the delay line dedicated to the beam station:
        Station beamStation = b.getStation();
        
        DelayLine selectedDelayLine = null;
        
        for (DelayLine dl : delayLines) {
          if (beamStation.equals(dl.getStation())) {
            if (!dlSet.contains(dl)) {
              selectedDelayLine = dl;
            }
            break;
          }
        }
        
        // If undefined, use any available delay line:
        if (selectedDelayLine == null) {
          for (DelayLine dl : delayLines) {
            if (!dlSet.contains(dl)) {
              selectedDelayLine = dl;
              break;
            }
          }
        }
        
        if (selectedDelayLine != null) {
          dlSet.add(selectedDelayLine);
          
          b.setDelayLine(selectedDelayLine);
          
        } else {
          // To be checked 
          throw new IllegalStateException("Impossible to associate a delay line to the beam [" + b + "].");
        }
      }

    } else {
      // Simpler interferometer : no channel definition nor switchyard :

      if (logger.isLoggable(Level.WARNING)) {
        logger.warning("Not validated code path : Case with no channel definition nor switchyard !");
      }
      
      // Warning : Not tested because both CHARA and VLTI use a switchyard !

      // Simple association between DL / Station = DL_n linked to Station_n
      // Use only the fixed offset of the station :
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
      // dump beam information:
      int i = 0;
      for (Beam b : this.beams) {
        logger.fine("beam [" + (i++) + "] : " + b.toString());
      }    
    }
    
    this.data.setBeams(this.beams);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Beams = " + this.beams);
    }
  }

  /**
   * Generate all PoP combinations and offsets for the given number of beams
   */
  private void preparePopCombinations() {

    final int nBeams = this.beams.size();

    // Get chosen PoPs :
    final List<Pop> userPoPs = this.observation.getInstrumentConfiguration().getPopList();

    if (userPoPs == null) {
      // Generate all PoP combinations for the given number of beams :
      final List<Pop> pops = this.interferometer.getPops();

      final int nPops = pops.size();

      // Generate all tuples :
      final List<int[]> tuples = CombUtils.generateTuples(nPops, nBeams);

      this.popCombinations = new ArrayList<PopCombination>(tuples.size());

      List<Pop> comb;
      for (int[] tuple : tuples) {
        comb = new ArrayList<Pop>(nBeams);
        for (int i = 0; i < nBeams; i++) {
          comb.add(pops.get(tuple[i]));
        }
        this.popCombinations.add(new PopCombination(comb));
      }

      this.data.setUserPops(false);

    } else {
      // use the user defined PoPs configuration :
      final PopCombination user = new PopCombination(userPoPs);
      this.popCombinations = new ArrayList<PopCombination>(1);
      this.popCombinations.add(user);

      this.data.setUserPops(true);
      this.data.setBestPops(user);
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

  /**
   * Find the optical length between the given station and the PoP
   * @param station used station
   * @param pop used Pop
   * @return optical length
   *
   * @throws IllegalStateException if bad configuration (missing pop value in switchyard)
   */
  private double getPopOpticalLength(final Station station, final Pop pop) throws IllegalStateException {
    for (PopLink pl : station.getPopLinks()) {
      if (pl.getPop() == pop) {
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

    // baseline count :
    final int blen = CombUtils.comb(nBeams, 2);

    this.baseLines = new ArrayList<BaseLine>(blen);
    this.wRanges = new ArrayList<Range>(blen);

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
   * and all intervals (day/night/twilights) in the jd bounds [jdLower; jdUpper]
   * @param sunTimes jd sun almanach times arround midnight (-1 day; + 2 days)
   */
  private void processSunAlmanach(final List<AstroAlmanacTime> sunTimes) {
    List<SunTimeInterval> intervals = null;

    if (sunTimes != null && !sunTimes.isEmpty()) {
      final int nbInterval = sunTimes.size() - 1;
      intervals = new ArrayList<SunTimeInterval>();

      // LST0 reference used to convert HA in JD:
      final double jdLst0 = this.sc.getJdForLst0();
      // lower LST bound = LST0 - 12h
      final double jdLstLower = jdLst0 - AstroSkyCalc.HALF_LST_DAY_IN_JD;
      // upper LST bound = LST0 + 24h + 12h
      final double jdLstUpper = jdLst0 + 3d * AstroSkyCalc.HALF_LST_DAY_IN_JD;

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("jdLst bounds = [" + jdLstLower + ", " + jdLstUpper + "]");
      }

      AstroAlmanacTime stFrom, stTo;
      double jdFrom, jdTo;
      Date from, to;
      SunType type = null;

      for (int i = 0; i < nbInterval; i++) {
        stFrom = sunTimes.get(i);
        stTo = sunTimes.get(i + 1);

        jdFrom = stFrom.getJd();
        jdTo = stTo.getJd();

        switch (stFrom.getType()) {
          case SunTwl18Rise:
            type = SunType.AstronomicalTwilight;
            break;
          case SunTwl12Rise:
            type = SunType.NauticalTwilight;
            break;
          case SunTwl06Rise:
            type = SunType.CivilTwilight;
            break;
          case SunRise:
            type = SunType.Day;
            break;
          case SunSet:
            type = SunType.CivilTwilight;
            break;
          case SunTwl06Set:
            type = SunType.NauticalTwilight;
            break;
          case SunTwl12Set:
            type = SunType.AstronomicalTwilight;
            break;
          case SunTwl18Set:
            type = SunType.Night;
            break;
          default:
            type = null;
        }

        if (type != null && type.isNight(this.twilightNightLimit)) {
          // Keep nights that are inside or overlapping the range [jdLstLower; jdLstUpper] range :
          if ((jdFrom >= jdLstLower && jdFrom <= jdLstUpper) || (jdTo >= jdLstLower && jdTo <= jdLstUpper)) {
            this.nightLimits.add(new Range(jdFrom, jdTo));
          }
        }

        // Keep intervals that are inside or overlapping the range [jdLower; jdUpper] range :
        if ((jdFrom >= this.jdLower && jdFrom <= this.jdUpper) || (jdTo >= this.jdLower && jdTo <= this.jdUpper)) {

          if (logger.isLoggable(Level.FINE)) {
            logger.fine("Range[" + jdFrom + " - " + jdTo + "] : " + type);
          }

          from = jdToDateInDateRange(jdFrom);
          to = jdToDateInDateRange(jdTo);

          if (logger.isLoggable(Level.FINE)) {
            logger.fine("SunInterval[" + from + " - " + to + "] : " + type);
          }

          intervals.add(new SunTimeInterval(from, to, type));
        }
      }

      // merge contiguous intervals (already ordered):
      Range.union(this.nightLimits);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("nightLimits : " + this.nightLimits);
      }
    }

    this.data.setSunIntervals(intervals);
  }

  /**
   * Convert a JD range to a date interval with respect for the LST range [0;24]
   * Note : due to HA limit [+/-12h], the converted JD / Date ranges
   * can have a discontinuity on the date axis !
   *
   * @param rangeJD range to convert
   * @param intervals interval list where new date intervals will be added
   */
  private void convertRangeToDateInterval(final Range rangeJD, final List<DateTimeInterval> intervals) {
    final double jdStart = rangeJD.getMin();
    final double jdEnd = rangeJD.getMax();

    if (jdStart >= this.jdLower) {

      if (jdEnd <= this.jdUpper) {

        // single interval [jdStart;jdEnd]
        intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart), jdToDateInDateRange(jdEnd)));

      } else {

        if (jdStart > this.jdUpper) {
          // two points over LST 24 :

          // single interval [jdStart - day;jdEnd - day]
          intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart - AstroSkyCalc.LST_DAY_IN_JD),
                  jdToDateInDateRange(jdEnd - AstroSkyCalc.LST_DAY_IN_JD)));

        } else {
          // end occurs after LST 24 :

          // interval [jdStart;jdLst24]
          intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart), this.data.getDateMax()));

          // add the second interval [jdLst0;jdEnd - day]
          intervals.add(new DateTimeInterval(this.data.getDateMin(), jdToDateInDateRange(jdEnd - AstroSkyCalc.LST_DAY_IN_JD)));
        }
      }

    } else {
      // start occurs before LST 0h :

      if (jdEnd < this.jdLower) {
        // two points before LST 0h :

        // single interval [jdStart + day;jdEnd + day]
        intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart + AstroSkyCalc.LST_DAY_IN_JD),
                jdToDateInDateRange(jdEnd + AstroSkyCalc.LST_DAY_IN_JD)));

      } else {
        // interval [jdLst0;jdEnd]
        intervals.add(new DateTimeInterval(this.data.getDateMin(), jdToDateInDateRange(jdEnd)));

        // add the second interval [jdStart + day;jdLst24]
        intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart + AstroSkyCalc.LST_DAY_IN_JD), this.data.getDateMax()));
      }
    }
  }

  /**
   * Convert a JD date to a date within LST range [0;24]
   *
   * @param jd date to convert
   * @return date
   */
  private Date convertJDToDate(final double jd) {
    if (jd >= this.jdLower) {

      if (jd <= this.jdUpper) {

        // date in [jdLst0;jdLst24]
        return jdToDateInDateRange(jd);

      } else {
        // over LST 24 :

        // return [jd - day]
        return jdToDateInDateRange(jd - AstroSkyCalc.LST_DAY_IN_JD);
      }

    } else {
      // start occurs before LST 0h :

      // return [jd + day]
      return jdToDateInDateRange(jd + AstroSkyCalc.LST_DAY_IN_JD);
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
  private Date jdToDateInDateRange(final double jd) {
    // adjust range limits :
    if (jd <= this.jdLower) {
      return this.data.getDateMin();
    } else {
      if (jd >= this.jdUpper) {
        return this.data.getDateMax();
      }
      return jdToDate(jd);
    }
  }

  /**
   * Convert a JD value to a Date Object (LST or UTC)
   * @see #useLST
   * @param jd julian day
   * @return Date Object (LST or UTC)
   */
  private Date jdToDate(final double jd) {
    return this.sc.toDate(jd, this.useLST);
  }

  /**
   * Generate a target list for the base line limits (every 5 deg)
   * @return target list
   */
  private List<Target> generateTargetsForBaseLineLimits() {

    final double obsLat = Math.toDegrees(this.interferometer.getPosSph().getLatitude());

    final int decStep = 2;

    int decMin = decStep * (int) Math.round((obsLat - 90d + this.minElev) / decStep);
    decMin = Math.max(decMin, -90);

    int decMax = decStep * (int) Math.round((obsLat + 90 - this.minElev) / decStep);
    decMax = Math.min(decMax, 90);

    final List<Target> targets = new ArrayList<Target>((decMax - decMin) / decStep + 1);
    Target t;
    for (int i = decMax; i >= decMin; i -= decStep) {
      t = new Target();
      // delta = n (deg)
      t.setName("\u0394 = " + Integer.toString(i));
      // 12:00:00
      t.setRADeg(180d);
      t.setDECDeg(i);
      t.setEQUINOX(AsproConstants.EPOCH_J2000);

      targets.add(t);
    }
    return targets;
  }

  /**
   * Define elevation marks on observability ranges for the current target
   * @param starObs star observability data
   * @param obsRangeJD observability ranges in JD
   * @param precRA precessed RA
   * @param precDEC precessed DEC
   */
  private void findElevations(final StarObservabilityData starObs, final List<Range> obsRangeJD, final double precRA, final double precDEC) {
    final List<ElevationDate> elevations = starObs.getElevations();

    int elev;
    double jd;
    double haElev;

    final int minElevation = (int) Math.round(this.minElev);

    // internal ticks for elevation :
    for (elev = 20; elev <= 80; elev += 20) {
      if (elev > minElevation) {
        haElev = this.sco.getHAForElevation(precDEC, elev);

        if (haElev > 0) {
          jd = this.sc.convertHAToJD(-haElev, precRA);
          if (Range.contains(obsRangeJD, jd)) {
            elevations.add(new ElevationDate(convertJDToDate(jd), elev));
          }

          jd = this.sc.convertHAToJD(haElev, precRA);
          if (Range.contains(obsRangeJD, jd)) {
            elevations.add(new ElevationDate(convertJDToDate(jd), elev));
          }
        }
      }
    }

    final AzEl azEl = new AzEl();

    // tick for transit :
    jd = this.sc.convertHAToJD(0d, precRA);
    if (Range.contains(obsRangeJD, jd)) {
      this.sco.getTargetPosition(jd, azEl);
      elev = (int) Math.round(azEl.getElevation());
      elevations.add(new ElevationDate(convertJDToDate(jd), elev));
    }

    // ticks for observability intervals (limits) :
    for (Range range : obsRangeJD) {
      jd = range.getMin();
      this.sco.getTargetPosition(jd, azEl);
      elev = (int) Math.round(azEl.getElevation());
      elevations.add(new ElevationDate(convertJDToDate(jd), elev));

      jd = range.getMax();
      this.sco.getTargetPosition(jd, azEl);
      elev = (int) Math.round(azEl.getElevation());
      elevations.add(new ElevationDate(convertJDToDate(jd), elev));
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("elevations : ");
      for (ElevationDate elevationDate : elevations) {
        logger.fine(elevationDate.toString());
      }
    }
  }

  /**
   * Convert the given list of HA ranges to date intervals in LST (used by Export OB only)
   *
   * TODO : check that code when LST / night center is done!
   *
   * Note : date intervals that are over (00:00 or 24:00) are merged. 
   * For example : 22:00->24:00 and 00:00->01:00 returns 22:00->01:00
   *
   * @see fr.jmmc.aspro.ob.ExportOBVLTI#processDateTime(String, ObservationSetting, Target)
   * 
   * @param ranges HA ranges
   * @param precRA precessed target right ascension in decimal hours
   * @return date intervals
   */
  public List<DateTimeInterval> convertHARangesToDateInterval(final List<Range> ranges, final double precRA) {
    if (ranges != null) {
      final List<Range> jdRanges = new ArrayList<Range>(ranges.size());
      for (Range range : ranges) {
        jdRanges.add(this.sc.convertHAToJDRange(range, precRA));
      }
      
      final List<DateTimeInterval> dateIntervals = new ArrayList<DateTimeInterval>(jdRanges.size() + 2);
      // convert JD ranges to date ranges :
      for (Range range : jdRanges) {
        convertRangeToDateInterval(range, dateIntervals);
      }
      if (dateIntervals.size() > 1) {
        // merge contiguous date ranges :
        DateTimeInterval.merge(dateIntervals);
      }

      // Replace the 24:00:00 date by 00:00:00 to merge contiguous intervals in LST :
      final Date lst0 = getData().getDateMin();
      final Date lst24 = getData().getDateMax();
      
      boolean found = false;
      DateTimeInterval interval;
      for (final ListIterator<DateTimeInterval> it = dateIntervals.listIterator(); it.hasNext();) {
        interval = it.next();
        if (lst24.equals(interval.getEndDate())) {
          found = true;
          it.set(new DateTimeInterval(interval.getStartDate(), lst0));
        }
      }

      if (found && dateIntervals.size() > 1) {
        // sort the intervals in reverse order :
        Collections.sort(dateIntervals);
        Collections.reverse(dateIntervals);

        // merge contiguous date ranges :
        DateTimeInterval.mergeSorted(dateIntervals);

        if (dateIntervals.size() > 1) {
          Collections.sort(dateIntervals);
        }
      }

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("dateIntervals = " + dateIntervals);
      }
      return dateIntervals;
    }
    return null;
  }

  /**
   * Return the jd at the middle of the range [jdLower; jdUpper]
   * @return jd at the middle of the range
   */
  private double jdCenter() {
    return (this.jdLower + this.jdUpper) / 2d;
  }
  
  /**
   * Return the observability data (available after compute has been invoked)
   * @return observability data
   */
  public ObservabilityData getData() {
    return this.data;
  }
}