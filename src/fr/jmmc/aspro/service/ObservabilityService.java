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
import fr.jmmc.aspro.model.HorizonShape;
import fr.jmmc.aspro.model.ObservabilityContext;
import fr.jmmc.aspro.model.observability.DateTimeInterval;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.observability.PopCombination;
import fr.jmmc.aspro.model.observability.GroupedPopObservabilityData;
import fr.jmmc.aspro.model.observability.PopObservabilityData;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.observability.TargetPositionDate;
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
import fr.jmmc.aspro.model.oi.MoonPointingRestriction;
import fr.jmmc.aspro.model.oi.MoonRestriction;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Pop;
import fr.jmmc.aspro.model.oi.PopLink;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.StationLinks;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.Telescope;
import fr.jmmc.oitools.util.CombUtils;
import fr.jmmc.aspro.util.TestUtils;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import javax.xml.datatype.XMLGregorianCalendar;

/**
 * This service determines the observability of a list of targets given an observation setting
 *
 * @author bourgesl
 */
public final class ObservabilityService {

  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(ObservabilityService.class.getName());
  /** flag to slow down the service to detect concurrency problems */
  private final static boolean DEBUG_SLOW_SERVICE = false;
  /** number of best Pops displayed in warnings */
  private final static int MAX_POPS_IN_WARNING = 15;
  /** jd step = 1 minute */
  private final double JD_STEP = (1d / 60d) / 24d;

  /* members */
  /** cached log debug enabled */
  private final boolean isLogDebug = logger.isDebugEnabled();

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
  /** double formatter for moon separation */
  private final NumberFormat df1 = new DecimalFormat("0.0");
  /** 24h date formatter like in france */
  private final DateFormat timeFormatter = DateFormat.getTimeInstance(DateFormat.SHORT, Locale.FRANCE);
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
  /** flag to indicate that wind restriction is used */
  private boolean hasWindRestriction = false;
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
  /** create the observability context (temporary variables) */
  private ObservabilityContext obsCtx = null;
  /** azimuth expressed in [0; 360 deg] */
  private Double windAzimuth = null;
  /** discarded azimuth ranges due to wind direction */
  private List<Range> azimuthRanges = null;
  /** optional moon pointing restrictions */
  private MoonPointingRestriction moonPointingRestriction = null;

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
    if (isLogDebug) {
      logger.debug("\n\n--------------------------------------------------------------------------------\n\n");
      logger.debug("compute: {}", this.observation);
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
      logger.debug("No target defined.");
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
      if (isLogDebug) {
        logger.debug("Star observability intervals:");

        for (List<StarObservabilityData> soList : this.data.getMapStarVisibilities().values()) {
          for (StarObservabilityData so : soList) {
            logger.debug(so.toString());
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

    if (logger.isInfoEnabled()) {
      logger.info("compute : duration = {} ms.", 1e-6d * (System.nanoTime() - start));
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

    if (isLogDebug) {
      logger.debug("jdLst0:  {}", this.jdLower);
      logger.debug("jdLst24: {}", this.jdUpper);
    }

    this.data.setJdMin(this.jdLower);
    this.data.setJdMax(this.jdUpper);

    this.data.setDateMin(jdToDate(this.jdLower));
    this.data.setDateMax(jdToDate(this.jdUpper));

    if (isLogDebug) {
      logger.debug("date min: {}", this.data.getDateMin());
      logger.debug("date max: {}", this.data.getDateMax());
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

        if (isLogDebug) {
          logger.debug("jdMidnight: {}", jdMidnight);
        }

        // adjust the jd bounds :
        this.jdLower = jdMidnight - AstroSkyCalc.HALF_LST_DAY_IN_JD;
        this.jdUpper = jdMidnight + AstroSkyCalc.HALF_LST_DAY_IN_JD;

        if (isLogDebug) {
          logger.debug("jdLower: {}", this.jdLower);
          logger.debug("jdUpper: {}", this.jdUpper);
        }

        this.data.setJdMin(this.jdLower);
        this.data.setJdMax(this.jdUpper);

        this.data.setDateMin(jdToDate(this.jdLower));
        this.data.setDateMax(jdToDate(this.jdUpper));

        if (isLogDebug) {
          logger.debug("date min: {}", this.data.getDateMin());
          logger.debug("date max: {}", this.data.getDateMax());
        }
      }

      //  sun rise/set with twilight : see NightlyAlmanac
      final AstroAlmanac almanac = this.sc.getAlmanac();

      // get sun times arround midnight (-1 day; + 2 days):
      final List<AstroAlmanacTime> sunTimes = new ArrayList<AstroAlmanacTime>(almanac.getSunTimes());

      // Extract night limits in [LST0 -12; LST0 + 36] and sun intervals in range [jdLower; jdUpper]:
      processSunAlmanach(sunTimes);

      // moon rise/set arround night:
      final List<Range> moonRiseRanges = this.sc.findMoonRiseSet(almanac, this.jdLower, this.jdUpper);

      if (isLogDebug) {
        logger.debug("moonRiseRanges: {}", moonRiseRanges);
      }

      // compute moon illumination over night:
      final List<Range> obsMoonRanges = new ArrayList<Range>(6);
      obsMoonRanges.addAll(this.nightLimits);
      obsMoonRanges.addAll(moonRiseRanges);

      final List<Range> moonRanges = Range.intersectRanges(obsMoonRanges, 2);

      if (isLogDebug) {
        logger.debug("moonRanges: {}", moonRanges);
      }

      // Moon filters:
      // 1- use ranges in  [LST0 -12; LST0 + 36] over night ranges
      // 2- moon Illum in jdLower / jdUpper
      final double moonIllum = this.sc.getMaxMoonIllum(moonRanges);

      this.data.setMoonIllumPercent(100d * moonIllum);

      if (isLogDebug) {
        logger.debug("moon illum: {}", moonIllum);
      }
    }
  }

  /**
   * Find the observability ranges for the complete list of targets
   * @param targets target list
   */
  private void findObservability(final List<Target> targets) {
    if (this.hasPops) {
      // show baseline and beams in warnings:
      final StringBuilder sbConf = new StringBuilder(64);
      sbConf.append("Baseline: ");
      for (Beam b : this.beams) {
        sbConf.append(b.getStation().getName()).append(" ");
      }

      sbConf.append("- Beams: ");
      for (Beam b : this.beams) {
        sbConf.append(b.getChannel().getName()).append(" ");
      }

      if (this.popCombinations.size() == 1) {
        // user defined Pop combination:
        sbConf.append("- PoPs: ").append(this.popCombinations.get(0).getIdentifier());
      }

      addWarning(sbConf.toString());

      // PoPs : Compatible Mode if no user defined Pop Combination :
      if (targets.size() > 1 && this.popCombinations.size() > 1) {
        // Objective : find the pop combination that maximize the observability of the complete list of target

        final PopCombination bestPopCombination = findCompatiblePoPs(targets);

        if (bestPopCombination != null) {
          this.data.setBestPops(bestPopCombination);

          // use the user defined PoPs configuration :
          this.popCombinations.clear();
          this.popCombinations.add(bestPopCombination);

          // Use arrays instead of List for performance:
          this.obsCtx.setPopCombs(new PopCombination[1]);
          this.popCombinations.toArray(this.obsCtx.getPopCombs());
        }
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

    final int capacity = Math.max(32, this.popCombinations.size() / 5);
    final Map<String, GroupedPopObservabilityData> popMap = new HashMap<String, GroupedPopObservabilityData>(capacity);

    final int nTargets = targets.size();

    int nObsTarget = 0;

    // pop data per target
    List<PopObservabilityData> targetPopDataList;

    GroupedPopObservabilityData popMergeData;
    List<PopObservabilityData> flatPopDataList;
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
    if (isLogDebug) {
      logger.debug("Complete GroupedPopData : ");
      for (GroupedPopObservabilityData pm : popMergeList) {
        logger.debug(pm.toString());
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

    if (isLogDebug) {
      logger.debug("Observable targets : max: {} - total: ", maxObsTarget, nObsTarget);
    }

    // filter to keep only results for all valid targets :
    for (final Iterator<GroupedPopObservabilityData> it = popMergeList.iterator(); it.hasNext();) {
      popMergeData = it.next();
      if (popMergeData.getPopDataList().size() != maxObsTarget) {
        it.remove();
      }
    }

    if (popMergeList.isEmpty()) {
      addWarning("Impossible to find a PoPs combination compatible with all targets !");
    } else {

      if (isLogDebug) {
        logger.debug("Filtered GroupedPopData : ");
        for (GroupedPopObservabilityData pm : popMergeList) {
          logger.debug(pm.toString());
        }
      }

      // estimator to maximize observability for all observable targets :
      for (GroupedPopObservabilityData pm : popMergeList) {
        pm.estimateData();
      }

      // Sort pop merge data according to its estimator :
      Collections.sort(popMergeList);

      if (isLogDebug) {
        logger.debug("Sorted GroupedPopData with estimation : ");
        for (GroupedPopObservabilityData pm : popMergeList) {
          logger.debug(pm.toString());
        }
      }

      // maximum length for this Pop Combination :
      final int end = popMergeList.size() - 1;

      final GroupedPopObservabilityData popBestData = popMergeList.get(end);

      if (popMergeList.size() > 1) {
        final List<GroupedPopObservabilityData> bestPoPs = new ArrayList<GroupedPopObservabilityData>(MAX_POPS_IN_WARNING);

        final StringBuilder sbBestPops = new StringBuilder(128);
        final StringBuilder sbBetterPops = new StringBuilder(128);
        sbBestPops.append("Equivalent Best PoPs found: ");

        // find all equivalent pop combinations :
        for (int i = end, n = 0; i >= 0 && n < MAX_POPS_IN_WARNING; i--, n++) {
          GroupedPopObservabilityData pm = popMergeList.get(i);
          if (popBestData.getEstimation() == pm.getEstimation()) {
            sbBestPops.append(pm.getPopCombination().getIdentifier()).append(" ");
            bestPoPs.add(pm);
          } else {
            sbBetterPops.append(pm.getPopCombination().getIdentifier()).append(" ");
            bestPoPs.add(pm);
          }
        }

        if (isLogDebug) {
          logger.debug("best PoPs: {}", bestPoPs);
        }

        addWarning(sbBestPops.toString());

        if (sbBetterPops.length() > 0) {
          sbBetterPops.insert(0, "Next good PoPs: ");
          addWarning(sbBetterPops.toString());
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

        Range rangeHA;
        for (Range rangeJD : this.nightLimits) {
          rangeHA = this.sc.convertJDToHARange(rangeJD, precRA);
          if (rangeHA != null) {
            haNightLimits.add(rangeHA);
          }
        }

        if (isLogDebug) {
          logger.debug("HA night limits: {}", haNightLimits);
        }
        rangesTarget.addAll(haNightLimits);
      }

      popDataList = getPopObservabilityData(target.getName(), Math.toRadians(precDEC), rangesTarget, true);

    } else {
      if (isLogDebug) {
        logger.debug("Target never rise: {}", target);
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

    if (isLogDebug) {
      logger.debug("targetName: {}", targetName);
    }

    final int listSize = (this.doDetailedOutput) ? (4 + this.baseLines.size()) : 1;
    final List<StarObservabilityData> starVisList = new ArrayList<StarObservabilityData>(listSize);
    this.data.addStarVisibilities(targetName, starVisList);

    final StarObservabilityData starObs = new StarObservabilityData(targetName, StarObservabilityData.TYPE_STAR);
    // add the result to have also unobservable targets :
    starVisList.add(starObs);

    final StarData starData = new StarData(target.getName());
    this.data.addStarData(starData);

    // get Target coordinates precessed to JD CENTER and define target to get later az/alt positions from JSkyCalc :
    final double[] raDec = this.sco.defineTarget(jdCenter(), target.getRADeg(), target.getDECDeg());

    // precessed target right ascension in decimal hours :
    final double precRA = raDec[0];

    // precessed target declination in degrees :
    final double precDEC = raDec[1];

    if (isLogDebug) {
      logger.debug("target[{}] {} - precessed: {}", target.getName(),
                AstroSkyCalcObservation.asString(target.getRADeg(), target.getDECDeg()),
                AstroSkyCalcObservation.asString(15d * precRA, precDEC));
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

      if (isLogDebug) {
        logger.debug("rangeHARiseSet: {}", rangeHARiseSet);
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

          Range rangeHA;
          for (Range rangeJD : this.nightLimits) {
            rangeHA = this.sc.convertJDToHARange(rangeJD, precRA);
            if (rangeHA != null) {
              haNightLimits.add(rangeHA);
            }
          }

          if (isLogDebug) {
            logger.debug("HA night limits: {}", haNightLimits);
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

      // For now : only VLTI/CHARA has horizon profiles :
      List<Range> rangesJDHorizon = null;
      if (this.hasHorizon) {
        // check horizon profiles inside rise/set range :
        rangesJDHorizon = checkHorizonProfile(precDEC, rangeJDRiseSet);

        if (isLogDebug) {
          logger.debug("rangesJDHz: {}", rangesJDHorizon);
        }

        // fast interrupt :
        if (this.currentThread.isInterrupted()) {
          return;
        }
      }

      // Check Moon restriction:
      List<Range> rangesJDMoon = null;
      if (this.useNightLimit && this.moonPointingRestriction != null) {
        rangesJDMoon = checkMoonRestriction(targetName, precDEC, rangeJDRiseSet);

        if (isLogDebug) {
          logger.debug("rangesJDMoon: {}", rangesJDMoon);
        }

        // fast interrupt :
        if (this.currentThread.isInterrupted()) {
          return;
        }
      }

      // Check wind restriction:
      List<Range> rangesJDWind = null;
      if (this.hasWindRestriction) {
        // check target azimuth inside rise/set range :
        rangesJDWind = checkWindRestriction(precDEC, rangeJDRiseSet);

        if (isLogDebug) {
          logger.debug("rangesJDWind: {}", rangesJDWind);
        }

        // fast interrupt :
        if (this.currentThread.isInterrupted()) {
          return;
        }
      }

      // observable ranges (jd) :
      final List<Range> obsRanges = new ArrayList<Range>(13);

      if (this.doDetailedOutput) {

        // Add Rise/Set :
        final StarObservabilityData soRiseSet = new StarObservabilityData(targetName, "Rise/Set", StarObservabilityData.TYPE_RISE_SET);
        // get target position (ha, az, el) at range boundaries:
        getTargetPosition(soRiseSet, Arrays.asList(new Range[]{rangeJDRiseSet}), precRA, precDEC, false);
        starVisList.add(soRiseSet);

        // convert JD ranges to date ranges :
        convertRangeToDateInterval(rangeJDRiseSet, soRiseSet.getVisible());
        if (soRiseSet.getVisible().size() > 1) {
          // merge contiguous date ranges :
          DateTimeInterval.merge(soRiseSet.getVisible());
        }

        if (rangesJDHorizon != null) {
          // Add Horizon :
          final StarObservabilityData soHz = new StarObservabilityData(targetName, "Horizon", StarObservabilityData.TYPE_HORIZON);
          // get target position (ha, az, el) at range boundaries:
          getTargetPosition(soHz, rangesJDHorizon, precRA, precDEC, false);
          starVisList.add(soHz);

          // convert JD ranges to date ranges :
          for (Range range : rangesJDHorizon) {
            convertRangeToDateInterval(range, soHz.getVisible());
          }
          if (soHz.getVisible().size() > 1) {
            // merge contiguous date ranges :
            DateTimeInterval.merge(soHz.getVisible());
          }
        }

        if (rangesJDMoon != null) {
          // Add Moon separation :
          final StarObservabilityData soMoon = new StarObservabilityData(targetName, "Moon Sep.", StarObservabilityData.TYPE_MOON_DIST);
          // get target position (ha, az, el) at range boundaries:
          getTargetPosition(soMoon, rangesJDMoon, precRA, precDEC, false);
          starVisList.add(soMoon);

          // convert JD ranges to date ranges :
          for (Range range : rangesJDMoon) {
            convertRangeToDateInterval(range, soMoon.getVisible());
          }
          if (soMoon.getVisible().size() > 1) {
            // merge contiguous date ranges :
            DateTimeInterval.merge(soMoon.getVisible());
          }
        }

        if (rangesJDWind != null) {
          // Add Horizon :
          final StarObservabilityData soWind = new StarObservabilityData(targetName, "Wind", StarObservabilityData.TYPE_WIND);
          // get target position (ha, az, el) at range boundaries:
          getTargetPosition(soWind, rangesJDWind, precRA, precDEC, false);
          starVisList.add(soWind);

          // convert JD ranges to date ranges :
          for (Range range : rangesJDWind) {
            convertRangeToDateInterval(range, soWind.getVisible());
          }
          if (soWind.getVisible().size() > 1) {
            // merge contiguous date ranges :
            DateTimeInterval.merge(soWind.getVisible());
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
            if (isLogDebug) {
              logger.debug("baseLine: {}", baseLine);
              logger.debug("JD ranges: {}", obsRanges);
            }

            soBl = new StarObservabilityData(targetName, baseLine.getName(), StarObservabilityData.TYPE_BASE_LINE + i);
            // get target position (ha, az, el) at range boundaries:
            getTargetPosition(soBl, obsRanges, precRA, precDEC, false);
            starVisList.add(soBl);

            // convert JD ranges to date ranges :
            for (Range range : obsRanges) {
              convertRangeToDateInterval(range, soBl.getVisible());
            }
            if (soBl.getVisible().size() > 1) {
              // merge contiguous date ranges :
              DateTimeInterval.merge(soBl.getVisible());
            }

            if (isLogDebug) {
              logger.debug("Date ranges: {}", soBl.getVisible());
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

      // Intersect with wind ranges :
      if (this.hasWindRestriction) {
        obsRanges.addAll(rangesJDWind);
        nValid++;
      }

      // Intersect with moon separation ranges :
      if (rangesJDMoon != null) {
        obsRanges.addAll(rangesJDMoon);
        nValid++;
      }

      // Intersect with night limits :
      if (this.useNightLimit) {
        obsRanges.addAll(this.nightLimits);
        nValid++;
      }

      if (isLogDebug) {
        logger.debug("obsRanges: {}", obsRanges);
      }

      // finally : merge intervals :
      final List<Range> finalRanges = Range.intersectRanges(obsRanges, nValid);

      if (isLogDebug) {
        logger.debug("finalRanges: {}", finalRanges);
      }

      // store merge result as date intervals :
      if (finalRanges != null) {
        // get detailled target position (ha, az, el) for the current target (ticks):
        getTargetPosition(starObs, finalRanges, precRA, precDEC, true);

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

        Range rangeHA;
        for (Range rangeJD : finalRanges) {
          rangeHA = this.sc.convertJDToHARange(rangeJD, precRA);
          if (rangeHA != null) {
            haObsRanges.add(rangeHA);
          }
        }

        if (isLogDebug) {
          logger.debug("HA observability: {}", haObsRanges);
        }
        starData.setObsRangesHA(haObsRanges);
      } else {
        if (isLogDebug) {
          logger.debug("Target not observable: {}", target);
        }
        addWarning("The target [" + targetName + "] is not observable");
      }

    } else {
      if (isLogDebug) {
        logger.debug("Target never rise: {}", target);
      }
      addWarning("The target [" + targetName + "] is not observable (never rise)");
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

    // flag to have extra information about DL (even if unobservable target):
//    final boolean doSkipDL = !this.doDetailedOutput;
    final boolean doSkipDL = true;

    // First Pass :
    // For all PoP combinations : find the HA interval merged with the HA Rise/set interval
    // list of observability data associated to a pop combination :
    final List<PopObservabilityData> popDataList = getPopObservabilityData(starObs.getTargetName(), dec, rangesTarget, doSkipDL);

    // Current pop observability :
    PopObservabilityData popData;

    // Find the PoP combination that gives the longest HA interval :
    if (popDataList != null && !popDataList.isEmpty()) {

      if (this.popCombinations.size() > 1) {
        // Sort pop observability data according to max length :
        Collections.sort(popDataList);

        if (isLogDebug) {
          logger.debug("Sorted PopData: {}", popDataList);
        }
      }

      // maximum length for this Pop Combination :
      final int end = popDataList.size() - 1;

      final PopObservabilityData popBestData = popDataList.get(end);

      if (this.popCombinations.size() > 1) {
        final List<PopObservabilityData> bestPoPs = new ArrayList<PopObservabilityData>(MAX_POPS_IN_WARNING);

        final StringBuilder sbBestPops = new StringBuilder(128);
        final StringBuilder sbBetterPops = new StringBuilder(128);
        sbBestPops.append("Equivalent Best PoPs found: ");

        // find all equivalent pop combinations :
        for (int i = end, n = 0; i >= 0 && n < MAX_POPS_IN_WARNING; i--, n++) {
          popData = popDataList.get(i);
          if (popBestData.getMaxLength() == popData.getMaxLength()) {
            sbBestPops.append(popData.getPopCombination().getIdentifier()).append(" ");
            bestPoPs.add(popData);
          } else {
            sbBetterPops.append(popData.getPopCombination().getIdentifier()).append(" ");
            bestPoPs.add(popData);
          }
        }

        if (isLogDebug) {
          logger.debug("best PoPs: {}", bestPoPs);
        }

        addWarning(sbBestPops.toString());

        if (sbBetterPops.length() > 0) {
          sbBetterPops.insert(0, "Next good PoPs: ");
          addWarning(sbBetterPops.toString());
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
   * @param doSkipDL skip pop combination if any DL is unobservable (useful for performance and detailed output)
   * @return intervals (hour angles) or null if thread interrupted
   */
  private List<PopObservabilityData> getPopObservabilityData(final String targetName, final double dec, final List<Range> rangesTarget,
          final boolean doSkipDL) {
    final double cosDec = Math.cos(dec);
    final double sinDec = Math.sin(dec);

    // Use arrays instead of List for performance:
    final PopCombination[] popCombs = this.obsCtx.getPopCombs();
    final BaseLine[] bls = this.obsCtx.getBaseLines();
    final Range[] wranges = this.obsCtx.getWRanges();

    final int sizeCb = popCombs.length;
    final int sizeBL = bls.length;

    // For all PoP combinations : find the HA interval merged with the HA Rise/set interval
    // list of observability data associated to a pop combination :
    final List<PopObservabilityData> popDataList = new ArrayList<PopObservabilityData>(20);
    // Current pop observability :
    PopObservabilityData popData;

    // list of HA ranges for all base lines :
    final List<List<Range>> rangesBL = new ArrayList<List<Range>>(sizeBL);

    // Current list of HA ranges for a base line :
    List<Range> ranges;

    // w range using the pop offset for a given base line :
    final Range wRangeWithOffset = new Range();

    final double[] ha = new double[2];
    final double[] haValues = new double[6];

    BaseLine bl;
    double[] wExtrema;

    // precompute W extrema per baseline:
    final double[][] wExtremas = new double[sizeBL][2];

    // For every Base Line :
    for (int i = 0; i < sizeBL; i++) {
      bl = bls[i];

      wExtrema = DelayLineService.findWExtrema(cosDec, sinDec, bl);

      if (isLogDebug) {
        logger.debug("wExtrema[{}] = {}", bl.getName(), Arrays.toString(wExtrema));
      }

      wExtremas[i] = wExtrema;
    }

    Range wRange;
    Double offset;

    PopCombination popComb;
    double[] popOffsets;

    // flag to skip DL evaluation when the target is not observable for at least one DL
    boolean skip;

    // For every Pop Combination :
    for (int k = 0; k < sizeCb; k++) {
      popComb = popCombs[k];
      popOffsets = popComb.getPopOffsets();

      // fast interrupt :
      if (this.currentThread.isInterrupted()) {
        return null;
      }

      skip = false;

      // For every Base Line :
      for (int i = 0; i < sizeBL; i++) {
        bl = bls[i];
        wExtrema = wExtremas[i];
        wRange = wranges[i];
        offset = popOffsets[i];

        // adjust w range with the current pop combination's Offset :
        wRangeWithOffset.setMin(wRange.getMin() + offset.doubleValue());
        wRangeWithOffset.setMax(wRange.getMax() + offset.doubleValue());

        ranges = DelayLineService.findHAIntervalsForBaseLine(cosDec, sinDec, bl, wExtrema, wRangeWithOffset, ha, haValues);

        if (ranges.isEmpty()) {
          // this base line is incompatible with that W range :
          if (doSkipDL) {
            skip = true;
            break;
          }
        }
        rangesBL.add(ranges);
      }

      if (!skip) {
        popData = new PopObservabilityData(targetName, popComb, new ArrayList<List<Range>>(rangesBL));

        // note : rangesTarget contains both rise/set intervals + night limits in HA

        this.obsCtx.resetFlagRanges();
        this.obsCtx.addInFlatRangeLimits(rangesTarget);

        // merge the baseline ranges with the target intervals :
        popData.computeMaxLength(sizeBL + 1 + ((this.useNightLimit) ? 1 : 0), this.obsCtx);

        if (popData.getMaxLength() > 0d || !doSkipDL) {
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
   * @param precDEC precessed DEC in degrees
   * @param jdRiseSet target rise/set range (JD)
   * @return list of observable ranges (no obstruction) or null if thread interrupted
   */
  private List<Range> checkHorizonProfile(final double precDEC, final Range jdRiseSet) {
    // output :
    final List<Range> ranges = new ArrayList<Range>(3);

    // Prepare profiles :
    final HorizonService hs = HorizonService.getInstance();

    final int nBeams = this.beams.size();
    final HorizonShape[] profiles = new HorizonShape[nBeams];

    for (int i = 0; i < nBeams; i++) {
      final Beam b = this.beams.get(i);
      profiles[i] = hs.getProfile(this.interferometer.getName(), b.getStation());
    }

    // prepare cosDec/sinDec:
    final double dec = Math.toRadians(precDEC);
    final double cosDec = Math.cos(dec);
    final double sinDec = Math.sin(dec);

    final double jdMin = jdRiseSet.getMin();
    final double jdMax = jdRiseSet.getMax();

    final AzEl azEl = new AzEl();
    boolean visible;
    boolean last = false;

    Range range = new Range();

    for (double jd = jdMin, jdIn; jd < jdMax; jd += JD_STEP) {

      // fix JD in LST range [0; 24] in order to have accurate target position:
      jdIn = getJDInLstRange(jd);

      this.sco.getTargetPosition(cosDec, sinDec, jdIn, azEl);

      visible = true;

      // For every beam (station) :
      // check if there is no horizon obstruction :
      for (final HorizonShape profile : profiles) {
        if (!hs.checkProfile(profile, azEl.getAzimuth(), azEl.getElevation())) {
          visible = false;

          if (isLogDebug) {
            logger.debug("Target hidden by horizon profile = {} [{} {}]",
                    profile.getName(), azEl.getAzimuth(), azEl.getElevation());
          }

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
   * Check the wind restriction given the target rise/set range (JD)
   * @param precDEC precessed DEC in degrees
   * @param jdRiseSet target rise/set range (JD)
   * @return list of observable ranges (no obstruction) or null if thread interrupted
   */
  private List<Range> checkWindRestriction(final double precDEC, final Range jdRiseSet) {
    // output :
    final List<Range> ranges = new ArrayList<Range>(2);

    // prepare cosDec/sinDec:
    final double dec = Math.toRadians(precDEC);
    final double cosDec = Math.cos(dec);
    final double sinDec = Math.sin(dec);

    final double jdMin = jdRiseSet.getMin();
    final double jdMax = jdRiseSet.getMax();

    final AzEl azEl = new AzEl();
    boolean visible;
    boolean last = false;

    Range range = new Range();

    for (double jd = jdMin, jdIn; jd < jdMax; jd += JD_STEP) {

      // fix JD in LST range [0; 24] in order to have accurate target position:
      jdIn = getJDInLstRange(jd);

      this.sco.getTargetPosition(cosDec, sinDec, jdIn, azEl);

      visible = true;

      // Check pointing restrictions according to the wind direction:
      if (Range.contains(this.azimuthRanges, azEl.getAzimuth())) {

        if (isLogDebug) {
          logger.debug("Target pointing discarded by wind direction = {} [{}]", azEl.getAzimuth(), this.windAzimuth);
        }

        visible = false;
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
   * Check the moon restriction given the target rise/set range (JD) (FLI threshold and object magnitude)
   * @param targetName target name
   * @param precDEC precessed DEC in degrees
   * @param jdRiseSet target rise/set range (JD)
   * @return list of observable ranges (no restriction) or null if thread interrupted
   */
  private List<Range> checkMoonRestriction(final String targetName, final double precDEC, final Range jdRiseSet) {
    final double warningThreshold = this.moonPointingRestriction.getWarningThreshold();

    // get moon restriction rules as array:
    final MoonRestriction[] moonRestrictions = new MoonRestriction[this.moonPointingRestriction.getRestrictions().size()];
    this.moonPointingRestriction.getRestrictions().toArray(moonRestrictions);

    // get FLI on current night:
    final double fli = this.data.getMoonIllumPercent();

    // Add 5% margin for quick check:
    final double threshold = warningThreshold * 1.05d;

    // output :
    final List<Range> ranges = new ArrayList<Range>(2);

    // prepare cosDec/sinDec:
    final double dec = Math.toRadians(precDEC);
    final double cosDec = Math.cos(dec);
    final double sinDec = Math.sin(dec);

    final double jdMin = jdRiseSet.getMin();
    final double jdMax = jdRiseSet.getMax();
    final double jdMid = 0.5d * (jdMin + jdMax);

    boolean doCheck = false;

    // First check if moon and target are close enough to check for the complete rise/set interval:
    double separation;

    // check at jd min:
    separation = getMoonSeparation(cosDec, sinDec, jdMin);

    if (separation <= threshold) {
      doCheck = true;
    } else {
      // check at jd mid:
      separation = getMoonSeparation(cosDec, sinDec, jdMid);

      if (separation <= threshold) {
        doCheck = true;
      } else {
        // check at jd max:
        separation = getMoonSeparation(cosDec, sinDec, jdMax);

        if (separation <= threshold) {
          doCheck = true;
        }
      }
    }

    if (doCheck) {
      // 0.5 arcmin for uncertainty:
      final double margin = 0.5d / 60d;

      Double ruleFli;
      double minSeparation = Double.POSITIVE_INFINITY;
      double minJd = 0d;

      boolean visible;
      boolean last = false;

      Range range = new Range();

      for (double jd = jdMin; jd < jdMax; jd += JD_STEP) {

        visible = true;

        // check at jd:
        separation = getMoonSeparation(cosDec, sinDec, jd);

        if (separation < minSeparation) {
          minSeparation = separation;
          minJd = jdMid;
        }

        // use uncertainty:
        separation -= margin;

        // evaluate moon restriction rules:
        for (MoonRestriction restriction : moonRestrictions) {
          ruleFli = restriction.getFli();
          if (ruleFli != null) {
            if (fli < ruleFli.doubleValue()) {
              // skip rule
              continue;
            }
          }
          if (separation < restriction.getSeparation()) {
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

      // check again warning threshold:
      if (minSeparation < warningThreshold) {
        // add warning:
        this.addWarning("Moon separation is " + df1.format(minSeparation)
                + " deg at " + timeFormatter.format(convertJDToDate(minJd))
                + " for target [" + targetName + "]<br> Please check pointing restrictions.");
      }

    } else {
      // moon is too far:
      ranges.add(jdRiseSet);
    }

    return ranges;
  }

  /**
   * Return the moon separation in degrees of the current target at the given julian date
   * @param cosDec cosinus of target declination
   * @param sinDec sinus of target declination
   * @param jd julian date
   * @return moon separation in degrees or +INFINITY if moon is not visible
   */
  private double getMoonSeparation(final double cosDec, final double sinDec, final double jd) {
    // fix JD in LST range [0; 24] in order to have accurate target position:
    final double jdIn = getJDInLstRange(jd);

    return this.sco.getMoonSeparation(cosDec, sinDec, jdIn);
  }

  /**
   * Compute discarded target azimuth ranges by telescope pointing restrictions due to the wind direction
   * @param windAzimuth wind direction as azimuth in degrees (0 to north)
   * @param windAzimuthalRestriction angle in degrees forbidden
   * @return discarded target azimuth ranges or null
   */
  private static List<Range> getAzimuthRange(final Double windAzimuth, final Double windAzimuthalRestriction) {
    if (windAzimuth == null || windAzimuthalRestriction == null || windAzimuthalRestriction < 0d) {
      return null;
    }
    final List<Range> ranges = new ArrayList<Range>(2);
    double azMin, azMax;

    // discarded pointing azimuth opposite to wind direction:
    final double discardedAzimuth = (windAzimuth < 180d) ? windAzimuth + 180d : windAzimuth - 180d;

    azMin = discardedAzimuth - windAzimuthalRestriction;
    azMax = discardedAzimuth + windAzimuthalRestriction;

    if (azMin >= 0d && azMax <= 360d) {
      ranges.add(new Range(azMin, azMax));
    } else {
      if (azMin < 0d) {

        if (azMax < 0d) {
          azMin += 360d;
          azMax += 360d;

          ranges.add(new Range(Math.min(azMin, azMax), Math.max(azMin, azMax)));
        } else {
          ranges.add(new Range(azMin + 360d, 360d));
          ranges.add(new Range(0d, azMax));
        }
      } else {
        ranges.add(new Range(azMin, 360d));
        ranges.add(new Range(0d, azMax - 360d));
      }
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

    if (isLogDebug) {
      logger.debug("interferometer: {}", this.interferometer.getName());
      logger.debug("instrument: {}", this.instrument.getName());
    }

    // check Pops :
    this.hasPops = !this.interferometer.getPops().isEmpty();

    if (isLogDebug) {
      logger.debug("hasPops: {}", this.hasPops);
    }

    // wind restriction only if night limits are enabled :
    if (this.useNightLimit) {
      this.windAzimuth = this.observation.getWhen().getWindAzimuth();
      final Double windAzimuthalRestriction = this.interferometer.getWindPointingRestriction();

      this.azimuthRanges = getAzimuthRange(this.windAzimuth, windAzimuthalRestriction);
      this.hasWindRestriction = this.azimuthRanges != null;

      if (isLogDebug) {
        logger.debug("windAzimuth: {}", windAzimuth);
        logger.debug("windRestriction: {}", windAzimuthalRestriction);
        logger.debug("azimuthRanges: {}", azimuthRanges);
      }
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
      throw new IllegalStateException("The station list is empty !");
    }

    if (isLogDebug) {
      logger.debug("stations: {}", stations);
    }

    this.data.setStationNames(this.observation.getInstrumentConfiguration().getStations());

    // All telescopes have the same properties for a given baseline :
    final Telescope tel = stations.get(0).getTelescope();

    final MoonPointingRestriction mpr = tel.getMoonPointingRestriction();
    if (mpr != null) {
      this.moonPointingRestriction = mpr;
    }

    final int nBeams = stations.size();

    // find the optional channels associated to the stations in the instrument configuration :
    // CHARA : predefined channel per station for a specific base line :
    final List<Channel> relatedChannels = ConfigurationManager.getInstance().getInstrumentConfigurationChannels(
            this.observation.getInterferometerConfiguration().getName(),
            this.observation.getInstrumentConfiguration().getName(),
            this.observation.getInstrumentConfiguration().getStations());

    if (isLogDebug) {
      logger.debug("relatedChannels: {}", relatedChannels);
    }

    final int nRelChannels = relatedChannels.size();
    final boolean useRelatedChannels = nRelChannels > 0;

    if (useRelatedChannels && nBeams != nRelChannels) {
      throw new IllegalStateException("The number of associated channels does not match the station list : " + stations + " <> " + relatedChannels);
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

            if (isLogDebug) {
              logger.debug("station = {} - channel = {}", b.getStation(), b.getChannel().getName());
              logger.debug("switchyard = {} - fixed offset = {}", cl.getOpticalLength(), b.getStation().getDelayLineFixedOffset());
              logger.debug("total: {}", b.getOpticalLength());
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

      logger.warn("Not validated code path : Case with no channel definition nor switchyard !");

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

    if (isLogDebug) {
      // dump beam information:
      int i = 0;
      for (Beam b : this.beams) {
        logger.debug("beam [{}]: ", i, b.toString());
        i++;
      }
    }

    this.data.setBeams(this.beams);

    if (isLogDebug) {
      logger.debug("Beams: {}", this.beams);
    }
  }

  /**
   * Generate all PoP combinations and offsets for the given number of beams
   */
  private void preparePopCombinations() {

    final int nBeams = this.beams.size();
    final int sizeBL = this.baseLines.size();

    // Get chosen PoPs :
    final List<Pop> userPoPs = this.observation.getInstrumentConfiguration().getPopList();

    final StringBuilder sb = new StringBuilder(nBeams);

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
        this.popCombinations.add(PopCombination.newInstance(comb, sb));
      }

      this.data.setUserPops(false);

    } else {
      // use the user defined PoPs configuration :
      final PopCombination user = PopCombination.newInstance(userPoPs, sb);
      this.popCombinations = new ArrayList<PopCombination>(1);
      this.popCombinations.add(user);

      this.data.setUserPops(true);
      this.data.setBestPops(user);

      if (isLogDebug) {
        logger.debug("User PoPs: {}", user);
      }
    }

    // Use arrays instead of List for performance:
    final Beam[] bs = new Beam[nBeams];
    this.beams.toArray(bs);

    Beam b1, b2;
    Pop p1, p2;

    List<Pop> popList;
    double[] popOffsets;
    double t;

    for (PopCombination popComb : this.popCombinations) {
      popList = popComb.getPopList();

      popOffsets = new double[sizeBL];

      int k = 0;

      // use indices to get Pop associated to the station :
      for (int i = 0; i < nBeams; i++) {
        for (int j = i + 1; j < nBeams; j++) {
          b1 = bs[i];
          p1 = popList.get(i);

          b2 = bs[j];
          p2 = popList.get(j);

          // Note : the beams are in the same order as the stations :

          // optical path difference = difference of pops delays :
          t = getPopOpticalLength(b1.getStation(), p1) - getPopOpticalLength(b2.getStation(), p2);

          popOffsets[k++] = t;
        }
      }
      popComb.setPopOffsets(popOffsets);
    }

    // Prepare observability context:
    this.obsCtx = new ObservabilityContext(sizeBL);

    final int sizeCb = this.popCombinations.size();

    // Use arrays instead of List for performance:
    this.obsCtx.setPopCombs(new PopCombination[sizeCb]);
    this.popCombinations.toArray(this.obsCtx.getPopCombs());

    this.obsCtx.setBaseLines(new BaseLine[sizeBL]);
    this.baseLines.toArray(this.obsCtx.getBaseLines());

    this.obsCtx.setWRanges(new Range[sizeBL]);
    this.wRanges.toArray(this.obsCtx.getWRanges());
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
    for (PopLink pl : station.getPopLinkArray()) {
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
      intervals = new ArrayList<SunTimeInterval>(9);

      // LST0 reference used to convert HA in JD:
      final double jdLst0 = this.sc.getJdForLst0();
      // lower LST bound = LST0 - 12h
      final double jdLstLower = jdLst0 - AstroSkyCalc.HALF_LST_DAY_IN_JD;
      // upper LST bound = LST0 + 24h + 12h
      final double jdLstUpper = jdLst0 + 3d * AstroSkyCalc.HALF_LST_DAY_IN_JD;

      if (isLogDebug) {
        logger.debug("jdLst bounds = [{} - {}]", jdLstLower, jdLstUpper);
      }

      AstroAlmanacTime stFrom, stTo;
      double jdFrom, jdTo;
      Date from, to;
      SunType type;

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

          if (isLogDebug) {
            logger.debug("Range[{} - {}] : {}", jdFrom, jdTo, type);
          }

          from = jdToDateInDateRange(jdFrom);
          to = jdToDateInDateRange(jdTo);

          if (isLogDebug) {
            logger.debug("SunInterval[{} - {}] : {}", from, to, type);
          }

          intervals.add(new SunTimeInterval(from, to, type));
        }
      }

      // merge contiguous intervals (already ordered):
      Range.union(this.nightLimits);

      if (isLogDebug) {
        logger.debug("nightLimits: {}", this.nightLimits);
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
   * Fix a JD date into LST range [0;24]
   *
   * @param jd date to fix
   * @return date
   */
  private double getJDInLstRange(final double jd) {
    if (jd >= this.jdLower) {

      if (jd <= this.jdUpper) {

        // JD in [jdLst0;jdLst24]
        return jd;

      } else {
        // over LST 24 :

        // return [jd - day]
        return jd - AstroSkyCalc.LST_DAY_IN_JD;
      }

    } else {
      // start occurs before LST 0h :

      // return [jd + day]
      return jd + AstroSkyCalc.LST_DAY_IN_JD;
    }
  }

  /**
   * Convert a JD date to a date within LST range [0;24]
   *
   * @param jd date to convert
   * @return date
   */
  private Date convertJDToDate(final double jd) {
    return jdToDateInDateRange(getJDInLstRange(jd));
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
   * Define HA / azimuth / elevation information about observability ranges and transit for the current target
   * @param starObs star observability data
   * @param obsRangeJD observability ranges in JD
   * @param precRA precessed RA in decimal hours
   * @param precDEC precessed DEC in degrees
   * @param doDetails flag to enable detailled (ticks and transit information)
   */
  private void getTargetPosition(final StarObservabilityData starObs, final List<Range> obsRangeJD,
          final double precRA, final double precDEC,
          final boolean doDetails) {
    final Map<Date, TargetPositionDate> targetPositions = starObs.getTargetPositions();

    // prepare cosDec/sinDec:
    final double dec = Math.toRadians(precDEC);
    final double cosDec = Math.cos(dec);
    final double sinDec = Math.sin(dec);

    final AzEl azEl = new AzEl();

    double jd;

    if (doDetails) {
      final int minElevation = (int) Math.round(this.minElev);

      // internal ticks for elevation :
      for (int elevation = 20; elevation <= 80; elevation += 20) {
        if (elevation > minElevation) {
          final double haElev = this.sco.getHAForElevation(precDEC, elevation);

          if (haElev > 0d) {
            jd = this.sc.convertHAToJD(-haElev, precRA);
            if (Range.contains(obsRangeJD, jd)) {
              addTargetPosition(targetPositions, cosDec, sinDec, azEl, jd);
            }

            jd = this.sc.convertHAToJD(haElev, precRA);
            if (Range.contains(obsRangeJD, jd)) {
              addTargetPosition(targetPositions, cosDec, sinDec, azEl, jd);
            }
          }
        }
      }

      // tick for transit :
      jd = this.sc.convertHAToJD(0d, precRA);
      if (Range.contains(obsRangeJD, jd)) {
        addTargetPosition(targetPositions, cosDec, sinDec, azEl, jd);
      }
    }

    // ticks for observability intervals (limits):
    for (Range range : obsRangeJD) {
      jd = range.getMin();
      addTargetPosition(targetPositions, cosDec, sinDec, azEl, jd);

      jd = range.getMax();
      addTargetPosition(targetPositions, cosDec, sinDec, azEl, jd);
    }

    if (isLogDebug) {
      logger.debug("elevations : ");
      for (TargetPositionDate elevationDate : targetPositions.values()) {
        logger.debug(elevationDate.toString());
      }
    }
  }

  /**
   * Add new target position (azimuth and elevation) for the given target and julian date
   * @param targetPositions map of target positions keyed by date
   * @param cosDec cosinus of target declination
   * @param sinDec sinus of target declination
   * @param azEl AzEl instance
   * @param jd julian date
   */
  private void addTargetPosition(final Map<Date, TargetPositionDate> targetPositions, final double cosDec, final double sinDec, final AzEl azEl, final double jd) {
    // fix JD in LST range [0; 24] in order to have accurate target position:
    final double jdIn = getJDInLstRange(jd);

    final double ha = this.sco.getTargetPosition(cosDec, sinDec, jdIn, azEl);

    final Date date = convertJDToDate(jd);
    targetPositions.put(date, new TargetPositionDate(date, ha, (int) Math.round(azEl.getAzimuth()), (int) Math.round(azEl.getElevation())));
  }

  /**
   * Convert the given list of HA ranges to date intervals in LST (used by Export OB only)
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

      if (isLogDebug) {
        logger.debug("dateIntervals: {}", dateIntervals);
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

  /**
   * Add a warning message in the OIFits file
   * @param msg message to add
   */
  private void addWarning(final String msg) {
    this.data.getWarningContainer().addWarningMessage(msg);
  }
}
