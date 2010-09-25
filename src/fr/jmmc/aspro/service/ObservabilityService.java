/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityService.java,v 1.56 2010-09-25 14:03:35 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.55  2010/09/25 13:58:57  bourgesl
 * test failure in service (JNLP)
 *
 * Revision 1.54  2010/09/24 15:51:20  bourgesl
 * removed catch RuntimeExceptionS to get it at higher level (
 *
 * Revision 1.53  2010/09/15 13:55:07  bourgesl
 * disabled moon rise/set as a star data
 *
 * Revision 1.52  2010/07/22 12:32:49  bourgesl
 * added moon information (rise/set and moon illumination fraction) when the night restrictions are enabled
 *
 * Revision 1.51  2010/06/30 15:02:53  bourgesl
 * use CombUtils to simplify code (number of baselines)
 *
 * Revision 1.50  2010/06/30 14:54:45  bourgesl
 * use CombUtils to simplify code (Pops combination and number of baselines)
 *
 * Revision 1.49  2010/06/25 14:17:21  bourgesl
 * refactoring due to changes done in AstroSkyCalc and AstroSkyCalcObservation
 *
 * Revision 1.48  2010/06/23 12:55:14  bourgesl
 * added Beam list to use it in OIFits generation
 *
 * Revision 1.47  2010/06/17 10:02:50  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.46  2010/06/10 08:55:00  bourgesl
 * only log the compute duration if the operation completed normally
 *
 * Revision 1.45  2010/05/26 15:33:06  bourgesl
 * fixed constructors
 *
 * Revision 1.44  2010/05/26 15:29:51  bourgesl
 * added a constructor for CHARA OB to ignore night limits
 *
 * Revision 1.43  2010/05/26 09:14:20  bourgesl
 * CHARA : use the predefined channel per station for a specific base line
 *
 * Revision 1.42  2010/05/06 15:41:26  bourgesl
 * use AsproConstants HA Min/Max
 *
 * Revision 1.41  2010/05/05 14:33:43  bourgesl
 * javadoc
 * new constructor(target, minElev) to generate OB (z>30°)
 * new method convertHARangesToDateInterval() to convert restricted HA ranges and merge them arround midnight
 *
 * Revision 1.40  2010/04/13 15:35:46  bourgesl
 * Fixed bug on date intervals that have a discontinuity due to conversions from HA [-12;+12]
 *
 * Revision 1.39  2010/04/09 10:23:29  bourgesl
 * side effect of RA/DEC in HMS/DMS
 * corrected Target raDeg/decDeg (degrees)
 *
 * Revision 1.38  2010/04/02 14:40:39  bourgesl
 * added elevation data and transit date
 *
 * Revision 1.37  2010/02/08 17:00:35  bourgesl
 * moved several reference checks to ObservationManager
 *
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
import edu.dartmouth.AstroSkyCalcObservation;
import edu.dartmouth.AlmanacTime;
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
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
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

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.service.ObservabilityService";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** flag to return moon rise/set as star data */
  private final static boolean MOON_RISE_SET = false;

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
  /** observation sky calc instance */
  private final AstroSkyCalcObservation sco = new AstroSkyCalcObservation();
  /** jd corresponding to LST=00:00:00 for the observation date */
  private double jdLst0;
  /** jd corresponding to LST=23:59:59 for the observation date */
  private double jdLst24;
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
  /** selected target (used by OB) */
  private Target selectedTarget = null;
  /** flag to disable the observability restriction due to the night */
  private boolean ignoreUseNightLimit = false;

  /**
   * Constructor.
   * Note : This service is statefull so it can not be reused by several calls.
   *
   * @param observation observation settings
   * @param useLST indicates if the timestamps are expressed in LST or in UTC
   * @param doDetails flag to produce detailed output with all BL / horizon / rise intervals per target
   * @param doBaseLineLimits flag to find base line limits
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
   * Specific Constructor to prepare CHARA Observing blocks for all targets (LST) ignoring night restrictions
   * Note : This service is statefull so it can not be reused by several calls.
   *
   * @param observation observation settings
   * @param minElev minimum elevation (deg)
   */
  public ObservabilityService(final ObservationSetting observation, final double minElev) {
    this(observation, null, minElev, true);
  }

  /**
   * Specific Constructor to prepare VLTI Observing blocks for a single target (LST)
   * Note : This service is statefull so it can not be reused by several calls.
   *
   * @param observation observation settings
   * @param target target to use
   * @param minElev minimum elevation (deg)
   */
  public ObservabilityService(final ObservationSetting observation,
                              final Target target, final double minElev) {
    this(observation, target, minElev, false);
  }

  /**
   * Specific Constructor to prepare Observing blocks
   * Note : This service is statefull so it can not be reused by several calls.
   *
   * @param observation observation settings
   * @param target target to use
   * @param minElev minimum elevation (deg)
   * @param ignoreNightLimits true to disable the observability restriction due to the night
   */
  private ObservabilityService(final ObservationSetting observation,
                               final Target target, final double minElev, final boolean ignoreNightLimits) {
    // use LST :
    this(observation, true, false, false);

    // select target :
    this.selectedTarget = target;

    // force to use the given minimum elevation :
    this.minElev = minElev;

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
      if (this.selectedTarget == null) {
        // copy the list to avoid concurrent modification during iteration :
        targets = new ArrayList<Target>(this.observation.getTargets());
      } else {
        // use the given target (OB) :
        targets = new ArrayList<Target>(1);
        targets.add(selectedTarget);
      }
    }

    // define site :
    this.sc.defineSite(this.interferometer.getName(), this.interferometer.getPosSph());
    this.sco.defineSite(this.sc);

    // define date :
    final XMLGregorianCalendar cal = this.observation.getWhen().getDate();

    // find the julian date corresponding to the LST origin LST=00:00:00 for the given date :
    this.jdLst0 = this.sc.defineDate(cal.getYear(), cal.getMonth(), cal.getDay());

    // warning : in LST, remove 1s to avoid 00:00:00 :
    this.jdLst24 = this.sc.findJdForLst0(this.jdLst0 + 1d) - 1d / 86400d;

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("jd   min = " + this.jdLst0);
      logger.fine("jd   max = " + this.jdLst24);
    }

    this.data.setDateCalc(this.sc);
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

      // Use the LST range [0;24h] +/- 1 day to have valid night ranges to merge with target ranges :
      final List<AlmanacTime> sunEvents = this.sc.findSunRiseSet();

      processSunAlmanach(sunEvents);

      // moon rise/set :
      final List<Range> moonRanges = this.sc.findMoonRiseSet(this.jdLst24);
      final double moonIllum = this.sc.getMaxMoonIllum(moonRanges);

      if (MOON_RISE_SET) {
        final StarObservabilityData soMoon = new StarObservabilityData("Moon [" + (int) Math.round(100 * moonIllum) + " %]",
                StarObservabilityData.TYPE_MOON);
        this.data.getStarVisibilities().add(soMoon);

        for (Range range : moonRanges) {
          convertRangeToDateInterval(range, soMoon.getVisible());
        }
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("moon visible : " + soMoon.getVisible());
        }
      }

      this.data.setMoonIllumPercent(100d * moonIllum);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("moon illum   : " + moonIllum);
      }
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

      // dump star visibilities :
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("Star observability intervals : ");

        for (StarObservabilityData so : this.data.getStarVisibilities()) {
          logger.fine(so.toString());
        }
      }

    }

    // fast interrupt :
    if (this.currentThread.isInterrupted()) {
      return null;
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

    // Target coordinates precessed to jd and to get az/alt positions from JSkyCalc :
    final double[] raDec = this.sco.defineTarget(this.jdLst0, target.getRADeg(), target.getDECDeg());

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

    // reset current target :
    this.sco.reset();

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

    // get Target coordinates precessed to jd and define target to get later az/alt positions from JSkyCalc :
    final double[] raDec = this.sco.defineTarget(this.jdLst0, target.getRADeg(), target.getDECDeg());

    // precessed target right ascension in decimal hours :
    final double precRA = raDec[0];

    // precessed target declination in degrees :
    final double precDEC = raDec[1];

    // define transit date (HA = 0) :
    starObs.setTransitDate(jdToDate(this.sc.convertHAToJD(0d, precRA)));

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

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("rangesJDHz : " + rangesJDHz);
        }

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

            // convert JD ranges to date ranges :
            for (Range range : obsRanges) {
              convertRangeToDateInterval(range, soBl.getVisible());
            }
            // merge contiguous date ranges :
            mergeDateIntervals(soBl.getVisible());

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
        // elevation marks for the current target :
        findElevations(starObs, finalRanges, precRA, precDEC);

        // convert JD ranges to date ranges :
        for (Range range : finalRanges) {
          convertRangeToDateInterval(range, starObs.getVisible());
        }
        // merge contiguous date ranges :
        mergeDateIntervals(starObs.getVisible());

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
  private List<PopObservabilityData> getPopObservabilityData(final String targetName, final double dec, final List<Range> rangesTarget) {

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

      azEl = this.sco.getTargetPosition(jd);

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
    if (stations == null) {
      throw new IllegalStateException("prepareBeams : the station list is null !");
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("stations = " + stations);
    }

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
      final HashSet<Channel> channelSet = new HashSet<Channel>();

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
   * and all intervals (day/night/twilight) in the LST range [0;24]
   * @param sunEvents jd sun events in the LST range [0;24] +/- 12h
   */
  private void processSunAlmanach(final List<AlmanacTime> sunEvents) {
    List<SunTimeInterval> intervals = null;

    if (sunEvents != null && !sunEvents.isEmpty()) {
      final int nbInterval = sunEvents.size() - 1;
      intervals = new ArrayList<SunTimeInterval>(nbInterval);

      AlmanacTime stFrom, stTo;
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
   * @param precRA precessed target right ascension in decimal hours
   * @return JD range
   */
  private Range convertHAToJDRange(final Range rangeHA, final double precRA) {

    final double ha1 = rangeHA.getMin();
    final double ha2 = rangeHA.getMax();

    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("ha1 = " + ha1);
      logger.finest("ha2 = " + ha2);
    }

    final double jd1 = this.sc.convertHAToJD(ha1, precRA);
    final double jd2 = this.sc.convertHAToJD(ha2, precRA);

    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("jd1 = " + this.sc.toDateLST(jd1));
      logger.finest("jd2 = " + this.sc.toDateLST(jd2));
    }

    return new Range(jd1, jd2);
  }

  /**
   * Convert a JD range to an HA range but keep only ranges with an HA in [-12;12]
   * @param rangeJD JD range
   * @param precRA precessed target right ascension in decimal hours
   * @return HA range in [-12;12]
   */
  private Range convertJDToHARange(final Range rangeJD, final double precRA) {

    final double jd1 = rangeJD.getMin();
    final double jd2 = rangeJD.getMax();

    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("jd1 = " + this.sc.toDateLST(jd1));
      logger.finest("jd2 = " + this.sc.toDateLST(jd2));
    }

    double ha1 = this.sc.convertJDToHA(jd1, precRA);
    double ha2 = this.sc.convertJDToHA(jd2, precRA);

    if (ha1 < AsproConstants.HA_MIN) {
      ha1 = AsproConstants.HA_MIN;
    }
    if (ha1 > AsproConstants.HA_MAX) {
      // invalid range :
      return null;
    }
    if (ha2 < AsproConstants.HA_MIN) {
      // invalid range :
      return null;
    }
    if (ha2 > AsproConstants.HA_MAX) {
      ha2 = AsproConstants.HA_MAX;
    }

    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("ha1 = " + ha1 + " h");
      logger.finest("ha2 = " + ha2 + " h");
    }

    return new Range(ha1, ha2);
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
    // one Day in LST is different than one Day in JD :
    final double day = AstroSkyCalc.LST_DAY_IN_JD;

    final double jdStart = rangeJD.getMin();
    final double jdEnd = rangeJD.getMax();

    if (jdStart >= this.jdLst0) {

      if (jdEnd <= this.jdLst24) {

        // single interval [jdStart;jdEnd]
        intervals.add(new DateTimeInterval(jdToDate(jdStart), jdToDate(jdEnd)));

      } else {

        if (jdStart > this.jdLst24) {
          // two points over LST 24 :

          // single interval [jdStart - day;jdEnd - day]
          intervals.add(new DateTimeInterval(jdToDate(jdStart - day), jdToDate(jdEnd - day)));

        } else {
          // end occurs after LST 24 :

          // interval [jdStart;jdLst24]
          intervals.add(new DateTimeInterval(jdToDate(jdStart), this.data.getDateMax()));

          // add the second interval [jdLst0;jdEnd - day]
          intervals.add(new DateTimeInterval(this.data.getDateMin(), jdToDate(jdEnd - day)));
        }
      }

    } else {
      // start occurs before LST 0h :

      if (jdEnd < this.jdLst0) {
        // two points before LST 0h :

        // single interval [jdStart + day;jdEnd + day]
        intervals.add(new DateTimeInterval(jdToDate(jdStart + day), jdToDate(jdEnd + day)));

      } else {
        // interval [jdLst0;jdEnd]
        intervals.add(new DateTimeInterval(this.data.getDateMin(), jdToDate(jdEnd)));

        // add the second interval [jdStart + day;jdLst24]
        intervals.add(new DateTimeInterval(jdToDate(jdStart + day), this.data.getDateMax()));
      }
    }
  }

  /**
   * Sort and traverse the given list of date intervals to merge contiguous intervals.
   * This fixes the problem due to HA limit [+/-12h] i.e. the converted JD / Date ranges
   * can have a discontinuity on the date axis.
   *
   * @param intervals date intervals to fix
   */
  private void mergeDateIntervals(final List<DateTimeInterval> intervals) {
    // first sort date intervals :
    Collections.sort(intervals);
    mergeSortedDateIntervals(intervals);
  }

  /**
   * Traverse the given list of date intervals to merge contiguous intervals.
   * This fixes the problem due to HA limit [+/-12h] i.e. the converted JD / Date ranges
   * can have a discontinuity on the date axis.
   *
   * @param intervals SORTED date intervals to fix
   */
  private void mergeSortedDateIntervals(final List<DateTimeInterval> intervals) {
    final int size = intervals.size();
    if (size > 1) {
      DateTimeInterval interval = null;
      DateTimeInterval interval2 = null;
      for (int i = 0, j = 1, end = size - 1; i < end; i++, j++) {
        interval = intervals.get(i);
        interval2 = intervals.get(j);

        if (interval.getEndDate().compareTo(interval2.getStartDate()) == 0) {
          // merge interval :
          intervals.set(i, new DateTimeInterval(interval.getStartDate(), interval2.getEndDate()));
          // merge interval2 :
          intervals.remove(j);
          end--;
        }
      }
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

    int decMin = 5 * (int) Math.round((obsLat - 90d + this.minElev) / 5d);
    decMin = Math.max(decMin, -90);

    int decMax = 5 * (int) Math.round((obsLat + 90 - this.minElev) / 5d);
    decMax = Math.min(decMax, 90);

    final List<Target> targets = new ArrayList<Target>();
    Target t;
    for (int i = decMax; i > decMin; i -= 5) {
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
    // internal ticks for elevation :
    for (elev = 20; elev <= 80; elev += 20) {
      if (elev != Math.round(this.minElev)) {
        haElev = this.sco.getHAForElevation(precDEC, elev);

        if (haElev > 0) {
          jd = this.sc.convertHAToJD(-haElev, precRA);
          if (Range.contains(obsRangeJD, jd)) {
            elevations.add(new ElevationDate(jdToDate(jd), elev));
          }

          jd = this.sc.convertHAToJD(haElev, precRA);
          if (Range.contains(obsRangeJD, jd)) {
            elevations.add(new ElevationDate(jdToDate(jd), elev));
          }
        }
      }
    }

    // ticks for observability intervals (limits) :
    AzEl azEl;
    for (Range range : obsRangeJD) {
      jd = range.getMin();
      azEl = this.sco.getTargetPosition(jd);
      elev = (int) Math.round(azEl.getElevation());
      elevations.add(new ElevationDate(jdToDate(jd), elev));

      jd = range.getMax();
      azEl = this.sco.getTargetPosition(jd);
      elev = (int) Math.round(azEl.getElevation());
      elevations.add(new ElevationDate(jdToDate(jd), elev));
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
   * Note : date intervals that are over (00:00 or 24:00) are merged. 
   * For example : 22:00->24:00 and 00:00->01:00 returns 22:00->01:00
   *
   * @see fr.jmmc.aspro.ob.ExportOBVLTI#processDateTime(document, observation, target, haMin, haMax)
   * 
   * @param ranges HA ranges
   * @param precRA precessed target right ascension in decimal hours
   * @return date intervals
   */
  public List<DateTimeInterval> convertHARangesToDateInterval(final List<Range> ranges, final double precRA) {
    if (ranges != null) {
      final List<Range> jdRanges = new ArrayList<Range>();
      for (Range range : ranges) {
        jdRanges.add(convertHAToJDRange(range, precRA));
      }

      final List<DateTimeInterval> dateIntervals = new ArrayList<DateTimeInterval>();
      // convert JD ranges to date ranges :
      for (Range range : jdRanges) {
        convertRangeToDateInterval(range, dateIntervals);
      }
      // merge contiguous date ranges :
      mergeDateIntervals(dateIntervals);

      // Replace the 23:59:59 date by 00:00:00 to merge contiguous intervals in LST :
      final Calendar cal = new GregorianCalendar();
      cal.set(Calendar.HOUR_OF_DAY, 0);
      cal.set(Calendar.MINUTE, 0);
      cal.set(Calendar.SECOND, 0);
      // fix milliseconds to 0 to be able to compare date instances :
      cal.set(Calendar.MILLISECOND, 0);

      final Date lst0 = cal.getTime();

      cal.set(Calendar.HOUR_OF_DAY, 23);
      cal.set(Calendar.MINUTE, 59);
      cal.set(Calendar.SECOND, 59);

      final Date lst24 = cal.getTime();

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
        mergeSortedDateIntervals(dateIntervals);

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
}
