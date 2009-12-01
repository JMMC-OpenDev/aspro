/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityService.java,v 1.17 2009-12-01 17:14:45 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
import fr.jmmc.aspro.model.DateTimeInterval;
import fr.jmmc.aspro.model.ObservabilityData;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.StarObservability;
import fr.jmmc.aspro.model.SunTimeInterval;
import fr.jmmc.aspro.model.oi.AzEl;
import fr.jmmc.aspro.model.oi.Channel;
import fr.jmmc.aspro.model.oi.ChannelLink;
import fr.jmmc.aspro.model.oi.DelayLine;
import fr.jmmc.aspro.model.oi.FocalInstrument;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Pop;
import fr.jmmc.aspro.model.oi.PopLink;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.StationLinks;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.service.HorizonService.Profile;
import fr.jmmc.aspro.util.CombinationGenerator;
import fr.jmmc.aspro.util.PermutationGenerator;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
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

  /* members */

  /* output */
  /** observability data */
  private ObservabilityData data = new ObservabilityData();

  /* inputs */
  /** observation settings */
  private final ObservationSetting observation;
  /** minimum of elevation to observe any target (rad) */
  private final double minElev;
  /** flag to enable the observability restriction due to the night */
  private boolean useNightLimit;
  /** indicates if the timestamps are expressed in LST or in UTC */
  private final boolean useLST;
  /** flag to find baseline limits */
  private final boolean doBaseLineLimits;
  /** flag to produce detailed output with all BL / horizon / rise intervals per target */
  private boolean doDetails;

  /* internal */
  /** sky calc instance */
  private final AstroSkyCalc sc = new AstroSkyCalc();
  /** jd corresponding to LST=0 for the observation date */
  private double jdLst0;
  /** jd corresponding to LST=23:59:59 for the observation date */
  private double jdLst24;
  /** Night ranges defined in julian day */
  private List<Range> nightLimits = null;
  /** interferometer description */
  private InterferometerDescription interferometer;
  /** focal instrument description */
  private FocalInstrument instrument;
  /** flag to indicate that a station has an horizon profile */
  private boolean hasHorizon = false;
  /** flag to indicate that pops are used */
  private boolean hasPops = false;
  /** beam list */
  private List<Beam> beams;
  /** base line list */
  private List<BaseLine> baseLines = new ArrayList<BaseLine>();
  /** W ranges corresponding to the base line list */
  private List<Range> wRanges = new ArrayList<Range>();
  /** list of Pop combinations */
  private List<List<Pop>> popCombinations = new ArrayList<List<Pop>>();
  /** list of Pop offsets per base line */
  private List<List<Double>> popOffsets = new ArrayList<List<Double>>();

  /**
   * This service is statefull so it can not be reused by several calls
   *
   * @param observation observation settings
   * @param minElev minimum of elevation to observe any target (rad)
   * @param useNightLimit flag to enable the observability restriction due to the night
   * @param useLST indicates if the timestamps are expressed in LST or in UTC
   * @param doDetails flag to produce detailed output with all BL / horizon / rise intervals per target
   */
  public ObservabilityService(final ObservationSetting observation, final double minElev,
          final boolean useNightLimit, final boolean useLST, final boolean doDetails, final boolean doBaseLineLimits) {
    // Inputs :
    this.observation = observation;
    this.minElev = minElev;
    this.useNightLimit = useNightLimit;
    this.useLST = useLST;
    this.doDetails = doDetails;
    this.doBaseLineLimits = doBaseLineLimits;
  }

  /**
   * Main operation to determine the source observability for a given interferometer configuration
   *
   * @param observation observation settings
   * @param useLst true indicates to return date/time values in LST, false to use UTC reference
   * @param minElev minimum elevation (rad)
   * @return ObservabilityData container
   */
  public ObservabilityData calcObservability() {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("start : " + observation);
    }

    // Get the current thread to check if the computation is interrupted :
    final Thread currentThread = Thread.currentThread();

    // Start the computations :
    final long start = System.nanoTime();

    try {
      // Get interferometer / instrument :
      prepareObservation();

      final List<Target> targets;
      if (this.doBaseLineLimits) {
        targets = generateTargetsForBaseLineLimits();
      } else {
        targets = observation.getTargets();
      }

      // define site :
      this.sc.defineSite(this.interferometer.getName(), this.interferometer.getPosSph());

      final XMLGregorianCalendar cal = this.observation.getWhen().getDate();

      // define date :
      this.sc.defineDate(cal.getYear(), cal.getMonth(), cal.getDay());

      // fast interrupt :
      if (currentThread.isInterrupted()) {
        return null;
      }

      // 0 - Find the julian date corresponding to the LST origin LST=0 for the given date :
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
      if (currentThread.isInterrupted()) {
        return null;
      }

      // 1 - Find the day / twlight / night zones :
      if (this.useNightLimit) {
        //  sun rise/set with twilight : see NightlyAlmanac

        /*
        Use the LST range [0;24h] +- 12h to have valid night ranges to merge with target ranges :
         */

        final List<SunAlmanachTime> sunEvents = this.sc.findSunRiseSet(this.jdLst0);

        processSunAlmanach(sunEvents);
      }

      // fast interrupt :
      if (currentThread.isInterrupted()) {
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
          preparePopCombinations(this.interferometer.getPops(), this.beams.size());
        }

        for (Target target : targets) {

          // fast interrupt :
          if (currentThread.isInterrupted()) {
            return null;
          }

          findTargetObservability(target);

        } // for Target

        // fast interrupt :
        if (currentThread.isInterrupted()) {
          return null;
        }

        // dump star visibilities :
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("Star observability intervals : ");

          for (StarObservability so : this.data.getStarVisibilities()) {
            logger.fine(so.toString());
          }
        }

      }

    } catch (RuntimeException re) {
      logger.log(Level.SEVERE, "calcObservability failure :", re);
      logger.log(Level.SEVERE, "observation :" + ObservationManager.getInstance().toString(observation));
      // clear invalid data :
      this.data = null;
    }

    if (logger.isLoggable(Level.INFO)) {
      logger.info("calcObservability : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");
    }

    return this.data;
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
        logger.fine("nightLimits : " + nightLimits);
      }

    }

    this.data.setSunIntervals(intervals);
  }

  private void findTargetObservability(final Target target) {

    // Get the current thread to check if the computation is interrupted :
    final Thread currentThread = Thread.currentThread();

    final StarObservability starObs = new StarObservability(target.getName());
    // add the result to keep an unobservable target :
    this.data.getStarVisibilities().add(starObs);

    // Target coordinates precessed to jd and to get position from JSkyCalc :
    final double[] raDec = this.sc.defineTarget(jdLst0, target.getRA(), target.getDEC());

    // target right ascension in decimal hours :
    final double precRA = raDec[0];

    // target declination in degrees :
    final double precDEC = raDec[1];

    // offset used to convert HA to LST = ra in dec hours :
    final double lstOffset = precRA;

    // Find LST range corresponding to the rise / set of the target :
    final double haElev = this.sc.getHAForElevation(precDEC, this.minElev);

    // target rise :
    if (haElev > 0d) {
      // rise/set range :
      final Range rangeHARiseSet = new Range(-haElev, haElev);

      // convert HA range to JD range :
      final Range rangeJDRiseSet = convertHARange(rangeHARiseSet, lstOffset);

      // For now : only VLTI has horizon profiles :
      List<Range> rangesJDHz = null;
      if (this.hasHorizon) {
        // check horizon profiles inside rise/set range :
        rangesJDHz = checkHorizonProfile(rangeJDRiseSet);

        // fast interrupt :
        if (currentThread.isInterrupted()) {
          return;
        }
      }

      // HA intervals for every base line :
      List<List<Range>> rangesHABaseLines;

      if (this.hasPops) {
        // handle here all possible combinations for POPs :
        // keep only the POPs that maximize the DL+rise intersection ...
        rangesHABaseLines = findHAIntervalsWithPops(Math.toRadians(precDEC), rangeHARiseSet, starObs);

      } else {
        // Get intervals (HA) compatible with all base lines (switchyard / delay line / pops) :
        rangesHABaseLines = DelayLineService.findHAIntervals(Math.toRadians(precDEC), this.baseLines, this.wRanges);
      }

      // rangesHABaseLines can be null if the thread was interrupted :
      // fast interrupt :
      if (currentThread.isInterrupted()) {
        return;
      }

      // observable ranges (jd) :
      final List<Range> obsRanges = new ArrayList<Range>();

      if (this.doDetails) {
        final String prefix = target.getName() + " ";

        // Add Rise/Set :
        final StarObservability soRiseSet = new StarObservability(prefix + "Rise/Set");
        this.data.getStarVisibilities().add(soRiseSet);

        convertRangeToDateInterval(rangeJDRiseSet, soRiseSet.getVisible());

        if (rangesJDHz != null) {
          // Add Horizon :
          final StarObservability soHz = new StarObservability(prefix + "Horizon");
          this.data.getStarVisibilities().add(soHz);

          for (Range range : rangesJDHz) {
            convertRangeToDateInterval(range, soHz.getVisible());
          }
        }

        // Add ranges per BL :
        BaseLine baseLine;
        List<Range> ranges;
        StarObservability soBl;
        for (int i = 0, size = this.baseLines.size(); i < size; i++) {
          baseLine = this.baseLines.get(i);
          ranges = rangesHABaseLines.get(i);

          if (ranges != null) {
            for (Range range : ranges) {
              obsRanges.add(convertHARange(range, lstOffset));
            }
          }
          if (logger.isLoggable(Level.FINE)) {
            logger.fine("baseLine : " + baseLine);
            logger.fine("JD ranges  : " + obsRanges);
          }

          soBl = new StarObservability(prefix + baseLine.getName());
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

      // Merge then all JD intervals :

      // nValid = nBL [dl] + 1 [rise or horizon] + 1 if night limits
      int nValid = 0;

      // flatten and convert HA ranges to JD range :
      for (List<Range> ranges : rangesHABaseLines) {
        if (ranges != null) {
          for (Range range : ranges) {
            obsRanges.add(convertHARange(range, lstOffset));
          }
        }
        nValid++;
      }

      if (rangesJDHz != null) {
        obsRanges.addAll(rangesJDHz);
      } else {
        // TODO : Check Shadowing for every stations ?

        obsRanges.add(rangeJDRiseSet);
      }
      nValid++;

      // Intersect with night limits :
      if (this.useNightLimit) {
        obsRanges.addAll(nightLimits);
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
      for (Range range : finalRanges) {
        convertRangeToDateInterval(range, starObs.getVisible());
        /*
        know bug : due to HA limit [+/-12h], the converted JD / Date ranges
        can have a discontinuity on the LST/UTC axis !
         */
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
   * @param dec target declination (rad)
   * @return intervals (hour angles) or null if thread interrupted
   */
  public List<List<Range>> findHAIntervalsWithPops(final double dec, final Range rangeHARiseSet, final StarObservability starObs) {
    
    // Get the current thread to check if the computation is interrupted :
    final Thread currentThread = Thread.currentThread();

    final int sizeBL = this.baseLines.size();
    final int sizeCb = this.popCombinations.size();

    BaseLine bl;
    Range wRange;
    Double offset;

    // w range using the pop offset for a given base line :
    final Range wRangeWithOffset = new Range();

    // First Pass :
    // For all PoP combinations : find the HA interval merged with the HA Rise/set interval
    List<Range> rangesPoP = new ArrayList<Range>();
    final List<List<Range>> rangesPoPs = new ArrayList<List<Range>>(sizeCb);

    List<Pop> popComb;
    List<Double> popOffset;

    for (int k = 0; k < sizeCb; k++) {
      popOffset = this.popOffsets.get(k);

      for (int i = 0; i < sizeBL; i++) {
        bl = this.baseLines.get(i);
        wRange = this.wRanges.get(i);
        offset = popOffset.get(i);

        wRangeWithOffset.setMin(wRange.getMin() + offset.doubleValue());
        wRangeWithOffset.setMax(wRange.getMax() + offset.doubleValue());

        // fast interrupt :
        if (currentThread.isInterrupted()) {
          return null;
        }

        rangesPoP.addAll(DelayLineService.findHAIntervalsForBaseLine(dec, bl, wRangeWithOffset));
      }
      
      // Merge HA ranges with HA Rise/set ranges :
      rangesPoP.add(rangeHARiseSet);
      
      rangesPoPs.add(Range.mergeRanges(rangesPoP, sizeBL + 1));

      // reset :
      rangesPoP.clear();
    }

    // Find the PoP that gives the better ranges :
    int maxIdx = -1;
    double maxGlobal = 0d;

    double maxPop = 0d;
    double len;
    for (int k = 0; k < sizeCb; k++) {
      popComb = this.popCombinations.get(k);
      rangesPoP = rangesPoPs.get(k);

      // maximum length per Pop combination :
      maxPop = 0d;
      for (Range range : rangesPoP) {
        len = range.getLength();
        if (len > maxPop) {
          maxPop = len;
        }
      }

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("pop : " + popComb + " = " + maxPop);
      }

      // maximum length for all combination :
      if (maxPop > maxGlobal) {
        maxGlobal = maxPop;
        maxIdx = k;
      }

    }

    if (maxIdx != -1) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("pop : " + this.popCombinations.get(maxIdx) + " = " + maxGlobal);
      }

      popComb = this.popCombinations.get(maxIdx);
      popOffset = this.popOffsets.get(maxIdx);

      starObs.setName(starObs.getName() + " " + popComb);

      // Second Pass :
      // For a particular PoP combination : find the HA interval (not merged)
      final List<List<Range>> rangesBL = new ArrayList<List<Range>>();

      for (int i = 0; i < sizeBL; i++) {
        bl = this.baseLines.get(i);
        wRange = this.wRanges.get(i);
        offset = popOffset.get(i);

        wRangeWithOffset.setMin(wRange.getMin() + offset.doubleValue());
        wRangeWithOffset.setMax(wRange.getMax() + offset.doubleValue());

        // fast interrupt :
        if (currentThread.isInterrupted()) {
          return null;
        }

        rangesBL.add(DelayLineService.findHAIntervalsForBaseLine(dec, bl, wRangeWithOffset));
      }
      return rangesBL;
    }
    return Collections.emptyList();
  }

  /**
   * Check the horizon profiles for all stations given the target rise/set range (JD)
   * @param jdRiseSet target rise/set range (JD)
   * @return list of observable ranges (no obstruction) or null if thread interrupted
   */
  private List<Range> checkHorizonProfile(final Range jdRiseSet) {
    // output :
    final List<Range> ranges = new ArrayList<Range>();

    // Get the current thread to check if the computation is interrupted :
    final Thread currentThread = Thread.currentThread();

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
      if (currentThread.isInterrupted()) {
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

  private void prepareObservation() {
    this.interferometer = this.observation.getInterferometerConfiguration().getInterferometerConfiguration().getInterferometer();
    this.instrument = this.observation.getInstrumentConfiguration().getInstrumentConfiguration().getFocalInstrument();

    this.hasPops = !this.interferometer.getPops().isEmpty();

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("interferometer = " + this.interferometer.getName());
      logger.fine("instrument     = " + this.instrument.getName());
    }
  }

  private void prepareBeams() {

    // Get chosen stations :
    final List<Station> stations = this.observation.getInstrumentConfiguration().getStationList();

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("stations = " + stations);
    }

    final int nBeams = stations.size();

    this.beams = new ArrayList<Beam>(nBeams);

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
      // Case Interferometer with a switchyard (VLTI) :

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
            b.setChannel(cl.getChannel());
            b.addOpticalLength(cl.getOpticalLength());

            if (logger.isLoggable(Level.FINE)) {
              logger.fine("station = " + b.getStation() + " = " + b.getChannel().getName());
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
   * Generates all Pop combinations for the given number of beams
   * @param pops list of pops for the interferometer
   * @param nBeams number of beams
   */
  private void preparePopCombinations(final List<Pop> pops, final int nBeams) {

    this.popCombinations = new ArrayList<List<Pop>>();

    final int n = pops.size();
    final int p = nBeams;

    final CombinationGenerator cg = new CombinationGenerator(n, nBeams);

    PermutationGenerator<Integer> pg;

    int[] indices;
    List<Pop> comb;
    final Integer[] ints = new Integer[p];

    while (cg.hasMore()) {
      indices = cg.getNext();

      int j = 0;
      for (int i : indices) {
        ints[j++] = Integer.valueOf(i);
      }
      pg = new PermutationGenerator<Integer>(ints);

      while (pg.next()) {
        comb = new ArrayList<Pop>(p);
        for (int i = 0; i < p; i++) {
          comb.add(pops.get(ints[i]));
        }

        this.popCombinations.add(comb);
      }
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("popCombinations : " + this.popCombinations);
    }

    // Compute the offset for every base line and every pop combination :
    this.popOffsets = new ArrayList<List<Double>>(this.popCombinations.size());

    // Note : the beams are in the same order as the stations :

    Beam b1, b2;
    Pop p1, p2;

    List<Double> poList;
    double t;

    for (List<Pop> pList : this.popCombinations) {

      poList = new ArrayList<Double>(this.baseLines.size());

      for (int i = 0; i < nBeams; i++) {
        for (int j = i + 1; j < nBeams; j++) {
          b1 = this.beams.get(i);
          p1 = pList.get(i);

          b2 = this.beams.get(j);
          p2 = pList.get(j);

          // optical path difference = difference of pops delays :
          t = getPopOpticalLength(b1.getStation(), p1) - getPopOpticalLength(b2.getStation(), p2);

          poList.add(Double.valueOf(t));
        }
      }
      this.popOffsets.add(poList);
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("popOffsets : " + this.popOffsets);
    }
  }

  private double getPopOpticalLength(final Station station, final Pop pop) {
    for (PopLink pl : station.getPopLinks()) {
      if (pl.getPop().equals(pop)) {
        return pl.getOpticalLength();
      }
    }
    return 0d;
  }

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
  }

  /**
   * Converts an HA range to a JD range
   * @param rangeHA given in hour angle (dec hours)
   * @param lstOffset right ascension (dec hours)
   * @return JD range
   */
  private Range convertHARange(final Range rangeHA, final double lstOffset) {

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

  private List<Target> generateTargetsForBaseLineLimits() {

    final double obsLat = Math.toDegrees(this.interferometer.getPosSph().getLatitude());

    final double minElevDeg = Math.toDegrees(this.minElev);

    int decMin = 5 * (int) Math.round((obsLat - 90d + minElevDeg) / 5d);
    decMin = Math.max(decMin, -90);

    int decMax = 5 * (int) Math.round((obsLat + 90 - minElevDeg) / 5d);
    decMax = Math.min(decMax, 90);

    final List<Target> targets = new ArrayList<Target>();
    Target t;
    for (int i = decMin; i <= decMax; i += 5) {
      t = new Target();
      // delta = x
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
