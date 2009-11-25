/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityService.java,v 1.13 2009-11-25 17:14:32 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.DateTimeInterval;
import fr.jmmc.aspro.model.ObservabilityData;
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
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.StationLinks;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.service.HorizonService.Profile;
import java.util.ArrayList;
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
  /** indicates if the timestamps are expressed in LST or in UTC */
  private final boolean useLST;
  /** minimum of elevation to observe any target (rad) */
  private final double minElev;
  /** flag to produce detailed output with all BL / horizon / rise intervals per target */
  private final boolean doDetails;
  /** flag to enable the observability restriction due to the night */
  private boolean useNightLimit = true;

  /* TODO : debug here = useNightLimit */
  
  /* internal */
  /** sky calc instance */
  private final AstroSkyCalc sc = new AstroSkyCalc();
  /** jd corresponding to LST=0 for the observation date */
  private double jdLst0;
  /** jd corresponding to LST=23:59:59 for the observation date */
  private double jdLst24;
  /** interferometer description */
  private InterferometerDescription interferometer;
  /** focal instrument description */
  private FocalInstrument instrument;
  /** flag to indicate that a station has an horizon profile */
  private boolean doCheckHorizon = false;
  /** beam list */
  private List<Beam> beams;
  /** base line list */
  private List<BaseLine> baseLines = new ArrayList<BaseLine>();
  /** W ranges */
  private List<Range> wRanges = new ArrayList<Range>();
  /** Night ranges defined in julian day */
  private List<Range> nightLimits = null;

  /**
   * This service is stateless so it can not be reused
   */
  public ObservabilityService(final ObservationSetting observation, final boolean useLST, final double minElev, final boolean doDetails) {
    // Inputs :
    this.observation = observation;
    this.useLST = useLST;
    this.minElev = minElev;
    this.doDetails = doDetails;
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

      // Prepare the beams (station / channel / delay line) :
      prepareBeams();

      // Prepare the base line (XYZ vector, wRange) :
      prepareBaseLines();

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
      if (this.useNightLimit && !this.doDetails) {

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

      if (this.observation.getTargets().isEmpty()) {
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("No target defined.");
        }
      } else {

        for (Target target : this.observation.getTargets()) {

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
      // clear invalid data :
      this.data = null;
    }

    if (logger.isLoggable(Level.INFO)) {
      logger.info("calcObservability : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");
    }

    return this.data;
  }

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

        // BUG HERE

        if (jdFrom > this.jdLst0 || jdTo < this.jdLst24) {

          // trim :
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

    final StarObservability starObs = new StarObservability(target.getName());
    // add the result to keep an unobservable target :
    this.data.getStarVisibilities().add(starObs);

    double targetRA = target.getRA();

    if (this.doDetails) {
      // In this case, the right ascension is ignored to have HA intervals centered arround LST = 12h
//      targetRA = 180d;
    }

    // Target coordinates precessed to jd :
    final double[] raDec = this.sc.defineTarget(jdLst0, targetRA, target.getDEC());

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

      List<Range> rangesJDHz = null;
      if (this.doCheckHorizon) {
        // check horizon profiles inside rise/set range :
        rangesJDHz = checkHorizonProfile(rangeJDRiseSet);
      }

      // Get intervals (HA) compatible with all base lines (switchyard / delay line / pops) :
      final List<List<Range>> rangesHABaseLines = DelayLineService.findHAIntervals(Math.toRadians(precDEC), this.baseLines, this.wRanges);

      // observable ranges (jd) :
      final List<Range> obsRanges = new ArrayList<Range>();

      if (this.doDetails) {

        // Add Rise/Set :
        final StarObservability soRiseSet = new StarObservability(target.getName() + " - Rise");
        this.data.getStarVisibilities().add(soRiseSet);

        convertRangeToDateInterval(rangeJDRiseSet, soRiseSet.getVisible());

        if (rangesJDHz != null) {
          // Add Horizon :
          final StarObservability soHz = new StarObservability(target.getName() + " - Hz");
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

          for (Range range : ranges) {
            obsRanges.add(convertHARange(range, lstOffset));
          }

          if (logger.isLoggable(Level.FINE)) {
            logger.fine("baseLine : " + baseLine);
            logger.fine("JD ranges  : " + obsRanges);
          }

          soBl = new StarObservability(target.getName() + " - " + baseLine.getName());
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
        for (Range range : ranges) {
          obsRanges.add(convertHARange(range, lstOffset));
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
      if (this.useNightLimit && !this.doDetails) {
        obsRanges.addAll(nightLimits);
        nValid++;
      }

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("obsRanges : " + obsRanges);
      }

      // finally : merge intervals and return date intervals :
      final List<Range> finalRanges = Range.mergeRanges(obsRanges, nValid);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("finalRanges : " + finalRanges);
      }

      // store merge result as date intervals :
      for (Range range : finalRanges) {
        convertRangeToDateInterval(range, starObs.getVisible());
        /*
        know bug : due to HA limit [12h or -12h], the converted JD / Date ranges
        can have a discontinuity on the LST/UTC axis !
         */
      }

    } else {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("Target never rise : " + target);
      }
    }
  }

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
          if (logger.isLoggable(Level.FINE)) {
            logger.fine("target is hidden by horizon profile = " + p.getName() + " [" +
                    azEl.getAzimuth() + ", " + azEl.getElevation() + "] @ " + this.sc.toDate(jd, true));
          }
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

      if (s.getHorizon() != null) {
        this.doCheckHorizon = true;
      }
    }

    // Get delay Lines :
    final List<Channel> channels = this.interferometer.getChannels();

    // Get delay Lines :
    final List<DelayLine> delayLines = this.interferometer.getDelayLines();

    final int nDelayLines = delayLines.size();

    // Has switchyard ?

    if (channels != null && this.interferometer.getSwitchyard() != null) {
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

    // Finally add Pops :

    // TODO : Pops for CHARA :

    /*
     *     Zero MyPops First
     *     DO I=1,NSTAT
     *        MYPOPS(I)=0.0
     *     ENDDO
     *
    POPSLOOP :  DO I=1,NSTAT
    !     convert the configuration like '1223' to integers, .
    READ(ARGLIST(I:I),'(I1)',IOSTAT=IER) KPOPS
    IF (IER.NE.0) THEN
    DDO_POPS=.FALSE.
    EXIT POPSLOOP
    ELSE
    KPOPS=MAX(KPOPS,1)
    KPOPS=MIN(KPOPS,NPOPS)
    MYPOPS(I)=POPS(KPOPS,SLIST(I))
    ENDIF
    ENDDO POPSLOOP
     *
     *
     */

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Beams = " + this.beams);
    }
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

        t = b1.getOpticalLength() - b2.getOpticalLength();

        // 2 DL for 2 telescopes => double throw :
        // note : for now, all DLs are equivalent (same throw) :

        wMin = t - b2.getDelayLine().getMaximumThrow();
        wMax = t + b1.getDelayLine().getMaximumThrow();

        /*
        XXX(KKK)= -STATX(SLIST(J))+STATX(SLIST(I))
        YYY(KKK)= -STATY(SLIST(J))+STATY(SLIST(I))
        ZZZ(KKK)= -STATZ(SLIST(J))+STATZ(SLIST(I))
        TTT(KKK)= T(I)-T(J)-DL_THROW(DLLIST(J))
        THROW(KKK)= T(I)-T(J)+DL_THROW(DLLIST(I))
         */

        /*
        !     The problem to solve is to find the range of hour angles h for which
        !     0 < w(stat2-stat1)(h)+wfix < throw(stat2),
        !     where wfix is tt(stat2)- [tt(stat1) + 0 or + throw(stat1)]
        !     so we want -wfix < w(h) < throw(stat2)-wfix
         */

        this.baseLines.add(new BaseLine(b1, b2, x, y, z));

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
    // one Day in LST is diffrent than one Day in JD :
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
}
