/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityService.java,v 1.9 2009-11-20 16:55:47 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
import fr.jmmc.aspro.model.oi.Channel;
import fr.jmmc.aspro.model.oi.ChannelLink;
import fr.jmmc.aspro.model.oi.DelayLine;
import fr.jmmc.aspro.model.oi.FocalInstrument;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.StationLinks;
import fr.jmmc.aspro.model.oi.Target;
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
  private ObservationSetting observation;
  /** indicates if the timestamps are expressed in LST or in UTC */
  private boolean useLST;
  /** minimum of elevation to observe any target (rad) */
  private double minElev;

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
  /** beam list */
  private List<Beam> beams;
  /** base line list */
  private List<BaseLine> baseLines = new ArrayList<BaseLine>();
  /** W ranges */
  private List<Range> wRanges = new ArrayList<Range>();

  /**
   * This service is stateless so it can not be reused
   */
  public ObservabilityService(final ObservationSetting observation, final boolean useLST, final double minElev) {

    // Inputs :
    this.observation = observation;
    this.useLST = useLST;
    this.minElev = minElev;

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


      this.sc.defineSite(this.interferometer.getName(), this.interferometer.getPosSph());

      final XMLGregorianCalendar cal = this.observation.getWhen().getDate();

      this.sc.defineDate(cal.getYear(), cal.getMonth(), cal.getDay());

      // fast interrupt :
      if (currentThread.isInterrupted()) {
        return null;
      }

      // 0 - Trouver l'origine LST=0 par rapport a la date donnee :
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

      // 1 - Trouver la nuit : sun rise/set with twilight : see NightlyAlmanac
      // indicateur pour cette date mais ne pas se limiter a la nuit et utiliser le domaine LST [0;24h]

      final List<SunAlmanachTime> sunEvents = this.sc.findSunRiseSet(this.jdLst0, this.jdLst24);

      processSunAlmanach(sunEvents);

      // fast interrupt :
      if (currentThread.isInterrupted()) {
        return null;
      }

      // 2 - Pour chaque source, etudier sa progression en altitude (degrees) pour voir ensuite si cet angle est possible avec les telescopes ...

      if (this.observation.getTargets().isEmpty()) {
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("No target defined.");
        }
      } else {

        final List<StarObservability> starVis = this.data.getStarVisibilities();

        StarObservability starObs;

        /*
        DateTimeInterval interval;

        AzAlt azAlt;
        boolean last = false;
        boolean overHorizon = false;
        boolean visible = false;

        double jd;

        // 1 minutes :
        final double jdStep = (1d / 3600d) / 24d;
         */

        for (Target target : this.observation.getTargets()) {

          this.sc.defineTarget(target.getRA(), target.getDEC());

          starObs = new StarObservability(target.getName());
          starVis.add(starObs);

          // fast interrupt :
          if (currentThread.isInterrupted()) {
            return null;
          }

          findTargetObservability(starObs.getVisible());

          // solve baseline limits :

          // check horizon profiles

    /*
     CALL PLOT_HORIZON(XXX,YYY,ZZZ,TTT,SNAM,THROW,KKK,
     &        PROJECT,ERROR)

    CALL DL_INTERVAL_LIST(TTT,THROW,KKK,RIGHTA(I),DECLIN(I),LATITUD,
     &  HORIZ(1),iTw,
     &  XXX,YYY,ZZZ,SNAM,HLONGLIST,NHLIST,MEMORY(IPWH),
     &  MEMORY(IPWHI),MEMORY(IPIWH))

      SUBROUTINE DL_INTERVAL_LIST(WMIN,WMAX,M,RA,D,LAT,Elev,iTw,
     &     X,Y,Z,SNAM,
     &     HLIST,N,
     &     WH,WHI,IWH)

     !     -----------------------------------------------------------------------
    !     Finds the intervals in hour angle where w(h) is .GE.WMIN AND .LE.WMAX
    !     for ALL the WMIN and WMAX (and corresponding X Y & Z).
    !     Result is a list of start-end times in HLIST
    !     WH and IWH are work arrays of size 6*M
    !     -----------------------------------------------------------------------
     */






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

          azAlt = this.sc.getTargetPosition(jd);

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
          interval.setStartDate(jdToDate(jd));
          }
          } else {
          if (last) {
          last = false;
          // end point
          interval.setEndDate(jdToDate(jd));
          intervals.add(interval);
          interval = new DateTimeInterval();
          }
          }

          //
          jd += jdStep;
          }

          // close last interval if open :
          if (interval.getStartDate() != null) {
          interval.setEndDate(jdToDate(jdMax));
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
      Date from, to;
      SunTimeInterval.SunType type = null;

      for (int i = 0; i < nbInterval; i++) {
        stFrom = sunEvents.get(i);
        stTo = sunEvents.get(i + 1);

        from = jdToDate(stFrom.getJd());
        to = jdToDate(stTo.getJd());

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

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("SunInterval[" + from + " - " + to + "] : " + type);
        }

        intervals.add(new SunTimeInterval(from, to, type));
      }
    }

    this.data.setSunIntervals(intervals);
  }

  private void findTargetObservability(final List<DateTimeInterval> intervals) {

    DateTimeInterval interval;

    // Find lst interval corresponding to the rise / set of the target :

    final double[] jdInterval = this.sc.getTimeIntervalForAltitude(this.jdLst0, this.minElev);
    if (jdInterval != null) {
      final double jdRise = jdInterval[0];
      final double jdSet = jdInterval[1];

      if (jdRise > this.jdLst0) {

        if (jdSet < this.jdLst24) {

          // single interval [jdRise;jdSet]
          interval = new DateTimeInterval();
          interval.setStartDate(jdToDate(jdRise));
          interval.setEndDate(jdToDate(jdSet));
          intervals.add(interval);

        } else {

          // interval [jdRise;jdMax]
          interval = new DateTimeInterval();
          interval.setStartDate(jdToDate(jdRise));
          interval.setEndDate(jdToDate(this.jdLst24));
          intervals.add(interval);

          // add the second interval [jdMin;jdSet - 1]

          interval = new DateTimeInterval();
          interval.setStartDate(jdToDate(this.jdLst0));
          interval.setEndDate(jdToDate(jdSet - 1d));
          intervals.add(interval);
        }

      } else {
        // rise occurs before jd0 :

        // interval [jdMin;jdSet]
        interval = new DateTimeInterval();
        interval.setStartDate(jdToDate(this.jdLst0));
        interval.setEndDate(jdToDate(jdSet));
        intervals.add(interval);

        // add the second interval [jdRise + 1;jdMax]

        interval = new DateTimeInterval();
        interval.setStartDate(jdToDate(jdRise + 1d));
        interval.setEndDate(jdToDate(this.jdLst24));
        intervals.add(interval);
      }
    }

  }

  private void prepareObservation() {
    this.interferometer = this.observation.getInterferometerConfiguration().getInterferometerConfiguration().getInterferometer();
    this.instrument = this.observation.getInstrumentConfiguration().getInstrumentConfiguration().getFocalInstrument();

    if (logger.isLoggable(Level.INFO)) {
      logger.info("interferometer = " + this.interferometer.getName());
      logger.info("instrument     = " + this.instrument.getName());
    }
  }

  private void prepareBeams() {

    // Get chosen stations :
    final List<Station> stations = this.observation.getInstrumentConfiguration().getStationList();

    if (logger.isLoggable(Level.INFO)) {
      logger.info("stations = " + stations);
    }

    final int nBeams = stations.size();

    this.beams = new ArrayList<Beam>(nBeams);

    for (Station s : stations) {
      this.beams.add(new Beam(s));
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

            if (logger.isLoggable(Level.INFO)) {
              logger.info("station = " + b.getStation() + " = " + b.getChannel().getName());
            }
            break;
          }
        }
        if (b.getChannel() == null) {
          throw new IllegalStateException("Impossible to associate a channel to every station among [" + stations + "].");
        }
      }


    /*
    DO I=1,OBS_NDL
    DLALLOCATED(I)=.FALSE.
    ENDDO
    CHAIN='I-HORIZON, No DLs specified, using: '
    LC = LENC(CHAIN)
    !     ...following the SW possibilities...
    DO I=1,NSTAT
    DO J=1,OBS_NDL
    IF ((.NOT.DLALLOCATED(J)).AND.
    &              SWITCH_OPD(J,SLIST(I)).NE.SWITCH_BLANK) THEN
    DLALLOCATED(J)=.TRUE.
    DLLIST(I)=J
    T(I) = SWITCH_OPD(DLLIST(I),SLIST(I))
    !     Add pops value
    IF (DDO_POPS) THEN
    write(*,*) 'adding pops[',i,']=',MYPOPS(I)
    T(I) = T(I) + MYPOPS(I)
    ENDIF
    WRITE(CHAIN(LC+1:),'(A,A)') DL_NAME(DLLIST(I)),','
    LC = LENC(CHAIN)
    GOTO 22
    ENDIF
    ENDDO
    */

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
      // Simpler interferometer : no channel definition or switchyard :

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

    if (logger.isLoggable(Level.INFO)) {
      logger.info("Beams = " + this.beams);
    }
  }

  private void prepareBaseLines() {

    final int nBeams = this.beams.size();

    Beam b1, b2;

    double x,y,z,t, wMin, wMax;
    for (int i = 0; i < nBeams; i++) {
      for (int j = i + 1; j < nBeams; j++) {
        b1 = this.beams.get(i);
        b2 = this.beams.get(j);

        x = b1.getStation().getRelativePosition().getPosX() - b2.getStation().getRelativePosition().getPosX();
        y = b1.getStation().getRelativePosition().getPosY() - b2.getStation().getRelativePosition().getPosY();
        z = b1.getStation().getRelativePosition().getPosZ() - b2.getStation().getRelativePosition().getPosZ();

        t = b1.getOpticalLength() - b2.getOpticalLength();
        
        // 2 DL for 2 telescopes => double throw :
        // note : for now, all DLs are equivalent (same throw).

        wMin = t - b2.getDelayLine().getMaximumThrow();
        wMax = t + b1.getDelayLine().getMaximumThrow();

/*
        XXX(KKK)= -STATX(SLIST(J))+STATX(SLIST(I))
        YYY(KKK)= -STATY(SLIST(J))+STATY(SLIST(I))
        ZZZ(KKK)= -STATZ(SLIST(J))+STATZ(SLIST(I))
        TTT(KKK)= T(I)-T(J)-DL_THROW(DLLIST(J))
        THROW(KKK)= T(I)-T(J)+DL_THROW(DLLIST(I))
 */

        this.baseLines.add(new BaseLine(b1, b2, x, y, z));

        this.wRanges.add(new Range(wMin, wMax));
      }
    }
  }

  private Date jdToDate(final double jd) {
    return this.sc.toDate(jd, this.useLST);
  }
}
