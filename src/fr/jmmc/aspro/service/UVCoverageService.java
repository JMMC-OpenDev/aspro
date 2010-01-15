/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: UVCoverageService.java,v 1.4 2010-01-15 16:14:16 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.3  2010/01/15 13:51:27  bourgesl
 * illegalStateException if any transient field is undefined
 *
 * Revision 1.2  2010/01/12 17:10:08  bourgesl
 * less log INFO outputs
 *
 * Revision 1.1  2010/01/08 16:50:53  bourgesl
 * initial uv coverage
 *
 */
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.uvcoverage.UVCoverageData;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.uvcoverage.UVBaseLineData;
import fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData;
import fr.jmmc.aspro.util.AngleUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

/**
 * This service is dedicated to compute the UV tracks for a given target
 * @author bourgesl
 */
public class UVCoverageService {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.service.UVCoverageService";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /* members */

  /* output */
  /** observability data */
  private UVCoverageData data = new UVCoverageData();

  /* inputs */
  /** observation settings */
  private final ObservationSetting observation;
  /** target to use */
  private final String targetName;

  /* internal */
  /** Get the current thread to check if the computation is interrupted */
  private final Thread currentThread = Thread.currentThread();
  /** hour angle step (see samplingPeriod) */
  private double haStep;
  /** minimal wavelength */
  private double lambdaMin;
  /** central wavelength */
  private double lambda;
  /** maximal wavelength */
  private double lambdaMax;

  /* reused observability data */
  /** observability data */
  private ObservabilityData obsData = null;
  /** base line list */
  private List<BaseLine> baseLines = null;
  /** star data */
  private StarData starData = null;

  /**
   * Constructor.
   * Note : This service is statefull so it can not be reused by several calls.
   *
   * @param observation observation settings
   * @param targetName target name
   */
  public UVCoverageService(final ObservationSetting observation, final String targetName) {
    this.observation = observation;
    this.targetName = targetName;
  }

  /**
   * Main operation to compute the UV tracks for a given target
   *
   * @return UVCoverageData container
   */
  public UVCoverageData compute() {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("compute : " + this.observation);
    }

    // Start the computations :
    final long start = System.nanoTime();

    try {
      // check if observability data are available :
      this.obsData = this.observation.getObservabilityData();

      if (this.obsData == null) {
        // invalid data because the observability data are not available :
        this.data = null;
      } else {
        // Get instrument and observability data :
        prepareObservation();

        // target name :
        this.data.setName(this.targetName);

        // Is the target visible :
        if (this.starData.getHaElev() > 0d) {

          computeUVSupport();

          computeObservableUV();
        }
      }

      // fast interrupt :
      if (this.currentThread.isInterrupted()) {
        return null;
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

  private void computeUVSupport() {

    // 10 minutes :
    final double step = 10d / 60d;

    final double haElev = this.starData.getHaElev();

    // precessed target declination in rad :
    final double precDEC = Math.toRadians(this.starData.getPrecDEC());

    final int sizeBL = this.baseLines.size();

    final List<UVBaseLineData> targetUVRiseSet = new ArrayList<UVBaseLineData>(sizeBL);

    UVBaseLineData uvData;
    BaseLine baseLine;
    double[] u;
    double[] v;
    int j, n;

    double haRad;

    for (int i = 0, size = this.baseLines.size(); i < size; i++) {
      baseLine = this.baseLines.get(i);

      uvData = new UVBaseLineData(baseLine.getName());

      n = (int) Math.round(2d * haElev / step) + 1;

      u = new double[n];
      v = new double[n];

      j = 0;

      for (double ha = -haElev; ha <= haElev; ha += step) {

        haRad = AngleUtils.hours2rad(ha);
        u[j] = CalcUVW.computeU(baseLine, haRad);
        v[j] = CalcUVW.computeV(precDEC, baseLine, haRad);

        // wavelength correction :

        u[j] = u[j] / this.lambda;
        v[j] = v[j] / this.lambda;

        j++;
      }

      uvData.setNPoints(j);
      uvData.setU(u);
      uvData.setV(v);

      targetUVRiseSet.add(uvData);

      // fast interrupt :
      if (this.currentThread.isInterrupted()) {
        return;
      }

    }

    this.data.setTargetUVRiseSet(targetUVRiseSet);
  }

  private void computeObservableUV() {

    final List<Range> obsRangesHA = this.starData.getObsRangesHA();

    logger.severe("obsRangesHA = " + obsRangesHA);

    if (obsRangesHA != null) {

      // TODO : get haMin / haMax from parameters :
      final double haMin = -12d;
      final double haMax = 12d;

      final double step = this.haStep;

      // precessed target declination in rad :
      final double precDEC = Math.toRadians(this.starData.getPrecDEC());

      final int sizeBL = this.baseLines.size();

      final List<UVRangeBaseLineData> targetUVObservability = new ArrayList<UVRangeBaseLineData>(sizeBL);

      UVRangeBaseLineData uvData;
      BaseLine baseLine;
      double[] u;
      double[] v;
      double[] u2;
      double[] v2;
      int j, n;

      double haRad;

      boolean observable;

      for (int i = 0, size = this.baseLines.size(); i < size; i++) {
        baseLine = this.baseLines.get(i);

        uvData = new UVRangeBaseLineData(baseLine.getName());

        n = (int) Math.round((haMax - haMin) / step) + 1;

        u = new double[n];
        v = new double[n];
        u2 = new double[n];
        v2 = new double[n];

        j = 0;

        for (double ha = haMin; ha <= haMax; ha += step) {

          // check HA :
          observable = checkObservability(ha, obsRangesHA);

          if (observable) {
            haRad = AngleUtils.hours2rad(ha);
            u[j] = CalcUVW.computeU(baseLine, haRad);
            v[j] = CalcUVW.computeV(precDEC, baseLine, haRad);

            u2[j] = u[j];
            v2[j] = v[j];

            // wavelength correction :

            u[j] = u[j] / this.lambdaMin;
            v[j] = v[j] / this.lambdaMin;

            u2[j] = u2[j] / this.lambdaMax;
            v2[j] = v2[j] / this.lambdaMax;

            j++;
          }
        }

        uvData.setNPoints(j);
        uvData.setU(u);
        uvData.setV(v);
        uvData.setU2(u2);
        uvData.setV2(v2);

        targetUVObservability.add(uvData);

        // fast interrupt :
        if (this.currentThread.isInterrupted()) {
          return;
        }

      }

      this.data.setTargetUVObservability(targetUVObservability);
    }
  }

  private boolean checkObservability(final double ha, final List<Range> obsRangesHA) {
    for (Range range : obsRangesHA) {
      if (ha > range.getMin() && ha < range.getMax()) {
        return true;
      }
    }
    return false;
  }

  /**
   * Define the baselines, star data and instrument mode's wavelengths
   * @throws IllegalStateException if the instrument mode is undefined
   */
  private void prepareObservation() throws IllegalStateException {
    // Get baselines :
    this.baseLines = this.obsData.getBaseLines();

    // Get starData for the selected target name :
    this.starData = this.obsData.getStarData(this.targetName);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("starData : " + this.starData);
    }

    final FocalInstrumentMode insMode = this.observation.getInstrumentConfiguration().getFocalInstrumentMode();
    if (insMode == null) {
      throw new IllegalStateException("prepareObservation : the instrumentMode is null !");
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("instrumentMode : " + insMode.getName());
    }

    this.lambdaMin = insMode.getWaveLengthMin() * 1e-6d;
    this.lambdaMax = insMode.getWaveLengthMax() * 1e-6d;

    this.lambda = (this.lambdaMax + this.lambdaMin) / 2d;

    // hour angle step in decimal hour :
    this.haStep = this.observation.getInstrumentConfiguration().getSamplingPeriod() / 60d;

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("lambdaMin : " + this.lambdaMin);
      logger.fine("lambda    : " + this.lambda);
      logger.fine("lambdaMax : " + this.lambdaMax);
    }
  }
}
