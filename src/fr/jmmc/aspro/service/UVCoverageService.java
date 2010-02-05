/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: UVCoverageService.java,v 1.11 2010-02-05 09:46:00 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.10  2010/02/04 17:05:06  bourgesl
 * UV bounds are coming from UVCoverageService
 *
 * Revision 1.9  2010/02/04 14:54:11  bourgesl
 * UVMapData refactoring (uvRect, min/max values) to keep the color mapping consistent when zooming
 * Compute an sub Image when a zoom occurs while the correct model is computed in the background
 *
 * Revision 1.8  2010/02/03 09:48:53  bourgesl
 * target model uvmap added on the uv coverage with zooming supported
 *
 * Revision 1.7  2010/01/29 16:01:20  bourgesl
 * added comments + uv max
 *
 * Revision 1.6  2010/01/21 16:41:30  bourgesl
 * added HA min / max sliders and used only to constraint the UV tracks
 *
 * Revision 1.5  2010/01/19 13:20:20  bourgesl
 * NPE fixed when the observability displays the baseline limits
 *
 * Revision 1.4  2010/01/15 16:14:16  bourgesl
 * added computation of UV points compatible with observability ranges, bandpass and sampling periodicity
 *
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

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.uvcoverage.UVCoverageData;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.uvcoverage.UVBaseLineData;
import fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData;
import fr.jmmc.aspro.util.AngleUtils;
import fr.jmmc.mcs.model.ModelUVMapService;
import java.awt.geom.Rectangle2D;
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
  /** HA min in decimal hours */
  private final double haMin;
  /** HA max */
  private final double haMax;

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
  /** maximum U or V coordinate (corrected by the minimal wavelength) */
  private double uvMax;

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
   * @param haMin HA min in decimal hours
   * @param haMax HA max in decimal hours
   */
  public UVCoverageService(final ObservationSetting observation, final String targetName, final double haMin, final double haMax) {
    this.observation = observation;
    this.targetName = targetName;
    this.haMin = haMin;
    this.haMax = haMax;
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

        if (this.starData != null) {
          // Note : for Baseline limits, the starData is null
          // (target observability is not available) :

          // target name :
          this.data.setName(this.targetName);

          // wave length :
          this.data.setLambda(this.lambda);

          // Is the target visible :
          if (this.starData.getHaElev() > 0d) {

            computeUVSupport();

            computeObservableUV();

            if (logger.isLoggable(Level.FINE)) {
              logger.fine("UV coordinate maximum = [" + this.uvMax + "]");
            }
          }

          // TODO : get max base line from user choice :

          // uv Max = max base line * uv margin / minimum wave length
          this.data.setUvMax(this.uvMax);

          final Rectangle2D.Float uvRect = new Rectangle2D.Float();
          uvRect.setFrameFromDiagonal(-uvMax, -uvMax, uvMax, uvMax);

          // Compute Target Model for the UV coverage limits :
          this.data.setUvMapData(ModelUVMapService.computeUVMap(
                  ObservationManager.getTarget(this.observation, this.targetName).getModels(),
                  uvRect,
                  ModelUVMapService.ImageMode.AMP));
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

    // 10 minutes is enough to get pretty ellipse :
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

        // Baseline projected vector (m) :
        u[j] = CalcUVW.computeU(baseLine, haRad);
        v[j] = CalcUVW.computeV(precDEC, baseLine, haRad);

        // wavelength correction :

        // Spatial frequency (rad-1) :
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

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("obsRangesHA = " + obsRangesHA);
    }

    if (obsRangesHA != null) {

      final double ha1 = this.haMin; /* -12d */
      final double ha2 = this.haMax; /* +12d */

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

        n = (int) Math.round((ha2 - ha1) / step) + 1;

        u = new double[n];
        v = new double[n];
        u2 = new double[n];
        v2 = new double[n];

        j = 0;

        for (double ha = ha1; ha <= ha2; ha += step) {

          // check HA :
          observable = checkObservability(ha, obsRangesHA);

          if (observable) {
            haRad = AngleUtils.hours2rad(ha);
            // Baseline projected vector (m) :
            u[j] = CalcUVW.computeU(baseLine, haRad);
            v[j] = CalcUVW.computeV(precDEC, baseLine, haRad);

            u2[j] = u[j];
            v2[j] = v[j];

            // wavelength correction :

            // Spatial frequency (rad-1) :
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
      if (ha >= range.getMin() && ha <= range.getMax()) {
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

    this.lambdaMin = insMode.getWaveLengthMin() * AsproConstants.MICRO_METER;
    this.lambdaMax = insMode.getWaveLengthMax() * AsproConstants.MICRO_METER;

    this.lambda = (this.lambdaMax + this.lambdaMin) / 2d;

    // hour angle step in decimal hours :
    this.haStep = this.observation.getInstrumentConfiguration().getSamplingPeriod() / 60d;

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("lambdaMin : " + this.lambdaMin);
      logger.fine("lambda    : " + this.lambda);
      logger.fine("lambdaMax : " + this.lambdaMax);
    }

    final InterferometerConfiguration intConf = this.observation.getInterferometerConfiguration().getInterferometerConfiguration();
    if (intConf == null) {
      throw new IllegalStateException("prepareObservation : the interferometerConfiguration is null !");
    }

    final InterferometerDescription interferometer = intConf.getInterferometer();

    // uv Max = max base line * uv margin / minimum wave length

    // note : use the minimum wave length to make all uv segment visible :

    this.uvMax = Math.round(interferometer.getMaxBaseLine() / this.lambdaMin);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("uvMax : " + this.uvMax);
    }
  }
}
