/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: UVCoverageService.java,v 1.2 2010-01-12 17:10:08 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/01/08 16:50:53  bourgesl
 * initial uv coverage
 *
 */
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.uvcoverage.UVCoverageData;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.uvcoverage.UVBaseLineData;
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

  /* reused observability data */
  /** base line list */
  private List<BaseLine> baseLines = null;
  /** star data */
  private StarData starData;

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
      final ObservabilityData obsData = this.observation.getObservabilityData();

      if (obsData == null) {
        this.data = null;
      } else {

        this.baseLines = obsData.getBaseLines();

        this.starData = obsData.getStarData(this.targetName);

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("starData : " + this.starData);
        }

        // target name :
        this.data.setName(this.targetName);

        if (this.starData.getHaElev() > 0d) {

          computeUVSupport();
        }
      }

    } catch (RuntimeException re) {
      logger.log(Level.SEVERE, "compute failure :", re);
      logger.log(Level.SEVERE, "observation : " + ObservationManager.toString(this.observation));
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

        j++;
      }

      uvData.setNPoints(j);
      uvData.setU(u);
      uvData.setV(v);

      targetUVRiseSet.add(uvData);
    }

    this.data.setTargetUVRiseSet(targetUVRiseSet);
  }
}
