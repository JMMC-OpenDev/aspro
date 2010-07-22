/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: NoiseService.java,v 1.1 2010-07-22 15:46:05 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.model.oi.ObservationSetting;

/**
 * This class performs the noise modelling of visibility data (error and noise) 
 * from the current observation
 *
 * Note : this code is inspired by the Aspro/tasks/lib/noise_lib.f90
 *
 * @author bourgesl
 */
public final class NoiseService {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.service.NoiseService";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** Planck's constant in standard units (6.6262e-34) */
  public final static double H_PLANCK = 6.62606896e-34d;
  /** Speed of light (6.6262e-34) */
  public final static double C_LIGHT = 2.99792458e8d;

  /**
   * Photometry band related information
   */
  public enum Band {

    /** U (ultra violet) */
    U("u", 0.334d, 0.066d, -1.4d, 0.3d),
    /** B (Visible) */
    B("b", 0.461875d, 0.08175d, -1.2d, 0.48d),
    /** V (Visible) */
    V("v", 0.556d, 0.1105d, -1.44d, 0.5d),
    /** R (Visible) */
    R("r", 0.6625d, 0.10651d, -1.65d, 0.65d),
    /** I (Near Infrared) */
    I("i", 0.869625d, 0.31176, -1.94d, 0.75d),
    /** J (Near Infrared) */
    J("j", 1.2365d, 0.426d, -2.5d, 0.77d),
    /** H (Near Infrared) */
    H("h", 1.679625d, 0.46425d, -2.94d, 0.84d),
    /** K (Near Infrared) */
    K("k", 2.365625d, 0.912d, -3.4d, 0.93d),
    /** L (Near Infrared) */
    L("l", 3.45875d, 1.2785, -4.15d, 0.972d),
    /** M (Mid Infrared) */
    M("m", 6.4035d, 4.615d, -4.69d, 0.985d),
    /** N (Mid Infrared) */
    N("n", 11.63d, 5.842d, -5.91d, 0.996d),
    /** Q (Mid Infrared) */
    Q("q", 16.575d, 4.05, -7.17d, 0.999d);
    /** single char band name (lower case) */
    private final String name;
    /** central wave length in microns */
    private final double lambda;
    /** band width in microns */
    private final double bandWidth;
    /** log10 zero magnitude flux at band in W/m^2/m */
    private final double logFluxZero;
    /** maximum strehl ratio */
    private final double strehlMax;

    /**
     * Custom constructor
     * @param name band name
     */
    private Band(final String name, final double lambda, final double bandWidth, final double logFluxZero, final double strehlMax) {
      this.name = name;
      this.lambda = lambda;
      this.bandWidth = bandWidth;
      this.logFluxZero = logFluxZero;
      this.strehlMax = strehlMax;
    }

    public String getName() {
      return name;
    }

    public double getLambda() {
      return lambda;
    }

    public double getBandWidth() {
      return bandWidth;
    }

    public double getLogFluxZero() {
      return logFluxZero;
    }

    public double getStrehlMax() {
      return strehlMax;
    }
  }

  /**
   * Protected constructor
   * @param observation observation settings
   */
  protected NoiseService(final ObservationSetting observation) {
    // TODO : extract parameters :
  }
}
