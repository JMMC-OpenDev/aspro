/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: NoiseService.java,v 1.2 2010-07-23 10:37:44 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/07/22 15:46:05  bourgesl
 * Band mapping done
 *
 */
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.model.oi.AdaptiveOptics;
import fr.jmmc.aspro.model.oi.AtmosphereQuality;
import fr.jmmc.aspro.model.oi.FocalInstrument;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.FringeTracker;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.model.oi.Telescope;
import fr.jmmc.aspro.model.util.AtmosphereQualityUtils;
import java.util.List;

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

  /* members */

  /* interferometer parameters */
  /** Telescope Diameter (m) */
  private double telDiam = 1d;
  /** Number of telescopes interfering */
  private int nbTel = 1;

  /* adaptive optics parameters */
  /** seeing (arc sec) */
  private double seeing = 1d;
  /** Number of Actuators of AO */
  private int nbOfActuators = 1;
  /** AO band */
  private SpectralBand aoBand;

  /* instrument parameters */
  /** Total Acquisition time per observed u,v point (s) */
  private double totalObsTime = 300d;
  /** Transmission of interferometer+instrument at observed wavelength */
  private double transmission = 1d;
  /** Detector individual integration time (s) */
  private double dit = 0.01d;
  /** Detector readout noise */
  private double ron = 12d;
  /** Detector is non-linear above (to avoid saturation/non-linearity) */
  private double detectorSaturation = 100000d;
  /** Instrumental Visibility [0.0-1.0] */
  private double instrumentalVisibility = 1d;
  /** Typical Visibility Bias (%) */
  private double instrumentalVisibilityBias = 0d;
  /** Typical Phase/Phase Closure Bias (deg) */
  private double instrumentalPhaseBias = 0d;
  /** number of pixels to code all fringes together (interferometric channel) */
  private int nbPixInterf = 600;
  /** number of pixels to code each photometric channel */
  private int nbPixPhoto = 4;
  /** fraction of flux going into interferometric channel */
  private double fracFluxInInterferometry = 0.9d;

  /* instrument mode parameters */
  /** Observation central wavelength (microns) */
  private double lambda = 2.2d;
  /** Spectral Resolution */
  private double spectralResolution = 0.0d;

  /* fringe tracker parameters */
  /** Fringe Tracker is Present */
  private boolean fringeTrackerPresent = false;
  /** Fringe Tracker (multiplicative) Instrumental Visibility */
  private double fringeTrackerInstrumentalVisibility = 1d;
  /** Fringe Tracker Usage limit (mag) */
  private double fringeTrackerLimit = 12d;
  /** Fringe Tracker Max Integration Time (s) */
  private double fringeTrackerMaxIntTime = 30d;
  /** FT band */
  private SpectralBand ftBand;

  /* object parameters */
  /** Magnitude in Observing Band (see lambda) (mag) */
  private double objectMag = 0d;
  /** Magnitude in Fringe Tracker's Band of FT ref star (mag) */
  private double fringeTrackerMag = 0d;
  /** Magnitude in V Band (for AO performances / for strehl) (mag) */
  private double adaptiveOpticsMag = 0d;

  /* internal */
  /** number of photons per pixel in interferometer channel */
  private double nbPhotPerPixelInI;
  /** number of photons per pixel in each photometric channel */
  private double nbPhotPerPixelInP;
  /** number of frames per second */
  private double frameRate;
  /** total instrumental visibility (with FT if any) */
  private double vInst;

  /**
   * Protected constructor
   * @param observation observation settings
   * @param target target to use
   */
  protected NoiseService(final ObservationSetting observation, final Target target) {
    // extract parameters in observation and configuration :
    prepareInterferometer(observation);
    prepareInstrument(observation);
    prepareFringeTracker(observation, target);
    prepareTarget(target);

    // init other parameters :
  }

  /**
   * Prepare interferometer and AO parameters
   * @param observation observation settings
   */
  private void prepareInterferometer(final ObservationSetting observation) {

    final List<Station> stations = observation.getInstrumentConfiguration().getStationList();

    this.nbTel = stations.size();

    // All telescopes have the same diameter :
    final Telescope tel = stations.get(0).getTelescope();

    this.telDiam = tel.getDiameter();

    // AO :
    final AdaptiveOptics ao = tel.getAdaptiveOptics();
    if (ao != null) {
      this.aoBand = ao.getBand();
      this.nbOfActuators = ao.getNumberActuators();
    }

    // Seeing :
    final AtmosphereQuality atmQual = observation.getWhen().getAtmosphereQuality();
    if (atmQual != null) {
      this.seeing = AtmosphereQualityUtils.getSeeing(atmQual);
    }

    logger.severe("nbTel                      = " + nbTel);
    logger.severe("telDiam                    = " + telDiam);
    logger.severe("aoBand                     = " + aoBand);
    logger.severe("nbOfActuators              = " + nbOfActuators);
    logger.severe("seeing                     = " + seeing);
  }

  /**
   * Prepare instrument and mode parameters
   * @param observation observation settings
   */
  private void prepareInstrument(final ObservationSetting observation) {

    if (observation.getInstrumentConfiguration().getAcquisitionTime() != null) {
      this.totalObsTime = observation.getInstrumentConfiguration().getAcquisitionTime().doubleValue();
    }

    final FocalInstrument instrument = observation.getInstrumentConfiguration().getInstrumentConfiguration().getFocalInstrument();

    this.transmission = instrument.getTransmission();
    this.dit = instrument.getDit();
    this.ron = instrument.getRon();
    this.detectorSaturation = instrument.getDetectorSaturation();
    this.instrumentalVisibility = instrument.getInstrumentVisibility();
    this.instrumentalVisibilityBias = instrument.getInstrumentVisibilityBias();
    this.instrumentalPhaseBias = instrument.getInstrumentPhaseBias();
    this.nbPixInterf = instrument.getNbPixInterferometry();
    this.nbPixPhoto = instrument.getNbPixPhotometry();
    this.fracFluxInInterferometry = instrument.getFracFluxInInterferometry();

    final FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();

    this.lambda = insMode.getWaveLength();
    this.spectralResolution = insMode.getResolution();

    logger.severe("totalObsTime               = " + totalObsTime);
    logger.severe("transmission               = " + transmission);
    logger.severe("dit                        = " + dit);
    logger.severe("ron                        = " + ron);
    logger.severe("detectorSaturation         = " + detectorSaturation);
    logger.severe("instrumentalVisibility     = " + instrumentalVisibility);
    logger.severe("instrumentalVisibilityBias = " + instrumentalVisibilityBias);
    logger.severe("instrumentalPhaseBias      = " + instrumentalPhaseBias);
    logger.severe("nbPixInterf                = " + nbPixInterf);
    logger.severe("nbPixPhoto                 = " + nbPixPhoto);
    logger.severe("fracFluxInInterferometry   = " + fracFluxInInterferometry);
    logger.severe("lambda                     = " + lambda);
    logger.severe("spectralResolution         = " + spectralResolution);
  }

  /**
   * Prepare fringe tracker parameters
   * @param observation observation settings
   * @param target target to use
   */
  private void prepareFringeTracker(final ObservationSetting observation, final Target target) {
    final TargetConfiguration targetConf = target.getConfiguration();

    if (targetConf != null && targetConf.getFringeTrackerMode() != null) {
      final InterferometerDescription interferometer = observation.getInterferometerConfiguration().getInterferometerConfiguration().getInterferometer();

      final FringeTracker ft = interferometer.getFringeTracker();
      if (ft != null) {
        this.fringeTrackerPresent = true;
        this.fringeTrackerInstrumentalVisibility = ft.getInstrumentVisibility();
        this.fringeTrackerLimit = ft.getMagLimit();
        this.fringeTrackerMaxIntTime = ft.getMaxIntegration();
        this.ftBand = ft.getBand();
      }
    }

    logger.severe("fringeTrackerPresent       = " + fringeTrackerPresent);
    if (fringeTrackerPresent) {
      logger.severe("fringeTrackerInstrumentalVisibility = " + fringeTrackerInstrumentalVisibility);
      logger.severe("fringeTrackerLimit         = " + fringeTrackerLimit);
      logger.severe("fringeTrackerMaxIntTime    = " + fringeTrackerMaxIntTime);
      logger.severe("ftBand                     = " + ftBand);
    }
  }

  /**
   * Prepare object parameters
   * @param target target to use
   */
  private void prepareTarget(final Target target) {
    Double flux;

    this.objectMag = 0d;
    logger.severe("objectMag                  = " + objectMag);

    if (fringeTrackerPresent) {
      flux = target.getFlux(this.ftBand);
      this.fringeTrackerMag = (flux != null) ? flux.doubleValue() : 0d;
      logger.severe("fringeTrackerMag           = " + fringeTrackerMag);
    }

    flux = target.getFlux(this.aoBand);
    this.adaptiveOpticsMag = (flux != null) ? flux.doubleValue() : 0d;
    logger.severe("adaptiveOpticsMag          = " + adaptiveOpticsMag);
  }

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
     * @param lambda central wave length in microns
     * @param bandWidth band width in microns
     * @param logFluxZero log10 zero magnitude flux at band in W/m^2/m
     * @param strehlMax maximum strehl ratio
     */
    private Band(final String name, final double lambda, final double bandWidth, final double logFluxZero, final double strehlMax) {
      this.name = name;
      this.lambda = lambda;
      this.bandWidth = bandWidth;
      this.logFluxZero = logFluxZero;
      this.strehlMax = strehlMax;
    }

    /**
     * Return the band name
     * @return band name
     */
    public String getName() {
      return name;
    }

    /**
     * Return the central wave length in microns
     * @return central wave length in microns
     */
    public double getLambda() {
      return lambda;
    }

    /**
     * Return the band width in microns
     * @return band width in microns
     */
    public double getBandWidth() {
      return bandWidth;
    }

    /**
     * Return the log10 zero magnitude flux at band in W/m^2/m
     * @return log10 zero magnitude flux at band in W/m^2/m
     */
    public double getLogFluxZero() {
      return logFluxZero;
    }

    /**
     * Return the maximum strehl ratio
     * @return maximum strehl ratio
     */
    public double getStrehlMax() {
      return strehlMax;
    }
  }
}
