/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: NoiseService.java,v 1.9 2010-08-25 15:56:15 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.8  2010/08/24 16:10:55  bourgesl
 * removed computeVisError and Vis2Error computation to be corrected later (gilles)
 *
 * Revision 1.7  2010/08/20 12:02:20  bourgesl
 * comments
 *
 * Revision 1.6  2010/08/19 15:29:13  bourgesl
 * minor changes to clarify code
 *
 * Revision 1.5  2010/08/19 10:53:23  bourgesl
 * proper noise computation for VIS2 but first try for VISDATA/VISAMP/VISPHI
 *
 * Revision 1.4  2010/07/23 15:22:36  bourgesl
 * first try to compute noise and errors on VIS2 like Aspro 1
 *
 * Revision 1.3  2010/07/23 12:29:16  bourgesl
 * set object parameters (mag in bands)
 *
 * Revision 1.2  2010/07/23 10:37:44  bourgesl
 * added parameter initialisation
 *
 * Revision 1.1  2010/07/22 15:46:05  bourgesl
 * Band mapping done
 *
 */
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.AsproConstants;
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
import java.util.Random;
import java.util.logging.Level;

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
  /** Speed of light (2.99792458e8) */
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
  /** instrument band */
  private SpectralBand insBand;

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
  /** number of photons in interferometer channel */
  private double nbPhotonInI;
  /** number of photons in each photometric channel */
  private double nbPhotonInP;
  /** number of frames per second */
  private double frameRate;
  /** total instrumental visibility (with FT if any) */
  private double vinst;
  /** random generator */
  public final Random random = new Random();

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
    initParameters();
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

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("nbTel                      = " + nbTel);
      logger.fine("telDiam                    = " + telDiam);
      logger.fine("aoBand                     = " + aoBand);
      logger.fine("nbOfActuators              = " + nbOfActuators);
      logger.fine("seeing                     = " + seeing);
    }
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

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("totalObsTime               = " + totalObsTime);
      logger.fine("transmission               = " + transmission);
      logger.fine("dit                        = " + dit);
      logger.fine("ron                        = " + ron);
      logger.fine("detectorSaturation         = " + detectorSaturation);
      logger.fine("instrumentalVisibility     = " + instrumentalVisibility);
      logger.fine("instrumentalVisibilityBias = " + instrumentalVisibilityBias);
      logger.fine("instrumentalPhaseBias      = " + instrumentalPhaseBias);
      logger.fine("nbPixInterf                = " + nbPixInterf);
      logger.fine("nbPixPhoto                 = " + nbPixPhoto);
      logger.fine("fracFluxInInterferometry   = " + fracFluxInInterferometry);
      logger.fine("lambda                     = " + lambda);
      logger.fine("spectralResolution         = " + spectralResolution);
    }
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

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("fringeTrackerPresent       = " + fringeTrackerPresent);
    }
    if (fringeTrackerPresent) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("fringeTrackerInstrumentalVisibility = " + fringeTrackerInstrumentalVisibility);
        logger.fine("fringeTrackerLimit         = " + fringeTrackerLimit);
        logger.fine("fringeTrackerMaxIntTime    = " + fringeTrackerMaxIntTime);
        logger.fine("ftBand                     = " + ftBand);
      }
    }
  }

  /**
   * Prepare object parameters
   * @param target target to use
   */
  private void prepareTarget(final Target target) {
    Double flux;

    final Band band = Band.findBand(this.lambda);

    this.insBand = Band.findBand(band);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("band                       = " + band);
      logger.fine("insBand                    = " + insBand);
    }

    flux = target.getFlux(this.insBand);
    this.objectMag = (flux != null) ? flux.doubleValue() : 0d;
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("objectMag                  = " + objectMag);
    }

    if (fringeTrackerPresent) {
      flux = target.getFlux(this.ftBand);
      this.fringeTrackerMag = (flux != null) ? flux.doubleValue() : 0d;
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("fringeTrackerMag           = " + fringeTrackerMag);
      }
    }

    flux = target.getFlux(this.aoBand);
    this.adaptiveOpticsMag = (flux != null) ? flux.doubleValue() : 0d;
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("adaptiveOpticsMag          = " + adaptiveOpticsMag);
    }
  }

  /**
   * Initialise other parameters
   */
  private void initParameters() {

    this.vinst = instrumentalVisibility;
    if (this.fringeTrackerPresent) {
      this.vinst *= this.fringeTrackerInstrumentalVisibility;
    }

    final Band band = Band.findBand(this.lambda);

    double dlam;
    if (this.spectralResolution <= 1d) {
      dlam = band.getBandWidth();
    } else {
      dlam = this.lambda / this.spectralResolution;
    }

    // strehl ratio of AO :
    final double sr = Band.strehl(this.adaptiveOpticsMag, this.lambda, this.telDiam, this.seeing, this.nbOfActuators);

    // nb of photons m^-2 m^-1 :
    final double fzero = Math.pow(10d, band.getLogFluxZero())
            / (H_PLANCK * C_LIGHT / (this.lambda * AsproConstants.MICRO_METER));

    final double nbTotalPhotPerS = fzero * this.nbTel * Math.PI * Math.pow(this.telDiam / 2d, 2d)
            * (dlam * AsproConstants.MICRO_METER) * this.transmission * sr
            * Math.pow(10d, -0.4d * this.objectMag);

    // number of expected photoevents in dit :
    double nbTotalPhot = nbTotalPhotPerS * this.dit;

    // fraction of total interferometric flux in peak pixel :
    final double peakflux = this.fracFluxInInterferometry * nbTotalPhot / this.nbPixInterf;

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("adaptiveOpticsMag          = " + adaptiveOpticsMag);
      logger.fine("strehl ratio               = " + sr);
      logger.fine("dlam                       = " + dlam);
      logger.fine("nbTotalPhot                = " + nbTotalPhot);
      logger.fine("peakflux                   = " + peakflux);
    }

    int nbFrameToSaturation;
    if (this.detectorSaturation < peakflux) {
      // the dit is too long
      this.dit *= this.detectorSaturation / peakflux;

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("DIT too long. Adjusting it to (possibly impossible) : " + dit + " s");
      }

      nbFrameToSaturation = 1;
    } else {
      nbFrameToSaturation = (int) Math.round(this.detectorSaturation / peakflux);
    }

    if (this.fringeTrackerPresent && (this.fringeTrackerMag <= this.fringeTrackerLimit) && (nbFrameToSaturation > 1)) {
      // FT is asked, can work, and is useful (need to integrate longer)
      this.dit = Math.min(this.dit * nbFrameToSaturation, this.totalObsTime);
      this.dit = Math.min(this.dit, this.fringeTrackerMaxIntTime);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("Observation can take advantage of FT. Adjusting DIT to : " + dit + " s");
      }
    }

    nbTotalPhot = nbTotalPhotPerS * this.dit;

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("nbFrameToSaturation        = " + nbFrameToSaturation);
      logger.fine("nbTotalPhot                = " + nbTotalPhot);
    }

    this.frameRate = 1d / this.dit;

    // give back the two useful values for the noise estimate :

    // number of photons in interferometric channel :
    this.nbPhotonInI = nbTotalPhot * this.fracFluxInInterferometry;
    // number of photons in photometric channel :
    this.nbPhotonInP = nbTotalPhot * (1 - this.fracFluxInInterferometry) / this.nbTel;

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("frameRate                  = " + frameRate);
      logger.fine("nbPhotonInI                = " + nbPhotonInI);
      logger.fine("nbPhotonInP                = " + nbPhotonInP);
    }
  }

  /**
   * Return the correlated flux of the object
   * @param vis2 square visibility
   * @return correlated flux
   */
  public double computeCorrelatedFlux(final double vis2) {

    // squared correlated flux
    double fcorrelsq = Math.pow(this.nbPhotonInI / this.nbTel, 2d) * vis2;

    return Math.sqrt(fcorrelsq);
  }

  /**
   * Compute error on square visibility
   *
   * @param vis visibility
   * @return square visiblity error
   */
  public double computeVis2Error(final double vis) {

    // include instrumental visib
    double visib = vis * this.vinst;

    // squared correlated flux
    double fcorrelsq = Math.pow(this.nbPhotonInI * visib / this.nbTel, 2d);

    final double sfcorrelsq = 2d * Math.pow(this.nbPhotonInI, 3d) * Math.pow(visib / this.nbTel, 2d)
            + 4d * Math.pow(this.nbPhotonInI * visib / this.nbTel, 2d) + Math.pow(this.nbPhotonInI, 2d) + this.nbPhotonInI
            + Math.pow(this.nbPixInterf, 2d) * Math.pow(this.ron, 4d) + 3d * this.nbPixInterf * Math.pow(this.ron, 4d)
            + 2d * this.nbPixInterf * this.nbPhotonInI * Math.pow(this.ron, 2d)
            + 2d * this.nbPixInterf * Math.pow(this.ron * this.nbPhotonInI * visib / this.nbTel, 2d);

    // photometric flux
    final double fphot = this.nbPhotonInP;
    // noise on photometric flux
    final double sfphot = Math.sqrt(this.nbPhotonInP + this.nbPixPhoto * Math.pow(this.ron, 2d));

    // protect zero divide
    fcorrelsq = Math.max(fcorrelsq, 1e-3d);

    // Uncertainty on square visibility
    double svis2;
    if (this.fracFluxInInterferometry >= 1.0) {
      // no photometry...
      svis2 = Math.pow(visib, 2d) * Math.sqrt(sfcorrelsq / Math.pow(fcorrelsq, 2d));
    } else {
      svis2 = Math.pow(visib, 2d) * Math.sqrt(sfcorrelsq / Math.pow(fcorrelsq, 2d) + 2d * Math.pow(sfphot / fphot, 2d));
    }
    // repeat OBS measurements to reach totalObsTime minutes
    svis2 /= Math.sqrt(this.totalObsTime * this.frameRate);

    // correct for instrumental visibility :
    svis2 /= Math.pow(this.vinst, 2d);

    // instrumentalVisibilityBias is in percents :
    return Math.max(svis2, this.instrumentalVisibilityBias * 0.01d);
  }

  /**
   * Compute error on closure phase
   *
   * @param visAmp12 visibility amplitude of baseline AB = 12
   * @param visAmp23 visibility amplitude of baseline BC = 23
   * @param visAmp13 visibility amplitude of baseline AC = 13
   * @return error on closure phase in degrees
   */
  public double computeT3PhiError(final double visAmp12, final double visAmp23, final double visAmp13) {

    // include instrumental visib
    final double v1 = visAmp12 * this.vinst;
    final double v2 = visAmp23 * this.vinst;
    final double v3 = visAmp13 * this.vinst;

    final double v123 = v1 * v2 * v3;
    final double v12 = v1 * v2;
    final double v13 = v1 * v2;
    final double v23 = v1 * v2;

    // photon noise on closure phase
    final double scpphot = (Math.pow(this.nbTel / this.nbPhotonInI, 3d) * (Math.pow(this.nbTel, 3d) - 2d * v123)
            + Math.pow(this.nbTel / this.nbPhotonInI, 2d) * (Math.pow(this.nbTel, 2d) * (Math.pow(v1, 2d) + Math.pow(v2, 2d) + Math.pow(v3, 2d))
            - (Math.pow(v1, 4d) + Math.pow(v2, 4d) + Math.pow(v3, 4d) + 2 * (Math.pow(v12, 2d) + Math.pow(v13, 2d) + Math.pow(v23, 2d))))
            + (this.nbTel / this.nbPhotonInI) * (this.nbTel * (Math.pow(v12, 2d) + Math.pow(v13, 2d) + Math.pow(v23, 2d))
            - 2 * v123 * (Math.pow(v1, 2d) + Math.pow(v2, 2d) + Math.pow(v3, 2d)))) / (2 * Math.pow(v123, 2d));

    // detector noise on closure phase
    final double scpdet = (Math.pow(this.nbTel / this.nbPhotonInI, 6d) * (Math.pow(this.nbPixInterf, 3d) * Math.pow(this.ron, 6d) + 3 * Math.pow(this.nbPixInterf, 2d) * Math.pow(this.ron, 6d))
            + Math.pow(this.nbTel / this.nbPhotonInI, 4d) * ((Math.pow(v1, 2d) + Math.pow(v2, 2d) + Math.pow(v3, 2d)) * (3 * this.nbPixInterf * Math.pow(this.ron, 4d) + Math.pow(this.nbPixInterf, 2d) * Math.pow(this.ron, 4d)))
            + Math.pow(this.nbTel / this.nbPhotonInI, 2d) * (this.nbPixInterf * Math.pow(ron, 2d) * (Math.pow(v12, 2d) + Math.pow(v13, 2d) + Math.pow(v23, 2d)))) / (2 * Math.pow(v123, 2d));

    // total noise on closure phase
    // per frame
    double sclosph = Math.sqrt(scpphot + scpdet);

    // repeat OBS measurements to reach totalObsTime minutes
    sclosph /= Math.sqrt(this.totalObsTime * this.frameRate);

    // t3PhiErr and t3AmpErr = t3Amp * radians(t3PhiErr) :
    return Math.max(Math.toDegrees(sclosph), this.instrumentalPhaseBias);
  }

  /**
   * Return a random value from a Normal (a.k.a. Gaussian) distribution with the given standard deviation
   *
   * Equivalent to fortran code (kernel/lib/gsys/sysfor.f90)
   *
   * function rangau(sigma)
   * !---------------------------------------------------------------------
   * !       Normal gaussian distribution with R.M.S. equal to sigma
   * !---------------------------------------------------------------------
   *
   * @param sigma standard deviation
   * @return random value
   */
  public final double randomGauss(final double sigma) {
    return sigma * random.nextGaussian();
  }

  /**
   * Photometry band related information
   */
  public enum Band {

    /** U (ultra violet) */
    U("U", 0.334d, 0.066d, -1.4d, 0.3d),
    /** B (Visible) */
    B("B", 0.461875d, 0.08175d, -1.2d, 0.48d),
    /** V (Visible) */
    V("V", 0.556d, 0.1105d, -1.44d, 0.5d),
    /** R (Visible) */
    R("R", 0.6625d, 0.10651d, -1.65d, 0.65d),
    /** I (Near Infrared) */
    I("I", 0.869625d, 0.31176, -1.94d, 0.75d),
    /** J (Near Infrared) */
    J("J", 1.2365d, 0.426d, -2.5d, 0.77d),
    /** H (Near Infrared) */
    H("H", 1.679625d, 0.46425d, -2.94d, 0.84d),
    /** K (Near Infrared) */
    K("K", 2.365625d, 0.912d, -3.4d, 0.93d),
    /** L (Near Infrared) */
    L("L", 3.45875d, 1.2785, -4.15d, 0.972d),
    /** M (Mid Infrared) */
    M("M", 6.4035d, 4.615d, -4.69d, 0.985d),
    /** N (Mid Infrared) */
    N("N", 11.63d, 5.842d, -5.91d, 0.996d),
    /** Q (Mid Infrared) */
    Q("Q", 16.575d, 4.05, -7.17d, 0.999d);

    /**
     * Find the band corresponding to the given wavelength
     * @param waveLength wave length in microns
     * @return corresponding band
     * @throws IllegalArgumentException if no band found
     */
    public static Band findBand(final double waveLength) throws IllegalArgumentException {
      for (Band b : values()) {
        if (Math.abs(waveLength - b.getLambda()) <= b.getBandWidth() / 2d) {
          return b;
        }
      }
      throw new IllegalArgumentException("no band found for the wave length = " + waveLength);
    }

    /**
     * Find the SpectralBand corresponding to the given Band
     * @param band band to use
     * @return SpectralBand or null
     */
    public static SpectralBand findBand(final Band band) {
      switch (band) {
        case V:
          return SpectralBand.V;
        case R:
          return SpectralBand.R;
        case I:
          return SpectralBand.I;
        case J:
          return SpectralBand.J;
        case H:
          return SpectralBand.H;
        case K:
          return SpectralBand.K;
        case N:
          return SpectralBand.N;
        default:
          return null;
      }
    }

    /**
     * Compute the strehl ratio. see le louarn et al (1998, mnras 295, 756), and amb-igr-011 p.5
     * @param magnitude object magnitude
     * @param waveLength wave length in microns
     * @param diameter telescope diameter in meters
     * @param seeing seeing in arc sec
     * @param nbOfActuators
     * @return strehl ratio
     */
    public static double strehl(final double magnitude, final double waveLength,
                                final double diameter, final double seeing, final int nbOfActuators) {

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("magnitude     = " + magnitude);
        logger.fine("waveLength    = " + waveLength);
        logger.fine("diameter      = " + diameter);
        logger.fine("seeing        = " + seeing);
        logger.fine("nbOfActuators = " + nbOfActuators);
      }

      final double lambdaV = 0.55d;

      final Band band = findBand(waveLength);

      final double r0 = 0.251d * lambdaV / seeing * Math.pow(waveLength / lambdaV, 6d / 5d);

      final double doverr0 = diameter / r0;

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("r0            = " + r0);
        logger.fine("doverr0       = " + doverr0);
      }

      final double sigmaphi2_alias = 0.87d * Math.pow((double) nbOfActuators, -5d / 6d) * Math.pow(doverr0, 5d / 3d);

      // ??? (doverr0 * doverr0)**2 = doverr0**4 ou bien erreur ??
      final double sigmaphi2_phot = 1.59e-8d * Math.pow(doverr0, 4d) * Math.pow(waveLength / lambdaV, -2d)
              * (double) nbOfActuators * Math.pow(10d, 0.4d * magnitude);
      final double sigmaphi2_fixe = -Math.log(band.getStrehlMax());
      final double sigmaphi2 = sigmaphi2_alias + sigmaphi2_phot + sigmaphi2_fixe;
      final double strehl = Math.exp(-sigmaphi2) + (1 - Math.exp(-sigmaphi2)) / (1 + doverr0 * doverr0);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("strehl        = " + strehl);
      }

      return strehl;
    }

    /* members */
    /** single char band name (upper case) */
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
