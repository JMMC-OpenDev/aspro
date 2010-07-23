/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: NoiseService.java,v 1.4 2010-07-23 15:22:36 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
  /** number of photons per pixel in interferometer channel */
  private double nbPhotPerPixelInI;
  /** number of photons per pixel in each photometric channel */
  private double nbPhotPerPixelInP;
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

    final Band band = Band.findBand(this.lambda);

    logger.severe("band                       = " + band);

    this.insBand = Band.findBand(band);

    logger.severe("insBand                    = " + insBand);

    flux = target.getFlux(this.insBand);
    this.objectMag = (flux != null) ? flux.doubleValue() : 0d;
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

    logger.severe("adaptiveOpticsMag          = " + adaptiveOpticsMag);
    logger.severe("strehl ratio               = " + sr);
    logger.severe("dlam                       = " + dlam);
    logger.severe("nbTotalPhot                = " + nbTotalPhot);
    logger.severe("peakflux                   = " + peakflux);

    int nbFrameToSaturation;
    if (this.detectorSaturation < peakflux) {
      // the dit is too long
      this.dit *= this.detectorSaturation / peakflux;

      logger.severe("DIT too long. Adjusting it to (possibly impossible) : " + dit + " s");

      nbFrameToSaturation = 1;
    } else {
      nbFrameToSaturation = (int) Math.round(this.detectorSaturation / peakflux);
    }

    if (this.fringeTrackerPresent && (this.fringeTrackerMag <= this.fringeTrackerLimit) && (nbFrameToSaturation > 1)) {
      // FT is asked, can work, and is useful (need to integrate longer)
      this.dit = Math.min(this.dit * nbFrameToSaturation, this.totalObsTime);
      this.dit = Math.min(this.dit, this.fringeTrackerMaxIntTime);

      logger.severe("Observation can take advantage of FT. Adjusting DIT to : " + dit + " s");
    }

    nbTotalPhot = nbTotalPhotPerS * this.dit;

    logger.severe("nbFrameToSaturation        = " + nbFrameToSaturation);
    logger.severe("nbTotalPhot                = " + nbTotalPhot);

    this.frameRate = 1d / this.dit;

    // give back the two useful values for the noise estimate :

    // number of photons in interferometric channel :
    this.nbPhotPerPixelInI = nbTotalPhot * this.fracFluxInInterferometry;
    // number of photons in photometric channel :
    this.nbPhotPerPixelInP = nbTotalPhot * (1 - this.fracFluxInInterferometry) / this.nbTel;

    logger.severe("nbPhotPerPixelInI          = " + nbPhotPerPixelInI);
    logger.severe("nbPhotPerPixelInP          = " + nbPhotPerPixelInP);
  }

  /**
   * Compute error and noise on complex visibility
   * @param visData complex visibility (0 = Re, 1 = Im)
   * @param visErr complex visibility Error (0 = Re, 1 = Im)
   */
  public void computeVnoise(final float[] visData, final float[] visErr) {
    // TODO :

    /*
    subroutine do_v_noise(v1,v2,errv)
    real, intent(inout) :: v1,v2
    real, intent(out) :: errv
    real t,r,b;
    real, external :: rangau
    include 'gbl_pi.inc'
    !save instrumentalVisibilityBias
    b=instrumentalVisibilityBias
    t=v1**2+v2**2
    ! use no instrumentalVisibilityBias
    instrumentalVisibilityBias=0.0
    call do_v2_noise(t,errv)
    !reset instrumentalVisibilityBias
    instrumentalVisibilityBias=b
    ! if randomizeOutput, t returned is randomized. don't want.
    t=sqrt(v1**2+v2**2)
    errv= errv/(2*t)
    ! convert instrumental phase bias as an error too. Use it as a limit.
    r=t*(instrumentalPhaseBias*pis/180)
    errv=max(errv,r)
    if (randomizeOutput) then
    v1=v1+rangau(errv/2)
    v2=v2+rangau(errv/2)
    endif
    end subroutine do_v_noise
     */
  }

  /**
   * Compute error on square visibility
   * @param vis2 square visibility
   * @return square visiblity error
   */
  public double computeV2noise(final double vis2) {
    return computeV2noise(vis2, false);
  }

  /**
   * Compute error on square visibility
   * @param v square visibility
   * @param useBias use instrumentalVisibilityBias
   * @return square visiblity error
   */
  private double computeV2noise(final double v, final boolean useBias) {

    // include instrumental visib
    double visib = v * this.vinst;

    // squared correlated flux
    double fcorrelsq = Math.pow(this.nbPhotPerPixelInI * visib / this.nbTel, 2d);

    final double sfcorrelsq = 2d * Math.pow(this.nbPhotPerPixelInI, 3d) * Math.pow(visib / this.nbTel, 2d)
            + 4d * Math.pow(this.nbPhotPerPixelInI * visib / this.nbTel, 2d) + Math.pow(this.nbPhotPerPixelInI, 2d) + this.nbPhotPerPixelInI
            + Math.pow(this.nbPixInterf, 2d) * Math.pow(this.ron, 4d) + 3d * this.nbPixInterf * Math.pow(this.ron, 4d)
            + 2d * this.nbPixInterf * this.nbPhotPerPixelInI * Math.pow(this.ron, 2d)
            + 2d * this.nbPixInterf * Math.pow(this.ron * this.nbPhotPerPixelInI * visib / this.nbTel, 2d);

    // SNR on square correlated flux
    //final double snrfcsq = fcorrelsq / Math.sqrt(sfcorrelsq);

    // photometric flux
    final double fphot = this.nbPhotPerPixelInP;
    // noise on photometric flux
    final double sfphot = Math.sqrt(this.nbPhotPerPixelInP + this.nbPixPhoto * Math.pow(ron, 2d));
    // Uncertainty on square visibility
    // protect zero divide
    fcorrelsq = Math.max(fcorrelsq, 1e-3d);

    double svisib;
    if (this.fracFluxInInterferometry >= 1.0) {
      // no photometry...
      svisib = Math.pow(visib, 2d) * Math.sqrt(sfcorrelsq / Math.pow(fcorrelsq, 2d));      // per frame
    } else {
      svisib = Math.pow(visib, 2d) * Math.sqrt(sfcorrelsq / Math.pow(fcorrelsq, 2d) + 2d * Math.pow(sfphot / fphot, 2d));      // per frame
    }
    // repeat OBS measurements to reach totalObsTime minutes
    svisib /= Math.sqrt(this.totalObsTime * this.frameRate);
    // correct for instrumental visibility
    //visib /= this.vinst;

    svisib /= Math.pow(this.vinst, 2d);

    if (useBias) {
      return Math.max(svisib, this.instrumentalVisibilityBias * 0.01);
    } else {
      return svisib;
    }
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

      logger.severe("magnitude     = " + magnitude);
      logger.severe("waveLength    = " + waveLength);
      logger.severe("diameter      = " + diameter);
      logger.severe("seeing        = " + seeing);
      logger.severe("nbOfActuators = " + nbOfActuators);

      final double lambdaV = 0.55d;

      final Band band = findBand(waveLength);

      final double r0 = 0.251d * lambdaV / seeing * Math.pow(waveLength / lambdaV, 6d / 5d);

      final double doverr0 = diameter / r0;

      logger.severe("r0            = " + r0);
      logger.severe("doverr0       = " + doverr0);

      final double sigmaphi2_alias = 0.87d * Math.pow(nbOfActuators, -5d / 6d) * Math.pow(doverr0, 5d / 3d);

      // ??? (doverr0 * doverr0)**2 = doverr0**4 ou bien erreur ??
      final double sigmaphi2_phot = 1.59e-8d * Math.pow(doverr0 * doverr0, 4d) * Math.pow(waveLength / lambdaV, -2d)
              * Math.pow(nbOfActuators, Math.pow(10d, 0.4d * magnitude));
      final double sigmaphi2_fixe = -Math.log(band.getStrehlMax());
      final double sigmaphi2 = sigmaphi2_alias + sigmaphi2_phot + sigmaphi2_fixe;
      final double strehl = Math.exp(-sigmaphi2) + (1 - Math.exp(-sigmaphi2)) / (1 + doverr0 * doverr0);

      logger.severe("strehl        = " + strehl);

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

  /**
   * Return a random value from a Normal (a.k.a. Gaussian) distribution with the given standard deviation
   * @param sigma standard deviation
   * @return random value
   */
  public final double randomGauss(final double sigma) {
    return sigma * random.nextGaussian();
  }
}
