/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: NoiseService.java,v 1.17 2010-10-14 13:12:52 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.16  2010/10/06 16:05:41  bourgesl
 * use band V instead of R till we do not support band R
 *
 * Revision 1.15  2010/10/01 15:39:58  bourgesl
 * formatted DIT value in warnings
 *
 * Revision 1.14  2010/09/02 15:55:55  bourgesl
 * handle missing object magnitudes : always return NaN for errors
 * report warning messages for DIT and missing magnitudes
 *
 * Revision 1.13  2010/08/31 10:42:07  bourgesl
 * fixed bug on T3noise
 *
 * Revision 1.12  2010/08/30 15:55:59  bourgesl
 * removed comment
 *
 * Revision 1.11  2010/08/26 15:27:15  bourgesl
 * restored computeVisError
 *
 * Revision 1.10  2010/08/26 08:53:24  bourgesl
 * minor changes (comments)
 *
 * Revision 1.9  2010/08/25 15:56:15  bourgesl
 * added computeT3PhiError (ported from noise_lib) and correlated flux
 *
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
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.util.AtmosphereQualityUtils;
import fr.jmmc.aspro.model.util.SpectralBandUtils;
import fr.jmmc.mcs.astro.Band;
import java.text.DecimalFormat;
import java.util.List;
import java.util.Random;
import java.util.logging.Level;
import org.apache.commons.math.complex.Complex;

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
  /** constant 1 / SQRT(2) */
  private final static double SQRT_2_INV = 1d / Math.sqrt(2d);
  /** invalid complex (NaN, NaN) */
  private final static Complex INVALID_COMPLEX = new Complex(Double.NaN, Double.NaN);

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
  private double objectMag = Double.NaN;
  /** Magnitude in Fringe Tracker's Band of FT ref star (mag) */
  private double fringeTrackerMag = Double.NaN;
  /** Magnitude in V Band (for AO performances / for strehl) (mag) */
  private double adaptiveOpticsMag = Double.NaN;

  /* internal */
  /** container for warning messages */
  private WarningContainer warningContainer;
  /** flag to indicate that a parameter is invalid in order the code to return errors as NaN values */
  private boolean invalidParameters = false;
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
   * @param warningContainer container for warning messages
   */
  protected NoiseService(final ObservationSetting observation, final Target target,
                         final WarningContainer warningContainer) {
    this.warningContainer = warningContainer;

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

    // All telescopes have the same diameter for a given baseline :
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
    if (this.fringeTrackerPresent) {
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

    this.insBand = SpectralBandUtils.findBand(band);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("band                       = " + band);
      logger.fine("insBand                    = " + insBand);
    }

    // If a flux / magnitude is missing => user message
    // and it is impossible to compute any error

    flux = target.getFlux(this.insBand);
    this.objectMag = (flux != null) ? flux.doubleValue() : Double.NaN;
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("objectMag                  = " + objectMag);
    }

    if (this.fringeTrackerPresent) {
      flux = target.getFlux(this.ftBand);
      this.fringeTrackerMag = (flux != null) ? flux.doubleValue() : Double.NaN;
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("fringeTrackerMag           = " + fringeTrackerMag);
      }
    }

    flux = target.getFlux(this.aoBand);
    this.adaptiveOpticsMag = (flux != null) ? flux.doubleValue() : Double.NaN;
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("adaptiveOpticsMag          = " + adaptiveOpticsMag);
    }

    if (Double.isNaN(this.objectMag)
            || (this.fringeTrackerPresent && Double.isNaN(this.fringeTrackerMag))
            || Double.isNaN(this.adaptiveOpticsMag)) {
      // missing magnitude
      this.invalidParameters = true;

      addWarning("Missing photometry on target [" + target.getName() + "] in following bands : "
              + (Double.isNaN(this.objectMag) ? this.insBand : "")
              + (this.fringeTrackerPresent && Double.isNaN(this.fringeTrackerMag) ? this.ftBand : "")
              + (Double.isNaN(this.adaptiveOpticsMag) ? this.aoBand : ""));
    }
  }

  /**
   * Initialise other parameters
   */
  private void initParameters() {
    // fast return if invalid configuration :
    if (this.invalidParameters) {
      return;
    }

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
      logger.fine("adaptiveOpticsMag          = " + this.adaptiveOpticsMag);
      logger.fine("strehl ratio               = " + sr);
      logger.fine("dlam                       = " + dlam);
      logger.fine("nbTotalPhot                = " + nbTotalPhot);
      logger.fine("peakflux                   = " + peakflux);
    }

    int nbFrameToSaturation;
    if (this.detectorSaturation < peakflux) {
      // the dit is too long
      this.dit *= this.detectorSaturation / peakflux;

      addWarning("DIT too long (saturation). Adjusting it to (possibly impossible) : " + formatDIT(this.dit));

      nbFrameToSaturation = 1;
    } else {
      nbFrameToSaturation = (int) Math.round(this.detectorSaturation / peakflux);
    }

    if (this.fringeTrackerPresent && (this.fringeTrackerMag <= this.fringeTrackerLimit) && (nbFrameToSaturation > 1)) {
      // FT is asked, can work, and is useful (need to integrate longer)
      this.dit = Math.min(this.dit * nbFrameToSaturation, this.totalObsTime);
      this.dit = Math.min(this.dit, this.fringeTrackerMaxIntTime);

      addWarning("Observation can take advantage of FT. Adjusting DIT to : " + formatDIT(this.dit));
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
   * Format DIT value for warning messages
   * @param dit detector integration time (s)
   * @return DIT value
   */
  private String formatDIT(final double dit) {
    final String unit;
    final double val;
    if (dit >= 1d) {
      val = dit;
      unit = " s";
    } else {
      val = dit * 1000d;
      unit = " ms";
    }
    final DecimalFormat df = new DecimalFormat("##0.##");
    return df.format(val) + unit;
  }

  /**
   * Add a warning message in the OIFits file
   * @param msg message to add
   */
  protected final void addWarning(final String msg) {
    if (logger.isLoggable(Level.INFO)) {
      logger.info(msg);
    }

    this.warningContainer.addWarningMessage(msg);
  }

  /**
   * Return true if all parameters are valid i.e. returned errors are valid
   * @return true if all parameters are valid
   */
  public boolean isValid() {
    return !this.invalidParameters;
  }

  /**
   * Compute error on complex visibility derived from computeVis2Error(visAmp).
   * It returns NaN if the flux can not be computed
   *
   * @param visAmp visibility amplitude
   * @return complex visiblity error or NaN if the error can not be computed
   */
  public Complex computeVisComplexError(final double visAmp) {
    // fast return NaN if invalid configuration :
    if (this.invalidParameters) {
      return INVALID_COMPLEX;
    }

    // visibility amplitude error :
    final double visAmpErr = computeVisError(visAmp);

    // Distribute the error on RE/IM parts for an uniform error distribution :
    // see These Martin Vannier (2003) p 76

    // sigma2(visRe) = 1/2 ( sigma2(visRe) + sigma2(visIm) = sigma2(vis) / 2

    // complex visibility error : visErrRe = visErrIm = visAmpErr / SQRT(2) :
    final double visErr = visAmpErr * SQRT_2_INV;

    return new Complex(visErr, visErr);
  }

  /**
   * Compute error on visibility amplitude derived from computeVis2Error(visAmp)
   *
   * @param visAmp visibility amplitude
   * @return visiblity error
   */
  private double computeVisError(final double visAmp) {

    // vis2 error without bias :
    final double errV2 = computeVis2Error(visAmp, false);

    // dvis = d(vis2) / (2 * vis) :
    final double visAmpErr = errV2 / (2d * visAmp);

    // convert instrumental phase bias as an error too. Use it as a limit.
    return Math.max(visAmpErr, visAmp * Math.toRadians(this.instrumentalPhaseBias));
  }

  /**
   * Return the correlated flux of the object. It returns NaN if the flux can not be computed
   * @param visAmp visibility amplitude
   * @return correlated flux or NaN if the flux can not be computed
   */
  public double computeCorrelatedFlux(final double visAmp) {
    // fast return NaN if invalid configuration :
    if (this.invalidParameters) {
      return Double.NaN;
    }

    // include instrumental visib
    final double visib = visAmp * this.vinst;

    // squared correlated flux
    final double fcorrelsq = this.nbPhotonInI * visib / this.nbTel;

    return fcorrelsq;
  }

  /**
   * Compute error on square visibility. It returns NaN if the error can not be computed
   *
   * @param visAmp visibility amplitude
   * @return square visiblity error or NaN if the error can not be computed
   */
  public double computeVis2Error(final double visAmp) {
    // fast return NaN if invalid configuration :
    if (this.invalidParameters) {
      return Double.NaN;
    }

    return computeVis2Error(visAmp, true);
  }

  /**
   * Compute error on square visibility
   *
   * @param visAmp visibility amplitude
   * @param useBias use instrumentalVisibilityBias
   * @return square visiblity error
   */
  private double computeVis2Error(final double visAmp, final boolean useBias) {

    // include instrumental visib
    final double visib = visAmp * this.vinst;

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

    if (useBias) {
      // instrumentalVisibilityBias is in percents :
      return Math.max(svis2, this.instrumentalVisibilityBias * 0.01d);
    }
    return svis2;
  }

  /**
   * Compute error on closure phase. It returns NaN if the error can not be computed
   *
   * @param visAmp12 visibility amplitude of baseline AB = 12
   * @param visAmp23 visibility amplitude of baseline BC = 23
   * @param visAmp31 visibility amplitude of baseline CA = 31
   * @return error on closure phase in radians or NaN if the error can not be computed
   */
  public double computeT3PhiError(final double visAmp12, final double visAmp23, final double visAmp31) {
    // fast return NaN if invalid configuration :
    if (this.invalidParameters) {
      return Double.NaN;
    }

    // include instrumental visib
    final double v1 = visAmp12 * this.vinst;
    final double v2 = visAmp23 * this.vinst;
    final double v3 = visAmp31 * this.vinst;

    final double v123 = v1 * v2 * v3;
    final double v12 = v1 * v2;
    final double v13 = v1 * v3;
    final double v23 = v2 * v3;

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

    // t3PhiErr and t3AmpErr = t3Amp * t3PhiErr :
    return Math.max(sclosph, Math.toRadians(this.instrumentalPhaseBias));
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
}
