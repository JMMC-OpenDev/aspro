/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.oi.AdaptiveOptics;
import fr.jmmc.aspro.model.oi.AtmosphereQuality;
import fr.jmmc.aspro.model.oi.FocalInstrument;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.FringeTracker;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.model.oi.Telescope;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.util.AtmosphereQualityUtils;
import fr.jmmc.aspro.model.util.SpectralBandUtils;
import fr.jmmc.jmal.Band;
import fr.jmmc.jmal.complex.Complex;
import fr.jmmc.jmal.complex.ImmutableComplex;
import fr.jmmc.jmal.model.VisNoiseService;
import java.text.DecimalFormat;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class performs the noise modelling of visibility data (error and noise) 
 * from the current observation
 *
 * Note : this code is inspired by the Aspro/tasks/lib/noise_lib.f90
 *
 * @author bourgesl
 */
public final class NoiseService extends VisNoiseService {

  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(NoiseService.class.getName());
  /** Planck's constant in standard units (6.6262e-34) */
  public final static double H_PLANCK = 6.62606896e-34d;
  /** Speed of light (2.99792458e8) */
  public final static double C_LIGHT = 2.99792458e8d;

  /* members */
  /** instrument name */
  private String instrumentName = null;

  /* interferometer parameters */
  /** Telescope Diameter (m) */
  private double telDiam = 1d;
  /** Number of telescopes interfering as double */
  private double nbTel = 1;

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
  /** number of pixels to code all fringes together (interferometric channel) */
  private int nbPixInterf = 600;
  /** number of pixels to code each photometric channel */
  private int nbPixPhoto = 4;
  /** fraction of flux going into interferometric channel */
  private double fracFluxInInterferometry = 0.9d;
  /** flag to use the photometry */
  private boolean usePhotometry = false;
  /** instrument band */
  private SpectralBand insBand;

  /* bias */
  /** Typical Visibility Bias (absolute) */
  private double instrumentalVisibilityBias = 0d;
  /** Typical Phase/Phase Closure Bias (rad) */
  private double instrumentalPhaseBias = 0d;
  /** flag to use the Vis2 Calibration Bias */
  private boolean useVis2CalibrationBias = false;
  /** Vis2 Calibration Bias (proportional to square visibility) */
  private double instrumentVis2CalibrationBias = 0d;
  /* instrument mode parameters */
  /** Observation central wavelength (microns) */
  private double lambda = 2.2d;
  /** Spectral Resolution */
  private double spectralResolution = 0.0d;

  /* fringe tracker parameters */
  /** Fringe Tracker is Present */
  private boolean fringeTrackerPresent = false;
  /** Fringe Tracking Mode i.e. not group tracking (FINITO) */
  private boolean fringeTrackingMode = false;
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
  /** number of photons in each photometric channel (photometric flux) */
  private double nbPhotonInP;
  /** total instrumental visibility (with FT if any) */
  private double vinst;

  /* cached intermediate constants */
  /** photometric error contribution on the square visiblity */
  private double errV2Phot;
  /** coefficient used to the squared correlated flux */
  private double sqCorFluxCoef;
  /** coefficient used to compute variance of the squared correlated flux */
  private double varSqCorFluxCoef;
  /** constant used to compute variance of the squared correlated flux */
  private double varSqCorFluxConst;
  /** error correction = 1 / SQRT(total frame) */
  private double totFrameCorrection;
  /** t3 phi error - coefficient */
  private double t3photCoef;
  /** t3 phi error - coefficient^2 */
  private double t3photCoef2;
  /** t3 phi error - coefficient^3 */
  private double t3photCoef3;
  /** t3 phi detector error - constant */
  private double t3detConst;
  /** t3 phi detector error - coefficient 1 */
  private double t3detCoef1;
  /** t3 phi detector error - coefficient 2 */
  private double t3detCoef2;

  /**
   * Protected constructor
   * @param observation observation settings used (read-only copy of the modifiable observation)
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
   * Prepare interferometer and AO parameters (related to telescopes so to each configuration)
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

    if (logger.isDebugEnabled()) {
      logger.debug("nbTel                         : {}", nbTel);
      logger.debug("telDiam                       : {}", telDiam);
      logger.debug("aoBand                        : {}", aoBand);
      logger.debug("nbOfActuators                 : {}", nbOfActuators);
      logger.debug("seeing                        : {}", seeing);
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

    this.instrumentName = instrument.getName();
    this.transmission = instrument.getTransmission();
    this.dit = instrument.getDit();
    this.ron = instrument.getRon();
    this.detectorSaturation = instrument.getDetectorSaturation();
    this.instrumentalVisibility = instrument.getInstrumentVisibility();
    this.instrumentalVisibilityBias = 0.01d * instrument.getInstrumentVisibilityBias(); // percents
    this.instrumentalPhaseBias = Math.toRadians(instrument.getInstrumentPhaseBias()); // radians
    this.nbPixInterf = instrument.getNbPixInterferometry();

    // optional photometry parameters
    this.nbPixPhoto = instrument.getNbPixPhotometry(); // can be 0
    this.fracFluxInInterferometry = instrument.getFracFluxInInterferometry(); // can be 1

    this.usePhotometry = (fracFluxInInterferometry < 1.0d && nbPixPhoto > 0);

    final Double vis2CalibrationBias = instrument.getInstrumentVis2CalibrationBias(); // percents
    if (vis2CalibrationBias == null) {
      this.useVis2CalibrationBias = false;
      this.instrumentVis2CalibrationBias = 0d;
    } else {
      this.useVis2CalibrationBias = true;
      this.instrumentVis2CalibrationBias = 0.01d * vis2CalibrationBias.doubleValue();
    }

    final FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();

    this.lambda = insMode.getWaveLength();
    this.spectralResolution = insMode.getResolution();

    if (logger.isDebugEnabled()) {
      logger.debug("instrumentName                : {}", instrumentName);
      logger.debug("totalObsTime                  : {}", totalObsTime);
      logger.debug("transmission                  : {}", transmission);
      logger.debug("dit                           : {}", dit);
      logger.debug("ron                           : {}", ron);
      logger.debug("detectorSaturation            : {}", detectorSaturation);
      logger.debug("instrumentalVisibility        : {}", instrumentalVisibility);
      logger.debug("instrumentalVisibilityBias    : {}", instrumentalVisibilityBias);
      logger.debug("instrumentalPhaseBias         : {}", instrumentalPhaseBias);
      logger.debug("useVis2CalibrationBias        : {}", useVis2CalibrationBias);
      logger.debug("instrumentVis2CalibrationBias : {}", instrumentVis2CalibrationBias);
      logger.debug("nbPixInterf                   : {}", nbPixInterf);
      logger.debug("nbPixPhoto                    : {}", nbPixPhoto);
      logger.debug("fracFluxInInterferometry      : {}", fracFluxInInterferometry);
      logger.debug("usePhotometry                 : {}", usePhotometry);
      logger.debug("lambda                        : {}", lambda);
      logger.debug("spectralResolution            : {}", spectralResolution);
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
      final FocalInstrument instrument = observation.getInstrumentConfiguration().getInstrumentConfiguration().getFocalInstrument();

      final FringeTracker ft = instrument.getFringeTracker();
      if (ft != null) {
        this.fringeTrackerPresent = true;
        // TODO: handle FT modes properly: GroupTrack is hard coded !
        this.fringeTrackingMode = !"GroupTrack".equalsIgnoreCase(targetConf.getFringeTrackerMode());
        this.fringeTrackerInstrumentalVisibility = ft.getInstrumentVisibility();
        this.fringeTrackerLimit = ft.getMagLimit();
        this.fringeTrackerMaxIntTime = ft.getMaxIntegration();
        this.ftBand = ft.getBand();
      }
    }

    if (logger.isDebugEnabled()) {
      logger.debug("fringeTrackerPresent          : {}", fringeTrackerPresent);
    }
    if (fringeTrackerPresent) {
      if (logger.isDebugEnabled()) {
        logger.debug("fringeTrackingMode            : {}", fringeTrackingMode);
        logger.debug("fringeTrackerInstrumentalVisibility : {}", fringeTrackerInstrumentalVisibility);
        logger.debug("fringeTrackerLimit            : {}", fringeTrackerLimit);
        logger.debug("fringeTrackerMaxIntTime       : {}", fringeTrackerMaxIntTime);
        logger.debug("ftBand                        : {}", ftBand);
      }
    }
  }

  /**
   * Prepare object parameters
   * @param target target to use
   */
  private void prepareTarget(final Target target) {
    Double flux;

    final Band band = Band.findBand(lambda);

    this.insBand = SpectralBandUtils.findBand(band);

    if (logger.isDebugEnabled()) {
      logger.debug("band                          : {}", band);
      logger.debug("insBand                       : {}", insBand);
    }

    // If a flux / magnitude is missing => user message
    // and it is impossible to compute any error

    flux = target.getFlux(insBand);
    this.objectMag = (flux != null) ? flux.doubleValue() : Double.NaN;
    if (logger.isDebugEnabled()) {
      logger.debug("objectMag                     : {}", objectMag);
    }

    if (fringeTrackerPresent) {
      flux = target.getFlux(ftBand);
      this.fringeTrackerMag = (flux != null) ? flux.doubleValue() : Double.NaN;
      if (logger.isDebugEnabled()) {
        logger.debug("fringeTrackerMag              : {}", fringeTrackerMag);
      }
    }

    flux = target.getFlux(aoBand);
    this.adaptiveOpticsMag = (flux != null) ? flux.doubleValue() : Double.NaN;
    if (logger.isDebugEnabled()) {
      logger.debug("adaptiveOpticsMag             : {}", adaptiveOpticsMag);
    }

    if (Double.isNaN(objectMag)
            || (fringeTrackerPresent && Double.isNaN(fringeTrackerMag))
            || Double.isNaN(adaptiveOpticsMag)) {
      // missing magnitude
      this.invalidParameters = true;

      addWarning("Missing photometry on target [" + target.getName() + "] in following bands: "
              + (Double.isNaN(objectMag) ? insBand : "")
              + (fringeTrackerPresent && Double.isNaN(fringeTrackerMag) ? ftBand : "")
              + (Double.isNaN(adaptiveOpticsMag) ? aoBand : ""));
    }
  }

  /**
   * Initialise other parameters
   */
  private void initParameters() {
    // fast return if invalid configuration :
    if (invalidParameters) {
      return;
    }

    this.vinst = instrumentalVisibility;

    final Band band = Band.findBand(lambda);

    // spectral bandwith per resolution element:
    final double deltalambda;
    if (spectralResolution <= 1d) {
      deltalambda = band.getBandWidth();
    } else {
      deltalambda = lambda / spectralResolution;
    }
    // deltalambda = (lambdaMax - lambdaMin) / nPixels[toute la largeur spectrale] 

    // strehl ratio of AO :
    final double sr = Band.strehl(adaptiveOpticsMag, lambda, telDiam, seeing, nbOfActuators);

    if (logger.isDebugEnabled()) {
      logger.debug("band                          : {}", band);
      logger.debug("lambda                        : {}", lambda);
      logger.debug("dlam                          : {}", deltalambda);
      logger.debug("strehl ratio                  : {}", sr);
    }

    // nb of photons m^-2 m^-1 :
    final double fzero = Math.pow(10d, band.getLogFluxZero()) / (H_PLANCK * C_LIGHT / (lambda * AsproConstants.MICRO_METER));

    // nbTotalPhot for the all spectra:
    final double nbTotalPhotSpectra = fzero * nbTel * Math.PI * Math.pow(telDiam / 2d, 2d)
            * transmission * sr * Math.pow(10d, -0.4d * objectMag);
    
    // nbTotalPhot per resolution element:
    final double nbTotalPhotPerS = nbTotalPhotSpectra * (deltalambda * AsproConstants.MICRO_METER);

    if (logger.isDebugEnabled()) {
      logger.debug("fzero                         : {}", fzero);
      logger.debug("nbTotalPhotSpectra            : {}", nbTotalPhotSpectra);
      logger.debug("nbTotalPhotPerS               : {}", nbTotalPhotPerS);
    }

    // dit used by this observation:
    double obsDit = dit;

    // number of expected photoevents in dit :
    double nbTotalPhot = nbTotalPhotPerS * obsDit;

    // fraction of total interferometric flux in peak pixel :
    final double peakflux = fracFluxInInterferometry * nbTotalPhot / nbPixInterf;

    if (logger.isDebugEnabled()) {
      logger.debug("nbTotalPhot                   : {}", nbTotalPhot);
      logger.debug("peakflux                      : {}", peakflux);
    }

    final int nbFrameToSaturation;
    if (detectorSaturation < peakflux) {
      // the dit is too long
      obsDit *= detectorSaturation / peakflux;

      addWarning("DIT too long (saturation). Adjusting it to (possibly impossible): " + formatTime(obsDit));

      nbFrameToSaturation = 1;
    } else {
      nbFrameToSaturation = (int) Math.floor(detectorSaturation / peakflux);
    }

    if (logger.isDebugEnabled()) {
      logger.debug("dit (no saturation)           : {}", obsDit);
      logger.debug("nbFrameToSaturation           : {}", nbFrameToSaturation);
    }

    if (fringeTrackerPresent) {
      if ((fringeTrackerMag <= fringeTrackerLimit) && (nbFrameToSaturation > 1)) {
        // correct instrumental visibility:
        this.vinst *= fringeTrackerInstrumentalVisibility;

        if (logger.isDebugEnabled()) {
          logger.debug("vinst (FT)                    : {}", vinst);
        }

        if (fringeTrackingMode) {
          // FT is asked, can work, and is useful (need to integrate longer)
          obsDit = Math.min(obsDit * nbFrameToSaturation, totalObsTime);
          obsDit = Math.min(obsDit, fringeTrackerMaxIntTime);

          if (logger.isDebugEnabled()) {
            logger.debug("dit (FT)                      : {}", obsDit);
          }

          addWarning("Observation can take advantage of FT. Adjusting DIT to: " + formatTime(obsDit));
        } else {
          addWarning("Observation can take advantage of FT (Group track).");
        }
      } else {
        addWarning("Observation can not use FT (magnitude limit or saturation).");
      }
    }

    // total number of frames:
    final double nbFrames = totalObsTime / obsDit;

    // total frame correction = 1 / SQRT(nFrames):
    this.totFrameCorrection = 1d / Math.sqrt(nbFrames);

    if (logger.isDebugEnabled()) {
      logger.debug("nbFrames                      : {}", nbFrames);
      logger.debug("totFrameCorrection            : {}", totFrameCorrection);
    }

    // corrected total number of photons using final observation dit:
    nbTotalPhot = nbTotalPhotPerS * obsDit;

    if (logger.isDebugEnabled()) {
      logger.debug("nbTotalPhot                   : {}", nbTotalPhot);
    }

    // give back the two useful values for the noise estimate :

    // number of photons in interferometric channel :
    this.nbPhotonInI = nbTotalPhot * fracFluxInInterferometry;
    // number of photons in photometric channel (photometric flux):
    this.nbPhotonInP = nbTotalPhot * (1d - fracFluxInInterferometry) / nbTel;

    if (logger.isDebugEnabled()) {
      logger.debug("nbPhotonInI                   : {}", nbPhotonInI);
      logger.debug("nbPhotonInP                   : {}", nbPhotonInP);
    }

    // Prepare numeric constants for fast error computation:
    prepareVis2Error();
    prepareT3PhiError();
  }

  /**
   * Format time value for warning messages
   * @param value time (s)
   * @return formatted value
   */
  private String formatTime(final double value) {
    final String unit;
    final double val;
    if (value >= 1d) {
      val = value;
      unit = " s";
    } else {
      val = value * 1000d;
      unit = " ms";
    }
    final DecimalFormat df = new DecimalFormat("##0.##");
    return df.format(val) + unit;
  }

  /**
   * Add a warning message in the OIFits file
   * @param msg message to add
   */
  private void addWarning(final String msg) {
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
   * Return true if this service is enabled
   * @return true if this service is enabled 
   */
  @Override
  public boolean isEnabled() {
    return isValid();
  }

  /**
   * Compute error on complex visibility derived from computeVis2Error(visAmp).
   * It returns Complex.NaN if the flux can not be computed
   *
   * @param visAmp visibility amplitude
   * @return complex visiblity error or NaN if the error can not be computed
   */
  public Complex computeVisComplexError(final double visAmp) {
    // complex visibility error:
    final double visErr = computeVisComplexErrorValue(visAmp);

    // fast return Complex.NaN if invalid configuration :
    if (Double.isNaN(visErr)) {
      return Complex.NaN;
    }

    return new ImmutableComplex(visErr, visErr); // immutable complex for safety
  }

  /**
   * Compute error on complex visibility derived from computeVis2Error(visAmp).
   * It returns Double.NaN if the error can not be computed
   *
   * @param visAmp visibility amplitude
   * @return complex visiblity error or NaN if the error can not be computed
   */
  @Override
  public double computeVisComplexErrorValue(final double visAmp) {
    // fast return Double.NaN if invalid configuration :
    if (this.invalidParameters) {
      return Double.NaN;
    }

    // visibility amplitude error :
    final double visAmpErr = computeVisError(visAmp);

    // Distribute the error on RE/IM parts for an uniform error distribution :
    // see These Martin Vannier (2003) p 76

    // sigma2(visRe) = 1/2 ( sigma2(visRe) + sigma2(visIm) = sigma2(vis) / 2

    // complex visibility error : visErrRe = visErrIm = visAmpErr / SQRT(2) :
    return visAmpErr * VIS_AMP_TO_VIS_CPX_ERR;
  }

  /**
   * Compute error on visibility amplitude derived from computeVis2Error(visAmp)
   *
   * @param visAmp visibility amplitude
   * @return visiblity error
   */
  private double computeVisError(final double visAmp) {
    // vis2 error without bias :
    final double errV2 = computeVis2ErrorNoBias(visAmp);

    // dvis = d(vis2) / (2 * vis) :
    final double visAmpErr = errV2 / (2d * visAmp);

    // convert instrumental phase bias as an error too. Use it as a limit:
    // Note: bias are normally not one gaussian distribution (mean = 0) so should not be used to compute gaussian noise !!
    return Math.max(visAmpErr, visAmp * instrumentalPhaseBias);
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

    // correlated flux (include instrumental visibility loss):
    final double fcorrel = nbPhotonInI * visAmp * vinst / nbTel;

    return fcorrel;
  }

  /**
   * Compute error on square visibility. It returns NaN if the error can not be computed
   *
   * @param visAmp visibility amplitude
   * @return square visiblity error or NaN if the error can not be computed
   */
  @Override
  public double computeVis2Error(final double visAmp) {
    // fast return NaN if invalid configuration :
    if (this.invalidParameters) {
      return Double.NaN;
    }

    return computeVis2Error(visAmp, true);
  }

  /**
   * Compute error on square visibility without bias. It returns NaN if the error can not be computed
   *
   * @param visAmp visibility amplitude
   * @return square visiblity error or NaN if the error can not be computed
   */
  public double computeVis2ErrorNoBias(final double visAmp) {
    // fast return NaN if invalid configuration :
    if (this.invalidParameters) {
      return Double.NaN;
    }

    return computeVis2Error(visAmp, false);
  }

  /**
   * Prepare numeric constants for square visiblity error
   * 
   * Note: this method is statefull and NOT thread safe
   */
  private void prepareVis2Error() {

    if (usePhotometry) {
      // variance of the photometric flux in photometric channel:
      final double varFluxPhot = nbPhotonInP + nbPixPhoto * Math.pow(ron, 2d);

      // photometric error contribution on the square visiblity:
      this.errV2Phot = 2d * varFluxPhot / Math.pow(nbPhotonInP, 2d);

      if (logger.isDebugEnabled()) {
        logger.debug("varFluxPhot                   : {}", varFluxPhot);
        logger.debug("errV2Phot                     : {}", errV2Phot);
      }
    } else {
      this.errV2Phot = 0d;
    }

    // squared correlated flux (include instrumental visibility loss) for vis2 = 1.0:
    this.sqCorFluxCoef = Math.pow(nbPhotonInI * vinst / nbTel, 2d);

    // variance of the squared correlated flux = sqCorFlux * coef + constant
    this.varSqCorFluxCoef = 2d * nbPhotonInI + 4d + 2d * nbPixInterf * Math.pow(ron, 2d);

    this.varSqCorFluxConst = nbPhotonInI * (1d + nbPhotonInI)
            + nbPixInterf * (nbPixInterf + 3d) * Math.pow(ron, 4d)
            + 2d * nbPixInterf * Math.pow(ron, 2d) * nbPhotonInI;

    if (logger.isDebugEnabled()) {
      logger.debug("sqCorFluxCoef                 : {}", sqCorFluxCoef);
      logger.debug("varSqCorFluxCoef              : {}", varSqCorFluxCoef);
      logger.debug("varSqCorFluxConst             : {}", varSqCorFluxConst);

      logger.debug("computeVis2Error(1.0)         : {}", computeVis2ErrorNoBias(1d));
      logger.debug("computeVis2Error(0.5)         : {}", computeVis2ErrorNoBias(0.5d));
      logger.debug("computeVis2Error(0.1)         : {}", computeVis2ErrorNoBias(0.1d));
      logger.debug("computeVis2Error(0.05)        : {}", computeVis2ErrorNoBias(0.05d));
      logger.debug("computeVis2Error(0.01)        : {}", computeVis2ErrorNoBias(0.01d));
      logger.debug("computeVis2Error(0.001)       : {}", computeVis2ErrorNoBias(0.001d));
      logger.debug("computeVis2Error(0.0001)      : {}", computeVis2ErrorNoBias(0.0001d));
      logger.debug("computeVis2Error(0.00001)     : {}", computeVis2ErrorNoBias(0.00001d));
      logger.debug("computeVis2Error(0.000001)    : {}", computeVis2ErrorNoBias(0.000001d));
    }
  }

  /**
   * Compute error on square visibility
   *
   * @param visAmp visibility amplitude
   * @param useBias use instrumentalVisibilityBias and instrumentVis2CalibrationBias
   * @return square visiblity error
   */
  private double computeVis2Error(final double visAmp, final boolean useBias) {

    final double vis2 = visAmp * visAmp;

    // squared correlated flux (include instrumental visibility loss):
    double sqCorFlux = sqCorFluxCoef * vis2;
    /*    
    double sqCorFlux = Math.pow(nbPhotonInI * vinst / nbTel, 2d) * vis2;
     */

    // variance of the squared correlated flux:
    // variance = sqCorFlux * coef + constant
    final double varSqCorFlux = sqCorFlux * varSqCorFluxCoef + varSqCorFluxConst;

    /*
    final double varSqCorFlux = sqCorFlux * (2d * nbPhotonInI + 4d + 2d * nbPixInterf * ron * ron)
    + nbPhotonInI * (1d + nbPhotonInI)
    + nbPixInterf * (nbPixInterf + 3d) * Math.pow(ron, 4d)
    + 2d * nbPixInterf * ron * ron * nbPhotonInI;
     */

    // protect zero divide: TODO: why 1e-3d ?
    sqCorFlux = Math.max(sqCorFlux, 1e-20d);

    // Uncertainty on square visibility:
    double errVis2;
    if (usePhotometry) {
      errVis2 = vis2 * Math.sqrt(varSqCorFlux / (sqCorFlux * sqCorFlux) + errV2Phot);
    } else {
      // no photometry...
      errVis2 = vis2 * Math.sqrt(varSqCorFlux / (sqCorFlux * sqCorFlux));
    }
    // repeat OBS measurements to reach totalObsTime minutes:
    errVis2 *= totFrameCorrection;

    if (useBias) {
      // Note: bias are normally not one gaussian distribution (mean = 0) so should not be used to compute gaussian noise !!
      if (useVis2CalibrationBias) {
        // JBLB: bias estimation (first order for PIONIER):
        return Math.max(errVis2, instrumentVis2CalibrationBias * vis2 + instrumentalVisibilityBias * instrumentalVisibilityBias);
      }
      // TODO: find correct coefficients for instruments AMBER, MIDI, VEGA and use instrumentVis2CalibrationBias in  estimation.
//        return Math.max(svis2, Math.pow(instrumentalVisibilityBias, 2d));
      return Math.max(errVis2, instrumentalVisibilityBias);
    }
    return errVis2;
  }

  /**
   * Prepare numeric constants for closure phase error
   * 
   * Note: this method is statefull and NOT thread safe
   */
  private void prepareT3PhiError() {

    // photon noise on closure phase
    this.t3photCoef = nbTel / nbPhotonInI;
    this.t3photCoef2 = Math.pow(nbTel / nbPhotonInI, 2d);
    this.t3photCoef3 = Math.pow(nbTel / nbPhotonInI, 3d);

    // detector noise on closure phase
    this.t3detConst = Math.pow(nbTel / nbPhotonInI, 6d) * (Math.pow(nbPixInterf, 3d) * Math.pow(ron, 6d) + 3 * Math.pow(nbPixInterf, 2d) * Math.pow(ron, 6d));
    this.t3detCoef1 = Math.pow(nbTel / nbPhotonInI, 4d) * (3d * nbPixInterf * Math.pow(ron, 4d) + Math.pow(nbPixInterf, 2d) * Math.pow(ron, 4d));
    this.t3detCoef2 = t3photCoef2 * nbPixInterf * Math.pow(ron, 2d);

    if (logger.isDebugEnabled()) {
      logger.debug("t3photCoef                    : {}", t3photCoef);
      logger.debug("t3detConst                    : {}", t3detConst);
      logger.debug("t3detCoef1                    : {}", t3detCoef1);
      logger.debug("t3detCoef2                    : {}", t3detCoef2);
    }
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
    final double v1 = visAmp12 * vinst;
    final double v2 = visAmp23 * vinst;
    final double v3 = visAmp31 * vinst;

    final double v123 = v1 * v2 * v3;
    final double v12 = v1 * v2;
    final double v13 = v1 * v3;
    final double v23 = v2 * v3;

    final double sv1 = v1 * v1;
    final double sv2 = v2 * v2;
    final double sv3 = v3 * v3;

    final double sv123 = v123 * v123;
    final double sv12 = v12 * v12;
    final double sv13 = v13 * v13;
    final double sv23 = v23 * v23;

    // photon noise on closure phase
    final double scpphot = (t3photCoef3 * (nbTel * nbTel * nbTel - 2d * v123)
            + t3photCoef2 * (nbTel * nbTel * (sv1 + sv2 + sv3) - (sv1 * sv1 + sv2 * sv2 + sv3 * sv3 + 2 * (sv12 + sv13 + sv23)))
            + t3photCoef * (nbTel * (sv12 + sv13 + sv23) - 2d * v123 * (sv1 + sv2 + sv3))) / (2d * sv123);
    /*
    final double scpphot = (Math.pow(nbTel / nbPhotonInI, 3d) * (nbTel * nbTel * nbTel - 2d * v123)
    + Math.pow(nbTel / nbPhotonInI, 2d) * (nbTel * nbTel * (sv1 + sv2 + sv3) - (sv1 * sv1 + sv2 * sv2 + sv3 * sv3 + 2 * (sv12 + sv13 + sv23)))
    + (nbTel / nbPhotonInI) * (nbTel * (sv12 + sv13 + sv23) - 2d * v123 * (sv1 + sv2 + sv3))) / (2d * sv123);
     */

    // detector noise on closure phase
    final double scpdet = (t3detConst + t3detCoef1 * (sv1 + sv2 + sv3) + t3detCoef2 * (sv12 + sv13 + sv23)) / (2d * sv123);
    /*
    double scpdet = (Math.pow(nbTel / nbPhotonInI, 6d) * (Math.pow(nbPixInterf, 3d) * Math.pow(ron, 6d) + 3 * Math.pow(nbPixInterf, 2d) * Math.pow(ron, 6d))
    + Math.pow(nbTel / nbPhotonInI, 4d) * (sv1 + sv2 + sv3) * (3d * nbPixInterf * Math.pow(ron, 4d) + Math.pow(nbPixInterf, 2d) * Math.pow(ron, 4d))
    + Math.pow(nbTel / nbPhotonInI, 2d) * nbPixInterf * Math.pow(ron, 2d) * (sv12 + sv13 + sv23) ) / (2d * sv123);
     */

    // total noise on closure phase per frame:
    double sclosph = Math.sqrt(scpphot + scpdet);

    // repeat OBS measurements to reach totalObsTime minutes
    sclosph *= totFrameCorrection;

    // t3PhiErr and t3AmpErr = t3Amp * t3PhiErr :
    // Note: bias are normally not one gaussian distribution (mean = 0) so should not be used to compute gaussian noise !!
    return Math.max(sclosph, instrumentalPhaseBias);
  }
}
