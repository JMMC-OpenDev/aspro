/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.oi.AdaptiveOptics;
import fr.jmmc.aspro.model.oi.AtmosphereQuality;
import fr.jmmc.aspro.model.oi.FocalInstrument;
import fr.jmmc.aspro.model.oi.FringeTracker;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.model.oi.Telescope;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.observability.TargetPointInfo;
import fr.jmmc.aspro.model.util.AtmosphereQualityUtils;
import fr.jmmc.aspro.model.util.SpectralBandUtils;
import fr.jmmc.jmal.Band;
import fr.jmmc.jmal.complex.Complex;
import fr.jmmc.jmal.complex.ImmutableComplex;
import fr.jmmc.jmal.model.VisNoiseService;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.List;
import net.jafama.FastMath;
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
    /** enable bound checks on iPoint / iChannel */
    private final static boolean DO_CHECKS = false;

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
    /* fixed values (spectrally independent) */
    /** Total Acquisition time per observed u,v point (s) */
    private double totalObsTime = 300d;
    /** Detector individual integration time (s) */
    private double dit = 0.01d;
    /** Detector readout noise */
    private double ron = 12d;
    /** Detector is non-linear above (to avoid saturation/non-linearity) */
    private double detectorSaturation = 100000d;
    /** fraction of flux going into interferometric channel */
    private double fracFluxInInterferometry = 0.9d;
    /** flag to use the photometry */
    private boolean usePhotometry = false;
    /* varying values (spectrally dependent) */
    /** instrument band */
    private Band insBand;
    /** instrument band corresponding to target mags */
    private SpectralBand insTargetBand;

    /* TODO: phase 2: get such values for MATISSE from each spectral modes */
    /** (W) Transmission of interferometer+instrument at observed wavelength */
    private double transmission = 1d;
    /** (W?) Instrumental Visibility [0.0-1.0] */
    private double instrumentalVisibility = 1d;
    /** number of pixels to code all fringes together (interferometric channel) */
    /* TODO: more detailled explanations */
    private int nbPixInterf = 600;
    /** number of pixels to code each photometric channel */
    /* TODO: more detailled explanations */
    private int nbPixPhoto = 4;

    /* bias */
    /** true to use instrument bias; false to compute only theoretical error */
    private final boolean useInstrumentBias;
    /** Typical Vis2 Bias (absolute) */
    private double instrumentalVis2Bias = 0d;
    /** Typical Phase/Phase Closure Bias (rad) */
    private double instrumentalPhaseBias = 0d;
    /** flag to use the Vis2 Calibration Bias */
    private boolean useVis2CalibrationBias = false;
    /** Vis2 Calibration Bias (proportional to square visibility) */
    private double instrumentVis2CalibrationBias = 0d;

    /* instrument mode parameters */
    /** number of spectral channels */
    private final int nSpectralChannels;
    /** index of central spectral channel */
    private final int iMidChannel;
    /** spectral channel central wavelength */
    private final double[] waveLengths;
    /** spectral channel widths */
    private final double[] waveBands;

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
    /* TODO: use band range (previous / next) to interpolate flux (SED like) ? */
    private double objectMag = Double.NaN;
    /** Magnitude in Fringe Tracker's Band of FT ref star (mag) */
    private double fringeTrackerMag = Double.NaN;
    /** Magnitude in V Band (for AO performances / for strehl) (mag) */
    private double adaptiveOpticsMag = Double.NaN;
    /** target information for each uv point couples */
    private final TargetPointInfo[] targetPointInfos;
    /** number of uv points */
    private final int nPoints;
    /** index of central uv point */
    private final int iMidPoint;

    /* internal */
    /** container for warning messages */
    private final WarningContainer warningContainer;
    /** flag to indicate that a parameter is invalid in order the code to return errors as NaN values */
    private boolean invalidParameters = false;

    /** (W?) total instrumental visibility (with FT if any) */
    private double vinst;

    /* cached intermediate constants */
    /** error correction = 1 / SQRT(total frame) */
    private double totFrameCorrection;
    /** noise computation parameters per uv point */
    private NoiseWParams[] params = null;

    /**
     * Protected constructor
     * @param observation observation settings used (read-only copy of the modifiable observation)
     * @param target target to use
     * @param targetPointInfos target information for each uv point couples
     * @param useInstrumentBias true to use instrument bias; false to compute only theoretical error
     * @param warningContainer container for warning messages
     * @param waveLengths concrete wavelength values (spectral channel central value) in meters
     * @param waveBands concrete spectral channel bandwidths in meters
     */
    protected NoiseService(final ObservationSetting observation,
                           final Target target,
                           final TargetPointInfo[] targetPointInfos,
                           final boolean useInstrumentBias,
                           final WarningContainer warningContainer,
                           final double[] waveLengths,
                           final double[] waveBands) {

        this.useInstrumentBias = useInstrumentBias;
        this.warningContainer = warningContainer;

        // Get spectral channels:
        this.nSpectralChannels = waveLengths.length;
        this.iMidChannel = this.nSpectralChannels / 2;
        this.waveLengths = waveLengths;
        this.waveBands = waveBands;

        this.targetPointInfos = targetPointInfos;
        this.nPoints = this.targetPointInfos.length;
        this.iMidPoint = this.nPoints / 2;

        if (logger.isDebugEnabled()) {
            logger.debug("spectralChannels              : {}", nSpectralChannels);
            logger.debug("nMidChannel                   : {}", iMidChannel);
            logger.debug("waveLengths                   : {}", Arrays.toString(waveLengths));
            logger.debug("waveBands                     : {}", Arrays.toString(waveBands));
        }

        // extract parameters in observation and configuration :
        prepareInterferometer(observation);
        prepareInstrument(observation);
        prepareFringeTracker(observation, target);
        prepareTarget(target);
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
        } else {
            // by default: compute strehl ratio on V band with only 1 actuator ?
            this.aoBand = SpectralBand.V;
            this.nbOfActuators = 1;
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

        // use alias or real instrument name:
        this.instrumentName = instrument.getAliasOrName();
        this.transmission = instrument.getTransmission();
        this.dit = instrument.getDit();
        this.ron = instrument.getRon();
        this.detectorSaturation = instrument.getDetectorSaturation();
        this.instrumentalVisibility = instrument.getInstrumentVisibility();
        this.nbPixInterf = instrument.getNbPixInterferometry();

        // optional photometry parameters
        this.nbPixPhoto = instrument.getNbPixPhotometry(); // can be 0
        this.fracFluxInInterferometry = instrument.getFracFluxInInterferometry(); // can be 1

        final String instrumentMode = observation.getInstrumentConfiguration().getInstrumentMode();
        if (instrumentMode.startsWith("HIGH_SENS_")) {
            // MATISSE hack:
            this.fracFluxInInterferometry = 1.0;
        }

        this.usePhotometry = (fracFluxInInterferometry < 1.0 && nbPixPhoto > 0);

        if (this.useInstrumentBias) {
            /* Convert Phase bias to radians */
            this.instrumentalPhaseBias = FastMath.toRadians(instrument.getInstrumentPhaseBias());

            /* Get Vis bias (percents) */
            final double instrumentalVisBias = getPercents(instrument.getInstrumentVisibilityBias());

            final Double vis2CalibrationBias = instrument.getInstrumentVis2CalibrationBias();
            if (vis2CalibrationBias == null) {
                this.useVis2CalibrationBias = false;
                this.instrumentVis2CalibrationBias = 0d;

                /* note: instrumentalVis2Bias = instrumentalVisBias (few percents) as there is no specific calibration bias */
                /* Correct Vis2 bias = 1/4 * visBias (peak to peak ~ 4 sigma) */
                this.instrumentalVis2Bias = 0.25 * instrumentalVisBias;
            } else {
                this.useVis2CalibrationBias = true;

                /* Get Vis2 calibration bias (percents) (peak to peak ~ 4 sigma) */
                this.instrumentVis2CalibrationBias = 0.25 * getPercents(vis2CalibrationBias.doubleValue());

                /* Correct Vis2 bias = 1/4 * visBias^2 (peak to peak ~ 4 sigma) */
                this.instrumentalVis2Bias = 0.25 * instrumentalVisBias * instrumentalVisBias;
            }
        } else {
            // disable instrumental bias:
            this.useVis2CalibrationBias = false;
        }

        if (logger.isDebugEnabled()) {
            logger.debug("instrumentName                : {}", instrumentName);
            logger.debug("totalObsTime                  : {}", totalObsTime);
            logger.debug("transmission                  : {}", transmission);
            logger.debug("dit                           : {}", dit);
            logger.debug("ron                           : {}", ron);
            logger.debug("detectorSaturation            : {}", detectorSaturation);
            logger.debug("instrumentalVisibility        : {}", instrumentalVisibility);
            logger.debug("useInstrumentBias             : {}", useInstrumentBias);
            logger.debug("instrumentalVisibilityBias    : {}", instrumentalVis2Bias);
            logger.debug("instrumentalPhaseBias         : {}", instrumentalPhaseBias);
            logger.debug("useVis2CalibrationBias        : {}", useVis2CalibrationBias);
            logger.debug("instrumentVis2CalibrationBias : {}", instrumentVis2CalibrationBias);
            logger.debug("nbPixInterf                   : {}", nbPixInterf);
            logger.debug("nbPixPhoto                    : {}", nbPixPhoto);
            logger.debug("fracFluxInInterferometry      : {}", fracFluxInInterferometry);
            logger.debug("usePhotometry                 : {}", usePhotometry);
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

        // Get band from wavelength range:
        final double lambdaMin = waveLengths[0];
        final double lambdaMax = waveLengths[nSpectralChannels - 1];
        final double lambdaMid = 0.5 * (lambdaMin + lambdaMax);

        // TODO: use band range to cover lambdaMin / lambdaMax:
        
        // JHK or LM or BVR
        final Band band = findBand(lambdaMid / AsproConstants.MICRO_METER); // microns
        this.insBand = band;
        this.insTargetBand = SpectralBandUtils.findBand(band);

        if (logger.isDebugEnabled()) {
            logger.debug("lambdaMin                     : {}", lambdaMin);
            logger.debug("lambda                        : {}", lambdaMid);
            logger.debug("lambdaMax                     : {}", lambdaMax);
            logger.debug("insBand                       : {}", insBand);
            logger.debug("insTargetBand                 : {}", insTargetBand);
        }

        // If a flux / magnitude is missing => user message
        // and it is impossible to compute any error
        
        // TODO: handle B|R vs V for VEGA:
        flux = target.getFlux(insTargetBand);
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
                    + (Double.isNaN(objectMag) ? insTargetBand : "")
                    + (fringeTrackerPresent && Double.isNaN(fringeTrackerMag) ? ftBand : "")
                    + (Double.isNaN(adaptiveOpticsMag) ? aoBand : ""));
        }
    }

    /**
     * Initialise other parameters
     */
    void initParameters() {
        // fast return if invalid configuration :
        if (invalidParameters) {
            return;
        }

        // Target flux per spectral channel:
        final double[] fluxSrcPerChannel = computeTargetFlux();

        // strehl ratio of AO :
        // TODO: add parameter use_strehl or not (transmission may already include it) 
        final boolean use_strehl = !(instrumentName.startsWith(AsproConstants.INS_VEGA));

        final double telSurface = Math.PI * FastMath.pow2(0.5 * telDiam);

        final int nWLen = nSpectralChannels;

        final int nObs = nPoints; // = targetPointInfos.length

        // Pass 1: find maximum flux per channel taking in to account the target elevation:
        // strehl is spectrally dependent:
        final double[][] strehlPerChannel;

        if (use_strehl) {
            strehlPerChannel = new double[nObs][];

            for (int n = 0; n < nObs; n++) {
                double elevation = targetPointInfos[n].getElevation();

                strehlPerChannel[n] = Band.strehl(adaptiveOpticsMag, waveLengths, telDiam, seeing, nbOfActuators, elevation);

                if (logger.isDebugEnabled()) {
                    logger.debug("elevation                     : {}", elevation);
                    logger.debug("strehl ratio                  : {}", Arrays.toString(strehlPerChannel[n]));
                }
            }
        } else {
            strehlPerChannel = null;
        }

        // total number of photons per channel and per second:
        final double[][] nbTotalPhotPerSec = new double[nObs][nWLen];

        // maximum number of photons per channel and per second:
        double maxNbTotalPhotPerSec = 0.0;

        for (int n = 0; n < nObs; n++) {

            for (int i = 0; i < nWLen; i++) {
                final double fluxPerChannel = fluxSrcPerChannel[i] /* + thermal photon number */;

                /* TODO: make transmission depend on wavelengths */
                double nbTotalPhotPerSecNoStrehl = telSurface * nbTel * transmission * fluxPerChannel;

                nbTotalPhotPerSec[n][i] = nbTotalPhotPerSecNoStrehl;

                if (use_strehl) {
                    nbTotalPhotPerSec[n][i] *= strehlPerChannel[n][i];
                }

                if (nbTotalPhotPerSec[n][i] > maxNbTotalPhotPerSec) {
                    maxNbTotalPhotPerSec = nbTotalPhotPerSec[n][i];
                }
            }
            if (logger.isDebugEnabled()) {
                logger.debug("elevation                     : {}", targetPointInfos[n].getElevation());
                logger.debug("nbTotalPhotPerSec             : {}", Arrays.toString(nbTotalPhotPerSec[n]));
            }
        }

        if (logger.isDebugEnabled()) {
            logger.debug("maxNbTotalPhotPerSec          : {}", maxNbTotalPhotPerSec);
        }

        // dit used by this observation:
        double obsDit = dit;

        // maximum number of expected photoevents in dit:
        final double maxTotalPhot = maxNbTotalPhotPerSec * obsDit;

        // fraction of total interferometric flux in peak pixel :
        final double peakflux = fracFluxInInterferometry * maxTotalPhot / nbPixInterf;

        if (logger.isDebugEnabled()) {
            logger.debug("maxTotalPhot                  : {}", maxTotalPhot);
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

        // Adjust instrumental visibility:
        this.vinst = instrumentalVisibility;

        if (fringeTrackerPresent) {
            if ((fringeTrackerMag <= fringeTrackerLimit) && (nbFrameToSaturation > 1)) {
                // correct instrumental visibility:
                // TODO: may be depend on the spectral band (H != K != N) ?
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

                    addInformation("Observation can take advantage of FT. Adjusting DIT to: " + formatTime(obsDit));
                } else {
                    addInformation("Observation can take advantage of FT (Group track).");
                }
            } else {
                addWarning("Observation can not use FT (magnitude limit or saturation).");
            }
        }

        // TODO: handle dead time = dit for MATISSE
        // total number of frames:
        final double nbFrames = totalObsTime / obsDit;

        // total frame correction = 1 / SQRT(nFrames):
        this.totFrameCorrection = 1.0 / Math.sqrt(nbFrames);

        if (logger.isDebugEnabled()) {
            logger.debug("nbFrames                      : {}", nbFrames);
            logger.debug("totFrameCorrection            : {}", totFrameCorrection);
        }

        // 2nd pass: obsDit is known = integration time (setup)
        this.params = new NoiseWParams[nObs];

        // number of expected photoevents in dit:
        final double[] nbTotalPhot = new double[nWLen];

        for (int n = 0; n < nObs; n++) {

            // corrected total number of photons using final observation dit:
            for (int i = 0; i < nWLen; i++) {
                nbTotalPhot[i] = nbTotalPhotPerSec[n][i] * obsDit;
            }

            if (logger.isDebugEnabled()) {
                logger.debug("nbTotalPhot                   : {}", Arrays.toString(nbTotalPhot));
            }

            // give back the two useful values for the noise estimate :
            final NoiseWParams param = new NoiseWParams(nWLen);
            this.params[n] = param;

            final double[] nbPhotonInI = param.nbPhotonInI;
            final double[] nbPhotonInP = param.nbPhotonInP;

            for (int i = 0; i < nWLen; i++) {
                // TODO: divide by nbTel to be consistent and avoid repeated expression [nbPhotonInI / nbTel] ?
                // consistency issue ?
                // number of photons in interferometric channel :
                nbPhotonInI[i] = nbTotalPhot[i] * fracFluxInInterferometry;
                // number of photons in photometric channel (photometric flux):
                nbPhotonInP[i] = nbTotalPhot[i] / nbTel * ((1.0 - fracFluxInInterferometry));
            }

            if (logger.isDebugEnabled()) {
                logger.debug("elevation                     : {}", targetPointInfos[n].getElevation());
                logger.debug("nbPhotonInI                   : {}", Arrays.toString(nbPhotonInI));
                logger.debug("nbPhotonInP                   : {}", Arrays.toString(nbPhotonInP));
            }

            // Prepare numeric constants for fast error computation:
            prepareVis2Error(n);
            prepareT3PhiError(n);
        }
        
        if (logger.isDebugEnabled()) {
            dumpVis2Error(0);
            dumpVis2Error(iMidChannel);
            dumpVis2Error(nSpectralChannels - 1);
        }
    }
    
    private void dumpVis2Error(final int iChannel) {
        logger.debug("channel                       : {}", iChannel);
        logger.debug("computeVis2Error(1.0)         : {}", computeVis2ErrorNoBias(iChannel, 1d));
        logger.debug("computeVis2Error(0.5)         : {}", computeVis2ErrorNoBias(iChannel, 0.5d));
        logger.debug("computeVis2Error(0.1)         : {}", computeVis2ErrorNoBias(iChannel, 0.1d));
        logger.debug("computeVis2Error(0.05)        : {}", computeVis2ErrorNoBias(iChannel, 0.05d));
        logger.debug("computeVis2Error(0.01)        : {}", computeVis2ErrorNoBias(iChannel, 0.01d));
        logger.debug("computeVis2Error(0.001)       : {}", computeVis2ErrorNoBias(iChannel, 0.001d));
        logger.debug("computeVis2Error(0.0001)      : {}", computeVis2ErrorNoBias(iChannel, 0.0001d));
    }

    double[] computeTargetFlux() {
        // nb of photons m^-2.s^-1.m^-1 for an object at magnitude 0:
        // note: fzero depends on the spectral band:
        final double fzero = FastMath.pow(10d, insBand.getLogFluxZero()) * (insBand.getLambda() * AsproConstants.MICRO_METER) / (H_PLANCK * C_LIGHT);

        // TODO: source flux may be spectrally dependent:
        // nb of photons m^-2.s^-1.m^-1 for the target object:
        final double fsrc = fzero * FastMath.pow(10d, -0.4d * objectMag);

        if (logger.isDebugEnabled()) {
            logger.debug("fzero                         : {}", fzero);
            logger.debug("fsrc                          : {}", fsrc);
        }

        final int nWLen = nSpectralChannels;
        // nb photons per surface and per second:
        final double[] fluxSrcPerChannel = new double[nWLen];

        /* TODO: use band range (previous / next) to interpolate flux or SED based on spectral type */
        for (int i = 0; i < nWLen; i++) {
            // nb of photons m^-2.s^-1 for the target object:
            fluxSrcPerChannel[i] = fsrc * waveBands[i];
        }
        if (logger.isDebugEnabled()) {
            logger.debug("fluxSrcPerChannel             : {}", Arrays.toString(fluxSrcPerChannel));
        }
        return fluxSrcPerChannel;
    }

    /**
     * Prepare numeric constants for square visiblity error
     *
     * Note: this method is statefull and NOT thread safe
     *
     * @param iPoint index of the observable point
     */
    private void prepareVis2Error(final int iPoint) {
        final int nWLen = nSpectralChannels;

        final NoiseWParams param = this.params[iPoint];

        final double[] nbPhotonInI = param.nbPhotonInI;
        final double[] nbPhotonInP = param.nbPhotonInP;

        final double[] errV2Phot = param.errV2Phot;
        final double[] sqCorFluxCoef = param.sqCorFluxCoef;
        final double[] varSqCorFluxCoef = param.varSqCorFluxCoef;
        final double[] varSqCorFluxConst = param.varSqCorFluxConst;

        if (usePhotometry) {
            for (int i = 0; i < nWLen; i++) {
                // variance of the photometric flux in photometric channel:
                final double varFluxPhot = nbPhotonInP[i] + nbPixPhoto * FastMath.pow2(ron);

                // photometric error contribution on the square visiblity:
                errV2Phot[i] = 2.0 * varFluxPhot / FastMath.pow2(nbPhotonInP[i]);
            }

            if (logger.isDebugEnabled()) {
                logger.debug("errV2Phot                     : {}", Arrays.toString(errV2Phot));
            }
        } else {
            Arrays.fill(errV2Phot, 0.0);
        }

        for (int i = 0; i < nWLen; i++) {
            // squared correlated flux (include instrumental visibility loss) for vis2 = 1.0:
            sqCorFluxCoef[i] = FastMath.pow2(nbPhotonInI[i] / nbTel * vinst);

            // variance of the squared correlated flux = sqCorFlux * coef + constant
            varSqCorFluxCoef[i] = 2.0 * nbPhotonInI[i] + 4.0 + 2.0 * nbPixInterf * FastMath.pow2(ron);

            varSqCorFluxConst[i] = nbPhotonInI[i] * (1.0 + nbPhotonInI[i] + 2.0 * nbPixInterf * FastMath.pow2(ron))
                    + nbPixInterf * (nbPixInterf + 3.0) * FastMath.pow(ron, 4.0);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("elevation                     : {}", targetPointInfos[iPoint].getElevation());
            logger.debug("sqCorFluxCoef                 : {}", Arrays.toString(sqCorFluxCoef));
            logger.debug("varSqCorFluxCoef              : {}", Arrays.toString(varSqCorFluxCoef));
            logger.debug("varSqCorFluxConst             : {}", Arrays.toString(varSqCorFluxConst));
        }
    }

    /**
     * Return the correlated flux of the object. It returns NaN if the flux can not be computed
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @param visAmp visibility amplitude
     * @return correlated flux or NaN if the flux can not be computed
     */
    public double computeCorrelatedFlux(final int iPoint, final int iChannel, final double visAmp) {
        // fast return NaN if invalid configuration :
        if (this.invalidParameters) {
            return Double.NaN;
        }
        if (DO_CHECKS && check(iPoint, iChannel)) {
            return Double.NaN;
        }

        final NoiseWParams param = this.params[iPoint];
        final double[] nbPhotonInI = param.nbPhotonInI;

        // correlated flux (include instrumental visibility loss):
        final double fcorrel = (nbPhotonInI[iChannel] / nbTel * vinst) * visAmp;

        return fcorrel;
    }

    /**
     * Compute error on square visibility
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @param visAmp visibility amplitude
     * @param useBias use instrumentalVisibilityBias and instrumentVis2CalibrationBias
     * @return square visiblity error
     */
    private double computeVis2Error(final int iPoint, final int iChannel,
                                    final double visAmp, final boolean useBias) {
        // fast return NaN if invalid configuration :
        if (this.invalidParameters) {
            return Double.NaN;
        }
        if (DO_CHECKS && check(iPoint, iChannel)) {
            return Double.NaN;
        }

        final NoiseWParams param = this.params[iPoint];
        final double[] errV2Phot = param.errV2Phot;
        final double[] sqCorFluxCoef = param.sqCorFluxCoef;
        final double[] varSqCorFluxCoef = param.varSqCorFluxCoef;
        final double[] varSqCorFluxConst = param.varSqCorFluxConst;

        final double vis2 = visAmp * visAmp;

        // squared correlated flux (include instrumental visibility loss):
        double sqCorFlux = sqCorFluxCoef[iChannel] * vis2;
        /*
         double sqCorFlux = FastMath.pow2(nbPhotonInI * vinst / nbTel) * vis2;
         */

        // variance of the squared correlated flux:
        // variance = sqCorFlux * coef + constant
        final double varSqCorFlux = sqCorFlux * varSqCorFluxCoef[iChannel] + varSqCorFluxConst[iChannel];

        /*
         final double varSqCorFlux = sqCorFlux * (2d * nbPhotonInI + 4d + 2d * nbPixInterf * ron * ron)
         + nbPhotonInI * (1d + nbPhotonInI + 2d * nbPixInterf * ron * ron)
         + nbPixInterf * (nbPixInterf + 3d) * FastMath.pow(ron, 4d);
         */
        // protect zero divide: TODO: why 1e-3d ?
        sqCorFlux = Math.max(sqCorFlux, 1e-20d);

        // Uncertainty on square visibility:
        double errVis2;
        if (usePhotometry) {
            errVis2 = vis2 * Math.sqrt(varSqCorFlux / (sqCorFlux * sqCorFlux) + errV2Phot[iChannel]);
        } else {
            // no photometry...
            errVis2 = vis2 * Math.sqrt(varSqCorFlux / (sqCorFlux * sqCorFlux));
        }
        // repeat OBS measurements to reach totalObsTime minutes:
        errVis2 *= totFrameCorrection;

        if (useBias) {
            // Note: bias are normally not a gaussian distribution (mean = 0) so should not be used to compute gaussian noise !!
            if (useVis2CalibrationBias) {
                // JBLB: bias estimation (first order for PIONIER):
                return Math.max(errVis2, instrumentVis2CalibrationBias * vis2 + instrumentalVis2Bias);
            }
            // TODO: find correct coefficients for instruments AMBER, MIDI, VEGA and use instrumentVis2CalibrationBias in estimation.
            return Math.max(errVis2, instrumentalVis2Bias);
        }
        return errVis2;
    }

    /**
     * Prepare numeric constants for closure phase error
     *
     * Note: this method is statefull and NOT thread safe
     *
     * @param iPoint index of the observable point
     */
    private void prepareT3PhiError(final int iPoint) {
        final int nWLen = nSpectralChannels;

        final NoiseWParams param = this.params[iPoint];

        final double[] nbPhotonInI = param.nbPhotonInI;

        final double[] t3photCoef = param.t3photCoef;
        final double[] t3photCoef2 = param.t3photCoef2;
        final double[] t3photCoef3 = param.t3photCoef3;

        final double[] t3detConst = param.t3detConst;
        final double[] t3detCoef1 = param.t3detCoef1;
        final double[] t3detCoef2 = param.t3detCoef2;

        for (int i = 0; i < nWLen; i++) {
            final double invNbPhotonInIPerTel = nbTel / nbPhotonInI[i];

            // photon noise on closure phase
            t3photCoef[i] = invNbPhotonInIPerTel;
            t3photCoef2[i] = FastMath.pow2(invNbPhotonInIPerTel);
            t3photCoef3[i] = FastMath.pow3(invNbPhotonInIPerTel);

            // detector noise on closure phase
            t3detConst[i] = FastMath.pow(invNbPhotonInIPerTel, 6d) * (FastMath.pow(nbPixInterf, 3d) * FastMath.pow(ron, 6d)
                    + 3 * FastMath.pow2(nbPixInterf) * FastMath.pow(ron, 6d));

            t3detCoef1[i] = FastMath.pow(invNbPhotonInIPerTel, 4d) * (3d * nbPixInterf * FastMath.pow(ron, 4d)
                    + FastMath.pow2(nbPixInterf) * FastMath.pow(ron, 4d));

            t3detCoef2[i] = FastMath.pow2(invNbPhotonInIPerTel) * nbPixInterf * FastMath.pow2(ron);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("elevation                     : {}", targetPointInfos[iPoint].getElevation());
            logger.debug("t3photCoef                    : {}", Arrays.toString(t3photCoef));
            logger.debug("t3photCoef2                   : {}", Arrays.toString(t3photCoef2));
            logger.debug("t3photCoef3                   : {}", Arrays.toString(t3photCoef3));
            logger.debug("t3detConst                    : {}", Arrays.toString(t3detConst));
            logger.debug("t3detCoef1                    : {}", Arrays.toString(t3detCoef1));
            logger.debug("t3detCoef2                    : {}", Arrays.toString(t3detCoef2));
        }
    }

    /**
     * Compute error on closure phase. It returns NaN if the error can not be computed
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @param visAmp12 visibility amplitude of baseline AB = 12
     * @param visAmp23 visibility amplitude of baseline BC = 23
     * @param visAmp31 visibility amplitude of baseline CA = 31
     * @return error on closure phase in radians or NaN if the error can not be computed
     */
    public double computeT3PhiError(final int iPoint, final int iChannel,
                                    final double visAmp12, final double visAmp23, final double visAmp31) {
        // fast return NaN if invalid configuration :
        if (this.invalidParameters) {
            return Double.NaN;
        }
        if (DO_CHECKS && check(iPoint, iChannel)) {
            return Double.NaN;
        }

        final NoiseWParams param = this.params[iPoint];
        final double[] t3photCoef = param.t3photCoef;
        final double[] t3photCoef2 = param.t3photCoef2;
        final double[] t3photCoef3 = param.t3photCoef3;

        final double[] t3detConst = param.t3detConst;
        final double[] t3detCoef1 = param.t3detCoef1;
        final double[] t3detCoef2 = param.t3detCoef2;

        // include instrumental visib
        final double v1 = visAmp12 * vinst;
        final double v2 = visAmp23 * vinst;
        final double v3 = visAmp31 * vinst;

        double v123 = v1 * v2 * v3;

        // protect zero divide: TODO: why 1e-4d / 1e-20d ?
        v123 = Math.max(v123, 1e-4d);

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
        final double scpphot = (t3photCoef3[iChannel] * (nbTel * nbTel * nbTel - 2d * v123)
                + t3photCoef2[iChannel] * (nbTel * nbTel * (sv1 + sv2 + sv3) - (sv1 * sv1 + sv2 * sv2 + sv3 * sv3 + 2 * (sv12 + sv13 + sv23)))
                + t3photCoef[iChannel] * (nbTel * (sv12 + sv13 + sv23) - 2d * v123 * (sv1 + sv2 + sv3))) / (2d * sv123);
        /*
         final double scpphot = (Math.pow(nbTel / nbPhotonInI, 3d) * (nbTel * nbTel * nbTel - 2d * v123)
         + Math.pow(nbTel / nbPhotonInI, 2d) * (nbTel * nbTel * (sv1 + sv2 + sv3) - (sv1 * sv1 + sv2 * sv2 + sv3 * sv3 + 2 * (sv12 + sv13 + sv23)))
         + (nbTel / nbPhotonInI) * (nbTel * (sv12 + sv13 + sv23) - 2d * v123 * (sv1 + sv2 + sv3))) / (2d * sv123);
         */

        // detector noise on closure phase
        final double scpdet = (t3detConst[iChannel]
                + t3detCoef1[iChannel] * (sv1 + sv2 + sv3)
                + t3detCoef2[iChannel] * (sv12 + sv13 + sv23)) / (2d * sv123);
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
        // Note: bias are normally not a gaussian distribution (mean = 0) so should not be used to compute gaussian noise !!
        return Math.max(sclosph, instrumentalPhaseBias);
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
     * It returns Complex.NaN if the flux can not be computed
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @param visAmp visibility amplitude
     * @return complex visiblity error or NaN if the error can not be computed
     */
    public Complex computeVisComplexError(final int iPoint, final int iChannel, final double visAmp) {
        // complex visibility error:
        final double visErr = computeVisComplexErrorValue(iPoint, iChannel, visAmp);

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
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @param visAmp visibility amplitude
     * @return complex visiblity error or NaN if the error can not be computed
     */
    public double computeVisComplexErrorValue(final int iPoint, final int iChannel, final double visAmp) {
        // fast return Double.NaN if invalid configuration :
        if (this.invalidParameters) {
            return Double.NaN;
        }
        // visibility amplitude error :
        final double visAmpErr = computeVisError(iPoint, iChannel, visAmp);

        // Distribute the error on RE/IM parts for an uniform error distribution :
        // see These Martin Vannier (2003) p 76
        // sigma2(visRe) = 1/2 ( sigma2(visRe) + sigma2(visIm) = sigma2(vis) / 2
        // complex visibility error : visErrRe = visErrIm = visAmpErr / SQRT(2) :
        return visAmpErr * VIS_AMP_TO_VIS_CPX_ERR;
    }

    /**
     * Compute error on visibility amplitude derived from computeVis2Error(visAmp)
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @param visAmp visibility amplitude
     * @return visiblity error
     */
    private double computeVisError(final int iPoint, final int iChannel, final double visAmp) {
        // vis2 error without bias :
        final double errV2 = computeVis2ErrorNoBias(iPoint, iChannel, visAmp);

        // dvis = d(vis2) / (2 * vis) :
        final double visAmpErr = errV2 / (2d * visAmp);

        // convert instrumental phase bias as an error too. Use it as a limit:
        // Note: bias are normally not a gaussian distribution (mean = 0) so should not be used to compute gaussian noise !!
        return Math.max(visAmpErr, visAmp * instrumentalPhaseBias);
    }

    /**
     * Compute error on square visibility. It returns NaN if the error can not be computed
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @param visAmp visibility amplitude
     * @return square visiblity error or NaN if the error can not be computed
     */
    public double computeVis2Error(final int iPoint, final int iChannel, final double visAmp) {
        return computeVis2Error(iPoint, iChannel, visAmp, true);
    }

    /**
     * Compute error on square visibility without bias. It returns NaN if the error can not be computed
     *
     * @param iChannel index of the channel
     * @param visAmp visibility amplitude
     * @return square visiblity error or NaN if the error can not be computed
     */
    public double computeVis2ErrorNoBias(final int iChannel, final double visAmp) {
        return computeVis2Error(iMidPoint, iChannel, visAmp, false);
    }
    /**
     * Compute error on square visibility without bias. It returns NaN if the error can not be computed
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @param visAmp visibility amplitude
     * @return square visiblity error or NaN if the error can not be computed
     */
    private double computeVis2ErrorNoBias(final int iPoint, final int iChannel, final double visAmp) {
        return computeVis2Error(iPoint, iChannel, visAmp, false);
    }

    private boolean check(final int iPoint, final int iChannel) {
        if (iPoint < 0 || iPoint >= nPoints) {
            logger.warn("invalid point index {}, expect [0 to {}]", iPoint, nPoints);
            return true;
        }
        if (iChannel < 0 || iChannel >= nSpectralChannels) {
            logger.warn("invalid channel index {}, expect [0 to {}]", iChannel, nSpectralChannels);
            return true;
        }
        return false;
    }

    /* --- VisNoiseService implementation --- */
    /**
     * Return true if this service is enabled
     * @return true if this service is enabled
     */
    @Override
    public boolean isEnabled() {
        return isValid();
    }

    /**
     * Compute error on complex visibility given its amplitude.
     * It returns Double.NaN if the error can not be computed
     *
     * @param visAmp visibility amplitude
     * @return complex visiblity error or NaN if the error can not be computed
     */
    @Override
    public double computeVisComplexErrorValue(final double visAmp) {
        // fast return NaN if invalid configuration :
        if (this.invalidParameters) {
            return Double.NaN;
        }
        return computeVisComplexErrorValue(iMidPoint, iMidChannel, visAmp);
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
        return computeVis2Error(iMidPoint, iMidChannel, visAmp);
    }

    /* --- utility methods --- */
    /**
     * Find the band corresponding to the given wavelength
     * but always use V band instead of I & R bands
     *
     * @param waveLength wave length in microns
     * @return corresponding band
     * @throws IllegalArgumentException if no band found
     */
    public static Band findBand(final double waveLength) throws IllegalArgumentException {
        Band band = Band.findBand(waveLength);
        // TODO: fix that logic to use all possible bands within the instrument bandwidth
        switch (band) {
            case B:
            case V:
            case R:
            case I:
                // always use V for VEGA:
                return Band.V;
            default:
                return band;
        }
    }

    /**
     * Add a warning message in the OIFits file
     * @param msg message to add
     */
    private void addWarning(final String msg) {
        this.warningContainer.addWarningMessage(msg);
    }

    /**
     * Add an information message in the OIFits file
     * @param msg message to add
     */
    private void addInformation(final String msg) {
        this.warningContainer.addInformationMessage(msg);
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

    private static double getPercents(final double bias) {
        return 0.01d * bias; // percents
    }

    static class NoiseWParams {
        /* varying values (spectrally dependent) */

        /** (W) number of photons in interferometer channel */
        final double[] nbPhotonInI;
        /** (W) number of photons in each photometric channel (photometric flux) */
        final double[] nbPhotonInP;
        /** (W) photometric error contribution on the square visiblity */
        final double[] errV2Phot;
        /** (W) coefficient used to the squared correlated flux */
        final double[] sqCorFluxCoef;
        /** (W) coefficient used to compute variance of the squared correlated flux */
        final double[] varSqCorFluxCoef;
        /** (W) constant used to compute variance of the squared correlated flux */
        final double[] varSqCorFluxConst;
        /** (W) t3 phi error - coefficient */
        final double[] t3photCoef;
        /** (W) t3 phi error - coefficient^2 */
        final double[] t3photCoef2;
        /** (W) t3 phi error - coefficient^3 */
        final double[] t3photCoef3;
        /** (W) t3 phi detector error - constant */
        final double[] t3detConst;
        /** (W) t3 phi detector error - coefficient 1 */
        final double[] t3detCoef1;
        /** (W) t3 phi detector error - coefficient 2 */
        final double[] t3detCoef2;

        NoiseWParams(final int nWLen) {
            this.nbPhotonInI = init(nWLen);
            this.nbPhotonInP = init(nWLen);
            this.errV2Phot = init(nWLen);
            this.sqCorFluxCoef = init(nWLen);
            this.varSqCorFluxCoef = init(nWLen);
            this.varSqCorFluxConst = init(nWLen);
            this.t3photCoef = init(nWLen);
            this.t3photCoef2 = init(nWLen);
            this.t3photCoef3 = init(nWLen);
            this.t3detConst = init(nWLen);
            this.t3detCoef1 = init(nWLen);
            this.t3detCoef2 = init(nWLen);
        }

        static double[] init(final int len) {
            final double[] array = new double[len];
            Arrays.fill(array, Double.NaN);
            return array;
        }
    }
}
