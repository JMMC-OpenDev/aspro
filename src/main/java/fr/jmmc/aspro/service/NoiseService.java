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
import fr.jmmc.aspro.model.oi.AdaptiveOpticsSetup;
import fr.jmmc.aspro.model.oi.BiasType;
import fr.jmmc.aspro.model.oi.BiasUnit;
import fr.jmmc.aspro.model.oi.BiasValue;
import fr.jmmc.aspro.model.oi.DelayLineMode;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.FocalInstrumentSetup;
import fr.jmmc.aspro.model.oi.ObservationSequence;
import fr.jmmc.aspro.model.oi.SpectralSetup;
import fr.jmmc.aspro.model.oi.SpectralSetupColumn;
import fr.jmmc.aspro.model.oi.SpectralSetupQuantity;
import fr.jmmc.aspro.model.util.AtmosphereQualityUtils;
import fr.jmmc.aspro.model.util.SpectralBandUtils;
import fr.jmmc.aspro.model.util.TargetRole;
import fr.jmmc.aspro.model.util.TargetUtils;
import static fr.jmmc.aspro.service.AbstractOIFitsProducer.convertWL;
import fr.jmmc.jmal.ALX;
import fr.jmmc.jmcs.util.StatUtils;
import fr.jmmc.jmal.Band;
import fr.jmmc.jmal.model.VisNoiseService;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.SpecialChars;
import fr.jmmc.jmcs.util.WelfordVariance;
import fr.jmmc.oitools.image.FitsUnit;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
public final class NoiseService implements VisNoiseService {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(NoiseService.class.getName());

    /** enable/disable atmosphere transmission & strehl (to see instrument effects only) */
    private final static boolean DO_ATM_STREHL = true;

    /** enable bound checks on iPoint / iChannel */
    private final static boolean DO_CHECKS = false;

    /** enable tests on vis2 error */
    private final static boolean DO_DUMP_VIS2 = false;

    /** dump atmospheric transmission */
    private final static boolean DO_DUMP_ATM_TRANS = false;

    /** dump strehl values */
    private final static boolean DO_DUMP_STREHL = false;

    /** maximum error on Vis to avoid excessively large errors */
    private final static double MAX_ERR_V = 10.0;
    /** maximum error on Vis2 to avoid excessively large errors */
    private final static double MAX_ERR_V2 = MAX_ERR_V * MAX_ERR_V;

    /** enable bias handling (only for debugging / ETC tests) */
    private final static boolean USE_BIAS = true;

    /** enable DEBUG mode (t3) */
    public final static boolean DEBUG = false;

    private final static double FIXED_STREHL = (false) ? 0.5 : Double.NaN;
    private final static boolean USE_FIXED_STREHL = !Double.isNaN(FIXED_STREHL);

    /** minimum RON value if RON < 1.0 (GRAVITY issue at LOW resolution) */
    private final static double MIN_RON_GRAVITY_LOW = 3.5;

    static {
        if (!DO_ATM_STREHL) {
            logger.warn("WARNING: DO_ATM_STREHL=false (dev) - DO NOT USE IN PRODUCTION !");
        }
        if (USE_FIXED_STREHL) {
            logger.warn("WARNING: FIXED_STREHL=" + FIXED_STREHL + " (dev) - DO NOT USE IN PRODUCTION !");
        }
        if (DO_DUMP_STREHL || DO_DUMP_ATM_TRANS) {
            logger.warn("WARNING: DO_DUMP_xxx enabled (dev) - DO NOT USE IN PRODUCTION !");
        }
    }

    /* members */
    /** instrument name */
    private String instrumentName = null;

    /* interferometer parameters */
    /** telescope in use */
    private Telescope telescope = null;
    /** Telescope Diameter (m) */
    private double telDiam = Double.NaN;
    /** Number of telescopes interfering as double */
    private double nbTel = Double.NaN;
    /** optional delayLine transmission (loss) */
    private double delayLineTransmission = 1.0;

    /* adaptive optics parameters */
    /** seeing (arc sec) */
    private double seeing = Double.NaN;
    /** coherence time (ms) */
    private double t0 = Double.NaN;
    /** turbulence height (m) */
    private double h0 = Double.NaN;
    /** AO setup */
    private AdaptiveOpticsSetup aoSetup = null;
    /** AO Usage limit (mag) */
    private double adaptiveOpticsLimit = Double.NaN;
    /** AO (multiplicative) Instrumental transmission (0..1) */
    private double aoInstrumentalTransmission = Double.NaN;
    /** distance between target and the AO star (deg) */
    private double distAO = 0.0;

    /** AO band */
    private SpectralBand aoBand;

    /* instrument parameters */
 /* fixed values (spectrally independent) */
    /** Total Acquisition time per observed u,v point (s) */
    private double totalObsTime = Double.NaN;
    /** Detector individual integration time (s) */
    private double dit = Double.NaN;
    /** Detector is non-linear above (to avoid saturation/non-linearity) */
    private double detectorSaturation = Double.NaN;
    /** Detector quantum efficiency (optional ie 1.0 by default) */
    private double quantumEfficiency = Double.NaN;

    /** flag to use the photometry */
    private boolean usePhotometry = false;
    /** flag to use the strehl correction */
    private boolean useStrehlCorrection = true;

    /* varying values (spectrally dependent) */
    /** (W) instrument band */
    private Band[] insBand;
    /** distinct instrument bands */
    private Set<Band> usedInsBands = null;

    /** fraction of flux going into the interferometric channel */
    private double fracFluxInInterferometry = Double.NaN;
    /** fraction of flux going into the photometric channel */
    private double fracFluxInPhotometry = Double.NaN;

    /** ratio photometry exposures per photometric channel (chopping) */
    private double ratioPhotoPerBeam = Double.NaN;
    /** ratio between photometry vs interferometry in time unit per beam  */
    private double ratioPhotoVsInterfero = Double.NaN;

    /** Observation DIT (s) */
    private double obsUsedDit = Double.NaN;

    /** frame ratio (overhead) */
    private double frameRatio = 1.0;

    /** (W) Detector readout noise */
    private double[] ron = null;
    /** (W) Transmission of interferometer+instrument at observed wavelength (no strehl, no QE) */
    private double[] transmission = null;
    /** (W) Instrumental Visibility [0.0-1.0] */
    private double[] instrumentalVisibility = null;
    /** (W) Number of thermal photon (background) per beam per second (no strehl, no QE) */
    private double[] nbPhotThermal = null;
    /** (W) number of pixels to code all fringes together (interferometric channel) */
    private double[] nbPixInterf = null;
    /** (W) number of pixels to code each photometric channel */
    private double[] nbPixPhoto = null;
    /** (W) Coefficient A_bkg in gravity background noise model: background = S_bkg * dit + ron**2 ; ron = A_bkg / SQRT(dit) + B_bkg */
    private double[] coeff_A_bkg = null;
    /** (W) Coefficient B_bkg in gravity background noise model: background = S_bkg * dit + ron**2 ; ron = A_bkg / SQRT(dit) + B_bkg */
    private double[] coeff_B_bkg = null;

    /* instrument &nd calibration bias */
    /** true to use calibration bias; false to compute only theoretical (optional systematic) error */
    private final boolean useCalibrationBias;
    /** true to use instrument or calibration bias; false to compute only theoretical error */
    private boolean useBias;
    /** true to use random bias; false to disable random sampling, only adjust error */
    private boolean useRandomCalBias;
    /** flag to prepare instrumentalVisRelBias / instrumentalVis2RelBias using instrumentalPhotRelBias */
    private boolean prepareVisRelBias = false;
    /** (W) Typical Photometric Bias (relative) */
    private double[] instrumentalPhotRelBias = null;
    /** (W) Typical Vis Bias (relative) */
    private double[] instrumentalVisRelBias = null;
    /** (W) Typical Vis Calibration Bias (absolute) */
    private double[] instrumentalVisCalBias = null;
    /** (W) Typical Vis2 Bias (relative) */
    private double[] instrumentalVis2RelBias = null;
    /** (W) Typical Vis. Phase Bias (rad) */
    private double[] instrumentalVisPhaseBias = null;
    /** (W) Typical Vis. Phase Calibration Bias (rad) */
    private double[] instrumentalVisPhaseCalBias = null;
    /** (W) Typical Phase Closure Bias (rad) */
    private double[] instrumentalT3PhaseBias = null;
    /** (W) Typical Phase Closure Calibration Bias (rad) */
    private double[] instrumentalT3PhaseCalBias = null;

    /* instrument mode parameters */
    /** number of spectral channels */
    private final int nSpectralChannels;
    /** index of reference (central) spectral channel */
    private final int iRefChannel;
    /** spectral channel central wavelength (meters) */
    private final double[] waveLengths;
    /** spectral channel widths */
    private final double[] waveBands;

    /* fringe tracker parameters */
    /** distance between SCI and FT stars */
    private double distSCI_FT = 0.0;
    /** Fringe Tracker is Present */
    private boolean fringeTrackerPresent = false;
    /** Fringe Tracking Mode i.e. not group tracking (FINITO) */
    private boolean fringeTrackingMode = false;
    /** Fringe Tracker (multiplicative) Instrumental Visibility (factor or loss) */
    private double fringeTrackerInstrumentalVisibility = Double.NaN;
    /** Fringe Tracker Usage limit (mag) */
    private double fringeTrackerLimit = Double.NaN;
    /** Fringe Tracker Max Integration Time (s) */
    private double fringeTrackerMaxDit = Double.NaN;
    /** Fringe Tracker Max Frame Time (s) */
    private double fringeTrackerMaxFrameTime = Double.NaN;
    /** FT band */
    private SpectralBand ftBand;

    /* object parameters */
    /** (W) Magnitude in Observing Band (mag) */
    private double[] objectMag = null;
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
 /* time formatter */
    private final DecimalFormat df = new DecimalFormat("##0.##");
    /** flag to indicate that parameter(s) are invalid */
    private boolean invalidParametersInit = false;
    /** flag to indicate that a parameter is invalid to return errors as NaN values */
    private boolean invalidParameters = false;

    /** (W) total instrumental visibility (with FT if any) */
    private double[] vinst;

    /* cached intermediate constants */
    /** max dit before saturation (s) */
    private double maxObsDit = 0.0;
    /** error correction = 1 / SQRT(total frame) */
    private double totFrameCorrection;
    /** error correction = 1 / SQRT(total frame photometry) */
    private double totFrameCorrectionPhot;
    /** noise computation parameters per uv point */
    private NoiseWParams[] params = null;
    /* varying values (spectrally dependent) */
    /** (W) number of thermal photons per spectral channel per second per telescope (with QE) */
    private double[] nbTotalPhotTherm = null;
    /** (W) number of science photons per spectral channel per second per telescope (target flux x telescope surface) */
    private double[][] nbTotalObjPhot = null;
    /** (W) number of science photons per spectral channel per second per telescope on detector (with QE, ins & atm transmissions, strehl) */
    private double[][] nbTotalPhot = null;
    /** (W) number of thermal photons per telescope in the interferometric channel */
    private double[] nbPhotThermInterf = null;
    /** (W) number of thermal photons per telescope in each photometric channel */
    private double[] nbPhotThermPhoto = null;

    /** mean(visibility scale) computed for the mid point and reference channel */
    private double visScaleMeanForMidRefPoint = 1.0;

    /** warning container used during compute phase */
    private final WarningContainer warningContainerCompute;
    /* stats */
    private final WelfordVariance strehlStats = new WelfordVariance();
    private final WelfordVariance atmTransStats = new WelfordVariance();

    /**
     * Protected constructor
     * @param observation observation settings used (read-only copy of the modifiable observation)
     * @param delayLineMode(optional) delayLine mode in use
     * @param targetMapping target mappings
     * @param targetRole target role of this observation
     * @param targetPointInfos target information for each uv point couples
     * @param useCalibrationBias true to use calibration bias; false to compute only theoretical (optional systematic) error
     * @param warningContainer to store warning & information during initialization
     * @param warningContainerCompute to store warning & information during computation
     * @param waveLengths concrete wavelength values (spectral channel central value) in meters
     * @param waveBands concrete spectral channel bandwidths in meters
     * @param useWavelengthRangeRestriction use wavelength restrictions
     * @param isFTValid true if FT observation is valid at initial state; false otherwise
     */
    protected NoiseService(final ObservationSetting observation,
                           final DelayLineMode delayLineMode,
                           final Map<TargetRole, Target> targetMapping,
                           final TargetRole targetRole,
                           final TargetPointInfo[] targetPointInfos,
                           final boolean useCalibrationBias,
                           final WarningContainer warningContainer,
                           final WarningContainer warningContainerCompute,
                           final double[] waveLengths,
                           final double[] waveBands,
                           final boolean useWavelengthRangeRestriction,
                           final boolean isFTValid) {

        this.useCalibrationBias = USE_BIAS && useCalibrationBias;

        // Get spectral channels:
        this.nSpectralChannels = waveLengths.length;
        this.waveLengths = waveLengths;
        this.waveBands = waveBands;

        final FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();
        if (insMode == null) {
            throw new IllegalStateException("The instrumentMode is empty !");
        }

        // Fix mid channel for image noising (MATISSE LM has a large hole at 4 microns):
        int midChannel = this.nSpectralChannels / 2;
        if ((nSpectralChannels > 1) && (insMode.getWaveLengthRef() != null)) {
            final double lambdaRef = AsproConstants.MICRO_METER * insMode.getWaveLengthRef().doubleValue();

            for (int i = 0, end = waveLengths.length - 1; i < end; i++) {
                if (Math.abs(waveLengths[i] - lambdaRef) <= Math.abs(waveLengths[i + 1] - lambdaRef)) {
                    midChannel = i;
                    break;
                }
            }
        }
        this.iRefChannel = midChannel;

        this.targetPointInfos = targetPointInfos;
        this.nPoints = this.targetPointInfos.length;
        this.iMidPoint = this.nPoints / 2;

        // Get the delayline transmission (double-path loss):
        this.delayLineTransmission = (delayLineMode != null) ? delayLineMode.getTransmissionOrDefault() : 1.0;

        if (logger.isDebugEnabled()) {
            logger.debug("spectralChannels              : {}", nSpectralChannels);
            logger.debug("iRefChannel                   : {}", iRefChannel);
            logger.debug("waveLength[iRefChannel]       : {}", waveLengths[iRefChannel]);
            logger.debug("waveLengths                   : {}", Arrays.toString(waveLengths));
            logger.debug("waveBands[iRefChannel]        : {}", waveBands[iRefChannel]);
            logger.debug("waveBands                     : {}", Arrays.toString(waveBands));
            logger.debug("useWavelengthRangeRestriction : {}", useWavelengthRangeRestriction);
            logger.debug("delayLineTransmission         : {}", delayLineTransmission);
        }

        // extract parameters in observation and configuration :
        prepareInterferometer(observation, targetMapping);
        prepareInstrument(warningContainer, observation, useWavelengthRangeRestriction);
        prepareFringeTracker(observation, targetMapping, targetRole);
        prepareTarget(warningContainer, targetMapping, targetRole);
        initParameters(warningContainer, targetMapping, targetRole, isFTValid);

        // after initialization to be sure:
        this.warningContainerCompute = warningContainerCompute;
    }

    /**
     * Prepare interferometer and AO parameters (related to telescopes so to each configuration)
     * @param observation observation settings
     * @param targetMapping target mappings
     */
    private void prepareInterferometer(final ObservationSetting observation, final Map<TargetRole, Target> targetMapping) {

        final List<Station> stations = observation.getInstrumentConfiguration().getStationList();

        this.nbTel = stations.size();

        // All telescopes have the same diameter for a given baseline :
        this.telescope = stations.get(0).getTelescope();

        this.telDiam = telescope.getDiameter();

        // AO handling
        AdaptiveOptics ao = null;

        // AO is defined on SCI target:
        final Target target = targetMapping.get(TargetRole.SCI);
        logger.debug("prepareInterferometer: science target = {}", target);

        final TargetConfiguration targetConf = target.getConfiguration();

        if ((targetConf != null) && (targetConf.getAoSetup() != null)) {
            this.aoSetup = telescope.findAOSetup(targetConf.getAoSetup());
            if (this.aoSetup == null) {
                // TODO: support multi-config VLTI UT and AT confs:
                logger.info("Invalid AO[{}] for telescope {}", targetConf.getAoSetup(), telescope.getName());
            } else {
                ao = this.aoSetup.getAdaptiveOptics();
            }
        }
        if (ao == null) {
            if (!telescope.getAdaptiveOptics().isEmpty()) {
                // use default AO for the telescope:
                ao = telescope.getAdaptiveOptics().get(0);
                if (!ao.getSetups().isEmpty()) {
                    this.aoSetup = ao.getSetups().get(0); // FIRST SETUP if present
                    logger.info("Using default AO setup[{}] for telescope {}", aoSetup.getName(), telescope.getName());
                }
            }
        }

        if (ao != null) {
            this.aoBand = ao.getBand();
            this.adaptiveOpticsLimit = (ao.getMagLimit() != null) ? ao.getMagLimit().doubleValue() : Double.NaN;
        } else {
            // by default: compute strehl ratio on V band with only 1 actuator ?
            this.aoBand = SpectralBand.V;
        }

        // Seeing :
        final AtmosphereQuality atmQual = observation.getWhen().getAtmosphereQuality();
        if (atmQual != null) {
            this.seeing = AtmosphereQualityUtils.getSeeing(atmQual);
            this.t0 = AtmosphereQualityUtils.getCoherenceTime(atmQual);
            this.h0 = AtmosphereQualityUtils.getTurbulenceHeight(atmQual);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("nbTel                         : {}", nbTel);
            logger.debug("telDiam                       : {}", telDiam);
            logger.debug("aoBand                        : {}", aoBand);
            logger.debug("aoSetup                       : {}", aoSetup);
            logger.debug("seeing                        : {}", seeing);
            logger.debug("t0                            : {}", t0);
        }
    }

    /**
     * Prepare instrument and mode parameters
     * @param warningContainer to store warning & information during initialization
     * @param observation observation settings
     * @param useWavelengthRangeRestriction use wavelength restrictions
     */
    private void prepareInstrument(final WarningContainer warningContainer, final ObservationSetting observation, final boolean useWavelengthRangeRestriction) {

        if (observation.getInstrumentConfiguration().getAcquisitionTime() != null) {
            this.totalObsTime = observation.getInstrumentConfiguration().getAcquisitionTime().doubleValue();
        }

        final FocalInstrument instrument = observation.getInstrumentConfiguration().getInstrumentConfiguration().getFocalInstrument();

        final FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();
        final FocalInstrumentSetup insSetup = insMode.getSetupRef();

        // use alias or real instrument name:
        this.instrumentName = instrument.getAliasOrName();

        this.detectorSaturation = insSetup.getDetectorSaturation();
        this.quantumEfficiency = insSetup.getQuantumEfficiency();

        // Read-out noise:
        this.ron = new double[nSpectralChannels];
        Arrays.fill(this.ron, insSetup.getRon());

        // DIT:
        this.dit = insSetup.getDit();
        if (insMode.getDit() != null) {
            this.dit = insMode.getDit();
        }

        // fractions:
        this.fracFluxInInterferometry = insSetup.getFracFluxInInterferometry();
        this.fracFluxInPhotometry = insSetup.getFracFluxInPhotometry();

        this.useStrehlCorrection = insSetup.isUseStrehlCorrection();

        // Use Sequence to get both time & beam ratios:
        final ObservationSequence sequence = insSetup.getSequence();

        final double ratioInterfero = sequence.getRatioInterferometry(); // [0..1]

        this.ratioPhotoPerBeam = sequence.getRatioPhotoPerBeam();
        this.ratioPhotoVsInterfero = sequence.getRatioPhotoVsInterfero();

        this.frameRatio = 1.0;
        if (insMode.getFrameTime() != null) {
            this.frameRatio = insMode.getFrameTime() / this.dit;
        }

        final double effectiveFrameTime = frameRatio * this.dit;
        // ratio in time [0..1]
        // note: ratio is not correct if FT is enabled !
        final double ratioTimeInterfero = ratioInterfero / frameRatio;

        if (logger.isDebugEnabled()) {
            logger.debug("ratioInterfero                : {}", ratioInterfero);
            logger.debug("ratioPhotoPerBeam             : {}", ratioPhotoPerBeam);
            logger.debug("frameRatio                    : {}", frameRatio);
            logger.debug("effectiveFrameTime            : {}", effectiveFrameTime);
            logger.debug("efficiency (%)                : {}", (100.0 * ratioTimeInterfero));
            logger.debug("totalObsTime                  : {}", totalObsTime);
            logger.debug("totalOBTime                   : {}", totalObsTime / ratioTimeInterfero);
        }

        if (ratioTimeInterfero < 0.99) {
            final double seqTimeMin = (totalObsTime / ratioTimeInterfero); // s

            warningContainer.addInformation("Min O.B. time: " + df.format(Math.round(seqTimeMin)) + " s ("
                    + df.format(Math.round(seqTimeMin / 60.0)) + " min) on acquisition"
                    + " - Ratio Interferometry: " + df.format(100.0 * ratioTimeInterfero) + " %");
        }

        final boolean insHasInstrumentBias = (!insSetup.getInstrumentBias().isEmpty());

        if (USE_BIAS && (this.useCalibrationBias || insHasInstrumentBias)) {
            this.useBias = true;
            this.useRandomCalBias = true; // default

            this.instrumentalPhotRelBias = new double[nSpectralChannels];
            this.instrumentalVisRelBias = new double[nSpectralChannels];
            this.instrumentalVisCalBias = new double[nSpectralChannels];
            this.instrumentalVis2RelBias = new double[nSpectralChannels];

            this.instrumentalVisPhaseBias = new double[nSpectralChannels];
            this.instrumentalVisPhaseCalBias = new double[nSpectralChannels];

            this.instrumentalT3PhaseBias = new double[nSpectralChannels];
            this.instrumentalT3PhaseCalBias = new double[nSpectralChannels];

            if (insHasInstrumentBias) {
                // New approach to only add calibration bias into error without random sampling:
                this.useRandomCalBias = false;

                // telescope is known:
                final Telescope tel = this.telescope;
                // AtmosphereQuality is known:
                final AtmosphereQuality atmQual = observation.getWhen().getAtmosphereQuality();

                // initialize vectors:
                for (int i = 0; i < nSpectralChannels; i++) {
                    // band: depends on spectral channel
                    final double lambda = this.waveLengths[i] / AsproConstants.MICRO_METER; // microns

                    // Band is known
                    final SpectralBand band = SpectralBandUtils.findBand(findBand(instrumentName, lambda));

                    for (final BiasType type : BiasType.values()) {
                        final BiasValue insBias = insSetup.getInstrumentBias(type, band, atmQual, tel);
                        // only if calibration bias enabled:
                        final BiasValue calBias = (this.useCalibrationBias) ? insSetup.getCalibrationBias(type, band, atmQual, tel) : null;

                        logger.debug("bias type: {} ins bias: {} cal bias: {}", type, insBias, calBias);

                        // note: calibration bias are always absolute (amp / phi)
                        // sum of variance:
                        switch (type) {
                            case VIS:
                                if (insBias != null) {
                                    if (insBias.getUnit() == BiasUnit.REL) {
                                        this.instrumentalVisRelBias[i] = insBias.getValue();
                                    } else {
                                        logger.warn("Absolute Vis instrumental bias not supported !");
                                    }
                                }
                                if (calBias != null) {
                                    if (calBias.getUnit() == BiasUnit.REL) {
                                        logger.warn("Relative Vis calibration bias not supported !");
                                    } else {
                                        this.instrumentalVisCalBias[i] = calBias.getValue();
                                    }
                                }
                                break;
                            case VISPHI:
                                if (insBias != null) {
                                    if (insBias.getUnit() == BiasUnit.REL) {
                                        logger.warn("Relative VisPhi instrumental bias not supported !");
                                    } else {
                                        this.instrumentalVisPhaseBias[i] = insBias.getValue();
                                    }
                                }
                                if (calBias != null) {
                                    if (calBias.getUnit() == BiasUnit.REL) {
                                        logger.warn("Relative VisPhi calibration bias not supported !");
                                    } else {
                                        this.instrumentalVisPhaseCalBias[i] = calBias.getValue();
                                    }
                                }
                                /* Convert Phase bias to radians */
                                this.instrumentalVisPhaseBias[i] = FastMath.toRadians(this.instrumentalVisPhaseBias[i]);
                                this.instrumentalVisPhaseCalBias[i] = FastMath.toRadians(this.instrumentalVisPhaseCalBias[i]);
                                break;
                            case T_3_PHI:
                                if (insBias != null) {
                                    if (insBias.getUnit() == BiasUnit.REL) {
                                        logger.warn("Relative T3Phi instrumental bias not supported !");
                                    } else {
                                        this.instrumentalT3PhaseBias[i] = insBias.getValue();
                                    }
                                }
                                if (calBias != null) {
                                    if (calBias.getUnit() == BiasUnit.REL) {
                                        logger.warn("Relative T3Phi calibration bias not supported !");
                                    } else {
                                        this.instrumentalT3PhaseCalBias[i] = calBias.getValue();
                                    }
                                }
                                /* Convert Phase bias to radians */
                                this.instrumentalT3PhaseBias[i] = FastMath.toRadians(this.instrumentalT3PhaseBias[i]);
                                this.instrumentalT3PhaseCalBias[i] = FastMath.toRadians(this.instrumentalT3PhaseCalBias[i]);
                                break;
                            case PHOT:
                                if (insBias != null) {
                                    if (insBias.getUnit() != BiasUnit.JY) {
                                        logger.warn("Only JY supported for Photometric instrumental bias !");
                                    } else {
                                        this.instrumentalPhotRelBias[i] = insBias.getValue(); // initial: e_phot_jy
                                    }
                                }
                                if (calBias != null) {
                                    logger.warn("Calibration bias not supported for Photometric bias !");
                                }
                                break;
                            default:
                        }
                    } // type
                } // channels

                // instrumentalPhotRelBias is incomplete (e_phot_jy only)
                // instrumentalVis2RelBias is undefined yet
                if (logger.isDebugEnabled()) {
                    logger.debug("instrumentalPhotRelBias:     {}", Arrays.toString(instrumentalPhotRelBias));     // e_phot_jy

                    logger.debug("instrumentalVisRelBias:      {}", Arrays.toString(instrumentalVisRelBias));      // rel
                    logger.debug("instrumentalVisCalBias:      {}", Arrays.toString(instrumentalVisCalBias));      // abs

                    logger.debug("instrumentalVisPhaseBias:    {}", Arrays.toString(instrumentalVisPhaseBias));    // rad
                    logger.debug("instrumentalVisPhaseCalBias: {}", Arrays.toString(instrumentalVisPhaseCalBias)); // rad

                    logger.debug("instrumentalT3PhaseBias:     {}", Arrays.toString(instrumentalT3PhaseBias));     // rad
                    logger.debug("instrumentalT3PhaseCalBias:  {}", Arrays.toString(instrumentalT3PhaseCalBias));  // rad
                }
                /* see next steps in prepareTarget() */
                this.prepareVisRelBias = true;

            } else {
                // former approach: absolute biases

                /* Get Vis bias (percents) */
                final double visBias = 0.01 * insSetup.getInstrumentVisibilityBias();
                Arrays.fill(this.instrumentalVisCalBias, visBias);

                /* Convert Phase bias to radians */
                final double phiBias = FastMath.toRadians(insSetup.getInstrumentPhaseBias());

                Arrays.fill(this.instrumentalVisPhaseCalBias, phiBias);
                Arrays.fill(this.instrumentalT3PhaseCalBias, phiBias);

                if (logger.isDebugEnabled()) {
                    logger.debug("instrumentalVisCalBias:      {}", Arrays.toString(instrumentalVisCalBias));    // abs
                    logger.debug("instrumentalVisPhaseCalBias: {}", Arrays.toString(instrumentalVisPhaseCalBias)); // rad
                    logger.debug("instrumentalT3PhaseBias:     {}", Arrays.toString(instrumentalT3PhaseBias)); // rad
                }
            }
        }
        logger.debug("useBias : {}", this.useBias);
        logger.debug("useRandomCalBias : {}", this.useRandomCalBias);

        // Check wavelength range:
        final double lambdaMin = this.waveLengths[0];
        final double lambdaMax = this.waveLengths[nSpectralChannels - 1];

        // TODO: fix message if FT enabled (no restriction ...)
        if (useWavelengthRangeRestriction) {
            warningContainer.addWarning("Detector can not be read completely within 1 DIT: the wavelength range is restricted to "
                    + df.format(insMode.getEffWaveLengthBandRef()) + " " + SpecialChars.UNIT_MICRO_METER);
        }

        final SpectralSetup table = insMode.getTable();
        int firstIdx = -1;
        int lastIdx = -1;

        if (table != null) {
            // check WLen range:
            final double[] lambda = table.getAndScaleColumn(SpectralSetupQuantity.LAMBDA, AsproConstants.MICRO_METER);
            if (lambda == null) {
                throw new IllegalStateException("Missing lambda column within spectral table !");
            }

            // TODO: fix check ranges
            for (int i = 0; i < lambda.length; i++) {
                if (Math.abs(lambda[i] - lambdaMin) < 1e-15) {
                    firstIdx = i;
                    break;
                }
            }

            for (int i = lambda.length - 1; i >= 0; i--) {
                if (Math.abs(lambda[i] - lambdaMax) < 1e-15) {
                    lastIdx = i + 1; // exclusive
                    break;
                }
            }

            if (firstIdx == -1 || lastIdx == -1) {
                throw new IllegalStateException("Invalid range within spectral table !");
            }
            // check the number of channels is consistent:
            if ((lastIdx - firstIdx) != nSpectralChannels) {
                throw new IllegalStateException("Inconsistent channel count: " + (lastIdx - firstIdx) + " expected : " + nSpectralChannels + " !");
            }

            // Get table data:
            SpectralSetupColumn col;
            // transmission:
            col = table.getColumnOrDefault(SpectralSetupQuantity.TRANSMISSION, telescope);
            if (col != null) {
                this.transmission = Arrays.copyOfRange(col.getValues(), firstIdx, lastIdx);
            }
            // visibility:
            col = table.getColumnOrDefault(SpectralSetupQuantity.VISIBILITY, telescope);
            if (col != null) {
                this.instrumentalVisibility = Arrays.copyOfRange(col.getValues(), firstIdx, lastIdx);
            }

            // nb photon thermal per beam per second (background):
            col = table.getColumnOrDefault(SpectralSetupQuantity.NB_PHOTON_THERMAL, telescope);
            if (col != null) {
                this.nbPhotThermal = Arrays.copyOfRange(col.getValues(), firstIdx, lastIdx);
            }

            // number of pixel per channel:
            col = table.getColumn(SpectralSetupQuantity.NB_PIX_INTERF);
            if (col != null) {
                this.nbPixInterf = Arrays.copyOfRange(col.getValues(), firstIdx, lastIdx);
            }
            col = table.getColumn(SpectralSetupQuantity.NB_PIX_PHOTO);
            if (col != null) {
                this.nbPixPhoto = Arrays.copyOfRange(col.getValues(), firstIdx, lastIdx);
            }

            // Coefficient A_bkg in gravity background noise model:
            col = table.getColumn(SpectralSetupQuantity.A_BKG);
            if (col != null) {
                this.coeff_A_bkg = Arrays.copyOfRange(col.getValues(), firstIdx, lastIdx);
            }
            // Coefficient B_bkg in gravity background noise model:
            col = table.getColumn(SpectralSetupQuantity.B_BKG);
            if (col != null) {
                this.coeff_B_bkg = Arrays.copyOfRange(col.getValues(), firstIdx, lastIdx);
            }
        }

        if (this.transmission == null) {
            this.transmission = new double[nSpectralChannels];
            Arrays.fill(this.transmission, insSetup.getTransmission());
        }
        if (this.instrumentalVisibility == null) {
            this.instrumentalVisibility = new double[nSpectralChannels];
            Arrays.fill(this.instrumentalVisibility, insSetup.getInstrumentVisibility());
        }
        if (this.nbPixInterf == null) {
            this.nbPixInterf = new double[nSpectralChannels];
            Arrays.fill(this.nbPixInterf, insSetup.getNbPixInterferometry());
        }
        if (this.nbPixPhoto == null) {
            this.nbPixPhoto = new double[nSpectralChannels];
            Arrays.fill(this.nbPixPhoto, insSetup.getNbPixPhotometry());
        }

        this.usePhotometry = ((fracFluxInPhotometry > 0.0) && (StatUtils.max(nbPixPhoto) > 0.0));

        if (logger.isDebugEnabled()) {
            logger.debug("instrumentName                : {}", instrumentName);
            logger.debug("instrumentSetup               : {}", insSetup.getName());
            logger.debug("totalObsTime                  : {}", totalObsTime);
            logger.debug("dit                           : {}", dit);
            logger.debug("ron                           : {}", ron);
            logger.debug("detectorSaturation            : {}", detectorSaturation);
            logger.debug("quantumEfficiency             : {}", quantumEfficiency);
            logger.debug("fracFluxInInterferometry      : {}", fracFluxInInterferometry);
            logger.debug("fracFluxInPhotometry          : {}", fracFluxInPhotometry);
            logger.debug("usePhotometry                 : {}", usePhotometry);
            logger.debug("useStrehlCorrection           : {}", useStrehlCorrection);
            logger.debug("transmission[iRefChannel]     : {}", transmission[iRefChannel]);
            logger.debug("transmission                  : {}", Arrays.toString(transmission));
            logger.debug("instrumentalVis[iRefChannel]  : {}", instrumentalVisibility[iRefChannel]);
            logger.debug("instrumentalVisibility        : {}", Arrays.toString(instrumentalVisibility));
            if (nbPhotThermal != null) {
                logger.debug("nbPhotThermal[iRefChannel]    : {}", nbPhotThermal[iRefChannel]);
                logger.debug("nbPhotThermal                 : {}", Arrays.toString(nbPhotThermal));
            }
            logger.debug("nbPixInterf[iRefChannel]      : {}", nbPixInterf[iRefChannel]);
            logger.debug("nbPixInterf                   : {}", Arrays.toString(nbPixInterf));
            logger.debug("nbPixPhoto[iRefChannel]       : {}", nbPixPhoto[iRefChannel]);
            logger.debug("nbPixPhoto                    : {}", Arrays.toString(nbPixPhoto));
        }
    }

    /**
     * Prepare fringe tracker parameters
     * @param observation observation settings
     * @param targetMapping target mappings
     * @param targetRole target role of this observation
     */
    private void prepareFringeTracker(final ObservationSetting observation,
                                      final Map<TargetRole, Target> targetMapping, final TargetRole targetRole) {
        // AtmosphereQuality is known:
        final AtmosphereQuality atmQual = observation.getWhen().getAtmosphereQuality();

        if (targetRole == TargetRole.SCI) {
            final Target target = targetMapping.get(TargetRole.SCI);
            logger.debug("prepareFringeTracker: science target = {}", target);

            final TargetConfiguration targetConf = target.getConfiguration();

            if ((targetConf != null) && (targetConf.getFringeTrackerMode() != null)) {
                final FocalInstrument instrument = observation.getInstrumentConfiguration().getInstrumentConfiguration().getFocalInstrument();

                final FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();

                final FringeTracker ft = instrument.getFringeTracker();
                if (ft != null) {
                    final String ftMode = targetConf.getFringeTrackerMode();
                    this.fringeTrackerPresent = true;
                    // TODO: handle FT modes properly: GroupTrack is hard coded !
                    this.fringeTrackingMode = (ftMode != null && !ftMode.startsWith(AsproConstants.FT_GROUP_TRACK));
                    this.fringeTrackerInstrumentalVisibility = ft.getInstrumentVisibility();

                    // Get the magnitude limit according to the atmosphere quality and telescope:
                    this.fringeTrackerLimit = ft.getMagLimit(atmQual, this.telescope);

                    this.fringeTrackerMaxDit = ft.getMaxIntegration();
                    this.fringeTrackerMaxFrameTime = this.fringeTrackerMaxDit;
                    this.ftBand = ft.getBand();

                    // use specific FT DIT defined for this instrument mode:
                    if (insMode.getFtDit() != null) {
                        this.fringeTrackerMaxDit = insMode.getFtDit();
                    }
                    if (insMode.getFtFrameTime() != null) {
                        this.fringeTrackerMaxFrameTime = insMode.getFtFrameTime();
                    }
                }
            }
        }
        if (logger.isDebugEnabled()) {
            logger.debug("atmQual                       : {}", atmQual);
            logger.debug("fringeTrackerPresent          : {}", fringeTrackerPresent);
        }
        if (fringeTrackerPresent) {
            if (logger.isDebugEnabled()) {
                logger.debug("fringeTrackingMode            : {}", fringeTrackingMode);
                logger.debug("fringeTrackerInstrumentalVisibility : {}", fringeTrackerInstrumentalVisibility);
                logger.debug("fringeTrackerLimit            : {}", fringeTrackerLimit);
                logger.debug("fringeTrackerMaxDit           : {}", fringeTrackerMaxDit);
                logger.debug("fringeTrackerMaxFrameTime     : {}", fringeTrackerMaxFrameTime);
                logger.debug("ftBand                        : {}", ftBand);
            }
        }
    }

    /**
     * Prepare object parameters
     * @param warningContainer to store warning & information during initialization
     * @param targetMapping target mappings
     * @param targetRole target role of this observation
     */
    private void prepareTarget(final WarningContainer warningContainer,
                               final Map<TargetRole, Target> targetMapping, final TargetRole targetRole) {

        // Get band from wavelength range:
        final double lambdaMin = waveLengths[0];
        final double lambdaMax = waveLengths[nSpectralChannels - 1];

        // use band range to cover lambdaMin / lambdaMax:
        // JHK or LM or BVR
        final Band bandMin = findBand(instrumentName, lambdaMin / AsproConstants.MICRO_METER); // microns
        final Band bandMax = findBand(instrumentName, lambdaMax / AsproConstants.MICRO_METER); // microns

        if (logger.isDebugEnabled()) {
            logger.debug("lambdaMin                     : {}", lambdaMin);
            logger.debug("bandMin                       : {}", bandMin);
            logger.debug("lambdaMax                     : {}", lambdaMax);
            logger.debug("bandMax                       : {}", bandMax);
        }

        this.insBand = new Band[nSpectralChannels];
        this.objectMag = new double[nSpectralChannels];

        // Distinct used bands specific to the instrument:
        this.usedInsBands = new LinkedHashSet<>(4);

        final Target target = targetMapping.get(targetRole);
        logger.debug("prepareTarget: {} target = {}", targetRole, target);

        // For science target only:
        final HashSet<SpectralBand> missingMags = new HashSet<SpectralBand>(4);
        Double flux;

        if (bandMin == bandMax) {
            // same band
            usedInsBands.add(bandMin);

            Arrays.fill(insBand, bandMin);

            /** instrument band corresponding to target mags */
            final SpectralBand insTargetBand = SpectralBandUtils.findBand(bandMin);

            if (logger.isDebugEnabled()) {
                logger.debug("insTargetBand                 : {}", insTargetBand);
            }

            // If a flux / magnitude is missing => user message
            // and it is impossible to compute any error
            flux = target.getFlux(insTargetBand);

            if (flux == null) {
                missingMags.add(insTargetBand);
            }

            Arrays.fill(objectMag, (flux != null) ? flux.doubleValue() : Double.NaN);
            if (logger.isDebugEnabled()) {
                logger.debug("objectMag                     : {}", flux);
            }
        } else {
            final int nWLen = nSpectralChannels;

            for (int i = 0; i < nWLen; i++) {
                final Band band = findBand(instrumentName, waveLengths[i] / AsproConstants.MICRO_METER); // microns
                usedInsBands.add(band);

                insBand[i] = band;

                /** instrument band corresponding to target mags */
                final SpectralBand insTargetBand = SpectralBandUtils.findBand(band);

                if (logger.isDebugEnabled()) {
                    logger.debug("insTargetBand[" + waveLengths[i] + "] : {}", insTargetBand);
                }

                // If a flux / magnitude is missing => user message
                // and it is impossible to compute any error
                flux = target.getFlux(insTargetBand);

                if (flux == null) {
                    missingMags.add(insTargetBand);
                    objectMag[i] = Double.NaN;
                } else {
                    objectMag[i] = flux.doubleValue();
                }
                if (logger.isDebugEnabled()) {
                    logger.debug("objectMag                     : {}", flux);
                }
            }
        }

        logger.debug("usedBands                     : {}", usedInsBands);

        if (this.prepareVisRelBias) {
            // ObjectMag in spectral channels is known:

            // initialize vectors:
            for (int i = 0; i < nSpectralChannels; i++) {
                final double mag = objectMag[i];
                final double flux_density = insBand[i].magToJy(mag); // Jy

                // relative error on photometry:
                final double rel_e_phot = this.instrumentalPhotRelBias[i] / flux_density;

                // (dV2 / V2) = 2 (dv / v)
                final double rel_e_v2 = 2.0 * this.instrumentalVisRelBias[i];
                final double rel_v2_error = Math.sqrt(rel_e_v2 * rel_e_v2 + 2.0 * rel_e_phot * rel_e_phot); // VIS2 implies 2 photometries
                final double rel_e_v = rel_v2_error / 2.0; // 1/2 error

                this.instrumentalPhotRelBias[i] = rel_e_phot;
                this.instrumentalVisRelBias[i] = rel_e_v;
                this.instrumentalVis2RelBias[i] = rel_v2_error;

            } // channels

            if (logger.isDebugEnabled()) {
                logger.debug("instrumentalPhotRelBias:  {}", Arrays.toString(instrumentalPhotRelBias));  // e_phot_jy
                logger.debug("instrumentalVisRelBias:   {}", Arrays.toString(instrumentalVisRelBias));   // rel
                logger.debug("instrumentalVis2RelBias:  {}", Arrays.toString(instrumentalVis2RelBias));   // rel
            }
        }

        final Target ftTarget = (targetRole == TargetRole.SCI) ? targetMapping.get(TargetRole.FT) : target;

        Target aoTarget = targetMapping.get(TargetRole.AO);
        if (aoTarget == null) {
            aoTarget = target;
        }
        logger.debug("ftTarget                      : {}", ftTarget);
        logger.debug("aoTarget                      : {}", aoTarget);

        if (fringeTrackerPresent) {
            flux = ftTarget.getFlux(ftBand);

            if (flux == null) {
                if (ftTarget == target) {
                    missingMags.add(ftBand);
                } else {
                    warningContainer.addWarning(AsproConstants.WARN_MISSING_MAGS + " on FT target [" + ftTarget.getName() + "] "
                            + "in band " + ftBand);
                }
                fringeTrackerMag = Double.NaN;
            } else {
                fringeTrackerMag = flux.doubleValue();
                if (ftTarget != target) {
                    this.distSCI_FT = TargetUtils.computeDistanceInDegrees(target, ftTarget);
                    final FitsUnit axisUnit = FitsUnit.ANGLE_ARCSEC;
                    warningContainer.addInformation("FT associated to target [" + ftTarget.getName() + "] ("
                            + ftBand + '=' + df.format(fringeTrackerMag) + " mag, "
                            + "dist: " + NumberUtils.trimTo3Digits(FitsUnit.ANGLE_DEG.convert(distSCI_FT, axisUnit))
                            + " " + axisUnit.getStandardRepresentation() + ")");
                }
            }
            if (logger.isDebugEnabled()) {
                logger.debug("fringeTrackerMag              : {}", fringeTrackerMag);
            }
        }

        SpectralBand fluxAOBand = aoBand;

        // Handle missing target magnitudes:
        if (fluxAOBand == SpectralBand.G) {
            // handle special case for G band (NAOMI): use G then R then V (if no flux)
            flux = aoTarget.getFlux(fluxAOBand);

            if (flux == null) {
                fluxAOBand = SpectralBand.R;
                flux = aoTarget.getFlux(fluxAOBand);
            }
            if (flux == null) {
                fluxAOBand = SpectralBand.V;
                flux = aoTarget.getFlux(fluxAOBand);
            }
        } else if (fluxAOBand == SpectralBand.G_RP) {
            // handle special case for G band (GPAO): use G_rp then R then G then V (if no flux)
            flux = aoTarget.getFlux(fluxAOBand);

            if (flux == null) {
                fluxAOBand = SpectralBand.R;
                flux = aoTarget.getFlux(fluxAOBand);
            }
            if (flux == null) {
                fluxAOBand = SpectralBand.G;
                flux = aoTarget.getFlux(fluxAOBand);
            }
            if (flux == null) {
                fluxAOBand = SpectralBand.V;
                flux = aoTarget.getFlux(fluxAOBand);
            }
        } else {
            flux = aoTarget.getFlux(fluxAOBand);
        }

        if (flux == null) {
            if (aoTarget == target) {
                missingMags.add(aoBand);
            } else {
                warningContainer.addWarning(AsproConstants.WARN_MISSING_MAGS + " on AO target [" + aoTarget.getName() + "] "
                        + "in band " + aoBand);
            }
            adaptiveOpticsMag = Double.NaN;
        } else {
            adaptiveOpticsMag = flux.doubleValue();

            // check AO mag limits:
            if (!Double.isNaN(adaptiveOpticsLimit) && (adaptiveOpticsMag > adaptiveOpticsLimit)) {
                // invalid setup:
                this.invalidParametersInit = true;
                warningContainer.addWarning("Observation can not use AO (magnitude limit = " + adaptiveOpticsLimit + ") in " + aoBand + " band");
            } else {
                if (this.aoSetup != null) {
                    warningContainer.addInformation("AO setup: " + aoSetup.getName() + " in " + aoBand + " band ("
                            + fluxAOBand + '=' + df.format(adaptiveOpticsMag) + " mag)");
                }
                if (aoTarget != target) {
                    this.distAO = TargetUtils.computeDistanceInDegrees(target, aoTarget);
                    final FitsUnit axisUnit = FitsUnit.ANGLE_ARCSEC;
                    warningContainer.addInformation("AO associated to target [" + aoTarget.getName() + "] ("
                            + fluxAOBand + '=' + df.format(adaptiveOpticsMag) + " mag, "
                            + "dist: " + NumberUtils.trimTo3Digits(FitsUnit.ANGLE_DEG.convert(distAO, axisUnit))
                            + " " + axisUnit.getStandardRepresentation() + ")");
                }
            }
        }
        if (logger.isDebugEnabled()) {
            logger.debug("fluxAOBand                    : {}", fluxAOBand);
            logger.debug("adaptiveOpticsMag             : {}", adaptiveOpticsMag);
        }

        if (!missingMags.isEmpty()) {
            // invalid setup:
            this.invalidParametersInit = true;

            final ArrayList<SpectralBand> mags = new ArrayList<SpectralBand>(missingMags);
            Collections.sort(mags);

            warningContainer.addWarning(AsproConstants.WARN_MISSING_MAGS + " on target [" + target.getName() + "] "
                    + "in following bands: " + mags.toString());
        }
    }

    /**
     * Initialise other parameters
     * @param warningContainer to store warning & information during initialization
     * @param targetMapping target mappings
     * @param targetRole target role of this observation
     * @param isFTValid true if FT observation is valid at initial state; false otherwise
     */
    private void initParameters(final WarningContainer warningContainer,
                                final Map<TargetRole, Target> targetMapping, final TargetRole targetRole,
                                final boolean isFTValid) {

        // set invalid flag to initial state:
        this.invalidParameters = invalidParametersInit;
        // fast return if invalid configuration :
        if (invalidParameters) {
            return;
        }
        final int nObs = nPoints; // = targetPointInfos.length
        final int nWLen = nSpectralChannels;

        // Pass 1: find maximum flux per channel taking into account the target elevation:
        // strehl is spectrally dependent:
        final double[][] strehlPerChannel;

        if (useStrehlCorrection) {
            strehlPerChannel = new double[nObs][];

            if (this.instrumentName.equals(AsproConstants.INS_SPICA)) {
                // SPICA case (waiting for CHARA AO):
                final double strehl;
                if (seeing <= 0.7) {
                    strehl = 0.25;
                } else if (seeing <= 1.0) {
                    strehl = 0.15;
                } else {
                    strehl = 0.1;
                }
                warningContainer.addTrace("Strehl (SPICA): " + NumberUtils.format(strehl));

                for (int n = 0; n < nObs; n++) {
                    strehlPerChannel[n] = new double[waveLengths.length];
                    Arrays.fill(strehlPerChannel[n], strehl);
                }
            } else {
                Band band = Band.V;
                int nbSubPupils = 1;
                int nbActuators = 1;
                double ao_td = 1.0;
                double ao_ron = 1.0;
                double ao_qe = 0.9;
                double magOffset = 0.0;
                double strehlMax = 0.0;
                boolean isGPAO_VIS = false;
                boolean isGPAO_LGS = false;
                double distLGS = 0.0;

                if (aoSetup != null) {
                    band = Band.valueOf(aoBand.name());
                    nbSubPupils = aoSetup.getNumberSubPupils();
                    if (aoSetup.getNumberActuators() != null) {
                        nbActuators = aoSetup.getNumberActuators();
                    } else {
                        nbActuators = nbSubPupils;
                    }
                    ao_td = aoSetup.getDit();
                    ao_ron = aoSetup.getRon();
                    ao_qe = aoSetup.getQuantumEfficiency();
                    magOffset = aoSetup.getMagOffsetOrZero();
                    strehlMax = aoSetup.getStrehlMaxOrZero();

                    // fix transmission factors:
                    if (aoSetup.getTransmission() != null) {
                        // correct AO transmission through QE:
                        ao_qe *= aoSetup.getTransmission();

                        if (usedInsBands.contains(band)) {
                            // set the potential transmission loss on interferometric instrument:
                            aoInstrumentalTransmission = 1.0 - aoSetup.getTransmission();
                            if (logger.isDebugEnabled()) {
                                logger.debug("aoInstrumentalTransmission      : {}", aoInstrumentalTransmission);
                            }
                        }
                    }

                    isGPAO_VIS = targetMapping.get(TargetRole.GPAO_VIS) != null;

                    final Target lgsTarget = targetMapping.get(TargetRole.GPAO_LGS);

                    if (lgsTarget != null) {
                        isGPAO_LGS = true;
                        final Target target = targetMapping.get(targetRole);

                        // add margin ~ 0.1 as ?
                        distLGS = (lgsTarget != target) ? TargetUtils.computeDistanceInDegrees(target, lgsTarget) : 0.0;
                        logger.debug("distLGS                         : {}", ALX.DEG_IN_ARCSEC * distLGS);

                        final FitsUnit axisUnit = FitsUnit.ANGLE_ARCSEC;
                        warningContainer.addInformation("AO LGS set close to target [" + lgsTarget.getName() + "] ("
                                + "dist: " + NumberUtils.trimTo3Digits(FitsUnit.ANGLE_DEG.convert(distLGS, axisUnit))
                                + " " + axisUnit.getStandardRepresentation() + ")");
                    }
                }

                for (int n = 0; n < nObs; n++) {
                    final double elevation = targetPointInfos[n].getElevation();

                    strehlPerChannel[n] = Band.strehl(band, (adaptiveOpticsMag + magOffset), waveLengths, telDiam,
                            seeing, nbSubPupils, nbActuators, ao_td, t0, ao_qe, ao_ron, elevation, strehlMax);

                    if (logger.isDebugEnabled()) {
                        logger.debug("elevation                     : {}", elevation);
                        logger.debug("strehlPerChannel[iRefChannel] : {}", strehlPerChannel[n][iRefChannel]);
                        logger.debug("strehlPerChannel              : {}", Arrays.toString(strehlPerChannel[n]));
                    }

                    if (DO_DUMP_STREHL) {
                        System.out.println("strehl table for elevation=" + elevation + " seeing=" + seeing);
                        System.out.println("channel\twaveLength\tstrehl");
                        for (int i = 0; i < waveLengths.length; i++) {
                            System.out.println(i + "\t" + waveLengths[i] + "\t" + strehlPerChannel[n][i]);
                        }
                    }

                    if (distAO > 0.0) {
                        final double distAs_LGS = ALX.DEG_IN_ARCSEC * distLGS;
                        final double distAs_NGS = ALX.DEG_IN_ARCSEC * distAO;
                        final double[] strehlIso;

                        if (isGPAO_LGS) {
                            strehlIso = Band.strehl_iso_LGS(band, isGPAO_VIS, waveLengths, seeing, h0, elevation, distAs_LGS, distAs_NGS);
                        } else {
                            strehlIso = Band.strehl_iso_NGS(band, isGPAO_VIS, waveLengths, seeing, h0, elevation, distAs_NGS);
                        }

                        if (logger.isDebugEnabled()) {
                            logger.debug("distLGS                       : {} as", distAs_LGS);
                            logger.debug("distAO                        : {} as", distAs_NGS);
                            logger.debug("strehlIso                     : {} as", Arrays.toString(strehlIso));
                        }

                        if (strehlIso != null) {
                            for (int l = 0; l < nWLen; l++) {
                                strehlPerChannel[n][l] *= strehlIso[l];
                            }
                        }
                    }
                    if (USE_FIXED_STREHL) {
                        for (int l = 0; l < nWLen; l++) {
                            strehlPerChannel[n][l] = FIXED_STREHL;
                        }
                    }
                }
            }
            // Stats:
            for (int n = 0; n < nObs; n++) {
                for (int l = 0; l < nWLen; l++) {
                    strehlStats.add(strehlPerChannel[n][l]);
                }
            }
        } else {
            strehlPerChannel = null;
        }

        // total number of thermal photons per spectral channel per second per telescope:
        if (this.nbPhotThermal == null) {
            this.nbTotalPhotTherm = null;
        } else {
            this.nbTotalPhotTherm = new double[nWLen];

            for (int i = 0; i < nWLen; i++) {
                // Per beam Per second:
                nbTotalPhotTherm[i] = Math.max(0.0, this.nbPhotThermal[i] * quantumEfficiency);
            }
        }

        // Target flux per spectral channel per second per m^2:
        final double[] fluxSrcPerChannel = computeTargetFlux();
        // compute transmission on spectral channels:
        final double[] atmTrans = computeAtmosphereTransmission();

        // Stats:
        for (int l = 0; l < nWLen; l++) {
            atmTransStats.add(atmTrans[l]);
        }

        // Transmission factor:
        double transmissionFactor = quantumEfficiency * delayLineTransmission;

        // Handle GRAVITY single/dual field mode:
        if (targetMapping.get(TargetRole.SINGLE_FIELD) != null) {
            // GRAVITY single-field: 50% / FT 50% SCI
            transmissionFactor *= 0.5;
            if (logger.isDebugEnabled()) {
                logger.debug("{} : SINGLE_FIELD", targetRole);
            }
        }

        // Include transmission losses by AO:
        if (!Double.isNaN(aoInstrumentalTransmission)) {
            // correct instrumental flux due to AO flux loss:
            transmissionFactor *= aoInstrumentalTransmission;
        }

        if (logger.isDebugEnabled()) {
            logger.debug("transmissionFactor: {}", transmissionFactor);
        }

        // total number of science photons per spectral channel per second per telescope:
        this.nbTotalObjPhot = new double[nObs][nWLen];
        this.nbTotalPhot = new double[nObs][nWLen];

        final double telSurface = Math.PI * FastMath.pow2(0.5 * telDiam);

        // maximum number of (science + thermal) photons per pixel in the interferometric channel and per second:
        double maxNbAllPhotInterfPerPixPerSec = 0.0;

        for (int n = 0; n < nObs; n++) {

            for (int i = 0; i < nWLen; i++) {
                // Per second:
                double nbPhot = telSurface * fluxSrcPerChannel[i];
                nbTotalObjPhot[n][i] = nbPhot;

                // include any transmission loss:
                nbPhot *= transmission[i] * transmissionFactor;

                if (DO_ATM_STREHL) {
                    nbPhot *= atmTrans[i];

                    if (strehlPerChannel != null) {
                        nbPhot *= strehlPerChannel[n][i];
                    }
                }
                nbTotalPhot[n][i] = nbPhot;

                if (nbTotalPhotTherm != null) {
                    // add thermal photons per sec:
                    nbPhot += nbTotalPhotTherm[i];
                }

                // Per pixel in the interferometric channel for the spectral channel:
                nbPhot /= nbPixInterf[i];

                if (nbPhot > maxNbAllPhotInterfPerPixPerSec) {
                    maxNbAllPhotInterfPerPixPerSec = nbPhot;
                }
            }
            if (logger.isDebugEnabled()) {
                logger.debug("elevation                     : {}", targetPointInfos[n].getElevation());
                logger.debug("nbTotalPhotPerSec             : {}", Arrays.toString(nbTotalPhot[n]));
                logger.debug("nbTotalPhotPerSec[iRefChannel]: {}", nbTotalPhot[n][iRefChannel]);
            }
        }

        if (logger.isDebugEnabled()) {
            logger.debug("maxNbAllPhotInterfPerPixPerSec: {}", maxNbAllPhotInterfPerPixPerSec);
        }

        // maximum number of expected photoevents (for all telescopes) per pixel per second:
        final double maxTotalPhotPerPixPerSec = (nbTel * maxNbAllPhotInterfPerPixPerSec); // include transmission losses ?

        // fraction of total interferometric flux in the peak pixel per second:
        final double peakFluxPixPerSec = fracFluxInInterferometry * maxTotalPhotPerPixPerSec;

        // max dit before saturation (s):
        this.maxObsDit = detectorSaturation / peakFluxPixPerSec;

        if (logger.isDebugEnabled()) {
            logger.debug("maxTotalPhotPerPixPerSec        : {}", maxTotalPhotPerPixPerSec);
            logger.debug("peakFluxPixPerSec               : {}", peakFluxPixPerSec);
            logger.debug("maxObsDit                       : {}", maxObsDit);
        }

        // Adjust obsDit if possible (classical automatical dit handling):
        // set dit used by this observation to instrument's dit:
        double obsDit = this.dit;
        final int nbFrameToSaturation;

        if (obsDit > maxObsDit) {
            final double badDit = obsDit;

            // the dit is too long, use max dit:
            obsDit = maxObsDit;
            nbFrameToSaturation = 0;

            // if GRAVITY_FT or GRAVITY:
            if (this.instrumentName.startsWith(AsproConstants.INS_GRAVITY)) {
                // invalid setup:
                this.invalidParametersInit = true;
                warningContainer.addWarning("DIT too long (saturation): " + formatTime(badDit) + ". Impossible to use DIT = " + formatTime(obsDit) + " !");
            } else {
                warningContainer.addWarning("DIT too long (saturation). Adjusting it to (possibly impossible): " + formatTime(obsDit));
            }
        } else {
            // fraction of total interferometric flux in the peak pixel :
            final double peakFluxPix = peakFluxPixPerSec * obsDit;

            if (logger.isDebugEnabled()) {
                logger.debug("peakfluxPix                   : {}", peakFluxPix);
            }
            nbFrameToSaturation = (int) Math.floor(detectorSaturation / peakFluxPix);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("dit (no saturation)           : {}", obsDit);
            logger.debug("nbFrameToSaturation           : {}", nbFrameToSaturation);
        }

        // Adjust instrumental visibility:
        this.vinst = instrumentalVisibility;

        if (fringeTrackerPresent) {
            if (isFTValid && (fringeTrackerMag <= fringeTrackerLimit) && (nbFrameToSaturation >= 1)) {
                // correct instrumental visibility due to FT flux loss:
                for (int i = 0; i < this.vinst.length; i++) {
                    this.vinst[i] *= fringeTrackerInstrumentalVisibility; // FINITO only
                }
                if (logger.isDebugEnabled()) {
                    logger.debug("vinst[iRefChannel]            : {}", vinst[iRefChannel]);
                    logger.debug("vinst (FT)                    : {}", Arrays.toString(vinst));
                }

                if (fringeTrackingMode) {
                    // FT is asked, can work, and is useful (need to integrate longer)

                    if (this.instrumentName.equals(AsproConstants.INS_GRAVITY)) {
                        double maxIdealDit = Math.min(maxObsDit, totalObsTime);
                        maxIdealDit = Math.min(maxIdealDit, fringeTrackerMaxDit);

                        // GRAVITY SCI: allowed DIT:
                        // (300s) only offered in visitor mode: 300.0
                        final double[] dits = new double[]{0.3, 1.0, 3.0, 10.0, 30.0, 100.0}; // TODO: put in configuration 24.3?

                        int pos = -1;
                        for (int j = 0; j < dits.length; j++) {
                            if (NumberUtils.greaterThan(maxIdealDit, dits[j])) {
                                pos = j;
                            } else {
                                break;
                            }
                        }
                        if (pos != -1) {
                            // Found a compatible dit:
                            obsDit = dits[pos];
                        }
                    } else {
                        obsDit = Math.min(obsDit * nbFrameToSaturation, totalObsTime);
                        obsDit = Math.min(obsDit, fringeTrackerMaxDit);
                    }

                    // Fix frame ratio:
                    this.frameRatio = fringeTrackerMaxFrameTime / fringeTrackerMaxDit;

                    if (logger.isDebugEnabled()) {
                        logger.debug("dit (SCI with FT)             : {}", obsDit);
                        logger.debug("frameRatio                    : {}", frameRatio);
                    }

                    warningContainer.addInformation("Observation can use FT. Adjusting DIT to: " + formatTime(obsDit));
                } else {
                    warningContainer.addInformation("Observation can use FT (Group track). DIT set to: " + formatTime(obsDit));
                }
            } else if (!invalidParametersInit) {
                // invalid setup:
                this.invalidParametersInit = true;

                if (!isFTValid) {
                    warningContainer.addWarning("Observation can not use FT !");
                } else if (nbFrameToSaturation < 1) {
                    warningContainer.addWarning("Observation can not use FT (saturation) !");
                } else if (fringeTrackerMag > fringeTrackerLimit) {
                    warningContainer.addWarning("Observation can not use FT (" + ftBand + " magnitude limit = " + fringeTrackerLimit + ") !");
                }

                if (logger.isDebugEnabled()) {
                    logger.debug("vinst[iRefChannel]              : {}", vinst[iRefChannel]);
                    logger.debug("vinst (noFT)                    : {}", Arrays.toString(vinst));
                }
            }
        } else {
            if ((targetRole == TargetRole.SCI) && !AsproConstants.INS_GRAVITY_FT.equals(this.instrumentName)) {
                warningContainer.addInformation("DIT set to: " + formatTime(obsDit));
            }
        }

        // Allocate arrays and noise parameters:
        this.nbPhotThermInterf = new double[nWLen];
        this.nbPhotThermPhoto = (usePhotometry) ? new double[nWLen] : null;

        // note: params are defined later in initDetectorParameters() and prepareParameters():
        // (at runtime when model flux is known)
        this.params = new NoiseWParams[nObs];
        for (int n = 0; n < nObs; n++) {
            this.params[n] = new NoiseWParams(nWLen);
        }

        // initialize parameters related to detector integration time (dit):
        initDetectorParameters(obsDit);

        // ensure prepare parameters (no correction) to finalize initialization (UVMap use case):
        prepareParameters(null, null, null, false, null);
    }

    public void initDetectorParameters(final double obsDit) {
        // set invalid flag to initial state:
        this.invalidParameters = invalidParametersInit;
        // fast return if invalid configuration :
        if (invalidParameters) {
            return;
        }
        if (logger.isDebugEnabled()) {
            logger.debug("initDetectorParameters({}): obs dit = {} s", instrumentName, obsDit);
        }

        // always check saturation:
        if (obsDit > maxObsDit) {
            if (logger.isDebugEnabled()) {
                logger.debug("DIT too long (saturation): {}. Impossible to use DIT = {} !", formatTime(obsDit), formatTime(maxObsDit));
            }
            // invalid setup:
            this.invalidParameters = true;
            return;
        }

        final int nObs = nPoints;
        final int nWLen = nSpectralChannels;

        // Set final dit:
        this.obsUsedDit = obsDit;

        // total number of frames:
        final double nbFrames = totalObsTime / obsDit;
        final double nbFramesWithOverheads = nbFrames / frameRatio;

        // total frame correction = 1 / SQRT(nFrames):
        this.totFrameCorrection = 1.0 / Math.sqrt(nbFramesWithOverheads);
        this.totFrameCorrectionPhot = (usePhotometry) ? 1.0 / Math.sqrt(nbFramesWithOverheads * ratioPhotoVsInterfero) : Double.NaN;

        if (logger.isDebugEnabled()) {
            logger.debug("nbFrames                      : {}", nbFrames);
            logger.debug("nbFramesWithOverheads         : {}", nbFramesWithOverheads);
            logger.debug("totFrameCorrection            : {}", totFrameCorrection);
            logger.debug("totFrameCorrectionPhot        : {}", totFrameCorrectionPhot);
        }

        // Gravity background & read-out noise model:
        if ((coeff_A_bkg != null) || (coeff_B_bkg != null)) {
            // compute read-out noise:
            // ron = A_bkg / SQRT(dit) + B_bkg
            final double dit_inv_sqrt = 1.0 / Math.sqrt(obsDit);

            for (int i = 0; i < nWLen; i++) {
                final double a = (coeff_A_bkg != null) ? coeff_A_bkg[i] : 0.0;
                final double b = (coeff_B_bkg != null) ? coeff_B_bkg[i] : 0.0;
                this.ron[i] = a * dit_inv_sqrt + b;

                // fix bad case in LOW resolution:
                if (ron[i] < 1.0) {
                    ron[i] = MIN_RON_GRAVITY_LOW; // 3.5e-
                }
                if (logger.isDebugEnabled()) {
                    logger.debug("ron({}): {}", waveLengths[i], ron[i]);
                }
            }
        }

        if (nbTotalPhotTherm != null) {
            for (int i = 0; i < nWLen; i++) {
                // corrected total number of thermal photons using the final observation dit per telescope:
                final double nbPhot = nbTotalPhotTherm[i] * obsDit;

                // number of thermal photons in the interferometric channel (per telescope):
                nbPhotThermInterf[i] = nbPhot * fracFluxInInterferometry;
                // number of thermal photons in each photometric channel (photometric flux):
                if (nbPhotThermPhoto != null) {
                    nbPhotThermPhoto[i] = nbPhot * fracFluxInPhotometry;
                }
            }

            if (logger.isDebugEnabled()) {
                logger.debug("nbPhotThermInterf[iRefChannel]: {}", nbPhotThermInterf[iRefChannel]);
                logger.debug("nbPhotThermInterf         : {}", Arrays.toString(nbPhotThermInterf));
                if (nbPhotThermPhoto != null) {
                    logger.debug("nbPhotThermPhoto[iRefChannel]: {}", nbPhotThermPhoto[iRefChannel]);
                    logger.debug("nbPhotThermPhoto          : {}", Arrays.toString(nbPhotThermPhoto));
                }
            }
        }

        // note: params are finalized later in prepareParameters()
        // (at runtime when model flux is known)
        for (int n = 0; n < nObs; n++) {
            final NoiseWParams param = this.params[n];

            // define parameters using dit:
            final double[] nbPhotObjPhoto = param.nbPhotObjPhoto;
            final double[] nbPhotInterf = param.nbPhotInterf;
            final double[] nbPhotPhoto = param.nbPhotPhoto;

            for (int i = 0; i < nWLen; i++) {
                // corrected total number of photons using the final observation dit per telescope:
                nbPhotObjPhoto[i] = nbTotalObjPhot[n][i] * obsDit;
                final double nbPhot = nbTotalPhot[n][i] * obsDit;

                // number of photons in the interferometric channel (per telescope):
                nbPhotInterf[i] = nbPhot * fracFluxInInterferometry;
                // number of photons in each photometric channel (photometric flux):
                nbPhotPhoto[i] = nbPhot * fracFluxInPhotometry;
            }

            if (logger.isDebugEnabled()) {
                logger.debug("nbPhotObjPhoto                : {}", Arrays.toString(nbPhotObjPhoto));
                logger.debug("nbPhotObjPhoto[iRefChannel]   : {}", nbPhotObjPhoto[iRefChannel]);
                logger.debug("nbTotalPhot[iRefChannel]      : {}", nbTotalPhot[n][iRefChannel]);
                logger.debug("nbTotalPhot                   : {}", Arrays.toString(nbTotalPhot[n]));
                logger.debug("nbPhotonInI[iRefChannel]      : {}", nbPhotInterf[iRefChannel]);
                logger.debug("nbPhotonInI                   : {}", Arrays.toString(nbPhotInterf));
                logger.debug("nbPhotonInP[iRefChannel]      : {}", nbPhotPhoto[iRefChannel]);
                logger.debug("nbPhotonInP                   : {}", Arrays.toString(nbPhotPhoto));
            }
        }
    }

    /**
     * Prepare final parameters at runtime
     * @param insBands instrumental band per channel
     * @param modelFlux user model's flux per channel (interpolated)
     * @param bandFluxes user model's mean flux per band
     * @param doWarnings do log warning messages
     * @param prefix prefix for warning messages
     */
    public void prepareParameters(final Band[] insBands, final double[] modelFlux, final Map<Band, Double> bandFluxes,
                                  final boolean doWarnings, final String prefix) {

        // fast return if invalid configuration :
        if (invalidParameters) {
            return;
        }

        if (doWarnings) {
            if (strehlStats.isSet()) {
                warningContainerCompute.addTrace(prefix + ": Strehl " + strehlStats.toString(false));
            }
            if (atmTransStats.isSet() && logger.isDebugEnabled()) {
                logger.debug("Atm. trans: {}", atmTransStats);
            }
        }

        final int nObs = nPoints;

        if ((insBands != null) && !bandFluxes.isEmpty()) {
            if (logger.isDebugEnabled()) {
                logger.debug("modelFlux:  {}", Arrays.toString(modelFlux));
                logger.debug("bandFluxes: {}", bandFluxes);
            }

            final int nWLen = nSpectralChannels;

            // Compute correction weights per wavelength:
            final double[] weights = new double[nWLen];

            // Iterate on spectral channels:
            for (int l = 0; l < nWLen; l++) {
                final Band band = insBands[l];

                final Double totalFluxBand = bandFluxes.get(band);
                if ((totalFluxBand != null) && (totalFluxBand > 0.0)) {
                    weights[l] = modelFlux[l] / totalFluxBand;
                } else {
                    weights[l] = 1.0;
                }

                if (logger.isDebugEnabled()) {
                    logger.debug("modelFlux[{} - {}] = {} - weight: {}", convertWL(waveLengths[l]), band, modelFlux[l], weights[l]);
                }
            }

            if (logger.isDebugEnabled()) {
                logger.debug("weights: {}", Arrays.toString(weights));
            }

            // Adjust nbPhot (nbPhotInterf & nbPhotPhoto) with flux weights:
            for (int n = 0; n < nObs; n++) {
                final NoiseWParams param = this.params[n];

                final double[] nbPhotObjPhoto = param.nbPhotObjPhoto;
                final double[] nbPhotInterf = param.nbPhotInterf;
                final double[] nbPhotPhoto = param.nbPhotPhoto;

                for (int l = 0; l < nWLen; l++) {
                    // corrected total number of photons using the model fluxes:
                    nbPhotObjPhoto[l] *= weights[l];

                    // number of photons in the interferometric channel (per telescope):
                    nbPhotInterf[l] *= weights[l];
                    // number of photons in each photometric channel (photometric flux):
                    nbPhotPhoto[l] *= weights[l];
                }

                if (logger.isDebugEnabled()) {
                    logger.debug("prepareParameters:");
                    logger.debug("nbPhotonInI[iRefChannel]      : {}", nbPhotInterf[iRefChannel]);
                    logger.debug("nbPhotonInI                   : {}", Arrays.toString(nbPhotInterf));
                    logger.debug("nbPhotonInP[iRefChannel]      : {}", nbPhotPhoto[iRefChannel]);
                    logger.debug("nbPhotonInP                   : {}", Arrays.toString(nbPhotPhoto));
                    logger.debug("nbPhotObjPhoto                 : {}", Arrays.toString(nbPhotObjPhoto));
                }
            }
        }

        // Finally prepare numeric constants for fast error computations:
        for (int n = 0; n < nObs; n++) {
            prepareVis2Error(n);
            if (!DEBUG) {
                prepareT3PhiError(n);
            }
        }
        if (DO_DUMP_VIS2) {
            dumpVis2Error(0);
            dumpVis2Error(iRefChannel);
            dumpVis2Error(nSpectralChannels - 1);
        }
    }

    double[] computeTargetFlux() {
        final int nWLen = nSpectralChannels;
        // nb photons per surface and per second:
        final double[] fluxSrcPerChannel = new double[nWLen];

        for (int i = 0; i < nWLen; i++) {
            // nb of photons m^-2.s^-1.m^-1 for an object at magnitude 0:
            // note: fzero depends on the spectral band:
            // consider flat profile:
            final double fzero = insBand[i].getNbPhotZero();

            // TODO: source flux may be spectrally dependent i.e. use 1 or several black body profiles ?
            // nb of photons m^-2.s^-1.m^-1 for the target object:
            final double fsrc = fzero * FastMath.pow(10d, -0.4d * objectMag[i]);

            if (logger.isDebugEnabled()) {
                logger.debug("insBand                       : {}", insBand[i]);
                logger.debug("objectMag                     : {}", objectMag[i]);
                logger.debug("fzero                         : {}", fzero);
                logger.debug("fsrc                          : {}", fsrc);
            }

            // nb of photons m^-2.s^-1 for the target object:
            fluxSrcPerChannel[i] = fsrc * waveBands[i];
        }
        if (logger.isDebugEnabled()) {
            logger.debug("fluxSrcPerChannel[iRefChannel]: {}", fluxSrcPerChannel[iRefChannel]);
            logger.debug("fluxSrcPerChannel             : {}", Arrays.toString(fluxSrcPerChannel));
        }
        return fluxSrcPerChannel;
    }

    double[] computeAtmosphereTransmission() {
        final double[] atmTrans = AtmosphereSpectrumService.getInstance().getTransmission(this.waveLengths, this.waveBands);

        if (logger.isDebugEnabled()) {
            logger.debug("atmTrans[iRefChannel]         : {}", atmTrans[iRefChannel]);
            logger.debug("atmTrans                      : {}", Arrays.toString(atmTrans));
        }

        if (DO_DUMP_ATM_TRANS) {
            System.out.println("AtmTransmission[" + instrumentName + "] [" + this.waveLengths[0] + " - "
                    + this.waveLengths[this.waveLengths.length - 1] + "]:");
            System.out.println("channel\twaveLength\tatm_trans");
            for (int i = 0; i < waveLengths.length; i++) {
                System.out.println(i + "\t" + waveLengths[i] + "\t" + atmTrans[i]);
            }
        }
        return atmTrans;
    }

    public boolean isUseBias() {
        return useBias;
    }

    public boolean isUseRandomCalBias() {
        return useRandomCalBias;
    }

    public int getIndexRefChannel() {
        return iRefChannel;
    }

    public int getIndexMidPoint() {
        return iMidPoint;
    }

    public Band[] getInstrumentBands() {
        return this.insBand;
    }

    public Set<Band> getDistinctInstrumentBands() {
        return this.usedInsBands;
    }

    /**
     * Return the correlated flux weight of the object (without visibility). 
     * It returns NaN if the flux can not be computed
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @return correlated flux or NaN if the flux can not be computed
     */
    public double getCorrelatedFluxWeight(final int iPoint, final int iChannel) {
        if (check(iPoint, iChannel)) {
            return Double.NaN;
        }
        // correlated flux (include instrumental visibility loss) (1T):
        return this.params[iPoint].nbPhotInterf[iChannel] * vinst[iChannel];
    }

    /**
     * Return the number of photons in interferometer channel per telescope
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @return number of photons in interferometer channel per telescope
     */
    public double getNbPhotInterf(final int iPoint, final int iChannel) {
        if (check(iPoint, iChannel)) {
            return Double.NaN;
        }
        return this.params[iPoint].nbPhotInterf[iChannel];
    }

    /**
     * Return the number of object photons in each photometric channel (photometric flux)
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @return number of photons in each photometric channel
     */
    public double getNbPhotObjPhoto(final int iPoint, final int iChannel) {
        if (check(iPoint, iChannel)) {
            return Double.NaN;
        }
        return this.params[iPoint].nbPhotObjPhoto[iChannel];
    }

    /**
     * Return the number of photons in each photometric channel (photometric flux)
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @return number of photons in each photometric channel
     */
    public double getNbPhotPhoto(final int iPoint, final int iChannel) {
        if (check(iPoint, iChannel)) {
            return Double.NaN;
        }
        return this.params[iPoint].nbPhotPhoto[iChannel];
    }

    /**
     * Return the error on the number of photons in each photometric channel
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @return error on the number of photons in each photometric channel
     */
    public double getErrorPhotPhoto(final int iPoint, final int iChannel) {
        if (check(iPoint, iChannel)) {
            return Double.NaN;
        }
        return this.params[iPoint].errPhotPhoto[iChannel];
    }

    /**
     * Return the squared correlated flux
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @return squared correlated flux
     */
    public double getSqCorrFlux(final int iPoint, final int iChannel) {
        if (check(iPoint, iChannel)) {
            return Double.NaN;
        }
        return this.params[iPoint].sqCorrFlux[iChannel];
    }

    /**
     * Return the error on squared correlated flux
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @return error on squared correlated flux
     */
    public double getErrorSqCorrFlux(final int iPoint, final int iChannel) {
        if (check(iPoint, iChannel)) {
            return Double.NaN;
        }
        return this.params[iPoint].errSqCorrFlux[iChannel];
    }

    /**
     * Prepare numeric constants for square visibility error
     *
     * Note: this method is statefull and NOT thread safe
     *
     * @param iPoint index of the observable point
     */
    private void prepareVis2Error(final int iPoint) {
        final int nWLen = nSpectralChannels;

        final NoiseWParams param = this.params[iPoint];

        final double[] nbPhotInterf = param.nbPhotInterf;
        final double[] nbPhotPhoto = param.nbPhotPhoto;
        final double[] errPhotPhoto = param.errPhotPhoto;

        final double[] sqErrVis2Phot = param.sqErrVis2Phot;
        final double[] sqCorFluxCoef = param.sqCorFluxCoef;
        final double[] varSqCorFluxCoef = param.varSqCorFluxCoef;
        final double[] varSqCorFluxConst = param.varSqCorFluxConst;

        if (usePhotometry) {
            for (int i = 0; i < nWLen; i++) {
                // error on the photometric flux in photometric channel:
                errPhotPhoto[i] = Math.sqrt(nbPhotPhoto[i]
                        + ratioPhotoPerBeam * (nbPhotThermPhoto[i] + nbPixPhoto[i] * FastMath.pow2(ron[i])));

                // square error contribution of 2 photometric channels on the square visiblity FiFj:
                sqErrVis2Phot[i] = 2.0 * FastMath.pow2(errPhotPhoto[i] / nbPhotPhoto[i]);

                // repeat OBS measurements to reach totalObsTime minutes (corrected by photo / interfero ratio):
                errPhotPhoto[i] *= totFrameCorrectionPhot;
            }

            if (logger.isDebugEnabled()) {
                logger.debug("sqErrVis2Phot[iRefChannel]    : {}", sqErrVis2Phot[iRefChannel]);
                logger.debug("sqErrVis2Phot                 : {}", Arrays.toString(sqErrVis2Phot));
            }
        } else {
            Arrays.fill(sqErrVis2Phot, 0.0);
        }

        for (int i = 0; i < nWLen; i++) {
            // squared correlated flux (include instrumental visibility loss) for vis2 = 1.0:
            sqCorFluxCoef[i] = FastMath.pow2(nbPhotInterf[i] * vinst[i]);

            // total number of photons for all telescopes:
            final double nbTotPhot = nbTel * (nbPhotInterf[i] + nbPhotThermInterf[i]);

            // variance of the squared correlated flux = sqCorFlux * coef + constant
            varSqCorFluxCoef[i] = 2.0 * (nbTotPhot + nbPixInterf[i] * FastMath.pow2(ron[i])) + 4.0;

            varSqCorFluxConst[i] = FastMath.pow2(nbTotPhot)
                    + nbTotPhot * (1.0 + 2.0 * nbPixInterf[i] * FastMath.pow2(ron[i]))
                    + (3.0 + nbPixInterf[i]) * nbPixInterf[i] * FastMath.pow(ron[i], 4.0);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("elevation                     : {}", targetPointInfos[iPoint].getElevation());
            logger.debug("sqCorFluxCoef[iRefChannel]    : {}", sqCorFluxCoef[iRefChannel]);
            logger.debug("sqCorFluxCoef                 : {}", Arrays.toString(sqCorFluxCoef));
            logger.debug("varSqCorFluxCoef[iRefChannel] : {}", varSqCorFluxCoef[iRefChannel]);
            logger.debug("varSqCorFluxCoef              : {}", Arrays.toString(varSqCorFluxCoef));
            logger.debug("varSqCorFluxConst[iRefChannel]: {}", varSqCorFluxConst[iRefChannel]);
            logger.debug("varSqCorFluxConst             : {}", Arrays.toString(varSqCorFluxConst));
        }
    }

    /**
     * Compute error on square visibility. It returns NaN if the error can not be computed
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @param vis2 squared visibility
     * @param vis2Scale (visibility scale)^2 in [0.0 - 1.0] to take into account coherence loss
     * @param usePhot do use the photometric error
     * @return square visiblity error or NaN if the error can not be computed
     */
    public double computeVis2Error(final int iPoint, final int iChannel, final double vis2, final double vis2Scale, final boolean usePhot) {
        if (check(iPoint, iChannel)) {
            return Double.NaN;
        }

        final NoiseWParams param = this.params[iPoint];

        // squared correlated flux (include instrumental visibility loss):
        final double sqCorFlux = param.sqCorFluxCoef[iChannel] * vis2Scale * vis2;

        // variance of the squared correlated flux:
        // variance = sqCorFlux * coef + constant
        final double varSqCorFlux = sqCorFlux * param.varSqCorFluxCoef[iChannel] + param.varSqCorFluxConst[iChannel];

        // error of the squared correlated flux:
        param.sqCorrFlux[iChannel] = sqCorFlux;
        param.errSqCorrFlux[iChannel] = Math.sqrt(varSqCorFlux);

        // Uncertainty on square visibility per frame:
        double errVis2;
        if (usePhot && usePhotometry) {
            // repeat OBS measurements to reach totalObsTime minutes (corrected by photo / interfero ratio):
            errVis2 = vis2 * Math.sqrt(
                    (varSqCorFlux / FastMath.pow2(sqCorFlux)) * FastMath.pow2(totFrameCorrection)
                    + param.sqErrVis2Phot[iChannel] * FastMath.pow2(totFrameCorrectionPhot)
            );
        } else {
            // no photometry...
            errVis2 = vis2 * (param.errSqCorrFlux[iChannel] / param.sqCorrFlux[iChannel]);
            // repeat OBS measurements to reach totalObsTime minutes:
            errVis2 *= totFrameCorrection;
        }
        param.errSqCorrFlux[iChannel] *= totFrameCorrection;

        // Limit excessively large errors (very low transmission or strehl):
        errVis2 = Math.min(errVis2, MAX_ERR_V2);

        return errVis2;
    }

    private double computeVisErrorForMatisse(final int iPoint, final int iChannel, final double visAmp, final double visScale) {
        if (check(iPoint, iChannel)) {
            return Double.NaN;
        }

        final NoiseWParams param = this.params[iPoint];

        final double[] nbPhotInterf = param.nbPhotInterf;

        // MATISSE SNR(Fc) = (nI * Vinst * V) / SQRT( ntel * (nI + nth) + npixI * RON^2 )
        // correlated flux (include instrumental visibility loss) for vis = 1.0:
        final double corFlux = nbPhotInterf[iChannel] * vinst[iChannel] * visScale * visAmp;

        // total number of photons for all telescopes:
        final double nbTotPhot = nbTel * (nbPhotInterf[iChannel] + nbPhotThermInterf[iChannel]);

        // variance of the correlated flux:
        final double varCorFlux = /* 2.0 * */ (nbTotPhot + nbPixInterf[iChannel] * FastMath.pow2(ron[iChannel])) + 4.0; // x 1 or x 2 ?

        // Uncertainty on visibility per frame:
        double errVis = Math.sqrt(varCorFlux) / corFlux;
        // repeat OBS measurements to reach totalObsTime minutes:
        errVis *= totFrameCorrection;

        // Limit excessively large errors (very low transmission or strehl):
        errVis = Math.min(errVis, MAX_ERR_V);

        return errVis;
    }

    /**
     * Prepare numeric constants for closure phase error
     *
     * Note: this method is statefull and NOT thread safe
     *
     * @param iPoint index of the observable point
     */
    private void prepareT3PhiError(final int iPoint) {
        if (!DEBUG) {
            return;
        }
        final int nWLen = nSpectralChannels;

        final NoiseWParams param = this.params[iPoint];

        final double[] nbPhotInterf = param.nbPhotInterf;

        final double[] t3photCoef = param.t3photCoef;
        final double[] t3photCoef2 = param.t3photCoef2;
        final double[] t3photCoef3 = param.t3photCoef3;

        final double[] t3detConst = param.t3detConst;
        final double[] t3detCoef1 = param.t3detCoef1;
        final double[] t3detCoef2 = param.t3detCoef2;

        for (int i = 0; i < nWLen; i++) {
            final double invNbPhotonInIPerTel = 1.0 / nbPhotInterf[i];

            // photon noise on closure phase
            t3photCoef[i] = invNbPhotonInIPerTel;
            t3photCoef2[i] = FastMath.pow2(invNbPhotonInIPerTel);
            t3photCoef3[i] = FastMath.pow3(invNbPhotonInIPerTel);

            // detector noise on closure phase
            t3detConst[i] = FastMath.pow(invNbPhotonInIPerTel, 6d) * (FastMath.pow(nbPixInterf[i], 3d) * FastMath.pow(ron[i], 6d)
                    + 3 * FastMath.pow2(nbPixInterf[i]) * FastMath.pow(ron[i], 6d));

            t3detCoef1[i] = FastMath.pow(invNbPhotonInIPerTel, 4d) * (3d * nbPixInterf[i] * FastMath.pow(ron[i], 4d)
                    + FastMath.pow2(nbPixInterf[i]) * FastMath.pow(ron[i], 4d));

            t3detCoef2[i] = FastMath.pow2(invNbPhotonInIPerTel) * nbPixInterf[i] * FastMath.pow2(ron[i]);
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
        if (!DEBUG || check(iPoint, iChannel)) {
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
        final double v1 = visAmp12 * vinst[iChannel];
        final double v2 = visAmp23 * vinst[iChannel];
        final double v3 = visAmp31 * vinst[iChannel];

        double v123 = v1 * v2 * v3;

        // protect zero divide: why 1e-3 ?
        v123 = Math.max(v123, 1e-3);

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

        return sclosph;
    }

    /**
     * Return true if all parameters are valid i.e. returned errors are valid
     * @return true if all parameters are valid
     */
    public boolean isValid() {
        return !this.invalidParameters;
    }

    /**
     * Compute error on complex visibility derived from computeVis2Error(visAmp) by default.
     * It returns Double.NaN if the error can not be computed
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @param visAmp visibility amplitude
     * @param visScale visibility scale in [0.0 - 1.0] to take into account coherence loss
     * @param forAmplitude true to compute error for amplitudes (including the photometric error); false to compute error for phases
     * @return complex visiblity error or NaN if the error can not be computed
     */
    public double computeVisComplexErrorValue(final int iPoint, final int iChannel, final double visAmp, final double visScale, final boolean forAmplitude) {
        // visibility amplitude error (gaussian distribution):
        double visAmpErr = computeVisError(iPoint, iChannel, visAmp, visScale, forAmplitude);

        // Limit excessively large errors (very low transmission or strehl):
        visAmpErr = Math.min(visAmpErr, MAX_ERR_V);

        // Distribute the error on RE/IM parts for an uniform error distribution :
        // see These Martin Vannier (2003) p 76
        // sigma2(visRe) = 1/2 ( sigma2(visRe) + sigma2(visIm) ) = sigma2(vis) / 2
        // complex visibility error : visErrRe = visErrIm = visAmpErr / SQRT(2) :
        // No, circular normal distribution:
        // visErrRe = visErrIm = visAmpErr
        return visAmpErr;
    }

    /**
     * Compute error on visibility amplitude derived from computeVis2Error(visAmp)
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @param visAmp visibility amplitude
     * @param visScale visibility scale in [0.0 - 1.0] to take into account coherence loss
     * @param forAmplitude true to compute error for amplitudes (including the photometric error); false to compute error for phases
     * @return visiblity error
     */
    private double computeVisError(final int iPoint, final int iChannel, final double visAmp, final double visScale, final boolean forAmplitude) {
        if (!forAmplitude) {
            if (AsproConstants.MATCHER_MATISSE.match(instrumentName)) {
                // special case for VISPHI / T3PHI and Correlated flux (VISDATA):
                return computeVisErrorForMatisse(iPoint, iChannel, visAmp, visScale);
            }
        }

        // vis2 error with or without photometric error:
        final double errV2 = computeVis2Error(iPoint, iChannel, visAmp * visAmp, visScale * visScale, forAmplitude); // for phases, do not use photometric error
        if (errV2 >= MAX_ERR_V2) {
            return MAX_ERR_V;
        }
        // dvis = d(vis2) / (2 * vis) :
        // in log scale: (dv / v) = (1/2) (dv2 / v2)
        return errV2 / (2d * visAmp);
    }

    public static double deriveVis2Error(final double cVisError, final double visAmp) {
        if (cVisError >= MAX_ERR_V) {
            return MAX_ERR_V2;
        }
        final double visAmpErr = cVisError; // visErrRe = visErrIm = visAmpErr

        final double errV2 = visAmpErr * (2d * visAmp);
        return errV2;
    }

    private boolean check(final int iPoint, final int iChannel) {
        if (DO_CHECKS) {
            // fast return NaN if invalid configuration :
            if (this.invalidParameters) {
                return true;
            }
            if (iPoint < 0 || iPoint >= nPoints) {
                logger.warn("invalid point index {}, expect [0 to {}]", iPoint, nPoints);
                return true;
            }
            if (iChannel < 0 || iChannel >= nSpectralChannels) {
                logger.warn("invalid channel index {}, expect [0 to {}]", iChannel, nSpectralChannels);
                return true;
            }
        }
        return false;
    }

    public double getVisAmpBias(final int iChannel, final double vamp) {
        return instrumentalVisRelBias[iChannel] * vamp;
    }

    public double getVisAmpCalBias(final int iChannel) {
        return instrumentalVisCalBias[iChannel];
    }

    public double getVisPhiBias(final int iChannel) {
        return instrumentalVisPhaseBias[iChannel]; // absolute in radians
    }

    public double getVisPhiCalBias(final int iChannel) {
        return instrumentalVisPhaseCalBias[iChannel]; // absolute in radians
    }

    public double getVis2Bias(final int iChannel, final double vis2) {
        return instrumentalVis2RelBias[iChannel] * vis2; // absolute
    }

    public double getVis2CalBias(final int iChannel, final double vis2) {
        // upper-limit for V = 1:
        // max [ d(V2) ] = max [ 2V d(V) ] = 2 d(V)
        return 2.0 * instrumentalVisCalBias[iChannel]; // absolute
    }

    public double getT3AmpBias(final int iChannel, final double t3amp) {
        return t3amp * getT3PhiBias(iChannel); // absolute
    }

    public double getT3AmpCalBias(final int iChannel, final double t3amp) {
        return t3amp * getT3PhiCalBias(iChannel); // absolute
    }

    public double getT3PhiBias(final int iChannel) {
        return instrumentalT3PhaseBias[iChannel]; // absolute in radians
    }

    public double getT3PhiCalBias(final int iChannel) {
        return instrumentalT3PhaseCalBias[iChannel]; // absolute in radians
    }

    public double getTotFrameCorrection() {
        return totFrameCorrection;
    }

    public double getObsUsedDit() {
        return obsUsedDit;
    }

    public double getTotalObsTime() {
        return totalObsTime;
    }

    public double getTelDiam() {
        return telDiam;
    }

    public double getH0() {
        return h0;
    }

    public double getT0() {
        return t0;
    }

    public double getSeeing() {
        return seeing;
    }

    public double getDistAO() {
        return distAO;
    }

    public double getDistSCI_FT() {
        return distSCI_FT;
    }

    public double getVisScaleMeanForMidRefPoint() {
        return visScaleMeanForMidRefPoint;
    }

    public void setVisScaleMeanForMidRefPoint(final double visScale) {
        this.visScaleMeanForMidRefPoint = ((visScale >= 0.0) && Double.isFinite(visScale)) ? visScale : 1.0;
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
     * @param forAmplitude true to compute error for amplitudes (including the photometric error); false to compute error for phases
     * @return complex visiblity error or NaN if the error can not be computed
     */
    @Override
    public double computeVisComplexErrorValue(final double visAmp, final boolean forAmplitude) {
        // Note: noise service is called by UVMapSwingWorker that needs it ready as soon as UV coverage is computed (promptly):
        return computeVisComplexErrorValue(iMidPoint, iRefChannel, visAmp, visScaleMeanForMidRefPoint, forAmplitude);
    }

    /* --- utility methods --- */
    private void dumpVis2Error(final int iChannel) {
        logger.info("channel: {} => {} microns", iChannel, waveLengths[iChannel]);
        dumpVis2ErrorSample(iChannel, 1d);
        dumpVis2ErrorSample(iChannel, 0.5d);
        dumpVis2ErrorSample(iChannel, 0.1d);
        dumpVis2ErrorSample(iChannel, 0.01d);
    }

    private void dumpVis2ErrorSample(final int iChannel, final double visAmp) {
        double v2 = visAmp * visAmp;
        double errV2 = computeVis2Error(iMidPoint, iChannel, v2, 1.0, true);
        double snr = v2 / errV2;

        logger.info("computeVis2Error({}) :{} SNR= {} bias= {}", NumberUtils.trimTo5Digits(v2), errV2, NumberUtils.trimTo3Digits(snr));
    }

    /**
     * Find the band corresponding to the given wavelength
     * but always use V band instead of I & R bands
     *
     * @param instrumentName instrument name
     * @param waveLength wave length in microns
     * @return corresponding band
     * @throws IllegalArgumentException if no band found
     */
    private static Band findBand(final String instrumentName, final double waveLength) throws IllegalArgumentException {
        // MATISSE: use specific bands:
        if (AsproConstants.MATCHER_MATISSE.match(instrumentName)) {
            // L <= 4.2
            if (waveLength <= 4.2) {
                return Band.L;
            }
            // N >= 7.0
            if (waveLength >= 7.0) {
                return Band.N;
            }
            // M ] 4.2 - 7.0 [
            return Band.M;
        }

        final Band band = Band.findBand(waveLength);
        // TODO: fix that logic to use all possible bands within the instrument bandwidth
        switch (band) {
            case U:
            // avoid 'band U not supported'
            case B:
            case G_BP:
            case V:
            case G:
            case R:
            case G_RP:
            case I:
                // always use V for Visible:
                return Band.V;
            case Q:
                // avoid 'band Q not supported'
                return Band.N;
            default:
                return band;
        }
    }

    /* --- utility methods --- */
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
        return df.format(val) + unit;
    }

    static class NoiseWParams {

        /* varying values (spectrally dependent) */
        /** (W) number of object photons in each photometric channel (photometric flux) */
        final double[] nbPhotObjPhoto;
        /** (W) number of photons in interferometer channel per telescope */
        final double[] nbPhotInterf;
        /** (W) number of photons in each photometric channel (photometric flux) */
        final double[] nbPhotPhoto;
        /** (W) error on the number of photons in each photometric channel (photometric flux) */
        final double[] errPhotPhoto;
        /** (W) square error contribution of 2 photometric channels on the square visiblity FiFj */
        final double[] sqErrVis2Phot;
        /** (W) coefficient used to the squared correlated flux */
        final double[] sqCorFluxCoef;
        /** (W) coefficient used to compute variance of the squared correlated flux */
        final double[] varSqCorFluxCoef;
        /** (W) constant used to compute variance of the squared correlated flux */
        final double[] varSqCorFluxConst;

        /** (W) squared correlated flux */
        final double[] sqCorrFlux;
        /** (W) error on squared correlated flux */
        final double[] errSqCorrFlux;

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
            this.nbPhotObjPhoto = init(nWLen);
            this.nbPhotInterf = init(nWLen);
            this.nbPhotPhoto = init(nWLen);
            this.errPhotPhoto = init(nWLen);
            this.sqErrVis2Phot = init(nWLen);
            this.sqCorFluxCoef = init(nWLen);
            this.varSqCorFluxCoef = init(nWLen);
            this.varSqCorFluxConst = init(nWLen);
            this.sqCorrFlux = init(nWLen);
            this.errSqCorrFlux = init(nWLen);
            this.t3photCoef = (DEBUG) ? init(nWLen) : null;
            this.t3photCoef2 = (DEBUG) ? init(nWLen) : null;
            this.t3photCoef3 = (DEBUG) ? init(nWLen) : null;
            this.t3detConst = (DEBUG) ? init(nWLen) : null;
            this.t3detCoef1 = (DEBUG) ? init(nWLen) : null;
            this.t3detCoef2 = (DEBUG) ? init(nWLen) : null;
        }

        static double[] init(final int len) {
            final double[] array = new double[len];
            Arrays.fill(array, Double.NaN);
            return array;
        }
    }
}
