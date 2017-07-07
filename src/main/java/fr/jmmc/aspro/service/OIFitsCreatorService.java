/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.observability.TargetPointInfo;
import fr.jmmc.aspro.model.oi.FocalInstrument;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Position3D;
import fr.jmmc.aspro.model.oi.SpectralSetup;
import fr.jmmc.aspro.model.oi.SpectralSetupQuantity;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.Telescope;
import fr.jmmc.aspro.model.oi.UserModel;
import fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData;
import fr.jmmc.aspro.service.UserModelService.MathMode;
import fr.jmmc.aspro.util.StatUtils;
import fr.jmmc.aspro.util.StatUtils.ComplexDistribution;
import static fr.jmmc.aspro.util.StatUtils.N_SAMPLES;
import static fr.jmmc.aspro.util.StatUtils.SAMPLING_FACTOR_MEAN;
import static fr.jmmc.aspro.util.StatUtils.SAMPLING_FACTOR_VARIANCE;
import fr.jmmc.jmal.ALX;
import fr.jmmc.jmal.complex.Complex;
import fr.jmmc.jmal.complex.ImmutableComplex;
import fr.jmmc.jmal.complex.MutableComplex;
import fr.jmmc.jmal.model.ModelComputeContext;
import fr.jmmc.jmal.model.ModelFunctionComputeContext;
import fr.jmmc.jmal.model.ModelManager;
import fr.jmmc.jmal.model.targetmodel.Model;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.SpecialChars;
import fr.jmmc.jmcs.util.concurrent.ParallelJobExecutor;
import fr.jmmc.oitools.OIFitsConstants;
import fr.jmmc.oitools.model.OIArray;
import fr.jmmc.oitools.model.OIFitsChecker;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIT3;
import fr.jmmc.oitools.model.OITarget;
import fr.jmmc.oitools.model.OIVis;
import fr.jmmc.oitools.model.OIVis2;
import fr.jmmc.oitools.model.OIWavelength;
import fr.jmmc.oitools.util.CombUtils;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import net.jafama.FastMath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class contains the code to create OIFits structure from the current observation and performs noise modeling
 * 
 * @author bourgesl
 */
public final class OIFitsCreatorService {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(OIFitsCreatorService.class.getName());
    /** enable DEBUG mode */
    private final static boolean DEBUG = false;
    /** target Id */
    private final static short TARGET_ID = (short) 1;
    /** enable the OIFits validation */
    private final static boolean DO_VALIDATE_OIFITS = false;
    /** flag to show compute task statistics */
    private final static boolean SHOW_COMPUTE_STATS = false;
    /** threshold to use parallel jobs for user models (32 UV points) */
    private final static int JOB_THRESHOLD_USER_MODELS = 32;
    /** SNR threshold to flag values with low SNR */
    private final static double SNR_THRESHOLD = 3.0;
    /** Jmcs Parallel Job executor */
    private static final ParallelJobExecutor jobExecutor = ParallelJobExecutor.getInstance();

    /* members */
 /* input */
    /** selected target */
    private final Target target;
    /** OIFits supersampling preference */
    private final int supersamplingOIFits;
    /** OIFits MathMode preference */
    private final UserModelService.MathMode mathModeOIFits;

    /* reused observability data */
    /** beam list */
    private final List<Beam> beams;
    /** number of beams */
    private final int nBeams;
    /** base line list */
    private final List<BaseLine> baseLines;
    /** number of baselines */
    private final int nBaseLines;
    /** sky calc instance */
    private final AstroSkyCalc sc;

    /* reused uv coverage data */
    /** number of uv points */
    private final int nObsPoints;
    /** target information for each uv point couples */
    private final TargetPointInfo[] targetPointInfos;
    /** list of uv point couples corresponding to the target observability */
    private final List<UVRangeBaseLineData> targetUVObservability;

    /* output */
    /** oifits structure */
    private OIFitsFile oiFitsFile = null;

    /* internal */
    /** selected instrument mode */
    private FocalInstrumentMode instrumentMode = null;
    /** minimal wavelength */
    private double lambdaMin;
    /** maximal wavelength */
    private double lambdaMax;
    /** number of wavelengths = number of concrete spectral channels */
    private int nWaveLengths;
    /** target has model (analytical or user model) */
    private final boolean hasModel;
    /** flag indicating if the target model wavelengths are compatible with the instrument mode */
    private boolean isModelWLValid = true;
    /** flag = true if returned errors are valid */
    private final boolean errorValid;
    /** flag to add gaussian noise to OIFits data; true if parameter doDataNoise = true and noise parameters are valid */
    private final boolean doNoise;
    /** true to use instrument bias; false to compute only theoretical error */
    private final boolean useInstrumentBias;
    /** interferometer description */
    private InterferometerDescription interferometer = null;
    /** instrument name */
    private String instrumentName = null;
    /** arrname keyword value (interferometer name) */
    private String arrNameKeyword = null;
    /** insname keyword value (instrument_LAMBDA1-LAMBDA2-Nch) */
    private String insNameKeyword = null;
    /** instrument experimental flag */
    private boolean instrumentExperimental = false;
    /** wavelengths */
    private double[] waveLengths;
    /** wave band (constant) */
    private double waveBand;
    /** channel widths */
    private double[] waveBands;
    /** Station mapping */
    private Map<Station, Short> stationMapping = null;
    /** beam mapping */
    private Map<Beam, Short> beamMapping = null;
    /** baseline mapping */
    private Map<BaseLine, short[]> baseLineMapping = null;
    /** integration time (s) */
    private double integrationTime = 300d;
    /** noise service */
    private final NoiseService noiseService;
    /** internal computed complex visibility [row][waveLength] */
    private Complex[][] visComplex = null;
    /** internal complex visibility error [row][waveLength] */
    private double[][] visError = null;
    /** internal complex distribution [row] */
    private ComplexDistribution[] visRndDist = null;
    /** internal random index [row][waveLength] */
    private int[][] visRndIdx = null;
    /** internal complex visibility SNR flag [row][waveLength] */
    private boolean[][] visSnrFlag = null;
    /** random generator */
    private final Random random = new Random();

    /**
     * Protected constructor
     * @param observation observation settings
     * @param target target to process
     * @param beams beam list
     * @param baseLines base line list
     * @param useInstrumentBias true to use instrument bias; false to compute only theoretical error
     * @param doDataNoise flag to add gaussian noise to OIFits data
     * @param supersamplingOIFits OIFits supersampling preference
     * @param mathModeOIFits OIFits MathMode preference
     * @param targetPointInfos target information for each uv point couples
     * @param targetUVObservability list of UV coordinates per baseline
     * @param sc sky calc instance
     * @param warningContainer container for warning messages
     */
    protected OIFitsCreatorService(final ObservationSetting observation,
            final Target target,
            final List<Beam> beams,
            final List<BaseLine> baseLines,
            final boolean useInstrumentBias,
            final boolean doDataNoise,
            final int supersamplingOIFits,
            final UserModelService.MathMode mathModeOIFits,
            final TargetPointInfo[] targetPointInfos,
            final List<UVRangeBaseLineData> targetUVObservability,
            final AstroSkyCalc sc,
            final WarningContainer warningContainer) {

        this.target = target;
        this.beams = beams;
        this.nBeams = this.beams.size();
        this.baseLines = baseLines;
        this.nBaseLines = this.baseLines.size();
        this.nObsPoints = targetPointInfos.length;
        this.targetPointInfos = targetPointInfos;
        this.targetUVObservability = targetUVObservability;
        this.sc = sc;

        if (observation.getInstrumentConfiguration().getAcquisitionTime() != null) {
            this.integrationTime = observation.getInstrumentConfiguration().getAcquisitionTime().doubleValue();
        }
        // use target model:
        this.hasModel = target.hasModel();

        // use Bias:
        this.useInstrumentBias = useInstrumentBias;

        // OIFits preferences:
        this.supersamplingOIFits = supersamplingOIFits;
        this.mathModeOIFits = mathModeOIFits;

        // Prepare with observation:
        // note: may adjust wavelengths:
        prepare(observation, warningContainer);

        // Prepare the noise service with exact wavelengths:
        // note: NoiseService parameter dependencies:
        // observation {target}
        // parameter: warningContainer
        this.noiseService = new NoiseService(observation, target, targetPointInfos, useInstrumentBias, warningContainer,
                this.waveLengths, this.waveBands);

        this.errorValid = this.noiseService.isValid()
                && !"DEMO".equalsIgnoreCase(this.arrNameKeyword); // do not generate errors for the DEMO interferometer

        // do noise :
        this.doNoise = (doDataNoise && this.errorValid);
    }

    /**
     * Prepare OIFits keywords and the instrumental spectral configuration and may add warning messages
     * @param observation observation to use
     * @param warningContainer warning container to use if needed
     */
    private void prepare(final ObservationSetting observation, final WarningContainer warningContainer) {

        final InterferometerConfiguration intConf = observation.getInterferometerConfiguration().getInterferometerConfiguration();

        this.interferometer = intConf.getInterferometer();

        // Use interferometer name (VLTI, CHARA ...) and not 'VLTI Period 91':
        this.arrNameKeyword = this.interferometer.getName();

        final FocalInstrument instrument = observation.getInstrumentConfiguration().getInstrumentConfiguration().getFocalInstrument();

        // use alias or real instrument name:
        this.instrumentName = instrument.getAliasOrName();
        this.instrumentExperimental = (instrument.isExperimental() != null) ? instrument.isExperimental().booleanValue() : false;

        if (logger.isDebugEnabled()) {
            logger.debug("arrNameKeyword: {}", this.arrNameKeyword);
            logger.debug("instrumentName: {}", this.instrumentName);
            logger.debug("instrumentExperimental: {}", this.instrumentExperimental);
        }

        final FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();
        if (insMode == null) {
            throw new IllegalStateException("The instrumentMode is empty !");
        }
        this.instrumentMode = insMode;

        prepareInstrumentMode(warningContainer);
    }

    /**
     * Prepare the instrumental spectral configuration: Wavelengths and may add warning messages
     * @param warningContainer warning container to use if needed
     */
    private void prepareInstrumentMode(final WarningContainer warningContainer) {
        if (logger.isDebugEnabled()) {
            logger.debug("instrumentMode: {}", instrumentMode.getName());
        }

        // Get wavelength range for the selected instrument mode :
        this.lambdaMin = AsproConstants.MICRO_METER * instrumentMode.getWaveLengthMin();
        this.lambdaMax = AsproConstants.MICRO_METER * instrumentMode.getWaveLengthMax();

        // TODO: handle properly spectral channels (rebinning):
        this.nWaveLengths = instrumentMode.getSpectralChannels();

        if (logger.isDebugEnabled()) {
            logger.debug("lambdaMin: {}", this.lambdaMin);
            logger.debug("lambdaMax: {}", this.lambdaMax);
            logger.debug("nChannels: {}", this.nWaveLengths);
        }

        // prepare wavelengths independently of the user model Fits cube (wavelengths):
        prepareChannels();

        // Initial Wavelength information:
        String firstChannel = Double.toString(convertWL(this.waveLengths[0]));
        String lastChannel = (this.nWaveLengths > 1) ? Double.toString(convertWL(this.waveLengths[this.nWaveLengths - 1])) : null;

        addInformation(warningContainer, this.instrumentName + " instrument mode: "
                + this.nWaveLengths + " channels "
                + '[' + firstChannel + ((lastChannel != null) ? (" - " + lastChannel) : "") + " " + SpecialChars.UNIT_MICRO_METER + "] "
                + "(band: " + convertWL(this.waveBand) + " " + SpecialChars.UNIT_MICRO_METER + ')');

        // keep number of channels:
        final int nChannels = this.nWaveLengths;

        // Keep only spectral channels where user model is defined:
        // note: Instrument spectral channels (waveLengths, nWaveLengths, waveBand, lambdaMin, lambdaMax) can be modified by this method:
        this.isModelWLValid = prepareUserModel(warningContainer);

        // adjust used spectral channels in information and log:
        if (this.nWaveLengths != nChannels) {
            // Wavelength information:
            firstChannel = Double.toString(convertWL(this.waveLengths[0]));
            lastChannel = (this.nWaveLengths > 1) ? Double.toString(convertWL(this.waveLengths[this.nWaveLengths - 1])) : null;

            addWarning(warningContainer, "Restricted instrument mode: "
                    + this.waveLengths.length + " channels "
                    + '[' + firstChannel + ((lastChannel != null) ? (" - " + lastChannel) : "") + " " + SpecialChars.UNIT_MICRO_METER + "] ");
        }

        this.insNameKeyword = this.instrumentName + '_' + firstChannel + ((lastChannel != null) ? ("-" + lastChannel) : "") + '-' + this.nWaveLengths + "ch";

        if (logger.isDebugEnabled()) {
            logger.debug("insNameKeyword: {}", insNameKeyword);
        }
    }

    private void prepareChannels() {
        // prepare wavelengths independently of the user model Fits cube (wavelengths):
        if (instrumentMode.getTable() != null) {
            final SpectralSetup table = instrumentMode.getTable();
            this.waveLengths = table.getAndScaleColumn(SpectralSetupQuantity.LAMBDA, 1e-6);
            this.waveBands = table.getAndScaleColumn(SpectralSetupQuantity.DELTA_LAMBDA, 1e-6);
            // TODO: handle variable bandwdith:
            this.waveBand = this.waveBands[0];

            // TODO: use mean(waveBands) ?
        } else {
            // TODO: handle variable bandwdith:
            this.waveBand = (this.lambdaMax - this.lambdaMin) / this.nWaveLengths;
            this.waveLengths = computeWaveLengths(this.lambdaMin, this.waveBand, this.nWaveLengths);
            this.waveBands = new double[this.nWaveLengths];
            Arrays.fill(waveBands, waveBand);
        }
    }

    /**
     * Compute the regularly sampled wavelengths (centered on each spectral channel) given its bounds and spectral channel width
     * @param min lower bound
     * @param width spectral channel width
     * @param nWLen number of wavelengths to compute
     * @return regularly sampled wavelengths
     */
    private static double[] computeWaveLengths(final double min, final double width, final int nWLen) {
        final double[] wLen = new double[nWLen];

        // effective wavelength corresponds to the channel center:
        double waveLength = min + 0.5d * width;

        for (int i = 0; i < nWLen; i++) {
            wLen[i] = waveLength;

            waveLength += width;
        }
        return wLen;
    }

    /**
     * Compute the regularly sampled wavelengths (centered on each spectral channel) given its bounds and spectral channel width
     * @param waveLengths wavelengths (central)
     * @param waveBands bandwidths
     * @param nSamples number of samples to compute
     * @return regularly sampled wavelengths
     */
    private static double[] resampleWaveLengths(final double[] waveLengths, final double[] waveBands, final int nSamples) {
        final int nWLen = waveLengths.length;
        final double[] wLen = new double[nWLen * nSamples];

        double step, lambda;

        for (int i = 0, k = 0; i < nWLen; i++) {

            lambda = waveLengths[i] - 0.5 * waveBands[i];
            step = waveBands[i] / (nSamples + 1);
            lambda += step;

            for (int j = 0; j < nSamples; j++) {
                wLen[k++] = lambda;
                lambda += step;
            }
        }
        /*        
        System.out.println("waveLengths: "+Arrays.toString(waveLengths));
        System.out.println("nSamples: "+nSamples);
        System.out.println("wLen: "+Arrays.toString(wLen));
         */
        return wLen;
    }

    /**
     * Prepare the user model vs the instrumental spectral configuration
     * @param warningContainer warning container to use if needed
     * @return true if OK; false if user model is invalid (so discard computation)
     */
    private boolean prepareUserModel(final WarningContainer warningContainer) {
        if (this.hasModel) {
            final boolean useAnalyticalModel = this.target.hasAnalyticalModel();

            // user model if defined:
            final UserModel userModel = (!useAnalyticalModel) ? target.getUserModel() : null;

            // Test if user model data is valid:
            if (userModel != null && userModel.isModelDataReady()) {
                final List<UserModelData> modelDataList = target.getUserModel().getModelDataList();

                final int nImages = modelDataList.size();
                final UserModelData modelDataFirst = modelDataList.get(0);
                final UserModelData modelDataLast = modelDataList.get(nImages - 1);

                // first and last images have wavelength ?
                final boolean isWL = !Double.isNaN(modelDataFirst.getWaveLength()) && !Double.isNaN(modelDataLast.getWaveLength());

                if (!isWL && nImages > 1) {
                    // Fits cube without wavelengths:
                    addWarning(warningContainer, "User model (Fits cube) without wavelength information is discarded");
                    return false;
                }

                if (isWL) {
                    final double wlFirst = modelDataFirst.getWaveLength();

                    if (nImages == 1) {
                        // check image wavelength is in instrument range:
                        if (wlFirst < this.lambdaMin || wlFirst > this.lambdaMax) {
                            addWarning(warningContainer, "User model (Fits image) wavelength ("
                                    + convertWL(wlFirst) + " " + SpecialChars.UNIT_MICRO_METER
                                    + ") outside of instrumental wavelength range");
                            return false;
                        }

                    } else {
                        // Fits cube:
                        final double wlLast = modelDataLast.getWaveLength();
                        final double wlInc = modelDataFirst.getWaveLengthIncrement(); // constant in Fits cube

                        addInformation(warningContainer, "User model [" + userModel.getName() + "]: " + nImages + " images "
                                + '[' + convertWL(wlFirst) + " - " + convertWL(wlLast) + " " + SpecialChars.UNIT_MICRO_METER + "] "
                                + "(increment: " + convertWL(wlInc) + " " + SpecialChars.UNIT_MICRO_METER + ')');

                        // check image wavelengths are overlapping the instrument range:
                        if (modelDataFirst.getWaveLengthRange().getMin() > this.lambdaMax) {
                            addWarning(warningContainer, "Incorrect model min wavelength [" + convertWL(wlFirst) + " " + SpecialChars.UNIT_MICRO_METER
                                    + "] higher than max instrument wavelength [" + convertWL(this.lambdaMax) + " " + SpecialChars.UNIT_MICRO_METER + ']');
                            return false;
                        }
                        if (modelDataLast.getWaveLengthRange().getMax() < this.lambdaMin) {
                            addWarning(warningContainer, "Incorrect model max wavelength [" + convertWL(wlLast) + " " + SpecialChars.UNIT_MICRO_METER
                                    + "] lower than min instrument wavelength [" + convertWL(this.lambdaMin) + " " + SpecialChars.UNIT_MICRO_METER + ']');
                            return false;
                        }

                        // navigate among spectral channels:
                        final double[] insWaves = this.waveLengths;
                        final double[] insBands = this.waveBands;
                        final int nWaves = insWaves.length;

                        if (logger.isDebugEnabled()) {
                            logger.debug("nWaves: {}", nWaves);
                            logger.debug("insWaves: {}", Arrays.toString(insWaves));
                        }

                        // note: hashset behaves like identity check:
                        final Set<UserModelData> uniqueModelDatas = new HashSet<UserModelData>(nImages);

                        final boolean[] hasUserModelPerChannel = new boolean[nWaves];

                        double wl, wlLower, wlUpper, halfBand;
                        UserModelData modelWlLower, modelWlUpper;

                        for (int i = 0; i < nWaves; i++) {
                            wl = insWaves[i];
                            halfBand = 0.5d * insBands[i];

                            wlLower = wl - halfBand;
                            wlUpper = wl + halfBand;

                            hasUserModelPerChannel[i] = false;

                            modelWlLower = findUserModelData(wlLower, modelDataList);

                            if (modelWlLower != null) {
                                hasUserModelPerChannel[i] = true;
                                uniqueModelDatas.add(modelWlLower);
                            }

                            modelWlUpper = findUserModelData(wlUpper, modelDataList);

                            if (modelWlUpper != null) {
                                hasUserModelPerChannel[i] = true;
                                uniqueModelDatas.add(modelWlUpper);
                            }
                        }

                        if (logger.isDebugEnabled()) {
                            logger.debug("hasUserModelPerChannel: {}", Arrays.toString(hasUserModelPerChannel));
                            logger.debug("nUniqueModelDatas: {}", uniqueModelDatas.size());
                        }

                        int firstChannel = -1;

                        for (int i = 0; i < nWaves; i++) {
                            if (hasUserModelPerChannel[i]) {
                                firstChannel = i;
                                break;
                            }
                        }

                        if (logger.isDebugEnabled()) {
                            logger.debug("firstChannel: {}", firstChannel);
                        }

                        int lastChannel = -1;
                        for (int i = nWaves - 1; i >= 0; i--) {
                            if (hasUserModelPerChannel[i]) {
                                lastChannel = i;
                                break;
                            }
                        }

                        if (logger.isDebugEnabled()) {
                            logger.debug("lastChannel: {}", lastChannel);
                        }

                        final int nChannels = lastChannel - firstChannel + 1;

                        // Test sub sampling (less than 1 IMAGE PER CHANNEL)
                        if (nChannels > uniqueModelDatas.size()) {
                            addWarning(warningContainer, "Sub sampling detected: " + nChannels + " channels but only "
                                    + uniqueModelDatas.size() + " user model images available");
                        }

                        // keep only channels where at least one image is present:
                        if (nWaves > nChannels) {
                            // only part of the instrument channels are used:

                            // Fix wavelength/waveband:
                            // skip waveband (constant) for now
                            this.waveLengths = new double[nChannels];
                            this.waveBands = new double[nChannels];

                            System.arraycopy(insWaves, firstChannel, this.waveLengths, 0, nChannels);
                            System.arraycopy(insBands, firstChannel, this.waveBands, 0, nChannels);

                            if (logger.isDebugEnabled()) {
                                logger.debug("waveLengths: {}", Arrays.toString(waveLengths));
                                logger.debug("waveBands: {}", Arrays.toString(waveBands));
                            }

                            // fix other related fields:
                            this.nWaveLengths = waveLengths.length;

                            this.lambdaMin = waveLengths[0] - 0.5 * waveBands[0];
                            this.lambdaMax = waveLengths[nWaveLengths - 1] + 0.5 * waveBands[nWaveLengths - 1];
                        }
                    }
                }
            }
        }
        return true;
    }

    /**
     * Find the user model data corresponding only to the given wavelength (within +/- 1/2 increment)
     * @param wavelength spectral channel wavelength
     * @param modelDataList user model data
     * @return user model data corresponding to the given wavelength
     */
    private static UserModelData findUserModelData(final double wavelength, final List<UserModelData> modelDataList) {

        final int nImages = modelDataList.size();

        UserModelData modelData;

        // suppose that model image wavelength ranges do not overlap (true for fitscube):
        for (int i = 0; i < nImages; i++) {
            modelData = modelDataList.get(i);

            if (modelData.getWaveLengthRange().contains(wavelength)) {
                return modelData;
            }
        }
        // No user model found:
        return null;
    }

    /**
     * Create the OIFits structure with OI_ARRAY, OI_TARGET, OI_WAVELENGTH, OI_VIS tables
     * @return OIFits structure
     */
    public OIFitsFile createOIFits() {
        if (!this.isModelWLValid) {
            // invalid user model against instrumental spectral configuration:
            return null;
        }
        // Start the computations :
        final long start = System.nanoTime();

        // create a new EMPTY OIFits structure :
        this.oiFitsFile = new OIFitsFile();

        // Create station mappings:
        this.stationMapping = createStationMapping(this.interferometer.getStations());

        // Create beam and base line mappings :
        this.beamMapping = createBeamMapping(this.stationMapping, this.beams);
        this.baseLineMapping = createBaseLineMapping(this.beamMapping, this.baseLines);

        // OI_ARRAY :
        this.createOIArray();

        // OI_TARGET :
        this.createOITarget();

        // OI_WAVELENGTH :
        this.createOIWaveLength();

        // Compute complex visibilities:
        if (!this.computeModelVisibilities()) {
            // unable to compute complex visiblities so abort:
            return null;
        }

        // fast interrupt :
        if (Thread.currentThread().isInterrupted()) {
            return null;
        }

        // OI_VIS :
        this.createOIVis();

        // fast interrupt :
        if (Thread.currentThread().isInterrupted()) {
            return null;
        }

        // OI_VIS2 :
        this.createOIVis2();

        // fast interrupt :
        if (Thread.currentThread().isInterrupted()) {
            return null;
        }

        // OI_T3 :
        this.createOIT3();

        // fast interrupt :
        if (Thread.currentThread().isInterrupted()) {
            return null;
        }

        // free computed complex visibilities :
        this.visComplex = null;

        // remove the OI_VIS table for instruments that do not produce such results (PIONIER):
        if (this.instrumentName.startsWith(AsproConstants.INS_PIONIER)) {
            // Remove OI_VIS table if instrument is PIONIER:
            final OIVis vis = this.oiFitsFile.getOiVis()[0];

            this.oiFitsFile.removeOiTable(vis);
        }

        logger.info("createOIFits: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

        if (DO_VALIDATE_OIFITS) {
            final OIFitsChecker checker = new OIFitsChecker();
            this.oiFitsFile.check(checker);

            // validation results
            if (logger.isInfoEnabled()) {
                logger.info("createOIFits: validation results\n{}", checker.getCheckReport());
            }
        }

        // fast interrupt :
        if (Thread.currentThread().isInterrupted()) {
            return null;
        }

        // Analyze the OIFits file to get its configuration:
        this.oiFitsFile.analyze();

        return this.oiFitsFile;
    }

    /**
     * Create the OI_ARRAY table
     *
     * Note : station indexes are given according to the beam list ordering starting from 1
     */
    private void createOIArray() {

        // Create OI_ARRAY table :
        final OIArray oiArray = new OIArray(this.oiFitsFile, this.interferometer.getStations().size());

        // Array Name :
        oiArray.setArrName(this.arrNameKeyword);

        // Position :
        oiArray.setFrame(OIFitsConstants.KEYWORD_FRAME_GEOCENTRIC);

        Position3D position = this.interferometer.getPosition();

        oiArray.setArrayXYZ(position.getPosX(), position.getPosY(), position.getPosZ());

        // Stations :
        int i = 0;
        Telescope tel;

        for (final Station station : this.interferometer.getStations()) {

            tel = station.getTelescope();
            oiArray.getTelName()[i] = tel.getName();
            oiArray.getDiameter()[i] = (float) tel.getDiameter();

            oiArray.getStaName()[i] = station.getName();
            oiArray.getStaIndex()[i] = this.stationMapping.get(station).shortValue();

            // TODO : rotate the horizontal position to geocentric :
            position = station.getRelativePosition();

            //MAYBE DONT RUN
            oiArray.setStaXYZ(i, position.getPosX(), position.getPosY(), position.getPosZ());

            i++;
        }

        this.oiFitsFile.addOiTable(oiArray);
    }

    /**
     * Create the OI_TARGET table
     *
     * Note : target index is 1
     */
    private void createOITarget() {

        // Create OI_TARGET table :
        final OITarget oiTarget = new OITarget(this.oiFitsFile, 1);
        oiTarget.getTargetId()[0] = TARGET_ID;
        oiTarget.getTarget()[0] = this.target.getName();

        // Coordinates RA/DEC :
        oiTarget.getRaEp0()[0] = this.target.getRADeg();
        oiTarget.getDecEp0()[0] = this.target.getDECDeg();
        oiTarget.getEquinox()[0] = this.target.getEQUINOX();

        // Missing RA/DEC errors :
        oiTarget.getRaErr()[0] = 0d;
        oiTarget.getDecErr()[0] = 0d;

        // Radial velocity :
        if (this.target.getSYSVEL() != null) {
            // toShort km/s in m/s :
            oiTarget.getSysVel()[0] = this.target.getSYSVEL().doubleValue() * 1e3;
        }
        oiTarget.getVelTyp()[0] = OIFitsConstants.UNKNOWN_VALUE;
        // Use VELTYP (mostly undefined) :
        oiTarget.getVelDef()[0] = OIFitsConstants.COLUMN_VELDEF_OPTICAL;

        // Proper motion :
        if (this.target.getPMRA() != null && this.target.getPMDEC() != null) {
            // toShort mas/year in deg/year :
            oiTarget.getPmRa()[0] = this.target.getPMRA().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES;
            oiTarget.getPmDec()[0] = this.target.getPMDEC().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES;
        }

        // Missing PM RA/DEC errors :
        oiTarget.getPmRaErr()[0] = 0d;
        oiTarget.getPmDecErr()[0] = 0d;

        // Parallax :
        if (this.target.getPARALLAX() != null && this.target.getPARAERR() != null) {
            // toShort mas in deg :
            oiTarget.getParallax()[0] = (float) (this.target.getPARALLAX().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES);
            oiTarget.getParaErr()[0] = (float) (this.target.getPARAERR().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES);
        }

        // Spectral type :
        if (this.target.getSPECTYP() != null && this.target.getSPECTYP().length() > 0) {
            oiTarget.getSpecTyp()[0] = target.getSPECTYP();
        } else {
            oiTarget.getSpecTyp()[0] = "";
        }

        this.oiFitsFile.addOiTable(oiTarget);
    }

    /**
     * Create the OI_WAVELENGTH table
     */
    private void createOIWaveLength() {
        // Create OI_WAVELENGTH table :
        final OIWavelength waves = new OIWavelength(this.oiFitsFile, this.nWaveLengths);
        waves.setInsName(this.insNameKeyword);

        final float[] effWave = waves.getEffWave();
        final float[] effBand = waves.getEffBand();

        for (int i = 0; i < this.nWaveLengths; i++) {
            effWave[i] = (float) this.waveLengths[i];
            effBand[i] = (float) this.waveBands[i];
        }

        this.oiFitsFile.addOiTable(waves);
    }

    /**
     * Compute complex visibilities using the target model (analytical or user model)
     * and store this data in local reference table
     * @return true if complex visibilities are computed; false otherwise
     */
    private boolean computeModelVisibilities() {

        boolean computed = false;

        if (this.hasModel) {
            /** Get the current thread to check if the computation is interrupted */
            final Thread currentThread = Thread.currentThread();

            final long start = System.nanoTime();

            // Effective number of spectral channels on the detector:
            final int nChannels = nWaveLengths;

            final boolean useAnalyticalModel = this.target.hasAnalyticalModel();

            // user model if defined:
            final UserModel userModel = (!useAnalyticalModel) ? target.getUserModel() : null;

            // Test if user model data is valid:
            if (userModel != null && !userModel.isModelDataReady()) {
                return false;
            }

            // Determine nSamples per spectral channel:
            // use the preference (QUICK, FAST, DEFAULT?) : QUICK = PREVIEW ie No super sampling
            final UserModelService.MathMode mathMode = this.mathModeOIFits;

            // TODO: determine correctly deltaLambda (object size (FOV) and Bmax/lambda) ie when super sampling is surely necessary
            // number of samples per spectral channel (1, 5, 9 ...) use the preference (SuperSampling)
            // should be an even number to keep wavelengths centered on each sub channels:
            // note: disable super sampling in high resolution:
            int nSamples = (nChannels > 100 || mathMode == MathMode.QUICK) ? 1 : this.supersamplingOIFits;

// TODO: fix resampling
            double deltaLambda = this.waveBand / nSamples;

            // If Fits cube: use all images at least i.e. adjust frequencies and nSamples:
            final double wlInc = (userModel != null && userModel.isModelDataReady()) ? userModel.getModelData(0).getWaveLengthIncrement() : Double.POSITIVE_INFINITY;

            // Sub channel width:
            if ((wlInc > 0.0) && (wlInc < deltaLambda)) {
                // adjust nSamples to have deltaLambda < wlInc:
                nSamples = (int) Math.ceil((nSamples * this.waveBand) / wlInc);
            }

            // Prefer odd number of sub channels:
            if (nSamples % 2 == 0) {
                nSamples++;
            }

            if (logger.isDebugEnabled()) {
                logger.debug("computeModelVisibilities: adjusted nSamples = {}", nSamples);
            }

            // TODO CHECK: resampling !
            // note: integration must be adjusted to use wavelengths in each spectral channel !
            // Only part of instrument spectral channels can be used (see prepare step):
            final double[] sampleWaveLengths = (nSamples > 1) ? resampleWaveLengths(this.waveLengths, this.waveBands, nSamples) : this.waveLengths;

            // local vars:
            final int nWLen = sampleWaveLengths.length;

            if (logger.isDebugEnabled()) {
                logger.debug("computeModelVisibilities: nWLen = {} - nChannels = {}", nWLen, nChannels);
            }

            final int nBl = nBaseLines;
            final int nObs = nObsPoints;

            final NoiseService ns = (errorValid) ? noiseService : null;

            // TODO: compare directFT vs FFT + interpolation
            // fast interrupt :
            if (currentThread.isInterrupted()) {
                return false;
            }

            final int nRows = nObs * nBl;
            final int nPoints = nRows * nWLen;

            logger.info("computeModelVisibilities: {} points [{} rows - {} spectral channels - {} samples]- please wait ...",
                    nPoints, nRows, nChannels, nSamples);

            // Allocate data array for complex visibility and error :
            final MutableComplex[][] cmVis = new MutableComplex[nRows][];

            // Iterate on rows :
            for (int k = 0; k < nRows; k++) {
                cmVis[k] = createArray(nWLen + 4); // cache line padding (Complex = 2 double = 16 bytes) so 4 complex = complete cache line
            }

            // Allocate data array for spatial frequencies:
            final double[][] ufreq = new double[nRows][nWLen];
            final double[][] vfreq = new double[nRows][nWLen];

            // Inverse wavelength:
            double[] invWaveLengths = new double[nWLen];
            for (int l = 0; l < nWLen; l++) {
                invWaveLengths[l] = 1d / sampleWaveLengths[l];
            }

            final int[] ptIdx = new int[nRows];

            // Iterate on baselines :
            for (int i, j = 0, k, l; j < nBl; j++) {

                final UVRangeBaseLineData uvBL = this.targetUVObservability.get(j);

                // Iterate on observable UV points :
                for (i = 0; i < nObs; i++) {
                    k = nBl * i + j;
                    // define point index:
                    ptIdx[k] = i;

                    // u/v freqs ...
                    final double[] uRow = ufreq[k];
                    final double[] vRow = vfreq[k];

                    // UV coords (m) :
                    final double u = uvBL.getU()[i];
                    final double v = uvBL.getV()[i];

                    // prepare spatial frequencies :
                    for (l = 0; l < nWLen; l++) {
                        uRow[l] = u * invWaveLengths[l];
                        vRow[l] = v * invWaveLengths[l];
                    }
                }
            }

            // Compute complex visibility using the target model:
            if (useAnalyticalModel) {
                // Clone models and normalize fluxes :
                final List<Model> normModels = ModelManager.normalizeModels(this.target.getModels());

                // Analytical models: no parallelization:
                final ModelManager modelManager = ModelManager.getInstance();

                // model computation context
                final ModelComputeContext context = modelManager.prepareModels(normModels, nWLen);

                // Iterate on rows:
                for (int k = 0; k < nRows; k++) {

                    // Compute complex visibility using the target model:
                    copyArray(modelManager.computeModels((ModelFunctionComputeContext) context, ufreq[k], vfreq[k]), cmVis[k]);

                    if (currentThread.isInterrupted()) {
                        // fast interrupt :
                        return false;
                    }

                } // rows

            } else {
                // Accuracy tests (see OIFitsWriterTest) on GRAVITY so VISAMPERR/VISPHIERR are only 'sampled':
                // FAST provides good accuracy on complete OIFits:
                /*                
                 WARNING: WARN:  Column[VISAMP]	    Max Absolute Error=3.122502256758253E-17	Max Relative Error=2.943534976436331E-14
                 WARNING: WARN:  Column[VISAMPERR]	Max Absolute Error=0.0013869817652312072	Max Relative Error=0.0997510362307679
                 WARNING: WARN:  Column[VISPHI]	    Max Absolute Error=1.8474111129762605E-12	Max Relative Error=3.3158630356109147E-12
                 WARNING: WARN:  Column[VISPHIERR]	Max Absolute Error=19.19686940833244	    Max Relative Error=0.9113901536192872
                 WARNING: WARN:  Column[VIS2DATA]	Max Absolute Error=1.5178830414797062E-18	Max Relative Error=5.915784768102743E-14
                 WARNING: WARN:  Column[VIS2ERR]	Max Absolute Error=5.421010862427522E-20	Max Relative Error=4.471809116292319E-16
                 WARNING: WARN:  Column[T3AMP]	    Max Absolute Error=9.740878893424454E-21	Max Relative Error=3.613928543746403E-14
                 WARNING: WARN:  Column[T3AMPERR]	Max Absolute Error=3.441071348220595E-22	Max Relative Error=3.6192130029721625E-14
                 WARNING: WARN:  Column[T3PHI]	    Max Absolute Error=1.8332002582610585E-12	Max Relative Error=1.6154567952731343E-13
                 */
                // QUICK provides only a preview (not accurate on VISPHI / T3PHI:
                /*
                 WARNING: WARN:  Column[VISDATA]	Max Absolute Error=0.21170902252197266	    Max Relative Error=155.32336222596265
                 WARNING: WARN:  Column[VISERR]	    Max Absolute Error=1.7113983631134033E-5	Max Relative Error=5.123198196063256E-4
                 WARNING: WARN:  Column[VISAMP]	    Max Absolute Error=0.012389325449663476	    Max Relative Error=0.9827257597222762
                 WARNING: WARN:  Column[VISAMPERR]	Max Absolute Error=4.3704479622192665E-4	Max Relative Error=0.24928702639103537
                 WARNING: WARN:  Column[VISPHI]	    Max Absolute Error=357.76365704000574	    Max Relative Error=132.85957062222084
                 WARNING: WARN:  Column[VISPHIERR]	Max Absolute Error=54.185925961293876	    Max Relative Error=0.9643291278418655
                 WARNING: WARN:  Column[VIS2DATA]	Max Absolute Error=4.3211132008532375E-4	Max Relative Error=3199.047758503112
                 WARNING: WARN:  Column[VIS2ERR]	Max Absolute Error=7.815369519747367E-9	    Max Relative Error=6.780968471059581E-5
                 WARNING: WARN:  Column[T3AMP]	    Max Absolute Error=1.8693429580914967E-7	Max Relative Error=0.11262751096020436
                 WARNING: WARN:  Column[T3AMPERR]	Max Absolute Error=4.423268697588574E-11	Max Relative Error=0.0016543021640061009
                 WARNING: WARN:  Column[T3PHI]	    Max Absolute Error=6.734992735788779	    Max Relative Error=6.813763216810152
                 */

                if (logger.isDebugEnabled()) {
                    logger.debug("computeModelVisibilities: MathMode = {}.", mathMode);
                }

                // define mapping between spectral channels and model images:
                final List<UserModelData> modelDataList = target.getUserModel().getModelDataList();

                // determine which images to use according to wavelength range:
                final List<UserModelComputePart> modelParts = mapUserModel(modelDataList, sampleWaveLengths);

                if (modelParts == null) {
                    // invalid wavelength range:
                    return false;
                }

                if (logger.isDebugEnabled()) {
                    for (UserModelComputePart modelPart : modelParts) {
                        logger.debug("modelPart: {}", modelPart);
                        logger.debug("waveLength min: {}", sampleWaveLengths[modelPart.fromWL]);
                        logger.debug("waveLength max: {}", sampleWaveLengths[modelPart.endWL - 1]);
                    }
                }

                // enable parallel jobs if many points using user model:
                final int nTh = (!jobExecutor.isWorkerThread() && (nPoints > JOB_THRESHOLD_USER_MODELS)) ? jobExecutor.getMaxParallelJob() : 1;

                // Prepare thread context variables:
                final int[][] nTaskThreads = (SHOW_COMPUTE_STATS) ? new int[nTh][16] : null; // cache line padding

                // adjust largest chunk size:
                final int nPixels = 80000; // less than 1 Mb

                // computation tasks = 1 job per row and chunk (work stealing):
                final List<Runnable> jobList = new ArrayList<Runnable>(nRows * 2 * modelParts.size()); // 2 chunks by default

                // Iterate on wavelength ranges i.e. UserModelComputePart:
                for (UserModelComputePart modelPart : modelParts) {

                    // use image corresponding to the model part:
                    final UserModelData modelData = modelPart.modelData;

                    if (logger.isDebugEnabled()) {
                        logger.debug("computeModelVisibilities: model part: {}", modelPart);
                    }

                    // process wavelengths:
                    final int from = modelPart.fromWL;
                    final int end = modelPart.endWL;

                    // This will change for each image in the Fits cube:
                    final int n1D = modelData.getNData(); // data, xfreq, yfreq

                    if (logger.isDebugEnabled()) {
                        logger.debug("computeModelVisibilities: {} bytes for image arrays", 4 * n1D); // (float) array
                    }

                    int chunk = nPixels * UserModelService.DATA_1D_POINT_SIZE;

                    final int nChunks = 1 + n1D / chunk;

                    if (logger.isDebugEnabled()) {
                        logger.debug("computeModelVisibilities: {} chunks", nChunks);
                    }

                    // note: chunk must a multiple of 3: see UserModelService.DATA_1D_POINT_SIZE
                    chunk = UserModelService.DATA_1D_POINT_SIZE * ((n1D / nChunks) / UserModelService.DATA_1D_POINT_SIZE);

                    if (logger.isDebugEnabled()) {
                        logger.debug("computeModelVisibilities: {} bytes for chunk", (chunk > n1D) ? 4 * n1D : 4 * chunk);// (float) array
                        logger.debug("computeModelVisibilities: chunk = {}", chunk);
                    }

                    final int[] fromThreads = new int[nChunks];
                    final int[] endThreads = new int[nChunks];

                    for (int c = 0; c < nChunks; c++) {
                        fromThreads[c] = c * chunk;
                        endThreads[c] = fromThreads[c] + chunk;
                    }
                    endThreads[nChunks - 1] = n1D;

                    // create tasks:
                    for (int c = 0; c < nChunks; c++) {
                        // Image chunks:
                        final int fromData = fromThreads[c];
                        final int endData = endThreads[c];

                        for (int k = 0; k < nRows; k++) {
                            // rows index to be processed by this task:
                            final int rowIndex = k;
                            final float[] data1D = modelData.getData1D();
                            final double[] uRow = ufreq[rowIndex];
                            final double[] vRow = vfreq[rowIndex];
                            final MutableComplex[] cmVisRow = cmVis[rowIndex];

                            jobList.add(new Runnable() {
                                /**
                                 * Called by the ParallelJobExecutor to perform task computation
                                 */
                                @Override
                                public void run() {
                                    // Compute complex visibility using the target model:
                                    UserModelService.computeModel(data1D, fromData, endData, uRow, vRow, cmVisRow, from, end, mathMode);

                                    if (SHOW_COMPUTE_STATS) {
                                        // Get thread index to get appropriate thread vars:
                                        final int threadIndex = ParallelJobExecutor.currentThreadIndex(nTh);
                                        nTaskThreads[threadIndex][0]++;
                                    }
                                }
                            });
                        }
                    }

                }

                final int nJobs = jobList.size();

                // note: have jobs a multiple of nTh to maximize parallelism !
                logger.debug("computeModelVisibilities: {} jobs", nJobs);

                final Runnable[] jobs = jobList.toArray(new Runnable[nJobs]);

                // execute jobs in parallel or using current thread if only one job (throws InterruptedJobException if interrupted):
                jobExecutor.forkAndJoin("OIFitsCreatorService.computeModelVisibilities", jobs);

                if (SHOW_COMPUTE_STATS) {
                    for (int t = 0; t < nTh; t++) {
                        logger.info("Thread[{}] done: {} processed jobs", t, nTaskThreads[t][0]);
                    }
                }
            }

            if (currentThread.isInterrupted()) {
                // fast interrupt :
                return false;
            }

            // Sampling integration on spectral channels:
            // use integration of several samples
            final ImmutableComplex[][] cVis = new ImmutableComplex[nRows][nChannels];

            // Super sampling ?
            if (sampleWaveLengths == this.waveLengths) {
                // Simple toShort array:
                // Iterate on rows :
                for (int k = 0; k < nRows; k++) {
                    // Simple copy array:
                    cVis[k] = convertArray(cmVis[k], nChannels);
                }
            } else {

                // Prepare mapping of sampled channels:
                final int[] fromWL = new int[nChannels];
                final int[] endWL = new int[nChannels];

                for (int l = 0; l < nChannels; l++) {
                    // Note: sometimes sub channels may overlap channel limits
                    final double halfBand = 0.5d * this.waveBands[l];
                    final double wlMin = this.waveLengths[l] - halfBand;
                    final double wlMax = this.waveLengths[l] + halfBand;

                    fromWL[l] = -1;
                    endWL[l] = -1;

                    for (int i = 0; i < nWLen; i++) {
                        if (sampleWaveLengths[i] < wlMin) {
                            continue;
                        }
                        if (sampleWaveLengths[i] > wlMax) {
                            endWL[l] = i;
                            break;
                        }
                        if (fromWL[l] == -1) {
                            fromWL[l] = i;
                        }
                    }
                    if (endWL[l] == -1) {
                        endWL[l] = nWLen;
                    }
                }

                final double[] normFactorWL = new double[nChannels];

                for (int l = 0; l < nChannels; l++) {
                    normFactorWL[l] = 1d / (endWL[l] - fromWL[l]);
                }

                final MutableComplex integrator = new MutableComplex();

                // Iterate on rows :
                for (int i, k = 0, l; k < nRows; k++) {
                    final MutableComplex[] cmVisRow = cmVis[k];
                    final ImmutableComplex[] cVisRow = cVis[k];

                    // Iterate on spectral channels:
                    for (l = 0; l < nChannels; l++) {
                        // reset
                        integrator.updateComplex(0d, 0d);

                        for (i = fromWL[l]; i < endWL[l]; i++) {
                            integrator.add(cmVisRow[i]);
                        }
                        // normalize:
                        integrator.multiply(normFactorWL[l]);

                        cVisRow[l] = new ImmutableComplex(integrator); // immutable for safety
                    }
                }
            }

            final StatUtils stat = StatUtils.getInstance();
            if (ns != null) {
                // prepare enough distributions for all baselines:
                stat.prepare(nBl);
            }

            final double[][] cVisError = new double[nRows][nChannels];
            final boolean[][] cVisSnrFlag = new boolean[nRows][nChannels];
            final ComplexDistribution[] cVisRndDist = new ComplexDistribution[nRows];

            // use the same sample index per ha point (as distributions are different instances)
            // to ensure T3 sample is correlated with C12, C23, C13 samples:
            final int[][] cVisRndIdx = new int[nRows][];

            int prevPt = -1;

            // Iterate on rows :
            for (int k = 0, l; k < nRows; k++) {
                final double[] cVisErrorRow = cVisError[k];
                final boolean[] cVisSnrFlagRow = cVisSnrFlag[k];

                if (ns == null) {
                    Arrays.fill(cVisErrorRow, Double.NaN);
                    Arrays.fill(cVisSnrFlagRow, true);
                } else {
                    final ImmutableComplex[] cVisRow = cVis[k];
                    // select a different complex distribution per row:
                    cVisRndDist[k] = stat.get();

                    double visAmp, visErr;
                    // Iterate on spectral channels:
                    for (l = 0; l < nChannels; l++) {
                        visAmp = cVisRow[l].abs();

                        // complex visibility error or Complex.NaN:
                        cVisErrorRow[l] = visErr = ns.computeVisComplexErrorValue(ptIdx[k], l, visAmp);

                        if (NoiseService.USE_DISTRIB_APPROACH) {
                            // check SNR:
                            if ((visErr > 0.1) && Math.abs(visAmp / visErr) < SNR_THRESHOLD) {
                                cVisSnrFlagRow[l] = true;
                            }
                        }
                    }

                    if (ptIdx[k] != prevPt) {
                        prevPt = ptIdx[k];

                        if (this.doNoise) {
                            final int[] cVisRndIdxRow = cVisRndIdx[k] = new int[nChannels];
                            for (l = 0; l < nChannels; l++) {
                                // Choose the jth sample (uniform probability):
                                cVisRndIdxRow[l] = this.random.nextInt(N_SAMPLES);
                            }
                        }
                    } else {
                        // use same random indexes:
                        cVisRndIdx[k] = cVisRndIdx[k - 1];
                    }
                }
            }

            computed = true;
            this.visComplex = cVis;
            this.visError = cVisError;
            this.visRndDist = cVisRndDist;
            this.visRndIdx = cVisRndIdx;
            this.visSnrFlag = cVisSnrFlag;

            logger.info("computeModelVisibilities: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        }
        return computed;
    }

    /**
     * Map user model images on the instrumental spectral channeld
     * @param modelDataList user model images
     * @param sampleWaveLengths sampled instrument spectral channels
     * @return UserModelComputePart list
     */
    private List<UserModelComputePart> mapUserModel(final List<UserModelData> modelDataList, final double[] sampleWaveLengths) {

        final int nImages = modelDataList.size();
        final UserModelData modelDataFirst = modelDataList.get(0);

        // images have wavelength ?
        final boolean isWL = !Double.isNaN(modelDataFirst.getWaveLength());

        final int nWLen = sampleWaveLengths.length;

        final List<UserModelComputePart> modelParts = new ArrayList<UserModelComputePart>(nWLen);

        if (!isWL && nImages > 1) {
            // Fits cube without wavelengths => cancel OIFits computation:
            return null;
        }

        if (!isWL || nImages == 1) {
            // use single image only (gray model):
            final UserModelComputePart part = new UserModelComputePart();
            part.modelData = modelDataFirst;
            part.fromWL = 0;
            part.endWL = nWLen;

            modelParts.add(part);

        } else {
            // Process wavelength ranges:
            UserModelComputePart current = null;
            UserModelData modelData;

            for (int i = 0; i < nWLen; i++) {
                modelData = findUserModel(sampleWaveLengths[i], modelDataList);

                if (current == null || current.modelData != modelData) {
                    // different model image:
                    current = new UserModelComputePart();
                    current.modelData = modelData;
                    current.fromWL = i;
                    current.endWL = i + 1;

                    modelParts.add(current);

                } else {
                    // increase endWL:
                    current.endWL = i + 1;
                }
            }
        }

        return modelParts;
    }

    /**
     * Find the user model data corresponding to the closest spectral channel
     * @param wavelength spectral channel wavelength
     * @param modelDataList user model data
     * @return user model data corresponding to the closest spectral channel
     */
    private static UserModelData findUserModel(final double wavelength, final List<UserModelData> modelDataList) {

        UserModelData modelData = modelDataList.get(0);

        // test first user model image:
        if (wavelength <= modelData.getWaveLengthRange().getMax()) {
            return modelData;
        }

        final int nImages = modelDataList.size();

        modelData = modelDataList.get(nImages - 1);

        if (wavelength >= modelData.getWaveLengthRange().getMin()) {
            return modelData;
        }

        // suppose that model image wavelength ranges do not overlap (true for fitscube):
        for (int i = 0; i < nImages; i++) {
            modelData = modelDataList.get(i);

            if (modelData.getWaveLengthRange().contains(wavelength)) {
                return modelData;
            }
        }
        throw new IllegalStateException("findUserModel: unable for find an user model at wavelength = "
                + convertWL(wavelength) + " " + SpecialChars.UNIT_MICRO_METER);
    }

    /**
     * Convert mutable complex array to immutable complex array for safety reasons
     * @param array mutable complex array
     * @param length array length
     * @return immutable complex array
     */
    private static ImmutableComplex[] convertArray(final MutableComplex[] array, final int length) {
        if (array == null) {
            return null;
        }
        final ImmutableComplex[] result = new ImmutableComplex[length];

        for (int i = length - 1; i >= 0; i--) {
            result[i] = new ImmutableComplex(array[i]); // immutable complex for safety
        }
        return result;
    }

    /**
     * Copy mutable complex array
     * @param array mutable complex array
     * @param dest mutable complex array
     */
    private static void copyArray(final MutableComplex[] array, final MutableComplex[] dest) {
        if (array == null) {
            return;
        }
        final int length = array.length;

        for (int i = length - 1; i >= 0; i--) {
            dest[i].updateComplex(array[i]);
        }
    }

    /**
     * Create a mutable complex array
     * @param length array length
     * @return mutable complex array
     */
    private static MutableComplex[] createArray(final int length) {
        final MutableComplex[] result = new MutableComplex[length];

        for (int i = length - 1; i >= 0; i--) {
            result[i] = new MutableComplex();
        }
        return result;
    }

    /**
     * Create the OI_VIS table using internal computed visComplex data
     */
    private void createOIVis() {
        final long start = System.nanoTime();

        /** Get the current thread to check if the computation is interrupted */
        final Thread currentThread = Thread.currentThread();

        // test if the instrument is AMBER to use dedicated diffVis algorithm :
        final boolean isAmber = AsproConstants.INS_AMBER.equals(this.instrumentName);

        // Create OI_VIS table :
        final OIVis vis = new OIVis(this.oiFitsFile, this.insNameKeyword, this.nObsPoints * this.nBaseLines);
        vis.setArrName(this.arrNameKeyword);

        // Get target information for each UV point:
        final TargetPointInfo[] obsPointInfos = this.targetPointInfos;

        // Compute UTC start date of the first point :
        final Calendar calObs = this.sc.toCalendar(obsPointInfos[0].getJd(), false);

        final String dateObs = calendarToString(calObs);
        vis.setDateObs(dateObs);

        // Columns :
        final short[] targetIds = vis.getTargetId();
        final double[] times = vis.getTime();
        final double[] mjds = vis.getMjd();
        final double[] intTimes = vis.getIntTime();

        final float[][][] visData = vis.getVisData();
        final float[][][] visErr = vis.getVisErr();

        final double[][] visAmp = vis.getVisAmp();
        final double[][] visAmpErr = vis.getVisAmpErr();

        final double[][] visPhi = vis.getVisPhi();
        final double[][] visPhiErr = vis.getVisPhiErr();

        final double[] uCoords = vis.getUCoord();
        final double[] vCoords = vis.getVCoord();

        final short[][] staIndexes = vis.getStaIndex();
        final boolean[][] flags = vis.getFlag();

        // complex visiblity with noise (sigma = visError) used by AMBER:
        final Complex[][] visComplexNoisy = new Complex[vis.getNbRows()][this.nWaveLengths];

        final int nPoints = this.nObsPoints * this.nBaseLines * this.nWaveLengths;

        if (this.hasModel && this.errorValid && !isAmber && this.instrumentExperimental) {
            logger.info("createOIVis: {} points - experimental instrument: VisAmp/Phi errors computed using {} random complex visiblities",
                    nPoints, N_SAMPLES);
        }

        // vars:
        double jd, time, mjd;
        double u, v;
        double visRe, visIm, flux, visErrCplx;
        double vamp, vphi, errAmp, errPhi;

        final NoiseService ns = (errorValid) ? noiseService : null;

        // distribution samples:
        double[] distRe = null, distIm = null;

        // try resampling:
        double re, im, sample, diff;
        double vamp_sum, vamp_sum_diff, vamp_sum_diff_square;
        double s_vamp_mean, s_vamp_err;
        double vphi_sum_cos, vphi_sum_sin;
        double vphi_sum_diff, vphi_sum_diff_square;
        double s_vphi_mean, s_vphi_err;

        final double[] vamp_samples = new double[N_SAMPLES];
        final double[] vphi_samples = new double[N_SAMPLES];
        int[] visRndIdxRow = null;
        boolean doFlag;

        // Iterate on observable UV points :
        for (int i = 0, j, k, l, n; i < nObsPoints; i++) {

            // jd at the observed uv point:
            jd = obsPointInfos[i].getJd();

            // UTC :
            time = calendarToTime(sc.toCalendar(jd, false), calObs);

            // modified julian day :
            mjd = AstroSkyCalc.mjd(jd);

            j = 0;

            // Iterate on baselines :
            for (final UVRangeBaseLineData uvBL : targetUVObservability) {

                k = nBaseLines * i + j;

                // target id
                targetIds[k] = TARGET_ID;

                // UTC :
                times[k] = time;

                // modified julian day :
                mjds[k] = mjd;

                // integration time (s) :
                intTimes[k] = integrationTime;

                // UV coords (m) :
                u = uvBL.getU()[i];
                v = uvBL.getV()[i];

                uCoords[k] = u;
                vCoords[k] = v;

                // if target has a model, then complex visibility are computed :
                if (!hasModel) {
                    // Invalid => NaN value :

                    // Iterate on wave lengths :
                    for (l = 0; l < nWaveLengths; l++) {
                        visData[k][l][0] = Float.NaN;
                        visData[k][l][1] = Float.NaN;

                        visErr[k][l][0] = Float.NaN;
                        visErr[k][l][1] = Float.NaN;

                        visAmp[k][l] = Double.NaN;
                        visAmpErr[k][l] = Double.NaN;

                        visPhi[k][l] = Double.NaN;
                        visPhiErr[k][l] = Double.NaN;

                        // mark this value as invalid :
                        flags[k][l] = true;
                    }

                } else {
                    if (ns != null) {
                        // Get the complex distribution for this row:
                        distRe = visRndDist[k].getSamples()[0];
                        distIm = visRndDist[k].getSamples()[1];

                        // Get proper index:
                        visRndIdxRow = this.visRndIdx[k];
                    }

                    // Iterate on wave lengths :
                    for (l = 0; l < this.nWaveLengths; l++) {
                        // Get pure complex visibility:
                        visComplexNoisy[k][l] = visComplex[k][l];

                        doFlag = visSnrFlag[k][l];

                        // Define first visData / visErr:
                        if (ns == null) {
                            visData[k][l][0] = Float.NaN;
                            visData[k][l][1] = Float.NaN;

                            visErr[k][l][0] = Float.NaN;
                            visErr[k][l][1] = Float.NaN;
                        } else {
                            // pure complex visibility data :
                            visRe = this.visComplex[k][l].getReal();
                            visIm = this.visComplex[k][l].getImaginary();

                            // complex visibility error : visErrRe = visErrIm = visAmpErr or Complex.NaN :
                            visErrCplx = visError[k][l];

                            if (doNoise) {
                                // Use the corresponding sample:
                                visComplexNoisy[k][l] = new ImmutableComplex(
                                        visRe + (visErrCplx * distRe[visRndIdxRow[l]]),
                                        visIm + (visErrCplx * distIm[visRndIdxRow[l]])
                                ); // immutable complex for safety
                            }

                            // pure correlated fluxes or NaN:
                            flux = ns.computeCorrelatedFluxWeight(i, l);

                            // store pure (0..1) or noisy correlated fluxes (NaN if no flux):
                            visData[k][l][0] = (float) (flux * visComplexNoisy[k][l].getReal());
                            visData[k][l][1] = (float) (flux * visComplexNoisy[k][l].getImaginary());

                            // error on correlated fluxes :
                            visErr[k][l][0] = (float) (flux * visErrCplx);
                            visErr[k][l][1] = (float) (flux * visErrCplx);
                        }

                        if (!isAmber) {
                            if (instrumentExperimental) {

                                // For experimental instruments: VisAmp/Phi are only amplitude and phase of complex visibility:
                                if (ns == null) {
                                    vamp = visComplex[k][l].abs();
                                    vphi = visComplex[k][l].getArgument();

                                    errAmp = errPhi = Double.NaN;

                                } else {
                                    // pure complex visibility data :
                                    visRe = visComplex[k][l].getReal();
                                    visIm = visComplex[k][l].getImaginary();

                                    // complex visibility error : visErrRe = visErrIm = visAmpErr or Complex.NaN :
                                    visErrCplx = visError[k][l];

                                    // pure visibility amplitude:
                                    vamp = Math.sqrt(visRe * visRe + visIm * visIm);

                                    // pure visibility phase:
                                    vphi = (visIm != 0.0) ? FastMath.atan2(visIm, visRe) : 0.0;

                                    // Sampling complex visibilities:
                                    vamp_sum = vamp_sum_diff = vamp_sum_diff_square = 0.0;
                                    vphi_sum_cos = vphi_sum_sin = 0.0;

                                    // bivariate distribution (complex normal):
                                    for (n = 0; n < N_SAMPLES; n++) {
                                        // update nth sample:
                                        re = visRe + (visErrCplx * distRe[n]);
                                        im = visIm + (visErrCplx * distIm[n]);

                                        // amplitude:
                                        sample = Math.sqrt(re * re + im * im);
                                        vamp_samples[n] = sample;

                                        // Compensated-summation variant for better numeric precision:
                                        vamp_sum += sample;
                                        diff = sample - vamp;
                                        vamp_sum_diff += diff;
                                        vamp_sum_diff_square += diff * diff;

                                        // mean angle: compute sum(cos) and sum(sin):
                                        // note: do not check div 0 (should never happen as distrib != pure 0.0):
                                        vphi_sum_cos += re / sample;
                                        vphi_sum_sin += im / sample;

                                        // phase in [-PI; PI]:
                                        sample = (im != 0.0) ? FastMath.atan2(im, re) : 0.0;
                                        vphi_samples[n] = sample;
                                    }

                                    // mean(vamp):
                                    s_vamp_mean = SAMPLING_FACTOR_MEAN * vamp_sum;

                                    // error(vamp):
                                    // note: this algorithm ensures correctness (stable) even if the mean used in diff is wrong !
                                    s_vamp_err = Math.sqrt(
                                            SAMPLING_FACTOR_VARIANCE * (vamp_sum_diff_square - (SAMPLING_FACTOR_MEAN * (vamp_sum_diff * vamp_sum_diff)))
                                    );

                                    // mean(vphi):
                                    s_vphi_mean = (vphi_sum_sin != 0.0) ? FastMath.atan2(vphi_sum_sin, vphi_sum_cos) : 0.0;

                                    vphi_sum_diff = vphi_sum_diff_square = 0.0;

                                    // compute angle variance:
                                    for (n = 0; n < N_SAMPLES; n++) {
                                        sample = vphi_samples[n];

                                        // Compensated-summation variant for better numeric precision:
                                        // check if diff is [-PI; PI]:
                                        diff = distance(sample, s_vphi_mean);
                                        vphi_sum_diff += diff;
                                        vphi_sum_diff_square += diff * diff;
                                    }

                                    // error(vphi):
                                    // note: this algorithm ensures correctness (stable) even if the mean used in diff is wrong !
                                    s_vphi_err = Math.sqrt(
                                            SAMPLING_FACTOR_VARIANCE * (vphi_sum_diff_square - (SAMPLING_FACTOR_MEAN * (vphi_sum_diff * vphi_sum_diff)))
                                    );

                                    if (DEBUG) {
                                        logger.info("Sampling[" + N_SAMPLES + "] snr=" + (s_vamp_mean / s_vamp_err) + " AMP "
                                                + " avg= " + s_vamp_mean + " vamp= " + vamp + " ratio: " + (s_vamp_mean / vamp)
                                                + " stddev= " + s_vamp_err + " errAmp= " + visErrCplx + " ratio: " + (s_vamp_err / visErrCplx)
                                        );
                                        logger.info("Sampling[" + N_SAMPLES + "] snr=" + (s_vphi_mean / s_vphi_err) + " PHI "
                                                + " avg= " + s_vphi_mean + " vphi= " + vphi + " ratio: " + (s_vphi_mean / vphi)
                                                + " stddev= " + s_vphi_err
                                        );
                                    }

                                    if (NoiseService.USE_DISTRIB_APPROACH) {
                                        // check SNR:
                                        // PI / 8  = 22.5 deg:
                                        if ((s_vphi_err > (Math.PI * 0.125)) && (Math.abs(s_vphi_mean / s_vphi_err) < SNR_THRESHOLD)) {
                                            doFlag = true;
                                        }
                                    }

                                    if (this.doNoise) {
                                        // Use the corresponding sample among vis, vis2, t3 on any baselines !
                                        final int nSample = visRndIdxRow[l];
                                        vamp = vamp_samples[nSample];
                                        vphi = vphi_samples[nSample];
                                    } else {
                                        vamp = s_vamp_mean;
                                        vphi = s_vphi_mean;
                                    }
                                    errAmp = s_vamp_err;
                                    errPhi = s_vphi_err;
                                }

                                // Set values:
                                visAmp[k][l] = vamp;
                                visAmpErr[k][l] = errAmp;

                                // toShort errPhi in degrees :
                                visPhi[k][l] = FastMath.toDegrees(vphi);
                                visPhiErr[k][l] = FastMath.toDegrees(errPhi);

                            } else {
                                // Waiting for explanations on every instrument processing to compute VisAmp/Phi:
                                // following values are considered as invalid :
                                visAmp[k][l] = Double.NaN;
                                visAmpErr[k][l] = Double.NaN;

                                visPhi[k][l] = Double.NaN;
                                visPhiErr[k][l] = Double.NaN;
                            }
                        }

                        // mark this value as valid only if error is valid and SNR is OK:
                        flags[k][l] = doFlag;
                    }
                }

                // station indexes :
                staIndexes[k] = baseLineMapping.get(uvBL.getBaseLine());

                // increment j:
                j++;

                // fast interrupt :
                if (currentThread.isInterrupted()) {
                    return;
                }

            } // baselines
        } // HA

        /* Compute visAmp / visPhi as amber does */
        if (isAmber && this.hasModel && this.errorValid) {
            // Should perform that algorithm using sampling ?
            OIFitsAMBERService.amdlibFakeAmberDiffVis(vis, visComplexNoisy, this.visError, this.waveLengths);
        }

        this.oiFitsFile.addOiTable(vis);

        if (logger.isDebugEnabled()) {
            logger.debug("createOIVis: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        }
        logger.info("createOIVis: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
    }

    /**
     * Create the OI_VIS2 table using internal computed visComplex data
     */
    private void createOIVis2() {
        final long start = System.nanoTime();

        // Get OI_VIS table :
        final OIVis vis = this.oiFitsFile.getOiVis()[0];
        final int nRows = vis.getNbRows();

        // Create OI_VIS2 table :
        final OIVis2 vis2 = new OIVis2(this.oiFitsFile, this.insNameKeyword, nRows);
        vis2.setArrName(this.arrNameKeyword);
        vis2.setDateObs(vis.getDateObs());

        // Columns :
        System.arraycopy(vis.getTargetId(), 0, vis2.getTargetId(), 0, nRows);
        System.arraycopy(vis.getTime(), 0, vis2.getTime(), 0, nRows);
        System.arraycopy(vis.getMjd(), 0, vis2.getMjd(), 0, nRows);
        System.arraycopy(vis.getIntTime(), 0, vis2.getIntTime(), 0, nRows);

        final double[][] vis2Data = vis2.getVis2Data();
        final double[][] vis2Err = vis2.getVis2Err();

        final boolean[][] flags = vis2.getFlag();

        final NoiseService ns = (errorValid) ? noiseService : null;

        // vars:
        double visRe, visIm, errCVis, bias;
        double v2, v2Err;

        // distribution samples:
        double[] distRe = null, distIm = null;

        // try resampling:
        double re, im, sample, diff;
        double v2_sum, v2_sum_diff, v2_sum_diff_square;
        double s_v2_mean, s_v2_err;

        final double[] v2_samples = new double[N_SAMPLES];
        int[] visRndIdxRow = null;
        boolean doFlag;

        // Iterate on observable UV points :
        for (int i = 0, j, k, l, n; i < this.nObsPoints; i++) {
            j = 0;

            // Iterate on baselines :
            for (final UVRangeBaseLineData uvBL : targetUVObservability) {
                k = nBaseLines * i + j;

                // if target has models, then complex visibility are computed :
                if (!this.hasModel) {

                    // Iterate on wave lengths :
                    for (l = 0; l < this.nWaveLengths; l++) {
                        vis2Data[k][l] = Double.NaN;
                        vis2Err[k][l] = Double.NaN;

                        // mark this value as invalid :
                        flags[k][l] = true;
                    }

                } else {
                    if (ns != null) {
                        // Get the complex distribution for this row:
                        distRe = visRndDist[k].getSamples()[0];
                        distIm = visRndDist[k].getSamples()[1];

                        // Get proper index:
                        visRndIdxRow = this.visRndIdx[k];
                    }

                    // Iterate on wave lengths :
                    for (l = 0; l < this.nWaveLengths; l++) {
                        // pure complex visibility data :
                        visRe = this.visComplex[k][l].getReal();
                        visIm = this.visComplex[k][l].getImaginary();

                        // pure square visibility :
                        v2 = visRe * visRe + visIm * visIm;

                        doFlag = visSnrFlag[k][l];

                        if (ns == null) {
                            v2Err = Double.NaN;
                        } else if (NoiseService.USE_DISTRIB_APPROACH) {
                            // Sampling complex visibilities:

                            // complex visibility error : visErrRe = visErrIm = visAmpErr or Complex.NaN :
                            errCVis = visError[k][l];
                            // bias = var(re) + var(im) = 2.0 * var(visAmpErr)
                            bias = 2.0 * errCVis * errCVis;

                            // Remove v2 bias:
                            v2 -= bias;

                            v2_sum = v2_sum_diff = v2_sum_diff_square = 0.0;

                            // bivariate distribution (complex normal):
                            for (n = 0; n < N_SAMPLES; n++) {
                                // update nth sample:
                                re = visRe + (errCVis * distRe[n]);
                                im = visIm + (errCVis * distIm[n]);

//                  biased_CV = amdlibPow2(R[nbGoodFrames]) + amdlibPow2(I[nbGoodFrames]);
                                /* bias: see formula AMB-IGR-019 number 36 */
//                    bias = sigma2_R[nbGoodFrames] + sigma2_I[nbGoodFrames];
                                /*
                     * <R^2 + I^2 - bias>_fram =
                     * <R^2>_fram + <I^2>_fram - <bias>_fram averaged =
                     * <C^2 - bias>
                                 */
//                    unbiased_CV[nbGoodFrames] = biased_CV - bias;
                                // compute unbiased V2 = re^2 + im^2 - 2 * varCVis:
                                sample = re * re + im * im - bias;
                                v2_samples[n] = sample;

                                // Compensated-summation variant for better numeric precision:
                                v2_sum += sample;
                                diff = sample - v2;
                                v2_sum_diff += diff;
                                v2_sum_diff_square += diff * diff;
                            }

                            // mean(V2):
                            s_v2_mean = SAMPLING_FACTOR_MEAN * v2_sum;

                            // error(V2):
                            // note: this algorithm ensures correctness (stable) even if the mean used in diff is wrong !
                            s_v2_err = Math.sqrt(
                                    SAMPLING_FACTOR_VARIANCE * (v2_sum_diff_square - (SAMPLING_FACTOR_MEAN * (v2_sum_diff * v2_sum_diff)))
                            );

                            if (DEBUG) {
                                // square visibility error :
                                v2Err = ns.computeVis2Error(i, l, Math.sqrt(v2)); // TODO: KILL

                                logger.info("Sampling[" + N_SAMPLES + "] snr=" + (s_v2_mean / s_v2_err) + " V2"
                                        + " (err(re,im)= " + errCVis + ")"
                                        + " avg= " + s_v2_mean + " V2= " + v2 + " ratio: " + (s_v2_mean / v2)
                                        + " stddev= " + s_v2_err + " err(V2)= " + v2Err + " ratio: " + (s_v2_err / v2Err)
                                );
                            }

                            if (NoiseService.USE_DISTRIB_APPROACH) {
                                // check SNR:
                                if ((s_v2_err > 0.1) && Math.abs(s_v2_mean / s_v2_err) < SNR_THRESHOLD) {
                                    doFlag = true;
                                }
                            }

                            if (this.doNoise) {
                                // Use the corresponding sample:
                                v2 = v2_samples[visRndIdxRow[l]];
                            } else {
                                v2 = s_v2_mean;
                            }
                            v2Err = s_v2_err;

                        } else {
                            // Old approach:
                            // square visibility error :
                            v2Err = ns.computeVis2Error(i, l, Math.sqrt(v2));

                            if (this.doNoise) {
                                // add gaussian noise with sigma = err :
                                v2 += v2Err * this.random.nextGaussian();
                            }
                        }

                        // Set values:
                        vis2Data[k][l] = v2;
                        vis2Err[k][l] = v2Err;

                        // mark this value as valid only if error is valid and SNR is OK:
                        flags[k][l] = doFlag;
                    }
                }

                // increment j:
                j++;
            }
        }

        System.arraycopy(vis.getUCoord(), 0, vis2.getUCoord(), 0, nRows);
        System.arraycopy(vis.getVCoord(), 0, vis2.getVCoord(), 0, nRows);

        System.arraycopy(vis.getStaIndex(), 0, vis2.getStaIndex(), 0, nRows);

        this.oiFitsFile.addOiTable(vis2);

        if (logger.isDebugEnabled()) {
            logger.debug("createOIVis2: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        }
        logger.info("createOIVis2: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
    }

    /**
     * Create the OI_T3 table
     */
    private void createOIT3() {
        if (this.nBeams < 3) {
            return;
        }

        final long start = System.nanoTime();

        // number of triplets :
        final List<int[]> iTriplets = CombUtils.generateCombinations(this.nBeams, 3);

        final int nTriplets = iTriplets.size();
        if (nTriplets == 0) {
            return;
        }

        final List<Triplet> triplets = new ArrayList<Triplet>(nTriplets);
        final short[][] orderedbaseLineIndexes = baseLineMapping.values().toArray(new short[baseLineMapping.size()][2]);

        for (int[] idx : iTriplets) {
            triplets.add(Triplet.create(idx, this.beams, this.stationMapping, orderedbaseLineIndexes));
        }

        if (logger.isDebugEnabled()) {
            logger.debug("triplets: {}", triplets);
        }

        // Get OI_VIS table :
        final OIVis vis = this.oiFitsFile.getOiVis()[0];

        // Create OI_T3 table :
        final OIT3 t3 = new OIT3(this.oiFitsFile, this.insNameKeyword, this.nObsPoints * nTriplets);
        t3.setArrName(this.arrNameKeyword);
        t3.setDateObs(vis.getDateObs());

        // OI_VIS Columns :
        final double[] visTimes = vis.getTime();
        final double[] visMjds = vis.getMjd();

        final double[] visUCoords = vis.getUCoord();
        final double[] visVCoords = vis.getVCoord();

        final short[][] visStaIndexes = vis.getStaIndex();

        // OI_T3 Columns :
        final short[] t3TargetIds = t3.getTargetId();
        final double[] t3Times = t3.getTime();
        final double[] t3Mjds = t3.getMjd();
        final double[] intTimes = t3.getIntTime();

        final double[][] t3Amp = t3.getT3Amp();
        final double[][] t3AmpErr = t3.getT3AmpErr();

        final double[][] t3Phi = t3.getT3Phi();
        final double[][] t3PhiErr = t3.getT3PhiErr();

        final double[] t3U1Coords = t3.getU1Coord();
        final double[] t3V1Coords = t3.getV1Coord();

        final double[] t3U2Coords = t3.getU2Coord();
        final double[] t3V2Coords = t3.getV2Coord();

        final short[][] t3StaIndexes = t3.getStaIndex();

        final boolean[][] flags = t3.getFlag();

        final NoiseService ns = (errorValid) ? noiseService : null;

        // The following code use some hypothesis on the OI_VIS table as defined in createOIVis()
        // 1 - the number of rows per HA point corresponds to the number of baselines.
        // 2 - OI_VIS rows have the same ordering than the list of baselines per HA points.
        // vars :
        Complex cvis;
        Complex[] visData12, visData23, visData13;
        double[] visErr12, visErr23, visErr13;
        ComplexDistribution dist12, dist23, dist13;
        boolean[] visSnrFlag12, visSnrFlag23, visSnrFlag13;
        double u12, v12, u23, v23;
        double t3amp, t3phi, errAmp, errPhi;

        // temporary mutable complex:
        int[] relPos;
        int pos;

        // try resampling:
        double t3amp_sum, t3amp_sum_diff, t3amp_sum_diff_square;
        double s_t3amp_mean, s_t3amp_err;
        double t3phi_sum_cos, t3phi_sum_sin;
        double t3phi_sum_diff, t3phi_sum_diff_square;
        double s_t3phi_mean, s_t3phi_err;

        final double[] t3amp_samples = new double[N_SAMPLES];
        final double[] t3phi_samples = new double[N_SAMPLES];
        int[] visRndIdxRow;
        boolean doFlag;

        double[] distRe_12 = null, distIm_12 = null;
        double[] distRe_23 = null, distIm_23 = null;
        double[] distRe_31 = null, distIm_31 = null;

        double visRe12, visIm12, visErrCplx12;
        double visRe23, visIm23, visErrCplx23;
        double visRe31, visIm31, visErrCplx31;

        double t3Re, t3Im, sample, diff;
        double sRe12, sIm12, sRe23, sIm23, sRe31, sIm31;

        // Iterate on observable UV points :
        for (int i = 0, j, k, l, vp, n; i < this.nObsPoints; i++) {

            // position in OI_VIS HA row group :
            vp = this.nBaseLines * i;

            j = 0;

            // Iterate on baselines :
            for (Triplet triplet : triplets) {

                k = nTriplets * i + j;

                // target id
                t3TargetIds[k] = TARGET_ID;

                // UTC :
                t3Times[k] = visTimes[vp];

                // modified julian day :
                t3Mjds[k] = visMjds[vp];

                // integration time (s) :
                intTimes[k] = this.integrationTime;

                // Use relative positions to get the 3 complex vectors (AB, BC, AC)
                relPos = triplet.getRelativePosition();

                // Find baseline AB = 12 :
                pos = relPos[0];

                if (logger.isDebugEnabled()) {
                    logger.debug("vis baseline: {}", Arrays.toString(visStaIndexes[vp + pos]));
                    logger.debug("T3  baseline: {}", Arrays.toString(triplet.getBaselineIndexes()[0]));
                }

                // pure complex visibility data :
                visData12 = (this.hasModel) ? this.visComplex[vp + pos] : null;
                visErr12 = (this.hasModel) ? this.visError[vp + pos] : null;
                dist12 = this.visRndDist[vp + pos];
                visSnrFlag12 = this.visSnrFlag[vp + pos];
                u12 = visUCoords[vp + pos];
                v12 = visVCoords[vp + pos];

                // Find baseline BC = 23 :
                pos = relPos[1];

                if (logger.isDebugEnabled()) {
                    logger.debug("vis baseline: {}", Arrays.toString(visStaIndexes[vp + pos]));
                    logger.debug("T3  baseline: {}", Arrays.toString(triplet.getBaselineIndexes()[1]));
                }

                // pure complex visibility data :
                visData23 = (this.hasModel) ? this.visComplex[vp + pos] : null;
                visErr23 = (this.hasModel) ? this.visError[vp + pos] : null;
                dist23 = this.visRndDist[vp + pos];
                visSnrFlag23 = this.visSnrFlag[vp + pos];
                u23 = visUCoords[vp + pos];
                v23 = visVCoords[vp + pos];

                // Find baseline AC = 13 :
                pos = relPos[2];

                if (logger.isDebugEnabled()) {
                    logger.debug("vis baseline: {}", Arrays.toString(visStaIndexes[vp + pos]));
                    logger.debug("T3  baseline: {}", Arrays.toString(triplet.getBaselineIndexes()[2]));
                }

                if (logger.isDebugEnabled()) {
                    logger.debug("UV 13    = ({}, {})", visUCoords[vp + pos], visVCoords[vp + pos]);
                    logger.debug("UV 12+23 = ({}, {})", (u12 + u23), (v12 + v23));
                }

                // pure complex visibility data :
                visData13 = (this.hasModel) ? this.visComplex[vp + pos] : null;
                visErr13 = (this.hasModel) ? this.visError[vp + pos] : null;
                dist13 = this.visRndDist[vp + pos];
                visSnrFlag13 = this.visSnrFlag[vp + pos];

                // Get proper index:
                // note: visRndIdx[12] = visRndIdx[23] = visRndIdx[13] by construction
                visRndIdxRow = this.visRndIdx[vp + pos];

                // if target has models, then complex visibility are computed :
                if (!this.hasModel) {

                    // Iterate on wave lengths :
                    for (l = 0; l < this.nWaveLengths; l++) {
                        t3Amp[k][l] = Double.NaN;
                        t3AmpErr[k][l] = Double.NaN;

                        t3Phi[k][l] = Double.NaN;
                        t3PhiErr[k][l] = Double.NaN;

                        // mark this value as invalid :
                        flags[k][l] = true;
                    }

                } else {
                    if (ns != null) {
                        // Get the complex distributions for the 3 rows:
                        distRe_12 = dist12.getSamples()[0];
                        distIm_12 = dist12.getSamples()[1];

                        distRe_23 = dist23.getSamples()[0];
                        distIm_23 = dist23.getSamples()[1];

                        distRe_31 = dist13.getSamples()[0];
                        distIm_31 = dist13.getSamples()[1];

                        if (dist12 == dist23 || dist23 == dist13 || dist12 == dist13) {
                            logger.warn("Bad distribution associations !");
                        }
                    }

                    // Iterate on wave lengths :
                    for (l = 0; l < this.nWaveLengths; l++) {

                        // baseline AB = 12 :
                        cvis = visData12[l];
                        visRe12 = cvis.getReal();
                        visIm12 = cvis.getImaginary();

                        // baseline BC = 23
                        cvis = visData23[l];
                        visRe23 = cvis.getReal();
                        visIm23 = cvis.getImaginary();

                        // baseline AC = 13 => conjugate for 31 (im = -im)
                        cvis = visData13[l];
                        visRe31 = cvis.getReal();
                        // conjugate for 31 (im = -im)
                        visIm31 = -cvis.getImaginary();

                        // Compute RE/IM bispectrum with C12*C23*~C13 :
//                        ComplexUtils.bispectrum(vis12, vis23, vis31, t3Data);
                        t3Re = visRe12 * visRe23 * visRe31 - visRe12 * visIm23 * visIm31 - visIm12 * visRe23 * visIm31 - visIm12 * visIm23 * visRe31;
                        t3Im = visRe12 * visRe23 * visIm31 + visRe12 * visIm23 * visRe31 + visIm12 * visRe23 * visRe31 - visIm12 * visIm23 * visIm31;

                        // pure amplitude :
                        t3amp = Math.sqrt(t3Re * t3Re + t3Im * t3Im);

                        // pure phase [-PI;PI] in degrees :
                        t3phi = (t3Im != 0.0) ? FastMath.atan2(t3Im, t3Re) : 0.0;

                        doFlag = visSnrFlag12[l] || visSnrFlag23[l] || visSnrFlag13[l];

                        if (ns == null) {
                            errAmp = errPhi = Double.NaN;
                        } else {
                            // Sampling complex visibilities:
                            // complex visibility errors : visErrRe = visErrIm = visAmpErr or Complex.NaN :
                            visErrCplx12 = visErr12[l];
                            visErrCplx23 = visErr23[l];
                            visErrCplx31 = visErr13[l];

                            t3amp_sum = t3amp_sum_diff = t3amp_sum_diff_square = 0.0;
                            t3phi_sum_cos = t3phi_sum_sin = 0.0;

                            // bivariate distribution (complex normal):
                            for (n = 0; n < N_SAMPLES; n++) {
                                // update nth sample:

                                // baseline AB = 12 :
                                sRe12 = visRe12 + (visErrCplx12 * distRe_12[n]);
                                sIm12 = visIm12 + (visErrCplx12 * distIm_12[n]);

                                // baseline BC = 23
                                sRe23 = visRe23 + (visErrCplx23 * distRe_23[n]);
                                sIm23 = visIm23 + (visErrCplx23 * distIm_23[n]);

                                // baseline CA = 31
                                sRe31 = visRe31 + (visErrCplx31 * distRe_31[n]);
                                // Use conjuguate on distribution too:
                                sIm31 = visIm31 - (visErrCplx31 * distIm_31[n]);

                                // Compute RE/IM bispectrum with C12*C23*~C13 :
                                //                        ComplexUtils.bispectrum(vis12, vis23, vis31, t3Data);
                                t3Re = sRe12 * sRe23 * sRe31 - sRe12 * sIm23 * sIm31 - sIm12 * sRe23 * sIm31 - sIm12 * sIm23 * sRe31;
                                t3Im = sRe12 * sRe23 * sIm31 + sRe12 * sIm23 * sRe31 + sIm12 * sRe23 * sRe31 - sIm12 * sIm23 * sIm31;

                                // amplitude :
                                sample = Math.sqrt(t3Re * t3Re + t3Im * t3Im);
                                t3amp_samples[n] = sample;

                                // Compensated-summation variant for better numeric precision:
                                t3amp_sum += sample;
                                diff = sample - t3amp;
                                t3amp_sum_diff += diff;
                                t3amp_sum_diff_square += diff * diff;

                                // mean angle: compute sum(cos) and sum(sin):
                                // note: do not check div 0 (should never happen as distrib != pure 0.0):
                                t3phi_sum_cos += t3Re / sample;
                                t3phi_sum_sin += t3Im / sample;

                                // phase in [-PI; PI]:
                                sample = (t3Im != 0.0) ? FastMath.atan2(t3Im, t3Re) : 0.0;
                                t3phi_samples[n] = sample;
                            }

                            // mean(T3amp):
                            s_t3amp_mean = SAMPLING_FACTOR_MEAN * t3amp_sum;

                            // error(t3amp):
                            // note: this algorithm ensures correctness (stable) even if the mean used in diff is wrong !
                            s_t3amp_err = Math.sqrt(
                                    SAMPLING_FACTOR_VARIANCE * (t3amp_sum_diff_square - (SAMPLING_FACTOR_MEAN * (t3amp_sum_diff * t3amp_sum_diff)))
                            );

                            // mean(T3phi):
                            s_t3phi_mean = (t3phi_sum_sin != 0.0) ? FastMath.atan2(t3phi_sum_sin, t3phi_sum_cos) : 0.0;

                            t3phi_sum_diff = t3phi_sum_diff_square = 0.0;

                            // compute angle variance:
                            for (n = 0; n < N_SAMPLES; n++) {
                                sample = t3phi_samples[n];

                                // Compensated-summation variant for better numeric precision:
                                // check if diff is [-PI; PI]:
                                diff = distance(sample, s_t3phi_mean);
                                t3phi_sum_diff += diff;
                                t3phi_sum_diff_square += diff * diff;
                            }

                            // error(t3phi):
                            // note: this algorithm ensures correctness (stable) even if the mean used in diff is wrong !
                            s_t3phi_err = Math.sqrt(
                                    SAMPLING_FACTOR_VARIANCE * (t3phi_sum_diff_square - (SAMPLING_FACTOR_MEAN * (t3phi_sum_diff * t3phi_sum_diff)))
                            );

                            if (DEBUG) {
                                // phase closure error (rad) :
                                errPhi = ns.computeT3PhiError(i, l, cvis.abs(), cvis.abs(), cvis.abs());

                                // amplitude error t3AmpErr = t3Amp * t3PhiErr :
                                errAmp = t3amp * errPhi;

                                logger.info("Sampling[" + N_SAMPLES + "] snr=" + (s_t3amp_mean / s_t3amp_err) + " AMP "
                                        + " avg= " + s_t3amp_mean + " T3amp= " + t3amp + " ratio: " + (s_t3amp_mean / t3amp)
                                        + " stddev= " + s_t3amp_err + " errAmp= " + errAmp + " ratio: " + (s_t3amp_err / errAmp)
                                );
                                logger.info("Sampling[" + N_SAMPLES + "] snr=" + (s_t3phi_mean / s_t3phi_err) + " PHI "
                                        + " avg= " + s_t3phi_mean + " T3phi= " + t3phi + " ratio: " + (s_t3phi_mean / t3phi)
                                        + " stddev= " + s_t3phi_err + " errPhi= " + errPhi + " ratio: " + (s_t3phi_err / errPhi)
                                );
                            }

                            if (NoiseService.USE_DISTRIB_APPROACH) {
                                // check SNR:
                                // PI / 8  = 22.5 deg:
                                if ((s_t3phi_err > (Math.PI * 0.125)) && (Math.abs(s_t3phi_mean / s_t3phi_err) < SNR_THRESHOLD)) {
                                    doFlag = true;
                                }
                            }

                            if (this.doNoise) {
                                // Use the corresponding sample among vis, vis2, t3 on any baselines !
                                final int nSample = visRndIdxRow[l];
                                t3amp = t3amp_samples[nSample];
                                t3phi = t3phi_samples[nSample];
                            } else {
                                t3amp = s_t3amp_mean;
                                t3phi = s_t3phi_mean;
                            }
                            errAmp = s_t3amp_err;
                            errPhi = s_t3phi_err;
                        }

                        // Set values:
                        t3Amp[k][l] = t3amp;
                        t3AmpErr[k][l] = errAmp;

                        // toShort errPhi in degrees :
                        t3Phi[k][l] = FastMath.toDegrees(t3phi);
                        t3PhiErr[k][l] = FastMath.toDegrees(errPhi);

                        // mark this value as valid only if error is valid and SNR is OK:
                        flags[k][l] = doFlag;
                    }
                }

                // UV 1 coords (m) :
                t3U1Coords[k] = u12;
                t3V1Coords[k] = v12;

                // UV 2 coords (m) :
                t3U2Coords[k] = u23;
                t3V2Coords[k] = v23;

                // station indexes :
                t3StaIndexes[k] = triplet.getTripletIndexes();

                j++;
            }
        }

        this.oiFitsFile.addOiTable(t3);

        if (logger.isDebugEnabled()) {
            logger.debug("createOIT3: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        }
        logger.info("createOIT3: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
    }

    /**
     * Return the station mapping
     * @param stations station list
     * @return station mapping
     */
    private static Map<Station, Short> createStationMapping(final List<Station> stations) {
        // Create Station - index mapping :
        // Note : as Station.hashCode is not implemented, the map acts as an IdentityMap (pointer equality)
        final Map<Station, Short> stationMapping = new IdentityHashMap<Station, Short>(stations.size());

        int i = 0;
        for (Station s : stations) {
            stationMapping.put(s, Short.valueOf((short) (i + 1)));
            i++;
        }

        logger.debug("stationMapping: {}", stationMapping);

        return stationMapping;
    }

    /**
     * Return the beam mapping
     * @param stationMapping station mapping
     * @param beams beam list
     * @return beam mapping
     */
    private static Map<Beam, Short> createBeamMapping(final Map<Station, Short> stationMapping, final List<Beam> beams) {
        // Create Beam - index mapping :
        // Note : as Beam.hashCode is not implemented, the map acts as an IdentityMap (pointer equality)
        final Map<Beam, Short> beamMapping = new IdentityHashMap<Beam, Short>(beams.size());

        for (Beam b : beams) {
            beamMapping.put(b, stationMapping.get(b.getStation()));
        }

        logger.debug("beamMapping: {}", beamMapping);

        return beamMapping;
    }

    /**
     * Return the station indexes (2) per base line mapping (ordered)
     * @param beamMapping beam mapping
     * @param baseLines base line list
     * @return baseline station indexes
     */
    private static Map<BaseLine, short[]> createBaseLineMapping(final Map<Beam, Short> beamMapping, final List<BaseLine> baseLines) {
        // Create BaseLine - indexes mapping :
        // Note : as BaseLine.hashCode is not implemented, the map acts as an IdentityMap (pointer equality)
        final Map<BaseLine, short[]> baseLineIndexes = new LinkedHashMap<BaseLine, short[]>(baseLines.size());

        for (BaseLine bl : baseLines) {
            baseLineIndexes.put(bl, new short[]{
                beamMapping.get(bl.getBeam1()).shortValue(),
                beamMapping.get(bl.getBeam2()).shortValue()
            });
        }

        if (logger.isDebugEnabled()) {
            logger.debug("BaseLine indexes = ");
            for (short[] idx : baseLineIndexes.values()) {
                logger.debug("\t{} {}", idx[0], idx[1]);
            }
        }

        return baseLineIndexes;
    }

    /**
     * Convert UTC time in seconds
     * @param cal UTC time
     * @param calObs UTC start date of observation
     * @return UTC time in seconds
     */
    private static double calendarToTime(final Calendar cal, final Calendar calObs) {
        final double time = 3600d * cal.get(Calendar.HOUR_OF_DAY)
                + 60d * cal.get(Calendar.MINUTE)
                + cal.get(Calendar.SECOND);

        if (cal.get(Calendar.DAY_OF_MONTH) != calObs.get(Calendar.DAY_OF_MONTH)) {
            // observation is over 2 UTC days = observation starts before midnight and ends after :
            return 86400d + time;
        }
        return time;
    }

    /**
     * Convert UTC date to string 'YYYY-MM-DD'
     * @param cal UTC date
     * @return string representation
     */
    private static String calendarToString(final Calendar cal) {
        final StringBuilder sb = new StringBuilder(12);
        sb.append(cal.get(Calendar.YEAR)).append('-');

        final int month = cal.get(Calendar.MONTH) + 1;
        if (month < 10) {
            sb.append('0');
        }
        sb.append(month).append('-');

        final int day = cal.get(Calendar.DAY_OF_MONTH);
        if (day < 10) {
            sb.append('0');
        }
        sb.append(day);

        return sb.toString();

    }

    /**
     * Simple object representing a triplet (3 beams A, B, C) with corresponding baselines and table indexes
     */
    private final static class Triplet {

        /** station indexes (1 .. nBeams) */
        private final short[] tripletIndexes;
        /** baseline indexes (3 couples) */
        private final short[][] baselineIndexes;
        /** relative row positions in OI_VIS table (3) */
        private final int[] relativePosition;

        /**
         * Triplet factory method
         * @param idx 0-based indexes
         * @param beams list of beams
         * @param stationMapping station mapping
         * @param orderedbaseLineIndexes ordered baseline arrays (OI_VIS)
         * @return triplet instance
         */
        static Triplet create(final int[] idx, final List<Beam> beams, final Map<Station, Short> stationMapping, final short[][] orderedbaseLineIndexes) {

            final short[] tIndexes = new short[3];

            for (int i = 0; i < 3; i++) {
                final Beam b = beams.get(idx[i]);
                tIndexes[i] = stationMapping.get(b.getStation()).shortValue();
            }

            // for tIndexes = [123] i.e. ABC
            // couples gives { 12 13 23 } i.e. AB AC BC
            // 3 couples :
            final short[][] bIndexes = new short[3][2];

            for (int i = 0, n = 0; i < 3; i++) {
                for (int j = i + 1; j < 3; j++) {
                    bIndexes[n][0] = tIndexes[i];
                    bIndexes[n][1] = tIndexes[j];
                    n++;
                }
            }

            // Permutations to have { 12 23 13 } i.e. AB BC AC
            // i.e. exchange 2 and 3
            final short[] tmp = bIndexes[1];
            bIndexes[1] = bIndexes[2];
            bIndexes[2] = tmp;

            // Find relative positions in baseline ordering:
            final int[] pos = new int[3];

            final int size = orderedbaseLineIndexes.length;

            short[] find, other;
            for (int n = 0; n < 3; n++) {
                find = bIndexes[n];
                pos[n] = -1;
                for (int i = 0; i < size; i++) {
                    other = orderedbaseLineIndexes[i];

                    if (Arrays.equals(find, other)) {
                        pos[n] = i;
                        break;
                    }
                }
                if (pos[n] == -1) {
                    throw new IllegalStateException("Impossible to find couple [" + find[0] + find[1] + "]");
                }
            }

            return new Triplet(tIndexes, bIndexes, pos);
        }

        /**
         * Protected constructor
         * @param tIndexes station indexes
         * @param bIndexes baseline indexes
         * @param pos relative row positions
         */
        private Triplet(final short[] tIndexes, final short[][] bIndexes, final int[] pos) {
            this.tripletIndexes = tIndexes;

            // 3 couples :
            this.baselineIndexes = bIndexes;

            // 3 positions :
            this.relativePosition = pos;
        }

        /**
         * Return the station indexes
         * @return station indexes (3)
         */
        public short[] getTripletIndexes() {
            return tripletIndexes;
        }

        /**
         * Return the baseline indexes (3 couples)
         * @return baseline indexes
         */
        public short[][] getBaselineIndexes() {
            return baselineIndexes;
        }

        /**
         * Return the relative row positions in OI_VIS table (3)
         * @return relative row positions
         */
        public int[] getRelativePosition() {
            return relativePosition;
        }

        /**
         * Return a string representation for this triplet
         * @return string representation
         */
        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder(32);
            sb.append("Triplet[");

            for (short s : this.tripletIndexes) {
                sb.append(s).append(' ');
            }
            sb.append("]{ ");

            for (short[] b : this.baselineIndexes) {
                sb.append(b[0]).append('-').append(b[1]).append(' ');
            }

            sb.append("} = [ ");

            for (int i : this.relativePosition) {
                sb.append(i).append(' ');
            }

            sb.append(']');
            return sb.toString();
        }
    }

    /**
     * Return the flag to add gaussian noise to OIFits data; true if parameter doDataNoise = true and noise parameters are valid
     * @return flag to add gaussian noise to OIFits data; true if parameter doDataNoise = true and noise parameters are valid
     */
    public boolean isDoNoise() {
        return doNoise;
    }

    /**
     * Return true to use instrument bias; false to compute only theoretical error
     * @return true to use instrument bias; false to compute only theoretical error
     */
    public boolean isUseInstrumentBias() {
        return useInstrumentBias;
    }

    /**
     * Return true if returned errors are valid
     * @return true if returned errors are valid
     */
    public boolean isErrorValid() {
        return errorValid;
    }

    /**
     * Return the noise service
     * @return noise service
     */
    public NoiseService getNoiseService() {
        return noiseService;
    }

    /**
     * Add a warning message in the OIFits file
     * @param warningContainer warning container to fill
     * @param msg message to add
     */
    private static void addWarning(final WarningContainer warningContainer, final String msg) {
        warningContainer.addWarningMessage(msg);
    }

    /**
     * Add an information message in the OIFits file
     * @param warningContainer warning container to fill
     * @param msg message to add
     */
    private static void addInformation(final WarningContainer warningContainer, final String msg) {
        warningContainer.addInformationMessage(msg);
    }

    /**
     * Return the OIFits supersampling preference
     * @return OIFits supersampling preference
     */
    public int getSupersamplingOIFits() {
        return supersamplingOIFits;
    }

    /**
     * Return the OIFits MathMode preference
     * @return OIFits MathMode preference
     */
    public MathMode getMathModeOIFits() {
        return mathModeOIFits;
    }

    /**
     * Return the given wavelength rounded in microns
     * @param wl wavelength to toShort
     * @return given wavelength rounded in microns
     */
    private static double convertWL(final double wl) {
        return NumberUtils.trimTo5Digits(1e6d * wl);
    }

    /**
     * UserModel computation part (wavelength indexes)
     */
    private static final class UserModelComputePart {

        /* members */
        /** user model data */
        UserModelData modelData;
        /** first wavelength index (inclusive) */
        int fromWL;
        /** last wavelength index (exclusive) */
        int endWL;

        /**
         * String representation for debugging purposes
         * @return 
         */
        @Override
        public String toString() {
            return "UserModelComputePart[" + fromWL + " - " + endWL + "]: " + modelData;
        }
    }

    private static double distance(final double a1, final double a2) {
        final double delta = a1 - a2;
        if (delta > Math.PI) {
            return delta - (2.0 * Math.PI);
        } else if (delta < -Math.PI) {
            return delta + (2.0 * Math.PI);
        }
        return delta;
    }
}
