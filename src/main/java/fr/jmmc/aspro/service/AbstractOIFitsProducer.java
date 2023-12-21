/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.WarningMessage;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.UserModel;
import fr.jmmc.aspro.model.util.TargetRole;
import fr.jmmc.aspro.model.util.UserModelDataComparator;
import fr.jmmc.aspro.service.UserModelService.MathMode;
import fr.jmmc.jmal.ALX;
import fr.jmmc.jmal.Band;
import fr.jmmc.jmcs.util.StatUtils;
import fr.jmmc.jmcs.util.StatUtils.ComplexDistribution;
import fr.jmmc.jmal.complex.ImmutableComplex;
import fr.jmmc.jmal.complex.MutableComplex;
import fr.jmmc.jmal.model.ModelComputeContext;
import fr.jmmc.jmal.model.ModelFunctionComputeContext;
import fr.jmmc.jmal.model.ModelManager;
import fr.jmmc.jmal.model.targetmodel.Model;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.SpecialChars;
import fr.jmmc.jmcs.util.concurrent.ParallelJobExecutor;
import static fr.jmmc.jmcs.util.StatUtils.N_SAMPLES;
import fr.jmmc.jmal.complex.Complex;
import fr.jmmc.jmcs.util.WelfordVariance;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.range.Range;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import net.jafama.FastMath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author bourgesl
 */
public abstract class AbstractOIFitsProducer {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(AbstractOIFitsProducer.class.getName());

    /** flag to enable FT SNR outputs */
    public static final boolean TRACE_SNR_FT = "true".equalsIgnoreCase(System.getProperty("TRACE_SNR_FT", "false"));

    /** use sampled mean(sample) instead of theoretical value */
    protected final static boolean DO_USE_SAMPLED_MEAN = false;
    /** enable interpolation in image cubes */
    protected final static boolean USE_CUBE_INTERPOLATION = true;
    /** enable instrument channels restriction on user model */
    protected final static boolean DO_RESTRICTION_MODEL = true;
    /** flag to show compute task statistics */
    protected final static boolean SHOW_COMPUTE_STATS = false;
    /** threshold to use parallel jobs for user models (32 UV points) */
    protected final static int JOB_THRESHOLD_USER_MODELS = 32;
    /** Jmcs Parallel Job executor */
    protected static final ParallelJobExecutor JOB_EXECUTOR = ParallelJobExecutor.getInstance();
    /** supersampling threshold (number of channels) */
    protected final static int SUPER_SAMPLING_THRESHOLD = 100;
    /** 2 x PI */
    protected static final double TWO_PI = 2d * Math.PI;

    /* members */
 /* input */
    /** selected target */
    protected final Target target;
    /** flag indicating the role of this service (SCI or FT) */
    protected final TargetRole targetRole;
    /** OIFits options */
    protected final OIFitsProducerOptions options;

    /* output */
    /** oifits structure */
    protected OIFitsFile oiFitsFile = null;

    /* internal */
    /** instrument name (null before prepare call) */
    protected String instrumentName = null;
    /** base line list */
    protected List<BaseLine> baseLines = null;
    /** target has model (analytical or user model) */
    protected final boolean hasModel;
    /** wavelengths */
    protected double[] waveLengths;
    /** channel widths */
    protected double[] waveBands;
    /** random generator */
    private final Random random = new Random();
    /** noise service */
    protected NoiseService noiseService = null;
    /** flag to add gaussian noise to OIFits data; true if parameter doDataNoise = true and noise parameters are valid */
    protected boolean doNoise = false;
    /* complex visibility fields */
    protected UVFreqTable freqTable = null;
    /** internal computed complex visibility and error data [row][waveLength] */
    protected VisDataTable dataTable = null;
    /** triplet mapping */
    protected List<Triplet> triplets = null;

    /** warning container used during compute phase */
    protected WarningContainer warningContainerCompute = null;

    /** (optional) FT OIFits creator */
    private final AbstractOIFitsProducer ftOiFitsCreator;

    static {
        if (TRACE_SNR_FT) {
            System.out.printf("[TRACE]\t%s\t%s\t%s\n",
                    "K (mag)", "DIT (ms)", "SNR"
            );
        }
    }

    /**
     * Protected constructor
     * @param target target to process
     * @param targetRole target role of this observation
     * @param options OIFits options
     * @param ftOiFitsCreator optional FT OIFits creator
     */
    protected AbstractOIFitsProducer(final Target target, final TargetRole targetRole, final OIFitsProducerOptions options,
                                     final AbstractOIFitsProducer ftOiFitsCreator) {

        this.target = target;
        this.targetRole = targetRole;

        // use target model:
        this.hasModel = target.hasModel();

        // OIFits options:
        this.options = options;

        this.ftOiFitsCreator = ftOiFitsCreator;

        logger.debug("options: {}", options);
    }

    /**
     * Prepare the user model vs the instrumental spectral configuration
     * @param warningContainer warning container to use if needed
     * @return true if OK; false if user model is invalid (so discard computation)
     */
    protected final boolean prepareUserModel(final WarningContainer warningContainer) {
        if (this.hasModel) {
            final boolean useAnalyticalModel = this.target.hasAnalyticalModel();

            // user model if defined:
            final UserModel userModel = (!useAnalyticalModel) ? target.getUserModel() : null;

            // Test if user model data is valid:
            if ((userModel != null) && userModel.isModelDataReady()) {
                final List<UserModelData> modelDataList = target.getUserModel().getModelDataList();

                final int nImages = modelDataList.size();
                final UserModelData modelDataFirst = modelDataList.get(0);
                final UserModelData modelDataLast = modelDataList.get(nImages - 1);

                // first and last images have wavelength ?
                final boolean isWL = !Double.isNaN(modelDataFirst.getWaveLength()) && !Double.isNaN(modelDataLast.getWaveLength());

                if (!isWL && nImages > 1) {
                    // Fits cube without wavelengths:
                    warningContainer.addWarning("User model (Fits cube) without wavelength information is discarded");
                    return false;
                }

                if (isWL) {
                    final double[] effWaves = this.waveLengths;
                    final double[] effBands = this.waveBands;
                    final int nWaves = effWaves.length;

                    // wavelength array can be unordered:
                    final double lambdaMin = StatUtils.min(this.waveLengths);
                    final double lambdaMax = StatUtils.max(this.waveLengths);

                    final double wlFirst = modelDataFirst.getWaveLength();

                    if (nImages == 1) {
                        // check image wavelength is in instrument range:
                        if (wlFirst < lambdaMin || wlFirst > lambdaMax) {
                            warningContainer.addWarning("User model (Fits image) wavelength ("
                                    + convertWL(wlFirst) + " " + SpecialChars.UNIT_MICRO_METER
                                    + ") outside of instrumental wavelength range");
                            return false;
                        }

                    } else {
                        // Fits cube:
                        final double wlLast = modelDataLast.getWaveLength();
                        final double wlIncMin = modelDataFirst.getWaveLengthIncrement(); // not constant in Fits cube (hertz)

                        warningContainer.addInformation("User model [" + userModel.getName() + "]: " + nImages + " images "
                                + '[' + convertWL(wlFirst) + " - " + convertWL(wlLast) + " " + SpecialChars.UNIT_MICRO_METER + "] "
                                + "(increment: " + convertWL(wlIncMin) + " " + SpecialChars.UNIT_MICRO_METER + ')');

                        // check image wavelengths are overlapping the instrument range:
                        if (modelDataFirst.getWaveLengthRange().getMin() > lambdaMax) {
                            warningContainer.addWarning("Incorrect model min wavelength [" + convertWL(wlFirst) + " " + SpecialChars.UNIT_MICRO_METER
                                    + "] higher than max instrument wavelength [" + convertWL(lambdaMax) + " " + SpecialChars.UNIT_MICRO_METER + ']');
                            return false;
                        }
                        if (modelDataLast.getWaveLengthRange().getMax() < lambdaMin) {
                            warningContainer.addWarning("Incorrect model max wavelength [" + convertWL(wlLast) + " " + SpecialChars.UNIT_MICRO_METER
                                    + "] lower than min instrument wavelength [" + convertWL(lambdaMin) + " " + SpecialChars.UNIT_MICRO_METER + ']');
                            return false;
                        }

                        // navigate among spectral channels (not super sampling):
                        if (logger.isDebugEnabled()) {
                            logger.debug("nWaves: {}", nWaves);
                            logger.debug("effWaves: {}", Arrays.toString(effWaves));
                        }

                        // note: hashset behaves like identity check:
                        final Set<UserModelData> uniqueModelDatas = new HashSet<UserModelData>(nImages);

                        final boolean[] hasUserModelPerChannel = new boolean[nWaves];

                        double wl, wlLower, wlUpper, halfBand;
                        UserModelData modelWlLower, modelWlUpper;

                        for (int i = 0; i < nWaves; i++) {
                            wl = effWaves[i];
                            halfBand = 0.5d * effBands[i];

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

                        if (uniqueModelDatas.isEmpty()) {
                            warningContainer.addWarning("Incorrect model wavelength range [" + convertWL(wlFirst) + " - " + convertWL(wlLast) + " " + SpecialChars.UNIT_MICRO_METER
                                    + "] smaller than the typical instrumental wavelength band [" + convertWL(StatUtils.mean(effBands)) + " " + SpecialChars.UNIT_MICRO_METER + ']');
                            return false;
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
                            warningContainer.addWarning("Sub sampling detected: " + nChannels + " channels for only "
                                    + uniqueModelDatas.size() + " user model image(s) available");
                        }

                        // keep only channels where at least one image is present:
                        if (nWaves > nChannels) {
                            // only part of the instrument channels corresponds to the user model:
                            if (!options.userModelCubeExtrapolation) {
                                // Fix wavelength/waveband:
                                // skip waveband (constant) for now
                                this.waveLengths = new double[nChannels];
                                this.waveBands = new double[nChannels];

                                System.arraycopy(effWaves, firstChannel, this.waveLengths, 0, nChannels);
                                System.arraycopy(effBands, firstChannel, this.waveBands, 0, nChannels);

                                if (logger.isDebugEnabled()) {
                                    logger.debug("waveLengths: {}", Arrays.toString(waveLengths));
                                    logger.debug("waveBands: {}", Arrays.toString(waveBands));
                                }
                            } else {
                                // Extrapolation on boundaries:
                                warningContainer.addInformation("Extrapolation applied on " + (firstChannel) + " lower "
                                        + " and " + (nWaves - 1 - lastChannel) + " upper channels");
                            }
                        }
                    }
                }
            }
        }
        return true;
    }

    protected boolean enableSuperSampling() {
        // note: disable super sampling in high resolution:
        return ((this.waveLengths != null) && (this.waveLengths.length <= SUPER_SAMPLING_THRESHOLD));
    }

    /**
     * Compute complex visibilities using the target model (analytical or user model)
     * and store this data in local reference table
     * @return true if complex visibilities are computed; false otherwise
     */
    protected final boolean computeModelVisibilities() {
        if (!this.hasModel) {
            return false;
        }

        /** Get the current thread to check if the computation is interrupted */
        final Thread currentThread = Thread.currentThread();

        final long start = System.nanoTime();

        final boolean useAnalyticalModel = this.target.hasAnalyticalModel();

        // user model if defined:
        final UserModel userModel = (!useAnalyticalModel) ? target.getUserModel() : null;

        // Test if user model data is valid:
        if ((userModel != null) && !userModel.isModelDataReady()) {
            return false;
        }

        // Determine nSamples per spectral channel:
        // number of samples per spectral channel (1, 5, 9 ...) use the preference (SuperSampling)
        // should be an even number to keep wavelengths centered on each sub channels:
        // use the preference (QUICK, FAST, DEFAULT?) : QUICK = PREVIEW ie No super sampling
        // TODO: determine correctly deltaLambda (object size (FOV) and Bmax/lambda) ie when super sampling is surely necessary
        int nSamples = (!enableSuperSampling() || (options.mathMode == MathMode.QUICK)) ? 1 : options.supersampling;

        if (logger.isDebugEnabled()) {
            logger.debug("computeModelVisibilities: initial nSamples = {}", nSamples);
        }

        final double waveBand = StatUtils.mean(this.waveBands);

        final double deltaLambda = waveBand / nSamples;

        // If Fits cube: try using all images at least i.e. adjust nSamples then frequencies:
        final double wlIncMin = ((userModel != null) && userModel.isModelDataReady()) ? userModel.getModelData(0).getWaveLengthIncrement() : Double.NaN;

        // Sub channel width:
        if (!Double.isNaN(wlIncMin) && (wlIncMin < deltaLambda)) {
            // adjust nSamples to have deltaLambda < wlInc:
            nSamples = (int) Math.ceil(waveBand / wlIncMin);

            if (logger.isDebugEnabled()) {
                logger.debug("computeModelVisibilities: adjusted nSamples = {} (wlIncMin = {})", nSamples, wlIncMin);
            }
            nSamples = Math.min(nSamples, AsproConstants.MAX_SUPER_SAMPLING);
        }

        // Prefer odd number of sub channels:
        if (nSamples % 2 == 0) {
            nSamples++;
        }

        if (logger.isDebugEnabled()) {
            logger.debug("computeModelVisibilities: adjusted nSamples = {}", nSamples);
        }

        // note: integration must be adjusted to use wavelengths in each spectral channel !
        // Only part of instrument spectral channels can be used (see prepare step):
        final double[] sampleWaveLengths = (nSamples > 1) ? resampleWaveLengths(this.waveLengths, this.waveBands, nSamples) : this.waveLengths;

        // Effective number of spectral channels on the detector:
        final int nChannels = this.waveLengths.length;

        // local vars:
        final int nWLen = sampleWaveLengths.length;

        if (logger.isDebugEnabled()) {
            logger.debug("computeModelVisibilities: nWLen = {} - nChannels = {}", nWLen, nChannels);
        }
        // fast interrupt:
        if (currentThread.isInterrupted()) {
            return false;
        }

        // Compute spatial frequencies:
        this.freqTable = computeSpatialFreqTable(sampleWaveLengths);
        if (freqTable == null) {
            return false;
        }

        final double[][] ufreq = freqTable.ufreq;
        final double[][] vfreq = freqTable.vfreq;

        final int nRows = freqTable.nRows;
        final int nDataPoints = nRows * nWLen;

        logger.info("computeModelVisibilities(): {} data points [{} rows - {} spectral channels - {} samples]({}) - please wait ...",
                instrumentName, nDataPoints, nRows, nChannels, nSamples, options.mathMode);

        // Allocate data array for complex visibility and error :
        final MutableComplex[][] cmVis = new MutableComplex[nRows][];

        // Iterate on rows :
        for (int k = 0; k < nRows; k++) {
            cmVis[k] = createArray(nWLen + 4); // cache line padding (Complex = 2 double = 16 bytes) so 4 complex = complete cache line
        }

        final NoiseService ns = this.noiseService;

        /* (W) instrument band */
        Band[] insBands = null;

        // Initialize flux array:
        final double[] mFluxes = new double[nWLen];

        // Flux per used instrument band:
        final Map<Band, Double> bandFluxes = new LinkedHashMap<>(4);

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

            // update flux vs wavelengths:
            Arrays.fill(mFluxes, 1.0); // already normalized

        } else {
            // TODO: compare directFT vs FFT + interpolation

            if (logger.isDebugEnabled()) {
                logger.debug("computeModelVisibilities: MathMode = {}.", options.mathMode);
            }

            if (ns != null) {
                // get used instrument bands:
                insBands = ns.getInstrumentBands();

                // Initialize used bands to get their flux from the user model:
                final Set<Band> usedInsBands = ns.getDistinctInstrumentBands();

                for (Band b : usedInsBands) {
                    bandFluxes.put(b, 0.0);
                }
            }

            // Initialize interpolation weights:
            final double[] weightsL = new double[nWLen];
            final double[] weightsR = new double[nWLen];

            // define mapping between spectral channels and model images:
            final List<UserModelData> modelDataList = target.getUserModel().getModelDataList();

            // determine which images to use according to wavelength range:
            final List<UserModelComputePart> modelParts = mapUserModel(modelDataList, sampleWaveLengths,
                    weightsL, weightsR, mFluxes, bandFluxes, options.userModelCubeInterpolation);

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
            final int nTh = (!JOB_EXECUTOR.isWorkerThread() && (nDataPoints > JOB_THRESHOLD_USER_MODELS)) ? JOB_EXECUTOR.getMaxParallelJob() : 1;

            // Prepare thread context variables:
            final int[][] nTaskThreads = (SHOW_COMPUTE_STATS) ? new int[nTh][16] : null; // cache line padding

            // adjust largest chunk size:
            final int maxPixelsPerChunk = 85000; // to ensure chunk less than 1 Mb (and fit in CPU caches)

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

                // use left / right flag to select appropriate weights:
                final double[] weights = (modelPart.left) ? weightsL : weightsR;

                // This will change for each image in the Fits cube:
                final int n1D = modelData.getNData();
                // flattened data points (1D) [data xfreq yfreq]:
                final float[] data1D = modelData.getData1D();

                if (logger.isDebugEnabled()) {
                    logger.debug("computeModelVisibilities: {} bytes for image arrays", 4 * n1D); // (float) array
                }

                // Ensure 1 chunk per thread ?
                int chunk = maxPixelsPerChunk * UserModelService.DATA_1D_POINT_SIZE;

                final int nChunks = 1 + (n1D / chunk);

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

                if (logger.isDebugEnabled()) {
                    for (int c = 0; c < nChunks; c++) {
                        logger.debug("chunk[" + c + "]: range = [" + fromThreads[c] + " - " + endThreads[c] + "[");
                    }
                }

                // create tasks:
                for (int c = 0; c < nChunks; c++) {
                    // Image chunks:
                    final int fromData = fromThreads[c];
                    final int endData = endThreads[c];

                    for (int k = 0; k < nRows; k++) {
                        // rows index to be processed by this task:
                        final int rowIndex = k;
                        final double[] ufreqRow = ufreq[rowIndex];
                        final double[] vfreqRow = vfreq[rowIndex];
                        final MutableComplex[] cmVisRow = cmVis[rowIndex];

                        jobList.add(new Runnable() {
                            /**
                             * Called by the ParallelJobExecutor to perform task computation
                             */
                            @Override
                            public void run() {
                                // Compute complex visibility using the target model:
                                UserModelService.computeModel(data1D, fromData, endData, ufreqRow, vfreqRow, cmVisRow, from, end, weights, options.mathMode);

                                if (SHOW_COMPUTE_STATS) {
                                    // Get thread index to get appropriate thread vars:
                                    final int threadIndex = ParallelJobExecutor.currentThreadIndex(nTh);
                                    nTaskThreads[threadIndex][0]++;
                                }
                            }
                        });
                    }
                }
                // fast interrupt :
                if (currentThread.isInterrupted()) {
                    return false;
                }
            }

            final int nJobs = jobList.size();

            // note: have jobs a multiple of nTh to maximize parallelism !
            if (logger.isDebugEnabled()) {
                logger.debug("computeModelVisibilities: {} jobs", nJobs);
            }

            final Runnable[] jobs = jobList.toArray(new Runnable[nJobs]);

            // execute jobs in parallel or using current thread if only one job (throws InterruptedJobException if interrupted):
            JOB_EXECUTOR.forkAndJoin("OIFitsCreatorService.computeModelVisibilities", jobs);

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

        if (logger.isDebugEnabled()) {
            logger.debug("mFluxes:  {}", Arrays.toString(mFluxes));
        }

        // Sample integration on spectral channels:
        this.dataTable = new VisDataTable(nRows, nChannels);

        // integration of several samples:
        final Complex[][] visComplex = dataTable.visComplex;

        final WelfordVariance visAmpStats = new WelfordVariance();

        // integrate modelFlux [nChannels]:
        final double[] modelFluxes;

        // Super sampling ?
        if (sampleWaveLengths == this.waveLengths) {
            // Simple copy array:
            modelFluxes = mFluxes;
            // Iterate on rows :
            for (int k = 0; k < nRows; k++) {
                // Simply convert array:
                final MutableComplex[] cmVisRow = cmVis[k];
                final Complex[] visComplexRow = visComplex[k];

                for (int l = 0; l < nChannels; l++) {
                    visComplexRow[l] = new ImmutableComplex(cmVisRow[l]); // immutable for safety
                    // update stats:
                    visAmpStats.add(visComplexRow[l].abs());
                }
            }
        } else {
            modelFluxes = new double[nChannels];

            // Prepare mapping of sampled channels (inclusive indices):
            final int[] fromWL = new int[nChannels];
            final int[] endWL = new int[nChannels];

            for (int l = 0; l < nChannels; l++) {
                // Note: sometimes sub channels may overlap channel limits
                final double halfBand = 0.5d * this.waveBands[l];

                fromWL[l] = -1;
                endWL[l] = -1;

                for (int i = 0; i < nWLen; i++) {
                    // note: wavelength array can be unordered:
                    if (Math.abs(this.waveLengths[l] - sampleWaveLengths[i]) <= halfBand) {
                        // within range:
                        // identify part [fromWL - endWL]
                        if (fromWL[l] == -1) {
                            fromWL[l] = i;
                            endWL[l] = i;
                        } else {
                            endWL[l] = i;
                        }
                    } else {
                        // channel range already identified:
                        if (endWL[l] != -1) {
                            break;
                        }
                    }
                }
            }

            // normalize fluxes in channels:
            final double[] normFactorWL = new double[nChannels];

            for (int i, l = 0; l < nChannels; l++) {
                int nFlux = 0;
                double totalFlux = 0.0;

                for (i = fromWL[l]; i <= endWL[l]; i++) {
                    nFlux++;
                    totalFlux += mFluxes[i];
                }

                // update flux vs channels:
                modelFluxes[l] = totalFlux / nFlux;

                // normalize:
                normFactorWL[l] = 1.0 / totalFlux;
            }

            final MutableComplex integrator = new MutableComplex();

            // Iterate on rows :
            for (int i, k = 0, l; k < nRows; k++) {
                final MutableComplex[] cmVisRow = cmVis[k];
                final Complex[] visComplexRow = visComplex[k];

                // Iterate on spectral channels:
                for (l = 0; l < nChannels; l++) {
                    // reset
                    integrator.updateComplex(0d, 0d);

                    for (i = fromWL[l]; i <= endWL[l]; i++) {
                        final MutableComplex vis = cmVisRow[i];
                        // scale by flux in channel:
                        integrator.add(mFluxes[i] * vis.getReal(), mFluxes[i] * vis.getImaginary());
                    }
                    // normalize by total flux:
                    integrator.multiply(normFactorWL[l]);

                    visComplexRow[l] = new ImmutableComplex(integrator); // immutable for safety
                    // update stats:
                    visAmpStats.add(visComplexRow[l].abs());
                }
            }
        }
        // fast interrupt :
        if (currentThread.isInterrupted()) {
            return false;
        }

        if (visAmpStats.isSet()) {
            if (logger.isDebugEnabled()) {
                logger.debug("{}: VisAmp: {}", targetRole, visAmpStats);
            }
            if (warningContainerCompute != null) {
                warningContainerCompute.addMessage(targetRole + ": VisAmp " + visAmpStats.toString(false),
                        (targetRole == TargetRole.SCI) ? WarningMessage.Level.Information : WarningMessage.Level.Trace);
            }
        }

        // Compute complex visibility errors:
        double dit = Double.NaN;

        if ((ns != null) && AsproConstants.INS_GRAVITY_FT.equalsIgnoreCase(this.instrumentName)) {
            final double[] dits = new double[]{1, 3, 10}; // TODO: GET from configuration !
            final double[] sigmaOpdMean = new double[dits.length];

            int minIdx = -1;
            double minSigmaOpdMean = Double.MAX_VALUE; // nm

            for (int i = 0; i < dits.length; i++) {
                if (logger.isDebugEnabled()) {
                    logger.debug("Testing DIT: {} ms", dits[i]);
                }
                sigmaOpdMean[i] = computeVisibilityErrors(dits[i] * 1e-3, insBands, modelFluxes, bandFluxes, false);

                // fast interrupt :
                if (currentThread.isInterrupted()) {
                    return false;
                }
                if (sigmaOpdMean[i] < minSigmaOpdMean) {
                    minIdx = i;
                    minSigmaOpdMean = sigmaOpdMean[i];
                }
            }

            if (logger.isDebugEnabled()) {
                logger.debug("dits (ms)    :     {}", Arrays.toString(dits));
                logger.debug("sigmaOPD mean (m): {}", Arrays.toString(sigmaOpdMean));
            }

            if (minIdx == -1) {
                // no solution:
                return false;
            }
            final double bestDit = dits[minIdx];

            if (logger.isDebugEnabled()) {
                logger.debug("Using best DIT:     {} ms", bestDit);
                logger.debug("min(sigmaOPD mean): {} nm", minSigmaOpdMean);
            }
            if (warningContainerCompute != null) {
                warningContainerCompute.addInformation(targetRole + ": best DIT = " + bestDit + " ms - min(σOPD) = " + NumberUtils.format(minSigmaOpdMean) + " nm");
            }

            // Compute again to initialize the noise service with selected DIT:
            computeVisibilityErrors(bestDit * 1e-3, insBands, modelFluxes, bandFluxes, true);
        } else {
            computeVisibilityErrors(dit, insBands, modelFluxes, bandFluxes, true);
        }
        // fast interrupt :
        if (currentThread.isInterrupted()) {
            return false;
        }

        logger.info("computeModelVisibilities: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        return true;
    }

    private double computeVisibilityErrors(final double dit, final Band[] insBands,
                                           final double[] modelFluxes, final Map<Band, Double> bandFluxes,
                                           final boolean finalEstimate) {

        final long start = finalEstimate ? System.nanoTime() : 0L;

        /** Get the current thread to check if the computation is interrupted */
        final Thread currentThread = Thread.currentThread();

        // index of the baseline:
        final int[] blIdx = freqTable.blIdx;

        // index of the observation point (time):
        final int[] ptIdx = freqTable.ptIdx;

        final int nBl = freqTable.nBl;
        final int nRows = freqTable.nRows;

        // Effective number of spectral channels on the detector:
        final int nChannels = this.waveLengths.length;

        NoiseService ns = this.noiseService;

        // Compute complex visibility errors:
        final StatUtils stat;

        // ft sigmaOpd per row:
        double[] sigmaFTOpd = null;

        double sigmaFTDist = 0.0;

        if (ns == null) {
            stat = null;
        } else {
            if ((ftOiFitsCreator != null) && (ftOiFitsCreator.dataTable != null)) {
                // Get already computed sigmaOpd for FT:
                sigmaFTOpd = ftOiFitsCreator.dataTable.sigmaOpd;

                if ((sigmaFTOpd != null) && (sigmaFTOpd.length != nRows)) {
                    logger.debug("Invalid FT observation: bad dimensions (sigmaFTOpd vs rows): ({} vs {})", sigmaFTOpd.length, nRows);
                    sigmaFTOpd = null;
                }

                if (sigmaFTOpd == null) {
                    logger.debug("Invalid FT observation (sigmaFTOpd = null)");
                    // disable error estimation:
                    ns = null;
                    if (finalEstimate) {
                        this.noiseService = null;
                    }
                } else {
                    final double distFT = ns.getDistFT();

                    if (distFT > 0.0) {
                        final double h_turb = ns.getH0();
                        sigmaFTDist = getSigmaP(ns, h_turb, distFT * ALX.DEG_IN_ARCSEC);
                    }
                    if (logger.isDebugEnabled()) {
                        logger.debug("sigma_dist: {} m", sigmaFTDist);
                    }
                }
            }

            if ((ns != null) && !Double.isNaN(dit)) {
                // use given DIT:
                ns.initDetectorParameters(dit);

                // may become invalid
                if (!ns.isValid()) {
                    // disable error estimation:
                    ns = null;
                    if (finalEstimate) {
                        this.noiseService = null;
                    }
                }
            }

            if (ns == null) {
                stat = null;
            } else {
                // prepare final parameters (after initDetectorParameters):
                ns.prepareParameters(insBands, modelFluxes, bandFluxes, finalEstimate, targetRole.toString());

                stat = StatUtils.getInstance();
                // prepare enough distributions for all baselines:
                stat.prepare(nBl);
            }
        }

        final int iMid = (ns != null) ? ns.getIndexMidPoint() : -1;
        final int iRef = (ns != null) ? ns.getIndexRefChannel() : -1;

        final double snrThreshold = options.snrThreshold;

        final Complex[][] visComplex = dataTable.visComplex;
        final double[][] visAmpError = dataTable.visAmpError;
        final double[][] visPhiError = dataTable.visPhiError;

        final double[][] nbPhotObjPhoto = dataTable.nbPhotObjPhoto;
        final double[][] nbPhotPhoto = dataTable.nbPhotPhoto;
        final double[][] errPhotPhoto = dataTable.errPhotPhoto;
        final double[][] sqCorrFlux = dataTable.sqCorrFlux;
        final double[][] errSqCorrFlux = dataTable.errSqCorrFlux;

        final boolean[][] photSNRFlag = dataTable.photSNRFlag;
        final boolean[][] visAmpSNRFlag = dataTable.visAmpSNRFlag;
        final boolean[][] visPhiSNRFlag = dataTable.visPhiSNRFlag;

        final ComplexDistribution[] visRndDist = dataTable.visRndDist;

        // use the same sample index per ha point (as distributions are different instances)
        // to ensure T3 sample is correlated with C12, C23, C13 samples:
        final int[][] visRndIdx = dataTable.visRndIdx;

        // stats:
        final WelfordVariance snrVisAmpStats = new WelfordVariance();
        // only for reference channel:
        final WelfordVariance visLossStats = (sigmaFTOpd != null) ? new WelfordVariance() : null;
        final WelfordVariance visLossPhiStats = (sigmaFTOpd != null) ? new WelfordVariance() : null;
        final WelfordVariance visLossDistStats = (sigmaFTOpd != null) ? new WelfordVariance() : null;
        final WelfordVariance visLossMidStats = (sigmaFTOpd != null) ? new WelfordVariance() : null;

        int pt, prevPt = -1;

        // Iterate on rows :
        for (int k = 0, l; k < nRows; k++) {
            final double[] visAmpErrorRow = visAmpError[k];
            final double[] visPhiErrorRow = visPhiError[k];

            final double[] nbPhotObjPhotoRow = nbPhotObjPhoto[k];
            final double[] nbPhotPhotoRow = nbPhotPhoto[k];
            final double[] errPhotPhotoRow = errPhotPhoto[k];
            final double[] sqCorrFluxRow = sqCorrFlux[k];
            final double[] errSqCorrFluxRow = errSqCorrFlux[k];

            final boolean[] photSNRFlagRow = photSNRFlag[k];
            final boolean[] visAmpSNRFlagRow = visAmpSNRFlag[k];
            final boolean[] visPhiSNRFlagRow = visPhiSNRFlag[k];

            if ((ns == null) || (stat == null)) {
                Arrays.fill(visAmpErrorRow, Double.NaN);
                Arrays.fill(visPhiErrorRow, Double.NaN);

                Arrays.fill(nbPhotObjPhotoRow, Double.NaN);
                Arrays.fill(nbPhotPhotoRow, Double.NaN);
                Arrays.fill(errPhotPhotoRow, Double.NaN);
                Arrays.fill(sqCorrFluxRow, Double.NaN);
                Arrays.fill(errSqCorrFluxRow, Double.NaN);

                Arrays.fill(photSNRFlagRow, true);
                Arrays.fill(visAmpSNRFlagRow, true);
                Arrays.fill(visPhiSNRFlagRow, true);
            } else {
                final Complex[] visComplexRow = visComplex[k];
                // select a different complex distribution per row:
                visRndDist[k] = stat.get();
                pt = ptIdx[k];

                // Iterate on spectral channels:
                for (l = 0; l < nChannels; l++) {
                    final double visAmp = visComplexRow[l].abs();
                    double visScale = 1.0;

                    // Handle FT residuals (sigmaOpd):
                    if (sigmaFTOpd != null) {
                        // per row (ie per obs point and baseline):
                        final double sigma_phi = (sigmaFTOpd[k] * TWO_PI) / waveLengths[l];
                        double visLoss_phi = Math.exp(-sigma_phi * sigma_phi / 2.0);

                        if (!Double.isFinite(visLoss_phi) || (visLoss_phi < 1e-6)) {
                            visLoss_phi = 0.0;
                        }

                        final double sigma_p = (sigmaFTDist != 0.0) ? (sigmaFTDist * TWO_PI) / waveLengths[l] : 0.0;
                        double visLoss_p = (sigma_p != 0.0) ? Math.exp(-sigma_p * sigma_p / 2.0) : 1.0;

                        if (!Double.isFinite(visLoss_p) || (visLoss_p < 1e-6)) {
                            visLoss_p = 0.0;
                        }

                        // combined visibility loss:
                        visScale = visLoss_phi * visLoss_p;

                        if (!Double.isFinite(visScale) || (visScale < 1e-6)) {
                            visScale = 0.0;
                        }
                        // keep stats at ref wavelength:
                        if (l == iRef) {
                            visLossPhiStats.add(visLoss_phi);
                            visLossDistStats.add(visLoss_p);
                            visLossStats.add(visScale);

                            if (pt == iMid) {
                                visLossMidStats.add(visScale);
                            }
                        }
                        if (logger.isDebugEnabled()) {
                            logger.debug("waveLength:  {} m", waveLengths[l]);
                            logger.debug("sigma_opd:   {} m", sigmaFTOpd[k]);
                            logger.debug("sigma_phi:   {}", sigma_phi);
                            logger.debug("sigma_p:     {}", sigma_p);
                            logger.debug("visLoss_phi: {}", visLoss_phi);
                            logger.debug("visLoss_p:   {}", visLoss_p);
                            logger.debug("visScale:    {}", visScale);
                        }
                    }

                    // complex visibility error for phases (no photometry):
                    // note: call this method first as it modifies internally other vectors:
                    visPhiErrorRow[l] = ns.computeVisComplexErrorValue(pt, l, visAmp, visScale, false);

                    // complex visibility error for amplitudes (with photometry):
                    visAmpErrorRow[l] = ns.computeVisComplexErrorValue(pt, l, visAmp, visScale, true);

                    // object flux:
                    nbPhotObjPhotoRow[l] = ns.getNbPhotObjPhoto(pt, l);

                    // extra Vis2 columns (photo + square correlated fluxes):
                    nbPhotPhotoRow[l] = ns.getNbPhotPhoto(pt, l);
                    errPhotPhotoRow[l] = ns.getErrorPhotPhoto(pt, l);
                    // note: following columns corresponds to last computeVisComplexErrorValue(true) call
                    // for amplitudes (with photometry):
                    sqCorrFluxRow[l] = ns.getSqCorrFlux(pt, l);
                    errSqCorrFluxRow[l] = ns.getErrorSqCorrFlux(pt, l);

                    if (logger.isDebugEnabled()) {
                        logger.debug("waveLength:    {}", this.waveLengths[l]);
                        logger.debug("VisAmp       : ({}, {})", visAmp, visAmpErrorRow[l]);
                        logger.debug("VisPhi       : ({}, {})", visAmp, visPhiErrorRow[l]);
                        logger.debug("nbPhotObjPhoto : {}", nbPhotObjPhotoRow[l]);
                        logger.debug("nbPhotPhoto  : {}", nbPhotPhotoRow[l]);
                        logger.debug("errPhotPhoto : {}", errPhotPhotoRow[l]);
                        logger.debug("sqCorrFlux   : {}", sqCorrFluxRow[l]);
                        logger.debug("errSqCorrFlux: {}", errSqCorrFluxRow[l]);
                    }

                    // check SNR(V) for flux:
                    // 0.25 to mimic snrThreshold = 2 threshold (v2):
                    final double snrPhoto = 0.25 * nbPhotPhotoRow[l] / errPhotPhotoRow[l];

                    if (snrPhoto < snrThreshold) {
                        photSNRFlagRow[l] = true;
                        if (logger.isDebugEnabled()) {
                            logger.debug("Low SNR[{}] (phot): {} < {}", this.waveLengths[l], snrPhoto, snrThreshold);
                        }
                    }

                    // check SNR(V) for amplitudes:
                    final double snrVisAmp = visAmp / visAmpErrorRow[l];
                    snrVisAmpStats.add(snrVisAmp);

                    if (snrVisAmp < snrThreshold) {
                        visAmpSNRFlagRow[l] = true;
                        if (logger.isDebugEnabled()) {
                            logger.debug("Low SNR[{}] (amp): {} < {}", this.waveLengths[l], snrVisAmp, snrThreshold);
                        }
                    }

                    // check SNR(V) for phases:
                    final double snrVisPhi = visAmp / visPhiErrorRow[l];

                    if (snrVisPhi < snrThreshold) {
                        visPhiSNRFlagRow[l] = true;
                        if (logger.isDebugEnabled()) {
                            logger.debug("Low SNR[{}] (phi): {} < {}", this.waveLengths[l], snrVisPhi, snrThreshold);
                        }
                    }

                    // TODO: make SNR(V2) stats per channel to add warning if no channel is below 3 !
                }

                if (NoiseService.DEBUG) {
                    final double visAmp = visComplexRow[iRef].abs();

                    logger.info("cVisAmpErrorRow(mid) = {} % (SNR = {}) (SNR V2 = {})",
                            100.0 * (visAmpErrorRow[iRef] / visAmp), (visAmp / visAmpErrorRow[iRef]), (visAmp / visAmpErrorRow[iRef]) / 2.0);
                    logger.info("cVisPhiErrorRow(mid) = {} % (SNR = {})",
                            100.0 * (visPhiErrorRow[iRef] / visAmp), (visAmp / visPhiErrorRow[iRef]));
                }

                if (pt != prevPt) {
                    prevPt = pt;

                    if (this.doNoise) {
                        final int[] cVisRndIdxRow = visRndIdx[k] = new int[nChannels];
                        for (l = 0; l < nChannels; l++) {
                            // Choose the jth sample (uniform probability):
                            cVisRndIdxRow[l] = getNextRandomSampleIndex();
                        }
                    }
                } else {
                    // use same random indexes:
                    visRndIdx[k] = visRndIdx[k - 1];
                }
            }
            // fast interrupt:
            if (currentThread.isInterrupted()) {
                return Double.NaN;
            }
        } // rows

        // fast interrupt :
        if (currentThread.isInterrupted()) {
            return Double.NaN;
        }

        if (snrVisAmpStats.isSet()) {
            if (logger.isDebugEnabled()) {
                logger.debug("{}: SNR(V): {}", targetRole, snrVisAmpStats);
            }
            if (finalEstimate && (warningContainerCompute != null)) {
                warningContainerCompute.addMessage(targetRole + ": SNR(V) " + snrVisAmpStats.toString(false),
                        (targetRole == TargetRole.SCI) ? WarningMessage.Level.Information : WarningMessage.Level.Trace);
            }
        }
        if (ns != null) {
            if ((visLossPhiStats != null) && visLossPhiStats.isSet()) {
                if (logger.isDebugEnabled()) {
                    logger.debug("{}: VisLoss(Phi): {}", targetRole, visLossPhiStats);
                }
                if (finalEstimate && (warningContainerCompute != null)) {
                    warningContainerCompute.addTrace(targetRole + ": VisLoss(Phi) " + visLossPhiStats.toString(false));
                }
            }
            if ((visLossDistStats != null) && visLossDistStats.isSet()) {
                if (logger.isDebugEnabled()) {
                    logger.debug("{}: VisLoss(DistFT): {}", targetRole, visLossDistStats);
                }
                if (finalEstimate && (warningContainerCompute != null)) {
                    warningContainerCompute.addTrace(targetRole + ": VisLoss(DistFT) " + visLossDistStats.toString(false));
                }
            }
            if ((visLossStats != null) && visLossStats.isSet()) {
                if (logger.isDebugEnabled()) {
                    logger.debug("{}: VisLoss: {}", targetRole, visLossStats);
                }
                if (finalEstimate && (warningContainerCompute != null)) {
                    warningContainerCompute.addInformation(targetRole + ": VisLoss " + visLossStats.toString(false));
                }
            }
            if (visLossMidStats != null) {
                if (visLossMidStats.isSet() && logger.isDebugEnabled()) {
                    logger.debug("VisLoss (mid/ref): {}", visLossMidStats);
                }
                // set VisScale (mean) to generate noisy images (later):
                ns.setVisScaleMeanForMidRefPoint(visLossMidStats.mean());
            }
        }

        double sigmaOpdMean = Double.NaN;

        // Post-process SNR_FT to compute sigmaOpd:
        if ((ns != null) && AsproConstants.INS_GRAVITY_FT.equalsIgnoreCase(this.instrumentName)) {

            // TODO: put in configuration VLTI telescope (UT/AT ?)
            /*
            Lacour 2019: https://www.aanda.org/articles/aa/abs/2019/04/aa34981-18/aa34981-18.html
            The median fringe-tracking residuals are 150 nm on the ATs and 250 nm on the UTs.

            // 200 nm residual vibration in GRAVITY+ simulator for UTs:
             */
            final double sigma_vib = ((ns.getTelDiam() > 7.0) ? 200.0 : 100.0) * 1e-9; // nm // TODO: put config

            final double t0 = ns.getT0(); // in ms
            final double ftDit = ns.getObsUsedDit() * 1000.0; // in ms

            // use SQRT(NDIT) to determine SNR for 1 DIT
            final double invSqrtNDIT = ns.getTotFrameCorrection();

            final double lambdaFT = this.waveLengths[iRef]; // center of K band (=2.2 microns for gravity)
            final double nbWaveFT = lambdaFT / TWO_PI;

            // Compute sigmaOpd (fixed term):
            final double varOpdLow = Math.pow(Math.pow(ftDit / (2.6 * t0), (5.0 / 6.0)) * nbWaveFT, 2.0) + Math.pow(sigma_vib, 2.0);

            if (logger.isDebugEnabled()) {
                logger.debug("computeErrors() for {}", this.instrumentName);
                logger.debug("sigma_vib:          {} nm", sigma_vib);
                logger.debug("t0:                 {} ms", t0);
                logger.debug("ftDit:              {} ms", ftDit);
                logger.debug("lambdaFT:           {} m", lambdaFT);
                logger.debug("SQRT(NDIT):         {}", 1.0 / invSqrtNDIT);
                logger.debug("sigma_opd_lo(FT):   {} nm", 1e9 * Math.sqrt(varOpdLow));
            }

            // compute sigmaOpd(FT) for later use by the SCIENCE observation:
            final double[] sigmaOpd = dataTable.getSigmaOpd();

            final double[] snrFTs = new double[nRows];

            final WelfordVariance snrFTStats = new WelfordVariance();
            final WelfordVariance sigmaOpdStats = new WelfordVariance();

            // Iterate on rows :
            // (baselines) x nObsPoints
            for (int j, k = 0, l; k < nRows; k++) {
                pt = ptIdx[k]; // obs point
                j = blIdx[k];

                if (logger.isDebugEnabled()) {
                    logger.debug("Row: {} - Obs point: {} - Baseline({}): {}", k, pt, j, baseLines.get(j));
                }

                final Complex[] visComplexRow = visComplex[k];
                final double[] visAmpErrorRow = visAmpError[k];

                double sum_weight_mean = 0.0;
                double sum_weight = 0.0;

                // Iterate on spectral channels (FT):
                for (l = 0; l < nChannels; l++) {
                    final double visAmp = visComplexRow[l].abs();

                    // retrieve SNR(V) for 1 DIT only:
                    final double snrVisAmp = (visAmp / visAmpErrorRow[l]) * invSqrtNDIT;

                    final double nbPhotInterf = ns.getNbPhotInterf(pt, l);

                    // SNR(V) = 2 SNR(V2):
                    final double sigma_phi = 1.0 / snrVisAmp;
                    final double weight = Math.pow(nbPhotInterf, 2.0);

                    if (logger.isDebugEnabled()) {
                        logger.debug("waveLength: {}", this.waveLengths[l]);
                        logger.debug("VisAmp:     {} VisAmpErr={}", visAmp, visAmpErrorRow[l]);
                        logger.debug("SNR(V):     {}", snrVisAmp);
                        logger.debug("sigma_phi:  {} rad", sigma_phi);
                        logger.debug("weight:     {}", weight);
                    }

                    sum_weight_mean += Math.pow(sigma_phi * weight, 2.0);
                    sum_weight += Math.pow(weight, 2.0);
                }

                final double snrFT = Math.sqrt(sum_weight / sum_weight_mean);

                if (logger.isDebugEnabled()) {
                    logger.debug("snrFT[pt: {} - bl: {}] ({}): {}", pt, j, baseLines.get(j).getName(), snrFT);
                }

                snrFTs[k] = snrFT;
                snrFTStats.add(snrFT);

                // fast interrupt :
                if (currentThread.isInterrupted()) {
                    return Double.NaN;
                }
            } // rows

            // Bootstrapping over triangles:
            final int nIter = 4; // TODO: fix ntel ?

            final int nObsPoints = nRows / nBl;
            final int nTriplets = triplets.size();

            if (logger.isDebugEnabled()) {
                logger.debug("nIter:       {}", nIter);
                logger.debug("nTriplets:   {}", nTriplets);
                logger.debug("nObsPoints:  {}", nObsPoints);
                logger.debug("nBaselines:  {}", nBl);
                logger.debug("triplets:    {}", triplets);
            }

            final int[] idx = new int[3];

            // Iterate on observable UV points :
            for (int i = 0, j, n, m; i < nObsPoints; i++) {
                // position in row group :
                final int vp = nBl * i;

                if (logger.isDebugEnabled()) {
                    logger.debug("Point: {}", i);
                }

                // Loop several time over triplet to also
                // get the baseline tracked by quadruplets.
                for (n = 0; n < nIter; n++) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("Iteration: {}", n);
                    }
                    m = 0;

                    // Iterate on baselines :
                    for (j = 0; j < nTriplets; j++) {
                        final Triplet triplet = triplets.get(j);

                        // Use relative positions to get the 3 complex vectors (AB, BC, AC)
                        final int[] relPos = triplet.getRelativePosition();

                        // Find baseline AB = 12 :
                        idx[0] = vp + relPos[0];

                        // Find baseline BC = 23 :
                        idx[1] = vp + relPos[1];

                        // Find baseline AC = 13 :
                        idx[2] = vp + relPos[2];

                        if (logger.isDebugEnabled()) {
                            logger.debug("Triplet:        {}", triplet);
                            logger.debug("SNR(T):         {}", toString(idx, snrFTs));
                        }

                        // sort SNR in ascending order using indices:
                        compareAndSwap(idx, snrFTs, 0, 1);
                        compareAndSwap(idx, snrFTs, 0, 2);
                        compareAndSwap(idx, snrFTs, 1, 2);

                        if (logger.isDebugEnabled()) {
                            logger.debug("Sorted SNR(T):  {}", toString(idx, snrFTs));
                        }

                        // Set SNR as the worst of the two best
                        // ie set lower SNR value to the second one:
                        if (snrFTs[idx[0]] != snrFTs[idx[1]]) {
                            m++;
                            snrFTs[idx[0]] = snrFTs[idx[1]];

                            if (logger.isDebugEnabled()) {
                                logger.debug("Fixed SNR(T):   {}", toString(idx, snrFTs));
                            }
                        }
                    } // triplets

                    if (m == 0) {
                        break;
                    }
                    // fast interrupt :
                    if (currentThread.isInterrupted()) {
                        return Double.NaN;
                    }
                } // iters
            } // obs points

            final WelfordVariance snrFTBSStats = new WelfordVariance();

            // Iterate on rows :
            // (baselines) x nObsPoints
            for (int i, j, k = 0; k < nRows; k++) {
                i = ptIdx[k]; // obs point
                j = k % nBl;

                final double snrFT = snrFTs[k];
                snrFTBSStats.add(snrFT);

                // Compute sigmaOpd (variable term):
                final double varOpdHigh = Math.pow((4.0 * nbWaveFT) / snrFT, 2.0);
                sigmaOpd[k] = Math.sqrt(varOpdHigh + varOpdLow);
                sigmaOpdStats.add(1e9 * sigmaOpd[k]); // nm

                if (logger.isDebugEnabled()) {
                    logger.debug("snrFT[{} - {}]:   {}", i, j, snrFT);
                    logger.debug("sigma_opd_hi(FT): {} nm", 1e9 * Math.sqrt(varOpdHigh));
                    logger.debug("sigma_opd   (FT): {} nm", 1e9 * sigmaOpd[k]);
                }

                // fast interrupt :
                if (currentThread.isInterrupted()) {
                    return Double.NaN;
                }
            }
            // fast interrupt :
            if (currentThread.isInterrupted()) {
                return Double.NaN;
            }

            // Make stats per point (all baselines):
            if (snrFTStats.isSet()) {
                if (logger.isDebugEnabled()) {
                    logger.debug("ftDit: {} ms", ftDit);
                    logger.debug("SNR(FT):    {}", snrFTStats);
                    logger.debug("BS SNR(FT): {}", snrFTBSStats);
                }
                if (finalEstimate && (warningContainerCompute != null)) {
                    warningContainerCompute.addInformation("FT: SNR(FT) " + snrFTBSStats.toString(false));
                }
                if (TRACE_SNR_FT && finalEstimate) {
                    System.out.printf("[TRACE]\t%.3f\t%.1f\t%.3f\n",
                            target.getFLUXK(),
                            ftDit,
                            snrFTBSStats.min()
                    );
                }
            }
            if (sigmaOpdStats.isSet()) {
                if (logger.isDebugEnabled()) {
                    logger.debug("sigmaOpd (nm): {}", sigmaOpdStats); // depends on DIT(FT)
                }
            }

            // overall mean sigmaOpd(FT):
            sigmaOpdMean = sigmaOpdStats.mean(); // nm
        } // FT

        if (finalEstimate) {
            logger.info("computeVisibilityErrors: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        }
        return sigmaOpdMean;
    }

    private static double getSigmaP(final NoiseService ns, final double h_turb, final double dist) {

        final double telDiam = ns.getTelDiam(); // m

        final double seeing = ns.getSeeing(); // as

        final double lambdaV = 0.5 * 1e-6; // seeing is given at 500 nm

        // note: different from Band.strehl r0 (1.22 not 0.98):
        final double r0 = 0.98 * lambdaV / as2rad(seeing); // m

        final double theta0 = rad2as(0.31 * (r0 / h_turb)); // as

        final double sig_p = 0.12 * Math.pow(Math.PI, (1.0 / 3.0)) * lambdaV
                * Math.pow(telDiam / r0, -1.0 / 6.0) * (dist / theta0);
        /*
        lambda_500 = 500 * 1e-9

        theta0 = 0.31 * (r0 / self.h_turb) / (0.48 * 1e-5)
        sigma_p = 0.12 * np.pi ** (1 / 3) * lambda_500 * (diam / r0) ** (-1 / 6) * (self.ftsc_sep / theta0)
        vis_loss = np.exp(-2 * (np.pi / self.gravi_wave) ** 2 * sigma_p ** 2)
         */

        if (logger.isDebugEnabled()) {
            logger.debug("telDiam: {} m", telDiam);
            logger.debug("seeing:  {} as", seeing);
            logger.debug("r0:      {} m", r0);
            logger.debug("theta0:  {} as", theta0);
            logger.debug("sig_p:   {} as", sig_p);
        }

        return sig_p;
    }

    private static void compareAndSwap(final int[] indices, final double[] values, final int i1, final int i2) {
        if (values[indices[i1]] > values[indices[i2]]) {
            final int tmp = indices[i1];
            indices[i1] = indices[i2];
            indices[i2] = tmp;
        }
    }

    private static String toString(final int[] indices, final double[] values) {
        final StringBuilder sb = new StringBuilder(32).append("[");
        for (int i = 0; i < indices.length; i++) {
            sb.append(values[indices[i]]).append(" ");
        }
        return sb.append("]").toString();
    }

    protected abstract UVFreqTable computeSpatialFreqTable(final double[] sampleWaveLengths);

    protected final int getNextRandomSampleIndex() {
        return this.random.nextInt(N_SAMPLES);
    }

    /**
     * @param other another OIFits producer
     * @return true if the OIFITS producer options are the same; false otherwise
     */
    public boolean isSameOptions(final AbstractOIFitsProducer other) {
        if (!options.equals(other.getOptions())) {
            return false;
        }
        return (doNoise == other.doNoise);
    }

    /**
     * Return OIFits options
     * @return OIFits options
     */
    public final OIFitsProducerOptions getOptions() {
        return options;
    }

    /**
     * Return the noise service
     * @return noise service
     */
    public final NoiseService getNoiseService() {
        return noiseService;
    }

    /* --- utility methods --- */
    protected static double distanceAngle(final double a1, final double a2) {
        final double delta = a1 - a2;
        if (delta > Math.PI) {
            return delta - (2.0 * Math.PI);
        } else if (delta < -Math.PI) {
            return delta + (2.0 * Math.PI);
        }
        return delta;
    }

    /**
     * Find the user model data corresponding only to the given wavelength (within +/- 1/2 increment)
     * @param wavelength spectral channel wavelength
     * @param modelDataList user model data
     * @return user model data corresponding to the given wavelength
     */
    protected static UserModelData findUserModelData(final double wavelength, final List<UserModelData> modelDataList) {
        final int nImages = modelDataList.size();

        // suppose that model image wavelength ranges do not overlap (true for fitscube):
        for (int i = 0; i < nImages; i++) {
            final UserModelData modelData = modelDataList.get(i);

            if (modelData.getWaveLengthRange().contains(wavelength)) {
                return modelData;
            }
        }
        // No user model found:
        return null;
    }

    /**
     * Map user model images on the instrumental spectral channels
     * @param modelDataList user model images
     * @param sampleWaveLengths sampled instrument spectral channels
     * @param weightsL computed interpolation weights on the left side
     * @param weightsR computed interpolation weights on the right side
     * @param mFluxes computed flux array from user model (cube)
     * @param bandFluxes computed flux over used bands (cube)
     * @param useInterpolation flag to enable image lerp
     * @return UserModelComputePart list
     */
    protected static List<UserModelComputePart> mapUserModel(final List<UserModelData> modelDataList,
                                                             final double[] sampleWaveLengths,
                                                             final double[] weightsL,
                                                             final double[] weightsR,
                                                             final double[] mFluxes,
                                                             final Map<Band, Double> bandFluxes,
                                                             final boolean useInterpolation) {

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
            part.left = true; // single-sided

            modelParts.add(part);

            Arrays.fill(weightsL, 1.0);
            Arrays.fill(weightsR, Double.NaN);

            // update flux vs wavelengths:
            Arrays.fill(mFluxes, 1.0); // already normalized

            // Ignore flux correction:
            bandFluxes.clear();
        } else {
            // Process wavelength ranges:
            // Ensure model list is sorted by ascending wavelength:
            // note: following indices refers to this sorted list, not modelDataList:
            final ArrayList<UserModelData> sortedModelData = new ArrayList<>(modelDataList);
            Collections.sort(sortedModelData, UserModelDataComparator.getInstance());

            final UserModelInterpolator modelLerp = new UserModelInterpolator(sortedModelData);

            // 0 - compute total flux per used band:
            if (logger.isDebugEnabled()) {
                logger.debug("used insBands: {}", bandFluxes.keySet());
            }

            for (Map.Entry<Band, Double> e : bandFluxes.entrySet()) {
                final Band b = e.getKey();

                final Range wavelengthRange = new Range(
                        b.getLambdaLower() * AsproConstants.MICRO_METER,
                        b.getLambdaUpper() * AsproConstants.MICRO_METER);

                e.setValue(computeMeanFlux(wavelengthRange, modelLerp));
            }
            if (logger.isDebugEnabled()) {
                logger.debug("bandFluxes: {}", bandFluxes);
            }

            // 1 - identify model left / right => weights on left / right sides:
            final int[] idxL = new int[nWLen];
            final int[] idxR = new int[nWLen];

            // Initialize:
            Arrays.fill(idxL, -1);
            Arrays.fill(idxR, -1);

            Arrays.fill(weightsL, Double.NaN);
            Arrays.fill(weightsR, Double.NaN);

            for (int i = 0; i < nWLen; i++) {
                final double wavelength = sampleWaveLengths[i];

                if (!useInterpolation) {
                    final int idx = findUserModel(wavelength, sortedModelData);

                    final UserModelData modelData = sortedModelData.get(idx);

                    // only use left side:
                    idxL[i] = idx;
                    weightsL[i] = 1.0;

                    // update flux vs wavelengths:
                    mFluxes[i] = modelData.getTotalFlux();
                } else {
                    // initialize interpolator:
                    modelLerp.interpolate(wavelength);

                    idxL[i] = modelLerp.left;
                    idxR[i] = modelLerp.right;

                    // update flux vs wavelengths:
                    mFluxes[i] = modelLerp.totalFlux;

                    // weights on visibility ie normalization by F(k): 
                    // w(A) = alpha * F(A) / F(k) and w(B) = (1 - alpha) * F(B) / F(k)
                    // implicit total flux > 0:
                    weightsL[i] = modelLerp.alpha * modelLerp.totalFluxLeft / mFluxes[i];
                    weightsR[i] = modelLerp.oneMinusAlpha * modelLerp.totalFluxRight / mFluxes[i];

                    if (logger.isDebugEnabled()) {
                        logger.debug("TotalFlux:  left = {} right = {} interp = {}",
                                modelLerp.totalFluxLeft, modelLerp.totalFluxRight, mFluxes[i]);
                        logger.debug("alpha     = {}", modelLerp.alpha);
                        logger.debug("weights: left = {} right = {}", weightsL[i], weightsR[i]);
                    }
                }
            }

            if (logger.isDebugEnabled()) {
                logger.debug("idxL:     {}", Arrays.toString(idxL));
                logger.debug("weightsL: {}", Arrays.toString(weightsL));
                logger.debug("idxR:     {}", Arrays.toString(idxR));
                logger.debug("weightsR: {}", Arrays.toString(weightsR));
                logger.debug("mFluxes:  {}", Arrays.toString(mFluxes));
            }

            // pass 2 - create parts grouping left/right indices (blocks):
            UserModelComputePart curLeft = null;
            UserModelComputePart curRight = null;

            for (int i = 0; i < nWLen; i++) {
                final int left = idxL[i];

                if (left != -1) {
                    final UserModelData modelData = sortedModelData.get(left);

                    if ((curLeft == null) || (curLeft.modelData != modelData)) {
                        // different model image:
                        curLeft = new UserModelComputePart();
                        curLeft.modelData = modelData;
                        curLeft.fromWL = i;
                        curLeft.endWL = i + 1;
                        curLeft.left = true; // left side

                        modelParts.add(curLeft);
                    } else {
                        // increase endWL:
                        curLeft.endWL = i + 1;
                    }
                }

                final int right = idxR[i];

                if (right != -1) {
                    final UserModelData modelData = sortedModelData.get(right);

                    if ((curRight == null) || (curRight.modelData != modelData)) {
                        // different model image:
                        curRight = new UserModelComputePart();
                        curRight.modelData = modelData;
                        curRight.fromWL = i;
                        curRight.endWL = i + 1;
                        curRight.left = false; // right side

                        modelParts.add(curRight);
                    } else {
                        // increase endWL:
                        curRight.endWL = i + 1;
                    }
                }
            }
        }
        return modelParts;
    }

    private static double computeMeanFlux(final Range wavelengthRange,
                                          final UserModelInterpolator modelLerp) {

        final double wlA = wavelengthRange.getMin();
        final double wlB = wavelengthRange.getMax();

        if (logger.isDebugEnabled()) {
            logger.debug("computeMeanFlux[{} - {}]", wlA, wlB);
        }

        final Map<Double, Double> samples = new TreeMap<>();

        // left side: A
        modelLerp.interpolate(wlA);

        // Insert left interpolated point:
        samples.put(wlA, modelLerp.totalFlux);

        final int idxA = modelLerp.right;

        if ((idxA != -1) && (wavelengthRange.contains(modelLerp.wavelengthRight))) {
            samples.put(modelLerp.wavelengthRight, modelLerp.totalFluxRight);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("Model[{} = {}] for WL[{}]: {}", idxA, modelLerp.wavelengthRight, wlA, modelLerp.totalFluxRight);
        }

        // right side: B
        modelLerp.interpolate(wlB);

        // Insert right interpolated point:
        samples.put(wlB, modelLerp.totalFlux);

        final int idxB = modelLerp.left;

        if ((idxB != -1) && (wavelengthRange.contains(modelLerp.wavelengthLeft))) {
            samples.put(modelLerp.wavelengthLeft, modelLerp.totalFluxLeft);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("Model[{} = {}] for WL[{}]: {}", idxB, modelLerp.wavelengthLeft, wlB, modelLerp.totalFluxLeft);
        }

        // Middle part:
        for (int idx = idxA + 1; idx < idxB; idx++) {
            final UserModelData modelData = modelLerp.sortedModelData.get(idx);
            final double wavelength = modelData.getWaveLength();

            if (wavelengthRange.contains(wavelength)) {
                samples.put(wavelength, modelData.getTotalFlux());
            }
        }

        if (logger.isDebugEnabled()) {
            logger.debug("samples: {}", samples);
        }

        // Trapezoid integration:
        final int len = samples.size();

        final double[] x = new double[len];
        final double[] f = new double[len];

        int n = 0;
        for (Map.Entry<Double, Double> e : samples.entrySet()) {
            x[n] = e.getKey();
            f[n] = e.getValue();
            n++;
        }

        // Trapezoid integration:
        double totalFlux = 0.0;

        for (int i = 0, end = len - 1; i < end; i++) {
            totalFlux += (x[i + 1] - x[i]) * (f[i + 1] + f[i]);
        }
        totalFlux /= 2.0; // trapezoid

        final double meanFlux = totalFlux / (wlB - wlA);

        if (logger.isDebugEnabled()) {
            logger.debug("totalFlux: {} - meanFlux: {}", totalFlux, meanFlux);
        }
        return meanFlux;
    }

    /**
     * Find the user model data corresponding to the closest spectral channel
     * @param wavelength spectral channel wavelength
     * @param modelDataList user model data
     * @return user model data corresponding to the closest spectral channel
     */
    protected static int findUserModel(final double wavelength, final List<UserModelData> modelDataList) {

        UserModelData modelData = modelDataList.get(0);

        // test first user model image:
        if (wavelength <= modelData.getWaveLengthRange().getMax()) {
            return 0;
        }

        final int nImages = modelDataList.size();
        final int last = nImages - 1;

        modelData = modelDataList.get(last);

        // test last user model image:
        if (wavelength >= modelData.getWaveLengthRange().getMin()) {
            return last;
        }

        // suppose that model image wavelength ranges do not overlap (true for fitscube):
        for (int i = 0; i < nImages; i++) {
            modelData = modelDataList.get(i);

            if (modelData.getWaveLengthRange().contains(wavelength)) {
                return i;
            }
        }
        throw new IllegalStateException("findUserModel: unable for find an user model at wavelength = "
                + convertWL(wavelength) + " " + SpecialChars.UNIT_MICRO_METER);
    }

    /**
     * Copy mutable complex array
     * @param array mutable complex array
     * @param dest mutable complex array
     */
    protected static void copyArray(final MutableComplex[] array, final MutableComplex[] dest) {
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
    protected static MutableComplex[] createArray(final int length) {
        final MutableComplex[] result = new MutableComplex[length];

        for (int i = length - 1; i >= 0; i--) {
            result[i] = new MutableComplex();
        }
        return result;
    }

    protected static double[] convertArray(final float[] values) {
        final double[] array = new double[values.length];

        for (int i = 0; i < values.length; i++) {
            array[i] = values[i];
        }
        return array;
    }

    /**
     * Compute the regularly sampled wavelengths (centered on each spectral channel) given its bounds and spectral channel width
     * @param min lower bound
     * @param width spectral channel width
     * @param nWLen number of wavelengths to compute
     * @return regularly sampled wavelengths
     */
    protected static double[] computeWaveLengths(final double min, final double width, final int nWLen) {
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
    protected static double[] resampleWaveLengths(final double[] waveLengths, final double[] waveBands, final int nSamples) {
        // compute interpolation coefficients between ]-0.5, 0.5[
        final double[] interp = new double[nSamples];

        for (int j = 0; j < nSamples; j++) {
            interp[j] = -0.5 + ((1.0 + j)) / (1.0 + nSamples);
        }

        final int nWLen = waveLengths.length;
        final double[] wLen = new double[nWLen * nSamples];

        double lambda, delta_lambda, dl = waveBands[0];
        double sign = 1.0;

        for (int i = 0, k = 0; i < nWLen; i++) {
            lambda = waveLengths[i];
            if (i + 1 < nWLen) {
                // note: wavelength array can be unordered:
                dl = waveLengths[i + 1] - lambda;
                if (dl >= 0.0) {
                    sign = 1.0;
                } else {
                    sign = -1.0;
                    dl = -dl;
                }
            }
            // preserve sign (increasing or decreasing) and reduce bandwidth if larger than (lambda1 - lambda2)
            delta_lambda = sign * Math.min(dl, waveBands[i]);

            for (int j = 0; j < nSamples; j++) {
                wLen[k++] = lambda + interp[j] * delta_lambda;
            }
        }
        if (logger.isDebugEnabled()) {
            logger.debug("waveLengths: {}", Arrays.toString(waveLengths));
            logger.debug("resampleWaveLengths: {}", Arrays.toString(wLen));
        }
        return wLen;
    }

    /**
     * Return the station mapping
     * @param stations station list
     * @return station mapping
     */
    protected static Map<Station, Short> createStationMapping(final List<Station> stations) {
        // Create Station - index mapping :
        // Note : as Station.hashCode is not implemented, the map acts as an IdentityMap (pointer equality)
        final Map<Station, Short> stationMapping = new IdentityHashMap<Station, Short>(stations.size());

        int i = 1;
        for (Station s : stations) {
            stationMapping.put(s, Short.valueOf((short) i++));
        }

        if (logger.isDebugEnabled()) {
            logger.debug("stationMapping: {}", stationMapping);
        }
        return stationMapping;
    }

    /**
     * Return the beam mapping
     * @param stationMapping station mapping
     * @param beams beam list
     * @return beam mapping
     */
    protected static Map<Beam, Short> createBeamMapping(final Map<Station, Short> stationMapping, final List<Beam> beams) {
        // Create Beam - index mapping :
        // Note : as Beam.hashCode is not implemented, the map acts as an IdentityMap (pointer equality)
        final Map<Beam, Short> beamMapping = new IdentityHashMap<Beam, Short>(beams.size());

        for (Beam b : beams) {
            beamMapping.put(b, stationMapping.get(b.getStation()));
        }

        if (logger.isDebugEnabled()) {
            logger.debug("beamMapping: {}", beamMapping);
        }
        return beamMapping;
    }

    /**
     * Return the station indexes (2) per base line mapping (ordered)
     * @param beamMapping beam mapping
     * @param baseLines base line list
     * @return baseline station indexes
     */
    protected static Map<BaseLine, short[]> createBaseLineMapping(final Map<Beam, Short> beamMapping, final List<BaseLine> baseLines) {
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
    protected static double calendarToTime(final Calendar cal, final Calendar calObs) {
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
    protected static String calendarToString(final Calendar cal) {
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
     * Return the given wavelength rounded in microns
     * @param wl wavelength to convert
     * @return given wavelength rounded in microns
     */
    protected static double convertWL(final double wl) {
        return NumberUtils.trimTo5Digits(wl / AsproConstants.MICRO_METER);
    }

    protected static double toDegrees(final double angRad) {
        return Double.isNaN(angRad) ? Double.NaN : FastMath.toDegrees(angRad);
    }

    protected static double toAngle(final double re, final double im) {
        return (im == 0.0) ? 0.0 : FastMath.atan2(im, re);
    }

    protected static double computeCumulativeError(final double err1, final double err2) {
        // sum of variances:
        return Math.sqrt(err1 * err1 + err2 * err2);
    }

    protected final static class UVFreqTable {

        /** number of baselines */
        final int nBl;
        /** number of rows */
        final int nRows;
        /** number of sampled wavelengths */
        final int nWLen;

        /** spatial frequency U */
        final double[][] ufreq;
        /** spatial frequency V */
        final double[][] vfreq;

        /** index of the baseline */
        final int[] blIdx;
        /** index of the observation point (time) */
        final int[] ptIdx;

        protected UVFreqTable(final int nBl, final int nRows, final int nWLen) {
            this.nBl = nBl;
            this.nRows = nRows;
            this.nWLen = nWLen;
            this.ufreq = new double[nRows][nWLen];
            this.vfreq = new double[nRows][nWLen];
            this.blIdx = new int[nRows];
            this.ptIdx = new int[nRows];
        }
    }

    protected final static class VisDataTable {

        /* varying values (spectrally dependent) */
        /** internal computed complex visibility [row][waveLength] */
        final Complex[][] visComplex;
        /** internal complex visibility error [row][waveLength] for amplitudes with photometry */
        final double[][] visAmpError;
        /** internal complex visibility error [row][waveLength] for phases without photometry */
        final double[][] visPhiError;

        /** number of object photons in each photometric channel (photometric flux) [row][waveLength] */
        final double[][] nbPhotObjPhoto;
        /** number of photons in each photometric channel (photometric flux) [row][waveLength] */
        final double[][] nbPhotPhoto;
        /** error on the number of photons in each photometric channel (photometric flux) [row][waveLength] */
        final double[][] errPhotPhoto;
        /** squared correlated flux [row][waveLength] */
        final double[][] sqCorrFlux;
        /** error on squared correlated flux [row][waveLength] */
        final double[][] errSqCorrFlux;

        /** internal SNR flag on photometry [row][waveLength] */
        final boolean[][] photSNRFlag;
        /** internal complex visibility SNR flag on amplitudes [row][waveLength] */
        final boolean[][] visAmpSNRFlag;
        /** internal complex visibility SNR flag on phases [row][waveLength] */
        final boolean[][] visPhiSNRFlag;

        /** internal complex distribution [row] */
        final ComplexDistribution[] visRndDist;
        /** internal random index [row][waveLength] */
        final int[][] visRndIdx;

        /* (optional) sigmaOpd per row for FT observation */
        double[] sigmaOpd;

        protected VisDataTable(final int nRows, final int nWLen) {
            this.visComplex = new Complex[nRows][nWLen];
            this.visAmpError = init(nRows, nWLen);
            this.visPhiError = init(nRows, nWLen);

            this.nbPhotObjPhoto = init(nRows, nWLen);
            this.nbPhotPhoto = init(nRows, nWLen);
            this.errPhotPhoto = init(nRows, nWLen);
            this.sqCorrFlux = init(nRows, nWLen);
            this.errSqCorrFlux = init(nRows, nWLen);

            this.photSNRFlag = new boolean[nRows][nWLen];
            this.visAmpSNRFlag = new boolean[nRows][nWLen];
            this.visPhiSNRFlag = new boolean[nRows][nWLen];

            this.visRndDist = new ComplexDistribution[nRows];
            this.visRndIdx = new int[nRows][];

            this.sigmaOpd = null;
        }

        protected double[] getSigmaOpd() {
            if (sigmaOpd == null) {
                sigmaOpd = new double[visComplex.length];
            }
            return sigmaOpd;
        }

        static double[][] init(final int nRows, final int nWLen) {
            final double[][] array = new double[nRows][nWLen];
            for (int i = 0; i < nRows; i++) {
                Arrays.fill(array[i], Double.NaN);
            }
            return array;
        }
    }

    /**
     * Simple object representing a triplet (3 beams A, B, C) with corresponding baselines and table indexes
     */
    protected final static class Triplet {

        /** station indexes (3) */
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
     * UserModel computation part (wavelength indexes)
     */
    protected static final class UserModelComputePart {

        /* members */
        /** user model data */
        UserModelData modelData;
        /** first wavelength index (inclusive) */
        int fromWL;
        /** last wavelength index (exclusive) */
        int endWL;
        /* left or right side */
        boolean left;

        /**
         * String representation for debugging purposes
         * @return 
         */
        @Override
        public String toString() {
            return "UserModelComputePart[" + fromWL + " - " + endWL + "]"
                    + "[" + (left ? "LEFT" : "RIGHT") + "]: " + modelData;
        }
    }

    protected final static class UserModelInterpolator {

        // inputs
        final ArrayList<UserModelData> sortedModelData;
        // state:
        int left = -1;
        int right = -1;
        double wavelengthLeft;
        double wavelengthRight;
        double totalFluxLeft;
        double totalFluxRight;
        // linear interpolation weights:
        double alpha;
        double oneMinusAlpha;
        // interpolated flux:
        double totalFlux;

        protected UserModelInterpolator(final ArrayList<UserModelData> sortedModelData) {
            this.sortedModelData = sortedModelData;
            reset();
        }

        private void reset() {
            left = -1;
            right = -1;
            wavelengthLeft = Double.NaN;
            wavelengthRight = Double.NaN;
            totalFluxLeft = 0.0;
            totalFluxRight = 0.0;
            alpha = 0.0;
            oneMinusAlpha = 0.0;
            totalFlux = 0.0;
        }

        public void interpolate(final double wavelength) {
            reset();

            final int idx = findUserModel(wavelength, sortedModelData);

            final UserModelData modelData = sortedModelData.get(idx);

            final double modelWavelength = modelData.getWaveLength();

            // delta > 0 if model on left side (lower); < 0 on right side (higher)
            final double delta = wavelength - modelWavelength;

            if (logger.isDebugEnabled()) {
                logger.debug("Model[{} = {}] for WL[{}] delta: {}", idx, modelWavelength, wavelength, delta);
            }

            left = -1;
            right = -1;

            if (delta > 0) {
                left = idx;
                if (left < sortedModelData.size() - 1) {
                    right = idx + 1;
                }
            } else {
                right = idx;
                if (right > 0) {
                    left = idx - 1;
                }
            }

            if (left != -1) {
                final UserModelData modelDataLeft = sortedModelData.get(left);
                wavelengthLeft = modelDataLeft.getWaveLength();
                totalFluxLeft = modelDataLeft.getTotalFlux();

                if (logger.isDebugEnabled()) {
                    logger.debug("Left  Model[{} = {}]", left, wavelengthLeft);
                }
            } else {
                wavelengthLeft = Double.NaN;
                totalFluxLeft = 0.0;
                logger.debug("Left  Model: undefined");
            }
            if (right != -1) {
                final UserModelData modelDataRight = sortedModelData.get(right);
                wavelengthRight = modelDataRight.getWaveLength();
                totalFluxRight = modelDataRight.getTotalFlux();

                if (logger.isDebugEnabled()) {
                    logger.debug("Right Model[{} = {}]", right, wavelengthRight);
                }
            } else {
                wavelengthRight = Double.NaN;
                totalFluxRight = 0.0;
                logger.debug("Right Model: undefined");
            }

            if ((left != -1) && (right != -1)) {
                // alpha = (lambda(B) - lambda(k)) / (lambda(B) - lambda(A))
                alpha = (wavelengthRight - wavelength) / (wavelengthRight - wavelengthLeft);
                oneMinusAlpha = 1.0 - alpha;
            } else {
                // missing left or right image:
                if (left != -1) {
                    alpha = 1.0;
                    oneMinusAlpha = 0.0;
                }
                if (right != -1) {
                    alpha = 0.0;
                    oneMinusAlpha = 1.0;
                }
            }

            // consider flux ratios are linear ie modelTotalFlux given in linear units (scaling do not matter):
            // F(k) = alpha * F(A) + (1 - alpha) * F(B)
            totalFlux = alpha * totalFluxLeft + oneMinusAlpha * totalFluxRight;

            if (logger.isDebugEnabled()) {
                logger.debug("TotalFlux:  left = {} right = {} interp = {}", totalFluxLeft, totalFluxRight, totalFlux);
                logger.debug("alpha     = {}", alpha);
            }
        }
    }

    private static double rad2as(double angRad) {
        return Math.toDegrees(angRad) * ALX.DEG_IN_ARCSEC;
    }

    private static double as2rad(double angAs) {
        return Math.toRadians(angAs * ALX.ARCSEC_IN_DEGREES);
    }

}
