/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.UserModel;
import fr.jmmc.aspro.service.UserModelService.MathMode;
import fr.jmmc.aspro.util.StatUtils;
import fr.jmmc.aspro.util.StatUtils.ComplexDistribution;
import fr.jmmc.jmal.complex.ImmutableComplex;
import fr.jmmc.jmal.complex.MutableComplex;
import fr.jmmc.jmal.model.ModelComputeContext;
import fr.jmmc.jmal.model.ModelFunctionComputeContext;
import fr.jmmc.jmal.model.ModelManager;
import fr.jmmc.jmal.model.targetmodel.Model;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.SpecialChars;
import fr.jmmc.jmcs.util.concurrent.ParallelJobExecutor;
import static fr.jmmc.aspro.util.StatUtils.N_SAMPLES;
import fr.jmmc.jmal.complex.Complex;
import fr.jmmc.oitools.model.OIFitsFile;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author bourgesl
 */
public abstract class AbstractOIFitsProducer {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(AbstractOIFitsProducer.class.getName());

    /** enable DEBUG mode */
    public final static boolean DEBUG = false;
    /** use sampled mean(sample) instead of theoretical value */
    protected final static boolean DO_USE_SAMPLED_MEAN = false;
    /** flag to show compute task statistics */
    protected final static boolean SHOW_COMPUTE_STATS = false;
    /** threshold to use parallel jobs for user models (32 UV points) */
    protected final static int JOB_THRESHOLD_USER_MODELS = 32;
    /** SNR threshold to flag values with low SNR */
    protected final static double SNR_THRESHOLD = 3.0;
    /** minimal V2 error (5%) to check for SNR */
    protected final static double SNR_THRESHOLD_VIS = 0.05;
    /** Jmcs Parallel Job executor */
    protected static final ParallelJobExecutor JOB_EXECUTOR = ParallelJobExecutor.getInstance();

    /* members */
 /* input */
    /** selected target */
    protected final Target target;
    /** OIFits supersampling */
    protected final int supersampling;
    /** OIFits MathMode */
    protected final MathMode mathMode;

    /* output */
    /** oifits structure */
    protected OIFitsFile oiFitsFile = null;

    /* internal */
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
    /** internal computed complex visibility [row][waveLength] */
    protected Complex[][] visComplex = null;
    /** internal complex visibility error [row][waveLength] */
    protected double[][] visError = null;
    /** internal complex distribution [row] */
    protected ComplexDistribution[] visRndDist = null;
    /** internal random index [row][waveLength] */
    protected int[][] visRndIdx = null;
    /** internal complex visibility SNR flag [row][waveLength] */
    protected boolean[][] visSnrFlag = null;

    /**
     * Protected constructor
     * @param target target to process
     * @param supersampling OIFits supersampling preference
     * @param mathMode OIFits MathMode preference
     */
    protected AbstractOIFitsProducer(final Target target,
                                     final int supersampling,
                                     final MathMode mathMode) {

        this.target = target;

        // use target model:
        this.hasModel = target.hasModel();

        // OIFits preferences:
        this.supersampling = supersampling;
        this.mathMode = mathMode;
    }

    /**
     * Prepare the user model vs the instrumental spectral configuration
     * @param warningContainer warning container to use if needed
     * @return true if OK; false if user model is invalid (so discard computation)
     */
    protected boolean prepareUserModel(final WarningContainer warningContainer) {
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
                    final double[] insWaves = this.waveLengths;
                    final double[] insBands = this.waveBands;
                    final int nWaves = insWaves.length;

                    // Supposed wavelengths are sorted:
                    final double lambdaMin = insWaves[0];
                    final double lambdaMax = insWaves[nWaves - 1];

                    final double wlFirst = modelDataFirst.getWaveLength();

                    if (nImages == 1) {
                        // check image wavelength is in instrument range:
                        if (wlFirst < lambdaMin || wlFirst > lambdaMax) {
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
                        if (modelDataFirst.getWaveLengthRange().getMin() > lambdaMax) {
                            addWarning(warningContainer, "Incorrect model min wavelength [" + convertWL(wlFirst) + " " + SpecialChars.UNIT_MICRO_METER
                                    + "] higher than max instrument wavelength [" + convertWL(lambdaMax) + " " + SpecialChars.UNIT_MICRO_METER + ']');
                            return false;
                        }
                        if (modelDataLast.getWaveLengthRange().getMax() < lambdaMin) {
                            addWarning(warningContainer, "Incorrect model max wavelength [" + convertWL(wlLast) + " " + SpecialChars.UNIT_MICRO_METER
                                    + "] lower than min instrument wavelength [" + convertWL(lambdaMin) + " " + SpecialChars.UNIT_MICRO_METER + ']');
                            return false;
                        }

                        // navigate among spectral channels:
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
                        
                        if (uniqueModelDatas.isEmpty()) {
                            addWarning(warningContainer, "Incorrect model wavelength range [" + convertWL(wlFirst) + " - " +convertWL(wlLast) + " " + SpecialChars.UNIT_MICRO_METER
                                    + "] smaller than the typical instrumental wavelength band [" + convertWL(StatUtils.mean(insBands)) + " " + SpecialChars.UNIT_MICRO_METER + ']');
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
                            addWarning(warningContainer, "Sub sampling detected: " + nChannels + " channels but only "
                                    + uniqueModelDatas.size() + " user model image(s) available");
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
                        }
                    }
                }
            }
        }
        return true;
    }

    /**
     * Compute complex visibilities using the target model (analytical or user model)
     * and store this data in local reference table
     * @return true if complex visibilities are computed; false otherwise
     */
    protected boolean computeModelVisibilities() {
        boolean computed = false;

        if (this.hasModel) {
            /** Get the current thread to check if the computation is interrupted */
            final Thread currentThread = Thread.currentThread();

            final long start = System.nanoTime();

            // Effective number of spectral channels on the detector:
            final int nChannels = this.waveLengths.length;

            final boolean useAnalyticalModel = this.target.hasAnalyticalModel();

            // user model if defined:
            final UserModel userModel = (!useAnalyticalModel) ? target.getUserModel() : null;

            // Test if user model data is valid:
            if (userModel != null && !userModel.isModelDataReady()) {
                return false;
            }

            // Determine nSamples per spectral channel:
            // number of samples per spectral channel (1, 5, 9 ...) use the preference (SuperSampling)
            // should be an even number to keep wavelengths centered on each sub channels:
            // note: disable super sampling in high resolution:
            // use the preference (QUICK, FAST, DEFAULT?) : QUICK = PREVIEW ie No super sampling

            // TODO: determine correctly deltaLambda (object size (FOV) and Bmax/lambda) ie when super sampling is surely necessary
            int nSamples = (nChannels > 100 || this.mathMode == MathMode.QUICK) ? 1 : this.supersampling;

// TODO: fix resampling
            final double waveBand = StatUtils.mean(this.waveBands);

            double deltaLambda = waveBand / nSamples;

            // If Fits cube: use all images at least i.e. adjust frequencies and nSamples:
            final double wlInc = (userModel != null && userModel.isModelDataReady()) ? userModel.getModelData(0).getWaveLengthIncrement() : Double.POSITIVE_INFINITY;

            // Sub channel width:
            if ((wlInc > 0.0) && (wlInc < deltaLambda)) {
                // adjust nSamples to have deltaLambda < wlInc:
                nSamples = (int) Math.ceil((nSamples * waveBand) / wlInc);
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

            // fast interrupt :
            if (currentThread.isInterrupted()) {
                return false;
            }

            // Compute spatial frequencies:
            final UVFreqTable freqTable = computeSpatialFreqTable(sampleWaveLengths);
            if (freqTable == null) {
                return false;
            }

            final double[][] ufreq = freqTable.ufreq;
            final double[][] vfreq = freqTable.vfreq;

            // index of the observation point (HA):
            final int[] ptIdx = freqTable.ptIdx;

            // TODO: ignore ?
            final int nBl = freqTable.nBl;
            final int nRows = freqTable.nRows;
            final int nDataPoints = nRows * nWLen;

            logger.info("computeModelVisibilities: {} data points [{} rows - {} spectral channels - {} samples]- please wait ...",
                    nDataPoints, nRows, nChannels, nSamples);

            // Allocate data array for complex visibility and error :
            final MutableComplex[][] cmVis = new MutableComplex[nRows][];

            // Iterate on rows :
            for (int k = 0; k < nRows; k++) {
                cmVis[k] = createArray(nWLen + 4); // cache line padding (Complex = 2 double = 16 bytes) so 4 complex = complete cache line
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
                if (logger.isDebugEnabled()) {
                    logger.debug("computeModelVisibilities: MathMode = {}.", mathMode);
                }

                // TODO: compare directFT vs FFT + interpolation
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
                final int nTh = (!JOB_EXECUTOR.isWorkerThread() && (nDataPoints > JOB_THRESHOLD_USER_MODELS)) ? JOB_EXECUTOR.getMaxParallelJob() : 1;

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
                                    UserModelService.computeModel(data1D, fromData, endData, ufreqRow, vfreqRow, cmVisRow, from, end, mathMode);

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
            final NoiseService ns = this.noiseService;

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

                        // check SNR(V2) without any bias:
                        final double snrV2 = ns.getSNRVis2NoBias();

                        if (snrV2 < SNR_THRESHOLD) {
                            cVisSnrFlagRow[l] = true;
//                            System.out.println("Low SNR["+this.waveLengths[l]+"]: " + snrV2);
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

    protected abstract UVFreqTable computeSpatialFreqTable(final double[] sampleWaveLengths);

    /**
     * Return the OIFits supersampling
     * @return OIFits supersampling
     */
    public int getSupersampling() {
        return supersampling;
    }

    /**
     * Return the OIFits MathMode
     * @return OIFits MathMode
     */
    public MathMode getMathMode() {
        return mathMode;
    }

    /**
     * Return the flag to add gaussian noise to OIFits data; true if parameter doDataNoise = true and noise parameters are valid
     * @return flag to add gaussian noise to OIFits data; true if parameter doDataNoise = true and noise parameters are valid
     */
    public boolean isDoNoise() {
        return doNoise;
    }

    /**
     * Return the noise service
     * @return noise service
     */
    public NoiseService getNoiseService() {
        return noiseService;
    }

    /* --- utility methods --- */
    /**
     * Add a warning message in the OIFits file
     * @param warningContainer warning container to fill
     * @param msg message to add
     */
    protected static void addWarning(final WarningContainer warningContainer, final String msg) {
        warningContainer.addWarningMessage(msg);
    }

    /**
     * Add an information message in the OIFits file
     * @param warningContainer warning container to fill
     * @param msg message to add
     */
    protected static void addInformation(final WarningContainer warningContainer, final String msg) {
        warningContainer.addInformationMessage(msg);
    }

    protected static double distance(final double a1, final double a2) {
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

        UserModelData modelData;

        // suppose that model image wavelength ranges do not overlap (true for fitscube):
        for (int i = 0; i < nImages; i++) {
            modelData = modelDataList.get(i);

/*
TODO: refine to lookup the closest model (range overlapping) ?
            
If you were dealing with, given two ranges [x1:x2] and [y1:y2], natural order ranges at the same time where:
    natural order: x1 <= x2 && y1 <= y2

then you may want to use this to check:
they are overlapped <=> (y2 - x1) * (x2 - y1) >= 0
*/
            if (modelData.getWaveLengthRange().contains(wavelength)) {
                return modelData;
            }
        }
        // No user model found:
        return null;
    }

    /**
     * Map user model images on the instrumental spectral channeld
     * @param modelDataList user model images
     * @param sampleWaveLengths sampled instrument spectral channels
     * @return UserModelComputePart list
     */
    protected static List<UserModelComputePart> mapUserModel(final List<UserModelData> modelDataList, final double[] sampleWaveLengths) {

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
    protected static UserModelData findUserModel(final double wavelength, final List<UserModelData> modelDataList) {

        UserModelData modelData = modelDataList.get(0);

        // test first user model image:
        if (wavelength <= modelData.getWaveLengthRange().getMax()) {
            return modelData;
        }

        final int nImages = modelDataList.size();

        modelData = modelDataList.get(nImages - 1);

        // test last user model image:
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
    protected static ImmutableComplex[] convertArray(final MutableComplex[] array, final int length) {
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

        double lambda, delta_lambda;

        for (int i = 0, k = 0; i < nWLen; i++) {
            lambda = waveLengths[i];
            delta_lambda = waveBands[i];

            for (int j = 0; j < nSamples; j++) {
                wLen[k++] = lambda + interp[j] * delta_lambda;
            }
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

        logger.debug("stationMapping: {}", stationMapping);
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

        logger.debug("beamMapping: {}", beamMapping);
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
     * @param wl wavelength to toShort
     * @return given wavelength rounded in microns
     */
    protected static double convertWL(final double wl) {
        return NumberUtils.trimTo5Digits(1e6d * wl);
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

        /** index of the observation point (HA) */
        final int[] ptIdx;

        protected UVFreqTable(final int nBl, final int nRows, final int nWLen) {
            this.nBl = nBl;
            this.nRows = nRows;
            this.nWLen = nWLen;
            this.ufreq = new double[nRows][nWLen];
            this.vfreq = new double[nRows][nWLen];
            this.ptIdx = new int[nRows];
        }
    }

    /**
     * Simple object representing a triplet (3 beams A, B, C) with corresponding baselines and table indexes
     */
    protected final static class Triplet {

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

        /**
         * String representation for debugging purposes
         * @return 
         */
        @Override
        public String toString() {
            return "UserModelComputePart[" + fromWL + " - " + endWL + "]: " + modelData;
        }
    }
}
