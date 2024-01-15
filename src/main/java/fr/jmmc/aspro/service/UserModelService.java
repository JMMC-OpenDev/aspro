/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.Preferences;
import fr.jmmc.oiexplorer.core.util.FitsImageUtils;
import fr.jmmc.aspro.model.oi.UserModel;
import fr.jmmc.jmal.ALX;
import fr.jmmc.jmal.complex.MutableComplex;
import fr.jmmc.jmal.image.ColorScale;
import fr.jmmc.jmal.image.FFTUtils;
import fr.jmmc.jmal.image.FloatArrayCache;
import fr.jmmc.jmal.image.ImageArrayUtils;
import fr.jmmc.jmal.image.ImageUtils;
import fr.jmmc.jmal.image.job.ImageFlipJob;
import fr.jmmc.jmal.image.job.ImageGaussianFilterJob;
import fr.jmmc.jmal.image.job.ImageLowerThresholdJob;
import fr.jmmc.jmal.image.job.ImageNormalizeJob;
import fr.jmmc.jmal.image.job.ImageRegionThresholdJob;
import fr.jmmc.jmal.model.ImageMode;
import fr.jmmc.jmal.model.ModelUVMapService;
import fr.jmmc.jmal.model.UVMapData;
import fr.jmmc.jmal.model.VisNoiseService;
import fr.jmmc.jmal.model.function.math.Functions;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.concurrent.InterruptedJobException;
import static fr.jmmc.oiexplorer.core.util.FitsImageUtils.updateDataRange;
import fr.jmmc.oitools.image.FitsImage;
import fr.jmmc.oitools.image.FitsImageFile;
import fr.jmmc.oitools.image.FitsImageHDU;
import fr.nom.tam.fits.FitsException;
import java.awt.Rectangle;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.image.IndexColorModel;
import java.io.IOException;
import java.lang.ref.WeakReference;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import net.jafama.DoubleWrapper;
import net.jafama.FastMath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This stateless class generates UV Map Image for the given user model (fits image or cube) and UV area
 * based on FFT computation
 * 
 * Note on FastMath performance:
 * FastMath: use two tables for cos/sin [2049 doubles] i.e. 2 x 2049 x 8 = 32784 bytes
 *
 * @author Laurent BOURGES.
 */
@SuppressWarnings("ArrayHashCode")
public final class UserModelService {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(UserModelService.class.getName());
    /** maximum fft size (power of two) */
    public static final int MAX_FFT_SIZE = 128 * 1024;
    /** Two PI constant */
    public static final double TWO_PI = 2d * Math.PI;
    /** 1 nano arcsec in radians (to compare increments) */
    private static final double INC_EPSILON_RAD = Math.toRadians(1e-9 * ALX.ARCSEC_IN_DEGREES);
    /** formatter for frequencies */
    private final static DecimalFormat df = new DecimalFormat("0.00#E0");
    /** formatter for angles */
    private final static DecimalFormat df3 = new DecimalFormat("0.0##");
    /** number of floats per data point */
    public final static int DATA_1D_POINT_SIZE = 3;
    /** shared InterruptedJobException instance */
    private static final InterruptedJobException ije = new InterruptedJobException("UserModelService.computeUVMap: interrupted");
    /** lock used by localDoubleWrappers */
    private final static Object lockDoubleWrappers = new Object();
    /**
     * ThreadLocal giving DoubleWrapper instances
     */
    private static final ThreadLocal<DoubleWrapper[]> localDoubleWrappers = new ThreadLocal<DoubleWrapper[]>() {
        @Override
        protected DoubleWrapper[] initialValue() {
            final int length = 8; // 4 x (8 Object + 8 double) = 64 (cache line padding)

            // synchronize to allocate arrays and instances together and (hopefully) contiguous:
            synchronized (lockDoubleWrappers) {
                final DoubleWrapper[] array = new DoubleWrapper[length];
                for (int i = 0; i < length; i++) {
                    array[i] = new DoubleWrapper();
                }
                return array;
            }
        }
    };

    /** FastMath mode (fast or quick) */
    public enum MathMode {

        /** default (JDK), slow but very accurate: more than 1e-15 */
        DEFAULT,
        /** fast but very accurate: up to 1e-15 */
        FAST,
        /** faster but not accurate: up to 1e-3 */
        QUICK
    }

    /**
     * Forbidden constructor
     */
    private UserModelService() {
        // no-op
    }

    /**
     * Load the given user model file and prepare ONLY the first image for FFT processing and direct Fourier transform
     * @param userModel user model to load and prepare
     * @throws FitsException if any FITS error occured
     * @throws IOException IO failure
     * @throws IllegalArgumentException if unsupported unit or unit conversion is not allowed or image has invalid keyword(s) / data
     */
    public static void prepareUserModel(final UserModel userModel) throws FitsException, IOException, IllegalArgumentException {
        prepareUserModel(userModel, Preferences.getInstance().isFastUserModel(), Preferences.getInstance().getFastError());
    }

    /**
     * Load the given user model file and prepare ONLY the first image for FFT processing and direct Fourier transform
     * @param userModel user model to load and prepare
     * @param useFastMode true to ignore useless data (faster); false to have highest precision
     * @param fastError fast mode threshold in percents
     * @throws FitsException if any FITS error occured
     * @throws IOException IO failure
     * @throws IllegalArgumentException if unsupported unit or unit conversion is not allowed or image has invalid keyword(s) / data
     */
    public static void prepareUserModel(final UserModel userModel, final boolean useFastMode, final double fastError) throws FitsException, IOException, IllegalArgumentException {
        prepareUserModel(userModel, useFastMode, fastError, false, new ApodizationParameters());
    }

    /**
     * Load the given user model file and prepare ONLY the first image for FFT processing and direct Fourier transform
     * @param userModel user model to load and prepare
     * @param params apodization parameters
     * @throws FitsException if any FITS error occured
     * @throws IOException IO failure
     * @throws IllegalArgumentException if unsupported unit or unit conversion is not allowed or image has invalid keyword(s) / data
     */
    public static void prepareUserModel(final UserModel userModel, final ApodizationParameters params) throws FitsException, IOException, IllegalArgumentException {
        prepareUserModel(userModel,
                Preferences.getInstance().isFastUserModel(), Preferences.getInstance().getFastError(),
                Preferences.getInstance().isDoUserModelApodization(), params
        );
    }

    /**
     * Load the given user model file and prepare ONLY the first image for FFT processing and direct Fourier transform
     * @param userModel user model to load and prepare
     * @param useFastMode true to ignore useless data (faster); false to have highest precision
     * @param fastError fast mode threshold in percents
     * @param doApodise true to perform image apodization
     * @param params apodization parameters
     * @throws FitsException if any FITS error occured
     * @throws IOException IO failure
     * @throws IllegalArgumentException if unsupported unit or unit conversion is not allowed or image has invalid keyword(s) / data
     */
    public static void prepareUserModel(final UserModel userModel, final boolean useFastMode, final double fastError,
                                        final boolean doApodise, final ApodizationParameters params) throws FitsException, IOException, IllegalArgumentException {
        // clear previously cached data:
        userModel.setModelDataList(null);

        logger.info("loading user model: {}", userModel.getFile());

        // throws FitsException or IOException or IllegalArgumentException if the image can not be read properly:
        // note: load only the first valid image HDU (image or cube):
        final FitsImageFile imgFitsFile = FitsImageUtils.load(userModel.getFile(), true);

        if (imgFitsFile.getImageHDUCount() == 0) {
            throw new FitsException("The Fits file '" + userModel.getFile() + "' does not contain any supported Fits image !");
        }

        logger.info("useFastMode: {}", useFastMode);

        if (userModel.getScaleX() != null) {
            logger.info("scaleX:      {}", userModel.getScaleX());
        }
        if (userModel.getScaleY() != null) {
            logger.info("scaleY:      {}", userModel.getScaleY());
        }
        if (userModel.getRotation() != null) {
            logger.info("rotation:    {}", userModel.getRotation());
        }

        final FitsImageHDU fitsImageHDU = imgFitsFile.getFitsImageHDUs().get(0); // only first HDU

        final List<UserModelData> modelDataList = new ArrayList<UserModelData>(fitsImageHDU.getImageCount());

        IllegalArgumentException firstException = null;
        final StringBuilder preparationReport = new StringBuilder(512);

        final long start = System.nanoTime();

        // Step 1: prepare images and get ROI:
        for (final FitsImage fitsImage : fitsImageHDU.getFitsImages()) {
            // Set User transform (scale & rotation):
            defineUserTransform(userModel, fitsImage);
            try {
                final UserModelData modelData = new UserModelData();

                // note: fits image instance can be modified by image preparation:
                // can throw IllegalArgumentException if image has invalid keyword(s) / data:
                prepareImageStep1(fitsImage, modelData, useFastMode, fastError, doApodise, params);

                logger.debug("Prepared FitsImage: {}", fitsImage);

                modelDataList.add(modelData);
            } catch (IllegalArgumentException iae) {
                if (firstException == null) {
                    firstException = iae;
                }
                preparationReport.append(iae.getMessage()).append('\n');
            }
        }

        // exception occured during image preparation(s):
        if (firstException != null) {
            logger.warn("FitsImage preparation has failure(s):\n{}", preparationReport.toString());

            if (modelDataList.isEmpty()) {
                throw firstException;
            }
        }

        // Step 2: adjust ROI to largest area for FITS cubes:
        updateRoi(modelDataList);

        // prepare images (roi + data1d):
        for (final UserModelData modelData : modelDataList) {
            prepareImageStep2(modelData);

            logger.debug("Prepared FitsImage: {}", modelData.getFitsImage());
        }

        // update min/max range to be consistent accross the cube:
        updateDataRange(fitsImageHDU);

        logger.info("prepareUserModel: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

        if (logger.isDebugEnabled()) {
            logger.debug("prepareUserModel: modelDataList:");

            for (final UserModelData modelData : modelDataList) {
                logger.debug("{}", modelData);
            }
        }
        // update cached data if no exception occured:
        userModel.setModelDataList(modelDataList);
    }

    private static void defineUserTransform(final UserModel userModel, final FitsImage fitsImage) {
        final double incCol = (userModel.getScaleX() != null) ? userModel.getScaleX() : Double.NaN;
        final double incRow = (userModel.getScaleY() != null) ? userModel.getScaleY() : Double.NaN;
        FitsImageUtils.rescaleImage(fitsImage, incCol, incRow);

        if (userModel.getRotation() != null) {
            fitsImage.setRotAngle(userModel.getRotation());
        }
    }

    /**
     * Validate the given user model i.e. both file and image values are valid
     *
     * @param model user model to validate
     * @param uvMaxFreq maximum UV frequency (rad-1)
     *
     * @throws IllegalArgumentException if the file or the image is invalid !
     */
    public static void validateModel(final UserModel model, final double uvMaxFreq) throws IllegalArgumentException {
        if (model == null) {
            throw new IllegalStateException("User model is empty !");
        }
        if (!model.isModelDataReady()) {
            throw new IllegalStateException("Fits image(s) are not prepared !");
        }

        for (final UserModelData modelData : model.getModelDataList()) {
            final FitsImage fitsImage = modelData.getFitsImage();

            // check CRC:
            // note: only possible with one Fits image or one Fits cube (single HDU):
            if (model.getChecksum() != fitsImage.getFitsImageHDU().getChecksum()) {
                throw new IllegalArgumentException("Fits image checksum is incorrect; please verify your file (probably modified) !");
            }
            checkFitsImage(fitsImage, uvMaxFreq);
        }
    }

    /**
     * Check the given fits image with the given corrected UV Max (rad-1)
     * @param fitsImage fits image to check
     * @param uvMaxFreq maximum UV frequency (rad-1)
     *
     * @throws IllegalArgumentException if the fits image is invalid (undefined increments or too small increments)
     * @throws IllegalStateException if the image is invalid (null or not square)
     */
    private static void checkFitsImage(final FitsImage fitsImage, final double uvMaxFreq) {
        if (fitsImage == null) {
            throw new IllegalStateException("Fits image is empty !");
        }

        // Suppose the image is square (see FitsImageUtils.prepareImageStep1):
        if (fitsImage.getNbCols() != fitsImage.getNbRows()) {
            throw new IllegalStateException("Fits image must be a square image !");
        }

        if (!fitsImage.isIncColDefined() || !fitsImage.isIncRowDefined()) {
            throw new IllegalArgumentException("Undefined pixel increments (rad) !");
        }

        // Note: different X/Y axis increments are not supported:
        if (!NumberUtils.equals(fitsImage.getIncCol(), fitsImage.getIncRow(), INC_EPSILON_RAD)) {
            throw new IllegalArgumentException("Fits image increments along row and column axes must be equals !");
        }
        if (uvMaxFreq > 0.0) {
            final double increment = fitsImage.getIncRow();
            final double maxIncrement = getMaxIncrement(uvMaxFreq);

            if (increment > maxIncrement) {
                final double maxFreq = getMaxFreq(fitsImage);

                throw new IllegalArgumentException("Fits image [" + fitsImage.getFitsImageIdentifier()
                        + "] must have smaller pixel increments [expected "
                        + FitsImage.getAngleAsString(increment, df3) + " < "
                        + FitsImage.getAngleAsString(maxIncrement, df3) + "] to have a maximum frequency [expected "
                        + df.format(maxFreq) + " rad-1 > " + df.format(uvMaxFreq) + " rad-1 ] !");
            }
        }
    }

    public static double getMaxFreq(final FitsImage fitsImage) {
        final double increment = fitsImage.getIncRow();
        return 1.0 / (2.0 * increment); // nyquist => factor 2
    }

    public static double getMaxIncrement(final double uvMaxFreq) {
        return 1.0 / (2.0 * uvMaxFreq); // = lambda_min / 2 Bmax
    }

    /**
     * Compute the UV Map for to the given user model (fits image or cube)
     *
     * @param fitsImage user model as FitsImage
     * @param uvRect expected UV frequency area in rad-1
     * @param mode image mode (amplitude or phase)
     * @param imageSize expected number of pixels for both width and height of the generated image
     * @param colorModel color model to use
     * @param colorScale color scaling method
     * @return UVMapData
     *
     * @throws InterruptedJobException if the current thread is interrupted (cancelled)
     * @throws IllegalArgumentException if the fits image is invalid (not square or too small increments)
     * @throws RuntimeException if any exception occured during the computation
     */
    public static UVMapData computeUVMap(final FitsImage fitsImage,
                                         final Rectangle2D.Double uvRect,
                                         final ImageMode mode,
                                         final int imageSize,
                                         final IndexColorModel colorModel,
                                         final ColorScale colorScale) {
        return computeUVMap(fitsImage, uvRect, mode, imageSize, colorModel, colorScale, null, null, null, null);
    }

    /**
     * Compute the UV Map for to the given user model (fits image or cube)
     *
     * @param fitsImage user model as FitsImage
     * @param uvRect expected UV frequency area in rad-1
     * @param mode image mode (amplitude or phase)
     * @param imageSize expected number of pixels for both width and height of the generated image
     * @param colorModel color model to use
     * @param colorScale color scaling method
     * @param noiseService optional noise service to compute noisy complex visibilities before computing amplitude or phase
     * @return UVMapData
     *
     * @throws InterruptedJobException if the current thread is interrupted (cancelled)
     * @throws IllegalArgumentException if the fits image is invalid (not square or too small increments)
     * @throws RuntimeException if any exception occured during the computation
     */
    public static UVMapData computeUVMap(final FitsImage fitsImage,
                                         final Rectangle2D.Double uvRect,
                                         final ImageMode mode,
                                         final int imageSize,
                                         final IndexColorModel colorModel,
                                         final ColorScale colorScale,
                                         final VisNoiseService noiseService) {
        return computeUVMap(fitsImage, uvRect, mode, imageSize, colorModel, colorScale, noiseService, null, null, null);
    }

    /**
     * Compute the UV Map for to the given user model (fits image or cube)
     *
     * @param fitsImage user model as FitsImage
     * @param uvRect expected UV frequency area in rad-1
     * @param mode image mode (amplitude or phase)
     * @param imageSize expected number of pixels for both width and height of the generated image
     * @param colorModel color model to use
     * @param colorScale color scaling method
     * @param refVisData reference complex visibility data (optional)
     * @param refMin minimum reference value used only for sub images
     * @param refMax maximum reference value used only for sub images
     * @param noiseService optional noise service to compute noisy complex visibilities before computing amplitude or phase
     * @return UVMapData
     *
     * @throws InterruptedJobException if the current thread is interrupted (cancelled)
     * @throws IllegalArgumentException if the fits image is invalid (not square or too small increments)
     * @throws RuntimeException if any exception occured during the computation
     */
    public static UVMapData computeUVMap(final FitsImage fitsImage,
                                         final Rectangle2D.Double uvRect,
                                         final ImageMode mode,
                                         final int imageSize,
                                         final IndexColorModel colorModel,
                                         final ColorScale colorScale,
                                         final VisNoiseService noiseService,
                                         final Float refMin, final Float refMax,
                                         final float[][] refVisData) {

        // Note: do not support sub region (uvRect)
        // Get uvMaxFreq from uv rectangle:
        final double rectUvMaxFreq = Math.max(Math.max(Math.max(Math.abs(uvRect.getX()), Math.abs(uvRect.getY())),
                Math.abs(uvRect.getX() + uvRect.getWidth())),
                Math.abs(uvRect.getY() + uvRect.getHeight()));

        // todo enhance image size to fit sub image!
        logger.debug("UserModelService.computeUVMap: rectUvMaxFreq (rad-1): {}", rectUvMaxFreq);

        // throws exceptions:
        // do not check uv max:
        checkFitsImage(fitsImage, 0.0);

        if (fitsImage == null) {
            return null;
        }

        // Fix uv max according to the image max frequency:
        final double imgMaxFreq = getMaxFreq(fitsImage);

        final double uvMaxFreq = (rectUvMaxFreq < imgMaxFreq) ? rectUvMaxFreq : imgMaxFreq;
        logger.debug("UserModelService.computeUVMap: Fixed uvMaxFreq (rad-1): {}", uvMaxFreq);

        /** Get the current thread to check if the computation is interrupted */
        final Thread currentThread = Thread.currentThread();

        // Start the computations :
        final long start = System.nanoTime();

        final double mapScale;
        final double rotationAngle;

        if (fitsImage.isRotAngleDefined()) {
            rotationAngle = fitsImage.getRotAngle();
            Rectangle2D imgRectRef = new Rectangle2D.Double(0, 0, uvMaxFreq, uvMaxFreq);

            // angle sign is same direction (North -> East):
            final double theta = Math.toRadians(rotationAngle);

            final AffineTransform at = AffineTransform.getRotateInstance(theta, 0, 0);

            if (logger.isDebugEnabled()) {
                logger.debug("uv rect: {}", imgRectRef);
            }

            imgRectRef = ImageUtils.getBoundingBox(at, imgRectRef);

            if (logger.isDebugEnabled()) {
                logger.debug("rotated uv rect: {}", imgRectRef);
            }
            mapScale = imgRectRef.getWidth() / uvMaxFreq;
        } else {
            mapScale = 1.0;
            rotationAngle = 0.0;
        }
        logger.debug("mapScale: {}", mapScale);

        // input image is always squared:
        final int inputSize = fitsImage.getNbRows();
        logger.debug("Image size: {}", inputSize);

        // TODO: handle asymetric increments:
        final double increment = fitsImage.getIncRow();
        logger.debug("Current increment (rad): {}", increment);

        final double scaleFreqPerPix = 1d / (2d * increment);
        logger.debug("Freq scale per Pixel (rad-1): {}", scaleFreqPerPix);

        // UV / maxFreq ratio per FT pixel:
        final double ratioFreqPerPix = uvMaxFreq / scaleFreqPerPix;
        logger.debug("Ratio freq per Pixel: {}", ratioFreqPerPix);

        // find best FFT size:
        final int fftSize = findBestFFTSize(ratioFreqPerPix, imageSize, inputSize);

        // use the next even integer for pixel size & use map scale (larger):
        int dataSize = getOutputSize(mapScale * ratioFreqPerPix, fftSize);

        // make FFT larger (2 pixels more to avoid boundary errors):
        int fftOutputSize = dataSize + 2;

        logger.debug("UV plane FFT size (pixels): {}", fftOutputSize);

        // Fix FFT size due to map scale:
        if (fftSize < fftOutputSize) {
            // use all possible FFT pixels => reduce UV max:
            fftOutputSize = fftSize;
            dataSize = fftOutputSize - 2;
        }

        final int outputSize = (int) Math.ceil(dataSize / mapScale);

        logger.debug("dataSize:   {}", dataSize);
        logger.debug("outputSize: {}", outputSize);

        final double mapUvMaxFreq = (scaleFreqPerPix * outputSize) / fftSize;
        logger.debug("UVMap exact uvMaxFreq (m): {}", mapUvMaxFreq);

        // fast interrupt :
        if (currentThread.isInterrupted()) {
            throw ije;
        }

        // fft data as float [rows][cols] packed:
        float[][] visData = null;
        float[][] imgData = null;

        try {
            if (refVisData == null || fftOutputSize != refVisData.length) {
                // use single precision for FFT performance (image needs not double precision) :

                // 1 - compute FFT
                // TODO: cache the FFT in the Target object or save it to disk (temp) ...
                visData = FFTUtils.computeFFT(inputSize, fitsImage.getData(), fftSize, fftOutputSize);

                // fast interrupt :
                if (currentThread.isInterrupted()) {
                    throw ije;
                }
            } else {
                // use reference complex visibility data:
                visData = refVisData;
            }

            // 2 - Extract the amplitude/phase/square amplitude to get the uv map :
            // data as float [rows][cols]:
            imgData = FFTUtils.convert(fftOutputSize, visData, mode, dataSize, noiseService);

            // fast interrupt :
            if (currentThread.isInterrupted()) {
                throw ije;
            }

            // 3 - Get the image with the given color model and color scale :
            final Rectangle2D.Double uvMapRect = new Rectangle2D.Double();
            uvMapRect.setFrameFromDiagonal(-mapUvMaxFreq, -mapUvMaxFreq, mapUvMaxFreq, mapUvMaxFreq);

            final UVMapData uvMapData = ModelUVMapService.computeImage(uvRect, refMin, refMax, mode, imageSize, colorModel, colorScale,
                    dataSize, visData, imgData, uvMapRect, noiseService, rotationAngle, outputSize);

            logger.info("compute : duration = {} ms.", 1e-6d * (System.nanoTime() - start));

            return uvMapData;

        } catch (RuntimeException re) {
            logger.debug("recycleArrays <= interrupted job:");
            // recycle arrays:
            if (visData != refVisData) {
                FloatArrayCache.recycleArray(visData);
            }
            // rethrow exception:
            throw re;
        } finally {
            // recycle arrays:
            FloatArrayCache.recycleArray(imgData);
        }
    }

    /**
     * Return the best FFT size (power of two) i.e. giving the output size closest than the expected image size
     * @param ratioFreqPerPix Ratio freq per Pixel
     * @param imageSize expected image size in pixels
     * @param inputSize image input size to check minimum FFT size
     * @return best FFT size (power of two)
     */
    private static int findBestFFTSize(final double ratioFreqPerPix, final int imageSize, final int inputSize) {

        final int fftSizeMax = getFFTSize(ratioFreqPerPix, imageSize, inputSize);
        final int fftSizeMin = getFFTSize(ratioFreqPerPix, imageSize / 2, inputSize);

        if (fftSizeMin == fftSizeMax) {
            return fftSizeMin;
        }

        final int outputSizeMax = getOutputSize(ratioFreqPerPix, fftSizeMax);
        final int outputSizeMin = getOutputSize(ratioFreqPerPix, fftSizeMin);

        // keep fftSize that gives outputSize closest to imageSize:
        final int fftSize = (imageSize - outputSizeMin < outputSizeMax - imageSize) ? fftSizeMin : fftSizeMax;

        logger.debug("Best FFT size (pixels): {}", fftSize);

        return fftSize;
    }

    /**
     * Return the FFT size (power of two) for the given ratio and expected image size
     * @param ratio UV / maxFreq ratio
     * @param imageSize expected image size in pixels
     * @param inputSize image input size to check minimum FFT size
     * @return best FFT size (power of two)
     */
    private static int getFFTSize(final double ratio, final int imageSize, final int inputSize) {
        // Correct power of two for FFT:
        int fftSize = FFTUtils.getPowerOfTwo((int) Math.ceil(imageSize / ratio));

        logger.debug("For UV plane size (pixels): {}", imageSize);
        logger.debug("FFT size (pixels): {}", fftSize);

        if (fftSize > MAX_FFT_SIZE) {
            fftSize = MAX_FFT_SIZE;
            logger.info("Max FFT size reached (pixels): {}", fftSize);
        }
        if (fftSize < inputSize) {
            fftSize = FFTUtils.getPowerOfTwo(inputSize);
            logger.info("Min FFT size reached (input pixels): {}", fftSize);
        }
        return fftSize;
    }

    /**
     * Return the output size (even number) for the given ratio and FFT size
     * @param ratioFreqPerPix Ratio freq per Pixel
     * @param fftSize FFT size (power of two)
     * @return best FFT size (power of two)
     */
    private static int getOutputSize(final double ratioFreqPerPix, final int fftSize) {

        final double outputExactSize = fftSize * ratioFreqPerPix;
        logger.debug("UV plane exact size (pixels): {}", outputExactSize);

        // use the next even integer for pixel size:
        int outputSize = (int) Math.ceil(outputExactSize);
        if (outputSize % 2 != 0) {
            outputSize++;
        }

        logger.debug("UV plane size (pixels): {}", outputSize);

        return outputSize;
    }

    /**
     * Compute the spatial coordinates (rad) (relative to the half length) given the number of values and the increment
     * @param length number of values
     * @param increment increment (rad)
     * @return spatial coordinates (rad) (relative to the half length)
     */
    public static double[] computeSpatialCoords(final int length, final double increment) {

        final double[] coords = new double[length];

        final double half = length / 2.0;

        for (int i = 0; i < length; i++) {
            coords[i] = increment * (i - half); //in sky (radians).
        }

        return coords;
    }

    /**
     * Compute the complex visiblity of the given user model for the given Ufreq and Vfreq arrays
     *
     * @param data1D user model data as 1D array
     * @param fromData index of the first data to process
     * @param endData index of the last data to process (exclusive)
     * @param ufreq U frequencies in rad-1 x 2 PI
     * @param vfreq V frequencies in rad-1 x 2 PI
     * @param vis complex visibility array
     * @param from index of the first wavelength to compute
     * @param end index of the last wavelength to compute (exclusive)
     * @param weights scaling weights applied to complex visiblity (re/im)
     * @param mathMode Math mode to use to compute trigonometric functions
     */
    public static void computeModel(final float[] data1D, final int fromData, final int endData,
                                    final double[] ufreq, final double[] vfreq, final MutableComplex[] vis, final int from, final int end,
                                    final double[] weights, final MathMode mathMode) {

        if (data1D != null && ufreq != null && vfreq != null && vis != null) {

            if (ufreq.length != vfreq.length || vis.length < ufreq.length) {
                throw new IllegalStateException("Incorrect array sizes (Ufreq, VFreq, Vis) !");
            }

            // compute complex visiblities using exact fourier transform (slow):
            compute1D(data1D, fromData, endData, ufreq, vfreq, vis, from, end, weights, mathMode);
        }
    }

    /**
     * Compute exact discrete fourier transform / complex visiblity of given user model for the given Ufreq and Vfreq arrays
     * @param data1D user model data as 1D array
     * @param fromData index of the first data to process
     * @param endData index of the last data to process (exclusive)
     * @param ufreq U frequencies in rad-1
     * @param vfreq V frequencies in rad-1
     * @param vis complex visibility array
     * @param from index of the first wavelength to compute
     * @param end index of the last wavelength to compute (exclusive)
     * @param weights scaling weights applied to complex visiblity (re/im)
     * @param mathMode Math mode to use to compute trigonometric functions
     */
    private static void compute1D(final float[] data1D, final int fromData, final int endData,
                                  final double[] ufreq, final double[] vfreq, final MutableComplex[] vis, final int from, final int end,
                                  final double[] weights, final MathMode mathMode) {

        /** Get the current thread to check if the computation is interrupted */
        final Thread currentThread = Thread.currentThread();

        final int len = DATA_1D_POINT_SIZE;
        final int lenData = endData;

        logger.debug("from {} to {}", from, end);

        if (mathMode == MathMode.FAST) {
            final DoubleWrapper[] dw = localDoubleWrappers.get();
            final DoubleWrapper cw = dw[0];

            // iterate on ufreq / vfreq / vis by wavelength:
            for (int i = from, j; i < end; i++) {
                final double kwCol = TWO_PI * ufreq[i];
                final double kwRow = TWO_PI * vfreq[i];

                // reset:
                double re = 0.0;
                double im = 0.0;

                // iterate on data points [data xfreq yfreq]:
                for (j = fromData; j < lenData; j += len) {
                    final double flux = data1D[j];
                    final double z = kwCol * data1D[j + 1] + kwRow * data1D[j + 2];

                    im -= flux * FastMath.sinAndCos(z, cw); // cw holds cosine
                    re += flux * cw.value;
                } // data1D

                // update complex instance (mutable):
                vis[i].add(weights[i] * re, weights[i] * im);

                // fast interrupt :
                if (currentThread.isInterrupted()) {
                    return;
                }
            } // vis
        } else if (mathMode == MathMode.QUICK) {
            // iterate on ufreq / vfreq / vis by wavelength:
            for (int i = from, j; i < end; i++) {
                final double kwCol = TWO_PI * ufreq[i];
                final double kwRow = TWO_PI * vfreq[i];

                // reset:
                double re = 0.0;
                double im = 0.0;

                // iterate on data points [data xfreq yfreq]:
                for (j = fromData; j < lenData; j += len) {
                    final double flux = data1D[j];
                    final double z = kwCol * data1D[j + 1] + kwRow * data1D[j + 2];

                    re += flux * FastMath.cosQuick(z);
                    im -= flux * FastMath.sinQuick(z);
                } // data1D

                // update complex instance (mutable):
                vis[i].add(weights[i] * re, weights[i] * im);

                // fast interrupt :
                if (currentThread.isInterrupted()) {
                    return;
                }
            } // vis
        } else {
            // iterate on ufreq / vfreq / vis by wavelength:
            for (int i = from, j; i < end; i++) {
                final double kwCol = TWO_PI * ufreq[i];
                final double kwRow = TWO_PI * vfreq[i];

                // reset:
                double re = 0.0;
                double im = 0.0;

                // iterate on data points [data xfreq yfreq]:
                for (j = fromData; j < lenData; j += len) {
                    final double z = kwCol * data1D[j + 1] + kwRow * data1D[j + 2];
                    final double flux = data1D[j];

                    // use Math (not StrictMath):
                    re += flux * Math.cos(z);
                    im -= flux * Math.sin(z);
                } // data1D

                // update complex instance (mutable):
                vis[i].add(weights[i] * re, weights[i] * im);

                // fast interrupt :
                if (currentThread.isInterrupted()) {
                    return;
                }
            } // vis
        } // math mode
    }

    /**
     * Prepare the given image for FFT (normalize, threshold, pad to next power of two) and direct Fourier transform.
     * Update the given FitsImage by the prepared FitsImage ready for FFT and prepared model data for direct Fourier transform
     * @param fitsImage FitsImage to process
     * @param modelData prepared model data for direct Fourier transform
     * @param useFastMode true to ignore useless data (faster); false to have highest precision
     * @param fastError fast mode threshold in percents
     * @param doApodise true to perform image apodization
     * @param params apodization parameters
     * @throws IllegalArgumentException if image has invalid keyword(s) / data
     */
    public static void prepareImageStep1(final FitsImage fitsImage, final UserModelData modelData,
                                         final boolean useFastMode, final double fastError,
                                         final boolean doApodise, final ApodizationParameters params) throws IllegalArgumentException {

        if (!fitsImage.isDataRangeDefined()) {
            // update boundaries excluding zero values:
            FitsImageUtils.updateDataRangeExcludingZero(fitsImage);
        }

        // in place modifications:
        float[][] data = fitsImage.getData();
        int nbRows = fitsImage.getNbRows();
        int nbCols = fitsImage.getNbCols();

        // 1 - Ignore negative values:
        if (fitsImage.getDataMax() <= 0.0) {
            throw new IllegalArgumentException("Fits image [" + fitsImage.getFitsImageIdentifier() + "] has only negative data !");
        }

        logger.debug("Image size: {} x {}", nbRows, nbCols);

        if (fitsImage.getDataMin() < 0.0) {
            final float threshold = 0.0f;

            final ImageLowerThresholdJob thresholdJob = new ImageLowerThresholdJob(data, nbCols, nbRows, threshold, 0.0f);
            logger.debug("ImageLowerThresholdJob - threshold = {} (ignore negative values)", threshold);

            thresholdJob.forkAndJoin();

            logger.debug("ImageLowerThresholdJob - updateCount: {}", thresholdJob.getUpdateCount());

            // update boundaries excluding zero values:
            FitsImageUtils.updateDataRangeExcludingZero(fitsImage);
        }

        if (doApodise) {
            // Perform image apodization
            final double airyRadius = apodize(fitsImage, params);

            // Store radius:
            modelData.setAiryRadius(airyRadius);

            if (!Double.isNaN(airyRadius)) {
                // update boundaries excluding zero values:
                FitsImageUtils.updateDataRangeExcludingZero(fitsImage);
            }
        }

        // keep initial total flux after apodisation (before normalization):
        final double totalFlux = fitsImage.getSum();
        logger.debug("Total flux: {}", totalFlux);

        // 2 - Normalize data (total flux):
        if (!NumberUtils.equals(totalFlux, 1.0, 1e-3)) {
            final double normFactor = 1.0 / totalFlux;

            final ImageNormalizeJob normJob = new ImageNormalizeJob(data, nbCols, nbRows, normFactor);
            logger.debug("ImageNormalizeJob - factor: {}", normFactor);

            normJob.forkAndJoin();

            // update boundaries excluding zero values:
            FitsImageUtils.updateDataRangeExcludingZero(fitsImage);
        }

        // 2.1 - Determine flux threshold to ignore useless values:
        final float thresholdFlux;

        if (useFastMode) {
            final double error = Math.max(0.0, Math.min(fastError, 0.1)); // clamp error between [0% - 10%]
            logger.debug("Fast error: {} %", 100.0 * error);

            final int nData = fitsImage.getNData();
            final float[] data1D = sortData(fitsImage);

            // Use 1.0 as data are normalized:
            final double upperThreshold = (1.0 - error);
            logger.debug("UpperThreshold: {}", upperThreshold);

            final int thIdx = findThresholdIndex(data1D, upperThreshold);

            if (thIdx != -1) {
                final double thPixRatio = (100.0 * (nData - thIdx)) / nData;
                logger.debug("Ratio: {} % selected pixels", NumberUtils.trimTo3Digits(thPixRatio));

                thresholdFlux = data1D[thIdx];
                logger.debug("thresholdFlux: {}", thresholdFlux);
            } else {
                thresholdFlux = 0.0f;
            }

            // 2.2 - Skip too small data values i.e. lower than thresholdImage ie below 10^-6:
            if (thresholdFlux > 0.0f && fitsImage.getDataMin() < thresholdFlux) {

                final ImageLowerThresholdJob thresholdJob = new ImageLowerThresholdJob(data, nbCols, nbRows, thresholdFlux, 0.0f);
                logger.debug("ImageLowerThresholdJob - threshold: {}", thresholdFlux);

                thresholdJob.forkAndJoin();

                logger.debug("ImageLowerThresholdJob - updateCount: {}", thresholdJob.getUpdateCount());

                // update boundaries excluding zero values:
                FitsImageUtils.updateDataRangeExcludingZero(fitsImage);
            }
        } else {
            thresholdFlux = 0.0f;
        }

        // 3 - Locate useful data values inside image:
        final ImageRegionThresholdJob regionJob = new ImageRegionThresholdJob(data, nbCols, nbRows, thresholdFlux);

        logger.debug("ImageRegionThresholdJob: thresholdFlux: {}", thresholdFlux);
        regionJob.forkAndJoin();

        // 4 - Extract ROI:
        int rows1, rows2, cols1, cols2;

        // use non zero area:
        rows1 = regionJob.getRowLowerIndex();
        rows2 = regionJob.getRowUpperIndex();
        cols1 = regionJob.getColumnLowerIndex();
        cols2 = regionJob.getColumnUpperIndex();

        logger.debug("ImageRegionThresholdJob: row indexes: [{} - {}]", rows1, rows2);
        logger.debug("ImageRegionThresholdJob: col indexes: [{} - {}]", cols1, cols2);

        // end of step 1:
        modelData.setFitsImage(fitsImage);
        modelData.setTotalFlux(totalFlux);
        modelData.setThresholdFlux(thresholdFlux);
        modelData.setRoi(new Rectangle(rows1, cols1, rows2 - rows1, cols2 - cols1));
    }

    public static void updateRoi(final List<UserModelData> modelDataList) {
        int rows1 = Integer.MAX_VALUE, rows2 = Integer.MIN_VALUE;
        int cols1 = Integer.MAX_VALUE, cols2 = Integer.MIN_VALUE;

        // Get overall ROI on all images:
        for (final UserModelData modelData : modelDataList) {
            final Rectangle roi = modelData.getRoi();

            if (roi.x < rows1) {
                rows1 = roi.x;
            }
            int max = roi.x + roi.width;
            if (max > rows2) {
                rows2 = max;
            }

            if (roi.y < cols1) {
                cols1 = roi.y;
            }
            max = roi.y + roi.height;
            if (max > cols2) {
                cols2 = max;
            }
        }

        logger.debug("updateRoi: row indexes: [{} - {}]", rows1, rows2);
        logger.debug("updateRoi: col indexes: [{} - {}]", cols1, cols2);

        final UserModelData modelDataFirst = modelDataList.get(0);
        final int nbRows = modelDataFirst.getFitsImage().getNbRows();
        final int nbCols = modelDataFirst.getFitsImage().getNbCols();

        // keep the center of the ROI and keep the image square (width = height = even number):
        final float halfRows = 0.5f * nbRows;
        final float halfCols = 0.5f * nbCols;

        final float rowDistToCenter = Math.max(Math.abs(halfRows - rows1), Math.abs(halfRows - rows2));
        final float colDistToCenter = Math.max(Math.abs(halfCols - cols1), Math.abs(halfCols - cols2));

        logger.debug("updateRoi: rowDistToCenter: {}", rowDistToCenter);
        logger.debug("updateRoi: colDistToCenter: {}", colDistToCenter);

        // ensure minimum size to 2 pixels + proper rounding:
        final float distToCenter = Math.max(1f, Math.max(rowDistToCenter, colDistToCenter)) + 0.5f;
        logger.debug("updateRoi: distToCenter: {}", distToCenter);

        // range check ?
        rows1 = (int) Math.floor(halfRows - distToCenter);
        rows2 = (int) Math.ceil(halfRows + distToCenter);

        // fix width to be an even number:
        if ((rows2 - rows1) % 2 != 0) {
            rows2++;
        }

        logger.debug("updateRoi: even row indexes: {} - {}", rows1, rows2);

        // range check ?
        cols1 = (int) Math.floor(halfCols - distToCenter);
        cols2 = (int) Math.ceil(halfCols + distToCenter);

        // fix width to be an even number:
        if ((cols2 - cols1) % 2 != 0) {
            cols2++;
        }

        logger.debug("updateRoi: even col indexes: {} - {}", cols1, cols2);

        final Rectangle rect = new Rectangle(rows1, cols1, rows2 - rows1, cols2 - cols1);
        logger.info("updateRoi: {}", rect);

        for (final UserModelData modelData : modelDataList) {
            modelData.setRoi(rect);
        }
    }

    /**
     * Prepare the given image for FFT and direct Fourier transform.
     * Update the given FitsImage by the prepared FitsImage ready for FFT and prepared model data for direct Fourier transform
     * @param modelData prepared model data for direct Fourier transform
     */
    public static void prepareImageStep2(final UserModelData modelData) {
        final FitsImage fitsImage = modelData.getFitsImage();

        // in place modifications:
        float[][] data = fitsImage.getData();
        int nbRows = fitsImage.getNbRows();
        int nbCols = fitsImage.getNbCols();

        // 4 - Extract ROI:
        int rows1, rows2, cols1, cols2;

        // use non zero area:
        final Rectangle roi = modelData.getRoi();
        modelData.setRoi(null); // reset
        rows1 = roi.x;
        rows2 = roi.x + roi.width;
        cols1 = roi.y;
        cols2 = roi.y + roi.height;

        // update fits image:
        // note: this extraction does not check boundary overlapping:
        data = ImageArrayUtils.extract(nbRows, nbCols, data, rows1, cols1, rows2, cols2);

        if (data == null) {
            // outside ranges:
            data = fitsImage.getData();
        } else {
            // update data:
            FitsImageUtils.updateFitsImage(fitsImage, data);

            // update ref pixel:
            fitsImage.setPixRefRow(fitsImage.getPixRefRow() - rows1);
            fitsImage.setPixRefCol(fitsImage.getPixRefCol() - cols1);

            nbRows = fitsImage.getNbRows();
            nbCols = fitsImage.getNbCols();

            logger.debug("ROI size = {} x {}", nbRows, nbCols);
        }

        // 5 - Make sure the image is square i.e. padding (width = height = even number):
        final int newSize = Math.max(
                (nbRows % 2 != 0) ? nbRows + 1 : nbRows,
                (nbCols % 2 != 0) ? nbCols + 1 : nbCols);

        if (newSize != nbRows || newSize != nbCols) {
            data = ImageArrayUtils.enlarge(nbRows, nbCols, data, newSize, newSize);

            // update data/dataMin/dataMax:
            FitsImageUtils.updateFitsImage(fitsImage, data, fitsImage.getDataMin(), fitsImage.getDataMax());

            // update ref pixel:
            fitsImage.setPixRefRow(fitsImage.getPixRefRow() + ((newSize - nbRows) / 2.0));
            fitsImage.setPixRefCol(fitsImage.getPixRefCol() + ((newSize - nbCols) / 2.0));

            nbRows = fitsImage.getNbRows();
            nbCols = fitsImage.getNbCols();

            logger.debug("Square size = {} x {}", nbRows, nbCols);
        }

        // 6 - flip axes to have positive increments (left to right for the column axis and bottom to top for the row axis)
        // note: flip operation requires image size to be an even number
        final double incRow = fitsImage.getSignedIncRow();
        if (incRow < 0.0) {
            // flip row axis:
            final ImageFlipJob flipJob = new ImageFlipJob(data, nbCols, nbRows, false);

            flipJob.forkAndJoin();

            logger.debug("ImageFlipJob - flipY done");

            fitsImage.setSignedIncRow(-incRow);
        }

        final double incCol = fitsImage.getSignedIncCol();
        if (incCol < 0.0) {
            // flip column axis:
            final ImageFlipJob flipJob = new ImageFlipJob(data, nbCols, nbRows, true);

            flipJob.forkAndJoin();

            logger.debug("ImageFlipJob - flipX done");

            fitsImage.setSignedIncCol(-incCol);
        }

        // Update FFT ready fits image in model data:
        modelData.setFitsImage(fitsImage);

        // 7 - prepare model data to compute direct Fourier transform:
        prepareModelData(fitsImage, modelData);
    }

    /**
     * Flatten 2D image data to 1D array and sort data by ascending order
     *
     * @param fitsImage user model as FitsImage
     * @return sorted model data as 1D array
     */
    public static float[] sortData(final FitsImage fitsImage) {

        final int nbRows = fitsImage.getNbRows();
        final int nbCols = fitsImage.getNbCols();
        final int nData = fitsImage.getNData();
        final float[][] data = fitsImage.getData();

        // prepare 1D data:
        float[] row;
        int n1D = 0;
        final float[] data1D = new float[nData];

        float flux;

        // iterate on rows:
        for (int r = 0, c; r < nbRows; r++) {
            row = data[r];

            // iterate on columns:
            for (c = 0; c < nbCols; c++) {
                flux = row[c];

                // skip values different from zero:
                if (flux > 0.0f) {
                    // keep this data point:
                    data1D[n1D] = flux;
                    n1D++;
                }
            } // columns
        } // rows

        logger.debug("FitsImage: used pixels = {} / {}", n1D, nData);

        // Ideally use parallel sort (JDK 1.8+):
//        edu.sorting.DualPivotQuicksort.sort(data1D, ParallelJobExecutor.getInstance().getMaxParallelJob(), 0, n1D);
        // For now use JDK sort (compatible with JDK 6/7):
        Arrays.sort(data1D, 0, n1D);

        logger.debug("FitsImage: {} float sorted.", n1D);

        if (n1D != nData) {
            return Arrays.copyOf(data1D, n1D);
        }
        return data1D;
    }

    /** weak reference on a recycled single float array for prepareModelData() */
    private static WeakReference<float[]> recycled_array = null;

    private static float[] getArray(final int length) {
        float[] array;
        if (SwingUtils.isEDT() && (recycled_array != null)) {
            array = recycled_array.get();
            if (array != null && array.length >= length) {
                recycled_array = null; // free reference
                if (logger.isDebugEnabled()) {
                    logger.debug("get array: {} @ {}", array.length, array.hashCode());
                }
                return array;
            }
        }
        return new float[(length * 12) / 10]; // 20% more
    }

    private static void putArray(final float[] array) {
        if (SwingUtils.isEDT()) {
            if (logger.isDebugEnabled()) {
                logger.debug("put array: {} @ {}", array.length, array.hashCode());
            }
            recycled_array = new WeakReference<float[]>(array);
        }
    }

    /**
     * Prepare the user model data to compute direct Fourier transform
     *
     * @param fitsImage user model as FitsImage
     * @param modelData prepared model data for direct Fourier transform
     */
    private static void prepareModelData(final FitsImage fitsImage, final UserModelData modelData) {

        final float threshold = modelData.getThresholdFlux();
        logger.debug("prepareModelData: threshold: {}", threshold);

        // note: square image (and even size):
        final int nbRows = fitsImage.getNbRows();
        final int nbCols = fitsImage.getNbCols();
        final int nData = fitsImage.getNData();
        final float[][] data = fitsImage.getData();
        final double dataMin = fitsImage.getDataMin();
        final double dataMax = fitsImage.getDataMax();

        logger.debug("prepareModelData: min: {} - max: {}", dataMin, dataMax);

        final int nPixels = nbRows * nbCols;

        logger.debug("prepareModelData: nData: {} / {}", nData, nPixels);

        final double cosTheta;
        final double sinTheta;

        if (fitsImage.isRotAngleDefined()) {
            final double rotationAngle = fitsImage.getRotAngle();

            // angle sign is inverse direction (North -> East):
            final double theta = Math.toRadians(rotationAngle);

            cosTheta = FastMath.cos(theta);
            sinTheta = FastMath.sin(theta);
        } else {
            cosTheta = 1.0;
            sinTheta = 0.0;
        }

        // prepare spatial coordinates:
        final double[] colCoords = UserModelService.computeSpatialCoords(nbCols, fitsImage.getSignedIncCol()); // X
        final double[] rowCoords = UserModelService.computeSpatialCoords(nbRows, fitsImage.getSignedIncRow()); // Y

        // prepare 1D data (eliminate values lower than threshold):
        float[] row;
        int nUsedData = 0;

        // Recycle large data arrays (weak reference cache) for fits cubes (many spectral channels having roughly same nData)
        final float[] data1D = getArray(nData * DATA_1D_POINT_SIZE);

        float flux;
        double rowCoord, colCoord;

        // iterate on rows:
        for (int r = 0, c; r < nbRows; r++) {
            row = data[r];
            rowCoord = rowCoords[r];

            // iterate on columns:
            for (c = 0; c < nbCols; c++) {
                flux = row[c];

                // skip values lower than threshold:
                if (flux > threshold) {
                    colCoord = colCoords[c];

                    // Transform coordinates:
                    // keep this data point:
                    data1D[nUsedData] = flux;
                    data1D[nUsedData + 1] = (float) Functions.transformU(colCoord, rowCoord, cosTheta, sinTheta);
                    data1D[nUsedData + 2] = (float) Functions.transformV(colCoord, rowCoord, cosTheta, sinTheta);
                    nUsedData += DATA_1D_POINT_SIZE;
                }
            } // columns
        } // rows

        logger.info("prepareModelData: used pixels = {} / {}", (nUsedData / DATA_1D_POINT_SIZE), nPixels);

        // trim array size:
        if (nUsedData != data1D.length) {
            final float[] mData1D = new float[nUsedData];

            System.arraycopy(data1D, 0, mData1D, 0, nUsedData);

            // recycle data1D array:
            putArray(data1D);

            modelData.set(mData1D);
        } else {
            modelData.set(data1D);
        }
    }

    /**
     * Find the threshold index when the sum of values becomes strictly higher than the given upper threshold
     * @param data1D sorted data (ascending order)
     * @param upperThreshold upper threshold to use
     * @return threshold value or -1 if not found
     */
    private static int findThresholdIndex(final float[] data1D, final double upperThreshold) {
        if (logger.isDebugEnabled()) {
            logger.debug("findThresholdIndex: upperThreshold: {}", upperThreshold);
        }

        float value;
        float lastValue = 0.0f;
        double partialFlux = 0.0;

        for (int i = data1D.length - 1; i >= 0; i--) {
            value = data1D[i];
            partialFlux += value;

            if (partialFlux > upperThreshold) {
                // keep equal values
                if (lastValue != 0.0f) {
                    if (value != lastValue) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("findThresholdValue: threshold reached: {} > {} - value = {} - nPixels = {}",
                                    partialFlux, upperThreshold, value, (data1D.length - 1 - i));
                        }
                        return i;
                    }
                } else {
                    lastValue = value;
                }
            }
        }
        return -1;
    }

    public static boolean checkAiryRadius(final UserModel userModel, final ApodizationParameters params) {
        if (!userModel.isModelDataReady()) {
            return false;
        }
        final UserModelData modelData = userModel.getModelData(0);
        final FitsImage fitsImage = modelData.getFitsImage();

        final double airyRadius = getAiryRadius(fitsImage, params);

        final boolean valid = (Double.isNaN(airyRadius) && Double.isNaN(modelData.getAiryRadius()))
                || (airyRadius == modelData.getAiryRadius());

        if (logger.isDebugEnabled()) {
            logger.debug("checkAiryRadius: lambda: {} - diameter: {}", params.lambda, params.diameter);
            logger.debug("checkAiryRadius[{}]: airy radius expected = {} - model = {}", valid,
                    FitsImage.getAngleAsString(airyRadius, df3),
                    FitsImage.getAngleAsString(modelData.getAiryRadius(), df3)
            );
        }

        return valid;
    }

    private static double getAiryRadius(final FitsImage fitsImage, final ApodizationParameters params) {
        // check real lambda ie airy radius of the given image: 
        double lambda = !Double.isNaN(fitsImage.getWaveLength()) ? fitsImage.getWaveLength() : params.lambdaMin;

        // ensure lambda is within the instrument (or mode) range:
        if (lambda < params.lambdaMin) {
            lambda = params.lambdaMin;
        }
        if (lambda > params.lambdaMax) {
            lambda = params.lambdaMax;
        }
        params.lambda = lambda;

        final double airyRadius = getAiryRadius(params);
        if (Double.isNaN(airyRadius)) {
            return Double.NaN;
        }

        // check airy radius vs image Fov:
        final double imageRadius = 0.5 * fitsImage.getOrigMaxAngle();

        if (logger.isDebugEnabled()) {
            logger.debug("getAiryRadius: airy radius  = {}", FitsImage.getAngleAsString(airyRadius, df3));
            logger.debug("getAiryRadius: image radius = {}", FitsImage.getAngleAsString(imageRadius, df3));

            // check weight (see apodize method):
            final double sigma = getGaussianStddev(airyRadius);

            // min(gaussian weight) on image boundaries:
            // see ImageGaussianFilterJob weights:
            final double weight = FastMath.exp(-(imageRadius * imageRadius) / (2.0 * sigma * sigma));

            logger.debug("weight[{}] = {}", NumberUtils.trimTo3Digits(imageRadius / airyRadius), weight);
        }

        // if ratio (image radius / airy radius) > 20% then gaussian weight becomes below 0.9 at image boundaries
        // weight[(imageRadius / airyRadius) = 0.2] = 0.8950250709279723
        if (imageRadius < getAiryRadiusThreshold(airyRadius)) {
            // skip apodization:
            return Double.NaN;
        }

        return airyRadius;
    }

    /**
     * Get threshold (percents) on the airy radius
     * @param airyRadius
     * @return threshold (percents) on the airy radius
     */
    public static double getAiryRadiusThreshold(final double airyRadius) {
        return 0.2 * airyRadius;
    }

    /**
     * Get the airy disk FHWM ~ 1.029 (lambdaMin / diameter) i.e smaller than radius = zero at 1.22 (lambdaMin / diameter)
     * @param params apodization parameters
     * @return airy disk FHWM
     */
    public static double getAiryRadius(final ApodizationParameters params) {
        if (Double.isNaN(params.diameter) || (params.diameter <= 0.0)
                || Double.isNaN(params.lambda) || (params.lambda <= 0.0)) {
            return Double.NaN;
        }
        final double wavelength = (!Double.isNaN(params.lambdaRef) && (params.lambdaRef > 0.0)) ? params.lambdaRef : params.lambda;
        if (wavelength != params.lambda) {
            params.lambda = wavelength; // report value
        }

        final double factor = (!Double.isNaN(params.scalingFactor) && (params.scalingFactor > 0.0))
                ? params.scalingFactor : ALX.AIRY_DISK_FWHM_FACTOR;

        // see https://en.wikipedia.org/wiki/Airy_disk
        // note: airy disk 1st zero at 1.22 x lambdaMin / diameter
        // use the airy disk FWHM instead (~ 1.0 * lambdaMin / diameter):
        return factor * wavelength / params.diameter;
    }

    /** constant 1 / 2.35 ~ 0.42 */
    private static final double FWHM_TO_STDDEV = 1.0 / (2.0 * Math.sqrt(2.0 * Math.log(2.0)));

    /**
     * Return the gaussian stddev equivalent to the given FWHM
     * @param fwhm gaussian FHWM
     * @return gaussian stddev 
     */
    public static double getGaussianStddev(final double fwhm) {
        return fwhm * FWHM_TO_STDDEV;
    }

    /**
     * Performs image apodization ie multiply image by the telescope airy disk (gaussian profile in fact) if necessary
     * @param fitsImage image to transform (in-place)
     * @param params apodization parameters
     * @return airy disk FHWM if apodisation performed; NaN otherwise
     */
    private static double apodize(final FitsImage fitsImage, final ApodizationParameters params) {
        // check if apodization is necessary according to the image FOV vs radius:
        final double airyRadius = getAiryRadius(fitsImage, params);
        // e.g. airyRadius is not NaN:
        if (!Double.isNaN(airyRadius)) {
            // Start the computations :
            final long start = System.nanoTime();

            logger.info("apodize: airy radius = {}", FitsImage.getAngleAsString(airyRadius, df3));

            // equivalent gaussian profile:
            final double sigma = getGaussianStddev(airyRadius);

            if (logger.isDebugEnabled()) {
                logger.debug("apodize: lambda: {} - diameter: {}", params.lambda, params.diameter);
                logger.debug("apodize: sigma       = {}", sigma);
            }

            final int nbRows = fitsImage.getNbRows();
            final int nbCols = fitsImage.getNbCols();

            final ImageGaussianFilterJob filterJob = new ImageGaussianFilterJob(
                    fitsImage.getData(), nbCols, nbRows,
                    UserModelService.computeSpatialCoords(nbCols, fitsImage.getSignedIncCol()),
                    UserModelService.computeSpatialCoords(nbRows, fitsImage.getSignedIncRow()),
                    sigma
            );

            filterJob.forkAndJoin();

            if (logger.isDebugEnabled()) {
                logger.debug("apodize: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
            }
        }
        return airyRadius;
    }

    /**
     * test
     * @param unused 
     */
    public static void main(String[] unused) {
        final double diameter = 1.8;

        final double lambdaMin = 1.5E-6;
//        final double lambdaMin = 3.5E-6;
//        final double lambdaMin = 10.5E-6;

        final double lambdaMax = 5E-6;

        final ApodizationParameters params = new ApodizationParameters(diameter, lambdaMin, lambdaMax);

        final double airyRadius = getAiryRadius(params);

        logger.info("airy radius  = {}", FitsImage.getAngleAsString(airyRadius, df3));

        // equivalent gaussian profile:
        final double sigma = getGaussianStddev(airyRadius);

        logger.info("sigma        = {}", FitsImage.getAngleAsString(sigma, df3));

        // min(gaussian weight) on image boundaries:
        for (double r = 0.0; r <= 1.3 * airyRadius; r += airyRadius * 0.05) {
            final double imageRadius = r;

            // see ImageGaussianFilterJob weights:
            final double weight = FastMath.exp(-(imageRadius * imageRadius) / (2.0 * sigma * sigma));

            logger.info("weight[{} <=> {}] = {}",
                    NumberUtils.trimTo3Digits(imageRadius / airyRadius), FitsImage.getAngleAsString(imageRadius, df3), weight);
        }
    }
}
