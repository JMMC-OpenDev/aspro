/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.image.FitsImageUtils;
import fr.jmmc.aspro.model.oi.UserModel;
import fr.jmmc.jmal.complex.MutableComplex;
import fr.jmmc.jmal.image.FFTUtils;
import fr.jmmc.jmal.image.ColorScale;
import fr.jmmc.jmal.image.ImageArrayUtils;
import fr.jmmc.jmal.image.job.ImageFlipJob;
import fr.jmmc.jmal.image.job.ImageLowerThresholdJob;
import fr.jmmc.jmal.image.job.ImageNormalizeJob;
import fr.jmmc.jmal.image.job.ImageRegionThresholdJob;
import fr.jmmc.jmal.model.ImageMode;
import fr.jmmc.jmal.model.ModelUVMapService;
import fr.jmmc.jmal.model.UVMapData;
import fr.jmmc.jmal.model.VisNoiseService;
import fr.jmmc.jmcs.util.concurrent.InterruptedJobException;
import fr.jmmc.oitools.image.FitsImage;
import fr.jmmc.oitools.image.FitsImageFile;
import fr.nom.tam.fits.FitsException;
import java.awt.geom.Rectangle2D;
import java.awt.image.IndexColorModel;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.Arrays;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This stateless class generates UV Map Image for the given user model (fits image or cube) and UV area
 * based on FFT computation
 *
 * @author Laurent BOURGES.
 */
public final class UserModelService {

  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(UserModelService.class.getName());
  /** minimum visiblity threshold (1e-2) for direct fourier transform */
  public static final double MIN_VISIBILITY_DATA = 1e-2d;
  /** ratio to ignore data values i.e. data value < LIMIT_RATIO * dataLowThreshold */
  public static final float LIMIT_RATIO = 1e-3f;
  /** maximum fft size (power of two) */
  public static final int MAX_FFT_SIZE = 256 * 1024;
  /** Two PI constant */
  public static final double TWO_PI = 6.28318530717958623199592693708837032d;
  /** formatter for frequencies */
  private final static DecimalFormat df = new DecimalFormat("0.00#E0");

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
   */
  public static void prepareUserModel(final UserModel userModel) throws FitsException, IOException {
    // clear previously cached data:
    userModel.setFitsImage(null);
    userModel.setModelData(null);

    // throws FitsException or IOException if the image can not be read properly:
    final FitsImageFile imgFitsFile = FitsImageUtils.load(userModel.getFile());

    if (imgFitsFile.getImageCount() == 0) {
      throw new FitsException("The Fits file '" + userModel.getFile() + "' does not contain any supported Fits image !");
    }

    final boolean useFastMode = Preferences.getInstance().isFastUserModel();
    logger.info("useFastMode: {}", useFastMode);

    final long start = System.nanoTime();

    final FitsImage fitsImage = imgFitsFile.getFirstFitsImage();
    final UserModelData modelData = new UserModelData();

    prepareImage(fitsImage, modelData, useFastMode);

    if (logger.isInfoEnabled()) {
      logger.info("prepareFitsFile: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
      logger.info("Prepared FitsImage: {}", fitsImage.toString(false));
    }

    // update cached data if no exception occured:
    userModel.setFitsImage(fitsImage);
    userModel.setModelData(modelData);
  }

  /**
   * Validate the given user model i.e. both file and image values are valid
   *
   * @param model user model to validate
   * @param uvMax maximum UV frequency (rad-1)
   *
   * @throws IllegalArgumentException if the file or the image is invalid !
   */
  public static void validateModel(final UserModel model, final double uvMax) throws IllegalArgumentException {
    if (model == null) {
      throw new IllegalStateException("User model is empty !");
    }
    if (model.getFitsImage() == null) {
      throw new IllegalStateException("Fits image is empty !");
    }

    final FitsImage fitsImage = model.getFitsImage();
    // check CRC:
    if (model.getChecksum() != fitsImage.getChecksum()) {
      throw new IllegalArgumentException("Fits image checksum is incorrect; please verify your file (probably modified) !");
    }
    checkFitsImage(fitsImage, uvMax);
  }

  /**
   * Check the given fits image with the given corrected UV Max (rad-1)
   * @param fitsImage fits image to check
   * @param uvMax maximum UV frequency (rad-1)
   *
   * @throws IllegalArgumentException if the fits image is invalid (undefined increments or too small increments)
   * @throws IllegalStateException if the image is invalid (null or not square)
   */
  private static void checkFitsImage(final FitsImage fitsImage, final double uvMax) {
    if (fitsImage == null) {
      throw new IllegalStateException("Fits image is empty !");
    }

    // Suppose the image is square (see FitsImageUtils.prepareImage):
    if (fitsImage.getNbCols() != fitsImage.getNbRows()) {
      throw new IllegalStateException("Fits image must be a square image !");
    }

    if (!fitsImage.isIncColDefined() || !fitsImage.isIncRowDefined()) {
      throw new IllegalArgumentException("Undefined pixel increments (rad) !");
    }

    if (fitsImage.getIncCol() != fitsImage.getIncRow()) {
      throw new IllegalArgumentException("Fits image increments along row and column axes must be equals !");
    }
    // TODO: support different axis increments ?

    final double increment = fitsImage.getIncRow();

    final double maxFreq = 1d / (2d * increment);

    if (maxFreq < uvMax) {
      throw new IllegalArgumentException("The Fits image [" + fitsImage.getFitsImageIdentifier()
              + "] must have smaller pixel increments (" + df.format(increment) + " rad) to have a maximum frequency ("
              + df.format(maxFreq) + " rad-1) larger than the corrected UV Max (" + df.format(uvMax) + " rad-1) !");
    }
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

    // Get corrected uvMax from uv rectangle (-this.uvMax, -this.uvMax, this.uvMax, this.uvMax):
    final double uvMax = Math.max(Math.max(Math.max(Math.abs(uvRect.getX()), Math.abs(uvRect.getY())),
            Math.abs(uvRect.getX() + uvRect.getWidth())),
            Math.abs(uvRect.getY() + uvRect.getHeight()));

    // todo enhance image size to fit sub image!

    logger.info("UserModelService.computeUVMap: uvMax (rad-1): {}", uvMax);

    // throws exceptions:
    checkFitsImage(fitsImage, uvMax);

    if (fitsImage == null) {
      return null;
    }

    /** Get the current thread to check if the computation is interrupted */
    final Thread currentThread = Thread.currentThread();

    // Start the computations :
    final long start = System.nanoTime();

    final int inputSize = fitsImage.getNbRows();
    logger.info("Image size: {}", inputSize);

    final double increment = fitsImage.getIncRow();
    logger.info("Current increment (rad): {}", increment);

    final double maxFreq = 1d / (2d * increment);
    logger.info("Max UV (rad-1): {}", maxFreq);

    // UV / maxFreq ratio
    final double ratio = uvMax / maxFreq;
    logger.info("ratio: {}", ratio);

    // find best FFT size:
    final int fftSize = findBestFFTSize(ratio, imageSize, inputSize);

    // use the next even integer for pixel size:
    final int outputSize = getOutputSize(ratio, fftSize);

    final double mapUvMax = (maxFreq * outputSize) / fftSize;
    logger.info("UVMap exact uvMax (m): {}", mapUvMax);

    // make FFT larger (2 pixels more to avoid boundary errors):
    final int fftOutputSize = outputSize + 2;

    logger.info("UV plane FFT size (pixels): {}", fftOutputSize);

    // fast interrupt :
    if (currentThread.isInterrupted()) {
      throw new InterruptedJobException("UserModelService.computeUVMap: interrupted");
    }


    // fft data as float [rows][cols] packed:
    final float[][] visData;

    if (refVisData == null || fftOutputSize != refVisData.length) {
      // use single precision for FFT performance (image needs not double precision) :

      // 1 - compute FFT
      // TODO: cache the FFT in the Target object or save it to disk (temp) ...

      visData = FFTUtils.computeFFT(inputSize, fitsImage.getData(), fftSize, fftOutputSize);

      // fast interrupt :
      if (currentThread.isInterrupted()) {
        throw new InterruptedJobException("UserModelService.computeUVMap: interrupted");
      }
    } else {
      // use reference complex visibility data:
      visData = refVisData;
    }


    // 2 - Extract the amplitude/phase to get the uv map :

    // data as float [rows][cols]:
    float[][] data = FFTUtils.convert(fftOutputSize, visData, mode, outputSize, noiseService);

    final int dataSize = outputSize;

    // fast interrupt :
    if (currentThread.isInterrupted()) {
      throw new InterruptedJobException("UserModelService.computeUVMap: interrupted");
    }


    // 3 - Get the image with the given color model and color scale :

    final Rectangle2D.Double uvMapRect = new Rectangle2D.Double();
    uvMapRect.setFrameFromDiagonal(-mapUvMax, -mapUvMax, mapUvMax, mapUvMax);

    final UVMapData uvMapData = ModelUVMapService.computeImage(uvRect, refMin, refMax, mode, imageSize, colorModel, colorScale,
            dataSize, visData, data, uvMapRect, noiseService);

    if (logger.isInfoEnabled()) {
      logger.info("compute : duration = {} ms.", 1e-6d * (System.nanoTime() - start));
    }
    return uvMapData;
  }

  /**
   * Return the best FFT size (power of two) i.e. giving the output size closest than the expected image size
   * @param ratio UV / maxFreq ratio
   * @param imageSize expected image size in pixels
   * @param inputSize image input size to check minimum FFT size
   * @return best FFT size (power of two)
   */
  private static int findBestFFTSize(final double ratio, final int imageSize, final int inputSize) {

    final int fftSizeMax = getFFTSize(ratio, imageSize, inputSize);
    final int fftSizeMin = getFFTSize(ratio, imageSize / 2, inputSize);

    if (fftSizeMin == fftSizeMax) {
      return fftSizeMin;
    }

    final int outputSizeMax = getOutputSize(ratio, fftSizeMax);
    final int outputSizeMin = getOutputSize(ratio, fftSizeMin);

    // keep fftSize that gives outputSize closest to imageSize:
    final int fftSize = (imageSize - outputSizeMin < outputSizeMax - imageSize) ? fftSizeMin : fftSizeMax;

    logger.info("Best FFT size (pixels): {}", fftSize);

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

    logger.info("For UV plane size (pixels): {}", imageSize);
    logger.info("FFT size (pixels): {}", fftSize);

    if (fftSize > MAX_FFT_SIZE) {
      fftSize = MAX_FFT_SIZE;
      logger.info("Max FFT size reached (pixels): {}", fftSize);
    }
    if (fftSize < inputSize) {
      fftSize = FFTUtils.getPowerOfTwo(inputSize);
      logger.info("Min FFT size reached (pixels): {}", fftSize);
    }
    return fftSize;
  }

  /**
   * Return the output size (even number) for the given ratio and FFT size
   * @param ratio UV / maxFreq ratio
   * @param fftSize FFT size (power of two)
   * @return best FFT size (power of two)
   */
  private static int getOutputSize(final double ratio, final int fftSize) {

    final double outputExactSize = fftSize * ratio;
    logger.info("UV plane exact size (pixels): {}", outputExactSize);

    // use the next even integer for pixel size:
    int outputSize = (int) Math.ceil(outputExactSize);
    if (outputSize % 2 != 0) {
      outputSize++;
    }

    logger.info("UV plane size (pixels): {}", outputSize);

    return outputSize;
  }

  /**
   * Compute the spatial coordinates (rad) (relative to the half length) given the number of values and the increment
   * @param length number of values
   * @param increment increment (rad)
   * @return spatial coordinates (rad) (relative to the half length)
   */
  public static float[] computeSpatialCoords(final int length, final double increment) {

    final float[] coords = new float[length];

    final int half = length / 2;

    for (int i = 0; i < length; i++) {
      coords[i] = (float) (increment * (i - half)); //in sky (radians).
      // On peut aussi utiliser les
      // valeurs du header avec values = (i-xref-1)*xdelt+xval
      // (fits commence à 1)
    }

    return coords;
  }

  /**
   * Clone the given context
   *
   * @param context compute context
   *
   * @return new compute context
   */
  public static UserModelComputeContext cloneContext(final UserModelComputeContext context) {
    return new UserModelComputeContext(context.getFreqCount(), context.getUserModelData());
  }

  /**
   * Prepare the complex visiblity computation of given models
   *
   * @param modelData user model data
   * @param freqCount uv frequencey count used to preallocate arrays
   * @return new compute context
   */
  public static UserModelComputeContext prepareModel(final UserModelData modelData, final int freqCount) {
    if (modelData == null || freqCount <= 0) {
      return null;
    }

    logger.info("ModelData: nData: {}", modelData.getNData());

    return new UserModelComputeContext(freqCount, modelData);
  }

  /**
   * Compute the complex visiblity of the given user model for the given Ufreq and Vfreq arrays
   *
   * @param context compute context
   * @param ufreq U frequencies in rad-1
   * @param vfreq V frequencies in rad-1
   * @return normalized complex visibility or null if thread interrupted
   */
  public static MutableComplex[] computeModel(final UserModelComputeContext context, final double[] ufreq, final double[] vfreq) {
    MutableComplex[] vis = null;

    if (ufreq != null && vfreq != null && context != null) {

      final int nVis = ufreq.length;

      if (nVis != vfreq.length || nVis != context.getFreqCount()) {
        throw new IllegalStateException("Incorrect array sizes (Ufreq, VFreq, freqCount) !");
      }

      // reset complex visiblities:
      vis = context.resetAndGetVis();

      // compute complex visiblities using exact fourier transform (slow):
      compute1D(context.getUserModelData(), ufreq, vfreq, nVis, vis);
    }

    return vis;
  }

  /**
   * Compute exact discrete fourier transform / complex visiblity of given user model for the given Ufreq and Vfreq arrays
   * @param modelData user model data
   * @param ufreq U frequencies in rad-1
   * @param vfreq V frequencies in rad-1
   * @param nVis number of visibility to compute
   * @param vis complex visibility array
   */
  private static void compute1D(final UserModelData modelData,
          final double[] ufreq, final double[] vfreq, final int nVis, final MutableComplex[] vis) {

    final int n1D = modelData.getNData();
    final float[] data1D = modelData.getData1D();
    final float[] colCoord1D = modelData.getColCoord1D();
    final float[] rowCoord1D = modelData.getRowCoord1D();

    /** Get the current thread to check if the computation is interrupted */
    final Thread currentThread = Thread.currentThread();

    double kwCol, kwRow, z, flux, re, im;

    // iterate on ufreq / vfreq:
    for (int i = 0, j; i < nVis; i++) {

      // divide by lambda ?
      kwCol = TWO_PI * ufreq[i];
      kwRow = TWO_PI * vfreq[i];

      // initialize:
      re = 0d;
      im = 0d;

      // iterate on data points:
      for (j = n1D - 1; j >= 0; j--) {
        flux = data1D[j];

        z = kwCol * colCoord1D[j] + kwRow * rowCoord1D[j];

        re += flux * Math.cos(z);
        im -= flux * Math.sin(z);

      } // data1D

      // fast interrupt :
      if (currentThread.isInterrupted()) {
        return;
      }

      // update complex instance (mutable):
      vis[i].updateComplex(re, im);

    } // vis
  }

  /**
   * Prepare the given image for FFT (normalize, threshold, pad to next power of two) and direct Fourier transform.
   * Update the given FitsImage by the prepared FitsImage ready for FFT and prepared model data for direct Fourier transform
   * @param fitsImage FitsImage to process
   * @param modelData prepared model data for direct Fourier transform
   * @param useFastMode true to ignore useless data
   */
  public static void prepareImage(final FitsImage fitsImage, final UserModelData modelData, final boolean useFastMode) {
    if (!fitsImage.isDataRangeDefined()) {
      // update boundaries excluding zero values:
      FitsImageUtils.updateDataRangeExcludingZero(fitsImage);
    }

    // in place modifications:
    float[][] data = fitsImage.getData();
    int nbRows = fitsImage.getNbRows();
    int nbCols = fitsImage.getNbCols();

    logger.info("Image size: {} x {}", nbRows, nbCols);


    // 1 - Ignore negative values:
    if (fitsImage.getDataMax() <= 0d) {
      throw new IllegalArgumentException("Fits image [" + fitsImage.getFitsImageIdentifier() + "] has only negative data !");
    }
    if (fitsImage.getDataMin() < 0d) {
      final float threshold = 0f;

      final ImageLowerThresholdJob thresholdJob = new ImageLowerThresholdJob(data, nbCols, nbRows, threshold, 0f);
      logger.info("ImageLowerThresholdJob - threshold = {} (ignore negative values)", threshold);

      thresholdJob.forkAndJoin();

      logger.info("ImageLowerThresholdJob - updateCount: {}", thresholdJob.getUpdateCount());

      // update boundaries excluding zero values:
      FitsImageUtils.updateDataRangeExcludingZero(fitsImage);
    }


    // 2 - Normalize data (total flux):
    if (fitsImage.getSum() != 1d) {
      final double normFactor = 1d / fitsImage.getSum();

      final ImageNormalizeJob normJob = new ImageNormalizeJob(data, nbCols, nbRows, normFactor);
      logger.info("ImageNormalizeJob - factor: {}", normFactor);

      normJob.forkAndJoin();

      // update boundaries excluding zero values:
      FitsImageUtils.updateDataRangeExcludingZero(fitsImage);
    }


    // 2.1 - Determine flux threshold to ignore useless values:
    final float thresholdImage;
    final float thresholdVis;

    if (useFastMode) {
      final double totalFlux = fitsImage.getSum();
      logger.info("Total flux: {}", totalFlux);

      final int nData = fitsImage.getNData();
      final float[] data1D = sortData(fitsImage);

      final int thLen = 4; // means MIN_VISIBILITY_DATA / 10^3
      final int[] thIdx = new int[thLen];
      int thValid = -1;

      double error = MIN_VISIBILITY_DATA;
      for (int i = 0; i < thLen; i++) {
        thIdx[i] = findThresholdIndex(data1D, totalFlux, error);
        if (thIdx[i] == -1) {
          break;
        }
        thValid++;
        error *= 1e-1d;
      }

      if (thValid != -1) {
        final int[] thPixRatio = new int[thValid + 1];

        for (int i = thValid; i >= 0; i--) {
          thPixRatio[i] = (100 * (nData - thIdx[i])) / nData;
        }

        logger.info("threshold ratios: {}", Arrays.toString(thPixRatio));

        // decide which valid threshold use (empirical):
        // idea: keep more pixels when the image is diluted:
        final int[] thPixLimits = new int[]{100, 95, 85, 50};
        int thLim = 0;
        for (int i = thValid; i >= 1; i--) {
          if (thPixRatio[i] < thPixLimits[i]) {
            thLim = i;
            break;
          }
        }
        logger.info("selected threshold ratio: {}", thPixRatio[thLim]);

        thresholdImage = data1D[thIdx[thLim]];

        thresholdVis = data1D[thIdx[0]];

        logger.info("thresholdVis: {}", thresholdVis);
        logger.info("thresholdImage: {}", thresholdImage);

      } else {
        thresholdImage = 0f;
        thresholdVis = 0f;
      }

    } else {
      thresholdImage = 0f;
      thresholdVis = 0f;
    }


    // 2.2 - Skip too small data values i.e. lower than thresholdImage / 10^6:
    if (useFastMode) {
      final float smallThreshold = LIMIT_RATIO * thresholdImage;

      if (fitsImage.getDataMin() < smallThreshold) {

        final ImageLowerThresholdJob thresholdJob = new ImageLowerThresholdJob(data, nbCols, nbRows, smallThreshold, 0f);
        logger.info("ImageLowerThresholdJob - threshold: {}", smallThreshold);

        thresholdJob.forkAndJoin();

        logger.info("ImageLowerThresholdJob - updateCount: {}", thresholdJob.getUpdateCount());

        // update boundaries excluding zero values:
        FitsImageUtils.updateDataRangeExcludingZero(fitsImage);
      }
    }


    // 3 - Locate useful data values inside image:
    final ImageRegionThresholdJob regionJob = new ImageRegionThresholdJob(data, nbCols, nbRows, thresholdImage);

    logger.info("ImageRegionThresholdJob: thresholdImage: {}", thresholdImage);
    regionJob.forkAndJoin();


    // 4 - Extract ROI:
    // keep the center of the ROI and keep the image square (width = height = even number):
    int rows1, rows2, cols1, cols2;

    final float halfRows = 0.5f * nbRows;
    final float halfCols = 0.5f * nbCols;

    final float distToCenter;

    // use non zero area:
    rows1 = regionJob.getRowLowerIndex();
    rows2 = regionJob.getRowUpperIndex();
    cols1 = regionJob.getColumnLowerIndex();
    cols2 = regionJob.getColumnUpperIndex();

    logger.info("ImageRegionThresholdJob: row indexes: {} - {}", rows1, rows2);
    logger.info("ImageRegionThresholdJob: col indexes: {} - {}", cols1, cols2);

    final float rowDistToCenter = Math.max(Math.abs(halfRows - rows1), Math.abs(halfRows - rows2));
    final float colDistToCenter = Math.max(Math.abs(halfCols - cols1), Math.abs(halfCols - cols2));

    logger.info("ImageRegionThresholdJob: rowDistToCenter: {}", rowDistToCenter);
    logger.info("ImageRegionThresholdJob: colDistToCenter: {}", colDistToCenter);

    distToCenter = Math.max(rowDistToCenter, colDistToCenter);
    logger.info("ImageRegionThresholdJob: distToCenter: {}", distToCenter);

    // range check ?
    rows1 = (int) Math.floor(halfRows - distToCenter);
    rows2 = (int) Math.ceil(halfRows + distToCenter);

    // fix width to be an even number:
    if ((rows2 - rows1) % 2 != 0) {
      rows2++;
    }

    logger.info("ImageRegionThresholdJob: even row indexes: {} - {}", rows1, rows2);

    // range check ?
    cols1 = (int) Math.floor(halfCols - distToCenter);
    cols2 = (int) Math.ceil(halfCols + distToCenter);

    // fix width to be an even number:
    if ((cols2 - cols1) % 2 != 0) {
      cols2++;
    }

    logger.info("ImageRegionThresholdJob: even col indexes: {} - {}", cols1, cols2);

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

      logger.info("ROI size = {} x {}", nbRows, nbCols);
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
      fitsImage.setPixRefRow(fitsImage.getPixRefRow() + 0.5d * (newSize - nbRows));
      fitsImage.setPixRefCol(fitsImage.getPixRefCol() + 0.5d * (newSize - nbCols));

      nbRows = fitsImage.getNbRows();
      nbCols = fitsImage.getNbCols();

      logger.info("Square size = {} x {}", nbRows, nbCols);
    }


    // 6 - flip axes to have positive increments (left to right for the column axis and bottom to top for the row axis)
    // note: flip operation requires image size to be an even number
    final double incRow = fitsImage.getSignedIncRow();
    if (incRow < 0d) {
      // flip row axis:
      final ImageFlipJob flipJob = new ImageFlipJob(data, nbCols, nbRows, false);

      flipJob.forkAndJoin();

      logger.info("ImageFlipJob - flipY done");

      fitsImage.setSignedIncRow(-incRow);
    }

    final double incCol = fitsImage.getSignedIncCol();
    if (incCol < 0d) {
      // flip column axis:
      final ImageFlipJob flipJob = new ImageFlipJob(data, nbCols, nbRows, true);

      flipJob.forkAndJoin();

      logger.info("ImageFlipJob - flipX done");

      fitsImage.setSignedIncCol(-incCol);
    }

    // 7 - prepare model data to compute direct Fourier transform:
    prepareModelData(fitsImage, modelData, thresholdVis);
  }

  /**
   * Flatten 2D image data to 1D array and sort data by ascending order
   *
   * @param fitsImage user model as FitsImage
   * @return sorted model data as 1D array
   */
  public static float[] sortData(final FitsImage fitsImage) {

    /** Get the current thread to check if the computation is interrupted */
    final Thread currentThread = Thread.currentThread();

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
        if (flux > 0f) {
          // keep this data point:
          data1D[n1D] = flux;
          n1D++;
        }
      } // columns

      // fast interrupt :
      if (currentThread.isInterrupted()) {
        return null;
      }
    } // rows

    logger.info("FitsImage: used pixels = {} / {}", n1D, nData);

    Arrays.sort(data1D);

    logger.info("FitsImage: {} float sorted.", n1D);
    if (false) {
      logger.info("FitsImage: {} float sorted: ", n1D, Arrays.toString(data1D));
    }

    return data1D;
  }

  /**
   * Prepare the user model data to compute direct Fourier transform
   *
   * @param fitsImage user model as FitsImage
   * @param modelData prepared model data for direct Fourier transform
   * @param threshold low threshold (ignore too small values)
   */
  private static void prepareModelData(final FitsImage fitsImage, final UserModelData modelData, final float threshold) {

    logger.info("prepareModelData: threshold: {}", threshold);

    /** Get the current thread to check if the computation is interrupted */
    final Thread currentThread = Thread.currentThread();

    final int nbRows = fitsImage.getNbRows();
    final int nbCols = fitsImage.getNbCols();
    final int nData = fitsImage.getNData();
    final float[][] data = fitsImage.getData();
    final double dataMin = fitsImage.getDataMin();
    final double dataMax = fitsImage.getDataMax();

    logger.info("prepareModelData: min: {} - max: {}", dataMin, dataMax);

    final int nPixels = nbRows * nbCols;

    logger.info("prepareModelData: nData: {} / {}", nData, nPixels);

    // prepare spatial coordinates:
    final float[] rowCoords = UserModelService.computeSpatialCoords(nbRows, fitsImage.getSignedIncRow());
    final float[] colCoords = UserModelService.computeSpatialCoords(nbCols, fitsImage.getSignedIncCol());


    // prepare 1D data (eliminate values lower than threshold):
    float[] row;
    int nUsedData = 0;
    final float[] data1D = new float[nData];
    final float[] colCoord1D = new float[nData];
    final float[] rowCoord1D = new float[nData];

    double totalFlux = 0d;
    float flux, rowCoord;

    // iterate on rows:
    for (int r = 0, c; r < nbRows; r++) {
      row = data[r];
      rowCoord = rowCoords[r];

      // iterate on columns:
      for (c = 0; c < nbCols; c++) {
        flux = row[c];

        // skip values lower than threshold:
        if (flux > threshold) {
          // keep this data point:
          totalFlux += flux;
          data1D[nUsedData] = flux;
          colCoord1D[nUsedData] = colCoords[c];
          rowCoord1D[nUsedData] = rowCoord;
          nUsedData++;
        }
      } // columns

      // fast interrupt :
      if (currentThread.isInterrupted()) {
        return;
      }
    } // rows

    logger.info("prepareModelData: used pixels = {} / {}", nUsedData, nPixels);


    // normalize flux to 1.0:
    logger.info("prepareModelData: totalFlux: {}", totalFlux);

    if (threshold != 0f) {
      final double normFactor = 1d / totalFlux;

      for (int i = nUsedData - 1; i >= 0; i--) {
        data1D[i] = (float) (normFactor * data1D[i]);
      }

      totalFlux = 0d;
      for (int i = nUsedData - 1; i >= 0; i--) {
        totalFlux += data1D[i];
      }
      logger.info("prepareModelData: totalFlux after normalization: {}", totalFlux);
    }

    // trim array size:
    if (nUsedData != nData) {
      final float[] mData1D = new float[nUsedData];
      final float[] mColCoord1D = new float[nUsedData];
      final float[] mRowCoord1D = new float[nUsedData];

      System.arraycopy(data1D, 0, mData1D, 0, nUsedData);
      System.arraycopy(rowCoord1D, 0, mRowCoord1D, 0, nUsedData);
      System.arraycopy(colCoord1D, 0, mColCoord1D, 0, nUsedData);

      modelData.set(nUsedData, mData1D, mColCoord1D, mRowCoord1D);
    } else {
      modelData.set(nUsedData, data1D, colCoord1D, rowCoord1D);
    }
  }

  /**
   * Find the threshold index when the sum of values becomes higher than the given upper threshold
   * @param data1D sorted data (ascending order)
   * @param total total of all values
   * @param error upper threshold
   * @return threshold value or 0.0 if not found
   */
  private static int findThresholdIndex(final float[] data1D, final double total, final double error) {
    final double upperThreshold = total * (1d - error);

    if (logger.isDebugEnabled()) {
      logger.debug("findThresholdIndex: upperThreshold: {}", upperThreshold);
    }

    float value;
    float lastValue = 0f;
    double partialFlux = 0d;

    for (int i = data1D.length - 1; i >= 0; i--) {
      value = data1D[i];
      partialFlux += value;

      if (partialFlux > upperThreshold) {
        // keep equal values
        if (lastValue != 0f) {
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
}
