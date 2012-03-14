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
  /** minimum visiblity threshold (1e-4) for model image */
  public static final float MIN_VISIBILITY_UV_MAP = 1e-4f;
  /** minimum visiblity threshold (1e-2) for direct fourier transform */
  public static final float MIN_VISIBILITY_DATA = 1e-2f;
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
   * Load the FitsImageFile structure and prepare ONLY the first image for FFT processing
   * @param file fits file to load and process 
   * @return fitsImage user model as FitsImage
   * @throws FitsException if any FITS error occured
   * @throws IOException IO failure
   */
  public static FitsImage prepareFitsFile(final String file) throws FitsException, IOException {
    final FitsImageFile imgFitsFile = FitsImageUtils.load(file);

    if (imgFitsFile.getImageCount() == 0) {
      throw new FitsException("The Fits file '" + file + "' does not contain any supported Fits image !");
    }

    final boolean useFastMode = Preferences.getInstance().isFastUserModel();
    logger.info("useFastMode: " + useFastMode);

    final long start = System.nanoTime();

    final FitsImage fitsImage = FitsImageUtils.prepareImage(imgFitsFile.getFirstFitsImage(),
            (useFastMode) ? MIN_VISIBILITY_UV_MAP : 0f);

    if (logger.isInfoEnabled()) {
      logger.info("prepareFitsFile : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");
      logger.info("Prepared FitsImage: " + fitsImage.toString(false));
    }

    return fitsImage;
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
    return computeUVMap(fitsImage, uvRect, mode, imageSize, colorModel, colorScale, null, null);
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
    return computeUVMap(fitsImage, uvRect, mode, imageSize, colorModel, colorScale, noiseService, null);
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
          final float[][] refVisData) {

    // Get corrected uvMax from uv rectangle (-this.uvMax, -this.uvMax, this.uvMax, this.uvMax):
    final double uvMax = -uvRect.getX();
    logger.info("UserModelService.computeUVMap: uvMax (rad-1) = " + uvMax);

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
    logger.info("Image size = " + inputSize);

    final double increment = fitsImage.getIncRow();
    logger.info("Current increment (rad) = " + increment);

    final double maxFreq = 1d / (2d * increment);
    logger.info("Max UV (rad-1) = " + maxFreq);

    // UV / maxFreq ratio
    final double ratio = uvMax / maxFreq;
    logger.info("ratio = " + ratio);

    // find best FFT size:
    final int fftSize = findBestFFTSize(ratio, imageSize, inputSize);

    // use the next even integer for pixel size:
    final int outputSize = getOutputSize(ratio, fftSize);

    final double mapUvMax = (maxFreq * outputSize) / fftSize;
    logger.info("UVMap exact uvMax (m) = " + mapUvMax);

    // make FFT larger (2 pixels more to avoid boundary errors):
    final int fftOutputSize = outputSize + 2;

    logger.info("UV plane FFT size (pixels) = " + fftOutputSize);

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

    final UVMapData uvMapData = ModelUVMapService.computeImage(uvRect, null, null, mode, imageSize, colorModel, colorScale,
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

    logger.info("Best FFT size (pixels) = " + fftSize);

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

    logger.info("For UV plane size (pixels) = " + imageSize);
    logger.info("FFT size (pixels) = " + fftSize);

    if (fftSize > MAX_FFT_SIZE) {
      fftSize = MAX_FFT_SIZE;
      logger.info("Max FFT size reached (pixels) = " + fftSize);
    }
    if (fftSize < inputSize) {
      fftSize = FFTUtils.getPowerOfTwo(inputSize);
      logger.info("Min FFT size reached (pixels) = " + fftSize);
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
    logger.info("UV plane exact size (pixels) = " + outputExactSize);

    // use the next even integer for pixel size:
    int outputSize = (int) Math.ceil(outputExactSize);
    if (outputSize % 2 == 1) {
      outputSize++;
    }

    logger.info("UV plane size (pixels) = " + outputSize);

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
    return new UserModelComputeContext(context.getFreqCount(), context.getN1D(), context.getData1D(), context.getColCoord1D(), context.getRowCoord1D());
  }

  /**
   * Prepare the complex visiblity computation of given models
   *
   * @param fitsImage user model as FitsImage
   * @param freqCount uv frequencey count used to preallocate arrays
   * @return new compute context
   */
  public static UserModelComputeContext prepareModel(final FitsImage fitsImage, final int freqCount) {
    if (fitsImage == null || freqCount <= 0) {
      return null;
    }

    /** Get the current thread to check if the computation is interrupted */
    final Thread currentThread = Thread.currentThread();

    final int nbRows = fitsImage.getNbRows();
    final int nbCols = fitsImage.getNbCols();
    final int nData = fitsImage.getNData();
    final float[][] data = fitsImage.getData();
    final double dataMin = fitsImage.getDataMin();
    final double dataMax = fitsImage.getDataMax();

    logger.info("FitsImage: min: " + dataMin + " - max: " + dataMax);

    final int nPixels = nbRows * nbCols;
    logger.info("FitsImage: nData: " + nData + " / " + nPixels);

    // prepare spatial coordinates:
    final float[] rowCoords = UserModelService.computeSpatialCoords(nbRows, fitsImage.getSignedIncRow());
    final float[] colCoords = UserModelService.computeSpatialCoords(nbCols, fitsImage.getSignedIncCol());


    final boolean useFastMode = Preferences.getInstance().isFastUserModel();

    // Skip zero values only as the image was previously filtered to ignore small values:
    final float threshold = (useFastMode) ? 2f * MIN_VISIBILITY_DATA / nData : 0f;
    logger.info("FitsImage: threshold = " + threshold);


    // prepare 1D data (eliminate values lower than threshold):
    float[] row;
    int n1D = 0;
    final float[] data1D = new float[nPixels];
    final float[] rowCoord1D = new float[nPixels];
    final float[] colCoord1D = new float[nPixels];

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
          data1D[n1D] = flux;
          rowCoord1D[n1D] = rowCoord;
          colCoord1D[n1D] = colCoords[c];
          n1D++;
        }

        // fast interrupt :
        if (currentThread.isInterrupted()) {
          return null;
        }
      } // columns
    } // rows

    logger.info("FitsImage: used pixels = " + n1D + " / " + nPixels);

    return new UserModelComputeContext(freqCount, n1D, data1D, colCoord1D, rowCoord1D);
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

      final int n1D = context.getN1D();
      final float[] data1D = context.getData1D();
      final float[] colCoord1D = context.getColCoord1D();
      final float[] rowCoord1D = context.getRowCoord1D();

      // compute complex visiblities using exact fourier transform (slow):
      compute1D(n1D, data1D, colCoord1D, rowCoord1D, ufreq, vfreq, nVis, vis);
    }

    return vis;
  }

  /**
   * Compute exact discrete fourier transform / complex visiblity of given user model for the given Ufreq and Vfreq arrays
   * @param n1D number number of data points
   * @param data1D flattened data points (1D)
   * @param colCoord1D spatial coordinates along the column axis (rad) corresponding to data points
   * @param rowCoord1D spatial coordinates along the row axis (rad) corresponding to data points
   * @param ufreq U frequencies in rad-1
   * @param vfreq V frequencies in rad-1
   * @param nVis number of visibility to compute
   * @param vis complex visibility array
   */
  private static void compute1D(final int n1D, final float[] data1D,
          final float[] colCoord1D, final float[] rowCoord1D,
          final double[] ufreq, final double[] vfreq, final int nVis, final MutableComplex[] vis) {

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
}
