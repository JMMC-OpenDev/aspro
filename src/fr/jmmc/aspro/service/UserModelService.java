/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.image.FitsImageUtils;
import fr.jmmc.jmal.image.FFTUtils;
import fr.jmmc.jmal.image.ColorScale;
import fr.jmmc.jmal.model.ImageMode;
import fr.jmmc.jmal.model.ModelUVMapService;
import fr.jmmc.jmal.model.UVMapData;
import fr.jmmc.jmcs.util.concurrent.InterruptedJobException;
import fr.jmmc.oitools.image.FitsImage;
import fr.jmmc.oitools.image.FitsImageFile;
import fr.nom.tam.fits.FitsException;
import java.awt.geom.Rectangle2D;
import java.awt.image.IndexColorModel;
import java.io.IOException;
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
  /** zeroThreshold lowest positive value (1e-5 for example) to ignore data values below AFTER normalization */
  public static final float ZERO_THRESHOLD = 1e-7f;
  /** maximum fft size (power of two) */
  public static final int MAX_FFT_SIZE = 256 * 1024;

  /**
   * Forbidden constructor
   */
  private UserModelService() {
    // no-op
  }

  /**
   * Load the FitsImageFile structure and prepare ONLY the first image for FFT processing
   * @param file fits file to load and process 
   * @return FitsImage
   * @throws FitsException if any FITS error occured
   * @throws IOException IO failure
   */
  public static FitsImage prepareFitsFile(final String file) throws FitsException, IOException {
    final FitsImageFile imgFitsFile = FitsImageUtils.load(file);

    if (imgFitsFile.getImageCount() == 0) {
      throw new FitsException("The Fits file '" + file + "' does not contain any Fits image !");
    }

    final FitsImage fitsImage = FitsImageUtils.prepareImage(imgFitsFile.getFirstFitsImage(), ZERO_THRESHOLD);

    logger.info("Prepared FitsImage: " + fitsImage.toString(false));

    return fitsImage;
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
   * @throws IllegalArgumentException if a model parameter value is invalid
   * @throws RuntimeException if any exception occured during the computation
   */
  public static UVMapData computeUVMap(final FitsImage fitsImage,
          final Rectangle2D.Double uvRect,
          final ImageMode mode,
          final int imageSize,
          final IndexColorModel colorModel,
          final ColorScale colorScale) {
    return computeUVMap(fitsImage, uvRect, mode, imageSize, colorModel, colorScale, null);
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
   * @return UVMapData
   * 
   * @throws InterruptedJobException if the current thread is interrupted (cancelled)
   * @throws IllegalArgumentException if a model parameter value is invalid
   * @throws RuntimeException if any exception occured during the computation
   */
  public static UVMapData computeUVMap(final FitsImage fitsImage,
          final Rectangle2D.Double uvRect,
          final ImageMode mode,
          final int imageSize,
          final IndexColorModel colorModel,
          final ColorScale colorScale,
          final float[][] refVisData) {

    if (fitsImage == null) {
      return null;
    }

    // Suppose the image is square (see FitsImageUtils.prepareImage:
    if (fitsImage.getNbCols() != fitsImage.getNbRows()) {
      throw new IllegalStateException("Fits image must be a square image !");
    }
    if (fitsImage.getIncCol() != fitsImage.getIncRow()) {
      throw new IllegalStateException("Fits image increments along row and column axes must be equals !");
    }
    // TODO: support different axis increments ?

    /** Get the current thread to check if the computation is interrupted */
    final Thread currentThread = Thread.currentThread();

    // Start the computations :
    final long start = System.nanoTime();

    // Get corrected uvMax from uv rectangle (-this.uvMax, -this.uvMax, this.uvMax, this.uvMax):
    final double uvMax = -uvRect.getX();
    logger.info("UserModelService.computeUVMap: uvMax (rad-1) = " + uvMax);

    final int inputSize = fitsImage.getNbRows();
    logger.info("Image size = " + inputSize);

    final double increment = fitsImage.getIncRow();
    logger.info("Current increment (rad) = " + increment);

    final double maxFreq = 1d / (2d * increment);
    logger.info("Max UV (rad-1) = " + maxFreq);

    if (maxFreq < uvMax) {
      throw new IllegalStateException("The Fits image [" + fitsImage.getFitsImageIdentifier()
              + "] must have smaller pixel increments (" + increment + " rad) to have a maximum frequency ("
              + maxFreq + " rad-1) larger than the corrected UV Max (" + uvMax + " rad-1) !");
    }

    // UV / maxFreq ratio
    final double ratio = uvMax / maxFreq;
    logger.info("ratio = " + ratio);

    // find best FFT size:
    final int fftSize = findBestFFTSize(ratio, imageSize);

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
    float[][] data = FFTUtils.convert(fftOutputSize, visData, mode, outputSize);

    final int dataSize = outputSize;

    // fast interrupt :
    if (currentThread.isInterrupted()) {
      throw new InterruptedJobException("UserModelService.computeUVMap: interrupted");
    }


    // 3 - Get the image with the given color model and color scale :

    final Rectangle2D.Double uvMapRect = new Rectangle2D.Double();
    uvMapRect.setFrameFromDiagonal(-mapUvMax, -mapUvMax, mapUvMax, mapUvMax);

    final UVMapData uvMapData = ModelUVMapService.computeImage(uvRect, null, null, mode, imageSize, colorModel, colorScale,
            dataSize, visData, data, uvMapRect);

    if (logger.isInfoEnabled()) {
      logger.info("compute : duration = {} ms.", 1e-6d * (System.nanoTime() - start));
    }
    return uvMapData;
  }

  /**
   * Return the best FFT size (power of two) i.e. giving the output size closest than the expected image size
   * @param imageSize expected image size in pixels
   * @param ratio UV / maxFreq ratio
   * @return best FFT size (power of two)
   */
  private static int findBestFFTSize(final double ratio, final int imageSize) {

    final int fftSizeMax = getFFTSize(ratio, imageSize);
    final int fftSizeMin = getFFTSize(ratio, imageSize / 2);

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
   * @return best FFT size (power of two)
   */
  private static int getFFTSize(final double ratio, final int imageSize) {
    // Correct power of two for FFT:
    int fftSize = FFTUtils.getPowerOfTwo((int) Math.ceil(imageSize / ratio));

    logger.info("For UV plane size (pixels) = " + imageSize);
    logger.info("FFT size (pixels) = " + fftSize);

    if (fftSize > MAX_FFT_SIZE) {
      fftSize = MAX_FFT_SIZE;
      logger.info("Max FFT size reached (pixels) = " + fftSize);
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
}
