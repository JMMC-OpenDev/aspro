/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.image;

import fr.jmmc.jmal.image.ImageArrayUtils;
import fr.jmmc.jmal.image.job.ImageFlipJob;
import fr.jmmc.jmal.image.job.ImageLowerThresholdJob;
import fr.jmmc.jmal.image.job.ImageMinMaxJob;
import fr.jmmc.jmal.image.job.ImageNormalizeJob;
import fr.jmmc.jmal.image.job.ImageXYProjectionJob;
import fr.jmmc.oitools.image.FitsImage;
import fr.jmmc.oitools.image.FitsImageFile;
import fr.jmmc.oitools.image.FitsImageLoader;
import fr.nom.tam.fits.FitsException;
import java.io.IOException;

/**
 * This utility class provides several helper methods over FitsImage class
 * 
 * TODO: enhance profile usage and add new dynamic histogram (log(value))
 * 
 * @author bourgesl
 */
public final class FitsImageUtils {
  /* constants */

  /** Logger associated to image classes */
  private final static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(FitsImageUtils.class.getName());
  /** flag to use flux profile threshold */
  private final static boolean USE_FLUX_PROFILE = false;

  /**
   * Forbidden constructor
   */
  private FitsImageUtils() {
    super();
  }

  /**
   * Create a new FitsImage given its data and updates dataMin/Max
   * @param data image data as float[nbRows][nbCols] ie [Y][X]
   * @return new FitsImage
   */
  public static FitsImage createFitsImage(final float[][] data) {
    final FitsImage image = new FitsImage();
    image.setData(data);

    // update dataMin/Max:
    updateDataRangeExcludingZero(image);

    return image;
  }

  /**
   * Create a new FitsImage given its data and updates dataMin/Max
   * @param data image data as float[nbRows][nbCols] ie [Y][X]
   * @param dataMin minimum value in data
   * @param dataMax maximum value in data
   * @return new FitsImage
   */
  public static FitsImage createFitsImage(final float[][] data,
          final double dataMin, final double dataMax) {
    final FitsImage image = new FitsImage();

    updateFitsImage(image, data, dataMin, dataMax);

    return image;
  }

  /**
   * Update data of the given FitsImage given its data and updates dataMin/Max
   * @param image FitsImage to update
   * @param data image data as float[nbRows][nbCols] ie [Y][X]
   * @param dataMin minimum value in data
   * @param dataMax maximum value in data
   */
  public static void updateFitsImage(final FitsImage image, final float[][] data,
          final double dataMin, final double dataMax) {
    image.setData(data);

    image.setDataMin(dataMin);
    image.setDataMax(dataMax);
  }

  /**
   * Create a new FitsImage given its data and coordinate informations
   * and updates dataMin/Max
   * @param data image data as float[nbRows][nbCols] ie [Y][X]
   * @param pixRefRow row index of the reference pixel (real starting from 1.0)
   * @param pixRefCol column position of the reference pixel (real starting from 1.0)
   * @param incRow signed coordinate increment along the row axis in radians
   * @param incCol signed coordinate increment along the column axis in radians
   * @return 
   */
  public static FitsImage createFitsImage(final float[][] data,
          final double pixRefRow, final double pixRefCol,
          final double incRow, final double incCol) {

    final FitsImage image = createFitsImage(data);

    image.setPixRefRow(pixRefRow);
    image.setPixRefCol(pixRefCol);

    image.setSignedIncRow(incRow);
    image.setSignedIncCol(incCol);

    return image;
  }

  /**
   * Create a new FitsImage given its data and coordinate informations
   * and updates dataMin/Max
   * @param data image data as float[nbRows][nbCols] ie [Y][X]
   * @param dataMin minimum value in data
   * @param dataMax maximum value in data
   * @param pixRefRow row index of the reference pixel (real starting from 1.0)
   * @param pixRefCol column position of the reference pixel (real starting from 1.0)
   * @param incRow signed coordinate increment along the row axis in radians
   * @param incCol signed coordinate increment along the column axis in radians
   * @return 
   */
  public static FitsImage createFitsImage(final float[][] data,
          final double dataMin, final double dataMax,
          final double pixRefRow, final double pixRefCol,
          final double incRow, final double incCol) {

    final FitsImage image = new FitsImage();

    updateFitsImage(image, data, dataMin, dataMax);

    image.setPixRefRow(pixRefRow);
    image.setPixRefCol(pixRefCol);

    image.setSignedIncRow(incRow);
    image.setSignedIncCol(incCol);

    return image;
  }

  /**
   * Copy the given FitsImage
   * @param input FitsImage to copy
   * @return FitsImage copy
   */
  public static FitsImage copyFitsImage(final FitsImage input) {

    final FitsImage image = createFitsImage(input.getData(), input.getDataMin(), input.getDataMax(),
            input.getPixRefRow(), input.getPixRefCol(), input.getSignedIncRow(), input.getSignedIncCol());

    image.setFitsImageIdentifier(input.getFitsImageIdentifier());
    // copy checksum of the original data:
    image.setChecksum(input.getChecksum());
    return image;
  }

  /**
   * Load the given file and return a FitsImageFile structure.
   * This methods updates dataMin/Max of each FitsImage
   *
   * @param absFilePath absolute File path on file system (not URL)
   * @throws FitsException if any FITS error occured
   * @throws IOException IO failure
   * @return FitsImageFile structure on success
   */
  public static FitsImageFile load(final String absFilePath) throws FitsException, IOException {
    final FitsImageFile imgFitsFile = FitsImageLoader.load(absFilePath);

    for (FitsImage fitsImage : imgFitsFile.getFitsImages()) {
      // update boundaries excluding zero values:
      updateDataRangeExcludingZero(fitsImage);
    }

    return imgFitsFile;
  }

  /** 
   * Update the data Min/Max of the given fitsImage
   * @param fitsImage fitsImage to process and update
   */
  public static void updateDataRange(final FitsImage fitsImage) {
    updateDataRange(fitsImage, false);
  }

  /** 
   * Update the data Min/Max of the given fitsImage excluding values equals to zero
   * @param fitsImage fitsImage to process and update
   */
  public static void updateDataRangeExcludingZero(final FitsImage fitsImage) {
    updateDataRange(fitsImage, true);
  }

  /** 
   * Update the data Min/Max of the given fitsImage
   * @param image fits image to process and update
   * @param excludeZero true to indicate to ignore zero values
   */
  private static void updateDataRange(final FitsImage image, final boolean excludeZero) {
    // update min/max ignoring zero:
    final ImageMinMaxJob minMaxJob = new ImageMinMaxJob(image.getData(),
            image.getNbCols(), image.getNbRows(), excludeZero);

    minMaxJob.forkAndJoin();

    logger.info("ImageMinMaxJob min: " + minMaxJob.getMin() + " - max: " + minMaxJob.getMax()
            + " - nData: " + minMaxJob.getNData() + " - sum: " + minMaxJob.getSum());

    // update nData:
    image.setNData(minMaxJob.getNData());

    // update dataMin/dataMax:
    image.setDataMin(minMaxJob.getMin());
    image.setDataMax(minMaxJob.getMax());
    // update sum:
    image.setSum(minMaxJob.getSum());
  }

  /**
   * Prepare the given image for FFT (normalize, threshold, pad to next power of two)
   * and return another FitsImage ready for FFT
   * @param image FitsImage to process
   * @param minVisibility minimum visiblity (1e-2) to define low threshold (ignore too small values)
   * @return another FitsImage
   */
  public static FitsImage prepareImage(final FitsImage image, final float minVisibility) {

    final FitsImage fitsImage = copyFitsImage(image);

    if (!fitsImage.isDataRangeDefined()) {
      // update boundaries excluding zero values:
      updateDataRangeExcludingZero(image);
    }

    // in place modifications:
    float[][] data = fitsImage.getData();
    int nbRows = fitsImage.getNbRows();
    int nbCols = fitsImage.getNbCols();

    logger.info("Image size:  " + nbRows + " x " + nbCols);


    // 1- Normalize data (amplitude):
    if (fitsImage.getDataMax() <= 0d) {
      throw new IllegalArgumentException("Input image has only negative data !");
    }

    double normFactor = 1d / fitsImage.getDataMax();

    ImageNormalizeJob normJob = new ImageNormalizeJob(data, nbCols, nbRows, normFactor);

    logger.info("ImageNormalizeJob - factor = " + normFactor);

    normJob.forkAndJoin();

    // update boundaries excluding zero values:
    updateDataRangeExcludingZero(fitsImage);


    // 2.1 - Ignore negative values and lower than given zeroThreshold i.e. replace them by 0.0:
    float tunedLowerThreshold = (minVisibility > 0f) ? 2f * minVisibility / fitsImage.getNData() : 0f;

    if (fitsImage.getDataMin() < tunedLowerThreshold) {

      final ImageLowerThresholdJob thresholdJob = new ImageLowerThresholdJob(data, nbCols, nbRows, tunedLowerThreshold, 0f);

      logger.info("ImageLowerThresholdJob - threshold = " + tunedLowerThreshold);

      thresholdJob.forkAndJoin();

      logger.info("ImageLowerThresholdJob - updateCount: " + thresholdJob.getUpdateCount());

      // update boundaries excluding zero values:
      updateDataRangeExcludingZero(fitsImage);
    }


    // 2.2 - Normalize data (total flux):
    normFactor = 1d / fitsImage.getSum();

    normJob = new ImageNormalizeJob(data, nbCols, nbRows, normFactor);

    logger.info("ImageNormalizeJob - factor = " + normFactor);

    normJob.forkAndJoin();

    // update boundaries excluding zero values:
    updateDataRangeExcludingZero(fitsImage);


    // 2.3 - Skip data lower than threshold again:
    tunedLowerThreshold = (minVisibility > 0f) ? 2f * minVisibility / fitsImage.getNData() : 0f;

    if (fitsImage.getDataMin() < tunedLowerThreshold) {

      final ImageLowerThresholdJob thresholdJob = new ImageLowerThresholdJob(data, nbCols, nbRows, tunedLowerThreshold, 0f);

      logger.info("ImageLowerThresholdJob - threshold = " + tunedLowerThreshold);

      thresholdJob.forkAndJoin();

      logger.info("ImageLowerThresholdJob - updateCount: " + thresholdJob.getUpdateCount());

      // update boundaries excluding zero values:
      updateDataRangeExcludingZero(fitsImage);
    }


    // 3 - Locate data inside image i.e. perform an XY Projection and determine data boundaries:
    final ImageXYProjectionJob projXYJob = new ImageXYProjectionJob(data, nbCols, nbRows);

    projXYJob.forkAndJoin();


    // 4 - Extract ROI:
    // keep the center of the ROI and keep the image square (width = height = even number):
    int rows1, rows2, cols1, cols2;

    final float halfRows = 0.5f * nbRows;
    final float halfCols = 0.5f * nbCols;

    final float distToCenter;

    if (USE_FLUX_PROFILE) {
      // use flux profile method:
      final double[] profile = makeDistanceProfile(nbRows, nbCols, projXYJob.getRowData(), projXYJob.getColumnData());

      if (false) {
        final StringBuilder sb = new StringBuilder(32 * 1024);

        for (int i = 0, len = profile.length; i < len; i++) {
          sb.append("localXYSeries.add(").append(i).append("D, ").append(profile[i]).append("D);\n");
        }
        logger.severe("Test profile histogram:\n" + sb.toString());
      }

      final double totalFlux = sum(profile);
      logger.info("flux total = " + totalFlux);

      final double upperFlux = totalFlux * (1d - minVisibility);
      logger.info("tuned flux threshold = " + upperFlux);

      final int distIndex = findBestDistance(profile, upperFlux);

      distToCenter = (distIndex != -1) ? distIndex + 0.5f : Math.max(halfRows, halfCols);
    } else {
      // use non zero area:
      rows1 = projXYJob.getRowLowerIndex();
      rows2 = projXYJob.getRowUpperIndex();
      cols1 = projXYJob.getColumnLowerIndex();
      cols2 = projXYJob.getColumnUpperIndex();

      logger.info("ImageXYProjectionJob: row indexes: " + rows1 + " - " + rows2);
      logger.info("ImageXYProjectionJob: col indexes: " + cols1 + " - " + cols2);

      final float rowDistToCenter = Math.max(Math.abs(halfRows - rows1), Math.abs(halfRows - rows2));
      final float colDistToCenter = Math.max(Math.abs(halfCols - cols1), Math.abs(halfCols - cols2));

      logger.info("ImageXYProjectionJob: rowDistToCenter = " + rowDistToCenter);
      logger.info("ImageXYProjectionJob: colDistToCenter = " + colDistToCenter);

      distToCenter = Math.max(rowDistToCenter, colDistToCenter);
    }

    // range check ?
    rows1 = (int) Math.floor(halfRows - distToCenter);
    rows2 = (int) Math.ceil(halfRows + distToCenter);

    // fix width to be an even number:
    if ((rows2 - rows1) % 2 == 1) {
      rows2++;
    }

    logger.info("ImageXYProjectionJob: fixed row indexes: " + rows1 + " - " + rows2);

    // range check ?
    cols1 = (int) Math.floor(halfCols - distToCenter);
    cols2 = (int) Math.ceil(halfCols + distToCenter);

    // fix width to be an even number:
    if ((cols2 - cols1) % 2 == 1) {
      cols2++;
    }

    logger.info("ImageXYProjectionJob: fixed col indexes: " + cols1 + " - " + cols2);

    // update fits image:
    // note: this extraction does not check boundary overlapping:
    data = ImageArrayUtils.extract(nbRows, nbCols, data, rows1, cols1, rows2, cols2);

    if (data == null) {
      // outside ranges:
      data = fitsImage.getData();
    } else {
      // update data/dataMin/dataMax:
      updateFitsImage(fitsImage, data, fitsImage.getDataMin(), fitsImage.getDataMax());

      // update ref pixel:
      fitsImage.setPixRefRow(fitsImage.getPixRefRow() - rows1);
      fitsImage.setPixRefCol(fitsImage.getPixRefCol() - cols1);

      nbRows = fitsImage.getNbRows();
      nbCols = fitsImage.getNbCols();

      logger.info("ROI size = " + nbRows + " x " + nbCols);
    }


    // 5 - Make sure the image is square i.e. padding (width = height = even number):
    final int newSize = Math.max(
            (nbRows % 2 == 1) ? nbRows + 1 : nbRows,
            (nbCols % 2 == 1) ? nbCols + 1 : nbCols);

    if (newSize != nbRows || newSize != nbCols) {
      data = ImageArrayUtils.enlarge(nbRows, nbCols, data, newSize, newSize);

      // update data/dataMin/dataMax:
      updateFitsImage(fitsImage, data, fitsImage.getDataMin(), fitsImage.getDataMax());

      // update ref pixel:
      fitsImage.setPixRefRow(fitsImage.getPixRefRow() + 0.5d * (newSize - nbRows));
      fitsImage.setPixRefCol(fitsImage.getPixRefCol() + 0.5d * (newSize - nbCols));

      nbRows = fitsImage.getNbRows();
      nbCols = fitsImage.getNbCols();

      logger.info("Fixed size = " + nbRows + " x " + nbCols);
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

    return fitsImage;
  }

  /**
   * Return the sum of all values in the given array
   * @param data values to sum
   * @return sum of all values
   */
  public static float sum(final float[] data) {
    float sum = 0f;

    for (int i = 0, len = data.length; i < len; i++) {
      sum += data[i];
    }
    return sum;
  }

  /**
   * Return the distance profile relative to the image center 
   * i.e. sum of data values according to the distance to the image center
   * @param nbRows number of rows
   * @param nbCols number of columns
   * @param rowData sum of data values on row axis (Y)
   * @param columnData sum of data values on column axis (X)
   * @return distance profile relative to the image center
   */
  private static double[] makeDistanceProfile(final int nbRows, final int nbCols, final double[] rowData, final double[] columnData) {
    final int length = Math.max(nbRows, nbCols) / 2;

    final double profile[] = new double[length];

    final int halfRows = nbRows / 2;
    final int halfCols = nbCols / 2;

    // process rows:
    for (int i = 0, distance; i < halfRows; i++) {
      distance = halfRows + i; // distance to row center
      profile[i] += rowData[distance];

      if (i != 0) {
        distance = halfRows - i; // distance to row center
        profile[i] += rowData[distance];
      }
    }

    // process columns:
    for (int i = 0, distance; i < halfCols; i++) {
      distance = halfCols + i; // distance to column center
      profile[i] += columnData[distance];

      if (i != 0) {
        distance = halfCols - i; // distance to column center
        profile[i] += columnData[distance];
      }
    }

    // divide by 2:
    for (int i = 0; i < length; i++) {
      profile[i] *= 0.5d;
    }
    return profile;
  }

  /**
   * Find the index (distance to image center) 
   * when the cumulative sum of profile values becomes higher than the given upper threshold
   * @param profile profile data
   * @param upperThreshold upper threshold
   * @return index (distance to image center) 
   */
  private static int findBestDistance(final double[] profile, final double upperThreshold) {
    double acc = 0d;
    for (int i = 0, len = profile.length; i < len; i++) {
      acc += profile[i];

      if (acc > upperThreshold) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Return the sum of all values
   * @param array values to sum
   * @return sum
   */
  private static double sum(final double[] array) {
    double acc = 0d;
    for (int i = 0, len = array.length; i < len; i++) {
      acc += array[i];
    }
    return acc;
  }
}
