/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fits;

import edu.emory.mathcs.jtransforms.fft.FloatFFT_2D;
import edu.emory.mathcs.jtransforms.fft.RealFFTUtils_2D;
import edu.emory.mathcs.utils.ConcurrencyUtils;
import fr.jmmc.aspro.gui.FitsImagePanel;
import fr.jmmc.aspro.gui.PreferencesView;
import fr.jmmc.jmal.complex.ImmutableComplex;
import fr.jmmc.jmal.image.FFTUtils;
import fr.jmmc.jmal.image.ImageArrayUtils;
import fr.jmmc.jmal.image.job.ImageLowerThresholdJob;
import fr.jmmc.jmal.image.job.ImageMinMaxJob;
import fr.jmmc.jmal.image.job.ImageNormalizeJob;
import fr.jmmc.jmal.image.job.ImageXYProjectionJob;
import fr.jmmc.jmcs.App;
import fr.jmmc.jmcs.gui.SwingUtils;
import fr.jmmc.oitools.image.FitsImage;
import fr.jmmc.oitools.image.FitsImageFile;
import fr.jmmc.oitools.image.FitsImageLoader;
import fr.jmmc.oitools.image.FitsImageWriter;
import fr.nom.tam.fits.BasicHDU;
import fr.nom.tam.fits.BinaryTable;
import fr.nom.tam.fits.BinaryTableHDU;
import fr.nom.tam.fits.Fits;
import fr.nom.tam.fits.FitsException;
import fr.nom.tam.fits.FitsFactory;
import fr.nom.tam.fits.Header;
import fr.nom.tam.fits.HeaderCard;
import fr.nom.tam.fits.ImageData;
import fr.nom.tam.fits.ImageHDU;
import fr.nom.tam.util.ArrayFuncs;
import java.awt.Dimension;
import java.io.File;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import javax.swing.JFrame;

/**
 * This class makes several image tests using nom.tam fits library
 * @author bourgesl
 */
public class ImageFitsTest {

  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(ImageFitsTest.class.getName());
  /** flag to disable infoFile() */
  private final static boolean INFO_ENABLE = false;
  /** flag to dump column content */
  private final static boolean PRINT_COL = false;
  /** flag to enable HIERARCH keyword support */
  private final static boolean USE_HIERARCH_FITS_KEYWORDS = true;

  /**
   * Forbidden constructor
   */
  private ImageFitsTest() {
    super();
  }

  public static void main(String[] args) {

    App.isReady();

    // Set the default locale to en-US locale (for Numerical Fields "." ",")
    Locale.setDefault(Locale.US);

    int errors = 0;

    // enable / disable HIERARCH keyword support :
    FitsFactory.setUseHierarch(USE_HIERARCH_FITS_KEYWORDS);

    // JTransforms: disable FFT_1D parallelization:
    ConcurrencyUtils.setThreadsBeginN_1D_FFT_2Threads(Integer.MAX_VALUE);
    ConcurrencyUtils.setThreadsBeginN_1D_FFT_4Threads(Integer.MAX_VALUE);

    if (true) {
      String file = "/home/bourgesl/ASPRO2/fits/SG_surface2.fits";
//      String file = "/home/bourgesl/ASPRO2/fits/SG_surface2_inv.fits";
//      String file = "/home/bourgesl/ASPRO2/fits/SG_surface2_vert.fits";
//      String file = "/home/bourgesl/ASPRO2/fits/diskUniform.fits";
//      String file = "/home/bourgesl/ASPRO2/fits/LITpro_disk.fits";

      FitsImageFile imgFitsFile = null;
      try {
        imgFitsFile = FitsImageLoader.load(file);

        logger.info("loaded FitsImageFile: " + imgFitsFile);

        final FitsImage fitsImage = imgFitsFile.getFitsImages().get(0);

        final double maxAngle = fitsImage.getMaxAngle();
        logger.info("Angle (deg) = " + Math.toDegrees(maxAngle));

        final double minInc = Math.min(fitsImage.getIncCol(), fitsImage.getIncRow());
        logger.info("Current increment (rad) = " + minInc);

        final double lambda = 2e-06d; // 2 mu
        final double maxUV = lambda / minInc;
        logger.info("Max UV (m) pour lambda = " + maxUV);

        // VLTI:  136.74 (available baselines)
        // CHARA: 347.20
        final double uvMax = 136.74d;
        logger.info("For uvMax (m) = " + uvMax);

        final double bestInc = lambda / uvMax;
        logger.info("Best increment (rad) = " + bestInc);

        final int newWidth = (int) Math.floor(fitsImage.getAngleCol() / bestInc);
        final int newHeight = (int) Math.floor(fitsImage.getAngleRow() / bestInc);

        logger.info("Image best size = " + newWidth + " x " + newHeight);

      } catch (Exception e) {
        logger.log(Level.SEVERE, "FitsImageUtils.load: failure occured while loading file : " + file, e);
      }

      if (imgFitsFile != null) {

        final FitsImage fitsImage = imgFitsFile.getFitsImages().get(0);

        // image padding;
        final float[][] paddedImage = FFTUtils.pad(fitsImage.getNbRows(), fitsImage.getNbCols(), fitsImage.getData());

        fitsImage.setData(paddedImage);

        // filters:
        ImageMinMaxJob minMaxJob = new ImageMinMaxJob(fitsImage.getData(), fitsImage.getNbCols(), fitsImage.getNbRows());

        logger.info("ImageMinMaxJob forkAndJoin");

        minMaxJob.forkAndJoin();

        logger.info("ImageMinMaxJob result: " + minMaxJob.getMin() + " - " + minMaxJob.getMax());

        // normalize:
        if (minMaxJob.getMax() != 0f) {
          final ImageNormalizeJob normJob = new ImageNormalizeJob(fitsImage.getData(), fitsImage.getNbCols(), fitsImage.getNbRows(),
                  1f / minMaxJob.getMax());

          logger.info("ImageNormalizeJob forkAndJoin - factor = " + (1f / minMaxJob.getMax()));

          normJob.forkAndJoin();

          // update boundaries except zero:
          minMaxJob = new ImageMinMaxJob(fitsImage.getData(), fitsImage.getNbCols(), fitsImage.getNbRows(), true);

          logger.info("ImageMinMaxJob forkAndJoin");

          minMaxJob.forkAndJoin();

          logger.info("ImageMinMaxJob result: " + minMaxJob.getMin() + " - " + minMaxJob.getMax());

          // update dataMin/dataMax except zero:
          fitsImage.setDataMin(minMaxJob.getMin());
          fitsImage.setDataMax(minMaxJob.getMax());
        }

        // precision arround zero after normalization:
        final float lowerThreshold = 1e-5f;

        if (minMaxJob.getMin() < lowerThreshold) {
          final float replaceBy = 0f; // minMaxJob.getMax();

          final ImageLowerThresholdJob fixNegativeJob = new ImageLowerThresholdJob(fitsImage.getData(), fitsImage.getNbCols(), fitsImage.getNbRows(),
                  lowerThreshold, replaceBy);

          logger.info("ImageLowerThresholdJob forkAndJoin - threshold = " + lowerThreshold);

          fixNegativeJob.forkAndJoin();

          logger.info("ImageLowerThresholdJob result: " + fixNegativeJob.getUpdateCount());

          // update boundaries except zero:
          minMaxJob = new ImageMinMaxJob(fitsImage.getData(), fitsImage.getNbCols(), fitsImage.getNbRows(), true);

          logger.info("ImageMinMaxJob forkAndJoin");

          minMaxJob.forkAndJoin();

          logger.info("ImageMinMaxJob result: " + minMaxJob.getMin() + " - " + minMaxJob.getMax());

          // update dataMin/dataMax except zero:
          fitsImage.setDataMin(minMaxJob.getMin());
          fitsImage.setDataMax(minMaxJob.getMax());
        }

        final ImageXYProjectionJob projXYJob = new ImageXYProjectionJob(fitsImage.getData(), fitsImage.getNbCols(), fitsImage.getNbRows());

        logger.info("ImageXYProjectionJob forkAndJoin");

        projXYJob.forkAndJoin();

        logger.info("ImageXYProjectionJob column indexes: " + projXYJob.getColumnLowerIndex() + " - " + projXYJob.getColumnUpperIndex());
        logger.info("ImageXYProjectionJob row    indexes: " + projXYJob.getRowLowerIndex() + " - " + projXYJob.getRowUpperIndex());

        // display fits image:
        new PreferencesView().setVisible(true);

        final List<FitsImage> images = imgFitsFile.getFitsImages();

        showFitsPanel(new File(imgFitsFile.getName()).getName(), images.get(0));

        if (false) {
          return;
        }

        file = "/home/bourgesl/ASPRO2/fits/SG_surface2_copy.fits";
        try {
          logger.info("writing: " + file);

          FitsImageWriter.write(file, imgFitsFile);

        } catch (Exception e) {
          logger.log(Level.SEVERE, "FitsImageUtils.load: failure occured while writing file : " + file, e);
        }
        try {
          imgFitsFile = FitsImageLoader.load(file);

          logger.info("loaded FitsImageFile: " + imgFitsFile);

        } catch (Exception e) {
          logger.log(Level.SEVERE, "FitsImageUtils.load: failure occured while loading file : " + file, e);
        }
        if (false) {
          file = "/home/bourgesl/ASPRO2/fits/SG_surface2_copy_2planes.fits";
          try {
            logger.info("writing: " + file);

            images.add(images.get(0));

            FitsImageWriter.write(file, imgFitsFile);

          } catch (Exception e) {
            logger.log(Level.SEVERE, "FitsImageUtils.load: failure occured while writing file : " + file, e);
          }
        }
      }

//      file = "/home/bourgesl/ASPRO2/fits/SG_surface2.fits";

//      final String file = "/home/bourgesl/ASPRO2/fits/images/rosat_pspc_rdf2_3_bk1.fits";
      errors += infoFile(file);
      errors += dumpFile(file);
      errors += showFile(file);
    }

    if (false) {
      final File directory = new File("/home/bourgesl/ASPRO2/fits/");
      if (directory.exists() && directory.isDirectory()) {

        final long start = System.nanoTime();

        final File[] files = directory.listFiles();

        for (File f : files) {
          if (f.isFile() && (f.getName().endsWith("fits") || f.getName().endsWith("fits.gz"))) {
            errors += infoFile(f.getAbsolutePath());
            errors += dumpFile(f.getAbsolutePath());
            errors += showFile(f.getAbsolutePath());
          }
        }

        logger.info("showDirectory : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");
      }
    }

    logger.info("Errors = " + errors);
  }

  public static int infoFile(final String absFilePath) {
    if (!INFO_ENABLE) {
      return 0;
    }
    int error = 0;

    try {
      logger.info("Reading file : " + absFilePath);

      final long start = System.nanoTime();

      final Fits fits = new Fits(absFilePath);

      BasicHDU hdu;

      int i = 0;
      do {
        hdu = fits.readHDU();
        if (hdu != null) {
          if (i == 0) {
            logger.info("\n\nPrimary header:\n");
          } else {
            logger.info("\n\nExtension " + i + ":\n");
          }
          i++;

          hdu.info();

        }
      } while (hdu != null);

      logger.info("infoFile : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");

    } catch (Throwable th) {
      logger.log(Level.SEVERE, "infoFile : IO failure occured while reading file : " + absFilePath, th);
      error = 1;
    }
    return error;
  }

  private static int dumpFile(final String absFilePath) {
    int error = 0;

    logger.info("Dump file : " + absFilePath);

    final StringBuilder sb = new StringBuilder(16384);

    final long start = System.nanoTime();
    try {

      final Fits fits = new Fits(absFilePath);

      final BasicHDU[] hdus = fits.read();

      final int len = hdus.length;
      sb.append("HDUs = ").append(len).append("\n");

      BasicHDU hdu;
      for (int i = 0; i < len; i++) {
        hdu = hdus[i];

        // dump HDU:
        dumpHDU(sb, hdu);
      }

    } catch (Throwable th) {
      logger.log(Level.SEVERE, "dumpFile : failure occured while dumping file : " + absFilePath, th);
      error = 1;
    } finally {
      final long end = System.nanoTime();

      logger.info(sb.toString());
      logger.info("buffer len = " + sb.length());
      logger.info("dumpFile : duration = " + 1e-6d * (end - start) + " ms.");
    }

    return error;
  }

  private static int showFile(final String absFilePath) {
    int error = 0;

    logger.info("Show file : " + absFilePath);

    final long start = System.nanoTime();
    try {

      final Fits fits = new Fits(absFilePath);

      final BasicHDU[] hdus = fits.read();

      final int len = hdus.length;

      BasicHDU hdu;
      for (int i = 0; i < len; i++) {
        hdu = hdus[i];

        // dump HDU:
        showHDU(hdu);
      }

    } catch (Throwable th) {
      logger.log(Level.SEVERE, "showFile : failure occured while dumping file : " + absFilePath, th);
      error = 1;
    } finally {
      final long end = System.nanoTime();

      logger.info("showFile : duration = " + 1e-6d * (end - start) + " ms.");
    }

    return error;
  }

  private static void dumpHDU(final StringBuilder sb, final BasicHDU hdu) throws FitsException {
    dumpHeader(sb, hdu.getHeader());

    // dump binary HDU only :
    if (hdu instanceof BinaryTableHDU) {
      dumpData(sb, (BinaryTableHDU) hdu);
    } else if (hdu instanceof ImageHDU) {
      dumpData(sb, (ImageHDU) hdu);
    } else {
      logger.warning("Unsupported HDU: " + hdu.getClass());
    }
  }

  private static void dumpHeader(final StringBuilder sb, final Header header) {

    final String extName = header.getTrimmedStringValue("EXTNAME");

    sb.append("--------------------------------------------------------------------------------\n");
    if (extName != null) {
      sb.append("EXTNAME = ").append(extName).append("\n");
    }

    final int nCards = header.getNumberOfCards();

    sb.append("KEYWORDS = ").append(nCards).append("\n");

    HeaderCard card;
    String key;
    for (Iterator<?> it = header.iterator(); it.hasNext();) {
      card = (HeaderCard) it.next();

      key = card.getKey();

      if ("END".equals(key)) {
        break;
      }

      sb.append("KEYWORD ").append(key).append(" = ");
      if (card.getValue() != null) {
        sb.append("'").append(card.getValue()).append("'");
      }
      sb.append("\t// ");
      if (card.getComment() != null) {
        sb.append(card.getComment());
      }
      sb.append("\n");
    }
  }

  private static void dumpData(final StringBuilder sb, final BinaryTableHDU hdu) throws FitsException {

    final BinaryTable data = (BinaryTable) hdu.getData();

    final int nCols = data.getNCols();

    sb.append("--------------------------------------------------------------------------------\n");
    sb.append("NCOLS = ").append(nCols).append("\n");

    final int nRows = data.getNRows();

    sb.append("NROWS = ").append(nRows).append("\n");

    String unit;
    Object array;
    for (int i = 0; i < nCols; i++) {
      // read all data and convert them to arrays[][] :
      array = data.getColumn(i);
      /*
      array = data.getFlattenedColumn(i);
       */

      sb.append("COLUMN ").append(hdu.getColumnName(i)).append(" [");
      sb.append(hdu.getColumnLength(i));
      sb.append(" ");
      sb.append(hdu.getColumnType(i));
      sb.append("] (");
      unit = hdu.getColumnUnit(i);
      if (unit != null) {
        sb.append(unit);
      }
      sb.append(")\t").append(ArrayFuncs.arrayDescription(array));

      if (PRINT_COL) {
        sb.append("\n").append(arrayToString(array));
      }
      sb.append("\n");
    }
  }

  private static void dumpData(final StringBuilder sb, final ImageHDU hdu) throws FitsException {

    final ImageData data = (ImageData) hdu.getData();

    sb.append("--------------------------------------------------------------------------------\n");

    // read all data and convert them to arrays[][] :
    final Object array = data.getData();

    sb.append("DATA\t").append(ArrayFuncs.arrayDescription(array));

    if (PRINT_COL) {
      sb.append("\n").append(arrayToString(array));
    }
    sb.append("\n");
  }

  private static void showHDU(final BasicHDU hdu) throws FitsException {

    // show image HDU only :
    if (hdu instanceof ImageHDU) {

      if (hdu.getBitPix() != BasicHDU.BITPIX_FLOAT) {
        logger.warning("Unsupported Image type: bitPix= " + hdu.getBitPix() + "; expected = " + BasicHDU.BITPIX_FLOAT);
//        return;
      }

      final int[] axes = hdu.getAxes();

      if (axes.length != 2) {
        logger.warning("Unsupported Image type: naxis= " + axes.length + "; expected = 2");
        return;
      }

      // axes are inverted in getAxes():
      final int width = axes[1];
      final int height = axes[0];

      final ImageData data = (ImageData) hdu.getData();

      // use raw array[1D] ??
      // convert automatically:
      // look at     Object o = ArrayFuncs.newInstance(base, dims);

      // read all data and convert them to arrays[][] :
      final float[][] array = getImageData(height, width, hdu.getBitPix(), data.getData());

      if (width != height) {
        throw new IllegalStateException("image must be square i.e. width == height !");
      }

      final int inputSize = width;

      final boolean testFullFFT = false;

      final int factor = 2;
      final int START = 1024 * 1;
      final int LIMIT = 1024 * 128; // 128
      final int OUTPUT = START / 32; // 32 pixels for 512 pixels

      for (int size = START, outputSize = OUTPUT; size <= LIMIT; size *= factor, outputSize *= factor) {

        // GC before computations:
        cleanup();

        if (testFullFFT) {
          testFFT(inputSize, array, size, outputSize, false);

          // GC before computations:
          cleanup();
        }

        testFFT(inputSize, array, size, outputSize, true);

        // GC before computations:
        cleanup();
      }

    } else {
      logger.warning("Unsupported HDU: " + hdu.getClass());
    }
  }

  /**
   * test FFT
   * @param inputSize input image size (width == height)
   * @param array input image
   * @param size FFT dimensions (width == height)
   * @param outputSize output size (width == height)
   */
  private static void testFFT(final int inputSize, final float[][] array, final int size, final int outputSize, final boolean fftSubset) {

    // TODO: normalize data:

    final boolean showPadded = false;
    final boolean showAmp = true;

    final boolean useLogScale = true;

    final boolean showPhi = false;
    final boolean doInverseFT = false;

    final boolean doCompare = false;

    final boolean doShift = true;

    final boolean doAmpFull = false;
    final boolean doAmp = false;
    final boolean doExtAmp = true;

    final int NFFTPass = 1;

    final int fftSize;
    float[][] fftArray = null;
    float[][] fftArrayCut = null;

    long start, time;
    long acc = 0l;


    logger.info("testFFT: image size = " + inputSize + " - FFT size = " + size
            + " [fftSubset = " + fftSubset + "] - output size = " + outputSize);

    // padded image but also FFT array (real):
    final int paddedSize = (fftSubset) ? Math.max(inputSize, outputSize) : size;

    if (showPadded) {
      float[][] paddedImage = ImageArrayUtils.enlarge(inputSize, inputSize, array, paddedSize, paddedSize);

      final FitsImage inImg = new FitsImage();

      // do copy as FFT is inplace:
      inImg.setData(ImageArrayUtils.copy(size, size, paddedImage));

      showFitsPanel("padded-" + size, inImg);

      paddedImage = null; // GC
    }

    // prepare FFT2D:
    fftSize = paddedSize;

    if (size >= 16384) {
      logger.severe("\n\nFFT complexForward: size = " + size + " ... please wait, slow computation ... \n\n");
    }

    // use size to have hyper resolution in fourier plane:
    FloatFFT_2D fft2d = new FloatFFT_2D(size, size); // rows, cols must be power of two !!

    for (int i = 0; i < NFFTPass; i++) {

      fftArray = null; // GC

      // GC before computations:
      cleanup();

      // DO copy:
      fftArray = ImageArrayUtils.enlarge(inputSize, inputSize, array, paddedSize, paddedSize);

      logger.info("FFT complexForward: start...");

      start = System.nanoTime();

      if (fftSubset) {
        // compute subset of real FFT (power of 2):
        fftArray = fft2d.realForwardSubset(fftSize, fftArray);

      } else {
        // compute real FFT (power of 2):
        fft2d.realForward(fftArray);
      }

      time = System.nanoTime() - start;
      acc += time;
      logger.info("FFT complexForward: duration = " + (1e-6d * time) + " ms.");
    }

    logger.info("FFT complexForward[" + fftSize + " - " + size + "]: average duration = "
            + (1e-6d * (acc) / NFFTPass) + " ms (" + NFFTPass + " iterations).");

    // free FFT2D (GC):
    fft2d = null;

    if (showAmp) {

      double dataMin = 0d;
      double dataMax = 0d;
      float[][] fftAmpCut;

      if (doAmp) {
        float[][] fftAmp;

        if (doAmpFull) {
          start = System.nanoTime();

          // extract amplitude (copy data):
          fftAmp = toAmplitude(fftSize, fftArray, doShift, useLogScale);

          logger.info("toAmplitude: duration = " + (1e-6d * (System.nanoTime() - start)) + " ms.");

          final FitsImage ampFull = new FitsImage();
          ampFull.setData(fftAmp);

          showFitsPanel("ampFull-" + size + "-" + fftSize, ampFull);

          dataMin = ampFull.getDataMin();
          dataMax = ampFull.getDataMax();
        }

        start = System.nanoTime();

        // extract amplitude (copy data):
        fftAmp = toAmplitude(fftSize, fftArray, true, useLogScale);

        logger.info("toAmplitude: duration = " + (1e-6d * (System.nanoTime() - start)) + " ms.");

        start = System.nanoTime();

        // extract amplitude (copy data):
        fftAmpCut = ImageArrayUtils.extract(fftSize, fftSize, fftAmp, outputSize, outputSize);

        logger.info("extractAmp: duration = " + (1e-6d * (System.nanoTime() - start)) + " ms.");

        final FitsImage ampImg = new FitsImage();
        ampImg.setData(fftAmpCut);

        showFitsPanel("ampCut-" + size + "-" + fftSize + "-" + outputSize, ampImg);

        if (doAmpFull) {
          ampImg.setDataMin(dataMin);
          ampImg.setDataMax(dataMax);
        } else {
          dataMin = ampImg.getDataMin();
          dataMax = ampImg.getDataMax();
        }
      }

      if (doExtAmp) {
        // TEST: cut FFT directly:
        start = System.nanoTime();

        fftArrayCut = extract(fftSize, fftArray, outputSize);

        logger.info("extractFFT: duration = " + (1e-6d * (System.nanoTime() - start)) + " ms.");

        start = System.nanoTime();

        // extract amplitude (copy data):
        float[][] fftAmp2 = toAmplitude(outputSize, fftArrayCut, doShift, useLogScale);

        logger.info("toAmplitude(2): duration = " + (1e-6d * (System.nanoTime() - start)) + " ms.");

        if (doCompare) {
          for (int r = 0; r < outputSize; r++) {
            for (int c = 0; c < outputSize; c++) {

              if (fftAmpCut[r][c] != fftAmp2[r][c]) {
                logger.info("values are different at row= " + r + ", col= " + c + " - left= " + fftAmpCut[r][c] + " - right= " + fftAmp2[r][c]);
              }
            }
          }
        }

        final FitsImage ampImg2 = new FitsImage();
        ampImg2.setData(fftAmp2);

        if (doAmp) {
          ampImg2.setDataMin(dataMin);
          ampImg2.setDataMax(dataMax);
        }

        showFitsPanel("ampSub-" + size + "-" + fftSize + "-" + outputSize, ampImg2);
      }
    }

    if (showPhi) {
      start = System.nanoTime();

      // extract phase (copy data):
      float[][] fftPhase = toPhase(fftSize, fftArray);

      logger.info("toPhase: duration = " + (1e-6d * (System.nanoTime() - start)) + " ms.");

      start = System.nanoTime();

      // extract phase (copy data):
      final float[][] fftPhiCut = ImageArrayUtils.extract(fftSize, fftSize, fftPhase, outputSize, outputSize);

      logger.info("extractPhi: duration = " + (1e-6d * (System.nanoTime() - start)) + " ms.");

      final FitsImage phiImg = new FitsImage();
      phiImg.setData(fftPhiCut);

      showFitsPanel("phase-" + size + "-" + fftSize, phiImg);

      start = System.nanoTime();

      // extract phase (copy data):
      float[][] fftPhi2 = toPhase(outputSize, fftArrayCut);

      logger.info("toPhase(2): duration = " + (1e-6d * (System.nanoTime() - start)) + " ms.");

      if (doCompare) {
        for (int r = 0; r < outputSize; r++) {
          for (int c = 0; c < outputSize; c++) {

            if (fftPhiCut[r][c] != fftPhi2[r][c]) {
              logger.info("values are different at row= " + r + ", col= " + c + " - left= " + fftPhiCut[r][c] + " - right= " + fftPhi2[r][c]);
            }
          }
        }
      }

      final FitsImage phiImg2 = new FitsImage();
      phiImg2.setData(fftPhi2);

      showFitsPanel("phiSub-" + size + "-" + fftSize + "-" + outputSize, phiImg2);
    }

    if (doInverseFT) {
      // prepare FFT2D:
      fft2d = new FloatFFT_2D(fftSize, fftSize); // rows, cols must be power of two !!

      start = System.nanoTime();

      // compute inverse real FFT with scaling:
      fft2d.realInverse(fftArray, true);

      logger.info("FFT complexInverse: duration = " + (1e-6d * (System.nanoTime() - start)) + " ms.");

      final FitsImage invImg = new FitsImage();
      invImg.setData(fftArray);

      showFitsPanel("inv-" + size + "-" + fftSize, invImg);
    }
  }

  private static float[][] toPhase(final int size, final float[][] fftData) {

    final RealFFTUtils_2D unpacker = new RealFFTUtils_2D(size, size); // rows, cols must be power of two !!

    final float[][] output = new float[size][size];

    float[] oRow;
    float re, im;
    for (int r = 0; r < size; r++) {
      oRow = output[r];
      for (int i = 0, c; i < size; i++) {
        c = 2 * i;

        re = unpacker.unpack(r, c, fftData);
        im = unpacker.unpack(r, c + 1, fftData);

        oRow[i] = (float) ImmutableComplex.getArgument(re, im);
      }
    }

    shiftQuadrants(size, output);

    return output;
  }

  private static float[][] toAmplitude(final int size, final float[][] fftData, final boolean doShift, final boolean useLogScale) {
    return toAmplitude(size, size, fftData, doShift, useLogScale);
  }

  private static float[][] toAmplitude(final int rows, final int cols, final float[][] fftData, final boolean doShift, final boolean useLogScale) {

    final RealFFTUtils_2D unpacker = new RealFFTUtils_2D(rows, cols); // rows, cols must be power of two !!

    final float[][] output = new float[rows][cols];

    // TODO: mix shift quadrants to process directly the center of the FT:
    float[] oRow;
    float re, im;
    for (int r = 0; r < rows; r++) {
      oRow = output[r];
      for (int i = 0, c; i < cols; i++) {
        c = 2 * i;

        re = unpacker.unpack(r, c, fftData);
        im = unpacker.unpack(r, c + 1, fftData);

        oRow[i] = (float) ImmutableComplex.abs(re, im);
      }
    }

    if (doShift) {
      shiftQuadrants(rows, output);
    }

    if (useLogScale) {
      for (int j = 0; j < rows; j++) {
        oRow = output[j];
        for (int i = 0; i < cols; i++) {
          oRow[i] = (oRow[i] > 0f) ? (float) Math.log10(oRow[i]) : 0f;
        }
      }
    }
    return output;
  }

  private static float[][] extract(final int size, final float[][] fftData, final int outputSize) {

    if (outputSize > size) {
      throw new IllegalStateException("invalid output size (" + outputSize + ") > fft size (" + size + ") !");
    }

    if (outputSize == size) {
      return fftData;
    }

    /*
     * <pre>
     * a[k1][2*k2] = Re[k1][k2] = Re[rows-k1][columns-k2], 
     * a[k1][2*k2+1] = Im[k1][k2] = -Im[rows-k1][columns-k2], 
     *       0&lt;k1&lt;rows, 0&lt;k2&lt;columns/2, 
     * 
     * a[0][2*k2] = Re[0][k2] = Re[0][columns-k2], 
     * a[0][2*k2+1] = Im[0][k2] = -Im[0][columns-k2], 
     *       0&lt;k2&lt;columns/2, 
     * a[k1][0] = Re[k1][0] = Re[rows-k1][0], 
     * a[k1][1] = Im[k1][0] = -Im[rows-k1][0], 
     * a[rows-k1][1] = Re[k1][columns/2] = Re[rows-k1][columns/2], 
     * a[rows-k1][0] = -Im[k1][columns/2] = Im[rows-k1][columns/2], 
     *       0&lt;k1&lt;rows/2, 
     * a[0][0] = Re[0][0], 
     * a[0][1] = Re[0][columns/2], 
     * a[rows/2][0] = Re[rows/2][0], 
     * a[rows/2][1] = Re[rows/2][columns/2]
     * </pre>
     */

    // TODO: see FloatFFT_2D.fillSymmetric(float[][])

    final int ro2 = outputSize / 2; // half of row dimension

    final int ef2 = size - ro2; // index of the first row in FFT (quadrant 3 / 4)

    final float[][] output = new float[outputSize][outputSize];

    float[] oRow, fRow;
    float[] o2Row, f2Row;

    for (int r = 0; r < ro2; r++) {
      oRow = output[r];
      o2Row = output[r + ro2];

      fRow = fftData[r];
      f2Row = fftData[ef2 + r];

      for (int i = 0, c; i < ro2; i++) {
        c = 2 * i;

        /*
         * a[k1][2*k2] = Re[k1][k2] = Re[rows-k1][columns-k2], 
         * a[k1][2*k2+1] = Im[k1][k2] = -Im[rows-k1][columns-k2], 
         *       0&lt;k1&lt;rows, 0&lt;k2&lt;columns/2, 
         */

        // TODO: use System.arrayCopy()

        // quadrant 1:
        oRow[c] = fRow[c];
        oRow[c + 1] = fRow[c + 1];

        // quadrant 4:
        o2Row[c] = f2Row[c];
        o2Row[c + 1] = f2Row[c + 1];
      }
    }

    // Probleme connu (symetrie horiz/vert) for row=rows/2 column=columns/2
/*
    12:56:24.879 INFO  [main] fits.ImageFitsTest - values are different at row= 0, col= 0 - left= 6.53674 - right= 6.1056275
    12:56:24.880 INFO  [main] fits.ImageFitsTest - values are different at row= 0, col= 1 - left= 6.4955583 - right= 6.60202
    12:56:24.881 INFO  [main] fits.ImageFitsTest - values are different at row= 0, col= 2 - left= 6.226657 - right= 6.675466
    12:56:24.883 INFO  [main] fits.ImageFitsTest - values are different at row= 0, col= 3 - left= 6.3349752 - right= 6.5520134
    12:56:24.883 INFO  [main] fits.ImageFitsTest - values are different at row= 0, col= 4 - left= 6.6794615 - right= 6.346796
    12:56:24.883 INFO  [main] fits.ImageFitsTest - values are different at row= 0, col= 5 - left= 6.78587 - right= 6.3059807
    12:56:24.883 INFO  [main] fits.ImageFitsTest - values are different at row= 0, col= 6 - left= 6.7859006 - right= 6.391479
    12:56:24.884 INFO  [main] fits.ImageFitsTest - values are different at row= 0, col= 7 - left= 6.7297072 - right= 6.4610043
    12:56:24.884 INFO  [main] fits.ImageFitsTest - values are different at row= 0, col= 8 - left= 6.608677 - right= 6.511784
    12:56:24.884 INFO  [main] fits.ImageFitsTest - values are different at row= 8, col= 0 - left= 7.0008745 - right= 6.983292
    12:56:24.888 INFO  [main] fits.ImageFitsTest - values are different at row= 9, col= 0 - left= 7.021206 - right= 6.8569174
    12:56:24.890 INFO  [main] fits.ImageFitsTest - values are different at row= 10, col= 0 - left= 6.979329 - right= 6.7267256
    12:56:24.891 INFO  [main] fits.ImageFitsTest - values are different at row= 11, col= 0 - left= 6.9652705 - right= 6.8951416
    12:56:24.892 INFO  [main] fits.ImageFitsTest - values are different at row= 12, col= 0 - left= 6.959093 - right= 6.9856753
    12:56:24.892 INFO  [main] fits.ImageFitsTest - values are different at row= 13, col= 0 - left= 6.857008 - right= 6.9495087
    12:56:24.893 INFO  [main] fits.ImageFitsTest - values are different at row= 14, col= 0 - left= 6.5514154 - right= 6.816455
    12:56:24.893 INFO  [main] fits.ImageFitsTest - values are different at row= 15, col= 0 - left= 5.9396 - right= 6.621644
     */

    // Solution: fixer a[rows][0/1] avec rows > 1
    /*
     * a[rows-k1][1] = Re[k1][columns/2] = Re[rows-k1][columns/2], 
     * a[rows-k1][0] = -Im[k1][columns/2] = Im[rows-k1][columns/2], 
     *       0&lt;k1&lt;rows/2, 
     */
    for (int r = 1, j; r < ro2; r++) {
      j = outputSize - r;

      output[j][1] = fftData[r][outputSize];
      output[j][0] = -fftData[r][outputSize + 1];
    }
    /*
     * a[0][0] = Re[0][0],        => OK
     * a[0][1] = Re[0][columns/2], 
     */
    output[0][1] = fftData[0][outputSize];

    /*
     * a[rows/2][0] = Re[rows/2][0], 
     * a[rows/2][1] = Re[rows/2][columns/2]
     */
    output[ro2][0] = fftData[ro2][0]; // quadrant 2
    output[ro2][1] = fftData[ro2][outputSize]; // quadrant 3

    return output;
  }

  /**
   * Shift quadrants in the given 2D array (in-place)
   * @param rows number of rows
   * @param data data array to process
   */
  private static void shiftQuadrants(final int rows, final float[][] data) {

    final long start = System.nanoTime();

    // shift quadrants:
    /*
     * | 1 2 | => | 3 4 |
     * | 4 3 |    | 2 1 |
     */

    final int ro2 = rows / 2; // half of row dimension

    float tmp;
    for (int r = 0, c; r < ro2; r++) {
      for (c = 0; c < ro2; c++) {
        // switch 1 <--> 3:
        tmp = data[r][c];
        data[r][c] = data[r + ro2][c + ro2];
        data[r + ro2][c + ro2] = tmp;

        // switch 2 <--> 4:
        tmp = data[r + ro2][c];
        data[r + ro2][c] = data[r][c + ro2];
        data[r][c + ro2] = tmp;
      }
    }

    logger.info("shiftQuadrants: duration = " + (1e-6d * (System.nanoTime() - start)) + " ms.");
  }

  private static float[] getBounds(final int rows, final int cols, final float[][] array) {
    // search min and max of input array

    final long start = System.nanoTime();

    float min = array[0][0];
    float max = array[0][0];

    float val;
    for (int j = 0; j < rows; j++) {
      for (int i = 0; i < cols; i++) {
        val = array[j][i];

        if (min > val) {
          min = val;
        }

        if (max < val) {
          max = val;
        }
      }
    }
    logger.info("getBounds: duration = " + (1e-6d * (System.nanoTime() - start)) + " ms.");
    logger.info("min = " + min + ", max = " + max);

    return new float[]{min, max};
  }

  private static float[][] getImageData(final int rows, final int cols, final int bitpix, final Object array) {
    if (bitpix == BasicHDU.BITPIX_FLOAT) {
      return (float[][]) array;
    }

    final float[][] output = new float[rows][cols];

    float[] oRow;
    switch (bitpix) {
      case BasicHDU.BITPIX_BYTE:
        final byte[][] bArray = (byte[][]) array;
        byte[] bRow;
        for (int j = 0; j < rows; j++) {
          oRow = output[j];
          bRow = bArray[j];
          for (int i = 0; i < cols; i++) {
            oRow[i] = (float) (bRow[i] & 0xFF);
          }
        }
        break;
      case BasicHDU.BITPIX_SHORT:
        final short[][] sArray = (short[][]) array;
        short[] sRow;
        for (int j = 0; j < rows; j++) {
          oRow = output[j];
          sRow = sArray[j];
          for (int i = 0; i < cols; i++) {
            oRow[i] = (float) sRow[i];
          }
        }
        break;
      case BasicHDU.BITPIX_INT:
        final int[][] iArray = (int[][]) array;
        int[] iRow;
        for (int j = 0; j < rows; j++) {
          oRow = output[j];
          iRow = iArray[j];
          for (int i = 0; i < cols; i++) {
            oRow[i] = (float) iRow[i];
          }
        }
        break;
      /*        
      case BasicHDU.BITPIX_LONG:
      final int[][] iArray = (int[][])array;
      int[] iRow;
      for (int j = 0; j < rows; j++) {
      oRow = output[j];
      iRow = iArray[j];
      for (int i = 0; i < cols; i++) {
      oRow[i] = (float)iRow[i];
      }
      }
      break;
       * 
       */

      default:
    }
    /*    
    public static final int BITPIX_LONG = 64;
    public static final int BITPIX_DOUBLE = -64;
     */
    return output;
  }

  public static String arrayToString(final Object o) {

    if (o == null) {
      return "null";
    }

    final Class<?> oClass = o.getClass();

    if (!oClass.isArray()) {
      return o.toString();
    }

    if (oClass == double[].class) {
      return Arrays.toString((double[]) o);
    } else if (oClass == float[].class) {
      return Arrays.toString((float[]) o);
    } else if (oClass == int[].class) {
      return Arrays.toString((int[]) o);
    } else if (oClass == long[].class) {
      return Arrays.toString((long[]) o);
    } else if (oClass == boolean[].class) {
      return Arrays.toString((boolean[]) o);
    } else if (oClass == short[].class) {
      return Arrays.toString((short[]) o);
    } else if (oClass == char[].class) {
      return Arrays.toString((char[]) o);
    } else if (oClass == byte[].class) {
      return Arrays.toString((byte[]) o);
    } else {
      /*
      if (oClass == String[].class) {
      StringBuilder sb = new StringBuilder(128).append("[");
      for (String val : (String[])o) {
      sb.append("'").append(val).append("'").append("=[");
      
      for (char ch : val.toCharArray()) {
      sb.append("0x").append(Integer.toHexString(ch)).append(' ');
      }
      
      sb.append("], ");
      }
      return sb.append("]").toString();
      }
       */
      // Non-primitive and multidimensional arrays can be
      // cast to Object[]
      final Object[] objArray = (Object[]) o;
      return Arrays.deepToString(objArray);
    }
  }

  /**
   * Show the given fits image
   * @param name name of the frame
   * @param fitsImage fits image structure
   */
  private static void showFitsPanel(final String name, final FitsImage fitsImage) {
    SwingUtils.invokeEDT(new Runnable() {

      @Override
      public void run() {
        final FitsImagePanel panel = new FitsImagePanel();
        panel.setFitsImage(fitsImage);

        final JFrame frame = new JFrame(name);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.add(panel);
        frame.setMinimumSize(new Dimension(800, 800));

        frame.pack();
        frame.setVisible(true);
      }
    });
  }

  /**
   * Cleanup (GC + 100ms pause)
   */
  private static void cleanup() {
    // Perform GC:
    System.gc();

    // pause for 100 ms :
    try {
      Thread.sleep(100l);
    } catch (InterruptedException ex) {
      logger.log(Level.INFO, "thread interrupted", ex);
    }
  }
}
