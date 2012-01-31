/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fits;

import edu.emory.mathcs.jtransforms.fft.FloatFFT_2D;
import fr.jmmc.aspro.gui.FitsImagePanel;
import fr.jmmc.aspro.gui.PreferencesView;
import fr.jmmc.jmal.complex.MutableComplex;
import fr.jmmc.jmal.image.ImageViewer;
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
  private final static boolean INFO_ENABLE = true;
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

        final double maxAngle = imgFitsFile.getFitsImages().get(0).getMaxAngle();
        logger.info("loaded FitsImageFile view angle (deg) = " + maxAngle);

        final double lambda = 2e-06d; // 2 mu
        final double maxUV = lambda / (Math.toRadians(maxAngle));
        logger.info("loaded FitsImageFile max UV (m) pour lambda = = " + maxUV);

      } catch (Exception e) {
        logger.log(Level.SEVERE, "FitsImageUtils.load: failure occured while loading file : " + file, e);
      }

      if (imgFitsFile != null) {
        
        new PreferencesView().setVisible(true);

        final List<FitsImage> images = imgFitsFile.getFitsImages();
        
        showFitsPanel(images.get(0));
        
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

      final float[][] array = getImageData(height, width, hdu.getBitPix(), data.getData());

      // read all data and convert them to arrays[][] :

      showImage(width, height, array);

      // fix min/max bounds
      final float[] bounds = getBounds(height, width, array);

      if (bounds[0] < 0f) {
        // fix negative values (astro): replace by blanking value (0.0) to avoid any impact on FT:
      }

      // Test FFT2D:
      final FloatFFT_2D fft2d = new FloatFFT_2D(height, width);

      /*
       * Computes 2D forward DFT of complex data leaving the result in
       * <code>a</code>. The data is stored in 2D array. Complex data is
       * represented by 2 float values in sequence: the real and imaginary part,
       * i.e. the input array must be of size rows by 2*columns. The physical
       * layout of the input data has to be as follows:<br>
       * 
       * <pre>
       * a[k1][2*k2] = Re[k1][k2], 
       * a[k1][2*k2+1] = Im[k1][k2], 0&lt;=k1&lt;rows, 0&lt;=k2&lt;columns,
       * </pre>
       * 
       * @param a
       *            data to transform
       */
      long start = System.nanoTime();

      final float[][] fftArray = toComplexArray(height, width, array);

      logger.info("toComplexArray: duration = " + (1e-6d * (System.nanoTime() - start)) + " ms.");

      final boolean useComplex = false;

      start = System.nanoTime();

      // compute real FFT:
      if (useComplex) {
        fft2d.complexForward(fftArray);
      } else {
        // must be power of 2 !!!
        fft2d.realForward(fftArray);
      }

      logger.info("complexForward: duration = " + (1e-6d * (System.nanoTime() - start)) + " ms.");

      start = System.nanoTime();

      // extract amplitude:
      final float[][] fftAmp = toAmplitude(height, width, fftArray, true);

      logger.info("toAmplitude: duration = " + (1e-6d * (System.nanoTime() - start)) + " ms.");

      showImage(width, height, fftAmp);

      start = System.nanoTime();

      // compute real FFT:
      if (useComplex) {
        fft2d.complexInverse(fftArray, true);
      } else {
        fft2d.realInverse(fftArray, true);
      }

      logger.info("complexInverse: duration = " + (1e-6d * (System.nanoTime() - start)) + " ms.");

      start = System.nanoTime();

      // extract amplitude:
      final float[][] fftInvAmp = toAmplitude(height, width, fftArray, false);

      logger.info("toAmplitude: duration = " + (1e-6d * (System.nanoTime() - start)) + " ms.");

      showImage(width, height, fftInvAmp);

    } else {
      logger.warning("Unsupported HDU: " + hdu.getClass());
    }
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

  /**
   * Copy all values from input array to output array
   * @param rows number of rows
   * @param cols number of columns
   * @param input input array
   * @param output output array
   */
  public void multiArrayCopy(final int rows, final int cols, final float[][] input, final float[][] output) {
    for (int j = 0; j < rows; j++) {
      System.arraycopy(input[j], 0, output[j], 0, cols);
    }
  }

  /**
   * a[k1][2*k2] = Re[k1][k2], 
   * a[k1][2*k2+1] = Im[k1][k2], 0&lt;=k1&lt;rows, 0&lt;=k2&lt;columns,
   * 
   * @param rows
   * @param cols
   * @param input
   * @return 
   */
  private static float[][] toComplexArray(final int rows, final int cols, final float[][] input) {
    final float[][] output = new float[rows][2 * cols];

    float[] iRow;
    float[] oRow;

    for (int j = 0; j < rows; j++) {
      iRow = input[j];
      oRow = output[j];
      for (int i = 0, k; i < cols; i++) {
        k = 2 * i;
        oRow[k] = iRow[i];
        oRow[k + 1] = 0f;
      }
    }
    return output;
  }

  /**
   * a[k1][2*k2] = Re[k1][k2], 
   * a[k1][2*k2+1] = Im[k1][k2], 0&lt;=k1&lt;rows, 0&lt;=k2&lt;columns,
   * 
   */
  private static float[][] toAmplitude(final int rows, final int cols, final float[][] input, final boolean useLogScale) {

    final float[][] output = new float[rows][cols];

    final MutableComplex complex = new MutableComplex();

    float[] iRow;
    float[] oRow;
    float re, im;
    for (int j = 0; j < rows; j++) {
      iRow = input[j];
      oRow = output[j];
      for (int i = 0, k; i < cols; i++) {
        k = 2 * i;

        re = iRow[k];
        im = iRow[k + 1];

        complex.updateComplex(re, im);

        oRow[i] = (float) complex.abs();
      }
    }

    if (useLogScale) {
      // log scale:
      final float[] bounds = getBounds(rows, cols, output);
      final float min = bounds[0];
      final float max = bounds[1];

      float zero = 0f;
      if (max < 0f) {
        zero = 2f * max;
      }
      zero += 1f;

      for (int j = 0; j < rows; j++) {
        oRow = output[j];
        for (int i = 0; i < cols; i++) {
          oRow[i] = oRow[i] - min + zero;
        }
      }

      getBounds(rows, cols, output);

      for (int j = 0; j < rows; j++) {
        oRow = output[j];
        for (int i = 0; i < cols; i++) {
          oRow[i] = (float) Math.log10(oRow[i]);
        }
      }
    }
    return output;
  }

  private static float[] getBounds(final int rows, final int cols, final float[][] array) {
    // search min and max of input array

    float min = array[0][0];
    float max = array[0][0];

    int iMin = 0, jMin = 0;
    float val;
    for (int j = 0; j < rows; j++) {
      for (int i = 0; i < cols; i++) {
        val = array[j][i];

        if (min > val) {
          min = val;
          iMin = i;
          jMin = j;
        }

        if (max < val) {
          max = val;
        }
      }
    }
    logger.info("min = " + min + ", max = " + max);
    logger.info("iMin = " + iMin + ", jMin = " + jMin);

    return new float[]{min, max};
  }

  private static void showImage(final int width, final int height, final float[][] data) {
    SwingUtils.invokeAndWaitEDT(new Runnable() {

      public void run() {
        final ImageViewer viewer = new ImageViewer();

        viewer.getImageCanvas().initImage(width, height, data);

        viewer.pack();
        viewer.setVisible(true);
      }
    });
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

  private static void showFitsPanel(final FitsImage fitsImage) {
    SwingUtils.invokeEDT(new Runnable() {

      public void run() {
        final FitsImagePanel panel = new FitsImagePanel();
        panel.setFitsImage(fitsImage);

        final JFrame frame = new JFrame("FitsPanel");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.add(panel);
        frame.setMinimumSize(new Dimension(800, 800));

        frame.pack();
        frame.setVisible(true);
      }
    });

  }
}
