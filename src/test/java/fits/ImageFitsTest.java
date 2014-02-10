/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fits;

import edu.emory.mathcs.jtransforms.fft.FloatFFT_2D;
import edu.emory.mathcs.utils.ConcurrencyUtils;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.gui.FitsImagePanel;
import fr.jmmc.aspro.gui.PreferencesView;
import fr.jmmc.aspro.image.FitsImageUtils;
import fr.jmmc.aspro.model.oi.UserModel;
import fr.jmmc.aspro.service.UserModelService;
import fr.jmmc.jmal.complex.ImmutableComplex;
import fr.jmmc.jmal.image.ColorModels;
import fr.jmmc.jmal.image.ColorScale;
import fr.jmmc.jmal.image.FFTUtils;
import fr.jmmc.jmal.image.ImageArrayUtils;
import fr.jmmc.jmal.model.ImageMode;
import fr.jmmc.jmal.model.UVMapData;
import fr.jmmc.jmcs.Bootstrapper;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.oitools.image.FitsImage;
import fr.jmmc.oitools.image.FitsImageFile;
import fr.jmmc.oitools.image.FitsImageHDU;
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
import java.awt.geom.Rectangle2D;
import java.io.File;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Locale;
import javax.swing.JFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class makes several image tests using nom.tam fits library
 * @author bourgesl
 */
public class ImageFitsTest {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(ImageFitsTest.class.getName());
    /** flag to disable infoFile() */
    private final static boolean INFO_ENABLE = false;
    /** flag to dump column content */
    private final static boolean PRINT_COL = false;
    /** flag to enable HIERARCH keyword support */
    private final static boolean USE_HIERARCH_FITS_KEYWORDS = true;
    /** default amplitude range */
    private final static float[] AMP_RANGE = new float[]{0.95f, 0.99f};

    /**
     * Forbidden constructor
     */
    private ImageFitsTest() {
        super();
    }

    /**
     * Main tests
     * @param args unused arguments 
     */
    public static void main(String[] args) {

        Bootstrapper.getState();

        // Set the default locale to en-US locale (for Numerical Fields "." ",")
        Locale.setDefault(Locale.US);

        int errors = 0;

        // enable / disable HIERARCH keyword support :
        FitsFactory.setUseHierarch(USE_HIERARCH_FITS_KEYWORDS);

        final ColorScale colorScale = ColorScale.LINEAR;

        // display fits image:
        new PreferencesView().setVisible(true);

        // Fits image with unit / lambda keywords tests:
        if (false) {
//            String file = "/home/bourgesl/ASPRO2/fits/aspro_keywords.fits"; // BUNIT / CUNIT
            String file = "/home/bourgesl/ASPRO2/fits/tests/Microlensing.fits.gz"; // BUNIT / CUNIT

            try {
                testFastMode(file, Boolean.FALSE);
                testFastMode(file, Boolean.TRUE);

            } catch (Exception e) {
                logger.error("An exception occured while working on file: " + file, e);
            }
            return;
        }

        // Fits cube tests:
        if (false) {
            //String file = "/home/bourgesl/ASPRO2/fits/cube/chromatic_cube_aspro_jk.fits"; // 7 images but no WL keywords
            String file = "/home/bourgesl/ASPRO2/fits/cube/chromatic_cube_aspro_jk_with_Wlens.fits"; // 7 images with WL keywords      

            try {
                testFastMode(file, Boolean.FALSE);

            } catch (Exception e) {
                logger.error("An exception occured while working on file: " + file, e);
            }
            return;
        }

        // fast mode tests:

        if (false) {
            String file = "/home/bourgesl/ASPRO2/fits/58Eri_clumpy_K_1024.fits"; // diluted

            try {
                testFastMode(file, Boolean.FALSE);
                testFastMode(file, Boolean.TRUE);

            } catch (Exception e) {
                logger.error("An exception occured while working on file: " + file, e);
            }
            return;
        }

        if (false) {
//      String file = "/home/bourgesl/ASPRO2/fits/SG_surface2.fits";
//      String file = "/home/bourgesl/ASPRO2/fits/tests/HighMass.fits.gz"; // spiral       
//      String file = "/home/bourgesl/ASPRO2/fits/ellipsePlusPunct.fits";

//      String file = "/home/bourgesl/ASPRO2/fits/tests/3c273_all_pl27.fits.gz";
//      String file = "/home/bourgesl/ASPRO2/fits/tests/58Eri_clumpy_K_1024.fits.gz"; // diluted
            String file = "/home/bourgesl/ASPRO2/fits/58Eri_clumpy_K_1024.fits"; // diluted

//      String file = "/home/bourgesl/ASPRO2/fits/tests/AGN.fits.gz";      

//      String file = "/home/bourgesl/ASPRO2/fits/tests/ch4_i_60_big.fits.gz";
//      String file = "/home/bourgesl/ASPRO2/fits/tests/ch4_i_60_big.fits";      
//      String file = "/home/bourgesl/ASPRO2/fits/tests/ch4_i_00_big2.fits.gz";

//      String file = "/home/bourgesl/ASPRO2/fits/tests/EvolvedStar.fits.gz";      

//      String file = "/home/bourgesl/ASPRO2/fits/tests/flux_bf_intens_continuum_002_extended.fits.gz";      

//java.lang.ClassCastException: [[[[I cannot be cast to [[I      
//      String file = "/home/bourgesl/ASPRO2/fits/tests/isella_large_grain_inc000.fits.gz";  
//      String file = "/home/bourgesl/ASPRO2/fits/tests/isella_large_grain_inc060.fits.gz";  
//      String file = "/home/bourgesl/ASPRO2/fits/tests/isella_small_grain_inc060.fits.gz";  

//      String file = "/home/bourgesl/ASPRO2/fits/tests/Microlensing.fits.gz";  
//      String file = "/home/bourgesl/ASPRO2/fits/tests/Microquasar_grs1915a.fits.gz";  
//      String file = "/home/bourgesl/ASPRO2/fits/tests/NGC1365.i60.K.ima279.fits.gz";  
//      String file = "/home/bourgesl/ASPRO2/fits/tests/SG_surface2.fits.gz";  
//      String file = "/home/bourgesl/ASPRO2/fits/tests/SG_surface.fits.gz";  
//      String file = "/home/bourgesl/ASPRO2/fits/tests/YSO_disk.fits.gz";  

            try {
                // load and prepare images:
                final UserModel model = new UserModel();
                model.setFile(file);

                UserModelService.prepareUserModel(model);

                FitsImage fitsImage = model.getModelData(0).getFitsImage();

                logger.info("Prepared FitsImage: " + fitsImage.toString(false));

                showFitsPanel(fitsImage.getFitsImageIdentifier(), fitsImage);

                if (false) {
                    return;
                }
                // TODO: TEST

                /*
                 * VLTI AMBER
                 */
                if (false) {
                    final String insName = "AMBER";
                    final double uvMax = 136.74d / 2e-6d;

                    for (int imageSize = 256; imageSize <= 2048; imageSize *= 2) {
                        final Rectangle2D.Double uvRect = new Rectangle2D.Double();
                        uvRect.setFrameFromDiagonal(-uvMax, -uvMax, uvMax, uvMax);

                        final UVMapData uvMap = UserModelService.computeUVMap(fitsImage, uvRect, ImageMode.AMP, imageSize,
                                ColorModels.getDefaultColorModel(), colorScale);

                        final float[][] visData = uvMap.getData();
                        final float[][] data = FFTUtils.convert(visData.length, visData, ImageMode.AMP, visData.length - 2);

                        showImage("UVMapData-" + insName + "-" + imageSize, data);
                    }
                }

                /*
                 * VLTI MIDI
                 */
                if (false) {
                    final String insName = "MIDI";
                    final double uvMax = 136.74d / 8e-6d;

                    for (int imageSize = 256; imageSize <= 2048; imageSize *= 2) {
                        final Rectangle2D.Double uvRect = new Rectangle2D.Double();
                        uvRect.setFrameFromDiagonal(-uvMax, -uvMax, uvMax, uvMax);

                        final UVMapData uvMap = UserModelService.computeUVMap(fitsImage, uvRect, ImageMode.AMP, imageSize,
                                ColorModels.getDefaultColorModel(), colorScale);

                        final float[][] visData = uvMap.getData();
                        final float[][] data = FFTUtils.convert(visData.length, visData, ImageMode.AMP, visData.length - 2);

                        showImage("UVMapData-" + insName + "-" + imageSize, data);
                    }
                }

                /*
                 * CHARA VEGA
                 */
                if (false) {
                    final String insName = "VEGA";
                    final double uvMax = 300d / 0.65e-06d;

                    for (int imageSize = 256; imageSize <= 2048; imageSize *= 2) {
                        final Rectangle2D.Double uvRect = new Rectangle2D.Double();
                        uvRect.setFrameFromDiagonal(-uvMax, -uvMax, uvMax, uvMax);

                        final UVMapData uvMap = UserModelService.computeUVMap(fitsImage, uvRect, ImageMode.AMP, imageSize,
                                ColorModels.getDefaultColorModel(), colorScale);

                        final float[][] visData = uvMap.getData();
                        final float[][] data = FFTUtils.convert(visData.length, visData, ImageMode.AMP, visData.length - 2);

                        showImage("UVMapData-" + insName + "-" + imageSize, data);
                    }
                }


                if (false) {
                    return;
                }

                file = "/home/bourgesl/ASPRO2/fits/SG_surface2_copy.fits";

                logger.info("writing: " + file);

                FitsImageHDU imgHDU = new FitsImageHDU();
                imgHDU.getFitsImages().add(fitsImage);

                FitsImageFile imgFitsFile = new FitsImageFile();
                imgFitsFile.getFitsImageHDUs().add(imgHDU);

                FitsImageWriter.write(file, imgFitsFile);

//        file = "/home/bourgesl/ASPRO2/fits/SG_surface2.fits";

                imgFitsFile = FitsImageLoader.load(file, true);

                logger.info("loaded FitsImageFile: " + imgFitsFile);

                errors += infoFile(file);
                errors += dumpFile(file);
                errors += showFile(file);

            } catch (Exception e) {
                logger.error("An exception occured while working on file: " + file, e);
            }
        }

        if (true) {
            final File directory = new File("/home/bourgesl/ASPRO2/fits/tests/");
            if (directory.exists() && directory.isDirectory()) {

                // display fits image:
                new PreferencesView().setVisible(true);

                final long start = System.nanoTime();

                final File[] files = directory.listFiles();

                // load and prepare images:
                final UserModel model = new UserModel();
                String file;
                for (File f : files) {
                    if (f.isFile() && !f.getName().startsWith("COPY_") && (f.getName().endsWith("fits") || f.getName().endsWith("fits.gz"))) {

                        file = f.getAbsolutePath();
                        try {
                            // load and prepare images:
                            model.setFile(file);

                            UserModelService.prepareUserModel(model);

                            FitsImage fitsImage = model.getModelData(0).getFitsImage();

                            logger.info("Prepared FitsImage: " + fitsImage.toString(false));

                            showFitsPanel(fitsImage.getFitsImageIdentifier(), fitsImage);

                            if (true) {
                                continue;
                            }

                            file = directory.getAbsolutePath() + "/COPY_" + f.getName();
                            logger.info("writing: " + file);

                            FitsImageHDU imgHDU = new FitsImageHDU();
                            imgHDU.getFitsImages().add(fitsImage);

                            FitsImageFile imgFitsFile = new FitsImageFile();
                            imgFitsFile.getFitsImageHDUs().add(imgHDU);

                            FitsImageWriter.write(file, imgFitsFile);

                            errors += infoFile(file);
                            errors += dumpFile(file);
                            errors += showFile(file);

                        } catch (Exception e) {
                            logger.error("An exception occured while working on file: " + file, e);
                        }

                    }
                }

                logger.info("showDirectory: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
            }
        }

        logger.info("Errors = " + errors);
    }

    /**
     * Prepare the given file using fast mode or not and show FFT amplitude
     * @param file file to process
     * @param fastMode true to use fast mode 
     * @throws Exception if any exception occurs
     */
    private static void testFastMode(final String file, final Boolean fastMode) throws Exception {
        // force fast mode preference:
        Preferences.getInstance().setPreference(Preferences.MODEL_USER_FAST, fastMode);

        // load and prepare images:
        final UserModel model = new UserModel();
        model.setFile(file);

        UserModelService.prepareUserModel(model);

        FitsImage fitsImage = model.getModelData(0).getFitsImage();

        logger.info("Prepared FitsImage[" + fastMode + "]: " + fitsImage.toString(false));

        showFitsPanel(fitsImage.getFitsImageIdentifier(), fitsImage);

        /*
         * VLTI AMBER
         */
        final String insName = "AMBER";
        final double uvMax = 200d / 2e-6d;
        final int imageSize = 1024;

        final Rectangle2D.Double uvRect = new Rectangle2D.Double();
        uvRect.setFrameFromDiagonal(-uvMax, -uvMax, uvMax, uvMax);

        final UVMapData uvMap = UserModelService.computeUVMap(fitsImage, uvRect, ImageMode.AMP, imageSize,
                ColorModels.getDefaultColorModel(), ColorScale.LOGARITHMIC);

        final float[][] visData = uvMap.getData();
        final float[][] data = FFTUtils.convert(visData.length, visData, ImageMode.AMP, visData.length - 2);

        showImage("UVMapData-" + fastMode + "-" + insName + "-" + imageSize, data);
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

            logger.info("infoFile: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

        } catch (Throwable th) {
            logger.error("infoFile : IO failure occured while reading file : " + absFilePath, th);
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
            logger.error("dumpFile : failure occured while dumping file : " + absFilePath, th);
            error = 1;
        } finally {
            final long end = System.nanoTime();

            logger.info(sb.toString());
            logger.info("buffer len = " + sb.length());
            logger.info("dumpFile: duration = {} ms.", 1e-6d * (end - start));
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
            logger.error("showFile : failure occured while dumping file : " + absFilePath, th);
            error = 1;
        } finally {
            final long end = System.nanoTime();

            logger.info("showFile: duration = {} ms.", 1e-6d * (end - start));
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
            logger.warn("Unsupported HDU: " + hdu.getClass());
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
                logger.warn("Unsupported Image type: bitPix= " + hdu.getBitPix() + "; expected = " + BasicHDU.BITPIX_FLOAT);
//        return;
            }

            final int[] axes = hdu.getAxes();

            if (axes.length != 2) {
                logger.warn("Unsupported Image type: naxis= " + axes.length + "; expected = 2");
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

            if (false) {
                showImage("input-" + inputSize, array);
            }

            if (false) {
                final int paddedSize = 4096;


                // direct full FFT:
                final FloatFFT_2D fft2d = new FloatFFT_2D(paddedSize, paddedSize);

                final float[][] testData = new float[paddedSize][2 * paddedSize];

                final float[][] paddedImage = new float[paddedSize][paddedSize];

                final int i2 = inputSize / 2;
                final int offset = paddedSize - i2;

                // shift quadrants:
                for (int r = 0; r < i2; r++) {
                    for (int c = 0; c < i2; c++) {
                        // quadrant 1:
                        paddedImage[r][c] = array[r + i2][c + i2];
                        // quadrant 2:
                        paddedImage[r][c + offset] = array[r + i2][c];
                        // quadrant 3:
                        paddedImage[offset + r][c + offset] = array[r][c];
                        // quadrant 4:
                        paddedImage[offset + r][c] = array[r][c + i2];
                    }
                }

                if (true) {
                    showImage("padded-" + paddedSize, ImageArrayUtils.copy(paddedSize, paddedSize, paddedImage));
                }

                for (int r = 0; r < paddedSize; r++) {
                    for (int c = 0; c < paddedSize; c++) {
                        testData[r][2 * c] = paddedImage[r][c];
                    }
                }

                // compute subset of real FFT (power of 2):
                fft2d.complexForward(testData);

                final float[][] ampData = new float[paddedSize][paddedSize];
                final float[][] phiData = new float[paddedSize][paddedSize];

                for (int r = 0; r < paddedSize; r++) {
                    for (int c = 0; c < paddedSize; c++) {
                        ampData[r][c] = (float) ImmutableComplex.abs(testData[r][2 * c], testData[r][2 * c + 1]);
                        phiData[r][c] = (float) ImmutableComplex.getArgument(testData[r][2 * c], testData[r][2 * c + 1]);
                    }
                }

                FFTUtils.shiftQuadrants(paddedSize, ampData);
                showImage("ampData-" + inputSize, ampData);

                FFTUtils.shiftQuadrants(paddedSize, phiData);
                showImage("phiData-" + inputSize, phiData);

                return;
            }


            if (false) {
                // test equivalence realForward / realForwardSubset:
                testFFT(inputSize, array, 1024, 1024, false);
                testFFT(inputSize, array, 1024, 1024, true);

                // test equivalence realForward / realForwardSubset:
                testFFT(inputSize, array, 4096, 512, false);
                testFFT(inputSize, array, 4096, 512, true);

                return;
            }

            /*
             * VLTI AMBER
             For FFT size (pixels) = 32768.0
             11:18:34.925 INFO  [main] fits.ImageFitsTest - UV plane size (pixels) = 537.68353
             */
            if (false) {
                testFFT(inputSize, array, 8192, 134, true);
                testFFT(inputSize, array, 32768, 538, true);
            }

            /*
             * VLTI MIDI
             14:42:31.437 INFO  [main] fits.ImageFitsTest - For FFT size (pixels) = 131072.0
             14:42:31.437 INFO  [main] fits.ImageFitsTest - UV plane size (pixels) = 537.68353
             */
            if (false) {
                testFFT(inputSize, array, 32768, 134, true);
                testFFT(inputSize, array, 65536, 268, true);
                testFFT(inputSize, array, 131072, 538, true);
            }

            /*
             * CHARA VEGA
             15:03:21.094 INFO  [main] fits.ImageFitsTest - For FFT size (pixels) = 8192.0
             15:03:21.094 INFO  [main] fits.ImageFitsTest - UV plane size (pixels) = 907.4215
             */
            if (false) {
                testFFT(inputSize, array, 2048, 228, true);
                testFFT(inputSize, array, 4096, 454, true);
                testFFT(inputSize, array, 8192, 908, true);
            }


            final int nThreads = ConcurrencyUtils.getNumberOfThreads();


            final boolean testFullFFT = false;
            final boolean testConcurrency = false;

            final int INIT = 32;

            final int factor = 2;
            final int START = 1024 * INIT;
            final int LIMIT = 1024 * 32; // 128
            final int OUTPUT = 128 * INIT; // 16.75 pixels for 1024 pixels (AMBER)

            for (int size = START, outputSize = OUTPUT; size <= LIMIT; size *= factor, outputSize *= factor) {

                // GC before computations:
                cleanup();

                if (testFullFFT) {
                    testFFT(inputSize, array, size, outputSize, false);

                    // GC before computations:
                    cleanup();
                }

                if (testConcurrency) {
                    ConcurrencyUtils.setNumberOfThreads(1);

                    testFFT(inputSize, array, size, outputSize, true);

                    // GC before computations:
                    cleanup();

                    if (nThreads > 2) {

                        ConcurrencyUtils.setNumberOfThreads(2);

                        testFFT(inputSize, array, size, outputSize, true);

                        // GC before computations:
                        cleanup();
                    }

                    ConcurrencyUtils.setNumberOfThreads(nThreads);
                }

                testFFT(inputSize, array, size, outputSize, true);

                // GC before computations:
                cleanup();
            }

        } else {
            logger.warn("Unsupported HDU: " + hdu.getClass());
        }
    }

    /**
     * test FFT
     * @param inputSize input image size (width == height)
     * @param array input image
     * @param fftSize FFT dimensions (width == height)
     * @param outputSize output size (width == height)
     * @param fftSubset use FFT_2D.realForwardSubset() faster operation
     */
    private static void testFFT(final int inputSize, final float[][] array, final int fftSize, final int outputSize, final boolean fftSubset) {

        final boolean showPadded = false;
        final boolean showAmp = true;

        final boolean doAmpFull = false;
        final boolean doAmp = false;
        final boolean doExtAmp = true;
        final boolean doCompare = false;

        final boolean showPhi = true;
        final boolean doPhiFull = false;
        final boolean doInverseFT = false;


        final int NFFTPass = 1;

        final int fftSubSize;
        float[][] fftArray = null;
        float[][] fftArrayCut = null;

        long start, time;
        long acc = 0l;


        logger.info("testFFT: image size = " + inputSize + " - FFT size = " + fftSize
                + " [fftSubset = " + fftSubset + "] - output size = " + outputSize);

        // padded image but also FFT array (real):
        final int paddedSize = (fftSubset) ? Math.max(inputSize, outputSize) : fftSize;

        if (showPadded) {
            // pad input image:
            final float[][] paddedImage = ImageArrayUtils.enlarge(inputSize, inputSize, array, paddedSize, paddedSize);

            final FitsImage paddedImg = FitsImageUtils.createFitsImage(paddedImage,
                    1d + paddedSize / 2d, 1d + paddedSize / 2d,
                    1d, 1d); // TODO

            showFitsPanel("padded-" + fftSize, paddedImg, null);
        }

        // prepare FFT2D:
        fftSubSize = paddedSize;

        if (fftSize >= 16384) {
            logger.error("\n\nFFT complexForward: size = " + fftSize + " ... please wait, slow computation ... \n\n");
        }

        // use size to have hyper resolution in fourier plane:
        FloatFFT_2D fft2d = new FloatFFT_2D(fftSize, fftSize, true); // rows, cols must be power of two !!

        for (int i = 0; i < NFFTPass; i++) {

            fftArray = null; // GC

            // GC before computations:
            cleanup();

            if (fftSubset) {
                fftArray = array;
            } else {
                // DO copy:
                fftArray = ImageArrayUtils.enlarge(inputSize, inputSize, array, paddedSize, paddedSize);
            }

            logger.info("FFT complexForward: start...");

            start = System.nanoTime();

            if (fftSubset) {
                // compute subset of real FFT (power of 2):
                fftArray = fft2d.realForwardSubset(fftSubSize, inputSize, fftArray);
            } else {
                // compute real FFT (power of 2):
                fft2d.realForward(fftArray);
            }

            time = System.nanoTime() - start;
            acc += time;

            logger.info("FFT complexForward: duration = {} ms.", 1e-6d * (time));
        }

        logger.info("FFT complexForward[" + fftSubSize + " - " + fftSize + "]: average duration = "
                + (1e-6d * acc / NFFTPass) + " ms (" + NFFTPass + " iterations).");

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
                    fftAmp = FFTUtils.convert(fftSubSize, fftArray, ImageMode.AMP);

                    logger.info("convert: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

                    final FitsImage ampFull = FitsImageUtils.createFitsImage(fftAmp);
                    showFitsPanel("ampFull-" + fftSize + "-" + fftSubSize, ampFull);

                    dataMin = ampFull.getDataMin();
                    dataMax = ampFull.getDataMax();
                }

                start = System.nanoTime();

                // extract amplitude (copy data):
                fftAmp = FFTUtils.convert(fftSubSize, fftArray, ImageMode.AMP);

                logger.info("convert: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

                start = System.nanoTime();

                // extract amplitude (copy data):
                fftAmpCut = ImageArrayUtils.extract(fftSubSize, fftSubSize, fftAmp, outputSize, outputSize);

                logger.info("extractAmp: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

                final FitsImage ampImg = FitsImageUtils.createFitsImage(fftAmpCut);
                showFitsPanel("ampCut-" + fftSize + "-" + fftSubSize + "-" + outputSize, ampImg);

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

                if (fftSubSize > outputSize) {
                    start = System.nanoTime();

                    fftArrayCut = FFTUtils.extractFFT(fftSubSize, fftArray, outputSize);

                    logger.info("extractFFT: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
                } else {
                    fftArrayCut = fftArray;
                }

                start = System.nanoTime();

                // extract amplitude (copy data):
                float[][] fftAmp2 = FFTUtils.convert(outputSize, fftArrayCut, ImageMode.AMP);

                logger.info("convert(2): duration = {} ms.", 1e-6d * (System.nanoTime() - start));

                if (doCompare) {
                    for (int r = 0; r < outputSize; r++) {
                        for (int c = 0; c < outputSize; c++) {

                            if (fftAmpCut[r][c] != fftAmp2[r][c]) {
                                logger.info("values are different at row= " + r + ", col= " + c + " - left= " + fftAmpCut[r][c] + " - right= " + fftAmp2[r][c]);
                            }
                        }
                    }
                }

                final FitsImage ampImg2 = FitsImageUtils.createFitsImage(fftAmp2);

                if (doAmp) {
                    ampImg2.setDataMin(dataMin);
                    ampImg2.setDataMax(dataMax);
                }

                showFitsPanel("ampSub-" + fftSize + "-" + fftSubSize + "-" + outputSize, ampImg2);
            }
        }

        if (showPhi) {
            float[][] fftPhiCut;

            if (doPhiFull) {
                start = System.nanoTime();

                // extract phase (copy data):
                float[][] fftPhase = FFTUtils.convert(fftSubSize, fftArray, ImageMode.PHASE);

                logger.info("convert: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

                showImage("phaseFULL-" + fftSize + "-" + fftSubSize, fftPhase);

                start = System.nanoTime();

                // extract phase (copy data):
                fftPhiCut = ImageArrayUtils.extract(fftSubSize, fftSubSize, fftPhase, outputSize, outputSize);

                logger.info("extractPhi: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

                showImage("phase-" + fftSize + "-" + fftSubSize, fftPhiCut);
            }

            start = System.nanoTime();

            // extract phase (copy data):
            float[][] fftPhi2 = FFTUtils.convert(outputSize, fftArrayCut, ImageMode.PHASE);

            logger.info("convert(2): duration = {} ms.", 1e-6d * (System.nanoTime() - start));

            if (doCompare) {
                for (int r = 0; r < outputSize; r++) {
                    for (int c = 0; c < outputSize; c++) {

                        if (fftPhiCut[r][c] != fftPhi2[r][c]) {
                            logger.info("values are different at row= " + r + ", col= " + c + " - left= " + fftPhiCut[r][c] + " - right= " + fftPhi2[r][c]);
                        }
                    }
                }
            }

            showImage("phiSub-" + fftSize + "-" + fftSubSize + "-" + outputSize, fftPhi2);
        }

        if (doInverseFT && ConcurrencyUtils.isPowerOf2(fftSubSize)) {

            // prepare FFT2D:
            fft2d = new FloatFFT_2D(fftSubSize, fftSubSize); // rows, cols must be power of two !!

            start = System.nanoTime();

            // compute inverse real FFT with scaling:
            fft2d.realInverse(fftArray, true);

            logger.info("FFT complexInverse: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

            showImage("inv-" + fftSubSize, fftArray);
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
     * Show the given image using the Fits Image panel
     * @param name name of the frame
     * @param data float[rows][columns] to use
     */
    public static void showImage(final String name, final float[][] data) {
        showFitsPanel(name, FitsImageUtils.createFitsImage(data), null);
    }

    /**
     * Show the given image using the Fits Image panel
     * @param name name of the frame
     * @param data float[rows][columns] to use
     * @param minDataRange optional minimal range for data
     */
    public static void showImage(final String name, final float[][] data, final float[] minDataRange) {
        showFitsPanel(name, FitsImageUtils.createFitsImage(data), minDataRange);
    }

    /**
     * Show the given fits image
     * @param name name of the frame
     * @param fitsImage fits image structure
     */
    private static void showFitsPanel(final String name, final FitsImage fitsImage) {
        showFitsPanel(name, fitsImage, null);
    }

    /**
     * Show the given fits image
     * @param name name of the frame
     * @param fitsImage fits image structure
     * @param minDataRange optional minimal range for data
     */
    private static void showFitsPanel(final String name, final FitsImage fitsImage, final float[] minDataRange) {
        SwingUtils.invokeEDT(new Runnable() {
            @Override
            public void run() {
                final FitsImagePanel panel = new FitsImagePanel(true, false, minDataRange);
                panel.setFitsImage(fitsImage);

                final JFrame frame = new JFrame(name);
                frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

                frame.setMinimumSize(new Dimension(800, 800));

                frame.add(panel);
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
            logger.info("thread interrupted", ex);
        }
    }
}
