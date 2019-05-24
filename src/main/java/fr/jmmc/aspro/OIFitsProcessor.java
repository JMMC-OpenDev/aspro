/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro;

import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.WarningMessage;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.UserModel;
import fr.jmmc.aspro.service.OIFitsProcessService;
import fr.jmmc.aspro.service.UserModelData;
import fr.jmmc.aspro.service.UserModelService;
import fr.jmmc.aspro.service.UserModelService.MathMode;
import fr.jmmc.aspro.util.StatUtils;
import fr.jmmc.jmal.ALX;
import fr.jmmc.jmcs.App;
import fr.jmmc.oitools.image.FitsImage;
import fr.jmmc.oitools.image.FitsImageHDU;
import fr.jmmc.oitools.model.OIData;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIFitsLoader;
import fr.jmmc.oitools.model.OIFitsWriter;
import fr.nom.tam.fits.FitsException;
import java.io.IOException;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author bourgesl
 */
public final class OIFitsProcessor {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(OIFitsProcessor.class.getName());
    /** CLI arg - image (fits) */
    public final static String ARG_IMAGE = "image";
    /** CLI arg - input (oifits) */
    public final static String ARG_INPUT = "input";
    /** CLI arg - output (oifits) */
    public final static String ARG_OUTPUT = "output";
    /** CLI arg - fast */
    public final static String ARG_FAST = "fast";
    /** CLI arg - fast error */
    public final static String ARG_FAST_ERROR = "fastError";
    /** CLI arg - supersampling */
    public final static String ARG_SUPER_SAMPLING = "supersampling";
    /** CLI arg - math */
    public final static String ARG_MATH = "math";
    /** CLI arg - scale (mas) */
    public final static String ARG_SCALE = "scale";
    /** CLI arg - rotation (deg) */
    public final static String ARG_ROTATE = "rotate";
    /** CLI arg - apodize */
    public final static String ARG_APODIZE = "apodize";
    /** CLI arg - apodization diameter */
    public final static String ARG_DIAMETER = "diameter";

    public static void defineCommandLineArguments(final App app) {
        app.addCustomCommandLineArgument(ARG_IMAGE, true, "the input FITS model to process", App.ExecMode.TTY);
        app.addCustomCommandLineArgument(ARG_INPUT, true, "the input OIFITS file to process", App.ExecMode.TTY);
        app.addCustomCommandLineArgument(ARG_OUTPUT, true, "the output OIFITS file to write", App.ExecMode.TTY);
        // optional arguments:
        app.addCustomCommandLineArgument(ARG_FAST, true, "[true] to ignore useless data; false to have highest precision", App.ExecMode.TTY);
        app.addCustomCommandLineArgument(ARG_FAST_ERROR, true, "optional Fast mode error in percents [0 - 10 %]; 1% by default", App.ExecMode.TTY);
        app.addCustomCommandLineArgument(ARG_SUPER_SAMPLING, true, "supersampling per spectral channel ["
                + AsproConstants.DEFAULT_SUPER_SAMPLING + "]", App.ExecMode.TTY);
        app.addCustomCommandLineArgument(ARG_MATH, true, "Math mode: ['FAST'] faster, 'DEFAULT' highest accuracy or 'QUICK' fastest BUT low accuracy", App.ExecMode.TTY);
        // transform arguments:
        app.addCustomCommandLineArgument(ARG_SCALE, true, "optional image scale (increment) expressed in milli-arcsec (mas)", App.ExecMode.TTY);
        app.addCustomCommandLineArgument(ARG_ROTATE, true, "optional image rotation expressed in degrees", App.ExecMode.TTY);
        // apodization arguments:
        app.addCustomCommandLineArgument(ARG_APODIZE, true, "[true] to perform image apodization; false to disable", App.ExecMode.TTY);
        app.addCustomCommandLineArgument(ARG_DIAMETER, true, "optional telescope diameter (meters) used by image apodization", App.ExecMode.TTY);
    }

    public static void processCommandLine(final App app, final Map<String, String> argValues) {
        final String inputFile = argValues.get(ARG_INPUT);

        if (inputFile == null) {
            logger.warn("Missing {} argument !", ARG_INPUT);
            app.showArgumentsHelp();
        }

        final String modelFile = argValues.get(ARG_IMAGE);
        if (modelFile == null) {
            logger.warn("Missing {} argument !", ARG_IMAGE);
            app.showArgumentsHelp();
        }

        if (inputFile != null && modelFile != null) {
            // Optional arguments:
            final String outputFile = argValues.get(ARG_OUTPUT);

            String optArg = argValues.get(ARG_FAST);
            final boolean useFastMode = (optArg != null) ? Boolean.parseBoolean(optArg) : true;

            optArg = argValues.get(ARG_FAST_ERROR);
            final double fastError = (optArg != null) ? Double.parseDouble(optArg) : AsproConstants.DEFAULT_FAST_ERROR;

            optArg = argValues.get(ARG_SUPER_SAMPLING);
            final int supersampling = (optArg != null) ? Integer.parseInt(optArg)
                    : AsproConstants.DEFAULT_SUPER_SAMPLING.intValue();

            optArg = argValues.get(ARG_MATH);
            final MathMode mathMode = (optArg != null) ? MathMode.valueOf(optArg) : MathMode.FAST;

            // transform arguments:
            optArg = argValues.get(ARG_SCALE);
            final double scale = (optArg != null) ? Double.parseDouble(optArg) : Double.NaN;

            optArg = argValues.get(ARG_ROTATE);
            final double rotate = (optArg != null) ? Double.parseDouble(optArg) : Double.NaN;

            optArg = argValues.get(ARG_APODIZE);
            final boolean doApodization = (optArg != null) ? Boolean.parseBoolean(optArg) : true;

            optArg = argValues.get(ARG_DIAMETER);
            final double diameter = (optArg != null) ? Double.parseDouble(optArg) : Double.NaN;

            // Process (in sync):
            new OIFitsProcessor(inputFile, modelFile, outputFile,
                    useFastMode, fastError, supersampling, mathMode,
                    doApodization, diameter,
                    scale, rotate
            ).process();
        }
    }

    /* members */
    /** input file (OIFITS) */
    private final String inputFile;
    /** model file (FITS) */
    private final String modelFile;
    /** output file (OIFITS) */
    private final String outputFile;
    /** fast mode: true to ignore useless data (faster); false to have highest precision */
    private final boolean useFastMode;
    /** fast mode error in percents */
    private final double fastError;
    /** OIFits supersampling */
    private final int supersampling;
    /** OIFits MathMode */
    private final MathMode mathMode;
    /** apodization flag: true to perform image apodization; false to disable */
    private final boolean doApodization;
    /** optional telescope diameter (meters) used by image apodization (m) */
    private final double diameter;
    /** scale / increment (mas) */
    private final double scale;
    /** rotation angle (deg) */
    private final double rotate;

    /**
     * Constructor
     * @param inputFile input file (OIFITS)
     * @param modelFile model file (FITS)
     * @param outputFile output file (OIFITS)
     * @param useFastMode true to ignore useless data (faster); false to have highest precision
     * @param fastError fast mode threshold in percents
     * @param supersampling OIFits supersampling preference
     * @param mathMode OIFits MathMode preference
     * @param doApodization true to perform image apodization; false to disable
     * @param diameter optional telescope diameter (meters) used by image apodization (m)
     * @param scale scale / increment (mas)
     * @param rotate rotation angle (deg)
     */
    private OIFitsProcessor(final String inputFile, final String modelFile, final String outputFile,
                            final boolean useFastMode, final double fastError,
                            final int supersampling, final UserModelService.MathMode mathMode,
                            final boolean doApodization, final double diameter,
                            final double scale, final double rotate) {

        this.inputFile = inputFile;
        this.modelFile = modelFile;
        this.outputFile = (outputFile != null) ? outputFile : (inputFile + "-processed.fits");
        this.useFastMode = useFastMode;
        this.fastError = 0.01 * fastError; // as percents
        this.supersampling = supersampling;
        this.mathMode = mathMode;
        this.doApodization = doApodization;
        this.diameter = diameter;
        this.scale = scale;
        this.rotate = rotate;

        logSeparator();
        logger.info("OIFitsProcessor arguments:");
        logger.info("inputFile:     {}", inputFile);
        logger.info("modelFile:     {}", modelFile);
        logger.info("outputFile:    {}", outputFile);
        logger.info("useFastMode:   {}", useFastMode);
        logger.info("fastError:     {}", fastError);
        logger.info("supersampling: {}", supersampling);
        logger.info("mathMode:      {}", mathMode);
        logger.info("doApodization: {}", doApodization);
        logger.info("diameter:      {}", diameter);
        logger.info("scale:         {}", scale);
        logger.info("rotate:        {}", rotate);
    }

    public void process() {
        final WarningContainer warningContainer = new WarningContainer();
        try {
            // load OIFITS:
            final OIFitsFile oiFitsFile = OIFitsLoader.loadOIFits(inputFile);
            logger.info("OIFits: {}", oiFitsFile);

            // load and prepare images:
            final UserModel userModel = new UserModel();
            userModel.setFile(modelFile);

            // define optional Transforms:
            if (!Double.isNaN(scale)) {
                final double inc = ALX.convertMasToRad(scale);
                userModel.setScaleX(inc);
                userModel.setScaleY(inc);
            }
            if (!Double.isNaN(rotate)) {
                userModel.setRotation(rotate);
            }

            // throws exceptions if the given fits file or image is incorrect:
            // note: apodization is defered to use appropriate wavelength
            UserModelService.prepareUserModel(userModel, useFastMode, fastError);

            final UserModelData modelData = userModel.getModelData(0);
            final FitsImage fitsImage = modelData.getFitsImage();
            logger.info("Prepared FitsImage: {}", fitsImage.toString());

            // update checksum before validation:
            if (userModel.isModelDataReady()) {
                // note: only possible with one Fits image or one Fits cube (single HDU):
                final FitsImageHDU fitsImageHDU = modelData.getFitsImageHDU();

                userModel.setChecksum(fitsImageHDU.getChecksum());
            }
            // Validate user model with the max frequency from the OIFITS file:
            UserModelService.validateModel(userModel, getMaxFreq(oiFitsFile));

            // anyway, update the valid flag:
            userModel.setFileValid(true);

            // Fake target
            final Target target = new Target();
            target.setName("PROCESS");
            target.setUserModel(userModel);
            target.setUseAnalyticalModel(Boolean.FALSE);

            final OIFitsProcessService service = new OIFitsProcessService(target, oiFitsFile,
                    useFastMode, fastError, supersampling, mathMode, doApodization, diameter);

            if (service.processOIFits(warningContainer)) {
                logger.info("Writing {}", outputFile);
                OIFitsWriter.writeOIFits(outputFile, oiFitsFile);
            }

        } catch (IllegalArgumentException iae) {
            logger.warn("Incorrect fits image in file [{}]", modelFile, iae);
        } catch (FitsException fe) {
            logger.info("FITS failure", fe);
        } catch (IOException ioe) {
            logger.info("IO failure", ioe);
        } finally {
            if (warningContainer.hasWarning()) {
                logSeparator();
                logger.info("OIFitsProcessService messages:");
                for (WarningMessage message : warningContainer.getWarnings()) {
                    logger.info("{}: {}", message.getLevel(), message.getMessage());
                }
            }
            logSeparator();
        }
    }

    private double getMaxFreq(final OIFitsFile oiFitsFile) {
        double freqMax = Double.NEGATIVE_INFINITY;

        for (OIData oiData : oiFitsFile.getOiDataList()) {
            final double[][] spatialFreqs = oiData.getSpatialFreq();

            // special case: OIFlux does not compute spatial frequencies:
            if (spatialFreqs != null) {
                for (int i = 0; i < spatialFreqs.length; i++) {
                    final double max = StatUtils.max(spatialFreqs[i]);
                    if (max > freqMax) {
                        freqMax = max;
                    }
                }
            }
        }
        return freqMax;
    }

    private static void logSeparator() {
        logger.info("------------------------------------------------------------------------------");
    }
}
