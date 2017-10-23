/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro;

import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.UserModel;
import fr.jmmc.aspro.service.OIFitsProcessService;
import fr.jmmc.aspro.service.UserModelData;
import fr.jmmc.aspro.service.UserModelService;
import fr.jmmc.aspro.util.StatUtils;
import fr.jmmc.oitools.image.FitsImage;
import fr.jmmc.oitools.image.FitsImageHDU;
import fr.jmmc.oitools.model.OIData;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIFitsLoader;
import fr.jmmc.oitools.model.OIFitsWriter;
import fr.nom.tam.fits.FitsException;
import java.io.IOException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author bourgesl
 */
public final class OIFitsProcessor {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(OIFitsProcessor.class.getName());

    /* members */
    /** image (oifits) */
    private final String imageFile;
    /** input (oifits) */
    private final String inputFile;
    /** output (oifits) */
    private final String outputFile;

    public OIFitsProcessor(final String imageFile, final String inputFile, final String outputFile) {
        this.imageFile = imageFile;
        this.inputFile = inputFile;
        this.outputFile = (outputFile != null) ? outputFile : inputFile + "-processed.fits";
    }

    public void process() {
        final WarningContainer warningContainer = new WarningContainer();
        try {
            // load OIFITS:
            final OIFitsFile oiFitsFile = OIFitsLoader.loadOIFits(inputFile);
            logger.info("OIFits: {}", oiFitsFile);

            // load and prepare images:
            final UserModel userModel = new UserModel();
            userModel.setFile(imageFile);

            // throws exceptions if the given fits file or image is incorrect:
            UserModelService.prepareUserModel(userModel);

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
            UserModelService.validateModel(userModel, getFreqMax(oiFitsFile));

            // anyway, update the valid flag:
            userModel.setFileValid(true);

            // Fake target
            final Target target = new Target();
            target.setName("PROCESS");
            target.setUserModel(userModel);
            target.setUseAnalyticalModel(Boolean.FALSE);

            // TODO: define CLI args for super-sampling & MathMode
            final OIFitsProcessService service = new OIFitsProcessService(
                    target, 9,
                    UserModelService.MathMode.FAST,
                    oiFitsFile);

            if (service.processOIFits(warningContainer)) {
                logger.info("Writing {}", outputFile);
                OIFitsWriter.writeOIFits(outputFile, oiFitsFile);
            }

        } catch (IllegalArgumentException iae) {
            logger.warn("Incorrect fits image in file [{}]", imageFile, iae);
        } catch (FitsException fe) {
            logger.info("FITS failure", fe);
        } catch (IOException ioe) {
            logger.info("IO failure", ioe);
        } finally {
            logger.info("Warnings messages:\n{}", warningContainer.getWarningMessages());
        }
    }

    private double getFreqMax(final OIFitsFile oiFitsFile) {
        double freqMax = Double.NEGATIVE_INFINITY;

        for (OIData oiData : oiFitsFile.getOiDataList()) {
            final double[][] spatialFreqs = oiData.getSpatialFreq();

            for (int i = 0; i < spatialFreqs.length; i++) {
                final double max = StatUtils.max(spatialFreqs[i]);
                if (max > freqMax) {
                    freqMax = max;
                }
            }
        }
        return freqMax;
    }
}
