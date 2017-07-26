/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro;

import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.UserModel;
import fr.jmmc.aspro.service.OIFitsProcessService;
import fr.jmmc.aspro.service.UserModelService;
import fr.jmmc.oitools.image.FitsImage;
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
        init();
    }

    private void init() {
        final WarningContainer warningContainer = new WarningContainer();
        try {
            // load and prepare images:
            final UserModel userModel = new UserModel();
            userModel.setFile(imageFile);

            UserModelService.prepareUserModel(userModel);

            FitsImage fitsImage = userModel.getModelData(0).getFitsImage();

            logger.info("Prepared FitsImage: {}", fitsImage.toString());

            // load OIFITS:
            final OIFitsFile oiFitsFile = OIFitsLoader.loadOIFits(inputFile);

            logger.info("OIFits: {}", oiFitsFile);

            // Analyze to get max(UV)
            // Validate user model:
//            UserModelService.validateModel(model, getUVMax(observation));
            // anyway, update the valid flag:
            userModel.setFileValid(true);

            // Fake target
            final Target target = new Target();
            target.setName("PROCESS");
            target.setUserModel(userModel);
            target.setUseAnalyticalModel(Boolean.FALSE);

            final OIFitsProcessService service = new OIFitsProcessService(
                    target, 9,
                    UserModelService.MathMode.FAST,
                    oiFitsFile);

            if (service.processOIFits(warningContainer)) {
                logger.info("Writing {}", outputFile);
                OIFitsWriter.writeOIFits(outputFile, oiFitsFile);
            }

        } catch (FitsException fe) {
            logger.info("init: failure", fe);
        } catch (IOException ioe) {
            logger.info("init: failure", ioe);
        } finally {
            logger.info("Warnings messages:\n{}", warningContainer.getWarningMessages());
        }
    }

}
