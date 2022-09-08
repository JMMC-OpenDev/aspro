/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.interop;

import fr.jmmc.aspro.gui.TargetEditorDialog;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.jmcs.data.MimeType;
import fr.jmmc.jmcs.gui.component.FileChooser;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.network.http.Http;
import fr.jmmc.jmcs.network.interop.SampCapability;
import fr.jmmc.jmcs.network.interop.SampMessageHandler;
import fr.jmmc.jmcs.util.FileUtils;
import fr.jmmc.jmcs.util.StringUtils;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.astrogrid.samp.Message;
import org.astrogrid.samp.client.SampException;

/**
 * This class handles the SAMP Message image.load.fits:
 * @author bourgesl
 */
public final class ImageSampMessageHandler extends SampMessageHandler {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(ImageSampMessageHandler.class.getName());

    /**
     * Public constructor
     */
    public ImageSampMessageHandler() {
        super(SampCapability.LOAD_FITS_IMAGE);
    }

    /**
     * Implements message processing
     *
     * @param senderId public ID of sender client
     * @param message message with MType this handler is subscribed to
     * @throws SampException if any error occured while message processing
     */
    @Override
    protected void processMessage(final String senderId, final Message message) throws SampException {
        if (logger.isDebugEnabled()) {
            logger.debug("\tReceived '{}' message from '{}' : '{}'.", this.handledMType(), senderId, message);
        }

        // get url of file (locally stored):
        final String imgURL = (String) message.getRequiredParam("url");
        logger.info("processMessage: Image URL = {}", imgURL);

        if (imgURL == null) {
            throw new SampException("Can not get the url of the observation file");
        }

        URI imgURI;
        try {
            imgURI = new URI(imgURL);
        } catch (URISyntaxException use) {
            logger.error("invalid URI", use);

            throw new SampException("Can not read the image : " + imgURL, use);
        }

        // Guess file name:
        String fileName = FileUtils.getName(imgURL);

        if (fileName.isEmpty()) {
            fileName = StringUtils.replaceNonAlphaNumericCharsByUnderscore(imgURI.getPath());
        }

        // Download or get local file:
        File imgFile = null;
        try {
            final String scheme = imgURI.getScheme();

            if (scheme.equalsIgnoreCase("file")) {
                try {
                    imgFile = new File(imgURI);
                } catch (IllegalArgumentException iae) {
                    logger.debug("Invalid URI: {}", imgURI, iae);

                    String path = imgURI.getPath();
                    logger.debug("imgURI path: {}", path);

                    final int nbSlash = countStartingSlash(path);
                    logger.debug("nbSlash: {}", nbSlash);

                    // always use '///path'
                    // ds9 bug: URI file://localhost//tmp/...
                    path = "///" + path.substring(nbSlash);
                    logger.debug("fixed path: {}", path);

                    // Try fixing the file URI:
                    // aladin bug: URI has an authority component
                    try {
                        imgURI = new URI(scheme, path, null);
                    } catch (URISyntaxException use) {
                        logger.error("invalid URI", use);

                        throw new SampException("Can not read the image : " + imgURL, use);
                    }
                }
                try {
                    imgFile = new File(imgURI);
                } catch (IllegalArgumentException iae) {
                    logger.info("Invalid URI: {}", imgURI, iae);
                }

            } else {
                final File file = FileUtils.getTempFile("aspro-" + fileName, ".fits");

                if (Http.download(imgURI, file, false)) {
                    imgFile = file;
                }
            }

            if (imgFile == null) {
                throw new SampException("Not supported URI scheme : " + imgURL);
            }

        } catch (IOException ioe) {
            MessagePane.showErrorMessage("Can not read the image :\n\n" + imgURL);

            throw new SampException("Can not read the image : " + imgURL, ioe);
        }

        final String urlFileName = fileName;
        final File localFile = imgFile;

        // Use invokeLater to avoid concurrency and ensure that 
        // data model is modified and fire events using Swing EDT:
        SwingUtils.invokeEDT(new Runnable() {
            @Override
            public void run() {
                logger.info("localFile: {}", localFile);

                // Use main observation to get current target:
                final ObservationSetting observation = ObservationManager.getInstance().getMainObservation();

                // retrieve the selected target from its name:
                final Target target = observation.getSelectedTarget();

                if (target != null) {
                    logger.debug("target = {}", target);

                    if (MessagePane.showConfirmMessage("Do you want to use the incoming image as an user model for the target '"
                            + target.getName() + "' ?\n\n" + urlFileName)) {

                        // Ask the user to rename the file as the filename is generated (hash) in persistent folder:
                        final File imageFile = FileChooser.showSaveFileChooser("Choose (persistent) destination to write the Fits image file",
                                null, MimeType.FITS_IMAGE, urlFileName);

                        // Cancel
                        if (imageFile == null) {
                            return;
                        }

                        // copy the file at the user location and name:
                        try {
                            FileUtils.copy(localFile, imageFile);
                        } catch (IOException ioe) {
                            MessagePane.showErrorMessage("Can not write the image to : " + imageFile.getAbsolutePath(), ioe);
                            return;
                        }
                        // Open target editor:
                        final TargetEditorDialog targetEditor = TargetEditorDialog.getTargetEditor();

                        if (targetEditor == null) {
                            // TODO: blocking so give the model file too as an optional argument
                            TargetEditorDialog.showEditor(target.getName(), TargetEditorDialog.TAB_MODELS, imageFile);
                        } else {
                            // refresh dialog to use the given model file for the current selected target
                            targetEditor.defineUserModel(imageFile);
                        }
                    }
                }
            }
        });
    }

    private static int countStartingSlash(final String path) {
        int i = 0;
        while (path.charAt(i) == '/') {
            i++;
        }
        return i;
    }
}
