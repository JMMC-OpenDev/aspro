/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.interop;

import fr.jmmc.aspro.model.TargetImporter;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.network.http.Http;
import fr.jmmc.jmcs.network.interop.SampCapability;
import fr.jmmc.jmcs.network.interop.SampMessageHandler;
import fr.jmmc.jmcs.util.FileUtils;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.astrogrid.samp.Message;
import org.astrogrid.samp.client.SampException;

/**
 * This class handles the SAMP Message load.observation:
 * @author bourgesl
 */
public final class ObservationSampMessageHandler extends SampMessageHandler {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(ObservationSampMessageHandler.class.getName());

    /**
     * Public constructor
     */
    public ObservationSampMessageHandler() {
        super(SampCapability.ASPRO_LOAD_OBSERVATION);
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
        final String obsURL = (String) message.getRequiredParam("url");

        logger.debug("processMessage: Observation URL = {}", obsURL);

        if (obsURL == null) {
            throw new SampException("Can not get the url of the observation file");
        }

        URI obsURI;
        try {
            obsURI = new URI(obsURL);
        } catch (URISyntaxException use) {
            logger.error("invalid URI", use);

            throw new SampException("Can not read the observation : " + obsURL, use);
        }

        File obsFile = null;

        try {
            final String scheme = obsURI.getScheme();

            if (scheme.equalsIgnoreCase("file")) {
                try {
                    obsFile = new File(obsURI);
                } catch (IllegalArgumentException iae) {
                    logger.debug("Invalid URI: {}", obsURL, iae);

                    // Try fixing the file URI:
                    // aladin bug: URI has an authority component
                    try {
                        obsURI = new URI(scheme, obsURI.getPath(), null);
                    } catch (URISyntaxException use) {
                        logger.error("invalid URI", use);

                        throw new SampException("Can not read the votable : " + obsURL, use);
                    }
                }
                try {
                    obsFile = new File(obsURI);
                } catch (IllegalArgumentException iae) {
                    logger.info("Invalid URI: {}", obsURL, iae);
                }

            } else {
                final File file = FileUtils.getTempFile("aspro-", ".asprox");

                if (Http.download(obsURI, file, false)) {
                    obsFile = file;
                }
            }

            if (obsFile == null) {
                throw new SampException("Not supported URI scheme : " + obsURL);
            }

            // add targets only:
            TargetImporter.processObservation(obsFile, true);

        } catch (IOException ioe) {
            MessagePane.showErrorMessage("Can not read the votable :\n\n" + obsURL);

            throw new SampException("Can not read the votable : " + obsURL, ioe);
        }
    }
}
