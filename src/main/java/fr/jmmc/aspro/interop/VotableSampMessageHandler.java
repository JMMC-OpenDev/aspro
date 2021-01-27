/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.interop;

import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.network.http.Http;
import fr.jmmc.jmcs.network.interop.SampCapability;
import fr.jmmc.jmcs.network.interop.SampManager;
import fr.jmmc.jmcs.network.interop.SampMessageHandler;
import fr.jmmc.jmcs.network.interop.SampMetaData;
import fr.jmmc.jmcs.util.FileUtils;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.astrogrid.samp.Message;
import org.astrogrid.samp.Metadata;
import org.astrogrid.samp.client.SampException;

/**
 * This class handles the SAMP Message load votable:
 * - coming from SearchCal to extract calibrators
 * - coming from PIVOT to create one new observation with given targets
 * @author bourgesl
 */
public final class VotableSampMessageHandler extends SampMessageHandler {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(VotableSampMessageHandler.class.getName());

    /**
     * Public constructor
     */
    public VotableSampMessageHandler() {
        super(SampCapability.LOAD_VO_TABLE);
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

        // get url of votable (locally stored) :
        final String voTableURL = (String) message.getRequiredParam("url");

        logger.debug("processMessage: VOTable URL = {}", voTableURL);

        if (voTableURL == null) {
            throw new SampException("Can not get the url of the votable");
        }

        URI voTableURI;
        try {
            voTableURI = new URI(voTableURL);
        } catch (URISyntaxException use) {
            logger.error("invalid URI", use);

            throw new SampException("Can not read the votable : " + voTableURL, use);
        }

        File voTableFile = null;

        try {
            final String scheme = voTableURI.getScheme();

            if (scheme.equalsIgnoreCase("file")) {
                try {
                    voTableFile = new File(voTableURI);
                } catch (IllegalArgumentException iae) {
                    logger.debug("Invalid URI: {}", voTableURL, iae);

                    // Try fixing the file URI:
                    // aladin bug: URI has an authority component
                    try {
                        voTableURI = new URI(scheme, voTableURI.getPath(), null);
                    } catch (URISyntaxException use) {
                        logger.error("invalid URI", use);

                        throw new SampException("Can not read the votable : " + voTableURL, use);
                    }
                }
                try {
                    voTableFile = new File(voTableURI);
                } catch (IllegalArgumentException iae) {
                    logger.info("Invalid URI: {}", voTableURL, iae);
                }

            } else {
                final File file = FileUtils.getTempFile("votable-", ".vot");

                if (Http.download(voTableURI, file, false)) {
                    voTableFile = file;
                }
            }

            if (voTableFile == null) {
                throw new SampException("Not supported URI scheme : " + voTableURL);
            }

            final String votable = FileUtils.readFile(voTableFile);

            logger.debug("votable :\n{}", votable);

            String searchCalVersion = null;

            // Note: Getting SearchCal version in sender meta data is not robust:
            // do not work with AppLauncher as senderMetadata corresponds to AppLauncher.metadata and not SearchCal !
            // TODO: use VOTABLE information instead !
            // Note: can be null if the client map is not up to date or the client disconnected:
            final Metadata senderMetadata = SampManager.getMetaData(senderId);

            if (senderMetadata != null) {
                logger.debug("senderMetadata: {}", senderMetadata);

                final String senderName = senderMetadata.getName();

                if ("searchcal".equalsIgnoreCase(senderName) || "getstar".equalsIgnoreCase(senderName)) {
                    // SearchCal release > 4.4.1:
                    searchCalVersion = senderMetadata.getString(SampMetaData.RELEASE_VERSION.id());

                    if (searchCalVersion == null) {
                        // SearchCal release <= 4.4.1:
                        searchCalVersion = senderMetadata.getString("searchcal.version");
                    }
                }
            }

            logger.debug("SearchCal version = {}", searchCalVersion);

            if (searchCalVersion == null) {
                // try to interpret votable as SearchCal one then as one generic votable (target):

                // TODO: use 'undefined' as SearchCal version temporarly:
                if (!SearchCalVOTableHandler.processMessage(votable, "undefined")) {
                    AnyVOTableHandler.processVOTable(votable, false);
                }
            } else {
                SearchCalVOTableHandler.processMessage(votable, searchCalVersion);
            }

        } catch (IOException ioe) {
            MessagePane.showErrorMessage("Can not read the votable :\n\n" + voTableURL);

            throw new SampException("Can not read the votable : " + voTableURL, ioe);
        }
    }
}
