/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.interop;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.network.interop.SampCapability;
import fr.jmmc.jmcs.network.interop.SampCapabilityAction;
import fr.jmmc.jmcs.service.XslTransform;
import fr.jmmc.jmcs.util.FileUtils;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This registered action represents an Interop Menu entry to
 * send targets as votable to any VO application.
 *
 * @author bourgesl
 */
public class SendVOTableAction extends SampCapabilityAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    private final static String className = SendVOTableAction.class.getName();
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "sendVOTableAction";
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(className);
    /** XSLT file path */
    private final static String XSLT_FILE = "fr/jmmc/aspro/interop/AsproObservation2vot.xsl";

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public SendVOTableAction() {
        super(className, actionName, SampCapability.LOAD_VO_TABLE);
    }

    /**
     * Should return the message you want to send
     * @throws IllegalStateException if the oifits file can not be written to a temporary file
     * @return Samp message parameters as a map
     */
    @Override
    public Map<?, ?> composeMessage() throws IllegalStateException {
        logger.debug("composeMessage");

        File file = null;
        try {
            file = FileUtils.getTempFile("votable-", ".vot");

            // Save the VOTable:
            saveVOTable(file);

        } catch (IOException ioe) {
            StatusBar.show("Could not write votable through SAMP.");

            logger.warn("Could not write votable to temp file '{}'.", file, ioe);
            return null;
        }

        // Store parameters into SAMP message:
        final Map<String, String> parameters = new HashMap<String, String>(4);
        addUrlParameter(parameters, file);
        return parameters;
    }

    /**
     * Save the current observation as votable
     *
     * @param file file to save
     * 
     * @throws IOException if an I/O exception occured
     * @throws IllegalArgumentException if the file is not an Observation
     */
    public static void saveVOTable(final File file) throws IOException {

        // serialize observation to xml :
        final String xmlObservation = ObservationManager.getInstance().saveToString();

        logger.debug("observation:\n{}", xmlObservation);

        // use an XSLT to transform the Aspro 2 Observation to a VOTable document:
        final long start = System.nanoTime();

        final String document = XslTransform.transform(xmlObservation, XSLT_FILE);

        logger.info("VOTable transformation (XSLT): {} ms.", 1e-6d * (System.nanoTime() - start));

        logger.debug("document:\n{}", document);

        // Finally, write the file :
        FileUtils.writeFile(file, document);
    }
}
