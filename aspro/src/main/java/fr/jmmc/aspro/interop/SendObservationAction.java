/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.interop;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.network.interop.SampCapability;
import fr.jmmc.jmcs.network.interop.SampCapabilityAction;
import fr.jmmc.jmcs.util.FileUtils;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This registered action represents an Interop Menu entry to
 * send observation to other ASPRO2 instances.
 *
 * @author bourgesl
 */
public class SendObservationAction extends SampCapabilityAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    private final static String className = SendObservationAction.class.getName();
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "sendObservationAction";
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(className);

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public SendObservationAction() {
        super(className, actionName, SampCapability.ASPRO_LOAD_OBSERVATION);
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
            file = FileUtils.getTempFile("aspro-", ".asprox");

            ObservationManager.getInstance().saveObservation(file);

        } catch (IOException ioe) {
            StatusBar.show("Could not send observation through SAMP.");

            logger.warn("Could not write observation to temp file '{}'.", file, ioe);
            return null;
        }

        // Store parameters into SAMP message:
        final Map<String, String> parameters = new HashMap<String, String>(4);
        addUrlParameter(parameters, file);
        return parameters;
    }
}
