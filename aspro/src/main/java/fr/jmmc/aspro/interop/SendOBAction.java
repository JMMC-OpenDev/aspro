/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.interop;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.ob.ExportOBXml;
import fr.jmmc.aspro.service.ObservabilityService;
import fr.jmmc.jmcs.gui.component.MessagePane;
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
 * send generated OB data to the A2P2 tool (python wrapper to ESO p2web) ...
 *
 * @author LAURENT BOURGES
 */
public final class SendOBAction extends SampCapabilityAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    private final static String CLASS_NAME = SendOBAction.class.getName();
    /** Action name. This name is used to register to the ActionRegistrar */
    private final static String ACTION_NAME = "sendOBAction";
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(CLASS_NAME);

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public SendOBAction() {
        super(CLASS_NAME, ACTION_NAME, SampCapability.LOAD_OB_DATA);
    }

    /**
     * Create the Samp message parameters with the Starlist
     * @return Samp message parameters as a map
     */
    @Override
    public Map<?, ?> composeMessage() {
        // Use main observation to check instrument :
        final ObservationSetting observation = ObservationManager.getInstance().getMainObservation();

        if (!observation.isSingle()) {
            MessagePane.showMessage("Aspro 2 can not send a star list when multiple configurations are selected !");
            return null;
        }

        // retrieve the selected target from its name:
        final Target target = observation.getTarget(observation.getSelectedTargetName());

        if (target == null) {
            return null;
        }

        logger.info("composeMessage: target = {}", target);

        // Compute Observability data using astronomical night (-18 deg) without night restrictions :
        final ObservabilityService os = ExportOBXml.processObservability(observation);

        final File file = FileUtils.getTempFile("ob-", ".obxml");
        try {
            ExportOBXml.process(file, observation, os, target);

        } catch (IOException ioe) {
            MessagePane.showErrorMessage("Could not export to file : " + file.getAbsolutePath(), ioe);
            return null;
        }

        // Store parameters into SAMP message:
        final Map<String, String> parameters = new HashMap<String, String>(4);
        addUrlParameter(parameters, file);
        return parameters;
    }
}
