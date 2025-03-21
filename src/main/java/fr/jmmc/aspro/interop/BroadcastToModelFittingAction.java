/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.interop;

import fr.jmmc.aspro.gui.action.ExportOIFitsAction;
import fr.jmmc.aspro.model.OIFitsData;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.jmal.model.targetmodel.Model;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.task.TaskSwingWorkerExecutor;
import fr.jmmc.jmcs.network.interop.SampCapability;
import fr.jmmc.jmcs.network.interop.SampCapabilityAction;
import fr.jmmc.jmcs.util.FileUtils;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIFitsWriter;
import fr.nom.tam.fits.FitsException;
import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This registered action represents an Interop Menu entry to
 * send model and generated oifits to one modelfitting application.
 *
 * @author mella
 */
public final class BroadcastToModelFittingAction extends SampCapabilityAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    private final static String className = BroadcastToModelFittingAction.class.getName();
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "broadcastToModelFittingAction";
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(className);

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public BroadcastToModelFittingAction() {
        super(className, actionName, SampCapability.LITPRO_START_SETTING);
    }

    /**
     * Should return the message you want to send
     * @throws IllegalStateException if the oifits file can not be written to a temporary file
     * @return Samp message parameters as a map
     */
    @Override
    public Map<?, ?> composeMessage() throws IllegalStateException {
        logger.debug("composeMessage");

        // Use main observation to check variants :
        if (!ObservationManager.getInstance().getMainObservation().isSingle()) {
            MessagePane.showMessage("Aspro 2 can not generate an OIFits file when multiple configurations are selected !");
            return null;
        }

        // check if there is any running task :
        if (TaskSwingWorkerExecutor.isTaskRunning()) {
            MessagePane.showMessage("Computations are in progress; please retry later.");
            return null;
        }

        // Get the oifits object
        // save it on disk
        // get Target model from the targetId given by oifits.oiTargets
        // serialize and build one samp message

        final List<OIFitsFile> oiFitsFiles = ObservationManager.getInstance().checkAndGetOIFitsList();

        if (oiFitsFiles == null) {
            MessagePane.showMessage("There is currently no OIFits data (your target is not observable)");
            return null;
        } else if (oiFitsFiles == OIFitsData.IGNORE_OIFITS_LIST) {
            return null;
        }

        // use first (for now): TODO FIX
        final OIFitsFile oiFitsFile = oiFitsFiles.get(0);

        final File file = FileUtils.getTempFile(ExportOIFitsAction.getDefaultFileName(oiFitsFile, false), ExportOIFitsAction.OIFITS_EXTENSION);

        try {
            OIFitsWriter.writeOIFits(file.getAbsolutePath(), oiFitsFile);
        } catch (FitsException fe) {
            throw new IllegalStateException("Could not export to temporary file : " + file.getAbsolutePath(), fe);
        } catch (IOException ioe) {
            throw new IllegalStateException("Could not export to temporary file : " + file.getAbsolutePath(), ioe);
        }

        // Get Model assuming that target name is the first one (and only one).. of oifits.
        final String targetName = oiFitsFile.getOiTarget().getTarget()[0];

        final Target target = ObservationManager.getInstance().getTarget(targetName);

        final Model targetModel = new Model();
        targetModel.setNameAndType("Container");
        
        for (Model model : target.getModels()) {
            targetModel.getModels().add(model);
        }

        // Create a 4K buffer for models :
        final StringWriter sw = new StringWriter(4096); // 4K buffer

        // serialize models to xml :
        ObservationManager.getInstance().saveObject(sw, targetModel);

        final String xmlModel = sw.toString();

        // Store parameters for reply message
        final Map<String, String> parameters = new HashMap<String, String>(4);
        parameters.put("model", xmlModel);
        parameters.put("filename", file.getAbsolutePath());

        return parameters;
    }
}
