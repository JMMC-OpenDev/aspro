/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.interop;

import fr.jmmc.aspro.gui.action.ExportOIFitsAction;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.task.TaskSwingWorkerExecutor;
import fr.jmmc.jmcs.network.interop.SampCapability;
import fr.jmmc.jmcs.network.interop.SampCapabilityAction;
import fr.jmmc.jmcs.util.FileUtils;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIFitsWriter;
import fr.nom.tam.fits.FitsException;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This registered action represents an Interop Menu entry to
 * send generated oifits data to any FITS application (OIFitsExplorer, topcat)...
 *
 * @author LAURENT BOURGES
 */
public final class SendOIFitsAction extends SampCapabilityAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    private final static String className = SendOIFitsAction.class.getName();
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "sendOIFitsAction";
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(className);

    /* members */
    /** current OIFitsFile to send */
    private transient OIFitsFile currentOIFitsFile = null;

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public SendOIFitsAction() {
        super(className, actionName, SampCapability.LOAD_FITS_TABLE);
    }

    /**
     * This method automatically sends the message returned by composeMessage()
     * to user selected client(s). Children classes should not overwrite this
     * method or must call super implementation to keep SAMP message management.
     *
     * @param e actionEvent coming from SWING objects. It contains in its
     * command the name of the destination.
     */
    @Override
    public void actionPerformed(final ActionEvent e) {

        // check if there is any running task :
        if (TaskSwingWorkerExecutor.isTaskRunning()) {
            MessagePane.showMessage("Computations are in progress; please retry later.");
            return;
        }

        // Get the oifits data
        final List<OIFitsFile> oiFitsFiles = ObservationManager.getInstance().getOIFitsList();

        if (oiFitsFiles == null) {
            MessagePane.showMessage("There is currently no OIFits data (your target is not observable)");
            return;
        }

        try {
            final String command = e.getActionCommand();

            for (OIFitsFile oiFitsFile : oiFitsFiles) {
                if (oiFitsFile != null) {
                    this.currentOIFitsFile = oiFitsFile;
                    composeAndSendMessage(command);
                }
            }
        } finally {
            // cleanup:
            currentOIFitsFile = null;
        }
    }

    /**
     * Should return the message you want to send
     * @throws IllegalStateException if the oifits file can not be written to a temporary file
     * @return Samp message parameters as a map
     */
    @Override
    public Map<?, ?> composeMessage() throws IllegalStateException {
        logger.debug("composeMessage");

        // use current OIFits file:
        final OIFitsFile oiFitsFile = this.currentOIFitsFile;

        final File file = FileUtils.getTempFile(ExportOIFitsAction.getDefaultFileName(oiFitsFile, false), ExportOIFitsAction.OIFITS_EXTENSION);

        try {
            OIFitsWriter.writeOIFits(file.getAbsolutePath(), oiFitsFile);
        } catch (FitsException fe) {
            throw new IllegalStateException("Could not export to temporary file : " + file.getAbsolutePath(), fe);
        } catch (IOException ioe) {
            throw new IllegalStateException("Could not export to temporary file : " + file.getAbsolutePath(), ioe);
        }

        // Store parameters for reply message
        final URI uri = file.toURI();

        final Map<String, String> parameters = new HashMap<String, String>(4);
        parameters.put("url", uri.toString());

        return parameters;
    }
}
