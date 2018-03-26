/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.model.TargetImporter;
import fr.jmmc.jmcs.data.MimeType;
import fr.jmmc.jmcs.gui.action.RegisteredAction;
import fr.jmmc.jmcs.gui.component.FileChooser;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.component.StatusBar;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Import Targets action
 * @author bourgesl
 */
public final class ImportTargetsAction extends RegisteredAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    public final static String className = ImportTargetsAction.class.getName();
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "importTargets";
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(className);
    /** AsproX MimeType */
    private final static MimeType mimeType = MimeType.ASPRO_OBSERVATION;

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public ImportTargetsAction() {
        super(className, actionName);
    }

    /** 
     * Handle the action event
     * @param evt action event
     */
    @Override
    public void actionPerformed(final ActionEvent evt) {
        logger.debug("actionPerformed");

        final File obsFile = FileChooser.showOpenFileChooser("Import targets from an observation", null, mimeType, null);

        // If a file was defined (No cancel in the dialog)
        if (obsFile != null) {
            try {
                StatusBar.show("file loaded : " + obsFile.getName());

                // add targets only:
                TargetImporter.processObservation(obsFile, true);

            } catch (IOException ioe) {
                MessagePane.showErrorMessage("Could not load the file : " + obsFile.getAbsolutePath(), ioe);
            }
        }
    }
}
