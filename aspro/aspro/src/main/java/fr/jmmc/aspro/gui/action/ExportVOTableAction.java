/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.interop.SendVOTableAction;
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
 * Export VOTable action (targets)
 * @author bourgesl
 */
public final class ExportVOTableAction extends RegisteredAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    public final static String className = ExportVOTableAction.class.getName();
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "exportVOTable";
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(className);
    /** AsproX MimeType */
    private final static MimeType mimeType = MimeType.VOTABLE;

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public ExportVOTableAction() {
        super(className, actionName);
    }

    /** 
     * Handle the action event
     * @param evt action event
     */
    @Override
    public void actionPerformed(final ActionEvent evt) {
        logger.debug("actionPerformed");

        final File voTableFile = FileChooser.showSaveFileChooser("Export targets to VOTable", null, mimeType, null);

        // If a file was defined (No cancel in the dialog)
        if (voTableFile != null) {
            try {
                // Save the VOTable:
                SendVOTableAction.saveVOTable(voTableFile);

                StatusBar.show("file saved : " + voTableFile.getName());

            } catch (IOException ioe) {
                MessagePane.showErrorMessage("Could not save the file : " + voTableFile.getAbsolutePath(), ioe);
            }
        }
    }
}
