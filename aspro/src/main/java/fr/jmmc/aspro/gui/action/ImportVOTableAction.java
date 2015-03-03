/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.interop.AnyVOTableHandler;
import fr.jmmc.jmcs.data.MimeType;
import fr.jmmc.jmcs.gui.action.RegisteredAction;
import fr.jmmc.jmcs.gui.component.FileChooser;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.util.FileUtils;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Import VOTable action (targets)
 * @author bourgesl
 */
public final class ImportVOTableAction extends RegisteredAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    public final static String className = ImportVOTableAction.class.getName();
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "importVOTable";
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(className);
    /** AsproX MimeType */
    private final static MimeType mimeType = MimeType.VOTABLE;

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public ImportVOTableAction() {
        super(className, actionName);
    }

    /** 
     * Handle the action event
     * @param evt action event
     */
    @Override
    public void actionPerformed(final ActionEvent evt) {
        logger.debug("actionPerformed");

        final File voTableFile = FileChooser.showOpenFileChooser("Import targets from VOTable", null, mimeType, null);

        // If a file was defined (No cancel in the dialog)
        if (voTableFile != null) {
            try {
                final String votable = FileUtils.readFile(voTableFile);

                StatusBar.show("file loaded : " + voTableFile.getName());

                logger.debug("votable :\n{}", votable);

                // add targets only:
                AnyVOTableHandler.processVOTable(votable, true);

            } catch (IOException ioe) {
                MessagePane.showErrorMessage("Could not load the file : " + voTableFile.getAbsolutePath(), ioe);
            }
        }
    }
}
