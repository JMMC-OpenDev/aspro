/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.Aspro2;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.jmcs.gui.action.RegisteredAction;
import fr.jmmc.jmcs.gui.component.StatusBar;
import java.awt.event.ActionEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * New observation settings action
 * @author bourgesl
 */
public final class NewObservationAction extends RegisteredAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    public final static String className = NewObservationAction.class.getName();
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "newObservation";
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(className);

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public NewObservationAction() {
        super(className, actionName);
    }

    /**
     * Handle the action event
     * @param evt action event
     */
    @Override
    public void actionPerformed(final ActionEvent evt) {
        logger.debug("actionPerformed");

        if (Aspro2.getInstance().checkAndConfirmSaveChanges("creating a new observation")) {
            ObservationManager.getInstance().reset();

            StatusBar.show("new observation created.");

            Aspro2.updateFrameTitle(null);
        }
    }
}
