/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.gui.ConfigurationManagerPanel;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.jmcs.gui.action.RegisteredAction;
import java.awt.event.ActionEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This action opens the Configuration editor
 * @author bourgesl
 */
public final class ConfigurationEditorAction extends RegisteredAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    public final static String className = ConfigurationEditorAction.class.getName();
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "showEditor";
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(className);

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public ConfigurationEditorAction() {
        super(className, actionName);

        setEnabled(ConfigurationManager.ENABLE_USER_CONFIG);
    }

    /**
     * Handle the action event
     * @param evt action event
     */
    @Override
    public void actionPerformed(final ActionEvent evt) {
        logger.debug("actionPerformed");

        ConfigurationManagerPanel.showDialog();
    }
}
