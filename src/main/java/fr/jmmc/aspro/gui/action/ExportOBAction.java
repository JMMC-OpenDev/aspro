/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.Aspro2;
import fr.jmmc.aspro.gui.UVCoveragePanel;
import fr.jmmc.aspro.ob.ExportOBMode;
import fr.jmmc.jmcs.gui.action.RegisteredAction;
import fr.jmmc.jmcs.gui.component.MessagePane;
import java.awt.event.ActionEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This registered action represents a File Menu entry to export an OB. 
 * It delegates the action handling to the UV coverage panel that knows which target is selected and
 * which instrument to use.
 * @author bourgesl
 */
public final class ExportOBAction extends RegisteredAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    private final static String className = ExportOBAction.class.getName();
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "exportOB";
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(className);

    /* members */
    /** flag enabling the new OB output */
    private final boolean xml;
    
    /**
     * Public constructor that automatically register the action in RegisteredAction.
     * @param xml true to use new OB output
     */
    public ExportOBAction(final boolean xml) {
        super(className, (xml) ? (actionName + "XML") : actionName);
        this.xml = xml;
    }

    /**
     * Handle the action event
     * @param evt action event
     */
    @Override
    public void actionPerformed(final ActionEvent evt) {
        logger.debug("actionPerformed");

        final UVCoveragePanel uvCoveragePanel = Aspro2.getInstance().getSettingPanel().getUVCoveragePanel();

        // note : there is at least one target :
        if (uvCoveragePanel != null) {
            uvCoveragePanel.performOBAction(evt, ExportOBMode.SINGLE, this.xml);
        } else {
            MessagePane.showMessage("Please enter first a target to export it as an Observing block.");
        }
    }
}
