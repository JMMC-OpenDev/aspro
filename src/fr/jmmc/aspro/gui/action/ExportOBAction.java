/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.AsproGui;
import fr.jmmc.aspro.gui.UVCoveragePanel;
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.util.RegisteredAction;
import java.awt.event.ActionEvent;
import java.util.logging.Level;

/**
 * This registered action represents a File Menu entry to export an OB. 
 * It delegates the action handling to the UV coverage panel that knows which target is selected and
 * which instrument to use.
 * @author bourgesl
 */
public class ExportOBAction extends RegisteredAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  private final static String className = "fr.jmmc.aspro.gui.action.ExportOBAction";
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "exportOB";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);

  /**
   * Public constructor that automatically register the action in RegisteredAction.
   */
  public ExportOBAction() {
    super(className, actionName);
  }

  /**
   * Handle the action event
   * @param evt action event
   */
  public void actionPerformed(final ActionEvent evt) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("actionPerformed");
    }

    final UVCoveragePanel uvCoveragePanel = AsproGui.getInstance().getSettingPanel().getUVCoveragePanel();

    // note : there is at least one target :
    if (uvCoveragePanel != null) {
      uvCoveragePanel.performOBAction(evt);
    } else {
      MessagePane.showMessage("Please enter first a target to export it as an Observing block.");
    }
  }
}
