/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ExportOBAction.java,v 1.2 2010-06-17 10:02:50 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/06/09 12:53:00  bourgesl
 * new action (menu File) that calls the uv coverage panel method performOBAction()
 *
 */
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.AsproGui;
import fr.jmmc.aspro.gui.UVCoveragePanel;
import fr.jmmc.mcs.gui.App;
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
    try {
      final AsproGui app = (AsproGui) App.getSharedInstance();
      final UVCoveragePanel uvCoveragePanel = app.getSettingPanel().getUVCoveragePanel();

      // be sure the uv panel is visible to avoid user wrong inputs :
      if (uvCoveragePanel != null && uvCoveragePanel.isVisible()) {
        uvCoveragePanel.performOBAction(evt);
      }
    } catch (Exception ex) {
      // @todo handle this error at user level
      logger.log(Level.SEVERE, "actionPerformed", ex);
    }
  }
}
