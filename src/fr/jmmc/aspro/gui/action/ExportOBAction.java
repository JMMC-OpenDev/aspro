/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ExportOBAction.java,v 1.6 2011-01-07 13:21:08 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.5  2010/10/05 15:06:00  bourgesl
 * use AsproGui:getInstance()
 *
 * Revision 1.4  2010/10/01 15:31:34  bourgesl
 * added message 'Please enter first a target to export it as an Observing block'
 *
 * Revision 1.3  2010/09/01 16:24:30  bourgesl
 * removed exception
 *
 * Revision 1.2  2010/06/17 10:02:50  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.1  2010/06/09 12:53:00  bourgesl
 * new action (menu File) that calls the uv coverage panel method performOBAction()
 *
 */
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

    // be sure the uv panel is visible to avoid user wrong inputs :
    if (uvCoveragePanel != null) {
      uvCoveragePanel.performOBAction(evt);
    } else {
      MessagePane.showMessage("Please enter first a target to export it as an Observing block.");
    }
  }
}
