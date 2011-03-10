/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: NewObservationAction.java,v 1.6 2011-02-03 17:29:55 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.5  2010/10/01 15:33:52  bourgesl
 * Added a confirm dialog for the 'New Observation' action
 *
 * Revision 1.4  2010/09/26 12:43:58  bourgesl
 * removed try catch runtime exception
 *
 * Revision 1.3  2010/09/24 15:54:25  bourgesl
 * better exception handling + use MessagePane
 *
 * Revision 1.2  2010/09/01 12:57:13  bourgesl
 * added runtime exception message to user message dialog
 *
 * Revision 1.1  2010/07/07 15:16:25  bourgesl
 * added 'New Observation' action
 *
 */
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.mcs.gui.App;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.util.ActionRegistrar;
import fr.jmmc.mcs.util.RegisteredAction;
import java.awt.event.ActionEvent;
import java.util.logging.Level;
import javax.swing.AbstractAction;
import javax.swing.JOptionPane;

/**
 * New observation settings action
 * @author bourgesl
 */
public class NewObservationAction extends RegisteredAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  public final static String className = "fr.jmmc.aspro.gui.action.NewObservationAction";
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "newObservation";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);

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
  public void actionPerformed(final ActionEvent evt) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("actionPerformed");
    }

    // Ask the user if he wants to save modifications
    final Object[] options = {"Save", "Cancel", "Don't Save"};
    final int result = JOptionPane.showOptionDialog(App.getFrame(),
            "Do you want to save changes before creating a new observation ?\nIf you don't save, your changes will be lost.\n\n",
            null, JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE, null, options,
            options[0]);

    // If the User clicked the "Save" button, save and go on
    if (result == 0) {
      final AbstractAction action = (SaveObservationAction) ActionRegistrar.getInstance().get(SaveObservationAction.className, SaveObservationAction.actionName);

      action.actionPerformed(null);
    }
    // If the user clicked the "Cancel" button, don't quit
    if (result == 1) {
      return;
    }

    // If the user clicked the "Don't Save" button, go on
    ObservationManager.getInstance().reset();

    StatusBar.show("new observation created.");
  }
}
