/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: NewObservationAction.java,v 1.4 2010-09-26 12:43:58 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.gui.StatusBar;
import java.awt.event.ActionEvent;
import java.util.logging.Level;

/**
 * New observation settings action
 * @author bourgesl
 */
public class NewObservationAction extends ObservationFileAction {

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
    final ObservationManager om = ObservationManager.getInstance();

    om.reset();

    StatusBar.show("new observation created.");
  }
}
