/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: NewObservationAction.java,v 1.1 2010-07-07 15:16:25 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.util.ActionRegistrar;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.logging.Level;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

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

    try {
      om.reset();

      StatusBar.show("new observation created.");

    } catch (RuntimeException re) {
      logger.log(Level.SEVERE, "runtime failure : ", re);

      JOptionPane.showMessageDialog(null,
              "Could not create new observation",
              "Error", JOptionPane.ERROR_MESSAGE);
    }
  }
}
