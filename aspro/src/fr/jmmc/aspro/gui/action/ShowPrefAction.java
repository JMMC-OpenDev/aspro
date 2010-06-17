/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ShowPrefAction.java,v 1.3 2010-06-09 12:54:12 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.gui.PreferencesView;
import fr.jmmc.mcs.util.RegisteredAction;
import java.awt.event.ActionEvent;
import java.util.logging.Level;

/**
 * This registered preference action simply show the Preference view.
 * @author bourgesl
 */
public class ShowPrefAction extends RegisteredAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  public final static String className = "fr.jmmc.aspro.gui.action.ShowPrefAction";
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "showPreferences";
  /** Class logger */
  private final static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className);

  /**
   * Public constructor that automatically register the action in RegisteredAction.
   */
  public ShowPrefAction() {
    super(className, actionName);
    flagAsPreferenceAction();
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
      new PreferencesView().setVisible(true);
    } catch (Exception ex) {
      // @todo handle this error at user level
      logger.log(Level.SEVERE, "actionPerformed", ex);
    }
  }
}
