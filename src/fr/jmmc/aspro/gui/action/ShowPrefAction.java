/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.gui.PreferencesView;
import fr.jmmc.jmcs.gui.action.RegisteredAction;
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

    new PreferencesView().setVisible(true);
  }
}
