/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.gui.PreferencesView;
import fr.jmmc.jmcs.gui.action.RegisteredAction;
import java.awt.event.ActionEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This registered preference action simply show the Preference view.
 * @author bourgesl
 */
public final class ShowPrefAction extends RegisteredAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  public final static String className = ShowPrefAction.class.getName();
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "showPreferences";
  /** Class logger */
  private final static Logger logger = LoggerFactory.getLogger(className);

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
  @Override
  public void actionPerformed(final ActionEvent evt) {
    logger.debug("actionPerformed");

    showPreferencesView();
  }

  /**
   * Show a new preferences view
   */
  public static void showPreferencesView() {
    new PreferencesView().setVisible(true);
  }
}
