package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.gui.PreferencesView;
import fr.jmmc.mcs.util.RegisteredAction;
import java.awt.event.ActionEvent;
import java.util.logging.Level;

public class ShowPrefAction extends RegisteredAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  private final static String className = "fr.jmmc.aspro.gui.action.ShowPrefAction";
  /** Class logger */
  private final static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className);

  public ShowPrefAction() {
    super(className, "showPreferences");
    flagAsPreferenceAction();
  }

  public void actionPerformed(ActionEvent e) {
    try {
      new PreferencesView().setVisible(true);
      logger.fine("Showing preferences");
    } catch (Exception ex) {
      // @todo handle this error at user level
      logger.log(Level.SEVERE, "actionPerformed", ex);
    }
  }
}
