package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.gui.*;
import fr.jmmc.mcs.util.RegisteredAction;
import java.awt.event.ActionEvent;

public class ShowPrefAction extends RegisteredAction {

    private final static String className = "fr.jmmc.mf.gui.actions.ShowPrefAction";
    /** Class logger */
    static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
            className);
    /** Preferences view */
    PreferencesView preferencesView;   

    public ShowPrefAction() {
        super(className, "showPreferences");        
        flagAsPreferenceAction();
        preferencesView = new PreferencesView();
    }

    public void actionPerformed(ActionEvent e) {
        try {
            preferencesView.setVisible(true);
            logger.fine("Showing preferences");
        } catch (Exception exc) {
            // @todo handle this error at user level
            exc.printStackTrace();
        }
    }
}
