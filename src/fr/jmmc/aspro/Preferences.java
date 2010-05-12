/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: Preferences.java,v 1.1 2010-05-12 08:44:10 mella Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 *
 */
package fr.jmmc.aspro;

import fr.jmmc.mcs.util.PreferencesException;

/**
 * Handles preference of aspro
 */
public class Preferences extends fr.jmmc.mcs.util.Preferences {

    /** Singleton instance */
    private static Preferences _singleton = null;
    /** Class Name */
    private final static String className_ = "fr.jmmc.aspro.Preferences";
    /** Logger */
    static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
            className_);

    public final static String MODELEDITOR_PREFERXY = "modeleditor.preferxy";
    public final static String HELP_TOOLTIPS_SHOW = "help.tooltips.show";
    /**
     * Privatized constructor that must be empty.
     */
    private Preferences() {
    }

    /**
     * Return the singleton instance of Preferences.
     *
     * @return the singleton preference instance
     */
    public final synchronized static Preferences getInstance() {
        // Build new reference if singleton does not already exist
        // or return previous reference
        if (_singleton == null) {
            _singleton = new Preferences();
        }

        return _singleton;
    }

    protected void setDefaultPreferences() throws PreferencesException {
        /* Place general preferences  */
        setDefaultPreference(HELP_TOOLTIPS_SHOW, "true");
        setDefaultPreference(MODELEDITOR_PREFERXY, "false");
    }

    /**
     * Return preference filename.
     *
     * @return preference filename.
     */
    protected String getPreferenceFilename() {
        logger.entering(className_, "getPreferenceFilename");
        return "fr.jmmc.aspro.properties";
    }

    /**
     *  Return preference version number.
     *
     * @return preference version number.
     */
    protected int getPreferencesVersionNumber() {
        logger.entering(className_, "getPreferencesVersionNumber");
        return 1;
    }
}
