/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro;

import fr.jmmc.aspro.model.observability.SunTimeInterval.SunType;
import fr.jmmc.jmcs.data.preference.PreferencesException;

/**
 * Handles preferences for Aspro.
 *
 * Note : There is a special preference 'splash.screen.show' used to disable the splash screen (dev mode) if its value is 'false'.
 */
public final class Preferences extends fr.jmmc.jmcs.data.preference.Preferences {

  /** Singleton instance */
  private static Preferences _singleton = null;
  /** Class Name */
  private final static String className_ = "fr.jmmc.aspro.Preferences";
  /** Logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /* Preferences */
  /** Preference : edit positions in XY (true) or rho/theta (false) in the model editor */
  public final static String MODELEDITOR_PREFERXY = "modeleditor.preferxy";
  /** Preference : LUT table to use for the object model image in the UV Coverage plot */
  public final static String MODEL_IMAGE_LUT = "model.image.lut";
  /** Preference : Image size to use for the object model image in the UV Coverage plot */
  public final static String MODEL_IMAGE_SIZE = "model.image.size";
  /** Preference : time reference (LST/UTC) */
  public final static String TIME_REFERENCE = "time.reference";
  /** Preference : minimum elevation */
  public final static String MIN_ELEVATION = "min.elevation";
  /** Preference : center observability plot arround night */
  public final static String CENTER_NIGHT = "center.night";
  /** Preference : twilight night (Astro_Twilight/Nautic_Twilight/Civil_Twilight): see SunTimeInterval */
  public final static String TWILIGHT_NIGHT = "twilight.night";

  /**
   * Private constructor that must be empty.
   *
   * @param notify flag to enable/disable observer notifications
   */
  private Preferences(final boolean notify) {
    super(notify);
  }

  /**
   * Return the singleton instance of Preferences.
   *
   * @return the singleton preference instance
   */
  public synchronized static Preferences getInstance() {
    // Build new reference if singleton does not already exist
    // or return previous reference
    if (_singleton == null) {
      logger.fine("Preferences.getInstance()");

      // disable notifications:
      _singleton = new Preferences(false);

      // enable future notifications:
      _singleton.setNotify(true);
    }

    return _singleton;
  }

  /**
   * Define the default properties used to reset default preferences.
   *
   * @throws PreferencesException if any preference value has a unsupported class type
   */
  protected void setDefaultPreferences() throws PreferencesException {
    logger.fine("Preferences.setDefaultPreferences()");

    // Model editor :
    setDefaultPreference(MODELEDITOR_PREFERXY, Boolean.FALSE);

    // UV Coverage - image size and LUT :
    setDefaultPreference(MODEL_IMAGE_LUT, AsproConstants.DEFAULT_IMAGE_LUT);
    setDefaultPreference(MODEL_IMAGE_SIZE, AsproConstants.DEFAULT_IMAGE_SIZE);

    // Time reference :
    setDefaultPreference(TIME_REFERENCE, AsproConstants.TIME_LST);

    // minimum elevation:
    setDefaultPreference(MIN_ELEVATION, Double.valueOf(AsproConstants.DEFAULT_MIN_ELEVATION));

    // center observability plot arround night:
    setDefaultPreference(CENTER_NIGHT, Boolean.TRUE);

    // twilight night (Astro_Twilight):
    setDefaultPreference(TWILIGHT_NIGHT, SunType.Night.toString());
  }

  /**
   * Return the preference filename.
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

  /**
   * Return the twilight night Preference : use preferences or Astro_Twilight if it is undefined
   * @return twilight night (Astro_Twilight/Nautic_Twilight/Civil_Twilight)
   */
  public SunType getTwilightAsNightLimit() {
    final String value = getPreference(TWILIGHT_NIGHT);

    try {
      return SunType.valueOf(value);
    } catch (IllegalArgumentException iae) {
      // ignore
    }
    return SunType.Night;
  }
}
