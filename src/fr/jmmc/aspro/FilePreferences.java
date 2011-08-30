/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro;

import fr.jmmc.jmcs.util.MimeType;
import fr.jmmc.jmcs.data.preference.PreferencesException;
import java.io.File;
import java.util.logging.Level;

/**
 * This class gathers user preferences related to local folders
 * @author bourgesl
 */
public final class FilePreferences extends fr.jmmc.jmcs.data.preference.Preferences {

  /** Singleton instance */
  private static FilePreferences _singleton = null;
  /** Logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(FilePreferences.class.getName());
  /* Preferences */
  /** Store the filename of the ASPRO file preference file */
  public static final String PREFERENCES_FILENAME = "fr.jmmc.aspro.file.properties";

  /**
   * Private constructor that must be empty.
   *
   * @param notify flag to enable/disable observer notifications
   */
  private FilePreferences(final boolean notify) {
    super(notify);
  }

  /**
   * Return the singleton instance of FilePreferences.
   *
   * @return the singleton instance
   */
  public synchronized static FilePreferences getInstance() {
    // Build new reference if singleton does not already exist
    // or return previous reference
    if (_singleton == null) {
      logger.fine("FilePreferences.getInstance()");
      // disable notifications:
      _singleton = new FilePreferences(false);
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
    logger.fine("FilePreferences.setDefaultPreferences");
    final String defaultDirectory = System.getProperty("user.home");
    setDefaultPreference(MimeType.ASPRO_OBSERVATION.toString(), defaultDirectory);
    setDefaultPreference(MimeType.OBX.toString(), defaultDirectory);
    setDefaultPreference(MimeType.OIFITS.toString(), defaultDirectory);
    setDefaultPreference(MimeType.PDF.toString(), defaultDirectory);
    setDefaultPreference(MimeType.STAR_LIST.toString(), defaultDirectory);
  }

  /**
   * Return the preference filename.
   *
   * @return preference filename.
   */
  protected String getPreferenceFilename() {
    return PREFERENCES_FILENAME;
  }

  /**
   *  Return preference version number.
   *
   * @return preference version number.
   */
  protected int getPreferencesVersionNumber() {
    return 1;
  }

  /**
   * Return the last directory used for files having this mime type. By default = user home
   * @param mimeType mime type to look for
   * @return last directory used or user home as File
   */
  public File getDirectoryFile(final MimeType mimeType) {
    return new File(getPreference(mimeType.toString()));
  }

  /**
   * Return the last directory used for files having this mime type. By default = user home
   * @param mimeType mime type to look for
   * @return last directory used or user home
   */
  public String getDirectory(final MimeType mimeType) {
    return getPreference(mimeType.toString());
  }

  /**
   * Define the last directory used for files having this mime type
   * @param mimeType mime type to look for
   * @param path file path to an existing directory
   */
  public void setDirectory(final MimeType mimeType, final String path) {
    if (path != null) {
      final String oldPath = getDirectory(mimeType);
      if (!path.equals(oldPath)) {
        try {
          setPreference(mimeType.toString(), path);
          saveToFile();
        } catch (PreferencesException pe) {
          logger.log(Level.WARNING, "Saving FilePreferences failure :", pe);
        }
      }
    }
  }

  /**
   * Run this program to generate the Aspro file preference file.
   * @param args NC
   */
  public static void main(String[] args) {
    try {
      FilePreferences.getInstance().saveToFile();
    } catch (PreferencesException pe) {
      logger.log(Level.SEVERE, "property failure : ", pe);
    }
  }
}
