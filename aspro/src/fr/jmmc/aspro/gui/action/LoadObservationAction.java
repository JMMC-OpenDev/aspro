/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.jmcs.data.preference.SessionSettingsPreferences;
import fr.jmmc.jmcs.gui.action.ActionRegistrar;
import fr.jmmc.jmcs.gui.action.RegisteredAction;
import fr.jmmc.jmcs.gui.component.FileChooser;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.data.MimeType;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Open observation settings action
 * @author bourgesl
 */
public final class LoadObservationAction extends RegisteredAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  public final static String className = LoadObservationAction.class.getName();
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "loadObservation";
  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(className);
  /** AsproX MimeType */
  private final static MimeType mimeType = MimeType.ASPRO_OBSERVATION;

  /**
   * Public constructor that automatically register the action in RegisteredAction.
   */
  public LoadObservationAction() {
    super(className, actionName);
    flagAsOpenAction();
  }

  /**
   * Handle the action event
   * @param evt action event
   */
  @Override
  public void actionPerformed(final ActionEvent evt) {
    logger.debug("actionPerformed");

    final ObservationManager om = ObservationManager.getInstance();

    File file;

    // If the action was automatically triggered from App launch
    if (evt.getSource() == ActionRegistrar.getInstance()) {
      file = new File(evt.getActionCommand());

      if (!file.exists() || !file.isFile()) {
        MessagePane.showErrorMessage("Could not load the file : " + file.getAbsolutePath());
        file = null;
      }

      if (file != null) {
        // update current directory for Observation settings:
        SessionSettingsPreferences.setCurrentDirectoryForMimeType(mimeType, file.getParent());
      }

    } else {

      final File obsFile = om.getObservationFile();

      final String defaultFileName;

      if (obsFile != null) {
        defaultFileName = obsFile.getName();
      } else {
        defaultFileName = null;
      }

      file = FileChooser.showOpenFileChooser("Load observation settings", null, mimeType, defaultFileName);
    }

    // If a file was defined (No cancel in the dialog)
    if (file != null) {
      try {
        final String message = om.load(file);

        StatusBar.show("file loaded : " + file.getName());

        if (message != null) {
          MessagePane.showMessage(message);
        }

      } catch (IOException ioe) {
        MessagePane.showErrorMessage("Could not load the file : " + file.getAbsolutePath(), ioe);
      } catch (IllegalArgumentException iae) {
        MessagePane.showErrorMessage("Invalid observation file : " + file.getAbsolutePath(), iae);
      }
    }
  }
}
