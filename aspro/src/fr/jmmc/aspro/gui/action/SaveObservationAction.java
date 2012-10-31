/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.jmcs.gui.action.RegisteredAction;
import fr.jmmc.jmcs.gui.component.FileChooser;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.util.MimeType;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Save observation settings action
 * @author bourgesl
 */
public final class SaveObservationAction extends RegisteredAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  public final static String className = SaveObservationAction.class.getName();
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "saveObservation";
  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(className);
  /** AsproX MimeType */
  private final static MimeType mimeType = MimeType.ASPRO_OBSERVATION;

  /**
   * Public constructor that automatically register the action in RegisteredAction.
   */
  public SaveObservationAction() {
    super(className, actionName);
  }

  /**
   * Handle the action event
   * @param evt action event
   */
  @Override
  public void actionPerformed(final ActionEvent evt) {
    logger.debug("actionPerformed");

    save();
  }

  /**
   * Save the current observation
   * @return true if successful
   */
  public boolean save() {
    final ObservationManager om = ObservationManager.getInstance();
    final File obsFile = om.getObservationFile();

    final String defaultFileName;

    if (obsFile != null) {
      defaultFileName = obsFile.getName();
    } else {
      defaultFileName = null;
    }

    final File file = FileChooser.showSaveFileChooser("Save the current observation settings", null, mimeType, defaultFileName);

    boolean result = true;

    // If a file was defined (No cancel in the dialog)
    if (file != null) {
      try {
        om.save(file);

        StatusBar.show("file saved : " + file.getName());

      } catch (IOException ioe) {
        result = false;
        MessagePane.showErrorMessage("Could not save the file : " + file.getAbsolutePath(), ioe);
      }
    }
    return result;
  }
}
