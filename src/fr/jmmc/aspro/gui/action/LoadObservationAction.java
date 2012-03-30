/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.FilePreferences;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.jmcs.gui.MessagePane;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.gui.action.ActionRegistrar;
import fr.jmmc.jmcs.util.FileUtils;
import fr.jmmc.jmcs.util.MimeType;
import fr.jmmc.jmcs.gui.action.RegisteredAction;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import javax.swing.JFileChooser;

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

    File file = null;

    // If the action was automatically triggered from App launch
    if (evt.getSource() == ActionRegistrar.getInstance()) {
      file = new File(evt.getActionCommand());
    } else {

      final JFileChooser fileChooser = new JFileChooser();
      fileChooser.setFileFilter(mimeType.getFileFilter());
      fileChooser.setCurrentDirectory(FilePreferences.getInstance().getDirectoryFile(mimeType));

      fileChooser.setDialogTitle("Load observation settings");

      final int returnVal = fileChooser.showOpenDialog(null);
      if (returnVal == JFileChooser.APPROVE_OPTION) {
        file = fileChooser.getSelectedFile();
      } else {
        file = null;
      }
    }

    // If a file was defined (No cancel in the dialog)
    if (file != null) {
      FilePreferences.getInstance().setDirectory(mimeType, file.getParent());

      boolean exist = file.exists();

      // check if file exists :
      if (!exist) {
        if (FileUtils.getExtension(file) == null) {
          // try using the same file name with extension :
          file = mimeType.checkFileExtension(file);
          // check again if that file exists :
          exist = file.exists();
        }
      }

      if (exist) {
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
      } else {
        MessagePane.showErrorMessage("Could not load the file : " + file.getAbsolutePath());
      }
    }
  }
}
