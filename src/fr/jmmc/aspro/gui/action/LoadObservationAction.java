/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.util.ActionRegistrar;
import fr.jmmc.mcs.util.FileUtils;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import javax.swing.JFileChooser;

/**
 * Open observation settings action
 * @author bourgesl
 */
public class LoadObservationAction extends ObservationFileAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  public final static String className = "fr.jmmc.aspro.gui.action.LoadObservationAction";
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "loadObservation";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);

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
  public void actionPerformed(final ActionEvent evt) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("actionPerformed");
    }
    final ObservationManager om = ObservationManager.getInstance();

    File file = null;

    // If the action was automatically triggered from App launch
    if (evt.getSource() == ActionRegistrar.getInstance()) {
      file = new File(evt.getActionCommand());
    } else {

      final JFileChooser fileChooser = new JFileChooser();
      fileChooser.setFileFilter(getFileFilter());

      if (this.getLastDir() != null) {
        fileChooser.setCurrentDirectory(new File(this.getLastDir()));
      }

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
      this.setLastDir(file.getParent());

      boolean exist = file.exists();

      // check if file exists :
      if (!exist) {
        if (FileUtils.getExtension(file) == null) {
          // try using the same file name with extension :
          file = checkFileExtension(file);
          // check again if that file exists :
          exist = file.exists();
        }
      }

      if (exist) {
        try {
          om.load(file);

          StatusBar.show("file loaded : " + file.getName());

        } catch (IllegalArgumentException iae) {
          MessagePane.showErrorMessage(
                  "Invalid observation file : " + file.getAbsolutePath(), iae);
        } catch (IOException ioe) {
          MessagePane.showErrorMessage(
                  "Could not load the file : " + file.getAbsolutePath(), ioe);
        }
      } else {
        MessagePane.showErrorMessage(
                "Could not load the file : " + file.getAbsolutePath());
      }
    }
  }
}
