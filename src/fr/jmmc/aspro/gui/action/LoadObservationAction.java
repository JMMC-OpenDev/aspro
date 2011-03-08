/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: LoadObservationAction.java,v 1.12 2011-03-08 13:50:18 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.11  2011/03/04 16:57:48  bourgesl
 * added comment
 *
 * Revision 1.10  2010/10/04 16:25:39  bourgesl
 * proper JAXB / IO exception handling
 *
 * Revision 1.9  2010/09/24 15:54:25  bourgesl
 * better exception handling + use MessagePane
 *
 * Revision 1.8  2010/09/01 12:57:13  bourgesl
 * added runtime exception message to user message dialog
 *
 * Revision 1.7  2010/07/07 15:11:51  bourgesl
 * fixed comment
 *
 * Revision 1.6  2010/06/17 10:02:50  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.5  2010/06/11 13:49:08  bourgesl
 * flagged as open action
 * handle the action event's file coming from ActionRegistar (open action on application startup)
 *
 * Revision 1.4  2010/06/09 12:54:12  bourgesl
 * javadoc
 *
 * Revision 1.3  2010/04/02 10:06:29  bourgesl
 * add missing xml extension to the file name
 *
 * Revision 1.2  2009/12/08 13:09:55  bourgesl
 * Added FileFilter for observation settings
 *
 * Revision 1.1  2009/12/04 16:26:58  bourgesl
 * Added Load action in the menu bar (partially handled)
 *
 */
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
