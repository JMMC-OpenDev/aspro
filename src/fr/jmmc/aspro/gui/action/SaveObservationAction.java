/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SaveObservationAction.java,v 1.4 2010-04-02 10:06:29 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.3  2009/12/08 13:09:55  bourgesl
 * Added FileFilter for observation settings
 *
 * Revision 1.2  2009/12/04 16:26:58  bourgesl
 * Added Load action in the menu bar (partially handled)
 *
 * Revision 1.1  2009/12/04 15:38:27  bourgesl
 * Added Save action in the menu bar
 *
 */
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.mcs.gui.StatusBar;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.logging.Level;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

/**
 * Save observation settings action
 * @author bourgesl
 */
public class SaveObservationAction extends ObservationFileAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  public final static String className = "fr.jmmc.aspro.gui.action.SaveObservationAction";
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "saveObservation";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);

  public SaveObservationAction() {
    super(className, actionName);
  }

  public void actionPerformed(final ActionEvent e) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("actionPerformed");
    }
    final ObservationManager om = ObservationManager.getInstance();

    File file = om.getObservationFile();

    final JFileChooser fileChooser = new JFileChooser();
    fileChooser.setFileFilter(getFileFilter());

    fileChooser.setSelectedFile(file);

    if (this.getLastDir() != null) {
      fileChooser.setCurrentDirectory(new File(this.getLastDir()));
    }

    fileChooser.setDialogTitle("Save the current observation settings");

    final int returnVal = fileChooser.showSaveDialog(null);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      file = fileChooser.getSelectedFile();
      if (file.exists()) {
        final int answer = JOptionPane.showConfirmDialog(null, "File \'" + file.getName() + "\' already exists\nDo you want to overwrite this file ?");
        if (answer != JOptionPane.YES_OPTION) {
          file = null;
        }
      }
    } else {
      file = null;
    }

    // If a file was defined (No cancel in the dialog)
    if (file != null) {
      this.setLastDir(file.getParent());

      try {
        file = checkFileExtension(file);

        om.save(file);

        StatusBar.show("file saved : " + file.getName());

      } catch (RuntimeException re) {
        logger.log(Level.SEVERE, "runtime failure : ", re);

        JOptionPane.showMessageDialog(null,
                "Could not save file " + file.getName(),
                "Error", JOptionPane.ERROR_MESSAGE);
      }

    }
  }
}
