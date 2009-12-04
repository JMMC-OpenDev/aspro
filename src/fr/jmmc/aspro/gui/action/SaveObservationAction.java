/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SaveObservationAction.java,v 1.1 2009-12-04 15:38:27 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.util.RegisteredAction;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.logging.Level;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

/**
 * Save observation settings action
 * @author bourgesl
 */
public class SaveObservationAction extends RegisteredAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  public final static String className = "fr.jmmc.aspro.gui.action.SaveObservationAction";
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "saveObservation";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);

  /* members */
  /** last directory used to save a file; by default = user home */
  private String lastDir = System.getProperty("user.home");

  public SaveObservationAction() {
    super(className, actionName);
  }

  public void actionPerformed(final ActionEvent e) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("actionPerformed");
    }

    final ObservationManager om = ObservationManager.getInstance();

    File file = om.getObservationFile();

    JFileChooser fileChooser = new JFileChooser();
    fileChooser.setSelectedFile(file);
    if (this.lastDir != null) {
      fileChooser.setCurrentDirectory(new File(this.lastDir));
    }

    fileChooser.setDialogTitle("Save the current observation settings to a file ...");

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
      this.lastDir = file.getParent();

      try {
        om.save(file);
      } catch (RuntimeException re) {
        logger.log(Level.SEVERE, "runtime failure : ", re);
      }

      StatusBar.show("file saved : " + file.getName());
    }
  }
}
