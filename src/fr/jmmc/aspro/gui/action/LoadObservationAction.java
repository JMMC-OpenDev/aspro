/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: LoadObservationAction.java,v 1.1 2009-12-04 16:26:58 bourgesl Exp $"
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
public class LoadObservationAction extends RegisteredAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  public final static String className = "fr.jmmc.aspro.gui.action.LoadObservationAction";
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "loadObservation";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);

  /* members */
  /** last directory used to save a file; by default = user home */
  private String lastDir = System.getProperty("user.home");

  public LoadObservationAction() {
    super(className, actionName);
  }

  public void actionPerformed(final ActionEvent e) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("actionPerformed");
    }

    final ObservationManager om = ObservationManager.getInstance();

    File file = null;

    JFileChooser fileChooser = new JFileChooser();

    if (this.lastDir != null) {
      fileChooser.setCurrentDirectory(new File(this.lastDir));
    }

    fileChooser.setDialogTitle("Load observation settings");

    final int returnVal = fileChooser.showOpenDialog(null);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      file = fileChooser.getSelectedFile();
    } else {
      file = null;
    }

    // If a file was defined (No cancel in the dialog)
    if (file != null) {
      this.lastDir = file.getParent();

      try {
        om.load(file);
        StatusBar.show("file loaded : " + file.getName());

      } catch (RuntimeException re) {
        logger.log(Level.SEVERE, "runtime failure : ", re);

        JOptionPane.showMessageDialog(null,
            "Could not load file " + file.getName(),
            "Error", JOptionPane.ERROR_MESSAGE);
      }

    }
  }
}
