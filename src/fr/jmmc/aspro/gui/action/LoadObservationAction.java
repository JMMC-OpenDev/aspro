/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: LoadObservationAction.java,v 1.5 2010-06-11 13:49:08 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.util.ActionRegistrar;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.logging.Level;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

/**
 * Save observation settings action
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
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);

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
