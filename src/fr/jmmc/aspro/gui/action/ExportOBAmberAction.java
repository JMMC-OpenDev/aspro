/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ExportOBAmberAction.java,v 1.2 2010-04-13 15:54:48 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/04/02 10:07:35  bourgesl
 * simple OB generation for AMBER
 *
 */
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.gui.UVCoveragePanel;
import fr.jmmc.aspro.ob.ExportOBAmber;
import fr.jmmc.aspro.util.FileUtils;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.util.FileFilterRepository;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.logging.Level;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileFilter;

/**
 * This class implement the export to VLTI AMBER OB action.
 *
 * TODO : use the standard MCS action to add it in the menu.
 *
 * @author bourgesl
 */
public class ExportOBAmberAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  public final static String className = "fr.jmmc.aspro.gui.action.ExportOBAction";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);
  /** PDF mime type */
  public static final String OBX_MIME_TYPE = "application/obx";
  /** PDF extension */
  public static final String OBX_EXT = "obx";
  /** action singleton */
  private static ExportOBAmberAction instance = new ExportOBAmberAction();

  /**
   * Return the singleton ExportOBAmberAction instance
   * @return ExportOBAmberAction instance
   */
  public static ExportOBAmberAction getInstance() {
    return instance;
  }

  /* members */
  /** last directory used to save a file; by default = user home */
  private String lastDir = System.getProperty("user.home");

  /**
   * Forbidden Constructor
   */
  private ExportOBAmberAction() {
    super();

    FileFilterRepository.getInstance().put(OBX_MIME_TYPE, OBX_EXT, "Observing Block (OB)");
  }

  /**
   * Execute the action.
   *
   * Note : the action event's source must be the UVCoveragePanel instance
   *
   * @param event action event
   */
  public void actionPerformed(final ActionEvent event) {
    if (!(event.getSource() instanceof UVCoveragePanel)) {
      return;
    }
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("actionPerformed");
    }

    final String targetName = ((UVCoveragePanel) event.getSource()).getSelectedTargetName();

    File file = null;

    final JFileChooser fileChooser = new JFileChooser();
    fileChooser.setFileFilter(getFileFilter());

    if (this.getLastDir() != null) {
      fileChooser.setCurrentDirectory(new File(this.getLastDir()));
    }

    // default P2PP file name :
    // replace invalid characters :
    final String altName = targetName.replaceAll("[^a-zA-Z_0-9]", "_");
    fileChooser.setSelectedFile(new File(fileChooser.getCurrentDirectory(), "SCI_" + altName + "." + OBX_EXT));

    fileChooser.setDialogTitle("Export the target [" + targetName + "] as an Observing Block");

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

        ExportOBAmber.process(targetName, file);

        StatusBar.show(file.getName() + " created.");

      } catch (RuntimeException re) {
        logger.log(Level.SEVERE, "runtime failure : ", re);

        JOptionPane.showMessageDialog(null,
                "Could not export to file " + file.getName(),
                "Error", JOptionPane.ERROR_MESSAGE);
      }
    }
  }

  protected FileFilter getFileFilter() {
    return FileFilterRepository.getInstance().get(OBX_MIME_TYPE);
  }

  protected File checkFileExtension(final File file) {
    final String ext = FileUtils.getExtension(file);

    if (!OBX_EXT.equals(ext)) {
      return new File(file.getParentFile(), file.getName() + "." + OBX_EXT);
    }
    return file;
  }

  public String getLastDir() {
    return lastDir;
  }

  public void setLastDir(String lastDir) {
    this.lastDir = lastDir;
  }
}
