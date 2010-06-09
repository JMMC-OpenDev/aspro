/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ExportOBVLTIAction.java,v 1.6 2010-06-09 12:53:42 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.5  2010/05/26 15:29:14  bourgesl
 * light refactoring and javadoc
 *
 * Revision 1.4  2010/05/11 12:03:17  bourgesl
 * fix : check the file extension before the existence of the file to display the confirm dialog
 *
 * Revision 1.3  2010/05/06 15:42:18  bourgesl
 * use HA Min/Max + FT Mode for the target in the observation settings
 *
 * Revision 1.2  2010/05/05 14:29:09  bourgesl
 * added ha Min / Max to generate OB with correct LST intervals
 *
 * Revision 1.1  2010/04/14 13:09:23  bourgesl
 * renamed Amber action to VLTI action (AMBER + MIDI)
 *
 * Revision 1.2  2010/04/13 15:54:48  bourgesl
 * javadoc
 *
 * Revision 1.1  2010/04/02 10:07:35  bourgesl
 * simple OB generation for AMBER
 *
 */
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.gui.UVCoveragePanel;
import fr.jmmc.aspro.ob.ExportOBVLTI;
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
 * This class implements the OB generation for VLTI instruments.
 *
 * @author bourgesl
 */
public class ExportOBVLTIAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  public final static String className = "fr.jmmc.aspro.gui.action.ExportOBVLTIAction";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);
  /** OBX mime type */
  public static final String OBX_MIME_TYPE = "application/obx";
  /** OBX extension */
  public static final String OBX_EXT = "obx";
  /** action singleton */
  private static final ExportOBVLTIAction instance = new ExportOBVLTIAction();

  /**
   * Return the singleton ExportOBVLTIAction instance
   * @return ExportOBVLTIAction instance
   */
  public static ExportOBVLTIAction getInstance() {
    return instance;
  }

  /* members */
  /** last directory used to save a file; by default = user home */
  private String lastDir = System.getProperty("user.home");

  /**
   * Forbidden Constructor
   */
  private ExportOBVLTIAction() {
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
  public void process(final ActionEvent event) {
    if (!(event.getSource() instanceof UVCoveragePanel)) {
      return;
    }
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("process");
    }

    final UVCoveragePanel uvCoveragePanel = (UVCoveragePanel) event.getSource();

    // extract UV Coverage Panel information :
    final String targetName = uvCoveragePanel.getSelectedTargetName();

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
      file = checkFileExtension(fileChooser.getSelectedFile());

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
        ExportOBVLTI.process(file, targetName);

        StatusBar.show(file.getName() + " created.");

      } catch (RuntimeException re) {
        logger.log(Level.SEVERE, "runtime failure : ", re);

        JOptionPane.showMessageDialog(null,
                "Could not export to file " + file.getName() + "\n" + re.getMessage(),
                "Error", JOptionPane.ERROR_MESSAGE);
      }
    }
  }

  /**
   * Return the file filter
   * @return file filter
   */
  protected FileFilter getFileFilter() {
    return FileFilterRepository.getInstance().get(OBX_MIME_TYPE);
  }

  /**
   * Check if the given file has the correct extension. If not, return a new file with it
   * @param file file to check
   * @return given file or new file with the correct extension
   */
  protected File checkFileExtension(final File file) {
    final String ext = FileUtils.getExtension(file);

    if (!OBX_EXT.equals(ext)) {
      return new File(file.getParentFile(), file.getName() + "." + OBX_EXT);
    }
    return file;
  }

  /**
   * Return the last directory used
   * @return last directory used
   */
  protected String getLastDir() {
    return this.lastDir;
  }

  /**
   * Define the last directory used
   * @param lastDir new value
   */
  protected void setLastDir(String lastDir) {
    this.lastDir = lastDir;
  }
}
