/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ExportOBVegaAction.java,v 1.10 2011-02-14 15:33:10 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.9  2010/12/15 13:34:35  bourgesl
 * removed warning
 *
 * Revision 1.8  2010/10/04 16:25:25  bourgesl
 * proper IO exception handling
 *
 * Revision 1.7  2010/10/01 15:32:28  bourgesl
 * use MessagePane.showConfirmFileOverwrite
 *
 * Revision 1.6  2010/09/24 15:54:25  bourgesl
 * better exception handling + use MessagePane
 *
 * Revision 1.5  2010/09/01 12:57:14  bourgesl
 * added runtime exception message to user message dialog
 *
 * Revision 1.4  2010/06/17 10:02:50  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.3  2010/06/10 13:43:42  bourgesl
 * fixed file description with extension
 *
 * Revision 1.2  2010/06/09 12:53:42  bourgesl
 * actionPerformed() method renamed to process() because it does not implement Action
 * javadoc
 *
 * Revision 1.1  2010/05/26 15:30:54  bourgesl
 * new CHARA Vega Star List generation (OB like)
 *
 *
 */
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.ob.ExportOBVega;
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.util.FileFilterRepository;
import fr.jmmc.mcs.util.FileUtils;
import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

/**
 * This class implements the OB generation for the CHARA Vega instrument.
 *
 * @author bourgesl
 */
public class ExportOBVegaAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  public final static String className = "fr.jmmc.aspro.gui.action.ExportOBVegaAction";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);
  /** PDF mime type */
  public static final String TXT_MIME_TYPE = "text/plain";
  /** PDF extension */
  public static final String TXT_EXT = "txt";
  /** action singleton */
  private static final ExportOBVegaAction instance = new ExportOBVegaAction();

  /**
   * Return the singleton ExportOBVegaAction instance
   * @return ExportOBVegaAction instance
   */
  public static ExportOBVegaAction getInstance() {
    return instance;
  }

  /* members */
  /** last directory used to save a file; by default = user home */
  private String lastDir = System.getProperty("user.home");

  /**
   * Forbidden Constructor
   */
  private ExportOBVegaAction() {
    super();

    FileFilterRepository.getInstance().put(TXT_MIME_TYPE, TXT_EXT, "Star List (" + TXT_EXT + ")");
  }

  /**
   * Execute the action.
   */
  public void process() {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("process");
    }

    File file = null;

    final JFileChooser fileChooser = new JFileChooser();
    fileChooser.setFileFilter(getFileFilter());

    if (this.getLastDir() != null) {
      fileChooser.setCurrentDirectory(new File(this.getLastDir()));
    }

    // default VEGA_PLAN file name :
    fileChooser.setSelectedFile(new File(fileChooser.getCurrentDirectory(), "StarList_V01." + TXT_EXT));

    fileChooser.setDialogTitle("Export targets as a Vega Star List");

    final int returnVal = fileChooser.showSaveDialog(null);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      file = checkFileExtension(fileChooser.getSelectedFile());

      if (file.exists()) {
        if (!MessagePane.showConfirmFileOverwrite(file.getName())) {
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
        ExportOBVega.process(file);

        StatusBar.show(file.getName() + " created.");

      } catch (IOException ioe) {
        MessagePane.showErrorMessage(
                "Could not export to file : " + file.getName(), ioe);
      }
    }
  }

  /**
   * Return the file filter
   * @return file filter
   */
  protected FileFilter getFileFilter() {
    return FileFilterRepository.get(TXT_MIME_TYPE);
  }

  /**
   * Check if the given file has the correct extension. If not, return a new file with it
   * @param file file to check
   * @return given file or new file with the correct extension
   */
  protected File checkFileExtension(final File file) {
    final String ext = FileUtils.getExtension(file);

    if (!TXT_EXT.equals(ext)) {
      return new File(file.getParentFile(), file.getName() + "." + TXT_EXT);
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
