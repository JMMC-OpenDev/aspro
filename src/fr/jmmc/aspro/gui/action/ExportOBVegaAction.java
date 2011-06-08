/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.FilePreferences;
import fr.jmmc.aspro.ob.ExportOBVega;
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.util.MimeType;
import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import javax.swing.JFileChooser;

/**
 * This class implements the OB generation for the CHARA Vega instrument.
 *
 * @author bourgesl
 */
public final class ExportOBVegaAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  public final static String className = "fr.jmmc.aspro.gui.action.ExportOBVegaAction";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);
  /** StarList MimeType */
  private final static MimeType mimeType = MimeType.STAR_LIST;
  /** action singleton */
  private static final ExportOBVegaAction instance = new ExportOBVegaAction();

  /**
   * Return the singleton ExportOBVegaAction instance
   * @return ExportOBVegaAction instance
   */
  public static ExportOBVegaAction getInstance() {
    return instance;
  }

  /**
   * Forbidden Constructor
   */
  private ExportOBVegaAction() {
    super();
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
    fileChooser.setFileFilter(mimeType.getFileFilter());
    fileChooser.setCurrentDirectory(FilePreferences.getInstance().getDirectoryFile(mimeType));

    // default VEGA_PLAN file name :
    fileChooser.setSelectedFile(new File(fileChooser.getCurrentDirectory(), getDefaultFileName()));

    fileChooser.setDialogTitle("Export targets as a Vega Star List");

    final int returnVal = fileChooser.showSaveDialog(null);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      file = mimeType.checkFileExtension(fileChooser.getSelectedFile());

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
      FilePreferences.getInstance().setDirectory(mimeType, file.getParent());

      try {
        ExportOBVega.process(file);

        StatusBar.show(file.getName() + " created.");

      } catch (IOException ioe) {
        MessagePane.showErrorMessage("Could not export to file : " + file.getAbsolutePath(), ioe);
      }
    }
  }

  /**
   * Generate a default name
   * @return default name [StarList_V01.txt]
   */
  private String getDefaultFileName() {
    return "StarList_V01." + mimeType.getExtension();
  }
}
