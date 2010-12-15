/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ExportOIFitsAction.java,v 1.9 2010-12-15 13:34:22 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.8  2010/12/13 16:39:39  mella
 * use new enum MimeType.OIFITS
 *
 * Revision 1.7  2010/10/22 13:45:45  bourgesl
 * fixed repackaging of nom.tam.fits library (oitools)
 *
 * Revision 1.6  2010/10/04 14:32:13  bourgesl
 * getDefaultFileName(fits) is now protected and static (used by BroadcastToMFGui)
 *
 * Revision 1.5  2010/10/01 15:33:11  bourgesl
 * added message 'There is currently no OIFits data (your target is not observable)'
 * use MessagePane.showConfirmFileOverwrite
 *
 * Revision 1.4  2010/09/26 11:59:39  bourgesl
 * catch correct exceptions
 *
 * Revision 1.3  2010/09/24 15:54:25  bourgesl
 * better exception handling + use MessagePane
 *
 * Revision 1.2  2010/09/02 15:47:19  bourgesl
 * use OI_VIS2 (always present)
 *
 * Revision 1.1  2010/06/29 12:13:21  bourgesl
 * added ExportToOIFits action
 *
 *
 */
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.util.FileUtils;
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.util.MimeType;
import fr.jmmc.mcs.util.RegisteredAction;
import fr.jmmc.oitools.model.OIArray;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIFitsWriter;
import fr.jmmc.oitools.model.OIVis2;
import fr.nom.tam.fits.FitsException;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

/**
 * This registered action represents a File Menu entry to export an OIFits file
 * containing the visibilities of the selected target.
 * @author bourgesl
 */
public class ExportOIFitsAction extends RegisteredAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  private final static String className = "fr.jmmc.aspro.gui.action.ExportOIFitsAction";
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "exportOIFits";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);
 
  /* members */
  /** last directory used to save a file; by default = user home */
  private String lastDir = System.getProperty("user.home");

  /**
   * Public constructor that automatically register the action in RegisteredAction.
   */
  public ExportOIFitsAction() {
    super(className, actionName);  
  }

  /**
   * Handle the action event
   * @param evt action event
   */
  public void actionPerformed(final ActionEvent evt) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("actionPerformed");
    }
    final OIFitsFile oiFitsFile = ObservationManager.getInstance().getObservation().getOIFitsFile();

    if (oiFitsFile != null) {

      File file = null;

      final JFileChooser fileChooser = new JFileChooser();
      fileChooser.setFileFilter(getFileFilter());

      fileChooser.setSelectedFile(file);

      if (oiFitsFile.getAbsoluteFilePath() != null) {
        fileChooser.setSelectedFile(new File(oiFitsFile.getAbsoluteFilePath()));
      } else {
        if (this.getLastDir() != null) {
          fileChooser.setCurrentDirectory(new File(this.getLastDir()));
        }

        fileChooser.setSelectedFile(new File(fileChooser.getCurrentDirectory(), getDefaultFileName(oiFitsFile)));
      }

      fileChooser.setDialogTitle("Export the current target as an OIFits file");

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
          OIFitsWriter.writeOIFits(file.getAbsolutePath(), oiFitsFile);

          StatusBar.show(file.getName() + " created.");

        } catch (FitsException fe) {
          MessagePane.showErrorMessage(
                  "Could not export to file : " + file.getName(), fe);
        } catch (IOException ioe) {
          MessagePane.showErrorMessage(
                  "Could not export to file : " + file.getName(), ioe);
        }
      }
    } else {
      MessagePane.showMessage("There is currently no OIFits data (your target is not observable)");
    }
  }

  /**
   * Generate a default name for the given OIFits structure
   * @param oiFitsFile OIFits structure
   * @return default name [Aspro2_<TARGET>_<INSTRUMENT>_<CONFIGURATION>_<DATE>]
   */
  protected static String getDefaultFileName(final OIFitsFile oiFitsFile) {

    final StringBuilder sb = new StringBuilder(32).append("Aspro2_");

    final String targetName = oiFitsFile.getOiTarget().getTarget()[0];
    final String altName = targetName.replaceAll("[^a-zA-Z_0-9]", "_");

    sb.append(altName).append('_');

    final OIVis2 vis2 = oiFitsFile.getOiVis2()[0];

    final String arrayName = vis2.getArrName();
    final String insName = vis2.getInsName();

    sb.append(insName).append('_');

    final OIArray array = oiFitsFile.getOiArray(arrayName);
    for (String station : array.getStaName()) {
      sb.append(station).append('-');
    }
    sb.deleteCharAt(sb.length() - 1);
    sb.append('_');

    final String dateObs = vis2.getDateObs();

    sb.append(dateObs);

    sb.append('.').append(MimeType.OIFITS.getExtension());

    return sb.toString();
  }

  /**
   * Return the file filter
   * @return file filter
   */
  protected FileFilter getFileFilter() {
    return MimeType.OIFITS.getFileFilter();
  }

  /**
   * Check if the given file has the correct extension. If not, return a new file with it
   * @param file file to check
   * @return given file or new file with the correct extension
   */
  protected File checkFileExtension(final File file) {
    final String ext = FileUtils.getExtension(file);
    final String oifitsExt = MimeType.OIFITS.getExtension();
    if (!oifitsExt.equals(ext)) {
      return new File(file.getParentFile(), file.getName() + "." + oifitsExt);
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
