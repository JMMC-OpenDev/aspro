/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.FilePreferences;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.jmcs.gui.MessagePane;
import fr.jmmc.jmcs.gui.StatusBar;
import fr.jmmc.jmcs.util.MimeType;
import fr.jmmc.oitools.model.OIArray;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIFitsWriter;
import fr.jmmc.oitools.model.OIVis2;
import fr.nom.tam.fits.FitsException;
import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import javax.swing.JFileChooser;

/**
 * This registered action represents a File Menu entry to export an OIFits file
 * containing the visibilities of the selected target.
 * @author bourgesl
 */
public final class ExportOIFitsAction extends WaitingTaskAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  private final static String className = ExportOIFitsAction.class.getName();
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "exportOIFits";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);
  /** OIFits MimeType */
  private final static MimeType mimeType = MimeType.OIFITS;

  /**
   * Public constructor that automatically register the action in RegisteredAction.
   */
  public ExportOIFitsAction() {
    super(className, actionName);
  }

  /**
   * Handle the action event
   */
  @Override
  public void actionPerformed() {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("actionPerformed");
    }

    // Use main observation to check variants :
    if (!ObservationManager.getInstance().getMainObservation().isSingle()) {
      MessagePane.showMessage("Aspro 2 can not generate an OIFits file when multiple configurations are selected !");
      return;
    }

    final OIFitsFile oiFitsFile = ObservationManager.getInstance().getOIFitsFile();

    if (oiFitsFile != null) {

      File file = null;

      final JFileChooser fileChooser = new JFileChooser();
      fileChooser.setFileFilter(mimeType.getFileFilter());

      if (oiFitsFile.getAbsoluteFilePath() != null) {
        fileChooser.setSelectedFile(new File(oiFitsFile.getAbsoluteFilePath()));
      } else {
        fileChooser.setCurrentDirectory(FilePreferences.getInstance().getDirectoryFile(mimeType));
        fileChooser.setSelectedFile(new File(fileChooser.getCurrentDirectory(), getDefaultFileName(oiFitsFile)));
      }

      fileChooser.setDialogTitle("Export the current target as an OIFits file");

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
          OIFitsWriter.writeOIFits(file.getAbsolutePath(), oiFitsFile);

          StatusBar.show(file.getName() + " created.");

        } catch (FitsException fe) {
          MessagePane.showErrorMessage("Could not export to file : " + file.getAbsolutePath(), fe);
        } catch (IOException ioe) {
          MessagePane.showErrorMessage("Could not export to file : " + file.getAbsolutePath(), ioe);
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
  public static String getDefaultFileName(final OIFitsFile oiFitsFile) {

    final StringBuilder sb = new StringBuilder(32).append("Aspro2_");

    final String targetName = oiFitsFile.getOiTarget().getTarget()[0];
    final String altName = targetName.replaceAll(AsproConstants.REGEXP_INVALID_TEXT_CHARS, "_");

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

    sb.append('.').append(mimeType.getExtension());

    return sb.toString();
  }
}
