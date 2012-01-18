/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.FilePreferences;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetInformation;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.ob.ExportOBVLTI;
import fr.jmmc.aspro.service.ObservabilityService;
import fr.jmmc.jmcs.gui.DismissableMessagePane;
import fr.jmmc.jmcs.gui.MessagePane;
import fr.jmmc.jmcs.gui.StatusBar;
import fr.jmmc.jmcs.util.MimeType;
import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.List;
import java.util.logging.Level;
import javax.swing.JFileChooser;

/**
 * This class implements the OB generation for VLTI instruments.
 *
 * @author bourgesl
 */
public final class ExportOBVLTIAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(ExportOBVLTIAction.class.getName());
  /** double formatter for min elevation */
  protected final static NumberFormat df1 = new DecimalFormat("0.#");
  /** OBX MimeType */
  private final static MimeType mimeType = MimeType.OBX;
  /** Eso warning message */
  public static final String ESO_WARNING = "Please check that your observing blocks \n conform to the current ESO Call for Proposal \n (object magnitudes, instrument limits ...)";
  /** action singleton */
  private static final ExportOBVLTIAction instance = new ExportOBVLTIAction();

  /**
   * Return the singleton ExportOBVLTIAction instance
   * @return ExportOBVLTIAction instance
   */
  public static ExportOBVLTIAction getInstance() {
    return instance;
  }

  /**
   * Forbidden Constructor
   */
  private ExportOBVLTIAction() {
    super();
  }

  /**
   * Execute the action.
   *
   * @param targets list of targets to export as VLTI Observing blocks
   */
  public void process(final List<Target> targets) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("process");
    }
    if (targets.isEmpty()) {
      return;
    }

    final boolean exportAll = targets.size() > 1;

    File file = null;

    final JFileChooser fileChooser = new JFileChooser();
    fileChooser.setCurrentDirectory(FilePreferences.getInstance().getDirectoryFile(mimeType));

    if (exportAll) {
      // select one directory:
      fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

      fileChooser.setDialogTitle("Export targets as Observing Blocks");

    } else {
      // select one file:
      fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
      fileChooser.setFileFilter(mimeType.getFileFilter());

      final Target target = targets.get(0);

      // default OB file name :
      fileChooser.setSelectedFile(new File(fileChooser.getCurrentDirectory(), ExportOBVLTI.generateOBFileName(target)));

      fileChooser.setDialogTitle("Export the target [" + target.getName() + "] as an Observing Block");
    }

    final int returnVal = fileChooser.showSaveDialog(null);

    if (returnVal == JFileChooser.APPROVE_OPTION) {

      if (exportAll) {
        file = fileChooser.getSelectedFile();
        if (!file.exists()) {
          if (!file.mkdirs()) {
            MessagePane.showErrorMessage("Could not create directory: " + file.getAbsolutePath());
            file = null;
          }
        }
      } else {
        file = mimeType.checkFileExtension(fileChooser.getSelectedFile());

        if (file.exists()) {
          if (!MessagePane.showConfirmFileOverwrite(file.getName())) {
            file = null;
          }
        }
      }
    } else {
      file = null;
    }

    // If a file was defined (No cancel in the dialog)
    if (file != null) {
      final String directory = (exportAll) ? file.getPath() : file.getParent();
      
      FilePreferences.getInstance().setDirectory(mimeType, directory);

      // report buffer :
      final StringBuilder sb = new StringBuilder(1024);

      // use main observation :
      final ObservationSetting observation = ObservationManager.getInstance().getMainObservation();
      final double minElev = observation.getInterferometerConfiguration().getMinElevation();
      
      try {

        // Compute Observability data using astronomical night (-18 deg)
        // (date and night restrictions depend on the current observation) :
        final ObservabilityService os = new ObservabilityService(observation);
        
        // compute observability data:
        os.compute();
        
        if (exportAll) {

          // report buffer :
          sb.append("Observing Blocks exported for all targets with following settings:\n");
          sb.append("  - minimum elevation set to ").append(df1.format(minElev)).append(" deg\n");
          sb.append("  - output folder :\n").append(directory).append("\n\n");

          for (Target target : targets) {

            file = new File(directory, ExportOBVLTI.generateOBFileName(target));
            
            ExportOBVLTI.process(file, observation, os, target);

            sb.append(file.getName()).append("\n");
          }

          StatusBar.show("Observing blocks saved in " + directory + ".");          
          
        } else {
          final File mainFile = file;
          final Target target = targets.get(0);
           
          // report buffer :
          sb.append("Observing Blocks exported for target [").append(target.getName()).append("] with following settings:\n");
          sb.append("  - minimum elevation set to ").append(df1.format(minElev)).append(" deg\n");
          sb.append("  - output folder :\n").append(directory).append("\n\n");

          ExportOBVLTI.process(mainFile, observation, os, target);
          
          sb.append(mainFile.getName()).append("\n");

          // Generate all calibrator OBs for a science target :
          final TargetUserInformations targetUserInfos = observation.getTargetUserInfos();

          if (targetUserInfos != null && !targetUserInfos.isCalibrator(target)) {
            final TargetInformation targetInfo = targetUserInfos.getTargetInformation(target);
            if (targetInfo != null) {
              final List<Target> calibrators = targetInfo.getCalibrators();

              if (!calibrators.isEmpty()) {
                for (Target calibrator : calibrators) {
                  file = new File(directory, ExportOBVLTI.generateOBFileName(calibrator));

                  ExportOBVLTI.process(file, observation, os, calibrator);
                  sb.append(file.getName()).append("\n");
                }
              }
            }
          }

          StatusBar.show(mainFile.getName() + " created.");
        }

        // display report message :
        MessagePane.showMessage(sb.toString());

        // PoP up to validate OB file against ESO CfP :
        DismissableMessagePane.show(ESO_WARNING, Preferences.getInstance(), "ESO_OB_WARNING");

      } catch (IOException ioe) {
        MessagePane.showErrorMessage("Could not export to file : " + file.getAbsolutePath(), ioe);
      }
    }
  }
}
