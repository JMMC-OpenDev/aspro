/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetInformation;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.ob.ExportOBVLTI;
import fr.jmmc.mcs.gui.DismissableMessagePane;
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.util.FileFilterRepository;
import fr.jmmc.mcs.util.FileUtils;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.logging.Level;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

/**
 * This class implements the OB generation for VLTI instruments.
 *
 * @author bourgesl
 */
public final class ExportOBVLTIAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  public final static String className = "fr.jmmc.aspro.gui.action.ExportOBVLTIAction";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);
  /** OBX mime type */
  public static final String OBX_MIME_TYPE = "application/obx";
  /** OBX extension */
  public static final String OBX_EXT = "obx";
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

  /* members */
  /** last directory used to save a file; by default = user home */
  private String lastDir = System.getProperty("user.home");

  /**
   * Forbidden Constructor
   */
  private ExportOBVLTIAction() {
    super();

    FileFilterRepository.getInstance().put(OBX_MIME_TYPE, OBX_EXT, "Observing Block (" + OBX_EXT + ")");
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

    if (this.getLastDir() != null) {
      fileChooser.setCurrentDirectory(new File(this.getLastDir()));
    }

    if (exportAll) {
      // select one directory:
      fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

      fileChooser.setDialogTitle("Export targets as Observing Blocks");

    } else {
      // select one file:
      fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
      fileChooser.setFileFilter(getFileFilter());

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
          file.mkdirs();
        }
      } else {
        file = checkFileExtension(fileChooser.getSelectedFile());

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
      this.setLastDir((exportAll) ? file.getPath() : file.getParent());

      // report buffer :
      final StringBuilder sb = new StringBuilder(1024);

      // use main observation :
      final ObservationSetting observation = ObservationManager.getInstance().getMainObservation();

      try {
        
        if (exportAll) {

          // report buffer :
          sb.append("Export Observing Blocks for all targets");
          sb.append(" in folder :\n").append(getLastDir()).append("\n\n");

          for (Target target : targets) {

            file = new File(getLastDir(), ExportOBVLTI.generateOBFileName(target));
            
            ExportOBVLTI.process(file, observation, target);

            sb.append(file.getName()).append("\n");
          }

          StatusBar.show("Observing blocks saved in " + getLastDir() + ".");          
          
        } else {
          final File mainFile = file;
          final Target target = targets.get(0);
           
          // report buffer :
          sb.append("Export Observing Blocks for target [").append(target.getName());
          sb.append("] in folder :\n").append(getLastDir()).append("\n\n");

          ExportOBVLTI.process(mainFile, observation, target);
          
          sb.append(mainFile.getName()).append("\n");

          // Generate all calibrator OBs for a science target :
          final TargetUserInformations targetUserInfos = observation.getTargetUserInfos();

          if (targetUserInfos != null && !targetUserInfos.isCalibrator(target)) {
            final TargetInformation targetInfo = targetUserInfos.getTargetInformation(target);
            if (targetInfo != null) {
              final List<Target> calibrators = targetInfo.getCalibrators();

              if (!calibrators.isEmpty()) {
                for (Target calibrator : calibrators) {
                  file = new File(getLastDir(), ExportOBVLTI.generateOBFileName(calibrator));

                  ExportOBVLTI.process(file, observation, calibrator);
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

  /**
   * Return the file filter
   * @return file filter
   */
  protected FileFilter getFileFilter() {
    return FileFilterRepository.get(OBX_MIME_TYPE);
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
