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
import fr.jmmc.aspro.service.ObservabilityService;
import fr.jmmc.jmcs.gui.component.DismissableMessagePane;
import fr.jmmc.jmcs.gui.component.FileChooser;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.data.MimeType;
import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class implements the OB generation for VLTI instruments.
 *
 * @author bourgesl
 */
public final class ExportOBVLTIAction {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(ExportOBVLTIAction.class.getName());
    /** double formatter for min elevation */
    private final static NumberFormat DF1 = new DecimalFormat("0.#");
    /** OBX MimeType */
    private final static MimeType MIMETYPE = MimeType.OBX;
    /** action singleton */
    private static final ExportOBVLTIAction INSTANCE = new ExportOBVLTIAction();

    /**
     * Return the singleton instance
     * @return singleton instance
     */
    public static ExportOBVLTIAction getInstance() {
        return INSTANCE;
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
        logger.debug("process");

        if (targets.isEmpty()) {
            return;
        }

        final boolean exportAll = targets.size() > 1;

        File file;

        if (exportAll) {
            file = FileChooser.showDirectoryChooser("Export targets as Observing Blocks", null, MIMETYPE);
        } else {
            final Target target = targets.get(0);

            file = FileChooser.showSaveFileChooser("Export the target [" + target.getName() + "] as an Observing Block", null, MIMETYPE, ExportOBVLTI.generateOBFileName(target));
        }

        logger.debug("Selected file: {}", file);

        // If a file was defined (No cancel in the dialog)
        if (file != null) {
            final String directory = (exportAll) ? file.getPath() : file.getParent();

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

                final TargetUserInformations targetUserInfos = observation.getTargetUserInfos();

                if (exportAll) {
                    // report buffer :
                    sb.append("Observing Blocks exported for all targets with following settings:\n");
                    sb.append("  - minimum elevation set to ").append(DF1.format(minElev)).append(" deg\n");
                    sb.append("  - output folder :\n").append(directory).append("\n\n");

                    // Export all SCI/CAL OBs:
                    for (Target target : targets) {
                        file = new File(directory, ExportOBVLTI.generateOBFileName(target));

                        ExportOBVLTI.process(file, observation, os, target);

                        sb.append(file.getName()).append('\n');
                    }

                    // Export concatenation OBs:
                    for (Target target : targets) {
                        file = new File(directory, ExportOBVLTI.generateOBFileName(target));

                        final String conFileName = ExportOBVLTI.generateConcatenation(file, targetUserInfos, target);
                        if (conFileName != null) {
                            sb.append(conFileName).append('\n');
                        }
                    }

                    StatusBar.show("Observing blocks saved in " + directory + ".");

                } else {
                    final File mainFile = new File(directory, ExportOBVLTI.fixFileName(file.getName())
                            + '.' + MIMETYPE.getExtension());
                    final Target target = targets.get(0);

                    // report buffer :
                    sb.append("Observing Blocks exported for target [").append(target.getName()).append("] with following settings:\n");
                    sb.append("  - minimum elevation set to ").append(DF1.format(minElev)).append(" deg\n");
                    sb.append("  - output folder :\n").append(directory).append("\n\n");

                    ExportOBVLTI.process(mainFile, observation, os, target);

                    sb.append(mainFile.getName()).append('\n');

                    // Generate all calibrator OBs for a science target :
                    if (targetUserInfos != null && !targetUserInfos.isCalibrator(target)) {
                        final TargetInformation targetInfo = targetUserInfos.getTargetInformation(target);
                        if (targetInfo != null) {
                            final List<Target> calibrators = targetInfo.getCalibrators();

                            if (!calibrators.isEmpty()) {
                                for (Target calibrator : calibrators) {
                                    file = new File(directory, ExportOBVLTI.generateOBFileName(calibrator));

                                    ExportOBVLTI.process(file, observation, os, calibrator);
                                    sb.append(file.getName()).append('\n');
                                }

                                final String conFileName = ExportOBVLTI.generateConcatenation(mainFile, targetUserInfos, target);
                                if (conFileName != null) {
                                    sb.append(conFileName).append('\n');
                                }
                            }
                        }
                    }

                    StatusBar.show(mainFile.getName() + " created.");
                }

                // display report message :
                MessagePane.showMessage(sb.toString());

                // PoP up to validate OB file against ESO CfP :
                DismissableMessagePane.show("Please check that your observing blocks \n"
                        + "conform to the current ESO Call for Proposal \n (object magnitudes, instrument limits ...)",
                        Preferences.getInstance(), "ESO_OB_WARNING");

            } catch (IOException ioe) {
                MessagePane.showErrorMessage("Could not export to file : " + file.getAbsolutePath(), ioe);
            }
        }
    }
}
