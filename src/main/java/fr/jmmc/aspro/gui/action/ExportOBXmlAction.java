/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.ob.ExportOBXml;
import fr.jmmc.aspro.service.ObservabilityService;
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
 * This class implements the generic OB generation as XML document.
 *
 * @author bourgesl
 */
public final class ExportOBXmlAction {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(ExportOBXmlAction.class.getName());
    /** double formatter for min elevation */
    private final static NumberFormat DF1 = new DecimalFormat("0.#");
    /** OBXML MimeType */
    private final static MimeType MIMETYPE = MimeType.ASPRO_OB_XML;
    /** action singleton */
    private static final ExportOBXmlAction INSTANCE = new ExportOBXmlAction();

    /**
     * Return the singleton instance
     * @return singleton instance
     */
    public static ExportOBXmlAction getInstance() {
        return INSTANCE;
    }

    /**
     * Forbidden Constructor
     */
    private ExportOBXmlAction() {
        super();
    }

    /**
     * Execute the action.
     *
     * @param targets list of targets to export as Observing blocks
     */
    public void process(final List<Target> targets) {
        logger.debug("process");

        if (targets.isEmpty()) {
            return;
        }

        final boolean exportAll = targets.size() > 1;

        File file;

        if (exportAll) {
            file = FileChooser.showDirectoryChooser("Export targets as Observing Blocks (XML)", null, MIMETYPE);
        } else {
            final Target target = targets.get(0);

            file = FileChooser.showSaveFileChooser("Export the target [" + target.getName() + "] as an Observing Block (XML)", null, MIMETYPE, ExportOBXml.generateOBFileName(target));
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
                // Compute Observability data using astronomical night (-18 deg) without night restrictions :
                final ObservabilityService os = ExportOBXml.processObservability(observation);
                
                final TargetUserInformations targetUserInfos = observation.getTargetUserInfos();

                if (exportAll) {
                    // report buffer :
                    sb.append("Observing Blocks exported for all targets with following settings:\n");
                    sb.append("  - minimum elevation set to ").append(DF1.format(minElev)).append(" deg\n");
                    sb.append("  - output folder :\n").append(directory).append("\n\n");

                    // Export all SCI/CAL OBs:
                    for (Target target : targets) {
                        // TODO: generate a single document gathering all targets ?
                        
                        // generate one XML document per science OB:
                        if (targetUserInfos != null && !targetUserInfos.isCalibrator(target)) {
                            file = new File(directory, ExportOBXml.generateOBFileName(target));

                            ExportOBXml.process(file, observation, os, target);

                            sb.append(file.getName()).append('\n');
                        }
                    }

                    StatusBar.show("Observing blocks saved in " + directory + ".");

                } else {
                    final File mainFile = new File(directory, file.getName());

                    final Target target = targets.get(0);

                    // report buffer :
                    sb.append("Observing Blocks exported for target [").append(target.getName()).append("] with following settings:\n");
                    sb.append("  - minimum elevation set to ").append(DF1.format(minElev)).append(" deg\n");
                    sb.append("  - output folder :\n").append(directory).append("\n\n");

                    ExportOBXml.process(mainFile, observation, os, target);

                    sb.append(mainFile.getName()).append('\n');

                    StatusBar.show(mainFile.getName() + " created.");
                }

                // display report message :
                MessagePane.showMessage(sb.toString());

            } catch (IOException ioe) {
                MessagePane.showErrorMessage("Could not export to file : " + file.getAbsolutePath(), ioe);
            }
        }
    }
}
