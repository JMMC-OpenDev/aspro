/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.interop;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.gui.TargetEditorDialog;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.TargetImporter;
import fr.jmmc.aspro.model.oi.BaseValue;
import fr.jmmc.aspro.model.oi.CalibratorInformations;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.StringValue;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.jmal.ALX;
import fr.jmmc.jmcs.App;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.service.XslTransform;
import java.io.IOException;
import java.io.StringReader;
import java.util.Iterator;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class handles SearchCal VOTables
 * 
 * @author bourgesl
 */
public final class SearchCalVOTableHandler {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(SearchCalVOTableHandler.class.getName());
    /** science object distance in degrees */
    public static final double SCIENCE_DETECTION_DISTANCE = 1d * ALX.ARCSEC_IN_DEGREES;
    /** XSLT file path */
    private final static String XSLT_FILE = "fr/jmmc/aspro/interop/scvot2AsproObservation.xsl";
    /** GetStar constant */
    private final static String GET_STAR_NAME = "GetStar";

    /**
     * Private constructor
     */
    private SearchCalVOTableHandler() {
        super();
    }

    /**
     * Process the given votable
     *
     * @param votable votable to process
     * @param searchCalVersion SearchCal GUI version
     * @return true if the votable was processed i.e. contains proper SearchCal data
     * 
     * @throws IOException if an I/O exception occured
     * @throws IllegalArgumentException if the file is not an Observation
     */
    static boolean processMessage(final String votable, final String searchCalVersion) throws IOException {

        // use an XSLT to transform the SearchCal votable document to an Aspro 2 Observation:
        final long start = System.nanoTime();

        final String document = XslTransform.transform(votable, XSLT_FILE).trim();

        logger.info("VOTable transformation (XSLT) duration = {} ms.", 1e-6d * (System.nanoTime() - start));

        if (document.length() == 0) {
            logger.debug("document is empty (probably not a SearchCal VOTable)");

            return false;
        }
        logger.debug("document:\n{}", document);

        final ObservationManager om = ObservationManager.getInstance();

        try {
            final ObservationSetting searchCalObservation = om.load(new StringReader(document));

            // check GetStar / SearchCal command:
            final String cmdName = searchCalObservation.getName();
            logger.debug("cmdName: {}", cmdName);

            if (!GET_STAR_NAME.equalsIgnoreCase(cmdName)) {
                // SearchCal case:
                final List<Target> calibrators = searchCalObservation.getTargets();

                final Target scienceTarget;

                if (calibrators.isEmpty()) {
                    scienceTarget = null;
                } else {
                    // first target is the science target (partial information only):
                    scienceTarget = calibrators.remove(0);

                    // format the target name:
                    scienceTarget.updateNameAndIdentifier();

                    logger.debug("science target: {}", scienceTarget);

                    if (logger.isDebugEnabled()) {
                        logger.debug("calibrators:");
                        for (Target cal : calibrators) {
                            logger.debug(cal.toString());
                        }
                    }

                    // Mimic SearchCal science object detection distance preference ("query.SCIENCE_DETECTION_DISTANCE")
                    // @note SCIENCE_DISTANCE_CHECK : filter science target if distance is less than science object detection distance preference (1 arcsec):
                    for (Iterator<Target> it = calibrators.iterator(); it.hasNext();) {
                        final Target cal = it.next();

                        final BaseValue dist = cal.getCalibratorInfos().getField(CalibratorInformations.FIELD_DISTANCE);

                        if (dist != null) {
                            final double rowDistance = dist.getNumber().doubleValue();

                            // If the distance is close enough to be detected as a science object
                            if (rowDistance < SCIENCE_DETECTION_DISTANCE) {
                                if (logger.isInfoEnabled()) {
                                    logger.info("calibrator distance is [{}] - skip this calibrator considered as science object: {} - IDS = {}",
                                            rowDistance, cal, cal.getIDS());
                                }
                                it.remove();

                                // reuse science target data to update scienceTarget object:
                                Target.merge(scienceTarget, cal);
                            }
                        }
                    }

                    // Add the SearchCalGuiVersion parameter to calibrators if missing:
                    final StringValue paramSearchCalVersion = new StringValue();
                    paramSearchCalVersion.setName(CalibratorInformations.PARAMETER_SCL_GUI_VERSION);
                    paramSearchCalVersion.setValue(searchCalVersion);

                    for (Target cal : calibrators) {
                        if (cal.getCalibratorInfos().getParameter(CalibratorInformations.PARAMETER_SCL_GUI_VERSION) == null) {
                            cal.getCalibratorInfos().getParameters().add(paramSearchCalVersion);
                        }
                    }
                }

                // Use invokeLater to avoid concurrency and ensure that 
                // data model is modified and fire events using Swing EDT:
                SwingUtils.invokeEDT(new Runnable() {
                    @Override
                    public void run() {

                        if (TargetEditorDialog.isTargetEditorActive()) {
                            MessagePane.showErrorMessage("Please close the target editor first !");
                            return;
                        }

                        // check the number of calibrators:
                        if (calibrators.isEmpty()) {
                            MessagePane.showErrorMessage("No calibrator found in SearchCal response !");
                            return;
                        }

                        if (!TargetImporter.confirmImport(calibrators.size())) {
                            return;
                        }

                        // find correct diameter among UD_ for the Aspro instrument band ...
                        // or using alternate diameters (in order of priority): UD, LD, UDDK, DIA12
                        om.defineCalibratorDiameter(calibrators);

                        // use deep copy of the current observation to manipulate target and calibrator list properly:
                        final ObservationSetting obsCloned = om.getMainObservation().deepClone();

                        // Prepare the data model (editable targets and user infos):
                        final List<Target> editTargets = obsCloned.getTargets();
                        final TargetUserInformations editTargetUserInfos = obsCloned.getOrCreateTargetUserInfos();

                        if (logger.isDebugEnabled()) {
                            logger.debug("initial targets:");
                            for (Target t : editTargets) {
                                logger.debug(t.toString());
                            }
                        }

                        // Find any target (id + position) within 5 arcsecs:
                        Target currentScienceTarget = Target.matchTarget(scienceTarget, editTargets);

                        if (currentScienceTarget == null) {
                            logger.info("Target '{}' not found in targets; adding it (partial information).", scienceTarget.getName());

                            editTargets.add(scienceTarget);
                            currentScienceTarget = scienceTarget;
                        }

                        final String report = mergeTargets(editTargets, editTargetUserInfos, currentScienceTarget, calibrators);

                        if (logger.isDebugEnabled()) {
                            logger.debug("updated targets:");
                            for (Target t : editTargets) {
                                logger.debug(t.toString());
                            }
                        }

                        // update the complete list of targets and force to update references:
                        // needed to replace old target references by the new calibrator targets:
                        om.updateTargets(editTargets, editTargetUserInfos);

                        if (logger.isInfoEnabled()) {
                            logger.info(report);
                        }

                        // bring this application to front:
                        App.showFrameToFront();

                        // display report message:
                        MessagePane.showMessage(report);
                    }
                });
            } else {
                // GetStar case:
                final List<Target> targets = searchCalObservation.getTargets();

                if (logger.isDebugEnabled()) {
                    logger.debug("targets:");
                    for (Target t : targets) {
                        logger.debug(t.toString());
                    }
                }

                // Use invokeLater to avoid concurrency and ensure that 
                // data model is modified and fire events using Swing EDT:
                SwingUtils.invokeEDT(new Runnable() {
                    @Override
                    public void run() {

                        if (TargetEditorDialog.isTargetEditorActive()) {
                            MessagePane.showErrorMessage("Please close the target editor first !");
                            return;
                        }

                        // check the number of targets:
                        if (targets.isEmpty()) {
                            MessagePane.showErrorMessage("No target found in GetStar response !");
                            return;
                        }

                        if (!TargetImporter.confirmImport(targets.size())) {
                            return;
                        }

                        // find correct diameter among UD_ for the Aspro instrument band ...
                        // or using alternate diameters (in order of priority): UD, LD, UDDK, DIA12
                        om.defineCalibratorDiameter(targets);

                        // use deep copy of the current observation to manipulate target and calibrator list properly:
                        final ObservationSetting obsCloned = om.getMainObservation().deepClone();

                        // Prepare the data model (editable targets and user infos):
                        final List<Target> editTargets = obsCloned.getTargets();
                        final TargetUserInformations editTargetUserInfos = obsCloned.getOrCreateTargetUserInfos();

                        if (logger.isDebugEnabled()) {
                            logger.debug("initial targets:");
                            for (Target t : editTargets) {
                                logger.debug(t.toString());
                            }
                        }

                        final String report = mergeTargets(editTargets, targets);

                        if (logger.isDebugEnabled()) {
                            logger.debug("updated targets:");
                            for (Target t : editTargets) {
                                logger.debug(t.toString());
                            }
                        }

                        // update the complete list of targets and force to update references:
                        // needed to replace old target references by the new calibrator targets:
                        om.updateTargets(editTargets, editTargetUserInfos);

                        if (logger.isInfoEnabled()) {
                            logger.info(report);
                        }

                        // bring this application to front:
                        App.showFrameToFront();

                        // display report message:
                        MessagePane.showMessage(report);
                    }
                });
            }

        } catch (IllegalArgumentException iae) {
            // Report both Observation and VOTable in a new IllegalArgumentException to get them in the feedback report:
            throw new IllegalArgumentException("Invalid generated Aspro2 Observation:\n\n" + document + "\n\nSAMP VOTable argument:\n\n" + votable, iae);
        }

        // interpreted as SearchCal votable:
        return true;
    }

    /**
     * Merge targets and calibrators
     * @param editTargets edited list of targets
     * @param editTargetUserInfos edited target user informations
     * @param scienceTarget science target
     * @param calibrators list of calibrators for the science target
     * @return merge operation report
     */
    private static String mergeTargets(final List<Target> editTargets, final TargetUserInformations editTargetUserInfos,
                                       final Target scienceTarget, final List<Target> calibrators) {
        // report buffer:
        final StringBuilder sb = new StringBuilder(512);
        sb.append("Import SearchCal calibrators to target [").append(scienceTarget.getName()).append("]\n\n");

        String calName;
        Target oldCal;

        for (Target newCal : calibrators) {
            // format the target name:
            newCal.updateNameAndIdentifier();
            calName = newCal.getName();

            newCal.setOrigin(AsproConstants.SEARCHCAL_SAMP_NAME);

            // Find any target (id + position) within 5 arcsecs:
            // note: SearchCal considers internally duplicates within 10 arcsecs (and discard all of them)
            oldCal = Target.matchTarget(newCal, editTargets);

            if (oldCal == null) {

                // append the missing target:
                editTargets.add(newCal);

                // define it as a calibrator:
                editTargetUserInfos.addCalibrator(newCal);

                // associate it to the science target:
                editTargetUserInfos.addCalibratorToTarget(scienceTarget, newCal);

                // report message:
                sb.append(calName).append(" added as a calibrator\n");

            } else {
                // target already exist: always replace old target by the SearchCal calibrator (gilles)

                // copy non empty values into new calibrator:
                Target.merge(newCal, oldCal);

                // check if the existing target had calibrators:
                if (editTargetUserInfos.hasCalibrators(oldCal)) {
                    final List<Target> oldCalibrators = editTargetUserInfos.getCalibrators(oldCal);

                    // report message:
                    sb.append("WARNING: ").append(calName).append(" had calibrators that were removed: ");
                    for (Target cal : oldCalibrators) {
                        sb.append(cal.getName()).append(' ');
                    }
                    sb.append('\n');

                    // remove calibrators related to the calibrator target:
                    oldCalibrators.clear();
                }

                // note: oldCal and newCal are equals() so following operations are possible:
                if (!editTargetUserInfos.isCalibrator(oldCal)) {
                    // define it as a calibrator:
                    editTargetUserInfos.addCalibrator(newCal);

                    // report message:
                    sb.append(calName).append(" updated and flagged as a calibrator\n");
                } else {
                    // report message:
                    sb.append(calName).append(" updated\n");
                }

                // associate it to the science target:
                editTargetUserInfos.addCalibratorToTarget(scienceTarget, newCal);

                // replace the old target by the new calibrator:
                // note: the position of the target is not the same:
                editTargets.remove(oldCal);
                editTargets.add(newCal);
            }
        }
        return sb.toString();
    }

    /**
     * Merge targets (GetStar)
     * @param editTargets edited list of targets
     * @param targets list of targets
     * @return merge operation report
     */
    private static String mergeTargets(final List<Target> editTargets, final List<Target> targets) {
        // report buffer:
        final StringBuilder sb = new StringBuilder(512);
        sb.append("Import GetStar targets\n\n");

        for (Target newTarget : targets) {
            // format the target name:
            newTarget.updateNameAndIdentifier();
            String targetName = newTarget.getName();

            newTarget.setOrigin(GET_STAR_NAME);

            // Find any target (id + position) within 5 arcsecs:
            // note: SearchCal considers internally duplicates within 10 arcsecs (and discard all of them)
            final Target oldTarget = Target.matchTarget(newTarget, editTargets);

            if (oldTarget == null) {
                // append the missing target:
                editTargets.add(newTarget);

                // report message:
                sb.append(targetName).append(" added\n");

            } else {
                // target already exist: merge information

                // copy non empty values into old target:
                Target.merge(oldTarget, newTarget);

                // Add model if none already present:
                // (do not merge with existing model)
                if (!oldTarget.hasModel() || !oldTarget.hasAnalyticalModel()) {
                    oldTarget.getModels().addAll(newTarget.getModels());
                }

                // report message:
                sb.append(targetName).append(" merged with target [").append(oldTarget.getName()).append("]\n");
            }
        }
        return sb.toString();
    }
}
