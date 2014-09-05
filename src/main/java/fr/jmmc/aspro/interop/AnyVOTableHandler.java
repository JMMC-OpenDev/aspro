/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.interop;

import fr.jmmc.aspro.gui.TargetEditorDialog;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetInformation;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.jmcs.App;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.service.XslTransform;
import java.io.IOException;
import java.io.StringReader;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class handles generic VOTables (targets) and PIVOT VOTables
 * 
 * @author bourgesl
 */
public final class AnyVOTableHandler {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(AnyVOTableHandler.class.getName());
    /** threshold to ask user confirmation */
    public final static int THRESHOLD_TARGETS = 50;
    /** maximum targets accepted at once */
    public final static int MAX_TARGETS = 1000;
    /** XSLT file path */
    private final static String XSLT_FILE = "fr/jmmc/aspro/interop/vot2AsproObservation.xsl";

    /**
     * Private constructor
     */
    private AnyVOTableHandler() {
        super();
    }

    /**
     * Process the given votable
     *
     * @param votable votable to process
     * @param forceAddTargets true to add targets only
     * 
     * @throws IOException if an I/O exception occured
     * @throws IllegalArgumentException if the file is not an Observation
     */
    public static void processVOTable(final String votable, final boolean forceAddTargets) throws IOException {

        // use an XSLT to transform the votable document to an Aspro 2 Observation:
        final long start = System.nanoTime();

        final String document = XslTransform.transform(votable, XSLT_FILE);

        logger.info("VOTable transformation (XSLT): {} ms.", 1e-6d * (System.nanoTime() - start));

        logger.debug("document:\n{}", document);

        final ObservationManager om = ObservationManager.getInstance();

        try {
            // throws IllegalArgumentException if the document can not be loaded (invalid or unmarshalling exception):
            final ObservationSetting newObservation = om.load(new StringReader(document));

            if (newObservation != null) {

                final List<Target> targets = newObservation.getTargets();

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
                            MessagePane.showErrorMessage("No valid target found in VOTable: missing name or RA/DEC coordinates (J2000) !");
                            return;
                        }

                        if (!confirmImport(targets.size())) {
                            return;
                        }

                        // get the observationContext.operation:
                        final boolean isOperationNew = (!forceAddTargets && newObservation.getContext() != null) ? newObservation.getContext().isOperationNew() : false;

                        if (!isOperationNew
                                || newObservation.getInterferometerConfiguration() == null
                                || newObservation.getInstrumentConfiguration() == null) {

                            // empty configuration: add targets only
                            final TargetUserInformations targetUserInfos = newObservation.getOrCreateTargetUserInfos();

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

                            final String report = mergeTargets(editTargets, editTargetUserInfos, targets, targetUserInfos);

                            if (logger.isDebugEnabled()) {
                                logger.debug("updated targets:");
                                for (Target t : editTargets) {
                                    logger.debug(t.toString());
                                }
                            }

                            // update the complete list of targets and force to update references:
                            // needed to replace old target references by the new targets:
                            om.updateTargets(editTargets, editTargetUserInfos);

                            if (logger.isInfoEnabled()) {
                                logger.info(report);
                            }

                            // bring this application to front:
                            App.showFrameToFront();

                            // display report message:
                            MessagePane.showMessage(report);

                        } else {
                            om.resetAndChangeObservation(newObservation);
                        }

                        // bring this application to front:
                        App.showFrameToFront();
                    }
                });
            }
        } catch (IllegalArgumentException iae) {
            // Report both Observation and VOTable in a new IllegalArgumentException to get them in the feedback report:
            throw new IllegalArgumentException("Invalid generated Aspro2 Observation:\n\n" + document + "\n\nVOTable:\n\n" + votable, iae);
        }
    }

    /**
     * Merge targets and calibrators
     * @param editTargets edited list of targets
     * @param editTargetUserInfos edited target user informations
     * @param targets list of new targets
     * @param targetUserInfos new target user informations
     * @return merge operation report
     */
    private static String mergeTargets(final List<Target> editTargets, final TargetUserInformations editTargetUserInfos,
                                       final List<Target> targets, final TargetUserInformations targetUserInfos) {
        // report buffer:
        final StringBuilder sb = new StringBuilder(512);
        sb.append("Import targets from VOTable\n\n");

        String targetName;
        Target oldTarget;

        for (Target newTarget : targets) {
            // format the target name:
            newTarget.updateNameAndIdentifier();
            targetName = newTarget.getName();

            newTarget.setOrigin("VOTable");

            // fix RA/DEC deg vs HMS/DMS formats
            // note: should check for invalid values ?
            newTarget.fixCoords();

            // Find any target (id + position) within 5 arcsecs:
            oldTarget = Target.matchTarget(newTarget, editTargets);

            if (oldTarget == null) {
                // append the missing target:
                editTargets.add(newTarget);

                // report message:
                sb.append(targetName).append(" added\n");

            } else {
                // target already exist

                // copy non empty values into old target:
                Target.mergeTarget(oldTarget, newTarget);

                // report message:
                sb.append(targetName).append(" updated\n");
            }
        }

        // Flag new calibrators:
        Target oldCal;
        String calName;

        for (Target newCal : targetUserInfos.getCalibrators()) {

            // Find any target (id + position) within 5 arcsecs:
            oldCal = Target.matchTarget(newCal, editTargets);

            // should not be null as it has been added before:
            if (oldCal != null) {
                calName = oldCal.getName();
                // not calibrator yet ?
                if (editTargetUserInfos.isCalibrator(oldCal)) {
                    // report message:
                    sb.append(calName).append(" already flagged as a calibrator\n");
                } else {
                    // check if the 'new' calibrator has calibrators already !
                    if (editTargetUserInfos.hasCalibrators(oldCal)) {
                        final List<Target> oldCalibrators = editTargetUserInfos.getCalibrators(oldCal);

                        // report message:
                        sb.append("WARNING: ").append(calName).append(" had calibrators already: \n");
                        for (Target cal : oldCalibrators) {
                            sb.append(cal.getName()).append(' ');
                        }
                        sb.append('\n');

                    } else {
                        // flag target as calibrator:
                        editTargetUserInfos.addCalibrator(oldCal);

                        // report message:
                        sb.append(calName).append(" flagged as a calibrator\n");
                    }
                }
            }
        }

        // Add calibrators to science targets:
        Target ref;
        for (TargetInformation targetInfo : targetUserInfos.getTargetInfos()) {
            ref = targetInfo.getTargetRef();

            // Find any target (id + position) within 5 arcsecs:
            oldTarget = Target.matchTarget(ref, editTargets);

            // should not be null as it has been added before:
            if (oldTarget != null) {
                targetName = oldTarget.getName();

                // process calibrator for this science target:
                if (!editTargetUserInfos.isCalibrator(oldTarget)) {
                    for (Target newCal : targetUserInfos.getCalibrators(ref)) {

                        // Find any target (id + position) within 5 arcsecs:
                        oldCal = Target.matchTarget(newCal, editTargets);

                        // should not be null as it has been added before:
                        if ((oldCal != null) && editTargetUserInfos.isCalibrator(oldCal)) {
                            calName = oldCal.getName();

                            // add calibrator:
                            editTargetUserInfos.addCalibratorToTarget(oldTarget, oldCal);

                            // report message:
                            sb.append(calName).append(" added as a calibrator to target ").append(targetName).append('\n');

                        }
                    }
                }
            }
        }

        return sb.toString();
    }

    /**
     * Displays one confirmation message if there is more than 50 targets to import
     * @param size size of the target list to import
     * @return true if there is less than 50 targets or the user confirms.
     */
    static boolean confirmImport(final int size) {
        if (size > MAX_TARGETS) {
            MessagePane.showErrorMessage("Too many targets to import (" + size + " / " + MAX_TARGETS + ") !");
            return false;
        }
        if (size > THRESHOLD_TARGETS) {
            return MessagePane.showConfirmMessage("Are you sure you want to import " + size + " targets to your current observation ?");
        }

        return true;
    }
}
