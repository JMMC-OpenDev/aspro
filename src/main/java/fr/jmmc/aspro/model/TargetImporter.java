/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.gui.TargetEditorDialog;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetInformation;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.model.util.TargetUtils;
import fr.jmmc.jmal.star.Star;
import fr.jmmc.jmal.star.StarResolver;
import fr.jmmc.jmal.star.StarResolverResult;
import fr.jmmc.jmal.star.StarResolverWidget;
import fr.jmmc.jmcs.App;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.util.StringUtils;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This utility class handles target imports from VOTables or Asprox
 * 
 * @author bourgesl
 */
public final class TargetImporter {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(TargetImporter.class.getName());
    /** threshold to ask user confirmation */
    public final static int THRESHOLD_TARGETS = 50;
    /** maximum targets accepted at once */
    public final static int MAX_TARGETS = 1000;

    /**
     * Private constructor
     */
    private TargetImporter() {
        super();
    }

    /**
     * Process the given observation
     *
     * @param file observation to load
     * @param forceAddTargets true to add targets only
     * 
     * @throws IOException if an I/O exception occured
     * @throws IllegalArgumentException if the file is not an Observation
     */
    public static void processObservation(final File file, final boolean forceAddTargets) throws IOException {
        final StringBuilder sb = new StringBuilder(512);
        sb.append("Import targets from an observation:\n");

        // throws IllegalArgumentException if the document can not be loaded (invalid or unmarshalling exception):
        final ObservationSetting loadedObservation = ObservationManager.getInstance().loadObservation(file, sb);

        processObservation(loadedObservation, forceAddTargets, file.getName(), sb);
    }

    /**
     * Process the given observation
     *
     * @param document document to load as an observation
     * @param forceAddTargets true to add targets only
     * 
     * @throws IOException if an I/O exception occured
     * @throws IllegalArgumentException if the file is not an Observation
     */
    public static void processObservation(final String document, final boolean forceAddTargets) throws IOException {
        final StringBuilder sb = new StringBuilder(512);
        sb.append("Import targets from VOTable:\n");

        // throws IllegalArgumentException if the document can not be loaded (invalid or unmarshalling exception):
        final ObservationSetting loadedObservation = ObservationManager.getInstance().load(new StringReader(document));

        processObservation(loadedObservation, forceAddTargets, "VOTable", sb);
    }

    /**
     * Process the given observation
     *
     * @param loadedObservation loaded observation
     * @param forceAddTargets true to add targets only
     * @param origin origin value
     * @param sb report buffer
     * 
     * @throws IOException if an I/O exception occured
     * @throws IllegalArgumentException if the file is not an Observation
     */
    private static void processObservation(final ObservationSetting loadedObservation, final boolean forceAddTargets,
                                           final String origin, final StringBuilder sb) throws IOException {
        if (loadedObservation != null) {
            final List<Target> targets = loadedObservation.getTargets();

            if (logger.isDebugEnabled()) {
                logger.debug("targets:");
                for (Target t : targets) {
                    logger.debug(t.toString());
                }
            }

            // Resolve targets without RA/DEC coordinates:
            if (resolveTargets(sb, targets)) {
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
                            MessagePane.showErrorMessage("No target found: missing name or RA/DEC coordinates (J2000) !");
                            return;
                        }

                        if (!confirmImport(targets.size())) {
                            return;
                        }

                        final ObservationManager om = ObservationManager.getInstance();

                        // get the observationContext.operation:
                        final boolean isOperationNew = (!forceAddTargets && loadedObservation.getContext() != null) ? loadedObservation.getContext().isOperationNew() : false;

                        if (!isOperationNew
                                || loadedObservation.getInterferometerConfiguration() == null
                                || loadedObservation.getInstrumentConfiguration() == null) {

                            // empty configuration: add targets only
                            final TargetUserInformations targetUserInfos = loadedObservation.getOrCreateTargetUserInfos();

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

                            final String report = mergeTargets(sb, origin,
                                    editTargets, editTargetUserInfos, targets, targetUserInfos);

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
                            om.resetAndChangeObservation(loadedObservation);
                        }

                        // bring this application to front:
                        App.showFrameToFront();
                    }
                });
            }
        }
    }

    /**
     * Resolve targets having no RA/DEC coordinates
     * @param sb report buffer
     * @param targets list of new targets
     * @return true if successfull; false otherwise
     */
    private static boolean resolveTargets(final StringBuilder sb, final List<Target> targets) {
        /*
         Note: target name can be updated as calibrator are defined by target references (same instance):
         changing the name is actually transparent (same instance)
         */
        // check RA/DEC = NaN
        final List<String> nameList = new ArrayList<String>();
        for (Target newTarget : targets) {
            if (Double.isNaN(newTarget.getRADeg()) || Double.isNaN(newTarget.getDECDeg())) {
                // update target name to find it later during merge:
                final String cleanName = StarResolver.cleanNames(newTarget.getName());
                newTarget.setName(cleanName);
                nameList.add(cleanName);
            }
        }

        if (!nameList.isEmpty()) {
            // Wait for StarResolver task done:
            final StarResolverResult result = StarResolver.waitFor(new StarResolver().multipleResolve(nameList));
            if (result == null) {
                MessagePane.showErrorMessage("Unable to resolve target identifiers: " + nameList, "Star resolver problem");
                return false;
            }

            // Report errors:
            StarResolverWidget.showResultMessage(result);

            // merge targets:
            for (String name : nameList) {
                final Target newTarget = Target.getTarget(name, targets);
                if (newTarget != null) {
                    final Star star = result.getSingleStar(name);

                    // convert star as target
                    final Target resolvedTarget = TargetUtils.convert(star);

                    if (resolvedTarget == null) {
                        sb.append("Skip target ").append(newTarget.getName()).append("\n");
                        targets.remove(newTarget);
                    } else {
                        Target.mergeTarget(newTarget, resolvedTarget);
                        // format the target name:
                        newTarget.updateNameAndIdentifier(resolvedTarget.getName());

                        // fix NaN / null values:
                        newTarget.checkValues();
                    }
                }
            }
        }
        return true;
    }

    /**
     * Merge targets and calibrators
     * @param sb report buffer
     * @param origin origin value
     * @param editTargets edited list of targets
     * @param editTargetUserInfos edited target user informations
     * @param targets list of new targets
     * @param targetUserInfos new target user informations
     * @return merge operation report
     */
    private static String mergeTargets(final StringBuilder sb, final String origin,
                                       final List<Target> editTargets, final TargetUserInformations editTargetUserInfos,
                                       final List<Target> targets, final TargetUserInformations targetUserInfos) {
        String targetName;
        Target oldTarget;

        for (Target newTarget : targets) {
            // format the target name:
            newTarget.updateNameAndIdentifier();
            targetName = newTarget.getName();

            newTarget.setOrigin(origin);

            // fix RA/DEC deg vs HMS/DMS formats
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

        // Add target notes & calibrators to science targets:
        Target ref;
        for (TargetInformation targetInfo : targetUserInfos.getTargetInfos()) {
            ref = targetInfo.getTargetRef();

            // Find any target (id + position) within 5 arcsecs:
            oldTarget = Target.matchTarget(ref, editTargets);

            // should not be null as it has been added before:
            if (oldTarget != null) {
                targetName = oldTarget.getName();

                if (!StringUtils.isEmpty(targetInfo.getDescription())) {
                    final TargetInformation editTargetInfo = editTargetUserInfos.getOrCreateTargetInformation(oldTarget);

                    if (StringUtils.isEmpty(editTargetInfo.getDescription())) {
                        editTargetInfo.setDescription(targetInfo.getDescription());
                    } else if (!targetInfo.getDescription().equals(editTargetInfo.getDescription())) {
                        sb.append(targetName).append(" notes are in conflict\n");

                        editTargetInfo.setDescription("=== Original ===\n"
                                + editTargetInfo.getDescription()
                                + "\n=== Imported ===\n"
                                + targetInfo.getDescription()
                        );
                    }
                }

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
    public static boolean confirmImport(final int size) {
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
