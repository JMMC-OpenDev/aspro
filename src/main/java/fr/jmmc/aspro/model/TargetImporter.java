/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.gui.TargetEditorDialog;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetGroup;
import fr.jmmc.aspro.model.oi.TargetGroupMembers;
import fr.jmmc.aspro.model.oi.TargetInformation;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.model.util.TargetMatch;
import fr.jmmc.aspro.model.util.TargetUtils;
import fr.jmmc.jmal.ALX;
import fr.jmmc.jmal.star.Star;
import fr.jmmc.jmal.star.StarResolver;
import fr.jmmc.jmal.star.StarListResolverResult;
import fr.jmmc.jmal.star.StarResolverWidget;
import fr.jmmc.jmcs.App;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.StringUtils;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.IdentityHashMap;
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
    public final static int THRESHOLD_TARGETS = 100;
    /** maximum targets accepted at once */
    public final static int MAX_TARGETS = 10000;

    /** distance in degrees to consider exact targets match = 0.01 arcsecs ie less than 10 mas */
    public final static double EXACT_TARGET_DISTANCE = 0.01 * fr.jmmc.jmal.ALX.ARCSEC_IN_DEGREES;

    /**
     * Private constructor
     */
    private TargetImporter() {
        super();
    }

    public static boolean addTarget(final Star star, final List<Target> editTargets, final boolean doConfirm, final StringBuilder sb) {
        boolean changed = false;
        if (star != null) {
            final Target newTarget = TargetUtils.convert(star);

            // TODO: if update ? then use queryName instead
            // update the data model or throw exception ?
            if (newTarget != null) {
                boolean add = true;

                // Find any target (id + position) within 5 arcsecs:
                final TargetMatch match = Target.doMatchTarget(newTarget, editTargets);

                if (match != null) {
                    final Target t = match.getMatch();
                    String msg;
                    add = false;

                    // exact match:
                    if (match.getDistance() <= EXACT_TARGET_DISTANCE) {
                        logger.info("Merge [{}] with [{}]", t, newTarget);
                        msg = "Target[" + t.getName() + "] updated.";

                        Target.mergeSimbadTarget(t, newTarget);
                        changed = true;
                    } else {
                        msg = "Target[" + newTarget.getName() + "](" + newTarget.getRA() + " , " + newTarget.getDEC()
                                + ") too close to Target[" + t.getName() + "](" + t.getRA() + " , " + t.getDEC()
                                + "): " + NumberUtils.trimTo3Digits(match.getDistance() * ALX.DEG_IN_ARCSEC) + " arcsec.";

                        if (doConfirm) {
                            // Ask user confirmation:
                            add = MessagePane.showConfirmMessage(msg + "\n\nDo you really want to add this target anyway ?");
                        }

                        msg += "\nTarget[" + newTarget.getName() + "] " + ((add) ? "added" : "skipped") + " (user)";
                    }
                    if (msg != null) {
                        logger.info("addTarget: {}", msg);
                        // Append warnings:
                        sb.append(msg).append('\n');
                    }
                }
                if (add) {
                    editTargets.add(newTarget);
                    changed = true;
                }
            }
        }
        return changed;
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
                        // check the number of targets:
                        if (targets.isEmpty()) {
                            MessagePane.showErrorMessage("No target found: missing name or RA/DEC coordinates (J2000) !");
                            return;
                        }

                        final ObservationManager om = ObservationManager.getInstance();

                        // get the observationContext.operation:
                        final boolean isOperationNew = (!forceAddTargets && loadedObservation.getContext() != null)
                                ? loadedObservation.getContext().isOperationNew() : false;

                        if (!isOperationNew
                                || (loadedObservation.getInterferometerConfiguration() == null)
                                || (loadedObservation.getInstrumentConfiguration() == null)) {

                            if (!confirmImport(targets.size())) {
                                return;
                            }

                            // empty configuration: add targets only
                            final TargetUserInformations targetUserInfos = loadedObservation.getOrCreateTargetUserInfos();

                            final TargetEditorDialog targetEditor = TargetEditorDialog.getTargetEditor();

                            // Prepare the data model (editable targets and user infos) :
                            final TargetEditContext editTargetCtx = (targetEditor != null) ? targetEditor.getTargetEditCtx()
                                    : om.getMainObservation().createTargetEditContext();

                            final List<Target> editTargets = editTargetCtx.getTargets();
                            final TargetUserInformations editTargetUserInfos = editTargetCtx.getTargetUserInfos();

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

                            if (targetEditor != null) {
                                // refresh target editor:
                                targetEditor.refreshDialog();
                            } else {
                                // update the complete list of targets and force to update references:
                                // needed to replace old target references by the new calibrator targets:
                                om.updateTargets(editTargetCtx);
                            }

                            if (logger.isInfoEnabled()) {
                                logger.info(report);
                            }

                            // display report message:
                            MessagePane.showMessage(report);

                        } else {
                            if (MessagePane.showConfirmMessage("Are you sure you want to load the incoming observation ?")) {

                                final TargetEditorDialog targetEditor = TargetEditorDialog.getTargetEditor();
                                if (targetEditor != null) {
                                    targetEditor.closeDialog();
                                }

                                // load the complete observation:
                                om.resetAndChangeObservation(loadedObservation);

                                // bring this application to front:
                                App.showFrameToFront();
                            }
                        }
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
            final Object rawResult = StarResolver.waitFor(new StarResolver().multipleResolve(nameList));
            if ((rawResult == null) || !(rawResult instanceof StarListResolverResult)) {
                MessagePane.showErrorMessage("Unable to resolve target identifiers: " + nameList, "Star resolver problem");
                return false;
            }
            final StarListResolverResult result = (StarListResolverResult) rawResult;
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
                        Target.merge(newTarget, resolvedTarget);
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
     * @param targets list of imported targets
     * @param targetUserInfos imported target user informations
     * @return merge operation report
     */
    private static String mergeTargets(final StringBuilder sb, final String origin,
                                       final List<Target> editTargets, final TargetUserInformations editTargetUserInfos,
                                       final List<Target> targets, final TargetUserInformations targetUserInfos) {

        final IdentityHashMap<Target, Target> mapTargetsNewToOld = new IdentityHashMap<Target, Target>(targets.size());

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
                mapTargetsNewToOld.put(newTarget, newTarget);

                // append the missing target:
                editTargets.add(newTarget);

                // report message:
                sb.append(targetName).append(" added\n");

            } else {
                // target already exist
                mapTargetsNewToOld.put(newTarget, oldTarget);

                // copy non empty values into old target:
                Target.merge(oldTarget, newTarget);

                // report message:
                sb.append(targetName).append(" updated\n");
            }
        }

        // Flag new calibrators:
        Target oldCal;
        String calName;

        for (Target newCal : targetUserInfos.getCalibrators()) {

            // Find corresponding target:
            oldCal = mapTargetsNewToOld.get(newCal);

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

        final IdentityHashMap<TargetGroup, TargetGroup> mapGroupsNewToOld = new IdentityHashMap<TargetGroup, TargetGroup>(4);

        for (TargetGroup oldGroup : editTargetUserInfos.getGroups()) {
            final String gid = oldGroup.getId();

            final TargetGroup newGroup = targetUserInfos.getGroupById(gid);
            if (newGroup != null) {
                mapGroupsNewToOld.put(newGroup, oldGroup);

                final TargetGroupMembers tgm = targetUserInfos.getGroupMembers(newGroup);
                if (tgm != null) {
                    for (Target newTarget : tgm.getTargets()) {

                        // Find corresponding target:
                        oldTarget = mapTargetsNewToOld.get(newTarget);

                        // should not be null as it has been added before:
                        if (oldTarget != null) {
                            targetName = oldTarget.getName();

                            editTargetUserInfos.addTargetToTargetGroup(oldGroup, oldTarget);

                            // report message:
                            sb.append(targetName).append(" added to the group ").append(oldGroup.getName()).append('\n');
                        }
                    }
                }
            }
        }

        // Process new groups:
        for (TargetGroup newGroup : targetUserInfos.getGroups()) {
            final String gid = newGroup.getId();

            TargetGroup oldGroup = editTargetUserInfos.getGroupById(gid);
            if (oldGroup == null) {
                oldGroup = newGroup;
                mapGroupsNewToOld.put(newGroup, oldGroup);

                editTargetUserInfos.addGroup(newGroup);

                final TargetGroupMembers tgm = targetUserInfos.getGroupMembers(newGroup);
                if (tgm != null) {
                    for (Target newTarget : tgm.getTargets()) {

                        // Find corresponding target:
                        oldTarget = mapTargetsNewToOld.get(newTarget);

                        // should not be null as it has been added before:
                        if (oldTarget != null) {
                            targetName = oldTarget.getName();

                            editTargetUserInfos.addTargetToTargetGroup(oldGroup, oldTarget);

                            // report message:
                            sb.append(targetName).append(" added to the group ").append(oldGroup.getName()).append('\n');
                        }
                    }
                }
            }
        }

        // Add target notes, calibrators and ancillary stars to science targets:
        Target newRef;
        for (TargetInformation targetInfo : targetUserInfos.getTargetInfos()) {
            newRef = targetInfo.getTargetRef();

            // Find corresponding target:
            oldTarget = mapTargetsNewToOld.get(newRef);

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
                    for (Target newCal : targetUserInfos.getCalibrators(newRef)) {

                        // Find corresponding target:
                        oldCal = mapTargetsNewToOld.get(newCal);

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

                // Process group members (AO / FT / Guide):
                for (TargetGroupMembers tgm : targetInfo.getGroupMembers()) {
                    // note: ignore user groups:
                    final TargetGroup newGroup = tgm.getGroupRef();

                    for (Target newTargetMember : tgm.getTargets()) {

                        // Find corresponding target:
                        final Target oldTargetMember = mapTargetsNewToOld.get(newTargetMember);

                        // should not be null as it has been added before:
                        if (oldTargetMember != null) {
                            final TargetInformation editTargetInfo = editTargetUserInfos.getOrCreateTargetInformation(oldTarget);

                            // Find corresponding group:
                            final TargetGroup oldGroup = mapGroupsNewToOld.get(newGroup);

                            // should not be null, predefined:
                            if (oldGroup != null) {
                                editTargetInfo.addTargetInGroupMembers(oldGroup, oldTargetMember);

                                // report message:
                                sb.append(oldTargetMember.getName()).append(" added as [").append(oldGroup.getName()).append("] to target ")
                                        .append(targetName).append('\n');
                            }
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
