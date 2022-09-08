/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import java.util.ArrayList;
import java.util.List;

/**
 * Basic List of Target  + TargetUserInformations tuple to ease editing target information
 */
public final class TargetEditContext {

    /** list of edited targets (clone) */
    private final List<Target> editTargets;
    /** list of edited target user informations (clone) */
    private final TargetUserInformations editTargetUserInfos;
    /** selected target */
    private Target selectedTarget;

    /**
     * Fake constructor (used to test swing interface in NetBeans)
     */
    public TargetEditContext() {
        this(new ArrayList<Target>(0), new TargetUserInformations());
    }

    public TargetEditContext(final List<Target> targets, final TargetUserInformations targetUserInfos) {
        // Define shared data model to edit:
        this.editTargets = targets;
        this.editTargetUserInfos = targetUserInfos;
    }

    public List<Target> getTargets() {
        return editTargets;
    }

    public TargetUserInformations getTargetUserInfos() {
        return editTargetUserInfos;
    }

    public Target getSelectedTarget() {
        return selectedTarget;
    }

    public void setSelectedTarget(final Target selectedTarget) {
        this.selectedTarget = selectedTarget;
    }

}
