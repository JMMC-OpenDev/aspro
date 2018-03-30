/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.jmcs.gui.component.GenericJTree;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import java.awt.event.MouseEvent;
import javax.swing.ToolTipManager;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

/**
 * This JTree contains Targets and their calibrators
 * @author bourgesl
 */
public class TargetJTree extends AbstractTargetJTree<Target> {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;

    /**
     * Public constructor
     * @param targetUserInfos target user informations
     */
    public TargetJTree(final TargetUserInformations targetUserInfos) {
        super(Target.class, targetUserInfos);
    }

    /**
     * Add the given calibrator target to the given science target
     * @param calibrator calibrator target
     * @param targetNode science target node to update
     * @param target science target to use
     * @return true if the calibrator was added
     */
    public final boolean addCalibrator(final Target calibrator, final DefaultMutableTreeNode targetNode, final Target target) {

        if (this.editTargetUserInfos.addCalibratorToTarget(target, calibrator)) {
            // Add node :
            this.addNodeAndRefresh(targetNode, calibrator);

            return true;
        }
        return false;
    }

    /**
     * Remove the given calibrator target from the given science target
     * @param calibratorNode calibrator node to remove
     * @param calibrator calibrator target
     * @param targetNode science target node to update
     * @param target science target to use
     * @param doSelectParent flag to indicate to select the parent node once the node removed
     * @return true if the calibrator was removed
     */
    public final boolean removeCalibrator(final DefaultMutableTreeNode calibratorNode, final Target calibrator,
                                          final DefaultMutableTreeNode targetNode, final Target target,
                                          final boolean doSelectParent) {

        if (this.editTargetUserInfos.removeCalibratorFromTarget(target, calibrator)) {
            // remove node :
            this.removeNodeAndRefresh(targetNode, calibratorNode, doSelectParent);

            return true;
        }
        return false;
    }
}
