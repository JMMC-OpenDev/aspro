/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.jmcs.gui.component.GenericJTree;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import java.awt.Point;
import java.awt.event.MouseEvent;
import javax.swing.ToolTipManager;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

/**
 * This JTree contains Targets and their calibrators
 * @author bourgesl
 */
public final class TargetJTree extends GenericJTree<Target> {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;

    /* members */
    /** edited target user informations (clone) */
    private final TargetUserInformations editTargetUserInfos;
    /** tooltip buffer */
    private final StringBuffer sbToolTip = new StringBuffer(512);
    /** last item index at the mouse position */
    private int lastIndex;
    /** last tooltip at item index */
    private String lastTooltip;

    /**
     * Public constructor
     * @param targetUserInfos target user informations
     */
    public TargetJTree(final TargetUserInformations targetUserInfos) {
        super(Target.class);

        this.editTargetUserInfos = targetUserInfos;

        // Register with the ToolTipManager so that tooltips from the
        // renderer show through.
        ToolTipManager toolTipManager = ToolTipManager.sharedInstance();
        toolTipManager.registerComponent(this);
    }

    /**
     * Fire node structure changed on the given tree node
     * and reset last tooltip
     * @param node changed tree node
     */
    @Override
    public void fireNodeChanged(final TreeNode node) {
        super.fireNodeChanged(node);

        // reset last tooltip:
        lastIndex = -1;
        lastTooltip = null;
    }

    /** 
     * This method is called as the cursor moves within the component
     * @param me mouse event
     * @return tooltip text
     */
    @Override
    public String getToolTipText(final MouseEvent me) {
        // Get item index :
        final int index = getRowForLocation(me.getX(), me.getY());
        if (index != -1) {
            if (lastIndex == index) {
                // use last tooltip:
                return lastTooltip;
            } else {
                String tooltip = null;
                // Get target :
                final TreePath path = getPathForRow(index);
                final DefaultMutableTreeNode node = (DefaultMutableTreeNode) path.getLastPathComponent();

                final Object userObject = node.getUserObject();
                if (userObject instanceof Target) {
                    final Target target = (Target) userObject;
                    // Return the tool tip text :
                    tooltip = target.toHtml(sbToolTip);
                }
                lastIndex = index;
                lastTooltip = tooltip;
                return tooltip;
            }
        }
        return getToolTipText();
    }

    /**
     * Select the target node for the given target name
     * @param target target to select
     */
    public final void selectTarget(final Target target) {
        if (target != null) {
            final DefaultMutableTreeNode targetNode = this.findTreeNode(target);

            if (targetNode != null) {
                // Select the target node :
                this.selectPath(new TreePath(targetNode.getPath()));

                // expand target node if there is at least one model :
                if (!targetNode.isLeaf()) {
                    final DefaultMutableTreeNode child = (DefaultMutableTreeNode) targetNode.getFirstChild();

                    this.scrollPathToVisible(new TreePath(child.getPath()));
                }

                return;
            }
        }

        // select first target :
        selectFirstChildNode(getRootNode());
    }

    /**
     * Convert a non-null value object to string
     * @param userObject user object to convert
     * @return string representation of the user object
     */
    @Override
    protected String convertUserObjectToString(final Target userObject) {
        return this.editTargetUserInfos.getTargetDisplayName(userObject);
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
    public final boolean removeCalibrator(final DefaultMutableTreeNode calibratorNode, final Target calibrator, final DefaultMutableTreeNode targetNode, final Target target,
                                          final boolean doSelectParent) {

        if (this.editTargetUserInfos.removeCalibratorFromTarget(target, calibrator)) {
            // remove node :
            this.removeNodeAndRefresh(targetNode, calibratorNode, doSelectParent);

            return true;
        }
        return false;
    }
}
