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
public class AbstractTargetJTree<E> extends GenericJTree<E> {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;

    /* members */
    /** edited target user informations (clone) */
    protected final TargetUserInformations editTargetUserInfos;
    /** temporary buffer */
    protected final StringBuffer sbTmp = new StringBuffer(512);
    /** last item index at the mouse position */
    private int lastIndex;
    /** last tooltip at item index */
    private String lastTooltip;

    /**
     * Public constructor
     * @param classType class corresponding to <E> generic type
     * @param targetUserInfos target user informations
     */
    public AbstractTargetJTree(final Class<E> classType, final TargetUserInformations targetUserInfos) {
        super(classType);

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
    public final void fireNodeChanged(final TreeNode node) {
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
    public final String getToolTipText(final MouseEvent me) {
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
                    tooltip = target.toHtml(sbTmp);
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
    public final void selectTarget(final E target) {
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
    protected String convertUserObjectToString(final E userObject) {
        if (userObject instanceof Target) {
            final StringBuffer sb = sbTmp;
            sb.setLength(0); // clear
            this.editTargetUserInfos.getTargetDisplayName((Target) userObject, sb);
            return sb.toString();
        }
        return toString(userObject);
    }
}
