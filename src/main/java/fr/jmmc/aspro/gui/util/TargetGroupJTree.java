/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.oi.TargetGroup;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.jmcs.gui.component.GenericJTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/**
 * This JTree contains Targets and their TargetGroups
 * @author bourgesl
 */
public final class TargetGroupJTree extends AbstractTargetJTree<Object> {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;

    /**
     * Public constructor
     * @param targetUserInfos target user informations
     */
    public TargetGroupJTree(final TargetUserInformations targetUserInfos) {
        super(null, targetUserInfos);
    }

    /**
     * Select the target node for the given target
     * @param items items to select (Target or TargetGroup)
     */
    public void selectTargetPath(final Object... items) {
        if (items != null && items.length != 0) {
            DefaultMutableTreeNode node = this.findTreeNode(items[0]);

            int i = 1;
            while ((node != null) && (items.length > i) && (items[i] != null)) {
                DefaultMutableTreeNode childNode = GenericJTree.findTreeNode(node, items[i]);

                if (childNode != null) {
                    node = childNode;
                } else {
                    _logger.debug("child node not found: {}", items[i]);
                    break;
                }
                i++;
            } // while

            if (node != null) {
                final TreePath path = new TreePath(node.getPath());
                _logger.debug("select node: {}", path);

                // Select the target node :
                this.selectPath(path);

                // expand target node if there is at least one child node :
                if (!node.isLeaf()) {
                    final DefaultMutableTreeNode child = (DefaultMutableTreeNode) node.getFirstChild();

                    final TreePath childPath = new TreePath(child.getPath());
                    this.scrollPathToVisible(childPath);

                    this.expandAll(childPath, true, true);
                }
            }
        }
    }

    /**
     * Return the tooltip text for the given user object
     * @param userObject user object to get tooltip
     * @param sbTmp temporary buffer 
     * @return tooltip of the user object or null
     */
    @Override
    protected String getTooltipText(final Object userObject, final StringBuilder sbTmp) {
        if (userObject instanceof TargetGroup) {
            final TargetGroup group = (TargetGroup) userObject;
            return group.getTooltip();
        }
        return super.getTooltipText(userObject, sbTmp);
    }

    /**
     * Convert a non-null value object to string
     * @param userObject user object to convert
     * @return string representation of the user object
     */
    @Override
    protected String convertUserObjectToString(final Object userObject) {
        if (userObject instanceof TargetGroup) {
            final TargetGroup group = (TargetGroup) userObject;
            // Return the description:
            return group.getName();
        }
        return super.convertUserObjectToString(userObject);
    }
}
