/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.jmal.model.targetmodel.Model;
import fr.jmmc.jmcs.gui.component.GenericJTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/**
 * This JTree contains Targets and their models
 * @author bourgesl
 */
public final class ModelJTree extends GenericJTree<Object> {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;

  /* members */
  /** edited target user informations (clone) */
  private final TargetUserInformations editTargetUserInfos;

  /**
   * Public constructor
   * @param targetUserInfos target user informations
   */
  public ModelJTree(final TargetUserInformations targetUserInfos) {
    super(null);

    this.editTargetUserInfos = targetUserInfos;
  }

  /**
   * Select the target node for the given target name
   * @param target target to select
   */
  public void selectTarget(final Target target) {

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
  protected String convertUserObjectToString(final Object userObject) {

    if (userObject instanceof Model) {
      return ((Model) userObject).getName();
    }

    if (userObject instanceof Object) {
      return this.editTargetUserInfos.getTargetDisplayName((Target) userObject);
    }

    return toString(userObject);
  }
}
