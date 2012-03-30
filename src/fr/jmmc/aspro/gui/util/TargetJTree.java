/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.jmcs.gui.component.GenericJTree;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/**
 * This JTree contains Targets and their calibrators
 * @author bourgesl
 */
public class TargetJTree extends GenericJTree<Target> {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;

  /* members */
  /** edited target user informations (clone) */
  private final TargetUserInformations editTargetUserInfos;

  /**
   * Public constructor
   * @param targetUserInfos target user informations
   */
  public TargetJTree(final TargetUserInformations targetUserInfos) {
    super(Target.class);

    this.editTargetUserInfos = targetUserInfos;
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
