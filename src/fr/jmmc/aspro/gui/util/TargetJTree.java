/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: TargetJTree.java,v 1.3 2010-12-07 17:37:54 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2010/12/06 17:01:58  bourgesl
 * added addCalibrator and removeCalibrator that use data model and refresh tree
 *
 * Revision 1.1  2010/12/03 16:10:57  bourgesl
 * JTree utility classes to handle targets / models
 *
 */
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/**
 * This JTree contains Targets and their calibrators
 * @author bourgesl
 */
public class TargetJTree extends GenericJTree {

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
    super();

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
   * Select the first child node
   * @param rootNode root node
   */
  public final void selectFirstChildNode(final DefaultMutableTreeNode rootNode) {
    // first child = first target :
    final DefaultMutableTreeNode firstChild = (DefaultMutableTreeNode) rootNode.getFirstChild();

    this.selectPath(new TreePath(firstChild.getPath()));

    // expand target node if there is at least one model :
    if (!firstChild.isLeaf()) {
      final DefaultMutableTreeNode secondChild = (DefaultMutableTreeNode) firstChild.getFirstChild();

      this.scrollPathToVisible(new TreePath(secondChild.getPath()));
    }
  }

  /**
   * Convert a value object to string
   * @param userObject user object to convert
   * @return string representation of the user object
   */
  @Override
  protected String convertUserObjectToString(final Object userObject) {
    if (userObject instanceof Target) {
      return this.editTargetUserInfos.getTargetDisplayName((Target) userObject);
    }
    return super.convertUserObjectToString(userObject);
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
