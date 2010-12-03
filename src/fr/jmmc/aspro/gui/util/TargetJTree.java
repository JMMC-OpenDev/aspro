/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: TargetJTree.java,v 1.1 2010-12-03 16:10:57 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetInformation;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import java.util.List;
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
      final Target target = (Target) userObject;

      String sValue = target.getName();

      if (isCalibrator(target)) {
        sValue += " (cal)";
      }
      return sValue;

    }
    return super.convertUserObjectToString(userObject);
  }

  /**
   * Return true if the given target is a calibrator
   * i.e. the calibrator list contains the given target
   * @param target target to use
   * @return true if the given target is a calibrator
   */
  public final boolean isCalibrator(final Target target) {
    return this.editTargetUserInfos.isCalibrator(target);
  }

  public final boolean addCalibrator(final Target calibrator, final DefaultMutableTreeNode destNode, final Target target) {

    if (!isCalibrator(target)) {
      final TargetInformation targetInfo = this.editTargetUserInfos.getTargetUserInformation(target);

      final List<Target> calibratorsModel = targetInfo.getCalibrators();

      if (!calibratorsModel.contains(calibrator)) {
        calibratorsModel.add(calibrator);

        // Add node :
        this.addNodeAndRefresh(destNode, calibrator);

        return true;
      }
    }
    return false;
  }
}
