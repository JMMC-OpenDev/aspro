/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: GenericJTree.java,v 1.3 2010-12-07 17:34:46 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2010/12/06 17:01:14  bourgesl
 * findNode(parent, node) made public
 *
 * Revision 1.1  2010/12/03 16:10:57  bourgesl
 * JTree utility classes to handle targets / models
 *
 */
package fr.jmmc.aspro.gui.util;

import java.util.logging.Level;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

/**
 * This custom JTree implementation provides several utility methods to manipulate
 * DefaultMutableTreeNode and visual representation of nodes
 *
 * @author bourgesl
 */
public class GenericJTree extends JTree {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.util.GenericJTree";
  /** Class logger */
  protected static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /**
   * Public constructor changing default values : SINGLE_TREE_SELECTION
   */
  public GenericJTree() {
    super();

    // single tree selection :
    this.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
  }

  /**
   * Return the tree model
   * @return tree model
   */
  public final DefaultTreeModel getTreeModel() {
    return ((DefaultTreeModel) this.getModel());
  }

  /**
   * Create a new node using the given user object and add it to the given parent node
   * @param parentNode parent node
   * @param userObject user object to create the new node
   * @return new created node
   */
  public DefaultMutableTreeNode addNode(final DefaultMutableTreeNode parentNode, final Object userObject) {
    final DefaultMutableTreeNode modelNode = new DefaultMutableTreeNode(userObject);

    parentNode.add(modelNode);

    return modelNode;
  }

  /**
   * Create a new node using the given user object and add it to the given parent node
   * and Fire node structure changed on the parent node
   * @param parentNode parent node
   * @param userObject user object to create the new node
   */
  public void addNodeAndRefresh(final DefaultMutableTreeNode parentNode, final Object userObject) {
    final DefaultMutableTreeNode newNode = this.addNode(parentNode, userObject);

    // fire node structure changed :
    this.fireNodeChanged(parentNode);

    // Select the new node = model :
    this.selectPath(new TreePath(newNode.getPath()));
  }

  /**
   * Remove the given current node from the parent node
   * and Fire node structure changed on the parent node
   * @param parentNode parent node
   * @param currentNode node to remove
   */
  public void removeNodeAndRefresh(final DefaultMutableTreeNode parentNode, final DefaultMutableTreeNode currentNode) {
    this.removeNodeAndRefresh(parentNode, currentNode, true);
  }

  /**
   * Remove the given current node from the parent node
   * and Fire node structure changed on the parent node
   * @param parentNode parent node
   * @param currentNode node to remove
   * @param doSelectParent flag to indicate to select the parent node once the node removed
   */
  public void removeNodeAndRefresh(final DefaultMutableTreeNode parentNode, final DefaultMutableTreeNode currentNode, final boolean doSelectParent) {
    parentNode.remove(currentNode);

    // fire node structure changed :
    this.fireNodeChanged(parentNode);

    if (doSelectParent) {
      // Select the parent node = target :
      this.selectPath(new TreePath(parentNode.getPath()));
    }
  }

  /**
   * Fire node structure changed on the given tree node
   * @param node changed tree node
   */
  public final void fireNodeChanged(final TreeNode node) {
    // fire node structure changed :
    this.getTreeModel().nodeStructureChanged(node);
  }

  /**
   * Return the root node
   * @return root node
   */
  public final DefaultMutableTreeNode getRootNode() {
    return (DefaultMutableTreeNode) getTreeModel().getRoot();
  }

  /**
   * Return the node corresponding to the last selected path in the tree
   * @return node or null
   */
  public final DefaultMutableTreeNode getLastSelectedNode() {
    return (DefaultMutableTreeNode) this.getLastSelectedPathComponent();
  }

  /**
   * Find the first tree node having the given user object
   * @param userObject user object to locate in the tree
   * @return tree node or null
   */
  public final DefaultMutableTreeNode findTreeNode(final Object userObject) {
    return findTreeNode(getRootNode(), userObject);
  }

  /**
   * Find the first tree node having the given user object recursively
   *
   * @param node current node to traverse
   * @param userObject user object to locate in the tree
   * @return tree node or null
   */
  public static DefaultMutableTreeNode findTreeNode(final DefaultMutableTreeNode node, final Object userObject) {
    if (node.getUserObject() == userObject) {
      return node;
    }

    final int size = node.getChildCount();
    if (size > 0) {
      DefaultMutableTreeNode result = null;

      DefaultMutableTreeNode childNode;
      for (int i = 0; i < size; i++) {
        childNode = (DefaultMutableTreeNode) node.getChildAt(i);

        result = findTreeNode(childNode, userObject);
        if (result != null) {
          return result;
        }
      }
    }
    return null;
  }

  /**
   * Change the selected path in the tree
   * This will send a selection event changed that will refresh the UI
   *
   * @param path tree path
   */
  public final void selectPath(final TreePath path) {
    this.setSelectionPath(path);
    this.scrollPathToVisible(path);
  }

  /**
   * Called by the renderers to convert the specified value to
   * text. This implementation returns <code>value.toString</code>, ignoring
   * all other arguments. To control the conversion, subclass this
   * method and use any of the arguments you need.
   *
   * @param value the <code>Object</code> to convert to text
   * @param selected true if the node is selected
   * @param expanded true if the node is expanded
   * @param leaf  true if the node is a leaf node
   * @param row  an integer specifying the node's display row, where 0 is
   *             the first row in the display
   * @param hasFocus true if the node has the focus
   * @return the <code>String</code> representation of the node's value
   */
  @Override
  public final String convertValueToText(
          final Object value,
          final boolean selected,
          final boolean expanded, final boolean leaf, final int row,
          final boolean hasFocus) {

    if (value != null) {
      String sValue = null;

      if (value instanceof DefaultMutableTreeNode) {
        final DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;
        final Object userObject = node.getUserObject();

        if (userObject != null) {
          sValue = convertUserObjectToString(userObject);
        }

      } else {
        if (logger.isLoggable(Level.SEVERE)) {
          logger.severe("unsupported class type = " + value.getClass());
        }

        sValue = value.toString();
      }

      if (sValue != null) {
        return sValue;
      }

    }
    return "";
  }

  /**
   * Convert a value object to string
   * @param userObject user object to convert
   * @return string representation of the user object
   */
  protected String convertUserObjectToString(final Object userObject) {
    if (!(userObject instanceof String) && logger.isLoggable(Level.SEVERE)) {
      logger.severe("Unsupported class type = " + userObject.getClass());
    }
    // String representation :
    return userObject.toString();
  }
}
