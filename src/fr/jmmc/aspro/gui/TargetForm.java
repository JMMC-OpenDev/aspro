/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: TargetForm.java,v 1.2 2010-11-19 16:57:04 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/11/18 17:20:33  bourgesl
 * initial GUI for target editor
 *
 */
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.mcs.gui.MessagePane;
import java.util.List;
import java.util.logging.Level;
import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

/**
 * This class represents the target information editor ...
 *
 * TODO : extract intermediate class for Tree methods
 *
 * @author bourgesl
 */
public final class TargetForm extends javax.swing.JPanel implements TreeSelectionListener {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.TargetForm";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /* members */
  /** list of edited targets (clone) */
  private List<Target> editTargets;
  /** current edited target */
  private Target currentTarget = null;

  /**
   * Creates new form TargetModelForm
   */
  public TargetForm() {
    initComponents();

    postInit();
  }

  /**
   * This method is useful to set the models and specific features of initialized swing components.
   */
  private void postInit() {

    // single tree selection :
    this.jTreeModels.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    // tree selection listener :
    this.jTreeModels.addTreeSelectionListener(this);

  }

  /* Tree related methods */
  /**
   * Return the tree model
   * @return tree model
   */
  private DefaultTreeModel getTreeModelsModel() {
    return ((DefaultTreeModel) this.jTreeModels.getModel());
  }

  /**
   * Return the root node (Models)
   * @return root node
   */
  private DefaultMutableTreeNode getRootNode() {
    return (DefaultMutableTreeNode) getTreeModelsModel().getRoot();
  }

  /**
   * Return the node corresponding to the last selected path in the tree
   * @return node or null
   */
  private DefaultMutableTreeNode getLastSelectedNode() {
    return (DefaultMutableTreeNode) this.jTreeModels.getLastSelectedPathComponent();
  }

  /**
   * Generate the tree from the given list of targets (single or all)
   * @param targets list of targets to edit
   */
  protected void generateTree(final List<Target> targets) {

    this.editTargets = targets;

    final DefaultMutableTreeNode rootNode = getRootNode();

    DefaultMutableTreeNode targetNode;
    for (Target target : targets) {

      targetNode = new DefaultMutableTreeNode(target);

      // TODO : add calibrators as children of the target Node
/*
      for (Model model : target.getModels()) {
      this.generateModelNodes(targetNode, model);
      }
       */
      rootNode.add(targetNode);
    }
    // fire node structure changed :
    getTreeModelsModel().nodeStructureChanged(rootNode);
  }

  /**
   * Find the first tree node having the given user object
   * @param userObject user object to locate in the tree
   * @return tree node or null
   */
  private DefaultMutableTreeNode findTreeNode(final Object userObject) {
    return findTreeNode(getRootNode(), userObject);
  }

  /**
   * Find the first tree node having the given user object recursively
   *
   * @param node current node to traverse
   * @param userObject user object to locate in the tree
   * @return tree node or null
   */
  private static DefaultMutableTreeNode findTreeNode(final DefaultMutableTreeNode node, final Object userObject) {
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
   * Select the target node for the given target name
   * @param targetName target name indicating which node to select
   */
  protected void selectTarget(final String targetName) {

    if (targetName != null) {
      final Target target = Target.getTarget(targetName, this.editTargets);

      if (target != null) {
        final DefaultMutableTreeNode targetNode = this.findTreeNode(target);

        if (targetNode != null) {
          // Select the target node :
          this.selectPath(new TreePath(targetNode.getPath()));

          // expand target node if there is at least one model :
          if (!targetNode.isLeaf()) {
            final DefaultMutableTreeNode child = (DefaultMutableTreeNode) targetNode.getFirstChild();

            this.jTreeModels.scrollPathToVisible(new TreePath(child.getPath()));
          }

          return;
        }
      }
    }

    // select first target :
    selectFirstTarget(getRootNode());
  }

  /**
   * Select the first target
   * @param rootNode root node
   */
  private void selectFirstTarget(final DefaultMutableTreeNode rootNode) {
    // first child = first target :
    final DefaultMutableTreeNode firstChild = (DefaultMutableTreeNode) rootNode.getFirstChild();

    this.selectPath(new TreePath(firstChild.getPath()));

    // expand target node if there is at least one model :
    if (!firstChild.isLeaf()) {
      final DefaultMutableTreeNode secondChild = (DefaultMutableTreeNode) firstChild.getFirstChild();

      this.jTreeModels.scrollPathToVisible(new TreePath(secondChild.getPath()));
    }
  }

  /**
   * Change the selected path in the tree (target / models)
   * This will send a selection event changed that will refresh the UI (buttons + parameters table)
   *
   * @param path tree path
   */
  private void selectPath(final TreePath path) {
    this.jTreeModels.setSelectionPath(path);
    this.jTreeModels.scrollPathToVisible(path);
  }

  /**
   * Process the tree selection events
   * @param e tree selection event
   */
  public void valueChanged(final TreeSelectionEvent e) {
    final DefaultMutableTreeNode node = getLastSelectedNode();

    /* if nothing is selected */
    if (node == null) {
      return;
    }

    /* React to the node selection. */

    // Check if it is the root node :
    final DefaultMutableTreeNode rootNode = getRootNode();
    if (node == rootNode) {
      selectFirstTarget(rootNode);
      return;
    }

    /* retrieve the node that was selected */
    final Object userObject = node.getUserObject();

    if (userObject != null) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("tree selection : " + userObject);
      }

      if (userObject instanceof Target) {
        // Target :
        this.processTargetSelection((Target) userObject);
      }
    }
  }

  /**
   * Update the UI when a target is selected in the target/model tree
   * @param target selected target
   */
  private void processTargetSelection(final Target target) {

    // name :
    this.jTextFieldName.setText(target.getName());
    // RA / DEC :
    this.jTextFieldRA.setText(target.getRA());
    this.jTextFieldDEC.setText(target.getDEC());

    // Fluxes :
    Double flux = target.getFLUXV();
    this.jTextFieldFluxV.setText((flux != null) ? Double.toString(flux) : "99");
    flux = target.getFLUXI();
    this.jTextFieldFluxI.setText((flux != null) ? Double.toString(flux) : "99");
    flux = target.getFLUXJ();
    this.jTextFieldFluxJ.setText((flux != null) ? Double.toString(flux) : "99");
    flux = target.getFLUXH();
    this.jTextFieldFluxH.setText((flux != null) ? Double.toString(flux) : "99");
    flux = target.getFLUXK();
    this.jTextFieldFluxK.setText((flux != null) ? Double.toString(flux) : "99");
    flux = target.getFLUXN();
    this.jTextFieldFluxN.setText((flux != null) ? Double.toString(flux) : "99");

  }

  /**
   * This method is called from within the constructor to
   * initialize the form.
   * WARNING: Do NOT modify this code. The content of this method is
   * always regenerated by the Form Editor.
   */
  @SuppressWarnings("unchecked")
  // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
  private void initComponents() {
    java.awt.GridBagConstraints gridBagConstraints;

    jPanelTargets = new javax.swing.JPanel();
    jScrollPaneTreeModels = new javax.swing.JScrollPane();
    jTreeModels = createJTree();
    jPanelModelActions = new javax.swing.JPanel();
    jButtonUp = new javax.swing.JButton();
    jButtonDown = new javax.swing.JButton();
    jToggleButtonMarkCal = new javax.swing.JToggleButton();
    jToggleButtonAssociateCal = new javax.swing.JToggleButton();
    jSeparatorButtons = new javax.swing.JSeparator();
    jPanelTarget = new javax.swing.JPanel();
    jLabel1 = new javax.swing.JLabel();
    jTextFieldName = new javax.swing.JTextField();
    jLabel2 = new javax.swing.JLabel();
    jSeparator2 = new javax.swing.JSeparator();
    jTextFieldRA = new javax.swing.JTextField();
    jLabel3 = new javax.swing.JLabel();
    jTextFieldDEC = new javax.swing.JTextField();
    jSeparator3 = new javax.swing.JSeparator();
    jLabel4 = new javax.swing.JLabel();
    jTextFieldFluxV = new javax.swing.JTextField();
    jLabel5 = new javax.swing.JLabel();
    jTextFieldFluxI = new javax.swing.JTextField();
    jLabel6 = new javax.swing.JLabel();
    jTextFieldFluxJ = new javax.swing.JTextField();
    jLabel7 = new javax.swing.JLabel();
    jTextFieldFluxH = new javax.swing.JTextField();
    jLabel8 = new javax.swing.JLabel();
    jTextFieldFluxK = new javax.swing.JTextField();
    jLabel9 = new javax.swing.JLabel();
    jTextFieldFluxN = new javax.swing.JTextField();
    jPanelDescription = new javax.swing.JPanel();
    jScrollPaneTargetInfos = new javax.swing.JScrollPane();
    jTextAreaTargetInfos = new javax.swing.JTextArea();

    setLayout(new java.awt.GridBagLayout());

    jPanelTargets.setMinimumSize(new java.awt.Dimension(300, 100));
    jPanelTargets.setPreferredSize(new java.awt.Dimension(300, 100));
    jPanelTargets.setLayout(new java.awt.GridBagLayout());

    jScrollPaneTreeModels.setMinimumSize(new java.awt.Dimension(100, 100));
    jScrollPaneTreeModels.setPreferredSize(new java.awt.Dimension(100, 100));

    javax.swing.tree.DefaultMutableTreeNode treeNode1 = new javax.swing.tree.DefaultMutableTreeNode("Targets");
    jTreeModels.setModel(new javax.swing.tree.DefaultTreeModel(treeNode1));
    jScrollPaneTreeModels.setViewportView(jTreeModels);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.gridheight = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.2;
    gridBagConstraints.weighty = 1.0;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTargets.add(jScrollPaneTreeModels, gridBagConstraints);

    jPanelModelActions.setBorder(javax.swing.BorderFactory.createTitledBorder("Target actions"));
    jPanelModelActions.setEnabled(false);
    jPanelModelActions.setMinimumSize(new java.awt.Dimension(200, 80));
    jPanelModelActions.setPreferredSize(new java.awt.Dimension(200, 80));
    jPanelModelActions.setLayout(new java.awt.GridBagLayout());

    jButtonUp.setText("Up");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
    jPanelModelActions.add(jButtonUp, gridBagConstraints);

    jButtonDown.setText("Down");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
    jPanelModelActions.add(jButtonDown, gridBagConstraints);

    jToggleButtonMarkCal.setText("Mark as calibrator");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
    jPanelModelActions.add(jToggleButtonMarkCal, gridBagConstraints);

    jToggleButtonAssociateCal.setText("Associate ...");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
    jPanelModelActions.add(jToggleButtonAssociateCal, gridBagConstraints);

    jSeparatorButtons.setOrientation(javax.swing.SwingConstants.VERTICAL);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.gridheight = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
    gridBagConstraints.insets = new java.awt.Insets(0, 20, 0, 20);
    jPanelModelActions.add(jSeparatorButtons, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weighty = 0.1;
    jPanelTargets.add(jPanelModelActions, gridBagConstraints);

    jPanelTarget.setBorder(javax.swing.BorderFactory.createTitledBorder("Target"));
    jPanelTarget.setLayout(new java.awt.GridBagLayout());

    jLabel1.setText("Name");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabel1, gridBagConstraints);

    jTextFieldName.setColumns(10);
    jTextFieldName.setEditable(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jTextFieldName, gridBagConstraints);

    jLabel2.setText("RA [HMS]");
    jLabel2.setToolTipText("RA coordinate (J2000) (HMS)");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabel2, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.weighty = 0.1;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jSeparator2, gridBagConstraints);

    jTextFieldRA.setColumns(10);
    jTextFieldRA.setEditable(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jTextFieldRA, gridBagConstraints);

    jLabel3.setText("DEC [DMS]");
    jLabel3.setToolTipText("DEC coordinate (J2000) (DMS)");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 3;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabel3, gridBagConstraints);

    jTextFieldDEC.setColumns(10);
    jTextFieldDEC.setEditable(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jTextFieldDEC, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 4;
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.weighty = 0.1;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jSeparator3, gridBagConstraints);

    jLabel4.setText("Magnitude V");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 5;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabel4, gridBagConstraints);

    jTextFieldFluxV.setColumns(10);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 5;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jTextFieldFluxV, gridBagConstraints);

    jLabel5.setText("Magnitude I");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 6;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabel5, gridBagConstraints);

    jTextFieldFluxI.setColumns(10);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 6;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jTextFieldFluxI, gridBagConstraints);

    jLabel6.setText("Magnitude J");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 7;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabel6, gridBagConstraints);

    jTextFieldFluxJ.setColumns(10);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 7;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jTextFieldFluxJ, gridBagConstraints);

    jLabel7.setText("Magnitude H");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 8;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabel7, gridBagConstraints);

    jTextFieldFluxH.setColumns(10);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 8;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jTextFieldFluxH, gridBagConstraints);

    jLabel8.setText("Magnitude K");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 9;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabel8, gridBagConstraints);

    jTextFieldFluxK.setColumns(10);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 9;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jTextFieldFluxK, gridBagConstraints);

    jLabel9.setText("Magnitude N");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 10;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabel9, gridBagConstraints);

    jTextFieldFluxN.setColumns(10);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 10;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jTextFieldFluxN, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.8;
    gridBagConstraints.weighty = 0.7;
    jPanelTargets.add(jPanelTarget, gridBagConstraints);

    jPanelDescription.setBorder(javax.swing.BorderFactory.createTitledBorder("Target information"));
    jPanelDescription.setMaximumSize(new java.awt.Dimension(2147483647, 250));
    jPanelDescription.setMinimumSize(new java.awt.Dimension(100, 150));
    jPanelDescription.setPreferredSize(new java.awt.Dimension(100, 150));
    jPanelDescription.setLayout(new java.awt.GridBagLayout());

    jTextAreaTargetInfos.setColumns(20);
    jTextAreaTargetInfos.setRows(5);
    jScrollPaneTargetInfos.setViewportView(jTextAreaTargetInfos);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.weighty = 1.0;
    jPanelDescription.add(jScrollPaneTargetInfos, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weighty = 0.2;
    jPanelTargets.add(jPanelDescription, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.weighty = 0.3;
    add(jPanelTargets, gridBagConstraints);
  }// </editor-fold>//GEN-END:initComponents

  /**
   * Validate the form
   * @return true only if the data are valid
   */
  protected boolean validateForm() {
    // Validate the models :
    for (Target target : this.editTargets) {
      try {
        // TODO
      } catch (IllegalArgumentException iae) {
        // display an error message for the first error found :
        MessagePane.showErrorMessage(
                iae.getMessage(), "Error on target " + target.getName());

        // stop and continue editing the form :
        return false;
      }
    }
    return true;
  }
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JButton jButtonDown;
  private javax.swing.JButton jButtonUp;
  private javax.swing.JLabel jLabel1;
  private javax.swing.JLabel jLabel2;
  private javax.swing.JLabel jLabel3;
  private javax.swing.JLabel jLabel4;
  private javax.swing.JLabel jLabel5;
  private javax.swing.JLabel jLabel6;
  private javax.swing.JLabel jLabel7;
  private javax.swing.JLabel jLabel8;
  private javax.swing.JLabel jLabel9;
  private javax.swing.JPanel jPanelDescription;
  private javax.swing.JPanel jPanelModelActions;
  private javax.swing.JPanel jPanelTarget;
  private javax.swing.JPanel jPanelTargets;
  private javax.swing.JScrollPane jScrollPaneTargetInfos;
  private javax.swing.JScrollPane jScrollPaneTreeModels;
  private javax.swing.JSeparator jSeparator2;
  private javax.swing.JSeparator jSeparator3;
  private javax.swing.JSeparator jSeparatorButtons;
  private javax.swing.JTextArea jTextAreaTargetInfos;
  private javax.swing.JTextField jTextFieldDEC;
  private javax.swing.JTextField jTextFieldFluxH;
  private javax.swing.JTextField jTextFieldFluxI;
  private javax.swing.JTextField jTextFieldFluxJ;
  private javax.swing.JTextField jTextFieldFluxK;
  private javax.swing.JTextField jTextFieldFluxN;
  private javax.swing.JTextField jTextFieldFluxV;
  private javax.swing.JTextField jTextFieldName;
  private javax.swing.JTextField jTextFieldRA;
  private javax.swing.JToggleButton jToggleButtonAssociateCal;
  private javax.swing.JToggleButton jToggleButtonMarkCal;
  private javax.swing.JTree jTreeModels;
  // End of variables declaration//GEN-END:variables

  /**
   * Create a custom JTree which convertValueToText() method is overriden
   * @return JTree
   */
  private static JTree createJTree() {
    return new JTree() {

      /** default serial UID for Serializable interface */
      private static final long serialVersionUID = 1;

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
      public String convertValueToText(
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

              if (userObject instanceof String) {
                // String type = root node :
                sValue = userObject.toString();
              } else if (userObject instanceof Target) {
                // Target :
                sValue = ((Target) userObject).getName();
              } else {
                if (logger.isLoggable(Level.SEVERE)) {
                  logger.severe("unsupported class type = " + userObject.getClass());
                }

              }
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
    };
  }
}
