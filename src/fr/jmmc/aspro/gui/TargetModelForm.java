/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: TargetModelForm.java,v 1.7 2010-02-17 17:09:01 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.6  2010/02/16 14:49:43  bourgesl
 * added OK/cancel i.e. use cloned targets in editor and use the ObservationManager to apply changes
 *
 * Revision 1.5  2010/02/15 16:47:26  bourgesl
 * model editor supports add / remove model
 *
 * Revision 1.4  2010/02/12 15:53:18  bourgesl
 * added target model editor
 *
 * Revision 1.3  2010/02/08 17:00:47  bourgesl
 * L&F changes
 *
 * Revision 1.2  2010/02/01 09:43:49  bourgesl
 * header + comments
 *
 */
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.AsproGui;
import fr.jmmc.aspro.gui.util.ComponentResizeAdapter;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.mcs.model.ModelManager;
import fr.jmmc.mcs.model.gui.ModelParameterTableModel;
import fr.jmmc.mcs.model.gui.ModelParameterTableModel.Mode;
import fr.jmmc.mcs.model.targetmodel.Model;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import javax.swing.DefaultCellEditor;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JTree;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

/**
 * This class represents the target model editor ...
 * @author bourgesl
 */
public class TargetModelForm extends javax.swing.JPanel implements ActionListener, TreeSelectionListener, ListSelectionListener {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.TargetModelForm";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /* members */
  /** editor result = true if the user validates the inputs */
  private boolean result = false;
  /** list of edited targets (clone) */
  private List<Target> editTargets = new ArrayList<Target>();
  /** current edited target to detect target changes to update the table model */
  private Target currentTarget = null;
  /** current model type to detect type changes to update the model description */
  private String currentModelType = null;
  /* Swing */
  /** dialog window */
  private JDialog dialog;

  /**
   * Display the model editor using the given target name as the initial selected target
   * @return true if the model editor changed anything
   */
  public static boolean showModelEditor(final String targetName) {

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("showing Model Editor : " + targetName);
    }

    boolean result = false;

    // Prepare the list of targets :
    final ObservationSetting observation = ObservationManager.getInstance().getObservation();

    List<Target> targets;
    if (targetName != null) {
      // single target editor :
      targets = Arrays.asList(new Target[]{ObservationManager.getTarget(observation, targetName)});
    } else {
      // full editor :
      targets = observation.getTargets();
    }

    final TargetModelForm form = new TargetModelForm();

    // generate the target/model tree :
    form.generateTree(targets);

    JDialog dialog = null;

    try {
      // 1. Create the dialog
      dialog = new JDialog(AsproGui.getInstance().getRootFrame(), "Model Editor", true);

      final Dimension dim = new Dimension(600, 600);
      dialog.setMinimumSize(dim);
      dialog.addComponentListener(new ComponentResizeAdapter(dim));

      // 2. Optional: What happens when the dialog closes ?
      dialog.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

      // 3. Create components and put them in the dialog
      dialog.add(form);

      // Associate the dialog to the form :
      form.setDialog(dialog);

      // 4. Size the dialog.
      dialog.pack();

      // Center it :
      dialog.setLocationRelativeTo(dialog.getOwner());

      // 5. Show it.
      dialog.setVisible(true);

      // get editor result :
      result = form.isResult();

    } finally {
      if (dialog != null) {
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("dispose Model Editor ...");
        }
        dialog.dispose();
      }
    }

    if (result) {
      if (targetName != null) {
        // single target editor :
        ObservationManager.replaceTarget(observation, form.getEditTargets().get(0));
      } else {
        // full editor :
        ObservationManager.setTargets(observation, form.getEditTargets());
      }
    }

    return result;
  }

  /**
   * Creates new form TargetModelForm
   */
  public TargetModelForm() {
    initComponents();

    postInit();
  }

  /**
   * This method is useful to set the models and specific features of initialized swing components.
   */
  private void postInit() {

    // model type choice :
    this.jComboBoxModelType.setModel(new DefaultComboBoxModel(ModelManager.getInstance().getSupportedModels()));
    this.jComboBoxModelType.addActionListener(this);

    // set one click edition on following table and show all decimals in numerical values
    ((DefaultCellEditor) this.jTableModelParameters.getDefaultEditor(String.class)).setClickCountToStart(1);
    this.jTableModelParameters.setDefaultEditor(Double.class, this.jTableModelParameters.getDefaultEditor(String.class));
    this.jTableModelParameters.setDefaultRenderer(Double.class, this.jTableModelParameters.getDefaultRenderer(String.class));

    // single table selection :
    this.jTableModelParameters.getSelectionModel().setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    this.jTableModelParameters.getSelectionModel().addListSelectionListener(this);

    // Fix lost focus issues on JTable :
    this.jTableModelParameters.putClientProperty("terminateEditOnFocusLost", Boolean.TRUE);

    // single tree selection :
    this.jTreeModels.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    this.jTreeModels.addTreeSelectionListener(this);

    // update the model description :
    this.updateModelDescription((String) this.jComboBoxModelType.getSelectedItem());
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

    final DefaultMutableTreeNode rootNode = getRootNode();

    Target target;
    DefaultMutableTreeNode targetNode;
    for (Target t : targets) {
      // clone target and models to allow undo/cancel actions :
      target = (Target) t.clone();

      // add the cloned target to the edited targets :
      this.editTargets.add(target);

      targetNode = new DefaultMutableTreeNode(target);

      for (Model model : target.getModels()) {
        this.generateModelNodes(targetNode, model);
      }

      rootNode.add(targetNode);
    }
    // fire node structure changed :
    getTreeModelsModel().nodeStructureChanged(rootNode);

    // select first target :
    selectFirstTarget(rootNode);
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
   * Generate the model nodes recursively
   *
   * @param parentNode parent node to append nodes
   * @param model model to process
   */
  private void generateModelNodes(final DefaultMutableTreeNode parentNode, final Model model) {

    // May need clone target and model to allow undo/cancel actions :

    final DefaultMutableTreeNode modelNode = new DefaultMutableTreeNode(model);

    parentNode.add(modelNode);

    final List<Model> children = model.getModels();
    if (!children.isEmpty()) {
      for (Model child : children) {
        generateModelNodes(modelNode, child);
      }
    }
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

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("tree selection node : " + node);
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
      } else if (userObject instanceof Model) {
        final TreeNode[] path = node.getPath();
        // target is after the root node in the tree path :
        final Target target = (Target) ((DefaultMutableTreeNode) path[1]).getUserObject();
        // Model :
        this.processModelSelection(target, (Model) userObject);
      }
    }
  }

  /**
   * Update the UI when a target is selected in the target/model tree
   * @param target selected target
   */
  private void processTargetSelection(final Target target) {
    this.jButtonAdd.setEnabled(true);
    this.jButtonUpdate.setEnabled(false);
    this.jButtonRemove.setEnabled(false);

    // reset text field name :
    this.jTextFieldName.setText(null);

    // update parameter table model :
    this.defineModels(target);
  }

  /**
   * Update the UI when a model is selected in the target/model tree
   * @param model selected model
   */
  private void processModelSelection(final Target target, final Model model) {
    // check if the model is a group ?
    this.jButtonAdd.setEnabled(false);
    this.jButtonUpdate.setEnabled(true);
    this.jButtonRemove.setEnabled(true);

    // update text field name :
    this.jTextFieldName.setText(model.getName());

    // update model type :
    this.jComboBoxModelType.setSelectedItem(model.getType());

    // update parameter table model :
    this.defineModels(target);
  }

  /**
   * Define the list of models to use in the table of parameters
   * @param target target models to use
   */
  private void defineModels(final Target target) {
    if (target != currentTarget) {
      this.currentTarget = target;
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("target changed : " + target);
      }
      getModelParameterTableModel().setData(target.getModels());
    }
  }

  /**
   * Return the custom table model
   * @return ModelParameterTableModel
   */
  private ModelParameterTableModel getModelParameterTableModel() {
    return (ModelParameterTableModel) this.jTableModelParameters.getModel();
  }

  /**
   * Process any comboBox change event
   * @param e action event
   */
  public void actionPerformed(final ActionEvent e) {
    if (e.getSource() == this.jComboBoxModelType) {
      final String type = (String) this.jComboBoxModelType.getSelectedItem();
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("model type changed : " + type);
      }
      updateModelDescription(type);
    }
  }

  /**
   * Update the model description given the model type
   * @param type model type to use
   */
  private void updateModelDescription(final String type) {
    if (!type.equals(this.currentModelType)) {
      this.currentModelType = type;

      final String desc = ModelManager.getInstance().getModelDescription(type);

      // note : use html to have multi line label :
      this.jLabelModelDescrption.setText("<html>" + desc.replaceAll("\n", "<br>") + "</html>");
    }
  }

  /* JTable selection listener */
  /**
   * Called whenever the value of the selection changes.
   * @param e the event that characterizes the change.
   */
  public void valueChanged(final ListSelectionEvent e) {
    final ListSelectionModel lsm = (ListSelectionModel) e.getSource();

    if (e.getValueIsAdjusting() || lsm.isSelectionEmpty()) {
      return;
    }

    // Single selection mode :
    final int minIndex = lsm.getMinSelectionIndex();

    if (minIndex != -1) {
      final Model model = getModelParameterTableModel().getModelAt(minIndex);

      final DefaultMutableTreeNode modelNode = this.findTreeNode(model);

      if (modelNode != null) {
        // Select the model node :
        this.selectPath(new TreePath(modelNode.getPath()));
      }
    }
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
    jPanel4 = new javax.swing.JPanel();
    jComboBoxModelType = new javax.swing.JComboBox();
    jButtonAdd = new javax.swing.JButton();
    jButtonRemove = new javax.swing.JButton();
    jButtonUpdate = new javax.swing.JButton();
    jLabel3 = new javax.swing.JLabel();
    jLabel5 = new javax.swing.JLabel();
    jTextFieldName = new javax.swing.JTextField();
    jPanelDescription = new javax.swing.JPanel();
    jScrollPaneModelDescription = new javax.swing.JScrollPane();
    jLabelModelDescrption = new javax.swing.JLabel();
    jPanelParameters = new javax.swing.JPanel();
    jScrollPaneTableModelParameters = new javax.swing.JScrollPane();
    jTableModelParameters = new javax.swing.JTable();
    jPanelButtons = new javax.swing.JPanel();
    jButtonOK = new javax.swing.JButton();
    jButtonCancel = new javax.swing.JButton();

    setLayout(new java.awt.GridBagLayout());

    jPanelTargets.setBorder(javax.swing.BorderFactory.createTitledBorder("Targets"));
    jPanelTargets.setMinimumSize(new java.awt.Dimension(300, 100));
    jPanelTargets.setPreferredSize(new java.awt.Dimension(300, 100));
    jPanelTargets.setLayout(new java.awt.GridBagLayout());

    jScrollPaneTreeModels.setMinimumSize(new java.awt.Dimension(100, 100));
    jScrollPaneTreeModels.setPreferredSize(new java.awt.Dimension(100, 100));

    javax.swing.tree.DefaultMutableTreeNode treeNode1 = new javax.swing.tree.DefaultMutableTreeNode("Models");
    jTreeModels.setModel(new javax.swing.tree.DefaultTreeModel(treeNode1));
    jScrollPaneTreeModels.setViewportView(jTreeModels);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.4;
    gridBagConstraints.weighty = 1.0;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTargets.add(jScrollPaneTreeModels, gridBagConstraints);

    jPanel4.setBorder(javax.swing.BorderFactory.createTitledBorder("Model actions"));
    jPanel4.setMinimumSize(new java.awt.Dimension(200, 80));
    jPanel4.setPreferredSize(new java.awt.Dimension(200, 80));
    jPanel4.setLayout(new java.awt.GridBagLayout());

    jComboBoxModelType.setMinimumSize(new java.awt.Dimension(100, 24));
    jComboBoxModelType.setPreferredSize(new java.awt.Dimension(100, 24));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanel4.add(jComboBoxModelType, gridBagConstraints);

    jButtonAdd.setText("Add");
    jButtonAdd.setEnabled(false);
    jButtonAdd.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonAddActionPerformed(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanel4.add(jButtonAdd, gridBagConstraints);

    jButtonRemove.setText("Remove");
    jButtonRemove.setEnabled(false);
    jButtonRemove.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonRemoveActionPerformed(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanel4.add(jButtonRemove, gridBagConstraints);

    jButtonUpdate.setText("Update");
    jButtonUpdate.setEnabled(false);
    jButtonUpdate.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonUpdateActionPerformed(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanel4.add(jButtonUpdate, gridBagConstraints);

    jLabel3.setText("model type");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanel4.add(jLabel3, gridBagConstraints);

    jLabel5.setText("Name");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanel4.add(jLabel5, gridBagConstraints);

    jTextFieldName.setColumns(15);
    jTextFieldName.setEditable(false);
    jTextFieldName.setMinimumSize(new java.awt.Dimension(100, 19));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanel4.add(jTextFieldName, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.6;
    gridBagConstraints.weighty = 1.0;
    jPanelTargets.add(jPanel4, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.weighty = 0.2;
    add(jPanelTargets, gridBagConstraints);

    jPanelDescription.setBorder(javax.swing.BorderFactory.createTitledBorder("Model description"));
    jPanelDescription.setMaximumSize(new java.awt.Dimension(2147483647, 250));
    jPanelDescription.setMinimumSize(new java.awt.Dimension(100, 150));
    jPanelDescription.setPreferredSize(new java.awt.Dimension(100, 150));
    jPanelDescription.setLayout(new java.awt.GridBagLayout());

    jLabelModelDescrption.setBackground(new java.awt.Color(255, 255, 255));
    jLabelModelDescrption.setFont(new java.awt.Font("Dialog", 0, 12));
    jLabelModelDescrption.setOpaque(true);
    jScrollPaneModelDescription.setViewportView(jLabelModelDescrption);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.weighty = 1.0;
    jPanelDescription.add(jScrollPaneModelDescription, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weighty = 0.2;
    add(jPanelDescription, gridBagConstraints);

    jPanelParameters.setBorder(javax.swing.BorderFactory.createTitledBorder("Model Parameters"));
    jPanelParameters.setMinimumSize(new java.awt.Dimension(300, 100));
    jPanelParameters.setPreferredSize(new java.awt.Dimension(300, 100));
    jPanelParameters.setLayout(new java.awt.GridBagLayout());

    jScrollPaneTableModelParameters.setMinimumSize(new java.awt.Dimension(200, 100));
    jScrollPaneTableModelParameters.setPreferredSize(new java.awt.Dimension(200, 100));

    jTableModelParameters.setModel(new ModelParameterTableModel(Mode.ASPRO));
    jTableModelParameters.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_ALL_COLUMNS);
    jTableModelParameters.setFillsViewportHeight(true);
    jScrollPaneTableModelParameters.setViewportView(jTableModelParameters);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.weighty = 1.0;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelParameters.add(jScrollPaneTableModelParameters, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.weighty = 0.5;
    add(jPanelParameters, gridBagConstraints);

    jButtonOK.setText("OK");
    jButtonOK.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonOKActionPerformed(evt);
      }
    });
    jPanelButtons.add(jButtonOK);

    jButtonCancel.setText("Cancel");
    jButtonCancel.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonCancelActionPerformed(evt);
      }
    });
    jPanelButtons.add(jButtonCancel);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 3;
    gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    add(jPanelButtons, gridBagConstraints);
  }// </editor-fold>//GEN-END:initComponents

  /**
   * Process the OK action
   * @param evt action event
   */
  private void jButtonOKActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonOKActionPerformed
    // update the validation flag :
    this.result = true;

    if (this.dialog != null) {
      this.dialog.setVisible(false);
    }
  }//GEN-LAST:event_jButtonOKActionPerformed

  /**
   * Process the Update action
   * @param evt action event
   */
  private void jButtonUpdateActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonUpdateActionPerformed

    final DefaultMutableTreeNode currentNode = getLastSelectedNode();

    if (currentNode == null) {
      return;
    }

    if (currentNode.getUserObject() instanceof Model) {

      final Model model = (Model) currentNode.getUserObject();
      if (model != null) {

        final String type = (String) this.jComboBoxModelType.getSelectedItem();

        // check if the type changed :
        if (model.getType().equals(type)) {
          return;
        }

        // Parent can be a target or a model :

        final DefaultMutableTreeNode parentNode = (DefaultMutableTreeNode) currentNode.getParent();

        if (parentNode.getUserObject() instanceof Target) {

          final Target target = (Target) parentNode.getUserObject();

          if (logger.isLoggable(Level.FINE)) {
            logger.fine("update model : " + model);
          }

          // create a new model with defined names (model and parameters) replacing the selected model :
          final Model newModel = ModelManager.getInstance().replaceModel(type, model, target.getModels());

          if (logger.isLoggable(Level.FINE)) {
            logger.fine("new merged model : " + newModel);
          }

          // Remove and add model at the right place :
          int idx;
          idx = target.getModels().indexOf(model);
          target.getModels().remove(idx);
          target.getModels().add(idx, newModel);

          // Replace node :
          final DefaultMutableTreeNode newNode = new DefaultMutableTreeNode(newModel);
          idx = parentNode.getIndex(currentNode);
          parentNode.remove(idx);
          parentNode.insert(newNode, idx);

          // fire node structure changed :
          getTreeModelsModel().nodeStructureChanged(parentNode);

          // force to refresh table model :
          this.currentTarget = null;

          // Select the new node = model :
          this.selectPath(new TreePath(newNode.getPath()));

        } else if (parentNode.getUserObject() instanceof Model) {

          // TODO : later
          JOptionPane.showMessageDialog(this, "Not implemented !");
        }
      }
    } else {
      if (logger.isLoggable(Level.SEVERE)) {
        logger.severe("invalid selected node to perform the update operation = " + currentNode);
      }
    }
  }//GEN-LAST:event_jButtonUpdateActionPerformed

  /**
   * Process the Remove action
   * @param evt action event
   */
  private void jButtonRemoveActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonRemoveActionPerformed
    final DefaultMutableTreeNode currentNode = getLastSelectedNode();

    if (currentNode == null) {
      return;
    }

    if (currentNode.getUserObject() instanceof Model) {

      final Model model = (Model) currentNode.getUserObject();

      if (model != null) {
        // Parent can be a target or a model :

        final DefaultMutableTreeNode parentNode = (DefaultMutableTreeNode) currentNode.getParent();

        if (parentNode.getUserObject() instanceof Target) {

          final Target target = (Target) parentNode.getUserObject();

          if (logger.isLoggable(Level.FINE)) {
            logger.fine("remove model : " + model);
          }

          // Remove model from target :
          int idx;
          idx = target.getModels().indexOf(model);
          target.getModels().remove(idx);

          if (idx == 0) {
            // first model is removed : special case : relocate other models :
            ModelManager.relocateModels(target.getModels());
          }

          // Remove node :
          parentNode.remove(currentNode);

          // fire node structure changed :
          getTreeModelsModel().nodeStructureChanged(parentNode);

          // force to refresh table model :
          this.currentTarget = null;

          // Select the parent node = target :
          this.selectPath(new TreePath(parentNode.getPath()));

        } else if (parentNode.getUserObject() instanceof Model) {

          // TODO : later
          JOptionPane.showMessageDialog(this, "Not implemented !");
        }
      }
    } else {
      if (logger.isLoggable(Level.SEVERE)) {
        logger.severe("invalid selected node to perform the remove operation = " + currentNode);
      }
    }
  }//GEN-LAST:event_jButtonRemoveActionPerformed

  /**
   * Process the Add action
   * @param evt action event
   */
  private void jButtonAddActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonAddActionPerformed
    final DefaultMutableTreeNode currentNode = getLastSelectedNode();

    if (currentNode == null) {
      return;
    }

    if (currentNode.getUserObject() instanceof Target) {

      final Target target = (Target) currentNode.getUserObject();

      if (target != null) {
        final String type = (String) this.jComboBoxModelType.getSelectedItem();

        // create a new model with defined names (model and parameters) :
        final Model newModel = ModelManager.getInstance().newModel(type, target.getModels());

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("add model : " + newModel);
        }

        // Add model to target :
        target.getModels().add(newModel);

        // Add node :
        final DefaultMutableTreeNode newNode = new DefaultMutableTreeNode(newModel);
        currentNode.add(newNode);

        // fire node structure changed :
        getTreeModelsModel().nodeStructureChanged(currentNode);

        // force to refresh table model :
        this.currentTarget = null;

        // Select the new node = model :
        this.selectPath(new TreePath(newNode.getPath()));

      }
    } else {
      if (logger.isLoggable(Level.SEVERE)) {
        logger.severe("invalid selected node to perform the add operation = " + currentNode);
      }
    }
  }//GEN-LAST:event_jButtonAddActionPerformed

  private void jButtonCancelActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonCancelActionPerformed
    if (this.dialog != null) {
      this.dialog.setVisible(false);
    }
  }//GEN-LAST:event_jButtonCancelActionPerformed
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JButton jButtonAdd;
  private javax.swing.JButton jButtonCancel;
  private javax.swing.JButton jButtonOK;
  private javax.swing.JButton jButtonRemove;
  private javax.swing.JButton jButtonUpdate;
  private javax.swing.JComboBox jComboBoxModelType;
  private javax.swing.JLabel jLabel3;
  private javax.swing.JLabel jLabel5;
  private javax.swing.JLabel jLabelModelDescrption;
  private javax.swing.JPanel jPanel4;
  private javax.swing.JPanel jPanelButtons;
  private javax.swing.JPanel jPanelDescription;
  private javax.swing.JPanel jPanelParameters;
  private javax.swing.JPanel jPanelTargets;
  private javax.swing.JScrollPane jScrollPaneModelDescription;
  private javax.swing.JScrollPane jScrollPaneTableModelParameters;
  private javax.swing.JScrollPane jScrollPaneTreeModels;
  private javax.swing.JTable jTableModelParameters;
  private javax.swing.JTextField jTextFieldName;
  private javax.swing.JTree jTreeModels;
  // End of variables declaration//GEN-END:variables

  /**
   * Return the editor result
   * @return true if the user validated (ok button)
   */
  public boolean isResult() {
    return result;
  }

  public List<Target> getEditTargets() {
    return editTargets;
  }

  /**
   * Define the JDialog for this form
   * @param dialog JDialog instance
   */
  protected void setDialog(final JDialog dialog) {
    this.dialog = dialog;
  }

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
      public String convertValueToText(final Object value, final boolean selected,
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
              } else if (userObject instanceof Model) {
                // Model :
                sValue = ((Model) userObject).getName();
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
