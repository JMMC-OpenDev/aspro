/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: TargetModelForm.java,v 1.36 2010-12-07 17:38:29 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.35  2010/12/03 16:11:52  bourgesl
 * refactoring to use new JTree classes
 *
 * Revision 1.34  2010/12/03 09:33:19  bourgesl
 * changed font for model tree
 *
 * Revision 1.33  2010/11/30 17:04:18  bourgesl
 * fixed focus/editor problems when the tree selection changes (use invokeLater)
 *
 * Revision 1.32  2010/11/29 15:27:30  bourgesl
 * small GUI changes for platform compatibility
 *
 * Revision 1.31  2010/11/25 08:00:54  bourgesl
 * added getEditTarget
 *
 * Revision 1.30  2010/11/19 16:57:04  bourgesl
 * always open full editor with selected target
 * added target name, RA/DEC, magnitudes
 *
 * Revision 1.29  2010/11/18 17:19:09  bourgesl
 * moved dialog related code to TargetEditorDialog
 * remove OK / Cancel buttons
 * minor UI changes
 *
 * Revision 1.28  2010/10/07 15:02:36  bourgesl
 * unused import
 *
 * Revision 1.27  2010/10/01 15:30:01  bourgesl
 * use ModelManager.normalizeFluxes(models)
 *
 * Revision 1.26  2010/09/24 15:55:22  bourgesl
 * use MessagePane
 *
 * Revision 1.25  2010/09/20 14:46:02  bourgesl
 * minor refactoring changes
 *
 * Revision 1.24  2010/09/08 15:58:51  bourgesl
 * renamed variable
 *
 * Revision 1.23  2010/06/23 12:50:38  bourgesl
 * class made final
 *
 * Revision 1.22  2010/06/17 10:02:51  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.21  2010/06/11 08:40:34  bourgesl
 * restored table selection listener to have both tree and description in synch with the current selected model in the parameters table
 *
 * Revision 1.20  2010/06/10 14:20:58  bourgesl
 * model separator is in dark gray if the grid color is white (mac)
 *
 * Revision 1.19  2010/06/09 15:51:25  bourgesl
 * added a specific renderer for the JTable of model parameters in order to :
 * - have a line separator between different models
 * - avoid repeated model names
 *
 * Revision 1.18  2010/05/19 12:04:10  mella
 * Use behaviour of common fr.jmmc.jmcs.gui.NumericJTable for parameterTables
 *
 * Revision 1.17  2010/05/18 15:47:35  bourgesl
 * more space for model list
 *
 * Revision 1.16  2010/05/12 08:44:10  mella
 * Add one preferences window first to choose the default style of display for positions
 *
 * Revision 1.15  2010/05/11 12:24:17  mella
 * Add one button to normalize the flux weights in the model editor
 *
 * Revision 1.14  2010/04/13 14:17:36  bourgesl
 * refactoring : use directly the App internal frame
 *
 * Revision 1.13  2010/04/08 14:06:32  bourgesl
 * javadoc
 *
 * Revision 1.12  2010/04/06 13:58:37  bourgesl
 * minor UI changes for mac os & other LAF
 *
 * Revision 1.11  2010/03/30 12:08:44  bourgesl
 * table cell editor behaviour : replace value instead of append value
 *
 * Revision 1.10  2010/02/19 16:06:27  bourgesl
 * added minimal rho/theta editor for X/Y positions
 *
 * Revision 1.9  2010/02/18 15:52:38  bourgesl
 * added parameter argument validation with an user message
 *
 * Revision 1.8  2010/02/17 17:18:35  bourgesl
 * removed java 1.6 JTable method
 *
 * Revision 1.7  2010/02/17 17:09:01  bourgesl
 * added update model (change type)
 * changed remove action to relocate models when the first model is removed
 * change tree selection to follow user choices (add/update model, table current model)
 *
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

import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.gui.util.ModelJTree;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.gui.NumericJTable;
import fr.jmmc.mcs.model.ModelManager;
import fr.jmmc.mcs.model.gui.ModelParameterTableModel;
import fr.jmmc.mcs.model.gui.ModelParameterTableModel.EditMode;
import fr.jmmc.mcs.model.gui.ModelParameterTableModel.Mode;
import fr.jmmc.mcs.model.targetmodel.Model;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.logging.Level;
import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.table.TableCellRenderer;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

/**
 * This class represents the target model editor ...
 *
 * @author bourgesl
 */
public final class TargetModelForm extends javax.swing.JPanel implements ActionListener, TreeSelectionListener, ListSelectionListener {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.TargetModelForm";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /* members */
  /** list of edited targets (clone) */
  private final List<Target> editTargets;
  /** edited target user informations (clone) */
  private final TargetUserInformations editTargetUserInfos;
  /** current edited target to detect target changes to update the table model */
  private Target currentTarget = null;
  /** current model type to detect type changes to update the model description */
  private String currentModelType = null;

  /**
   * Creates new form TargetModelForm (used by NetBeans editor only)
   */
  public TargetModelForm() {
    this(null, null);
  }

  /**
   * Creates new form TargetModelForm
   * @param targets list of targets to edit
   * @param targetUserInfos target user informations
   */
  public TargetModelForm(final List<Target> targets, final TargetUserInformations targetUserInfos) {
    super();

    this.editTargets = targets;
    this.editTargetUserInfos = targetUserInfos;

    initComponents();

    // Load Preferences and init default elements
    final Preferences myPreferences = Preferences.getInstance();

    final boolean preferXyMode = myPreferences.getPreferenceAsBoolean(Preferences.MODELEDITOR_PREFERXY);
    if (preferXyMode) {
      getModelParameterTableModel().setEditMode(ModelParameterTableModel.EditMode.X_Y);
    } else {
      getModelParameterTableModel().setEditMode(ModelParameterTableModel.EditMode.RHO_THETA);
    }
    // and set coherent state of edit mode radio buttons
    this.jRadioButtonRhoTheta.setSelected(!preferXyMode);
    this.jRadioButtonXY.setSelected(preferXyMode);

    postInit();
  }

  /**
   * This method is useful to set the models and specific features of initialized swing components.
   */
  private void postInit() {

    // model type choice :
    this.jComboBoxModelType.setModel(new DefaultComboBoxModel(ModelManager.getInstance().getSupportedModels()));
    this.jComboBoxModelType.addActionListener(this);

    // update the model description :
    this.updateModelDescription((String) this.jComboBoxModelType.getSelectedItem());

    // table selection listener :
    this.jTableModelParameters.getSelectionModel().addListSelectionListener(this);

    // tree selection listener :
    this.jTreeModels.addTreeSelectionListener(this);

    // edit mode :
    this.jRadioButtonXY.addActionListener(this);
    this.jRadioButtonRhoTheta.addActionListener(this);

    // disable column reordering :
    this.jTableModelParameters.getTableHeader().setReorderingAllowed(false);
  }

  /**
   * Initialize the internal model (tree) from the given list of targets
   * @param targetName target name to select
   */
  protected void initialize(final String targetName) {
    this.generateTree(this.editTargets);
    this.selectTarget(Target.getTarget(targetName, this.editTargets));
  }

  /* Tree related methods */
  /**
   * Return the custom ModelJTree
   * @return ModelJTree
   */
  private final ModelJTree getTreeModels() {
    return (ModelJTree) this.jTreeModels;
  }

  /**
   * Generate the tree from the given list of targets (single or all)
   * @param targets list of targets to edit
   */
  private void generateTree(final List<Target> targets) {

    final DefaultMutableTreeNode rootNode = this.getTreeModels().getRootNode();

    DefaultMutableTreeNode targetNode;
    for (Target target : targets) {

      targetNode = this.getTreeModels().addNode(rootNode, target);

      for (Model model : target.getModels()) {
        this.generateModelNodes(targetNode, model);
      }
    }

    // fire node structure changed :
    this.getTreeModels().fireNodeChanged(rootNode);
  }

  /**
   * Generate the model nodes recursively
   *
   * @param parentNode parent node to append nodes
   * @param model model to process
   */
  private void generateModelNodes(final DefaultMutableTreeNode parentNode, final Model model) {

    final DefaultMutableTreeNode modelNode = this.getTreeModels().addNode(parentNode, model);

    final List<Model> children = model.getModels();
    if (!children.isEmpty()) {
      for (Model child : children) {
        generateModelNodes(modelNode, child);
      }
    }
  }

  /**
   * Select the target node for the given target
   * @param target to select
   */
  protected void selectTarget(final Target target) {
    this.getTreeModels().selectTarget(target);
  }

  /**
   * Process the tree selection events
   * @param e tree selection event
   */
  public void valueChanged(final TreeSelectionEvent e) {
    final DefaultMutableTreeNode currentNode = this.getTreeModels().getLastSelectedNode();

    if (currentNode != null) {
      // Use invokeLater to avoid event ordering problems with focusLost on JTextArea
      // or JTable editors :
      SwingUtilities.invokeLater(new Runnable() {

        public void run() {

          // Check if it is the root node :
          final DefaultMutableTreeNode rootNode = getTreeModels().getRootNode();
          if (currentNode == rootNode) {
            getTreeModels().selectFirstChildNode(rootNode);
            return;
          }

          /* retrieve the node that was selected */
          final Object userObject = currentNode.getUserObject();

          if (logger.isLoggable(Level.FINE)) {
            logger.fine("tree selection : " + userObject);
          }

          if (userObject instanceof Target) {
            // Target :
            processTargetSelection((Target) userObject);
          } else if (userObject instanceof Model) {
            final TreeNode[] path = currentNode.getPath();
            // target is after the root node in the tree path :
            final Target target = (Target) ((DefaultMutableTreeNode) path[1]).getUserObject();
            // Model :
            processModelSelection(target, (Model) userObject);
          }
        }
      });
    }
  }

  /**
   * Update the UI when a target is selected in the target/model tree
   * @param target selected target
   */
  private void processTargetSelection(final Target target) {
    this.jButtonUpdate.setEnabled(false);
    this.jButtonRemove.setEnabled(false);

    // reset text field name :
    this.jTextFieldName.setText(null);

    // update parameter table model :
    this.defineModels(target);
  }

  /**
   * Update the UI when a model is selected in the target/model tree
   * @param target selected target
   * @param model selected model
   */
  private void processModelSelection(final Target target, final Model model) {
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
    defineModels(target, getModelParameterTableModel().getEditMode());
  }

  /**
   * Define the list of models to use in the table of parameters
   * @param target target models to use
   * @param mode edition mode
   */
  private void defineModels(final Target target, final EditMode mode) {
    final ModelParameterTableModel tableModel = getModelParameterTableModel();
    if (target != currentTarget || mode != tableModel.getEditMode()) {
      this.currentTarget = target;
      tableModel.setData(target.getModels(), mode);
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
   * Process any comboBox or radio change event
   * @param e action event
   */
  public void actionPerformed(final ActionEvent e) {
    if (e.getSource() == this.jComboBoxModelType) {
      final String type = (String) this.jComboBoxModelType.getSelectedItem();
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("model type changed : " + type);
      }
      updateModelDescription(type);
    } else if (e.getSource() == this.jRadioButtonXY) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("edit mode X/Y : " + this.jRadioButtonXY.isSelected());
      }
      // change edit mode to X/Y :
      this.defineModels(this.currentTarget, EditMode.X_Y);
    } else if (e.getSource() == this.jRadioButtonRhoTheta) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("edit mode Rho/Theta : " + this.jRadioButtonRhoTheta.isSelected());
      }
      // change edit mode to Rho/Theta :
      this.defineModels(this.currentTarget, EditMode.RHO_THETA);
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

      final DefaultMutableTreeNode modelNode = this.getTreeModels().findTreeNode(model);

      if (modelNode != null) {
        // Select the model node :
        this.getTreeModels().selectPath(new TreePath(modelNode.getPath()));
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

    buttonGroupEditMode = new javax.swing.ButtonGroup();
    jScrollPaneTreeModels = new javax.swing.JScrollPane();
    jTreeModels = new ModelJTree(this.editTargetUserInfos);
    jPanelModelActions = new javax.swing.JPanel();
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
    jTableModelParameters = createJTable();
    jRadioButtonXY = new javax.swing.JRadioButton();
    jRadioButtonRhoTheta = new javax.swing.JRadioButton();
    jLabelOffsetEditMode = new javax.swing.JLabel();
    jButtonNormalizeFluxes = new javax.swing.JButton();

    setLayout(new java.awt.GridBagLayout());

    jScrollPaneTreeModels.setMinimumSize(new java.awt.Dimension(100, 100));
    jScrollPaneTreeModels.setPreferredSize(new java.awt.Dimension(100, 100));

    jTreeModels.setFont(new java.awt.Font("Dialog", 1, 12)); // NOI18N
    javax.swing.tree.DefaultMutableTreeNode treeNode1 = new javax.swing.tree.DefaultMutableTreeNode("Models");
    jTreeModels.setModel(new javax.swing.tree.DefaultTreeModel(treeNode1));
    jScrollPaneTreeModels.setViewportView(jTreeModels);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.3;
    gridBagConstraints.weighty = 0.2;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    add(jScrollPaneTreeModels, gridBagConstraints);

    jPanelModelActions.setBorder(javax.swing.BorderFactory.createTitledBorder("Model actions"));
    jPanelModelActions.setMinimumSize(new java.awt.Dimension(200, 80));
    jPanelModelActions.setPreferredSize(new java.awt.Dimension(200, 80));
    jPanelModelActions.setLayout(new java.awt.GridBagLayout());

    jComboBoxModelType.setMinimumSize(new java.awt.Dimension(100, 24));
    jComboBoxModelType.setPreferredSize(new java.awt.Dimension(100, 24));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelModelActions.add(jComboBoxModelType, gridBagConstraints);

    jButtonAdd.setText("Add");
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
    jPanelModelActions.add(jButtonAdd, gridBagConstraints);

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
    jPanelModelActions.add(jButtonRemove, gridBagConstraints);

    jButtonUpdate.setText("Update");
    jButtonUpdate.setEnabled(false);
    jButtonUpdate.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonUpdateActionPerformed(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelModelActions.add(jButtonUpdate, gridBagConstraints);

    jLabel3.setText("model type");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelModelActions.add(jLabel3, gridBagConstraints);

    jLabel5.setText("Name");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelModelActions.add(jLabel5, gridBagConstraints);

    jTextFieldName.setColumns(10);
    jTextFieldName.setEditable(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelModelActions.add(jTextFieldName, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.7;
    gridBagConstraints.weighty = 0.2;
    add(jPanelModelActions, gridBagConstraints);

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
    gridBagConstraints.gridwidth = 2;
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
    jScrollPaneTableModelParameters.setViewportView(jTableModelParameters);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.weighty = 1.0;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelParameters.add(jScrollPaneTableModelParameters, gridBagConstraints);

    buttonGroupEditMode.add(jRadioButtonXY);
    jRadioButtonXY.setSelected(true);
    jRadioButtonXY.setText("x / y (mas)");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.weightx = 0.5;
    jPanelParameters.add(jRadioButtonXY, gridBagConstraints);

    buttonGroupEditMode.add(jRadioButtonRhoTheta);
    jRadioButtonRhoTheta.setText("rho (mas) / theta [-180°; 180°]");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.weightx = 0.5;
    jPanelParameters.add(jRadioButtonRhoTheta, gridBagConstraints);

    jLabelOffsetEditMode.setText("edit positions :");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelParameters.add(jLabelOffsetEditMode, gridBagConstraints);

    jButtonNormalizeFluxes.setText("Normalize fluxes");
    jButtonNormalizeFluxes.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonNormalizeFluxesActionPerformed(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelParameters.add(jButtonNormalizeFluxes, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.weighty = 0.6;
    add(jPanelParameters, gridBagConstraints);
  }// </editor-fold>//GEN-END:initComponents

  /**
   * Validate the form
   * @return true only if the data are valid
   */
  protected boolean validateForm() {
    // Validate the models :
    for (Target target : this.editTargets) {
      try {
        ModelManager.getInstance().validateModels(target.getModels());
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

  /**
   * Return the current edited target
   * @return current edited target
   */
  protected final Target getCurrentTarget() {
    return currentTarget;
  }

  /**
   * Process the Update action
   * @param evt action event
   */
  private void jButtonUpdateActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonUpdateActionPerformed
    final DefaultMutableTreeNode currentNode = this.getTreeModels().getLastSelectedNode();

    if (currentNode != null) {
      if (currentNode.getUserObject() instanceof Model) {
        final Model model = (Model) currentNode.getUserObject();

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
          int idx = target.getModels().indexOf(model);
          target.getModels().remove(idx);
          target.getModels().add(idx, newModel);

          // force to refresh table model :
          this.currentTarget = null;

          // Replace node :
          final DefaultMutableTreeNode newNode = new DefaultMutableTreeNode(newModel);
          idx = parentNode.getIndex(currentNode);
          parentNode.remove(idx);
          parentNode.insert(newNode, idx);

          // fire node structure changed :
          this.getTreeModels().fireNodeChanged(parentNode);

          // Select the new node = model :
          this.getTreeModels().selectPath(new TreePath(newNode.getPath()));

        } else if (parentNode.getUserObject() instanceof Model) {
          throw new UnsupportedOperationException("Not implemented !");
        }
      } else {
        if (logger.isLoggable(Level.SEVERE)) {
          logger.severe("invalid selected node to perform the update operation = " + currentNode);
        }
      }
    }
  }//GEN-LAST:event_jButtonUpdateActionPerformed

  /**
   * Process the Remove action
   * @param evt action event
   */
  private void jButtonRemoveActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonRemoveActionPerformed
    final DefaultMutableTreeNode currentNode = this.getTreeModels().getLastSelectedNode();

    if (currentNode != null) {

      if (currentNode.getUserObject() instanceof Model) {
        final Model model = (Model) currentNode.getUserObject();

        // Parent can be a target or a model :

        final DefaultMutableTreeNode parentNode = (DefaultMutableTreeNode) currentNode.getParent();

        if (parentNode.getUserObject() instanceof Target) {
          final Target target = (Target) parentNode.getUserObject();

          if (logger.isLoggable(Level.FINE)) {
            logger.fine("remove model : " + model);
          }

          // Remove model from target :
          int idx = target.getModels().indexOf(model);
          target.getModels().remove(idx);

          if (idx == 0) {
            // first model is removed : special case : relocate other models :
            ModelManager.relocateModels(target.getModels());
          }

          // force to refresh table model :
          this.currentTarget = null;

          // Remove node and refresh tree :
          this.getTreeModels().removeNodeAndRefresh(parentNode, currentNode);

        } else if (parentNode.getUserObject() instanceof Model) {
          throw new UnsupportedOperationException("Not implemented !");
        }
      } else {
        if (logger.isLoggable(Level.SEVERE)) {
          logger.severe("invalid selected node to perform the remove operation = " + currentNode);
        }
      }
    }
  }//GEN-LAST:event_jButtonRemoveActionPerformed

  /**
   * Process the Add action
   * @param evt action event
   */
  private void jButtonAddActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonAddActionPerformed
    final DefaultMutableTreeNode currentNode = this.getTreeModels().getLastSelectedNode();

    if (currentNode != null) {

      DefaultMutableTreeNode targetNode = null;
      Target target = null;

      if (currentNode.getUserObject() instanceof Target) {
        targetNode = currentNode;
        target = (Target) currentNode.getUserObject();

      } else if (currentNode.getUserObject() instanceof Model) {
        final DefaultMutableTreeNode parentNode = (DefaultMutableTreeNode) currentNode.getParent();

        // Parent can be a target or a model :

        if (parentNode.getUserObject() instanceof Target) {
          targetNode = parentNode;
          target = (Target) parentNode.getUserObject();

        } else if (parentNode.getUserObject() instanceof Model) {
          throw new UnsupportedOperationException("Not implemented !");
        }
      } else {
        if (logger.isLoggable(Level.SEVERE)) {
          logger.severe("invalid selected node to perform the add operation = " + currentNode);
        }
      }

      if (target != null) {
        final String type = (String) this.jComboBoxModelType.getSelectedItem();

        // create a new model with defined names (model and parameters) :
        final Model newModel = ModelManager.getInstance().newModel(type, target.getModels());

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("add model : " + newModel);
        }

        // Add model to target :
        target.getModels().add(newModel);

        // force to refresh table model :
        this.currentTarget = null;

        // Add node and refresh tree :
        this.getTreeModels().addNodeAndRefresh(targetNode, newModel);
      }
    }
  }//GEN-LAST:event_jButtonAddActionPerformed

  private void jButtonNormalizeFluxesActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonNormalizeFluxesActionPerformed
    if (this.currentTarget == null) {
      return;
    }

    ModelManager.normalizeFluxes(this.currentTarget.getModels());

    // refresh whole values
    getModelParameterTableModel().fireTableDataChanged();
  }//GEN-LAST:event_jButtonNormalizeFluxesActionPerformed
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.ButtonGroup buttonGroupEditMode;
  private javax.swing.JButton jButtonAdd;
  private javax.swing.JButton jButtonNormalizeFluxes;
  private javax.swing.JButton jButtonRemove;
  private javax.swing.JButton jButtonUpdate;
  private javax.swing.JComboBox jComboBoxModelType;
  private javax.swing.JLabel jLabel3;
  private javax.swing.JLabel jLabel5;
  private javax.swing.JLabel jLabelModelDescrption;
  private javax.swing.JLabel jLabelOffsetEditMode;
  private javax.swing.JPanel jPanelDescription;
  private javax.swing.JPanel jPanelModelActions;
  private javax.swing.JPanel jPanelParameters;
  private javax.swing.JRadioButton jRadioButtonRhoTheta;
  private javax.swing.JRadioButton jRadioButtonXY;
  private javax.swing.JScrollPane jScrollPaneModelDescription;
  private javax.swing.JScrollPane jScrollPaneTableModelParameters;
  private javax.swing.JScrollPane jScrollPaneTreeModels;
  private javax.swing.JTable jTableModelParameters;
  private javax.swing.JTextField jTextFieldName;
  private javax.swing.JTree jTreeModels;
  // End of variables declaration//GEN-END:variables

  /**
   * Create a custom JTable with specific behaviour (custom numeric editors and renderers)
   * @return JTable
   */
  private static JTable createJTable() {
    return new NumericJTable() {

      /** default serial UID for Serializable interface */
      private static final long serialVersionUID = 1;

      /* members */
      /** custom border used as a line separator */
      private Border rowBorder = null;

      /**
       * Overriden method to adjust borders in order to better split parameters belonging to the same model
       */
      @Override
      public Component prepareRenderer(final TableCellRenderer renderer, final int row, final int column) {
        final JComponent comp = (JComponent) super.prepareRenderer(renderer, row, column);

        // detect model changes :
        boolean displaySeparator = (row == 0);

        if (!displaySeparator) {
          final ModelParameterTableModel tableModel = (ModelParameterTableModel) getModel();

          // model changed ?
          displaySeparator = (tableModel.getModelAt(row) != tableModel.getModelAt(row - 1));
        }

        // adjust borders to have thicker line separator :
        if (displaySeparator) {

          if (this.rowBorder == null) {
            // avoid white color because the background is already white :
            final Color borderColor = (getGridColor().equals(Color.WHITE)) ? Color.DARK_GRAY : getGridColor();
            // lazy instanciation :
            this.rowBorder = BorderFactory.createMatteBorder(1, 0, 0, 0, borderColor);
          }

          comp.setBorder(BorderFactory.createCompoundBorder(this.rowBorder, comp.getBorder()));
        } else {
          // consecutive parameters of the same model :
          if (column == 0 && comp instanceof JLabel) {
            // avoid to repeat the model name :
            ((JLabel) comp).setText("");
          }
        }

        return comp;
      }
    };
  }
}
