/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: TargetModelForm.java,v 1.4 2010-02-12 15:53:18 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.mcs.model.ModelFunction;
import fr.jmmc.mcs.model.ModelManager;
import fr.jmmc.mcs.model.function.DiskModelFunction;
import fr.jmmc.mcs.model.gui.ModelParameterTableModel;
import fr.jmmc.mcs.model.gui.ModelParameterTableModel.Mode;
import fr.jmmc.mcs.model.targetmodel.Model;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import javax.swing.DefaultCellEditor;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;

/**
 * This class represents the target model editor ...
 * @author bourgesl
 */
public class TargetModelForm extends javax.swing.JPanel implements ActionListener {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.TargetModelForm";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /* swing */
  /** dialog window */
  private JDialog dialog;

  /**
   * TODO KILL
   * @return sample disk model
   */
  private static List<Model> diskModels() {

    final ModelManager mm = ModelManager.getInstance();

    final List<Model> models = new ArrayList<Model>();

    Model model;

    model = mm.createModel(ModelFunction.MODEL_DISK);

    model.setName("disk1");

    ModelManager.setParameterValue(model, ModelFunction.PARAM_FLUX_WEIGHT, 1.0);
    ModelManager.setParameterValue(model, ModelFunction.PARAM_X, 0);
    ModelManager.setParameterValue(model, ModelFunction.PARAM_Y, 0);
    ModelManager.setParameterValue(model, DiskModelFunction.PARAM_DIAMETER, 5);

    models.add(model);

    model = mm.createModel(ModelFunction.MODEL_DISK);

    model.setName("disk2");

    ModelManager.setParameterValue(model, ModelFunction.PARAM_FLUX_WEIGHT, 1.0);
    ModelManager.setParameterValue(model, ModelFunction.PARAM_X, 1);
    ModelManager.setParameterValue(model, ModelFunction.PARAM_Y, 2);
    ModelManager.setParameterValue(model, DiskModelFunction.PARAM_DIAMETER, 3);

    models.add(model);

    model = mm.createModel(ModelFunction.MODEL_PUNCT);
    model.setName("group1");

    models.add(model);

    final Model subModel = mm.createModel(ModelFunction.MODEL_PUNCT);
    model.setName("punct1");

    ModelManager.setParameterValue(model, ModelFunction.PARAM_FLUX_WEIGHT, 1.0);
    ModelManager.setParameterValue(model, ModelFunction.PARAM_X, 0);
    ModelManager.setParameterValue(model, ModelFunction.PARAM_Y, 0);

    model.getModels().add(subModel);

    return models;
  }

  /**
   * Display the model editor using the given target as the initial selected target
   * @return true if the model editor changed anything
   */
  public static boolean showModelEditor(final String targetName) {

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("showing Model Editor : " + targetName);
    }

    final TargetModelForm form = new TargetModelForm();

    form.defineModels(diskModels());


    JDialog dialog = null;

    try {
      //1. Create the dialog
      dialog = new JDialog(AsproGui.getInstance().getRootFrame(), "Model Editor", true);

      final Dimension dim = new Dimension(600, 700);
      dialog.setMinimumSize(dim);
      dialog.addComponentListener(new ComponentResizeAdapter(dim));

      //2. Optional: What happens when the dialog closes ?
      dialog.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

      //3. Create components and put them in the dialog
      dialog.add(form);

      // associate the dialog to the form :
      form.setDialog(dialog);

      //4. Size the dialog.
      dialog.pack();

      // center it :
      dialog.setLocationRelativeTo(dialog.getOwner());

      //5. Show it.
      dialog.setVisible(true);
    } finally {
      if (dialog != null) {
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("dispose Model Editor ...");
        }
        dialog.dispose();
      }
    }

    return true;
  }

  /** Creates new form TargetModelForm */
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
    ((DefaultCellEditor) jTableModelParameters.getDefaultEditor(String.class)).setClickCountToStart(1);
    jTableModelParameters.setDefaultEditor(Double.class, jTableModelParameters.getDefaultEditor(String.class));
    jTableModelParameters.setDefaultRenderer(Double.class, jTableModelParameters.getDefaultRenderer(String.class));

    // update tree :
    generateTree();

    updateModelDescription();
  }

  /**
   * Generates the tree from the complete observation (targets + models)
   */
  private void generateTree() {
    final List<Target> targets = ObservationManager.getInstance().getObservation().getTargets();

    final DefaultMutableTreeNode rootNode = (DefaultMutableTreeNode) this.jTreeModels.getModel().getRoot();

    DefaultMutableTreeNode node;
    for (Target target : targets) {
      node = new DefaultMutableTreeNode(target);

      rootNode.add(node);
    }

    ((DefaultTreeModel)this.jTreeModels.getModel()).nodeStructureChanged(rootNode);
  }

  public void defineModels(final Model model) {
    getModelParameterTableModel().setData(model);
  }

  public void defineModels(final List<Model> models) {
    getModelParameterTableModel().setData(models);
  }

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

  private void updateModelDescription() {
    updateModelDescription((String) this.jComboBoxModelType.getSelectedItem());
  }

  private void updateModelDescription(final String type) {
    final String desc = ModelManager.getInstance().getModelDescription(type);

    // note : use html to have multi line label :
    this.jLabelModelDescrption.setText("<html>" + desc.replaceAll("\n", "<br>") + "</html>");
  }

  /** This method is called from within the constructor to
   * initialize the form.
   * WARNING: Do NOT modify this code. The content of this method is
   * always regenerated by the Form Editor.
   */
  @SuppressWarnings("unchecked")
  // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
  private void initComponents() {
    java.awt.GridBagConstraints gridBagConstraints;

    jPanelTargets = new javax.swing.JPanel();
    jScrollPane2 = new javax.swing.JScrollPane();
    jTreeModels = createJTree();
    jPanel4 = new javax.swing.JPanel();
    jComboBoxModelType = new javax.swing.JComboBox();
    addModelButton = new javax.swing.JButton();
    removeModelButton = new javax.swing.JButton();
    jButtonChangeType = new javax.swing.JButton();
    jLabel3 = new javax.swing.JLabel();
    jLabel5 = new javax.swing.JLabel();
    jTextField1 = new javax.swing.JTextField();
    jPanelDescription = new javax.swing.JPanel();
    jScrollPane3 = new javax.swing.JScrollPane();
    jLabelModelDescrption = new javax.swing.JLabel();
    jPanelParameters = new javax.swing.JPanel();
    jScrollPane1 = new javax.swing.JScrollPane();
    jTableModelParameters = new javax.swing.JTable();
    jPanelButtons = new javax.swing.JPanel();
    jButtonOK = new javax.swing.JButton();

    setLayout(new java.awt.GridBagLayout());

    jPanelTargets.setBorder(javax.swing.BorderFactory.createTitledBorder("Targets"));
    jPanelTargets.setMinimumSize(new java.awt.Dimension(300, 100));
    jPanelTargets.setPreferredSize(new java.awt.Dimension(300, 100));
    jPanelTargets.setLayout(new java.awt.GridBagLayout());

    javax.swing.tree.DefaultMutableTreeNode treeNode1 = new javax.swing.tree.DefaultMutableTreeNode("Models");
    jTreeModels.setModel(new javax.swing.tree.DefaultTreeModel(treeNode1));
    jTreeModels.setMinimumSize(new java.awt.Dimension(100, 100));
    jTreeModels.setPreferredSize(new java.awt.Dimension(100, 100));
    jTreeModels.setVisibleRowCount(10);
    jScrollPane2.setViewportView(jTreeModels);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.4;
    gridBagConstraints.weighty = 1.0;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTargets.add(jScrollPane2, gridBagConstraints);

    jPanel4.setBorder(javax.swing.BorderFactory.createTitledBorder("Model list"));
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

    addModelButton.setText("Add");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanel4.add(addModelButton, gridBagConstraints);

    removeModelButton.setText("Remove");
    removeModelButton.setEnabled(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanel4.add(removeModelButton, gridBagConstraints);

    jButtonChangeType.setText("Update");
    jButtonChangeType.setEnabled(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanel4.add(jButtonChangeType, gridBagConstraints);

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

    jTextField1.setColumns(15);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanel4.add(jTextField1, gridBagConstraints);

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
    gridBagConstraints.weighty = 0.1;
    add(jPanelTargets, gridBagConstraints);

    jPanelDescription.setBorder(javax.swing.BorderFactory.createTitledBorder("Model description"));
    jPanelDescription.setMaximumSize(new java.awt.Dimension(2147483647, 250));
    jPanelDescription.setMinimumSize(new java.awt.Dimension(100, 150));
    jPanelDescription.setPreferredSize(new java.awt.Dimension(100, 150));
    jPanelDescription.setLayout(new java.awt.GridBagLayout());

    jLabelModelDescrption.setBackground(new java.awt.Color(255, 255, 255));
    jLabelModelDescrption.setFont(new java.awt.Font("Dialog", 0, 12));
    jLabelModelDescrption.setOpaque(true);
    jScrollPane3.setViewportView(jLabelModelDescrption);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.weighty = 1.0;
    jPanelDescription.add(jScrollPane3, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weighty = 0.1;
    add(jPanelDescription, gridBagConstraints);

    jPanelParameters.setBorder(javax.swing.BorderFactory.createTitledBorder("Model Parameters"));
    jPanelParameters.setMinimumSize(new java.awt.Dimension(300, 100));
    jPanelParameters.setPreferredSize(new java.awt.Dimension(300, 100));
    jPanelParameters.setLayout(new java.awt.GridBagLayout());

    jScrollPane1.setMinimumSize(new java.awt.Dimension(200, 100));
    jScrollPane1.setPreferredSize(new java.awt.Dimension(200, 100));

    jTableModelParameters.setModel(new ModelParameterTableModel(Mode.LITPRO));
    jTableModelParameters.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_ALL_COLUMNS);
    jTableModelParameters.setFillsViewportHeight(true);
    jTableModelParameters.setMinimumSize(new java.awt.Dimension(200, 50));
    jTableModelParameters.setPreferredSize(new java.awt.Dimension(200, 50));
    jScrollPane1.setViewportView(jTableModelParameters);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.weighty = 1.0;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelParameters.add(jScrollPane1, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.weighty = 0.7;
    add(jPanelParameters, gridBagConstraints);

    jButtonOK.setText("OK");
    jButtonOK.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonOKActionPerformed(evt);
      }
    });
    jPanelButtons.add(jButtonOK);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 3;
    gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    add(jPanelButtons, gridBagConstraints);
  }// </editor-fold>//GEN-END:initComponents

  private void jButtonOKActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonOKActionPerformed
    if (this.dialog != null) {
      this.dialog.setVisible(false);
    }
  }//GEN-LAST:event_jButtonOKActionPerformed
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JButton addModelButton;
  private javax.swing.JButton jButtonChangeType;
  private javax.swing.JButton jButtonOK;
  private javax.swing.JComboBox jComboBoxModelType;
  private javax.swing.JLabel jLabel3;
  private javax.swing.JLabel jLabel5;
  private javax.swing.JLabel jLabelModelDescrption;
  private javax.swing.JPanel jPanel4;
  private javax.swing.JPanel jPanelButtons;
  private javax.swing.JPanel jPanelDescription;
  private javax.swing.JPanel jPanelParameters;
  private javax.swing.JPanel jPanelTargets;
  private javax.swing.JScrollPane jScrollPane1;
  private javax.swing.JScrollPane jScrollPane2;
  private javax.swing.JScrollPane jScrollPane3;
  private javax.swing.JTable jTableModelParameters;
  private javax.swing.JTextField jTextField1;
  private javax.swing.JTree jTreeModels;
  private javax.swing.JButton removeModelButton;
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
      public String convertValueToText(final Object value, final boolean selected,
              final boolean expanded, final boolean leaf, final int row,
              final boolean hasFocus) {
        if (value != null) {
          final Class<?> type = value.getClass();

          String sValue = null;

          if (DefaultMutableTreeNode.class == type) {
            final DefaultMutableTreeNode node = (DefaultMutableTreeNode)value;

            final Object userObject = node.getUserObject();

            if (userObject != null) {
              final Class<?> userType = userObject.getClass();

              if (String.class == userType) {
                sValue = userObject.toString();
              } else if (Target.class == userType) {
                // Target :
                sValue = ((Target) userObject).getName();
              } else {
                logger.severe("unsupported class type = " + userType);
              }
            }

          } else {
            logger.severe("unsupported class type = " + type);
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

  protected void setDialog(final JDialog dialog) {
    this.dialog = dialog;
  }
}
