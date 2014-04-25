/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.gui.util.AnimatorPanel;
import fr.jmmc.aspro.gui.util.ModelJTree;
import fr.jmmc.aspro.gui.util.TargetRenderer;
import fr.jmmc.aspro.gui.util.TargetTreeCellRenderer;
import fr.jmmc.aspro.gui.util.UserModelAnimator;
import fr.jmmc.aspro.gui.util.UserModelAnimator.UserModelAnimatorListener;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.model.oi.UserModel;
import fr.jmmc.aspro.service.UserModelData;
import fr.jmmc.aspro.service.UserModelService;
import fr.jmmc.jmal.model.ModelManager;
import fr.jmmc.jmal.model.gui.ModelParameterTableModel;
import fr.jmmc.jmal.model.gui.ModelParameterTableModel.EditMode;
import fr.jmmc.jmal.model.gui.ModelParameterTableModel.Mode;
import fr.jmmc.jmal.model.targetmodel.Model;
import fr.jmmc.jmcs.gui.component.Disposable;
import fr.jmmc.jmcs.gui.component.FileChooser;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.component.NumericJTable;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.data.MimeType;
import fr.jmmc.jmcs.util.StringUtils;
import fr.jmmc.oitools.image.FitsImageHDU;
import fr.nom.tam.fits.FitsException;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.border.Border;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.table.TableCellRenderer;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class represents the target model editor ...
 *
 * @author bourgesl
 */
public final class TargetModelForm extends javax.swing.JPanel implements ActionListener, TreeSelectionListener, ListSelectionListener,
                                                                         UserModelAnimatorListener, Disposable {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(TargetModelForm.class.getName());
    /** OIFits MimeType */
    private final static MimeType mimeType = MimeType.FITS_IMAGE;
    /** user model animator singleton */
    private final static UserModelAnimator animator = UserModelAnimator.getInstance();

    /* members */
    /** list of edited targets (clone) */
    private final List<Target> editTargets;
    /** edited target user informations (clone) */
    private final TargetUserInformations editTargetUserInfos;
    /** current edited target to detect target changes to update the table model */
    private Target currentTarget = null;
    /** current model type to detect type changes to update the model description */
    private String currentModelType = null;
    /** fits image panel */
    private FitsImagePanel fitsImagePanel = null;
    /** animator panel */
    private AnimatorPanel animatorPanel = null;
    /** current user model to refresh images */
    private UserModel currentUserModel = null;

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
            /* use separation / position angle editor instead of rho / theta editor to respect astronomical conventions */
            getModelParameterTableModel().setEditMode(ModelParameterTableModel.EditMode.SEP_POS_ANGLE);
        }
        // and set coherent state of edit mode radio buttons
        this.jRadioButtonSepPosAngle.setSelected(!preferXyMode);
        this.jRadioButtonXY.setSelected(preferXyMode);

        postInit();
    }

    /**
     * This method is useful to set the models and specific features of initialized swing components.
     */
    private void postInit() {

        // model mode:
        this.jRadioButtonAnalytical.addActionListener(this);
        this.jRadioButtonUserModel.addActionListener(this);

        // enable/disable user model:
        this.jRadioButtonValid.addActionListener(this);
        this.jRadioButtonInvalid.addActionListener(this);

        // model type choice :
        this.jComboBoxModelType.setModel(new DefaultComboBoxModel(ModelManager.getInstance().getSupportedModels()));
        this.jComboBoxModelType.addActionListener(this);

        // update the model description :
        this.updateModelDescription((String) this.jComboBoxModelType.getSelectedItem());

        // table selection listener :
        this.jTableModelParameters.getSelectionModel().addListSelectionListener(this);

        // tree selection listener :
        this.jTreeModels.addTreeSelectionListener(this);
        this.jTreeModels.setCellRenderer(new TargetTreeCellRenderer(new TargetRenderer(this.editTargetUserInfos)));

        // edit mode :
        this.jRadioButtonXY.addActionListener(this);
        this.jRadioButtonSepPosAngle.addActionListener(this);

        // disable column reordering :
        this.jTableModelParameters.getTableHeader().setReorderingAllowed(false);
    }

    /**
     * Initialize the internal model (tree) from the given list of targets
     * @param targetName target name to select
     */
    void initialize(final String targetName) {
        this.generateTree();
        this.selectTarget(Target.getTarget(targetName, this.editTargets));

        if (targetName == null) {
            // Use a new target to reset form fields:
            processTargetSelection(new Target());
            resetForm();
        }
    }

    /**
     * Reset and disable this form when no target is present
     */
    private void resetForm() {
        // Use a new target to reset form fields:
        processTargetSelection(new Target());

        // disable buttons:
        this.jRadioButtonAnalytical.setEnabled(false);
        this.jRadioButtonUserModel.setEnabled(false);
        this.jButtonAdd.setEnabled(false);

        // disable fields:
        this.jComboBoxModelType.setEnabled(false);

        this.jRadioButtonXY.setEnabled(false);
        this.jRadioButtonSepPosAngle.setEnabled(false);
        this.jButtonNormalizeFluxes.setEnabled(false);

        // reset current target:
        this.currentTarget = null;
    }

    /**
     * Disable model animation
     */
    void disableAnimator() {
        // disable model animation:
        animator.unregister(this);
    }

    /**
     * Disable form when not displayed in tabbed pane
     */
    void disableForm() {
        disableAnimator();
    }

    /**
     * Free any resource or reference to this instance
     */
    @Override
    public void dispose() {
        disableAnimator();

        if (this.fitsImagePanel != null) {
            this.fitsImagePanel.dispose();
        }
    }

    /* Tree related methods */
    /**
     * Return the custom ModelJTree
     * @return ModelJTree
     */
    private ModelJTree getTreeModels() {
        return (ModelJTree) this.jTreeModels;
    }

    /**
     * Generate the tree from the current edited list of targets
     */
    private void generateTree() {

        final DefaultMutableTreeNode rootNode = this.getTreeModels().getRootNode();

        rootNode.removeAllChildren();

        DefaultMutableTreeNode targetNode;
        for (Target target : this.editTargets) {

            // add first science targets :
            if (!this.editTargetUserInfos.isCalibrator(target)) {
                targetNode = this.getTreeModels().addNode(rootNode, target);

                for (Model model : target.getModels()) {
                    this.generateModelNodes(targetNode, model);
                }
            }
        }

        //  add calibrators :
        for (Target target : this.editTargetUserInfos.getCalibrators()) {

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
    void selectTarget(final Target target) {
        this.getTreeModels().selectTarget(target);
    }

    /**
     * Process the tree selection events
     * @param e tree selection event
     */
    @Override
    public void valueChanged(final TreeSelectionEvent e) {
        final DefaultMutableTreeNode currentNode = this.getTreeModels().getLastSelectedNode();

        if (currentNode != null) {
            // Use invokeLater to selection change issues with editors :
            SwingUtils.invokeLaterEDT(new Runnable() {
                /**
                 * Update tree selection
                 */
                @Override
                public void run() {

                    // Check if it is the root node :
                    final DefaultMutableTreeNode rootNode = getTreeModels().getRootNode();
                    if (currentNode == rootNode) {
                        getTreeModels().selectFirstChildNode(rootNode);
                        return;
                    }

                    /* retrieve the node that was selected */
                    final Object userObject = currentNode.getUserObject();

                    logger.debug("tree selection : {}", userObject);

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

        // model mode:
        final boolean isAnalytical = target.hasAnalyticalModel();

        if (isAnalytical) {
            this.jRadioButtonAnalytical.setSelected(true);
        } else {
            this.jRadioButtonUserModel.setSelected(true);
        }

        boolean enableAnimator = false;

        // User model:
        final UserModel userModel = target.getUserModel();
        if (userModel != null) {
            if (userModel.isFileValid()) {
                this.jRadioButtonValid.setSelected(true);
            } else {
                this.jRadioButtonInvalid.setSelected(true);
            }
            this.jTextFieldFileReference.setText(userModel.getFile());

            if (!isAnalytical && userModel.isModelDataReady()) {
                // update fits Image:
                if (this.fitsImagePanel == null) {
                    // do not show id but options:
                    this.fitsImagePanel = new FitsImagePanel(false, true) {
                        private static final long serialVersionUID = 1L;

                        @Override
                        protected void resetPlot() {
                            try {
                                super.resetPlot();
                            } finally {
                                disableAnimator();
                            }
                        }
                    };
                }

                final List<UserModelData> modelDataList = userModel.getModelDataList();

                // use first image:
                this.fitsImagePanel.setFitsImage(modelDataList.get(0).getFitsImage());

                if (modelDataList.size() > 1) {
                    enableAnimator = true;
                    this.currentUserModel = userModel;
                }

                this.jPanelImage.add(this.fitsImagePanel);
            }

        } else {
            this.jRadioButtonInvalid.setSelected(true);
            this.jTextFieldFileReference.setText(null);

            if (this.fitsImagePanel != null) {
                // reset the FitsImage panel:
                this.fitsImagePanel.setFitsImage(null);
            }

            // remove the FitsImage panel:
            this.jPanelImage.removeAll();
        }

        // anyway enable or disable animator:
        if (enableAnimator) {
            if (this.animatorPanel == null) {
                this.animatorPanel = new AnimatorPanel(this, true);
                this.fitsImagePanel.addOptionPanel(this.animatorPanel);
            }
        } else {
            this.currentUserModel = null;
        }

        // update user model in animator panel to enable/disable animator:
        if (this.animatorPanel != null) {
            this.animatorPanel.setUserModel(this.currentUserModel);
            this.animatorPanel.setVisible(this.currentUserModel != null);
        }

        // Analytical models:
        // reset text field name :
        this.jTextFieldName.setText(null);

        // update parameter table model :
        this.defineModels(target);

        // Update visible panels:
        updateVisibleModelPanels();
    }

    /**
     * Update the UI when a model is selected in the target/model tree
     * @param target selected target
     * @param model selected model
     */
    private void processModelSelection(final Target target, final Model model) {
        this.jButtonUpdate.setEnabled(true);
        this.jButtonRemove.setEnabled(true);

        // model mode:
        this.jRadioButtonAnalytical.setSelected(true);

        // User model:
        this.jRadioButtonInvalid.setSelected(true);
        this.jTextFieldFileReference.setText(null);

        // Analytical models:
        // update text field name :
        this.jTextFieldName.setText(model.getName());

        // update model type :
        this.jComboBoxModelType.setSelectedItem(model.getType());

        // update parameter table model :
        this.defineModels(target);

        // Update visible panels:
        updateVisibleModelPanels();
    }

    /**
     * Update visible panels depending on model mode (analytical or user model)
     */
    private void updateVisibleModelPanels() {
        final boolean isAnalytical = this.jRadioButtonAnalytical.isSelected();

        this.jPanelModelAnalytic.setVisible(isAnalytical);
        this.jPanelDescription.setVisible(isAnalytical);
        this.jPanelParameters.setVisible(isAnalytical);
        this.jPanelUserModel.setVisible(!isAnalytical);
        this.jPanelImage.setVisible(!isAnalytical);
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
    @Override
    public void actionPerformed(final ActionEvent e) {
        if (e.getSource() == this.jComboBoxModelType) {
            final String type = (String) this.jComboBoxModelType.getSelectedItem();
            logger.debug("model type changed : {}", type);

            updateModelDescription(type);
        } else if (e.getSource() == this.jRadioButtonXY) {
            if (logger.isDebugEnabled()) {
                logger.debug("edit mode X/Y: {}", this.jRadioButtonXY.isSelected());
            }
            // change edit mode to X/Y :
            this.defineModels(this.currentTarget, EditMode.X_Y);
        } else if (e.getSource() == this.jRadioButtonSepPosAngle) {
            if (logger.isDebugEnabled()) {
                logger.debug("edit mode Sep/PosAngle: {}", this.jRadioButtonSepPosAngle.isSelected());
            }
            // change edit mode to Sep/PosAngle:
            this.defineModels(this.currentTarget, EditMode.SEP_POS_ANGLE);
        } else if (e.getSource() == this.jRadioButtonAnalytical) {
            if (logger.isDebugEnabled()) {
                logger.debug("mode Analytical: {}", this.jRadioButtonAnalytical.isSelected());
            }
            this.currentTarget.setUseAnalyticalModel(Boolean.TRUE);
            // reselect target to change panel visibility:
            processTargetSelection(currentTarget);
        } else if (e.getSource() == this.jRadioButtonUserModel) {
            if (logger.isDebugEnabled()) {
                logger.debug("mode User model: {}", this.jRadioButtonUserModel.isSelected());
            }
            this.currentTarget.setUseAnalyticalModel(Boolean.FALSE);
            // reselect target to change panel visibility:
            processTargetSelection(currentTarget);
        } else if (e.getSource() == this.jRadioButtonValid) {
            if (logger.isDebugEnabled()) {
                logger.debug("enable userModel: {}", this.jRadioButtonValid.isSelected());
            }
            final UserModel userModel = this.currentTarget.getUserModel();
            if (userModel == null) {
                this.jRadioButtonInvalid.setSelected(true);
            } else {
                prepareAndValidateUserModel(userModel);

                // reselect target to update image:
                processTargetSelection(currentTarget);
            }
        } else if (e.getSource() == this.jRadioButtonInvalid) {
            if (logger.isDebugEnabled()) {
                logger.debug("disable userModel: {}", this.jRadioButtonInvalid.isSelected());
            }
            final UserModel userModel = this.currentTarget.getUserModel();
            if (userModel != null) {
                userModel.setFileValid(false);
            }
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
            this.jLabelModelDescrption.setText("<html>" + StringUtils.replaceCR(desc, "<br>") + "</html>");
        }
    }

    /* JTable selection listener */
    /**
     * Called whenever the value of the selection changes.
     * @param e the event that characterizes the change.
     */
    @Override
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
        buttonGroupModelMode = new javax.swing.ButtonGroup();
        buttonGroupUserModelValid = new javax.swing.ButtonGroup();
        jScrollPaneTreeModels = new javax.swing.JScrollPane();
        jTreeModels = new ModelJTree(this.editTargetUserInfos);
        jPanelModel = new javax.swing.JPanel();
        jLabelMode = new javax.swing.JLabel();
        jRadioButtonAnalytical = new javax.swing.JRadioButton();
        jRadioButtonUserModel = new javax.swing.JRadioButton();
        jSeparatorMode = new javax.swing.JSeparator();
        jPanelModelAnalytic = new javax.swing.JPanel();
        jButtonAdd = new javax.swing.JButton();
        jButtonRemove = new javax.swing.JButton();
        jButtonUpdate = new javax.swing.JButton();
        jLabelName = new javax.swing.JLabel();
        jTextFieldName = new javax.swing.JTextField();
        jLabelType = new javax.swing.JLabel();
        jComboBoxModelType = new javax.swing.JComboBox();
        jPanelUserModel = new javax.swing.JPanel();
        jLabelValid = new javax.swing.JLabel();
        jRadioButtonValid = new javax.swing.JRadioButton();
        jRadioButtonInvalid = new javax.swing.JRadioButton();
        jLabelFile = new javax.swing.JLabel();
        jTextFieldFileReference = new javax.swing.JTextField();
        jButtonOpenFile = new javax.swing.JButton();
        jButtonImageInfo = new javax.swing.JButton();
        jPanelDescription = new javax.swing.JPanel();
        jScrollPaneModelDescription = new javax.swing.JScrollPane();
        jLabelModelDescrption = new javax.swing.JLabel();
        jPanelParameters = new javax.swing.JPanel();
        jScrollPaneTableModelParameters = new javax.swing.JScrollPane();
        jTableModelParameters = createJTable();
        jRadioButtonXY = new javax.swing.JRadioButton();
        jRadioButtonSepPosAngle = new javax.swing.JRadioButton();
        jLabelOffsetEditMode = new javax.swing.JLabel();
        jButtonNormalizeFluxes = new javax.swing.JButton();
        jPanelImage = new javax.swing.JPanel();

        setLayout(new java.awt.GridBagLayout());

        jScrollPaneTreeModels.setMinimumSize(new java.awt.Dimension(80, 100));
        jScrollPaneTreeModels.setName(""); // NOI18N
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
        add(jScrollPaneTreeModels, gridBagConstraints);

        jPanelModel.setBorder(javax.swing.BorderFactory.createTitledBorder("Model"));
        jPanelModel.setLayout(new java.awt.GridBagLayout());

        jLabelMode.setText("Mode");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 0.1;
        jPanelModel.add(jLabelMode, gridBagConstraints);

        buttonGroupModelMode.add(jRadioButtonAnalytical);
        jRadioButtonAnalytical.setSelected(true);
        jRadioButtonAnalytical.setText("Analytical");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelModel.add(jRadioButtonAnalytical, gridBagConstraints);

        buttonGroupModelMode.add(jRadioButtonUserModel);
        jRadioButtonUserModel.setText("User Model");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelModel.add(jRadioButtonUserModel, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanelModel.add(jSeparatorMode, gridBagConstraints);

        jPanelModelAnalytic.setMinimumSize(new java.awt.Dimension(200, 80));
        jPanelModelAnalytic.setLayout(new java.awt.GridBagLayout());

        jButtonAdd.setText("Add");
        jButtonAdd.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonAddActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        jPanelModelAnalytic.add(jButtonAdd, gridBagConstraints);

        jButtonRemove.setText("Remove");
        jButtonRemove.setEnabled(false);
        jButtonRemove.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonRemoveActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelModelAnalytic.add(jButtonRemove, gridBagConstraints);

        jButtonUpdate.setText("Update");
        jButtonUpdate.setEnabled(false);
        jButtonUpdate.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonUpdateActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelModelAnalytic.add(jButtonUpdate, gridBagConstraints);

        jLabelName.setText("Name");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelModelAnalytic.add(jLabelName, gridBagConstraints);

        jTextFieldName.setColumns(10);
        jTextFieldName.setEditable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelModelAnalytic.add(jTextFieldName, gridBagConstraints);

        jLabelType.setText("model type");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelModelAnalytic.add(jLabelType, gridBagConstraints);

        jComboBoxModelType.setMinimumSize(new java.awt.Dimension(100, 24));
        jComboBoxModelType.setPreferredSize(new java.awt.Dimension(100, 24));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelModelAnalytic.add(jComboBoxModelType, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.7;
        gridBagConstraints.weighty = 0.2;
        jPanelModel.add(jPanelModelAnalytic, gridBagConstraints);

        jPanelUserModel.setLayout(new java.awt.GridBagLayout());

        jLabelValid.setText("State");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        gridBagConstraints.weightx = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 6);
        jPanelUserModel.add(jLabelValid, gridBagConstraints);

        buttonGroupUserModelValid.add(jRadioButtonValid);
        jRadioButtonValid.setSelected(true);
        jRadioButtonValid.setText("enabled");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weightx = 0.4;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelUserModel.add(jRadioButtonValid, gridBagConstraints);

        buttonGroupUserModelValid.add(jRadioButtonInvalid);
        jRadioButtonInvalid.setText("disabled");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weightx = 0.4;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelUserModel.add(jRadioButtonInvalid, gridBagConstraints);

        jLabelFile.setText("File");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 6);
        jPanelUserModel.add(jLabelFile, gridBagConstraints);

        jTextFieldFileReference.setColumns(20);
        jTextFieldFileReference.setEditable(false);
        jTextFieldFileReference.setText("FileReference");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.6;
        jPanelUserModel.add(jTextFieldFileReference, gridBagConstraints);

        jButtonOpenFile.setText("Open");
        jButtonOpenFile.setToolTipText("Open a FITS image ...");
        jButtonOpenFile.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonOpenFileActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelUserModel.add(jButtonOpenFile, gridBagConstraints);

        jButtonImageInfo.setText("Info");
        jButtonImageInfo.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonImageInfoActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelUserModel.add(jButtonImageInfo, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.7;
        gridBagConstraints.weighty = 0.2;
        jPanelModel.add(jPanelUserModel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.6;
        gridBagConstraints.weighty = 0.2;
        add(jPanelModel, gridBagConstraints);

        jPanelDescription.setBorder(javax.swing.BorderFactory.createTitledBorder("Model description"));
        jPanelDescription.setMaximumSize(new java.awt.Dimension(2147483647, 250));
        jPanelDescription.setMinimumSize(new java.awt.Dimension(100, 150));
        jPanelDescription.setPreferredSize(new java.awt.Dimension(100, 150));
        jPanelDescription.setLayout(new java.awt.GridBagLayout());

        jLabelModelDescrption.setBackground(new java.awt.Color(255, 255, 255));
        jLabelModelDescrption.setFont(new java.awt.Font("Dialog", 0, 12)); // NOI18N
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
        gridBagConstraints.weightx = 1.0;
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

        buttonGroupEditMode.add(jRadioButtonSepPosAngle);
        jRadioButtonSepPosAngle.setText("sep. (mas) / pos. angle [-180°; 180°]");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 0.5;
        jPanelParameters.add(jRadioButtonSepPosAngle, gridBagConstraints);

        jLabelOffsetEditMode.setText("edit positions:");
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

        jPanelImage.setBorder(javax.swing.BorderFactory.createTitledBorder("Fits Image"));
        jPanelImage.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 0.8;
        add(jPanelImage, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    /**
     * Validate the form
     * @return true only if the data are valid
     */
    boolean validateForm() {
        // Validate only analytical models :
        for (Target target : this.editTargets) {
            try {
                if (target.hasAnalyticalModel()) {
                    // validate only analytical models for target using them:
                    ModelManager.getInstance().validateModels(target.getModels());
                }
            } catch (IllegalArgumentException iae) {
                // display an error message for the first error found :
                MessagePane.showErrorMessage(iae.getMessage(), "Error on target " + target.getName());

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
    Target getCurrentTarget() {
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
              final DefaultMutableTreeNode parentNode = this.getTreeModels().getParentNode(currentNode);

              if (parentNode.getUserObject() instanceof Target) {
                  final Target target = (Target) parentNode.getUserObject();

                  logger.debug("update model : {}", model);

                  // create a new model with defined names (model and parameters) replacing the selected model :
                  final Model newModel = ModelManager.getInstance().replaceModel(type, model, target.getModels());

                  logger.debug("new merged model : {}", newModel);

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
              logger.warn("invalid selected node to perform the update operation = {}", currentNode);
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
              final DefaultMutableTreeNode parentNode = this.getTreeModels().getParentNode(currentNode);

              if (parentNode.getUserObject() instanceof Target) {
                  final Target target = (Target) parentNode.getUserObject();

                  logger.debug("remove model : {}", model);

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
              logger.warn("invalid selected node to perform the remove operation = {}", currentNode);
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
              final DefaultMutableTreeNode parentNode = this.getTreeModels().getParentNode(currentNode);

              // Parent can be a target or a model :
              if (parentNode.getUserObject() instanceof Target) {
                  targetNode = parentNode;
                  target = (Target) parentNode.getUserObject();

              } else if (parentNode.getUserObject() instanceof Model) {
                  throw new UnsupportedOperationException("Not implemented !");
              }
          } else {
              logger.warn("invalid selected node to perform the add operation = {}", currentNode);
          }

          if (target != null) {
              final String type = (String) this.jComboBoxModelType.getSelectedItem();

              // create a new model with defined names (model and parameters) :
              final Model newModel = ModelManager.getInstance().newModel(type, target.getModels());

              logger.debug("add model : {}", newModel);

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

  private void jButtonOpenFileActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonOpenFileActionPerformed

      final UserModel userModel = this.currentTarget.getOrCreateUserModel();

      final File currentDir;
      final String defaultFileName;

      if (userModel.getFile() != null) {
          final File file = new File(userModel.getFile());
          currentDir = file.getParentFile();
          defaultFileName = file.getName();
      } else {
          currentDir = null;
          defaultFileName = null;
      }

      final File file = FileChooser.showOpenFileChooser("Open a FITS image as user model", currentDir, mimeType, defaultFileName);

      // If a file was defined (No cancel in the dialog)
      if (file != null) {
          disableAnimator();

          userModel.setFile(file.getAbsolutePath());
          userModel.setName(file.getName());

          prepareAndValidateUserModel(userModel);

          // reselect target to update image:
          processTargetSelection(currentTarget);
      }
  }//GEN-LAST:event_jButtonOpenFileActionPerformed

    private void jButtonImageInfoActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonImageInfoActionPerformed

        final UserModel userModel = this.currentTarget.getOrCreateUserModel();

        // update checksum before validation:
        if (userModel.isModelDataReady()) {
            // note: only possible with one Fits image or one Fits cube (single HDU):
            final FitsImageHDU fitsImageHDU = userModel.getModelData(0).getFitsImageHDU();

            final String hduHeader = "ImageHDU#" + fitsImageHDU.getHduIndex() + " has "
                    + fitsImageHDU.getImageCount() + " images.\n\n" + fitsImageHDU.getHeaderCardsAsString("\n");

            MessagePane.showMessage(hduHeader);
        }
    }//GEN-LAST:event_jButtonImageInfoActionPerformed
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroupEditMode;
    private javax.swing.ButtonGroup buttonGroupModelMode;
    private javax.swing.ButtonGroup buttonGroupUserModelValid;
    private javax.swing.JButton jButtonAdd;
    private javax.swing.JButton jButtonImageInfo;
    private javax.swing.JButton jButtonNormalizeFluxes;
    private javax.swing.JButton jButtonOpenFile;
    private javax.swing.JButton jButtonRemove;
    private javax.swing.JButton jButtonUpdate;
    private javax.swing.JComboBox jComboBoxModelType;
    private javax.swing.JLabel jLabelFile;
    private javax.swing.JLabel jLabelMode;
    private javax.swing.JLabel jLabelModelDescrption;
    private javax.swing.JLabel jLabelName;
    private javax.swing.JLabel jLabelOffsetEditMode;
    private javax.swing.JLabel jLabelType;
    private javax.swing.JLabel jLabelValid;
    private javax.swing.JPanel jPanelDescription;
    private javax.swing.JPanel jPanelImage;
    private javax.swing.JPanel jPanelModel;
    private javax.swing.JPanel jPanelModelAnalytic;
    private javax.swing.JPanel jPanelParameters;
    private javax.swing.JPanel jPanelUserModel;
    private javax.swing.JRadioButton jRadioButtonAnalytical;
    private javax.swing.JRadioButton jRadioButtonInvalid;
    private javax.swing.JRadioButton jRadioButtonSepPosAngle;
    private javax.swing.JRadioButton jRadioButtonUserModel;
    private javax.swing.JRadioButton jRadioButtonValid;
    private javax.swing.JRadioButton jRadioButtonXY;
    private javax.swing.JScrollPane jScrollPaneModelDescription;
    private javax.swing.JScrollPane jScrollPaneTableModelParameters;
    private javax.swing.JScrollPane jScrollPaneTreeModels;
    private javax.swing.JSeparator jSeparatorMode;
    private javax.swing.JTable jTableModelParameters;
    private javax.swing.JTextField jTextFieldFileReference;
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

    /**
     * Prepare and validate the given user model
     * @param userModel user model to use
     */
    private static void prepareAndValidateUserModel(final UserModel userModel) {
        // see ObservationManager.checkAndLoadFileReferences() 
        boolean valid = false;
        try {
            // throws exceptions if the given fits file or image is incorrect:
            UserModelService.prepareUserModel(userModel);

            // update checksum before validation:
            if (userModel.isModelDataReady()) {
                // note: only possible with one Fits image or one Fits cube (single HDU):
                final FitsImageHDU fitsImageHDU = userModel.getModelData(0).getFitsImageHDU();

                userModel.setChecksum(fitsImageHDU.getChecksum());
            }

            // validate image against the main observation:
            ObservationManager.getInstance().validateUserModel(userModel);

            // model is valid:
            valid = true;

        } catch (IllegalArgumentException iae) {
            MessagePane.showErrorMessage("Could not use FITS image in file: " + userModel.getFile(), iae);
        } catch (FitsException fe) {
            MessagePane.showErrorMessage("Could not read file: " + userModel.getFile(), fe);
        } catch (IOException ioe) {
            MessagePane.showErrorMessage("Could not read file: " + userModel.getFile(), ioe);
        } finally {
            // anyway, update the valid flag:
            userModel.setFileValid(valid);
        }
    }

    /**
     * Perform image refresh on the given user model and image index (always on [0; modelDataList.size - 1]
     * @param userModelFile user model file to use
     * @param imageIndex image index to display
     */
    @Override
    public void perform(final String userModelFile, final int imageIndex) {
        if (this.fitsImagePanel != null) {
            // show image at given index:
            this.fitsImagePanel.setFitsImage(this.currentUserModel.getModelDataList().get(imageIndex).getFitsImage());
        }
    }
}
