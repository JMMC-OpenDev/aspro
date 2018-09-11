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
import fr.jmmc.jmal.ALX;
import fr.jmmc.jmal.model.ModelManager;
import fr.jmmc.jmal.model.gui.ModelParameterTableModel;
import fr.jmmc.jmal.model.gui.ModelParameterTableModel.EditMode;
import fr.jmmc.jmal.model.gui.ModelParameterTableModel.Mode;
import fr.jmmc.jmal.model.targetmodel.Model;
import fr.jmmc.jmcs.data.MimeType;
import fr.jmmc.jmcs.gui.component.Disposable;
import fr.jmmc.jmcs.gui.component.FileChooser;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.component.NumericJTable;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.StringUtils;
import fr.jmmc.oiexplorer.core.gui.FitsImagePanel;
import fr.jmmc.oitools.image.FitsImage;
import fr.jmmc.oitools.image.FitsImageHDU;
import fr.nom.tam.fits.FitsException;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.Timer;
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
public final class TargetModelForm extends javax.swing.JPanel implements ActionListener, PropertyChangeListener,
                                                                         TreeSelectionListener, ListSelectionListener,
                                                                         UserModelAnimatorListener, Disposable {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(TargetModelForm.class.getName());
    /** OIFits MimeType */
    private final static MimeType mimeType = MimeType.FITS_IMAGE;
    /** user model animator singleton */
    private final static UserModelAnimator animator = UserModelAnimator.getInstance();
    /** Smallest positive number used in double comparisons (rounding). */
    public final static double MAS_EPSILON = 1e-6d * ALX.MILLI_ARCSEC_IN_DEGREES;
    /** default reload latency = 20 ms */
    private static final int RELOAD_LATENCY = 20;

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
    /** flag to enable / disable the automatic update of the user model when any swing component changes */
    private boolean doAutoUpdateUserModel = true;
    /** User model reload Swing timer */
    private final Timer timerUserModelReload;

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

        // Create the timer:
        this.timerUserModelReload = new Timer(RELOAD_LATENCY, new ActionListener() {
            /**
             * Invoked when the timer action occurs.
             */
            @Override
            public void actionPerformed(final ActionEvent ae) {
                logger.debug("timerUserModelReload: FIRED");
                prepareUserModel();
            }
        });
        // never repeats
        this.timerUserModelReload.setRepeats(false);
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

        // user-defined scale / rotation:
        this.jFormattedTextFieldScaleX.addPropertyChangeListener("value", this);
        this.jFormattedTextFieldScaleY.addPropertyChangeListener("value", this);
        this.jFormattedTextFieldRotation.addPropertyChangeListener("value", this);

        // disable column reordering :
        this.jTableModelParameters.getTableHeader().setReorderingAllowed(false);

        // Fix row height:
        SwingUtils.adjustRowHeight(jTableModelParameters);

        handleLinkedState();
    }

    /**
     * Initialize the internal model (tree) from the given list of targets
     * @param targetName target name to select
     */
    void initialize(final String targetName) {
        this.generateTree();
        this.selectTarget(Target.getTarget(targetName, this.editTargets));

        if (targetName == null) {
            resetForm();
        }
    }

    /**
     * Reset and disable this form when no target is present
     */
    private void resetForm() {
        // Use a new target to reset form fields:
        processTargetSelection(Target.EMPTY_TARGET);

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
        triggerReloadUserModel(false);
    }

    /**
     * Free any resource or reference to this instance
     */
    @Override
    public void dispose() {
        disableAnimator();
        this.animatorPanel = null;

        if (this.fitsImagePanel != null) {
            this.fitsImagePanel.dispose();
            this.fitsImagePanel = null; // GC
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
        final ModelJTree tree = this.getTreeModels();

        final DefaultMutableTreeNode rootNode = tree.getRootNode();
        rootNode.removeAllChildren();

        final List<Target> calTargets = this.editTargetUserInfos.getCalibrators();

        final ArrayList<Target> sciTargets = new ArrayList<Target>(this.editTargets);
        sciTargets.removeAll(calTargets);

        // add first science targets :
        for (Target target : sciTargets) {
            final DefaultMutableTreeNode targetNode = tree.addNode(rootNode, target);

            for (Model model : target.getModels()) {
                generateModelNodes(tree, targetNode, model);
            }
        }

        //  add calibrators :
        for (Target target : calTargets) {
            final DefaultMutableTreeNode targetNode = tree.addNode(rootNode, target);

            for (Model model : target.getModels()) {
                generateModelNodes(tree, targetNode, model);
            }
        }

        // fire node structure changed :
        tree.fireNodeChanged(rootNode);
    }

    /**
     * Generate the model nodes recursively
     *
     * @param tree to update
     * @param parentNode parent node to append nodes
     * @param model model to process
     */
    private static void generateModelNodes(final ModelJTree tree, final DefaultMutableTreeNode parentNode, final Model model) {
        final DefaultMutableTreeNode modelNode = tree.addNode(parentNode, model);

        final List<Model> children = model.getModels();
        if (!children.isEmpty()) {
            for (Model child : children) {
                generateModelNodes(tree, modelNode, child);
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
        final ModelJTree tree = (ModelJTree) e.getSource();
        final DefaultMutableTreeNode currentNode = tree.getLastSelectedNode();

        if (currentNode != null) {
            // Use invokeLater to selection change issues with editors :
            SwingUtils.invokeLaterEDT(new Runnable() {
                /**
                 * Update tree selection
                 */
                @Override
                public void run() {

                    // Check if it is the root node :
                    final DefaultMutableTreeNode rootNode = tree.getRootNode();
                    if (currentNode == rootNode) {
                        tree.selectFirstChildNode(rootNode);
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

        // disable the automatic user model:
        final boolean prevAutoUpdateUserModel = setAutoUpdateUserModel(false);
        try {
            // User model:
            final UserModel userModel = target.getUserModel();
            if (userModel != null) {
                if (userModel.isFileValid()) {
                    this.jRadioButtonValid.setSelected(true);
                } else {
                    this.jRadioButtonInvalid.setSelected(true);
                }
                this.jTextFieldFileReference.setText(userModel.getFile());
                enableUserModelFields(true);

                if (!isAnalytical && userModel.isModelDataReady()) {
                    // update fits Image:
                    if (this.fitsImagePanel == null) {
                        // do not show id but options:
                        this.fitsImagePanel = new FitsImagePanel(Preferences.getInstance(), false, true) {
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
                    final FitsImage fitsImage = modelDataList.get(0).getFitsImage();

                    // use only positive increments (no direction)
                    this.jFormattedTextFieldScaleX.setValue(UserModelService.convertRadToMas(fitsImage.getIncCol()));
                    this.jFormattedTextFieldScaleY.setValue(UserModelService.convertRadToMas(fitsImage.getIncRow()));
                    this.jFormattedTextFieldRotation.setValue(fitsImage.getRotAngle());

                    this.fitsImagePanel.setFitsImage(fitsImage);

                    if (modelDataList.size() > 1) {
                        enableAnimator = true;
                        this.currentUserModel = userModel;
                    }

                    this.jPanelImage.add(this.fitsImagePanel);
                }

            } else {
                this.jRadioButtonInvalid.setSelected(true);
                this.jTextFieldFileReference.setText(null);
                this.jFormattedTextFieldScaleX.setValue(null);
                this.jFormattedTextFieldScaleY.setValue(null);
                this.jFormattedTextFieldRotation.setValue(null);

                enableUserModelFields(false);

                if (this.fitsImagePanel != null) {
                    // reset the FitsImage panel:
                    this.fitsImagePanel.setFitsImage(null);
                }

                // remove the FitsImage panel:
                this.jPanelImage.removeAll();
            }
        } finally {
            // restore the automatic update observation :
            setAutoUpdateUserModel(prevAutoUpdateUserModel);
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
        this.jButtonImageInfo.setEnabled(false);

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

    private void enableUserModelFields(boolean enabled) {
        this.jButtonImageInfo.setEnabled(enabled);
        this.jFormattedTextFieldScaleX.setEnabled(enabled);
        handleLinkedState();
        this.jFormattedTextFieldRotation.setEnabled(enabled);
        this.jButtonReset.setEnabled(enabled);
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
            // trigger reload user model:
            triggerReloadUserModel(true);
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

    @Override
    public void propertyChange(final PropertyChangeEvent e) {
        if (doAutoUpdateUserModel && this.currentTarget != null) {
            final UserModel userModel = this.currentTarget.getUserModel();

            if (userModel != null && userModel.isModelDataReady()) {
                boolean changed = false;

                // use first image:
                final FitsImage fitsImage = userModel.getModelData(0).getFitsImage();

                if (e.getSource() == this.jFormattedTextFieldScaleX) {
                    final Number val = (Number) this.jFormattedTextFieldScaleX.getValue();
                    if (val == null) {
                        if (userModel.getScaleX() != null) {
                            changed = true;
                            userModel.setScaleX(null);
                        }
                    } else {
                        final double inc = UserModelService.convertMasToRad(val.doubleValue());
                        // check increment:
                        if (logger.isDebugEnabled()) {
                            logger.debug("IncCol: {}", fitsImage.getIncCol());
                            logger.debug("inc: {}", inc);
                        }

                        if (!NumberUtils.equals(fitsImage.getIncCol(), inc, MAS_EPSILON)) {
                            changed = true;
                            if (NumberUtils.equals(fitsImage.getOrigIncCol(), inc, MAS_EPSILON)) {
                                userModel.setScaleX(null);
                            } else {
                                userModel.setScaleX(inc);
                            }
                        }
                    }
                    if (logger.isDebugEnabled()) {
                        logger.debug("scaleX: {}", userModel.getScaleX());
                    }

                    // Handle linked state:
                    if (this.jToggleButtonLinked.isSelected()) {
                        this.jFormattedTextFieldScaleY.setValue(this.jFormattedTextFieldScaleX.getValue());
                    }

                } else if (e.getSource() == this.jFormattedTextFieldScaleY) {
                    final Number val = (Number) this.jFormattedTextFieldScaleY.getValue();
                    if (val == null) {
                        if (userModel.getScaleY() != null) {
                            changed = true;
                            userModel.setScaleY(null);
                        }
                    } else {
                        final double inc = UserModelService.convertMasToRad(val.doubleValue());
                        // check increment:
                        if (logger.isDebugEnabled()) {
                            logger.debug("IncRow: {}", fitsImage.getIncRow());
                            logger.debug("inc: {}", inc);
                        }

                        if (!NumberUtils.equals(fitsImage.getIncRow(), inc, MAS_EPSILON)) {
                            changed = true;
                            if (NumberUtils.equals(fitsImage.getOrigIncRow(), inc, MAS_EPSILON)) {
                                userModel.setScaleY(null);
                            } else {
                                userModel.setScaleY(inc);
                            }
                        }
                    }
                    if (logger.isDebugEnabled()) {
                        logger.debug("scaleY: {}", userModel.getScaleY());
                    }

                } else if (e.getSource() == this.jFormattedTextFieldRotation) {
                    Number val = (Number) this.jFormattedTextFieldRotation.getValue();
                    if (val == null) {
                        if (userModel.getRotation() != null) {
                            changed = true;
                            userModel.setRotation(null);
                        }
                    } else {
                        final double rot = val.doubleValue();
                        // check rotation:
                        if (logger.isDebugEnabled()) {
                            logger.debug("RotAngle: {}", fitsImage.getRotAngle());
                            logger.debug("rot: {}", rot);
                        }

                        if (!NumberUtils.equals(fitsImage.getRotAngle(), rot)) {
                            changed = true;
                            if (NumberUtils.equals(fitsImage.getOrigRotAngle(), rot)) {
                                userModel.setRotation(null);
                            } else {
                                userModel.setRotation(rot);
                            }
                        }
                    }
                    if (logger.isDebugEnabled()) {
                        logger.debug("rotation: {}", userModel.getRotation());
                    }
                }
                if (changed) {
                    // trigger reload user model:
                    triggerReloadUserModel(true);
                }
            }
        }
    }

    void prepareUserModel() {
        if (this.currentTarget == null) {
            return;
        }

        final UserModel userModel = this.currentTarget.getUserModel();
        if (userModel == null) {
            this.jRadioButtonInvalid.setSelected(true);
        } else {
            try {
                prepareAndValidateUserModel(userModel);
            } finally {
                if (!userModel.isFileValid()) {
                    this.jRadioButtonInvalid.setSelected(true);
                }
            }

            // reselect target to update image:
            processTargetSelection(currentTarget);
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
        jLabelScale = new javax.swing.JLabel();
        jFormattedTextFieldScaleX = new javax.swing.JFormattedTextField();
        jLabelRotation = new javax.swing.JLabel();
        jFormattedTextFieldRotation = new javax.swing.JFormattedTextField();
        jButtonReset = new javax.swing.JButton();
        jFormattedTextFieldScaleY = new javax.swing.JFormattedTextField();
        jToggleButtonLinked = new javax.swing.JToggleButton();
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

        javax.swing.tree.DefaultMutableTreeNode treeNode1 = new javax.swing.tree.DefaultMutableTreeNode("Models");
        jTreeModels.setModel(new javax.swing.tree.DefaultTreeModel(treeNode1));
        jScrollPaneTreeModels.setViewportView(jTreeModels);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.3;
        gridBagConstraints.weighty = 0.1;
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

        jTextFieldFileReference.setEditable(false);
        jTextFieldFileReference.setColumns(20);
        jTextFieldFileReference.setText("FileReference");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.6;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
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

        jLabelScale.setText("Scale");
        jLabelScale.setToolTipText("image increments in mas");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 6);
        jPanelUserModel.add(jLabelScale, gridBagConstraints);

        jFormattedTextFieldScaleX.setColumns(10);
        jFormattedTextFieldScaleX.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("0.00##E0"))));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_START;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelUserModel.add(jFormattedTextFieldScaleX, gridBagConstraints);

        jLabelRotation.setText("Rotation");
        jLabelRotation.setToolTipText("image rotation in degrees");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 6);
        jPanelUserModel.add(jLabelRotation, gridBagConstraints);

        jFormattedTextFieldRotation.setColumns(10);
        jFormattedTextFieldRotation.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0.00"))));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_START;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelUserModel.add(jFormattedTextFieldRotation, gridBagConstraints);

        jButtonReset.setText("reset");
        jButtonReset.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonResetActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_START;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelUserModel.add(jButtonReset, gridBagConstraints);

        jFormattedTextFieldScaleY.setColumns(10);
        jFormattedTextFieldScaleY.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("0.00##E0"))));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_START;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelUserModel.add(jFormattedTextFieldScaleY, gridBagConstraints);

        jToggleButtonLinked.setSelected(true);
        jToggleButtonLinked.setText("linked");
        jToggleButtonLinked.setEnabled(false);
        jToggleButtonLinked.setMargin(new java.awt.Insets(0, 0, 0, 0));
        jToggleButtonLinked.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jToggleButtonLinkedActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelUserModel.add(jToggleButtonLinked, gridBagConstraints);

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
        gridBagConstraints.weighty = 0.1;
        add(jPanelModel, gridBagConstraints);

        jPanelDescription.setBorder(javax.swing.BorderFactory.createTitledBorder("Model description"));
        jPanelDescription.setMaximumSize(new java.awt.Dimension(2147483647, 250));
        jPanelDescription.setMinimumSize(new java.awt.Dimension(100, 150));
        jPanelDescription.setPreferredSize(new java.awt.Dimension(100, 150));
        jPanelDescription.setLayout(new java.awt.GridBagLayout());

        jLabelModelDescrption.setBackground(java.awt.Color.white);
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
        gridBagConstraints.weighty = 0.3;
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
        gridBagConstraints.weighty = 0.9;
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
      final ModelJTree tree = this.getTreeModels();
      final DefaultMutableTreeNode currentNode = tree.getLastSelectedNode();

      if (currentNode != null) {
          if (currentNode.getUserObject() instanceof Model) {
              final Model model = (Model) currentNode.getUserObject();

              final String type = (String) this.jComboBoxModelType.getSelectedItem();

              // check if the type changed :
              if (model.getType().equals(type)) {
                  return;
              }

              // Parent can be a target or a model :
              final DefaultMutableTreeNode parentNode = tree.getParentNode(currentNode);

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
                  tree.fireNodeChanged(parentNode);

                  // Select the new node = model :
                  tree.selectPath(new TreePath(newNode.getPath()));

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
      final ModelJTree tree = this.getTreeModels();
      final DefaultMutableTreeNode currentNode = tree.getLastSelectedNode();

      if (currentNode != null) {

          if (currentNode.getUserObject() instanceof Model) {
              final Model model = (Model) currentNode.getUserObject();

              // Parent can be a target or a model :
              final DefaultMutableTreeNode parentNode = tree.getParentNode(currentNode);

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
                  tree.removeNodeAndRefresh(parentNode, currentNode);

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
      final ModelJTree tree = this.getTreeModels();
      final DefaultMutableTreeNode currentNode = tree.getLastSelectedNode();

      if (currentNode != null) {

          DefaultMutableTreeNode targetNode = null;
          Target target = null;

          if (currentNode.getUserObject() instanceof Target) {
              targetNode = currentNode;
              target = (Target) currentNode.getUserObject();

          } else if (currentNode.getUserObject() instanceof Model) {
              final DefaultMutableTreeNode parentNode = tree.getParentNode(currentNode);

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
              tree.addNodeAndRefresh(targetNode, newModel);
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
      if (this.currentTarget == null) {
          return;
      }

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

          // reset Transforms:
          userModel.setScaleX(null);
          userModel.setScaleY(null);
          userModel.setRotation(null);

          // trigger reload user model:
          triggerReloadUserModel(true);
      }
  }//GEN-LAST:event_jButtonOpenFileActionPerformed

    private void jButtonImageInfoActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonImageInfoActionPerformed
        if (this.currentTarget == null) {
            return;
        }

        final UserModel userModel = this.currentTarget.getOrCreateUserModel();

        if (userModel.isModelDataReady()) {
            // note: only possible with one Fits image or one Fits cube (single HDU):
            final FitsImageHDU fitsImageHDU = userModel.getModelData(0).getFitsImageHDU();

            final String hduHeader = "ImageHDU#" + fitsImageHDU.getExtNb() + " has "
                    + fitsImageHDU.getImageCount() + " images.\n\n" + fitsImageHDU.getHeaderCardsAsString("\n");

            MessagePane.showMessage(hduHeader);
        }
    }//GEN-LAST:event_jButtonImageInfoActionPerformed

    private void jButtonResetActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonResetActionPerformed
        if (this.currentTarget == null) {
            return;
        }

        final UserModel userModel = this.currentTarget.getUserModel();
        if (userModel != null && userModel.isModelDataReady()) {
            // use first image:
            final FitsImage fitsImage = userModel.getModelData(0).getFitsImage();

            // use only positive increments (no direction)
            this.jFormattedTextFieldScaleX.setValue(UserModelService.convertRadToMas(fitsImage.getOrigIncCol()));
            this.jFormattedTextFieldScaleY.setValue(UserModelService.convertRadToMas(fitsImage.getOrigIncRow()));
            this.jFormattedTextFieldRotation.setValue(fitsImage.getOrigRotAngle());
        }
    }//GEN-LAST:event_jButtonResetActionPerformed

    private void jToggleButtonLinkedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jToggleButtonLinkedActionPerformed
        handleLinkedState();
    }//GEN-LAST:event_jToggleButtonLinkedActionPerformed

    private void handleLinkedState() {
        // TODO: if linked is OFF, then allow editing scaleY field
        this.jFormattedTextFieldScaleY.setEnabled(!this.jToggleButtonLinked.isSelected() && this.jFormattedTextFieldScaleX.isEnabled());
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroupEditMode;
    private javax.swing.ButtonGroup buttonGroupModelMode;
    private javax.swing.ButtonGroup buttonGroupUserModelValid;
    private javax.swing.JButton jButtonAdd;
    private javax.swing.JButton jButtonImageInfo;
    private javax.swing.JButton jButtonNormalizeFluxes;
    private javax.swing.JButton jButtonOpenFile;
    private javax.swing.JButton jButtonRemove;
    private javax.swing.JButton jButtonReset;
    private javax.swing.JButton jButtonUpdate;
    private javax.swing.JComboBox jComboBoxModelType;
    private javax.swing.JFormattedTextField jFormattedTextFieldRotation;
    private javax.swing.JFormattedTextField jFormattedTextFieldScaleX;
    private javax.swing.JFormattedTextField jFormattedTextFieldScaleY;
    private javax.swing.JLabel jLabelFile;
    private javax.swing.JLabel jLabelMode;
    private javax.swing.JLabel jLabelModelDescrption;
    private javax.swing.JLabel jLabelName;
    private javax.swing.JLabel jLabelOffsetEditMode;
    private javax.swing.JLabel jLabelRotation;
    private javax.swing.JLabel jLabelScale;
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
    private javax.swing.JToggleButton jToggleButtonLinked;
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
                    if ((column == 0) && comp instanceof JLabel) {
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

    /**
     * Enable / Disable the automatic update of the user model when any swing component changes.
     * Return its previous value.
     *
     * Typical use is as following :
     * // disable the automatic user model:
     * final boolean prevAutoUpdateUserModel = setAutoUpdateUserModel(false);
     * try {
     *   // operations ...
     *
     * } finally {
     *   // restore the automatic update observation :
     *   setAutoUpdateUserModel(prevAutoUpdateUserModel);
     * }
     *
     * @param value new value
     * @return previous value
     */
    private boolean setAutoUpdateUserModel(final boolean value) {
        // first backup the state of the automatic update user model:
        final boolean previous = doAutoUpdateUserModel;

        // then change its state :
        doAutoUpdateUserModel = value;

        // return previous state :
        return previous;
    }

    /**
     * Trigger the internal User Model Reload timer
     * @param enable true to enable it, false otherwise
     */
    private void triggerReloadUserModel(final boolean enable) {
        if (enable) {
            disableAnimator();
            if (this.timerUserModelReload.isRunning()) {
                logger.debug("RE-starting timer: {}", this.timerUserModelReload);
                this.timerUserModelReload.restart();
            } else {
                logger.debug("Starting timer: {}", this.timerUserModelReload);
                this.timerUserModelReload.start();
            }
        } else if (this.timerUserModelReload.isRunning()) {
            logger.debug("Stopping timer: {}", this.timerUserModelReload);
            this.timerUserModelReload.stop();
        }
    }
}
