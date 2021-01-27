/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import com.jidesoft.swing.CheckBoxList;
import fr.jmmc.aspro.gui.util.AbstractTargetJTree;
import fr.jmmc.aspro.gui.util.TargetGroupJTree;
import fr.jmmc.aspro.gui.util.TargetGroupRenderer;
import fr.jmmc.aspro.gui.util.TargetGroupTransferHandler;
import fr.jmmc.aspro.gui.util.TargetGroupTreeCellRenderer;
import fr.jmmc.aspro.gui.util.TargetRenderer;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetGroup;
import fr.jmmc.aspro.model.oi.TargetGroupMembers;
import fr.jmmc.aspro.model.oi.TargetInformation;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.jmcs.gui.component.GenericListModel;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.util.ColorEncoder;
import fr.jmmc.jmcs.util.StringUtils;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JDialog;
import javax.swing.JFormattedTextField;
import javax.swing.TransferHandler;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.text.DefaultFormatter;
import javax.swing.tree.DefaultMutableTreeNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class represents the target group editor ...
 *
 * @author bourgesl
 */
public class TargetGroupForm extends javax.swing.JPanel implements PropertyChangeListener, TreeSelectionListener {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(TargetGroupForm.class.getName());
    /** widget name for the tree containing Targets */
    public static final String TREE_TARGETS = "TARGETS";
    /** widget name for the tree containing Target groups */
    public static final String TREE_GROUPS = "GROUPS";
    /** prefix for user group identifier */
    public static final String ID_GEN_PREFIX = "USER_";

    /* members */
    /** list of edited targets (clone) */
    private final List<Target> editTargets;
    /** edited target user informations (clone) */
    private final TargetUserInformations editTargetUserInfos;

    /** current edited target to detect target changes */
    private Target currentTarget = null;
    /** current edited group to detect group changes */
    private TargetGroup currentGroup = null;

    /** disabled groups in CheckBoxList */
    private final Set<TargetGroup> disabledGroupsInCheckBoxList = new HashSet<TargetGroup>();
    /** Custom color editor */
    private final ColorEditor colorEditor;
    /** flag to enable / disable the automatic update of the target when any swing component changes */
    private boolean doAutoUpdateTarget = true;
    /** flag to enable / disable the automatic update of the group when any swing component changes */
    private boolean doAutoUpdateGroup = true;
    /** user group identifier generator */
    private final AtomicInteger ID_GEN = new AtomicInteger(1);

    /**
     * Creates new form TargetGroupForm (used by NetBeans editor only)
     */
    public TargetGroupForm() {
        this(null, null);
    }

    /**
     * Creates new form TargetGroupForm
     * @param targets list of targets to edit
     * @param targetUserInfos target user informations
     */
    public TargetGroupForm(final List<Target> targets, final TargetUserInformations targetUserInfos) {
        super();

        this.editTargets = targets;
        this.editTargetUserInfos = targetUserInfos;

        initComponents();

        this.colorEditor = new ColorEditor(jButtonEdit);

        postInit();
    }

    /**
     * This method is useful to set the models and specific features of initialized swing components.
     */
    private void postInit() {

        final TargetRenderer renderer = new TargetRenderer(this.editTargetUserInfos);
        final TargetGroupTreeCellRenderer cellRenderer = new TargetGroupTreeCellRenderer(renderer);

        // tree selection listener :
        this.jTreeTargets.addTreeSelectionListener(this);
        this.jTreeTargets.setCellRenderer(cellRenderer);

        // tree selection listener :
        this.jTreeGroups.addTreeSelectionListener(this);
        this.jTreeGroups.setCellRenderer(cellRenderer);

        this.checkBoxListGroups.setModel(new GenericListModel<TargetGroup>(this.editTargetUserInfos.getGroups()));
        this.checkBoxListGroups.setCellRenderer(TargetGroupRenderer.INSTANCE);

        this.checkBoxListGroups.getCheckBoxListSelectionModel().addListSelectionListener(new ListSelectionListener() {
            @Override
            public void valueChanged(final ListSelectionEvent e) {
                if (!e.getValueIsAdjusting()) {
                    targetGroupsChanged();
                }
            }
        });

        // Add custom DnD support :
        final TransferHandler targetGroupTransferHandler = new TargetGroupTransferHandler(this.editTargets, this.editTargetUserInfos, this);
        this.jTreeTargets.setTransferHandler(targetGroupTransferHandler);
        this.jTreeGroups.setTransferHandler(targetGroupTransferHandler);

        // add property change listener to editable fields :
        this.jFieldGroupName.addPropertyChangeListener("value", this);
        this.jFieldGroupColor.addPropertyChangeListener("value", this);

        // add document listener to target description :
        this.jTextAreaGroupDesc.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void insertUpdate(final DocumentEvent e) {
                groupDescChanged();
            }

            @Override
            public void removeUpdate(final DocumentEvent e) {
                groupDescChanged();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                //Plain text components do not fire these events
            }
        });
    }

    public void refresh() {
        refresh(true);
    }

    private void refresh(boolean force) {
        final Target current = this.currentTarget; // backup
        if (force) {
            this.currentTarget = null; // reset to refresh target form
        }
        generateTreesAndSelectTarget(current);
    }

    /**
     * Initialize the internal model (tree) from the given list of targets
     * @param targetName target name to select
     */
    void initialize(final String targetName) {
        generateTreesAndSelectTarget(Target.getTarget(targetName, this.editTargets));
    }

    private void generateTreesAndSelectTarget(final Target target) {
        logger.debug("generateTreesAndSelectTarget: {}", target);

        final TargetGroup current = this.currentGroup; // backup

        generateTrees();
        selectTarget(target, null);
        if (isNotNull(current)) {
            SwingUtils.invokeLaterEDT(new Runnable() {
                @Override
                public void run() {
                    selectGroup(current);
                }
            });
        }

    }

    /* Tree related methods */
    /**
     * Generate the tree from the current edited list of targets
     */
    private void generateTrees() {
        generateTreeTargets();
        generateTreeGroups();
    }

    /**
     * Return the custom TargetJTree
     * @return TargetJTree
     */
    private TargetGroupJTree getTreeTargets() {
        return (TargetGroupJTree) this.jTreeTargets;
    }

    /**
     * Generate the tree from the current edited list of targets
     */
    private void generateTreeTargets() {
        final TargetGroupJTree tree = this.getTreeTargets();

        final DefaultMutableTreeNode rootNode = tree.getRootNode();
        rootNode.removeAllChildren();

        final List<Target> calTargets = this.editTargetUserInfos.getCalibrators();

        final ArrayList<Target> sciTargets = new ArrayList<Target>(this.editTargets);
        sciTargets.removeAll(calTargets);

        // remove any target in OB groups:
        for (TargetGroup group : this.editTargetUserInfos.getGroups()) {
            if (group.isCategoryOB()) {
                TargetGroupMembers tgm = this.editTargetUserInfos.getGroupMembers(group);
                if (tgm != null && !tgm.isEmpty()) {
                    sciTargets.removeAll(tgm.getTargets());
                }
            }
        }

        // add first science targets :
        for (Target target : sciTargets) {
            generateTargetNodes(tree, rootNode, target);
        }

        //  add calibrators :
        for (Target target : calTargets) {
            generateTargetNodes(tree, rootNode, target);
        }

        // fire node structure changed :
        tree.fireNodeChanged(rootNode);
    }

    /**
     * Generate the target and its group member nodes
     *
     * @param tree to update
     * @param rootNode root node
     * @param target target
     */
    private void generateTargetNodes(final TargetGroupJTree tree, final DefaultMutableTreeNode rootNode, final Target target) {
        final DefaultMutableTreeNode targetNode = tree.addNode(rootNode, target);

        // add target links as children of the target Node :
        final TargetInformation targetInfo = this.editTargetUserInfos.getOrCreateTargetInformation(target);

        for (TargetGroupMembers tgm : targetInfo.getGroupMembers()) {
            if (!tgm.isEmpty()) {
                final DefaultMutableTreeNode targetGroupNode = tree.addNode(targetNode, tgm.getGroupRef());

                for (Target childTarget : tgm.getTargets()) {
                    tree.addNode(targetGroupNode, childTarget);
                }
            }
        }
    }

    /**
     * Return the custom TargetGroupJTree
     * @return TargetGroupJTree
     */
    private TargetGroupJTree getTreeGroups() {
        return (TargetGroupJTree) this.jTreeGroups;
    }

    /**
     * Generate the tree from the current edited list of groups
     */
    private void generateTreeGroups() {
        final TargetGroupJTree tree = this.getTreeGroups();

        final DefaultMutableTreeNode rootNode = tree.getRootNode();
        rootNode.removeAllChildren();

        for (TargetGroup group : this.editTargetUserInfos.getGroups()) {
            final DefaultMutableTreeNode groupNode = tree.addNode(rootNode, group);

            // add target members as children of the Group Node :
            TargetGroupMembers gm = this.editTargetUserInfos.getOrCreateGroupMembers(group);
            for (Target target : gm.getTargets()) {
                tree.addNode(groupNode, target);
            }
        }

        // fire node structure changed :
        tree.fireNodeChanged(rootNode);
    }

    /**
     * Select the target in the target trees for the given target
     * @param target to select
     * @param source optional tree source
     */
    void selectTarget(final Target target, final Object source) {
        if (getTreeTargets() != source) {
            getTreeTargets().selectTarget(target, false); // ignore missing
        }
        if (getTreeGroups() != source) {
            getTreeGroups().selectTarget(target, false); // ignore missing
        }
        if (target == null) {
            // no target anymore:
            processTargetSelection(null);
        }
    }

    /**
     * Select the group for the given group
     * @param group to select
     */
    void selectGroup(final TargetGroup group) {
        getTreeGroups().selectTarget(group, false); // ignore missing
    }

    /**
     * Process the tree selection events
     * @param e tree selection event
     */
    @Override
    public void valueChanged(final TreeSelectionEvent e) {
        final AbstractTargetJTree<?> tree = (AbstractTargetJTree) e.getSource();
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

                    if (userObject instanceof Target) {
                        final Target target = (Target) userObject;

                        if (target != currentTarget) {
                            logger.debug("tree[{}] selection: {}", tree.getName(), target);

                            processTargetSelection(target);

                            // Finally update selection accross trees:
                            selectTarget(target, tree);
                        }

                        processGroupSelection(TargetGroup.EMPTY_GROUP);

                    } else if (userObject instanceof TargetGroup) {
                        final TargetGroup group = (TargetGroup) userObject;

                        if (group != currentGroup) {
                            logger.debug("tree[{}] selection: {}", tree.getName(), group);

                            processGroupSelection(group);
                        }
                    }
                }
            });
        }
    }

    /**
     * Update the UI when a target is selected in the target tree
     * @param target selected target
     */
    private void processTargetSelection(final Target target) {
        final String targetName = (target != null) ? target.getName() : null;
        logger.debug("processTargetSelection: {}", targetName);

        // update the current target :
        this.currentTarget = target;

        // disable the automatic update target :
        final boolean prevAutoUpdateTarget = this.setAutoUpdateTarget(false);
        try {
            // note : setText() / setValue() methods fire a property change event :

            // name :
            this.jFieldTargetName.setText(targetName);
            this.jFieldTargetName.setToolTipText(getTreeTargets().getTooltipText(target));

            // Update selection in checkboxlist :
            this.checkBoxListGroups.selectNone();
            this.disabledGroupsInCheckBoxList.clear();

            if (target != null) {
                // indicates if the target has associated targets (implicitely OB targets):
                final boolean hasGM = this.editTargetUserInfos.getOrCreateTargetInformation(target).hasGroupMembers();

                // get used groups by the target (associated OB target):
                final Set<TargetGroup> usedGroups = (hasGM) ? null : this.editTargetUserInfos.getGroupsUsedByTargetGroupMembers(target);

                boolean shouldScroll = true;

                for (TargetGroup group : this.editTargetUserInfos.getGroups()) {
                    TargetGroupMembers gm = this.editTargetUserInfos.getOrCreateGroupMembers(group);

                    if (gm.hasTarget(target)) {
                        this.checkBoxListGroups.addCheckBoxListSelectedValue(group, shouldScroll);
                        shouldScroll = false;
                    }
                    if (group.isCategoryOB()) {
                        if (hasGM || usedGroups.contains(group)) {
                            // disable the checkbox on this group:
                            this.disabledGroupsInCheckBoxList.add(group);
                        }
                    }
                }
            } else {
                this.disabledGroupsInCheckBoxList.addAll(this.editTargetUserInfos.getGroups());
            }
            // Force repaint:
            this.checkBoxListGroups.repaint();

            // TODO: show relations (its groups and its associated targets) ?
        } finally {
            // restore the automatic update target :
            this.setAutoUpdateTarget(prevAutoUpdateTarget);
        }
    }

    /**
     * Process the target selected groups
     */
    void targetGroupsChanged() {
        // check if the automatic update flag is enabled :
        if (this.doAutoUpdateTarget && currentTarget != null) {
            final List<?> selectedGroups = Arrays.asList(checkBoxListGroups.getCheckBoxListSelectedValues());

            // add target in group:
            for (Object o : selectedGroups) {
                final TargetGroup g = (TargetGroup) o;
                this.editTargetUserInfos.getOrCreateGroupMembers(g).addTarget(currentTarget); // avoid duplicates
            }

            // remove target in non-selected groups:
            for (TargetGroup g : this.editTargetUserInfos.getGroups()) {
                if (!selectedGroups.contains(g)) {
                    // TODO: confirm or use trees to check if used ?
                    this.editTargetUserInfos.removeTargetFromTargetGroup(g, currentTarget);
                }
            }

            // refresh trees:
            refresh(false);
        }
    }

    boolean isCheckBoxListGroupsEnabled(final int index) {
        final TargetGroup group = (TargetGroup) this.checkBoxListGroups.getModel().getElementAt(index);
        if (group != null) {
            logger.debug("isCheckBoxListGroupsEnabled: {} - disabled groups: {}", group, disabledGroupsInCheckBoxList);

            if (this.disabledGroupsInCheckBoxList.contains(group)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Update the UI when a group is selected in the target/group tree
     * @param group selected group
     */
    private void processGroupSelection(final TargetGroup group) {
        if (group != currentGroup) {
            // update the current group :
            this.currentGroup = group;

            // disable the automatic update group :
            final boolean prevAutoUpdateGroup = this.setAutoUpdateGroup(false);
            try {
                this.jFieldGroupName.setText(group.getName());

                // TODO: update category list:
                this.jComboBoxCategory.setSelectedItem(group.getCategory());
                this.jComboBoxCategory.setEnabled(false);

                this.jTextAreaGroupDesc.setText(group.getDescription());

                this.jFieldGroupColor.setText(group.getColor());
                this.jFieldGroupColor.setBackground(decodeColor()); // not automatic (disabled)

                // disable if EMPTY or OB group:
                final boolean enabled = (group != TargetGroup.EMPTY_GROUP && !group.isCategoryOB());

                this.jFieldGroupName.setEnabled(enabled);
                this.jTextAreaGroupDesc.setEnabled(enabled);
                this.jFieldGroupColor.setEnabled(enabled);
                this.jButtonEdit.setEnabled(enabled);
            } finally {
                // restore the automatic update group :
                this.setAutoUpdateGroup(prevAutoUpdateGroup);
            }
        }
    }

    private Color decodeColor() {
        final String col = this.jFieldGroupColor.getText();
        if (col != null && !col.isEmpty()) {
            try {
                return java.awt.Color.decode(col);
            } catch (NumberFormatException nfe) {
                logger.debug("Unable to decode color [{}]", col, nfe);
            }
        }
        return null;
    }

    /**
     * Process the change event for any field and update the associated group
     * @param evt property change event
     */
    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        // check if the automatic update flag is enabled :
        if (this.doAutoUpdateGroup && isNotNull(this.currentGroup)) {
            boolean refresh = false;

            final JFormattedTextField field = (JFormattedTextField) evt.getSource();

            final String oldValue = (String) evt.getOldValue();
            final String value = (String) evt.getNewValue();

            // check if value changed (null supported):
            if (!isChanged(value, oldValue)) {
                return;
            }

            if (logger.isDebugEnabled()) {
                logger.debug("field {} new: {} old: {}", field.getName(), value, oldValue);
            }

            if (field == this.jFieldGroupName) {
                this.currentGroup.setName(value);
                refresh = true;
            } else if (field == this.jFieldGroupColor) {
                this.currentGroup.updateColor(value); // to update cached decoded color
                refresh = true;
            }

            if (refresh) {
                refresh(); // to repaint checkbox list (color)
            }
        }
    }

    /**
     * Process the document change event for the group description
     */
    private void groupDescChanged() {
        // check if the automatic update flag is enabled :
        if (this.doAutoUpdateGroup && isNotNull(this.currentGroup)) {
            final String text = this.jTextAreaGroupDesc.getText();
            logger.debug("desc: {}", text);

            this.currentGroup.setDescription((text.length() > 0) ? text : null);
        }
    }

    /**
     * Validate the form
     * @return true only if the data are valid
     */
    boolean validateForm() {
        // nothing to validate
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
     * Enable / Disable the automatic update of the target when any swing component changes.
     * Return its previous value.
     *
     * Typical use is as following :
     * // disable the automatic update target :
     * final boolean prevAutoUpdateTarget = this.setAutoUpdateTarget(false);
     * try {
     *   // operations ...
     *
     * } finally {
     *   // restore the automatic update target :
     *   this.setAutoUpdateTarget(prevAutoUpdateTarget);
     * }
     *
     * @param value new value
     * @return previous value
     */
    private boolean setAutoUpdateTarget(final boolean value) {
        // first backup the state of the automatic update target :
        final boolean previous = this.doAutoUpdateTarget;

        // then change its state :
        this.doAutoUpdateTarget = value;

        // return previous state :
        return previous;
    }

    /**
     * Enable / Disable the automatic update of the group when any swing component changes.
     * Return its previous value.
     *
     * Typical use is as following :
     * // disable the automatic update group :
     * final boolean prevAutoUpdateGroup = this.setAutoUpdateGroup(false);
     * try {
     *   // operations ...
     *
     * } finally {
     *   // restore the automatic update group :
     *   this.setAutoUpdateGroup(prevAutoUpdateGroup);
     * }
     *
     * @param value new value
     * @return previous value
     */
    private boolean setAutoUpdateGroup(final boolean value) {
        // first backup the state of the automatic update group :
        final boolean previous = this.doAutoUpdateGroup;

        // then change its state :
        this.doAutoUpdateGroup = value;

        // return previous state :
        return previous;
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

        jScrollPaneTreeTargets = new javax.swing.JScrollPane();
        jTreeTargets = new TargetGroupJTree(this.editTargetUserInfos);
        jScrollPaneTreeGroups = new javax.swing.JScrollPane();
        jTreeGroups = new TargetGroupJTree(this.editTargetUserInfos);
        jPanelActions = new javax.swing.JPanel();
        jButtonRemoveAssociation = new javax.swing.JButton();
        jButtonAddGroup = new javax.swing.JButton();
        jButtonDeleteGroup = new javax.swing.JButton();
        jPanelTarget = new javax.swing.JPanel();
        jLabelTargetName = new javax.swing.JLabel();
        jFieldTargetName = new JFormattedTextField(createFieldNameFormatter());
        jScrollPaneGroups = new javax.swing.JScrollPane();
        checkBoxListGroups = createCheckBoxList();
        jLabelTargetGroups = new javax.swing.JLabel();
        jPanelGroup = new javax.swing.JPanel();
        jLabelGroupName = new javax.swing.JLabel();
        jFieldGroupName = new JFormattedTextField(createFieldNameFormatter());
        jLabelCategory = new javax.swing.JLabel();
        jComboBoxCategory = new javax.swing.JComboBox();
        jLabelDescription = new javax.swing.JLabel();
        jScrollPaneDesc = new javax.swing.JScrollPane();
        jTextAreaGroupDesc = new javax.swing.JTextArea();
        jLabelColor = new javax.swing.JLabel();
        jFieldGroupColor = new JFormattedTextField(createFieldColorFormatter());
        jButtonEdit = new javax.swing.JButton();

        setLayout(new java.awt.GridBagLayout());

        jScrollPaneTreeTargets.setMinimumSize(new java.awt.Dimension(80, 100));
        jScrollPaneTreeTargets.setPreferredSize(new java.awt.Dimension(160, 100));

        javax.swing.tree.DefaultMutableTreeNode treeNode1 = new javax.swing.tree.DefaultMutableTreeNode("Targets");
        jTreeTargets.setModel(new javax.swing.tree.DefaultTreeModel(treeNode1));
        jTreeTargets.setToolTipText("<html>\nthis tree contains only science targets (SCI / CAL), with their ancillary stars (AO / FT/ Guide).<br>\nUse <b>Drag &amp; Drop</b> to make target associations (groups) or link any ancillary star to the science target\n</html>\n");
        jTreeTargets.setDragEnabled(true);
        jTreeTargets.setName(TREE_TARGETS);
        jScrollPaneTreeTargets.setViewportView(jTreeTargets);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.3;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        add(jScrollPaneTreeTargets, gridBagConstraints);

        jScrollPaneTreeGroups.setMinimumSize(new java.awt.Dimension(80, 100));
        jScrollPaneTreeGroups.setPreferredSize(new java.awt.Dimension(160, 100));

        treeNode1 = new javax.swing.tree.DefaultMutableTreeNode("Groups");
        jTreeGroups.setModel(new javax.swing.tree.DefaultTreeModel(treeNode1));
        jTreeGroups.setToolTipText("<html>\nthis tree lists target groups, like ancillary stars (AO / FT/ Guide) or user-defined groups.<br>\nUse <b>Drag &amp; Drop</b> to add / move targets within groups or link any ancillary star to the science target\n</html>\n");
        jTreeGroups.setDragEnabled(true);
        jTreeGroups.setName(TREE_GROUPS);
        jScrollPaneTreeGroups.setViewportView(jTreeGroups);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        add(jScrollPaneTreeGroups, gridBagConstraints);

        jPanelActions.setLayout(new java.awt.GridBagLayout());

        jButtonRemoveAssociation.setText("Remove association");
        jButtonRemoveAssociation.setToolTipText("Remove the selected target association");
        jButtonRemoveAssociation.setFocusable(false);
        jButtonRemoveAssociation.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonRemoveAssociation.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButtonRemoveAssociation.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonRemoveAssociationActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelActions.add(jButtonRemoveAssociation, gridBagConstraints);

        jButtonAddGroup.setText("Add group");
        jButtonAddGroup.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonAddGroupActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelActions.add(jButtonAddGroup, gridBagConstraints);

        jButtonDeleteGroup.setText("Delete group");
        jButtonDeleteGroup.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonDeleteGroupActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelActions.add(jButtonDeleteGroup, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        add(jPanelActions, gridBagConstraints);

        jPanelTarget.setBorder(javax.swing.BorderFactory.createTitledBorder("Target"));
        jPanelTarget.setLayout(new java.awt.GridBagLayout());

        jLabelTargetName.setText("Name");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelTarget.add(jLabelTargetName, gridBagConstraints);

        jFieldTargetName.setEditable(false);
        jFieldTargetName.setColumns(10);
        jFieldTargetName.setName("NAME"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelTarget.add(jFieldTargetName, gridBagConstraints);

        checkBoxListGroups.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        checkBoxListGroups.setToolTipText("groups whose target belongs to");
        checkBoxListGroups.setVisibleRowCount(3);
        jScrollPaneGroups.setViewportView(checkBoxListGroups);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelTarget.add(jScrollPaneGroups, gridBagConstraints);

        jLabelTargetGroups.setText("Groups");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        jPanelTarget.add(jLabelTargetGroups, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.2;
        gridBagConstraints.weighty = 0.7;
        add(jPanelTarget, gridBagConstraints);

        jPanelGroup.setBorder(javax.swing.BorderFactory.createTitledBorder("Group"));
        jPanelGroup.setLayout(new java.awt.GridBagLayout());

        jLabelGroupName.setText("Name");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanelGroup.add(jLabelGroupName, gridBagConstraints);

        jFieldGroupName.setColumns(10);
        jFieldGroupName.setName("NAME"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        jPanelGroup.add(jFieldGroupName, gridBagConstraints);

        jLabelCategory.setText("Category");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanelGroup.add(jLabelCategory, gridBagConstraints);

        jComboBoxCategory.setEditable(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        jPanelGroup.add(jComboBoxCategory, gridBagConstraints);

        jLabelDescription.setText("Desc.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanelGroup.add(jLabelDescription, gridBagConstraints);

        jTextAreaGroupDesc.setColumns(10);
        jTextAreaGroupDesc.setLineWrap(true);
        jTextAreaGroupDesc.setRows(2);
        jTextAreaGroupDesc.setTabSize(4);
        jScrollPaneDesc.setViewportView(jTextAreaGroupDesc);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        jPanelGroup.add(jScrollPaneDesc, gridBagConstraints);

        jLabelColor.setText("Color");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanelGroup.add(jLabelColor, gridBagConstraints);

        jFieldGroupColor.setBackground(null);
        jFieldGroupColor.setColumns(10);
        jFieldGroupColor.setText("#FFFFFF");
        jFieldGroupColor.setToolTipText("");
        jFieldGroupColor.setName("NAME"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        jPanelGroup.add(jFieldGroupColor, gridBagConstraints);

        jButtonEdit.setText("edit");
        jButtonEdit.setMargin(new java.awt.Insets(0, 0, 0, 0));
        jButtonEdit.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonEditActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_START;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 4);
        jPanelGroup.add(jButtonEdit, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weighty = 0.3;
        add(jPanelGroup, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void jButtonEditActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonEditActionPerformed
        final Color color = colorEditor.show(decodeColor());
        if (color != null) {
            this.jFieldGroupColor.setValue(ColorEncoder.encode(color));
        }
    }//GEN-LAST:event_jButtonEditActionPerformed

    private void jButtonRemoveAssociationActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonRemoveAssociationActionPerformed
        final TargetGroupJTree tree = getTreeTargets();
        final DefaultMutableTreeNode currentNode = tree.getLastSelectedNode();

        if (currentNode != null) {
            /* retrieve the node that was selected */
            if (currentNode.getUserObject() instanceof Target) {
                final Target target = (Target) currentNode.getUserObject();

                logger.debug("tree selection: {}", target);

                // Parent can be a target or null :
                DefaultMutableTreeNode parentNode = tree.getParentNode(currentNode);

                if (parentNode != null && parentNode.getUserObject() instanceof TargetGroup) {
                    final TargetGroup parentGroup = (TargetGroup) parentNode.getUserObject();

                    if (parentGroup != null) {
                        logger.debug("group: {}", parentGroup);

                        parentNode = tree.getParentNode(parentNode);

                        if (parentNode != null && parentNode.getUserObject() instanceof Target) {
                            final Target parentTarget = (Target) parentNode.getUserObject();

                            logger.debug("parentTarget: {}", parentTarget);

                            // remove target association:
                            this.editTargetUserInfos.getOrCreateTargetInformation(parentTarget).removeTargetInGroupMembers(parentGroup, target);

                            refresh();
                        }
                    }
                }
            }
        }
    }//GEN-LAST:event_jButtonRemoveAssociationActionPerformed

    private void jButtonDeleteGroupActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonDeleteGroupActionPerformed
        if (this.editTargetUserInfos.removeGroup(currentGroup)) {
            this.currentGroup = null;
            refresh();
        } else {
            StatusBar.show("Unable to remove group (OB or still used)");
        }
    }//GEN-LAST:event_jButtonDeleteGroupActionPerformed

    private void jButtonAddGroupActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonAddGroupActionPerformed
        int counter;
        String id, name;

        do {
            counter = ID_GEN.getAndIncrement();
            id = ID_GEN_PREFIX + counter;
            name = "User Group " + counter;
        } while (editTargetUserInfos.getGroupById(id) != null
                || editTargetUserInfos.getGroupByName(name) != null);

        // identifier is available:
        TargetGroup g = new TargetGroup(id, name, TargetGroup.CATEGORY_USER, null, "#A0A0A0");
        this.editTargetUserInfos.addGroup(g);

        // really necessary to refresh view:
        this.checkBoxListGroups.setModel(new GenericListModel<TargetGroup>(this.editTargetUserInfos.getGroups()));

        this.currentGroup = g;
        refresh();
    }//GEN-LAST:event_jButtonAddGroupActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private com.jidesoft.swing.CheckBoxList checkBoxListGroups;
    private javax.swing.JButton jButtonAddGroup;
    private javax.swing.JButton jButtonDeleteGroup;
    private javax.swing.JButton jButtonEdit;
    private javax.swing.JButton jButtonRemoveAssociation;
    private javax.swing.JComboBox jComboBoxCategory;
    private javax.swing.JFormattedTextField jFieldGroupColor;
    private javax.swing.JFormattedTextField jFieldGroupName;
    private javax.swing.JFormattedTextField jFieldTargetName;
    private javax.swing.JLabel jLabelCategory;
    private javax.swing.JLabel jLabelColor;
    private javax.swing.JLabel jLabelDescription;
    private javax.swing.JLabel jLabelGroupName;
    private javax.swing.JLabel jLabelTargetGroups;
    private javax.swing.JLabel jLabelTargetName;
    private javax.swing.JPanel jPanelActions;
    private javax.swing.JPanel jPanelGroup;
    private javax.swing.JPanel jPanelTarget;
    private javax.swing.JScrollPane jScrollPaneDesc;
    private javax.swing.JScrollPane jScrollPaneGroups;
    private javax.swing.JScrollPane jScrollPaneTreeGroups;
    private javax.swing.JScrollPane jScrollPaneTreeTargets;
    private javax.swing.JTextArea jTextAreaGroupDesc;
    private javax.swing.JTree jTreeGroups;
    private javax.swing.JTree jTreeTargets;
    // End of variables declaration//GEN-END:variables

    private DefaultFormatter createFieldNameFormatter() {
        final DefaultFormatter df = new DefaultFormatter() {
            /** default serial UID for Serializable interface */
            private static final long serialVersionUID = 1;

            private final ParseException INVALID = new ParseException("Invalid", -1);

            /**
             * Hack to discard empty string or already-used names
             */
            @Override
            public Object stringToValue(final String text) throws ParseException {
                if (StringUtils.isEmpty(text)) {
                    throw INVALID;
                }
                // Check if the name is not already in use ?
                final TargetGroup match = editTargetUserInfos.getGroupByName(text);

                if (match != null && match != currentGroup) {
                    throw INVALID;
                }
                return super.stringToValue(text);
            }
        };
        df.setValueClass(String.class);
        df.setCommitsOnValidEdit(false);
        return df;
    }

    private DefaultFormatter createFieldColorFormatter() {
        final DefaultFormatter df = new DefaultFormatter() {
            /** default serial UID for Serializable interface */
            private static final long serialVersionUID = 1;

            private static final String HEX_PATTERN = "^#[A-Fa-f0-9]{6}$";

            private final ParseException INVALID = new ParseException("Invalid", -1);

            private final Pattern pattern = Pattern.compile(HEX_PATTERN);

            /**
             * Hack to discard empty string or invalid color
             */
            @Override
            public Object stringToValue(final String text) throws ParseException {
                if (StringUtils.isEmpty(text)) {
                    throw INVALID;
                }
                if (!pattern.matcher(text).matches()) {
                    throw INVALID;
                }
                return super.stringToValue(text);
            }
        };
        df.setValueClass(String.class);
        df.setCommitsOnValidEdit(false);
        return df;
    }

    private CheckBoxList createCheckBoxList() {
        final CheckBoxList list = new CheckBoxList() {
            /** default serial UID for Serializable interface */
            private static final long serialVersionUID = 1L;

            @Override
            public boolean isCheckBoxEnabled(final int index) {
                return isCheckBoxListGroupsEnabled(index);
            }
        };
        list.setClickInCheckBoxOnly(false);
        return list;
    }

    final static class ColorEditor implements ActionListener {

        private final JColorChooser colorChooser;
        private final JDialog dialog;

        private Color result = null;

        ColorEditor(final JButton jButtonEdit) {
            //Set up the dialog that the button brings up.
            colorChooser = new JColorChooser();
            dialog = JColorChooser.createDialog(jButtonEdit,
                    "Pick a Color", true, // Modal Window
                    colorChooser, this, // OK button handler
                    null); // No CANCEL button handler
        }

        public Color show(final Color initial) {
            if (initial != null) {
                colorChooser.setColor(initial);
            }
            result = null;

            // Show dialog and waits until dialog is not visible or disposed:
            dialog.setVisible(true);

            return result;
        }

        /**
         * Handles events from the dialog's OK button.
         */
        @Override
        public void actionPerformed(final ActionEvent e) {
            // User pressed dialog's "OK" button.
            result = colorChooser.getColor();
        }

    }

    /**
     * Check if the objects are different supporting null values
     * @param value1 string 1
     * @param value2 string 2
     * @return true only if objects are different
     */
    private static boolean isChanged(final Object value1, final Object value2) {
        return (value1 == null && value2 != null) || (value1 != null && value2 == null) || (value1 != null && value2 != null && !value1.equals(value2));
    }

    private boolean isNotNull(TargetGroup group) {
        return group != null && group != TargetGroup.EMPTY_GROUP;
    }
}
