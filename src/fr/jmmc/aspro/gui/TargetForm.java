/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.gui.util.CalibratorInfoTableModel;
import fr.jmmc.aspro.gui.util.TargetJTree;
import fr.jmmc.aspro.gui.util.TargetListRenderer;
import fr.jmmc.aspro.gui.util.TargetRenderer;
import fr.jmmc.aspro.gui.util.TargetTransferHandler;
import fr.jmmc.aspro.gui.util.TargetTreeCellRenderer;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetInformation;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.model.util.TargetRAComparator;
import fr.jmmc.jmcs.gui.component.GenericJTree;
import fr.jmmc.jmcs.gui.component.GenericListModel;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.network.BrowserLauncher;
import fr.jmmc.jmcs.resource.image.ResourceImage;
import fr.jmmc.jmcs.util.UrlUtils;
import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JFormattedTextField;
import javax.swing.JList;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.TransferHandler;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.text.NumberFormatter;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class represents the target information editor ...
 *
 * @author bourgesl
 */
public final class TargetForm extends javax.swing.JPanel implements PropertyChangeListener, TreeSelectionListener, ListSelectionListener {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(TargetForm.class.getName());
  /** Simbad URL (query by identifier) */
  private static final String SIMBAD_QUERY_ID = "http://simbad.u-strasbg.fr/simbad/sim-id?Ident=";
  /** custom number field formatter */
  private static NumberFormatter numberFieldFormatter = null;

  /* members */
  /** list of edited targets (clone) */
  private final List<Target> editTargets;
  /** edited target user informations (clone) */
  private final TargetUserInformations editTargetUserInfos;
  /** cached mapping between target and Target Information */
  private final Map<String, TargetInformation> mapIDTargetInformations = new HashMap<String, TargetInformation>();
  /** current edited target */
  private Target currentTarget = null;
  /** flag to enable / disable the automatic update of the target when any swing component changes */
  private boolean doAutoUpdateTarget = true;
  /* Swing */
  /** JList model containing calibrators */
  private GenericListModel<Target> calibratorsModel;

  /**
   * Creates new form TargetForm (used by NetBeans editor only)
   */
  public TargetForm() {
    this(null, null);
  }

  /**
   * Creates new form TargetForm
   * @param targets list of targets to edit
   * @param targetUserInfos target user informations
   */
  public TargetForm(final List<Target> targets, final TargetUserInformations targetUserInfos) {
    super();

    this.editTargets = targets;
    this.editTargetUserInfos = targetUserInfos;

    initComponents();

    postInit();
  }

  /**
   * This method is useful to set the specific features of initialized swing components.
   */
  private void postInit() {

    final TargetRenderer renderer = new TargetRenderer(this.editTargetUserInfos);

    // tree selection listener :
    this.jTreeTargets.addTreeSelectionListener(this);
    this.jTreeTargets.setCellRenderer(new TargetTreeCellRenderer(renderer));

    // list selection listener :
    this.jListCalibrators.getSelectionModel().addListSelectionListener(this);
    this.jListCalibrators.setCellRenderer(new TargetListRenderer(renderer));

    // add property change listener to editable fields :

    // radial velocity :
    this.jFieldSysVel.addPropertyChangeListener("value", this);

    // proper motion :
    this.jFieldPMRA.addPropertyChangeListener("value", this);
    this.jFieldPMDEC.addPropertyChangeListener("value", this);

    // parallax :
    this.jFieldParallax.addPropertyChangeListener("value", this);
    this.jFieldParaErr.addPropertyChangeListener("value", this);

    // Fluxes :
    this.jFieldMagV.addPropertyChangeListener("value", this);
    this.jFieldMagI.addPropertyChangeListener("value", this);
    this.jFieldMagJ.addPropertyChangeListener("value", this);
    this.jFieldMagH.addPropertyChangeListener("value", this);
    this.jFieldMagK.addPropertyChangeListener("value", this);
    this.jFieldMagN.addPropertyChangeListener("value", this);

    // add document listener to target description :
    this.jTextAreaTargetInfos.getDocument().addDocumentListener(new DocumentListener() {
      @Override
      public void insertUpdate(final DocumentEvent e) {
        targetInfosChanged();
      }

      @Override
      public void removeUpdate(final DocumentEvent e) {
        targetInfosChanged();
      }

      @Override
      public void changedUpdate(DocumentEvent e) {
        //Plain text components do not fire these events
      }
    });

    // Add custom DnD support :
    final TransferHandler targetTransferHandler = new TargetTransferHandler(this.editTargets, this.editTargetUserInfos);
    this.jListCalibrators.setTransferHandler(targetTransferHandler);
    this.jTreeTargets.setTransferHandler(targetTransferHandler);
  }

  /**
   * Initialize the internal model (tree) from the given list of targets
   * @param targetName target name to select
   */
  void initialize(final String targetName) {
    this.calibratorsModel = new GenericListModel<Target>(this.editTargetUserInfos.getCalibrators());
    this.jListCalibrators.setModel(this.calibratorsModel);

    this.generateTree();
    this.selectTarget(Target.getTarget(targetName, this.editTargets));
  }

  /* Tree related methods */
  /**
   * Return the custom TargetJTree
   * @return TargetJTree
   */
  private TargetJTree getTreeTargets() {
    return (TargetJTree) this.jTreeTargets;
  }

  /**
   * Generate the tree from the current edited list of targets
   */
  private void generateTree() {

    final DefaultMutableTreeNode rootNode = this.getTreeTargets().getRootNode();

    rootNode.removeAllChildren();

    TargetInformation targetInfo;
    DefaultMutableTreeNode targetNode;
    for (Target target : this.editTargets) {

      // display only science targets :
      if (!isCalibrator(target)) {
        targetNode = this.getTreeTargets().addNode(rootNode, target);

        // add calibrators as children of the target Node :
        targetInfo = getTargetUserInformation(target);
        for (Target calibrator : targetInfo.getCalibrators()) {
          this.getTreeTargets().addNode(targetNode, calibrator);
        }
      }
    }

    // fire node structure changed :
    this.getTreeTargets().fireNodeChanged(rootNode);
  }

  /**
   * Select the target int the target tree or in the calibrator list for the given target
   * @param target to select
   */
  void selectTarget(final Target target) {
    if (isCalibrator(target)) {
      this.jListCalibrators.setSelectedValue(target, true);
    } else {
      this.getTreeTargets().selectTarget(target);
    }
  }

  /**
   * Process the tree selection events
   * @param e tree selection event
   */
  @Override
  public void valueChanged(final TreeSelectionEvent e) {
    final DefaultMutableTreeNode currentNode = this.getTreeTargets().getLastSelectedNode();

    if (currentNode != null) {
      // Use invokeLater to selection change issues with editors :
      SwingUtils.invokeLaterEDT(new Runnable() {
        /**
         * Update tree selection
         */
        @Override
        public void run() {
          // Check if it is the root node :
          final DefaultMutableTreeNode rootNode = getTreeTargets().getRootNode();
          if (currentNode == rootNode) {
            getTreeTargets().selectFirstChildNode(rootNode);
            return;
          }

          /* retrieve the node that was selected */
          final Object userObject = currentNode.getUserObject();

          if (userObject instanceof Target) {

            // enable / disable before/after buttons:
            jButtonBefore.setEnabled(currentNode.getPreviousSibling() != null);
            jButtonAfter.setEnabled(currentNode.getNextSibling() != null);

            final Target target = (Target) userObject;

            logger.debug("tree selection: {}", target);

            final boolean isCalibrator = isCalibrator(target);

            // select the calibrator in the calibrator list :
            if (isCalibrator) {
              if (jListCalibrators.getSelectedValue() != target) {
                jListCalibrators.setSelectedValue(target, true);
              }
            } else {
              jListCalibrators.clearSelection();
            }

            processTargetSelection(target);

            // enable/disable the 'remove calibrator' action :
            jButtonRemoveCalibrator.setEnabled(isCalibrator);
          }
        }
      });
    }
  }

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

    // Use invokeLater to selection change issues with editors :
    SwingUtils.invokeLaterEDT(new Runnable() {
      /**
       * Update tree selection
       */
      @Override
      public void run() {

        // Single selection mode :
        final int minIndex = lsm.getMinSelectionIndex();

        if (minIndex != -1) {
          final Target target = calibratorsModel.get(minIndex);

          boolean found = false;

          // Check if the selected tree node :
          final DefaultMutableTreeNode currentNode = getTreeTargets().getLastSelectedNode();

          if (currentNode != null && currentNode.getUserObject() == target) {
            found = true;
          } else {
            // select the first target node having this calibrator :
            final DefaultMutableTreeNode targetNode = getTreeTargets().findTreeNode(target);

            if (targetNode != null) {
              found = true;
              // Select the target node that will send later a TreeSelectionEvent :
              getTreeTargets().selectPath(new TreePath(targetNode.getPath()));
            }
          }

          if (!found) {
            // No tree node found for the calibrator :

            // disable before / after buttons:
            jButtonBefore.setEnabled(false);
            jButtonAfter.setEnabled(false);

            // disable the 'remove calibrator' action :
            jButtonRemoveCalibrator.setEnabled(false);

            // clear tree selection :
            getTreeTargets().clearSelection();

            processTargetSelection(target);
          }
        }
      }
    });
  }

  /**
   * Update the UI when a target is selected in the target tree
   * @param target selected target
   */
  private void processTargetSelection(final Target target) {

    // update the current target :
    this.currentTarget = target;

    // disable the automatic update target :
    final boolean prevAutoUpdateTarget = this.setAutoUpdateTarget(false);
    try {

      // note : setText() / setValue() methods fire a property change event :

      // name :
      this.jFieldName.setText(target.getName());
      // RA / DEC :
      this.jFieldRA.setText(target.getRA());
      this.jFieldDEC.setText(target.getDEC());

      // radial velocity :
      this.jFieldSysVel.setValue(target.getSYSVEL());

      // proper motion :
      this.jFieldPMRA.setValue(target.getPMRA());
      this.jFieldPMDEC.setValue(target.getPMDEC());

      // parallax :
      this.jFieldParallax.setValue(target.getPARALLAX());
      this.jFieldParaErr.setValue(target.getPARAERR());

      // Fluxes :
      this.jFieldMagV.setValue(target.getFLUXV());
      this.jFieldMagI.setValue(target.getFLUXI());
      this.jFieldMagJ.setValue(target.getFLUXJ());
      this.jFieldMagH.setValue(target.getFLUXH());
      this.jFieldMagK.setValue(target.getFLUXK());
      this.jFieldMagN.setValue(target.getFLUXN());

      // spectral type :
      this.jFieldSpecType.setText(target.getSPECTYP());
      // object types :
      this.jFieldObjTypes.setText(target.getOBJTYP());
      // identifiers :
      this.jTextAreaIds.setText(target.getIDS());
      this.jTextAreaIds.setCaretPosition(0);

      // user description :
      this.jTextAreaTargetInfos.setText(getTargetUserInformation(target).getDescription());

      // update calibrator flag :
      final boolean calibrator = isCalibrator(target);
      this.jToggleButtonCalibrator.setSelected(calibrator);

      final boolean useTableCalibratorInfos = calibrator && target.getCalibratorInfos() != null;
      if (useTableCalibratorInfos) {
        getCalibratorInfoTableModel().setData(target.getCalibratorInfos());
      }
      // only display calibrator informations if available :
      this.jLabelCalibratorInfos.setVisible(useTableCalibratorInfos);
      this.jScrollPaneCalibratorInfos.setVisible(useTableCalibratorInfos);

    } finally {
      // restore the automatic update target :
      this.setAutoUpdateTarget(prevAutoUpdateTarget);
    }
  }

  /**
   * Process the change event for any number field.
   * Validates the new input (check valid range) and update the associated target
   * @param evt property change event
   */
  @Override
  public void propertyChange(final PropertyChangeEvent evt) {
    // check if the automatic update flag is enabled :
    if (this.doAutoUpdateTarget) {

      final JFormattedTextField field = (JFormattedTextField) evt.getSource();
      final Double oldValue = (Double) evt.getOldValue();
      Double value = (Double) evt.getNewValue();

      // check if value changed (null supported) :
      if (!isChanged(value, oldValue)) {
        return;
      }

      if (logger.isDebugEnabled()) {
        logger.debug("field {} new: {} old: {}", field.getName(), value, oldValue);
      }

      if (value != null) {

        // check the new value :
        final double val = value.doubleValue();

        if (field == this.jFieldParaErr) {
          // check if error is negative :
          if (val < 0d) {
            if (logger.isDebugEnabled()) {
              logger.debug("Parallax Error negative: {}", val);
            }

            field.setValue(oldValue);
            return;
          }
        } else if (field.getName().startsWith("FLUX")) {
          // check if magnitudes are in range [-30;100]
          if (val < -30d || val > 100d) {
            if (logger.isDebugEnabled()) {
              logger.debug("Magnitude {} invalid : {}", field.getName(), val);
            }

            field.setValue(oldValue);
            return;
          }
        }
      }

      // update the target :

      // note : we could use introspection to avoid such if/else cascade ...
      if (field == this.jFieldSysVel) {
        this.currentTarget.setSYSVEL(value);
      } else if (field == this.jFieldPMRA) {
        this.currentTarget.setPMRA(value);
      } else if (field == this.jFieldPMDEC) {
        this.currentTarget.setPMDEC(value);
      } else if (field == this.jFieldParallax) {
        this.currentTarget.setPARALLAX(value);
      } else if (field == this.jFieldParaErr) {
        this.currentTarget.setPARAERR(value);
      } else if (field == this.jFieldMagV) {
        this.currentTarget.setFLUXV(value);
      } else if (field == this.jFieldMagI) {
        this.currentTarget.setFLUXI(value);
      } else if (field == this.jFieldMagJ) {
        this.currentTarget.setFLUXJ(value);
      } else if (field == this.jFieldMagH) {
        this.currentTarget.setFLUXH(value);
      } else if (field == this.jFieldMagK) {
        this.currentTarget.setFLUXK(value);
      } else if (field == this.jFieldMagN) {
        this.currentTarget.setFLUXN(value);
      } else {
        logger.warn("unsupported field: {}", field);
      }
    }
  }

  /**
   * Process the document change event for the target user information
   */
  private void targetInfosChanged() {
    // check if the automatic update flag is enabled :
    if (this.doAutoUpdateTarget) {

      final String text = this.jTextAreaTargetInfos.getText();

      logger.debug("user infos: {}", text);

      getTargetUserInformation(this.currentTarget).setDescription((text.length() > 0) ? text : null);
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

    jPanelLeft = new javax.swing.JPanel();
    jScrollPaneTreeTargets = new javax.swing.JScrollPane();
    jTreeTargets = new TargetJTree(this.editTargetUserInfos);
    jPanelCalibrators = new javax.swing.JPanel();
    jScrollPaneCalibrators = new javax.swing.JScrollPane();
    jListCalibrators = new javax.swing.JList();
    jToolBarActions = new javax.swing.JToolBar();
    jButtonBefore = new javax.swing.JButton();
    jButtonAfter = new javax.swing.JButton();
    jSeparator1 = new javax.swing.JToolBar.Separator();
    jButtonSortRA = new javax.swing.JButton();
    jSeparator2 = new javax.swing.JToolBar.Separator();
    jToggleButtonCalibrator = new javax.swing.JToggleButton();
    jButtonRemoveCalibrator = new javax.swing.JButton();
    jPanelTarget = new javax.swing.JPanel();
    jLabelName = new javax.swing.JLabel();
    jFieldName = new javax.swing.JTextField();
    jLabelRA = new javax.swing.JLabel();
    jFieldRA = new javax.swing.JTextField();
    jLabelDEC = new javax.swing.JLabel();
    jFieldDEC = new javax.swing.JTextField();
    jLabelSysVel = new javax.swing.JLabel();
    jFieldSysVel = new JFormattedTextField(getNumberFieldFormatter());
    jSeparator3 = new javax.swing.JSeparator();
    jLabelPMRA = new javax.swing.JLabel();
    jFieldPMRA = new JFormattedTextField(getNumberFieldFormatter())
    ;
    jLabelRMDEC = new javax.swing.JLabel();
    jFieldPMDEC = new JFormattedTextField(getNumberFieldFormatter());
    jSeparator4 = new javax.swing.JSeparator();
    jLabelMag = new javax.swing.JLabel();
    jLabelMagV = new javax.swing.JLabel();
    jFieldMagV = new JFormattedTextField(getNumberFieldFormatter());
    jLabelMagI = new javax.swing.JLabel();
    jFieldMagI = new JFormattedTextField(getNumberFieldFormatter());
    jLabelMagJ = new javax.swing.JLabel();
    jFieldMagJ = new JFormattedTextField(getNumberFieldFormatter());
    jLabelMagH = new javax.swing.JLabel();
    jFieldMagH = new JFormattedTextField(getNumberFieldFormatter());
    jLabelMagK = new javax.swing.JLabel();
    jFieldMagK = new JFormattedTextField(getNumberFieldFormatter());
    jLabelMagN = new javax.swing.JLabel();
    jFieldMagN = new JFormattedTextField(getNumberFieldFormatter());
    jSeparator5 = new javax.swing.JSeparator();
    jLabelObjTypes = new javax.swing.JLabel();
    jLabelSpecTypes = new javax.swing.JLabel();
    jFieldSpecType = new javax.swing.JTextField();
    jFieldObjTypes = new javax.swing.JTextField();
    jLabelParallax = new javax.swing.JLabel();
    jFieldParallax = new JFormattedTextField(getNumberFieldFormatter());
    jLabelParaErr = new javax.swing.JLabel();
    jFieldParaErr = new JFormattedTextField(getNumberFieldFormatter());
    jLabelIds = new javax.swing.JLabel();
    jScrollPaneIds = new javax.swing.JScrollPane();
    jTextAreaIds = new javax.swing.JTextArea();
    jButtonSimbad = new javax.swing.JButton();
    jLabelCalibratorInfos = new javax.swing.JLabel();
    jScrollPaneCalibratorInfos = new javax.swing.JScrollPane();
    jTableCalibratorInfos = new javax.swing.JTable();
    jPanelDescription = new javax.swing.JPanel();
    jScrollPaneTargetInfos = new javax.swing.JScrollPane();
    jTextAreaTargetInfos = new javax.swing.JTextArea();

    setLayout(new java.awt.GridBagLayout());

    jPanelLeft.setLayout(new java.awt.GridBagLayout());

    jScrollPaneTreeTargets.setMinimumSize(new java.awt.Dimension(80, 100));
    jScrollPaneTreeTargets.setPreferredSize(new java.awt.Dimension(130, 100));

    jTreeTargets.setFont(new java.awt.Font("Dialog", 1, 12)); // NOI18N
    javax.swing.tree.DefaultMutableTreeNode treeNode1 = new javax.swing.tree.DefaultMutableTreeNode("Targets");
    jTreeTargets.setModel(new javax.swing.tree.DefaultTreeModel(treeNode1));
    jTreeTargets.setDragEnabled(true);
    jScrollPaneTreeTargets.setViewportView(jTreeTargets);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.gridheight = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.weighty = 0.7;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelLeft.add(jScrollPaneTreeTargets, gridBagConstraints);

    jPanelCalibrators.setBorder(javax.swing.BorderFactory.createTitledBorder("Calibrators"));
    jPanelCalibrators.setLayout(new java.awt.BorderLayout());

    jListCalibrators.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
    jListCalibrators.setToolTipText("this list contains targets considered as calibrators");
    jListCalibrators.setCellRenderer(createTargetListCellRenderer());
    jListCalibrators.setDragEnabled(true);
    jListCalibrators.setFixedCellWidth(100);
    jListCalibrators.setVisibleRowCount(3);
    jScrollPaneCalibrators.setViewportView(jListCalibrators);

    jPanelCalibrators.add(jScrollPaneCalibrators, java.awt.BorderLayout.CENTER);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weighty = 0.3;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelLeft.add(jPanelCalibrators, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridheight = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.4;
    gridBagConstraints.weighty = 1.0;
    add(jPanelLeft, gridBagConstraints);

    jToolBarActions.setFloatable(false);
    jToolBarActions.setRollover(true);

    jButtonBefore.setIcon(ResourceImage.UP_ARROW.icon());
    jButtonBefore.setToolTipText("");
    jButtonBefore.setFocusable(false);
    jButtonBefore.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    jButtonBefore.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    jButtonBefore.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonBeforeActionPerformed(evt);
      }
    });
    jToolBarActions.add(jButtonBefore);

    jButtonAfter.setIcon(ResourceImage.DOWN_ARROW.icon());
    jButtonAfter.setFocusable(false);
    jButtonAfter.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    jButtonAfter.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    jButtonAfter.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonAfterActionPerformed(evt);
      }
    });
    jToolBarActions.add(jButtonAfter);
    jToolBarActions.add(jSeparator1);

    jButtonSortRA.setText("Sort by R.A.");
    jButtonSortRA.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonSortRAActionPerformed(evt);
      }
    });
    jToolBarActions.add(jButtonSortRA);
    jToolBarActions.add(jSeparator2);

    jToggleButtonCalibrator.setText("Flag Target as Calibrator");
    jToggleButtonCalibrator.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jToggleButtonCalibratorActionPerformed(evt);
      }
    });
    jToolBarActions.add(jToggleButtonCalibrator);

    jButtonRemoveCalibrator.setText("Remove Calibrator");
    jButtonRemoveCalibrator.setFocusable(false);
    jButtonRemoveCalibrator.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    jButtonRemoveCalibrator.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    jButtonRemoveCalibrator.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonRemoveCalibratorActionPerformed(evt);
      }
    });
    jToolBarActions.add(jButtonRemoveCalibrator);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    add(jToolBarActions, gridBagConstraints);

    jPanelTarget.setBorder(javax.swing.BorderFactory.createTitledBorder("Target"));
    jPanelTarget.setLayout(new java.awt.GridBagLayout());

    jLabelName.setText("Name");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelName, gridBagConstraints);

    jFieldName.setColumns(5);
    jFieldName.setEditable(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldName, gridBagConstraints);

    jLabelRA.setText("RA [HMS]");
    jLabelRA.setToolTipText("RA coordinate (J2000) (HMS)");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelRA, gridBagConstraints);

    jFieldRA.setColumns(5);
    jFieldRA.setEditable(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldRA, gridBagConstraints);

    jLabelDEC.setText("DEC [DMS]");
    jLabelDEC.setToolTipText("DEC coordinate (J2000) (DMS)");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelDEC, gridBagConstraints);

    jFieldDEC.setColumns(5);
    jFieldDEC.setEditable(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldDEC, gridBagConstraints);

    jLabelSysVel.setText("Radial Velocity");
    jLabelSysVel.setToolTipText("radial velocity in km/s");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 9;
    gridBagConstraints.gridwidth = 3;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelSysVel, gridBagConstraints);

    jFieldSysVel.setColumns(5);
    jFieldSysVel.setName("SYSVEL"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 9;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldSysVel, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 3;
    gridBagConstraints.gridwidth = 4;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.weighty = 0.05;
    jPanelTarget.add(jSeparator3, gridBagConstraints);

    jLabelPMRA.setText("PMRA");
    jLabelPMRA.setToolTipText("proper motion in RA (mas/yr)");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelPMRA, gridBagConstraints);

    jFieldPMRA.setColumns(5);
    jFieldPMRA.setName("PMRA"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.weightx = 0.5;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldPMRA, gridBagConstraints);

    jLabelRMDEC.setText("PMDEC");
    jLabelRMDEC.setToolTipText("proper motion in DEC (mas/yr)");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelRMDEC, gridBagConstraints);

    jFieldPMDEC.setColumns(5);
    jFieldPMDEC.setName("PMDEC"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.weightx = 0.5;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldPMDEC, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 8;
    gridBagConstraints.gridwidth = 4;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.weighty = 0.05;
    jPanelTarget.add(jSeparator4, gridBagConstraints);

    jLabelMag.setText("Magnitudes :");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 4;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
    jPanelTarget.add(jLabelMag, gridBagConstraints);

    jLabelMagV.setText("V");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 5;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelMagV, gridBagConstraints);

    jFieldMagV.setColumns(5);
    jFieldMagV.setName("FLUXV"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 5;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldMagV, gridBagConstraints);

    jLabelMagI.setText("I");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 5;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelMagI, gridBagConstraints);

    jFieldMagI.setColumns(5);
    jFieldMagI.setName("FLUXI"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 5;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldMagI, gridBagConstraints);

    jLabelMagJ.setText("J");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 6;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelMagJ, gridBagConstraints);

    jFieldMagJ.setColumns(5);
    jFieldMagJ.setName("FLUXJ"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 6;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldMagJ, gridBagConstraints);

    jLabelMagH.setText("H");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 6;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelMagH, gridBagConstraints);

    jFieldMagH.setColumns(5);
    jFieldMagH.setName("FLUXH"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 6;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldMagH, gridBagConstraints);

    jLabelMagK.setText("K");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 7;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelMagK, gridBagConstraints);

    jFieldMagK.setColumns(5);
    jFieldMagK.setName("FLUXK"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 7;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldMagK, gridBagConstraints);

    jLabelMagN.setText("N");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 7;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelMagN, gridBagConstraints);

    jFieldMagN.setColumns(5);
    jFieldMagN.setName("FLUXN"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 7;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldMagN, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 11;
    gridBagConstraints.gridwidth = 4;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.weighty = 0.05;
    jPanelTarget.add(jSeparator5, gridBagConstraints);

    jLabelObjTypes.setText("Object types");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 13;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelObjTypes, gridBagConstraints);

    jLabelSpecTypes.setText("Spectral type");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 12;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelSpecTypes, gridBagConstraints);

    jFieldSpecType.setColumns(10);
    jFieldSpecType.setEditable(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 12;
    gridBagConstraints.gridwidth = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldSpecType, gridBagConstraints);

    jFieldObjTypes.setColumns(10);
    jFieldObjTypes.setEditable(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 13;
    gridBagConstraints.gridwidth = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldObjTypes, gridBagConstraints);

    jLabelParallax.setText("Parallax");
    jLabelParallax.setToolTipText("parallax in mas");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 10;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelParallax, gridBagConstraints);

    jFieldParallax.setColumns(5);
    jFieldParallax.setName("PARALLAX"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 10;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldParallax, gridBagConstraints);

    jLabelParaErr.setText("Error");
    jLabelParaErr.setToolTipText("Error in parallax (mas/yr)");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 10;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelParaErr, gridBagConstraints);

    jFieldParaErr.setColumns(5);
    jFieldParaErr.setName("PARA_ERR"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 10;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldParaErr, gridBagConstraints);

    jLabelIds.setText("Identifiers");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 14;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelIds, gridBagConstraints);

    jTextAreaIds.setColumns(20);
    jTextAreaIds.setEditable(false);
    jTextAreaIds.setLineWrap(true);
    jTextAreaIds.setRows(1);
    jTextAreaIds.setTabSize(2);
    jTextAreaIds.setWrapStyleWord(true);
    jScrollPaneIds.setViewportView(jTextAreaIds);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 14;
    gridBagConstraints.gridwidth = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.weighty = 0.1;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jScrollPaneIds, gridBagConstraints);

    jButtonSimbad.setText("Simbad");
    jButtonSimbad.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonSimbadActionPerformed(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jButtonSimbad, gridBagConstraints);

    jLabelCalibratorInfos.setText("<html>Calibrator<br>Information</html>");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 15;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelCalibratorInfos, gridBagConstraints);

    jScrollPaneCalibratorInfos.setPreferredSize(new java.awt.Dimension(50, 50));

    jTableCalibratorInfos.setModel(new CalibratorInfoTableModel());
    jTableCalibratorInfos.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_ALL_COLUMNS);
    jTableCalibratorInfos.setCellSelectionEnabled(true);
    jTableCalibratorInfos.setMinimumSize(new java.awt.Dimension(50, 50));
    jTableCalibratorInfos.getTableHeader().setReorderingAllowed(false);
    jScrollPaneCalibratorInfos.setViewportView(jTableCalibratorInfos);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 15;
    gridBagConstraints.gridwidth = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weighty = 0.2;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jScrollPaneCalibratorInfos, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.6;
    gridBagConstraints.weighty = 0.8;
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    add(jPanelTarget, gridBagConstraints);

    jPanelDescription.setBorder(javax.swing.BorderFactory.createTitledBorder("Target notes [\u2139]"));
    jPanelDescription.setMinimumSize(new java.awt.Dimension(10, 50));
    jPanelDescription.setPreferredSize(new java.awt.Dimension(100, 80));
    jPanelDescription.setLayout(new java.awt.GridBagLayout());

    jTextAreaTargetInfos.setBackground(new java.awt.Color(255, 255, 153));
    jTextAreaTargetInfos.setColumns(20);
    jTextAreaTargetInfos.setFont(new java.awt.Font("Monospaced", 0, 12)); // NOI18N
    jTextAreaTargetInfos.setRows(1);
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
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    add(jPanelDescription, gridBagConstraints);
  }// </editor-fold>//GEN-END:initComponents

  private void jButtonSimbadActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonSimbadActionPerformed
    final String url = SIMBAD_QUERY_ID + UrlUtils.encode(this.currentTarget.getName());

    logger.debug("Simbad url = {}", url);

    BrowserLauncher.openURL(url);
  }//GEN-LAST:event_jButtonSimbadActionPerformed

  private void jToggleButtonCalibratorActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jToggleButtonCalibratorActionPerformed
    final boolean isCalibrator = this.jToggleButtonCalibrator.isSelected();

    if (logger.isDebugEnabled()) {
      logger.debug("isCalibrator = {} for {}", isCalibrator, this.currentTarget.getName());
    }

    if (isCalibrator) {

      // check that the target has no calibrator yet :
      if (this.editTargetUserInfos.hasCalibrators(this.currentTarget)) {

        MessagePane.showErrorMessage(
                "There are already calibrators associated to this target [" + this.currentTarget.getName() + "] !");

        this.jToggleButtonCalibrator.setSelected(false);
        return;
      }

      // note : should be done directly by our data model classes (editTargetUserInfos) :
      if (!this.calibratorsModel.contains(this.currentTarget)) {
        this.calibratorsModel.add(this.currentTarget);

        // Remove the calibrator node in the target tree :
        final DefaultMutableTreeNode targetNode = this.getTreeTargets().findTreeNode(this.currentTarget);

        if (targetNode != null) {
          this.getTreeTargets().removeNodeAndRefresh(this.getTreeTargets().getParentNode(targetNode), targetNode, false);
        }

        // select the calibrator :
        this.jListCalibrators.setSelectedValue(this.currentTarget, true);
      }

    } else {

      // determine if the calibrator is really used in a target :
      final DefaultMutableTreeNode calibratorNode = getTreeTargets().findTreeNode(this.currentTarget);

      if (calibratorNode == null || MessagePane.showConfirmMessage(this.jToggleButtonCalibrator,
              "Do you really want to remove associations with this calibrator [" + this.currentTarget.getName() + "] ?")) {

        // remove the occurences of the calibrator and update the tree ...
        removeCalibrator(this.currentTarget);

        this.calibratorsModel.remove(this.currentTarget);

        // Restore the calibrator node in the target tree :
        final DefaultMutableTreeNode rootNode = this.getTreeTargets().getRootNode();

        int nScienceTargets = 0;

        for (Target target : this.editTargets) {
          if (!isCalibrator(target)) {
            if (target == this.currentTarget) {
              // create node :
              final DefaultMutableTreeNode targetNode = new DefaultMutableTreeNode(target);

              rootNode.insert(targetNode, nScienceTargets);

              // fire node structure changed :
              this.getTreeTargets().fireNodeChanged(rootNode);

              this.getTreeTargets().selectPath(new TreePath(targetNode.getPath()));

              break;
            }
            nScienceTargets++;
          }
        }

      } else {
        this.jToggleButtonCalibrator.setSelected(true);
      }
    }
  }//GEN-LAST:event_jToggleButtonCalibratorActionPerformed

  /**
   * Remove all occurences of the calibrator from the tree
   * @param calibrator calibrator target
   */
  private void removeCalibrator(final Target calibrator) {

    final DefaultMutableTreeNode rootNode = this.getTreeTargets().getRootNode();

    final int size = rootNode.getChildCount();
    if (size > 0) {
      DefaultMutableTreeNode targetNode, calibratorNode;

      for (int i = 0; i < size; i++) {
        targetNode = (DefaultMutableTreeNode) rootNode.getChildAt(i);

        // Find the first occurence of the calibrator in the target calibrators :
        calibratorNode = GenericJTree.findTreeNode(targetNode, calibrator);

        if (calibratorNode != null) {
          // Remove calibrator from target :
          this.getTreeTargets().removeCalibrator(calibratorNode, calibrator,
                  targetNode, (Target) targetNode.getUserObject(), true);
        }
      }
    }
  }

  private void jButtonRemoveCalibratorActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonRemoveCalibratorActionPerformed

    final DefaultMutableTreeNode currentNode = this.getTreeTargets().getLastSelectedNode();

    if (currentNode != null) {

      /* retrieve the node that was selected */
      final Object userObject = currentNode.getUserObject();

      if (userObject instanceof Target) {
        final Target target = (Target) userObject;

        logger.debug("tree selection: {}", target);

        // remove the current calibrator from its science target :
        if (isCalibrator(target)) {

          if (logger.isDebugEnabled()) {
            logger.debug("remove calibrator: {}", target.getName());
          }

          // Parent can be a target or null :
          final DefaultMutableTreeNode parentNode = this.getTreeTargets().getParentNode(currentNode);

          if (parentNode != null && parentNode.getUserObject() instanceof Target) {
            final Target parentTarget = (Target) parentNode.getUserObject();

            // Remove calibrator from target :
            this.getTreeTargets().removeCalibrator(currentNode, target,
                    parentNode, parentTarget, true);
          }
        }
      }
    }
  }//GEN-LAST:event_jButtonRemoveCalibratorActionPerformed

  private void jButtonSortRAActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonSortRAActionPerformed
    // sort edited targets :
    sortTargets(this.editTargets);

    // sort calibrator list :
    sortTargets(this.editTargetUserInfos.getCalibrators());

    TargetInformation targetInfo;
    for (Target target : this.editTargets) {

      // sort also every calibrator list of science targets :
      if (!isCalibrator(target)) {
        targetInfo = getTargetUserInformation(target);

        if (!targetInfo.getCalibrators().isEmpty()) {
          sortTargets(targetInfo.getCalibrators());
        }
      }
    }

    // Refresh the complete form :
    this.initialize(getCurrentTarget().getName());
  }//GEN-LAST:event_jButtonSortRAActionPerformed

  private void jButtonAfterActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonAfterActionPerformed
    final DefaultMutableTreeNode currentNode = this.getTreeTargets().getLastSelectedNode();
    final DefaultMutableTreeNode nextNode = currentNode.getNextSibling();

    if (nextNode != null) {
      Object userObject = currentNode.getUserObject();

      if (userObject instanceof Target) {
        final Target target = (Target) userObject;

        logger.debug("tree selection: {}", target);

        userObject = nextNode.getUserObject();

        if (userObject instanceof Target) {
          final Target refTarget = (Target) userObject;

          logger.debug("tree selection: {}", refTarget);

          // both are calibrators or not (same level in the tree):
          final boolean isCalibrator = isCalibrator(target);

          if (isCalibrator) {
            // Parent can be a target or null :
            final DefaultMutableTreeNode parentNode = this.getTreeTargets().getParentNode(currentNode);

            if (parentNode != null && parentNode.getUserObject() instanceof Target) {
              final Target parentTarget = (Target) parentNode.getUserObject();

              moveTarget(target, refTarget, parentTarget, true);
            }
          } else {
            moveTarget(target, refTarget, null, true);
          }
        }
      }
    }
  }//GEN-LAST:event_jButtonAfterActionPerformed

  private void jButtonBeforeActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonBeforeActionPerformed
    final DefaultMutableTreeNode currentNode = this.getTreeTargets().getLastSelectedNode();
    final DefaultMutableTreeNode prevNode = currentNode.getPreviousSibling();

    if (prevNode != null) {
      Object userObject = currentNode.getUserObject();

      if (userObject instanceof Target) {
        final Target target = (Target) userObject;

        logger.debug("tree selection: {}", target);

        userObject = prevNode.getUserObject();

        if (userObject instanceof Target) {
          final Target refTarget = (Target) userObject;

          logger.debug("tree selection: {}", refTarget);

          // both are calibrators or not (same level in the tree):
          final boolean isCalibrator = isCalibrator(target);

          if (isCalibrator) {
            // Parent can be a target or null :
            final DefaultMutableTreeNode parentNode = this.getTreeTargets().getParentNode(currentNode);

            if (parentNode != null && parentNode.getUserObject() instanceof Target) {
              final Target parentTarget = (Target) parentNode.getUserObject();

              moveTarget(target, refTarget, parentTarget, false);
            }
          } else {
            moveTarget(target, refTarget, null, false);
          }
        }
      }
    }
  }//GEN-LAST:event_jButtonBeforeActionPerformed

  /**
   * Move the given science or calibrator target before or after the given reference target
   * @param target target to move
   * @param refTarget reference target
   * @param parentTarget parent target for calibrators i.e. science target
   * @param after true to move the given target after the reference target; false before
   */
  private void moveTarget(final Target target, final Target refTarget, final Target parentTarget, final boolean after) {
    if (logger.isDebugEnabled()) {
      logger.debug("moveTarget - after: {}\ntarget: {}\nrefTarget: {}\nparentTarget: {}",
              after, target, refTarget, parentTarget);
    }

    final String name = target.getName();

    if (parentTarget != null) {
      // move target in the calibrator list of parent science target:

      final TargetInformation targetInfo = getTargetUserInformation(parentTarget);

      if (!targetInfo.getCalibrators().isEmpty()) {
        final List<Target> calibrators = targetInfo.getCalibrators();

        moveTarget(calibrators, target, refTarget, after);
      }
    } else {
      moveTarget(this.editTargets, target, refTarget, after);
    }

    // Refresh the complete form :
    this.initialize(name);
  }

  /**
   * Move the given target inside the given target list before or after the given reference target
   * @param targets target list to update
   * @param target target to move
   * @param refTarget reference target
   * @param after true to move the given target after the reference target; false before
   */
  private static void moveTarget(final List<Target> targets, final Target target, final Target refTarget, final boolean after) {
    // Remove target first:
    targets.remove(target);

    // Find ref target position:
    int refPos = -1;

    final String refName = refTarget.getName();

    int i = 0;
    for (Target t : targets) {
      if (t.getName().equals(refName)) {
        refPos = i;
        break;
      }
      i++;
    }

    if (refPos != -1) {
      targets.add(refPos + ((after) ? 1 : 0), target);
    } else {
      logger.info("reference target not found: {}", refTarget);

      // anyway: restore target:
      if (after) {
        targets.add(target);
      } else {
        targets.add(0, target);
      }
    }
  }

  /**
   * Sort the given target list by right ascension (R.A.)
   * @param targets target list to sort (modified)
   */
  private static void sortTargets(final List<Target> targets) {
    logger.debug("targets to sort: {}", targets);

    Collections.sort(targets, TargetRAComparator.getInstance());

    logger.debug("targets sorted: {}", targets);
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
   * Return the custom table model
   * @return CalibratorInfoTableModel
   */
  private CalibratorInfoTableModel getCalibratorInfoTableModel() {
    return (CalibratorInfoTableModel) this.jTableCalibratorInfos.getModel();
  }
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JButton jButtonAfter;
  private javax.swing.JButton jButtonBefore;
  private javax.swing.JButton jButtonRemoveCalibrator;
  private javax.swing.JButton jButtonSimbad;
  private javax.swing.JButton jButtonSortRA;
  private javax.swing.JTextField jFieldDEC;
  private javax.swing.JFormattedTextField jFieldMagH;
  private javax.swing.JFormattedTextField jFieldMagI;
  private javax.swing.JFormattedTextField jFieldMagJ;
  private javax.swing.JFormattedTextField jFieldMagK;
  private javax.swing.JFormattedTextField jFieldMagN;
  private javax.swing.JFormattedTextField jFieldMagV;
  private javax.swing.JTextField jFieldName;
  private javax.swing.JTextField jFieldObjTypes;
  private javax.swing.JFormattedTextField jFieldPMDEC;
  private javax.swing.JFormattedTextField jFieldPMRA;
  private javax.swing.JFormattedTextField jFieldParaErr;
  private javax.swing.JFormattedTextField jFieldParallax;
  private javax.swing.JTextField jFieldRA;
  private javax.swing.JTextField jFieldSpecType;
  private javax.swing.JFormattedTextField jFieldSysVel;
  private javax.swing.JLabel jLabelCalibratorInfos;
  private javax.swing.JLabel jLabelDEC;
  private javax.swing.JLabel jLabelIds;
  private javax.swing.JLabel jLabelMag;
  private javax.swing.JLabel jLabelMagH;
  private javax.swing.JLabel jLabelMagI;
  private javax.swing.JLabel jLabelMagJ;
  private javax.swing.JLabel jLabelMagK;
  private javax.swing.JLabel jLabelMagN;
  private javax.swing.JLabel jLabelMagV;
  private javax.swing.JLabel jLabelName;
  private javax.swing.JLabel jLabelObjTypes;
  private javax.swing.JLabel jLabelPMRA;
  private javax.swing.JLabel jLabelParaErr;
  private javax.swing.JLabel jLabelParallax;
  private javax.swing.JLabel jLabelRA;
  private javax.swing.JLabel jLabelRMDEC;
  private javax.swing.JLabel jLabelSpecTypes;
  private javax.swing.JLabel jLabelSysVel;
  private javax.swing.JList jListCalibrators;
  private javax.swing.JPanel jPanelCalibrators;
  private javax.swing.JPanel jPanelDescription;
  private javax.swing.JPanel jPanelLeft;
  private javax.swing.JPanel jPanelTarget;
  private javax.swing.JScrollPane jScrollPaneCalibratorInfos;
  private javax.swing.JScrollPane jScrollPaneCalibrators;
  private javax.swing.JScrollPane jScrollPaneIds;
  private javax.swing.JScrollPane jScrollPaneTargetInfos;
  private javax.swing.JScrollPane jScrollPaneTreeTargets;
  private javax.swing.JToolBar.Separator jSeparator1;
  private javax.swing.JToolBar.Separator jSeparator2;
  private javax.swing.JSeparator jSeparator3;
  private javax.swing.JSeparator jSeparator4;
  private javax.swing.JSeparator jSeparator5;
  private javax.swing.JTable jTableCalibratorInfos;
  private javax.swing.JTextArea jTextAreaIds;
  private javax.swing.JTextArea jTextAreaTargetInfos;
  private javax.swing.JToggleButton jToggleButtonCalibrator;
  private javax.swing.JToolBar jToolBarActions;
  private javax.swing.JTree jTreeTargets;
  // End of variables declaration//GEN-END:variables

  /**
   * Create a custom list renderer to display target name instead of target.toString()
   * @return custom list renderer
   */
  private static ListCellRenderer createTargetListCellRenderer() {
    return new DefaultListCellRenderer() {
      /** default serial UID for Serializable interface */
      private static final long serialVersionUID = 1;

      /**
       * Return a component that has been configured to display the specified
       * value. That component's <code>paint</code> method is then called to
       * "render" the cell.  If it is necessary to compute the dimensions
       * of a list because the list cells do not have a fixed size, this method
       * is called to generate a component on which <code>getPreferredSize</code>
       * can be invoked.
       *
       * @param list The JList we're painting.
       * @param value The value returned by list.getModel().getElementAt(index).
       * @param index The cells index.
       * @param isSelected True if the specified cell was selected.
       * @param cellHasFocus True if the specified cell has the focus.
       * @return A component whose paint() method will render the specified value.
       *
       * @see JList
       * @see ListSelectionModel
       * @see ListModel
       */
      @Override
      public Component getListCellRendererComponent(
              final JList list,
              final Object value,
              final int index,
              final boolean isSelected,
              final boolean cellHasFocus) {
        final String val;
        if (value == null) {
          val = null;
        } else {
          final Target target = (Target) value;
          val = target.getName();
        }
        return super.getListCellRendererComponent(list, val, index, isSelected, cellHasFocus);
      }
    };
  }

  /**
   * Return the custom double formatter that accepts null values
   * @return number formatter
   */
  private static NumberFormatter getNumberFieldFormatter() {
    if (numberFieldFormatter != null) {
      return numberFieldFormatter;
    }
    final NumberFormatter nf = new NumberFormatter(new DecimalFormat("####.####")) {
      /** default serial UID for Serializable interface */
      private static final long serialVersionUID = 1;

      /**
       * Hack to allow empty string
       */
      @Override
      public Object stringToValue(final String text) throws ParseException {
        if (text == null || text.length() == 0) {
          return null;
        }
        return super.stringToValue(text);
      }
    };
    nf.setValueClass(Double.class);
    nf.setCommitsOnValidEdit(false);

    numberFieldFormatter = nf;
    return nf;
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

  /**
   * Return true if the given target is a calibrator
   * i.e. the calibrator list contains the given target
   * @param target target to use
   * @return true if the given target is a calibrator
   */
  public boolean isCalibrator(final Target target) {
    return this.editTargetUserInfos.isCalibrator(target);
  }

  /**
   * Fast access to the target information associated to the given target (cached).
   * @param target target to use
   * @return target information
   */
  private TargetInformation getTargetUserInformation(final Target target) {

    TargetInformation targetInfo = this.mapIDTargetInformations.get(target.getIdentifier());

    if (targetInfo == null) {
      targetInfo = this.editTargetUserInfos.getOrCreateTargetInformation(target);
      this.mapIDTargetInformations.put(target.getIdentifier(), targetInfo);
    }

    return targetInfo;
  }
}
