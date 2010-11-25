/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: TargetForm.java,v 1.5 2010-11-25 17:55:26 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.4  2010/11/25 08:00:35  bourgesl
 * added open simbad action
 * updated data model
 *
 * Revision 1.3  2010/11/23 16:57:35  bourgesl
 * complete editor with optional fields (magnitudes ...) and user information
 * custom number formatter to allow null values in text fields
 *
 * Revision 1.2  2010/11/19 16:57:04  bourgesl
 * always open full editor with selected target
 * added target name, RA/DEC, magnitudes
 *
 * Revision 1.1  2010/11/18 17:20:33  bourgesl
 * initial GUI for target editor
 *
 */
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.mcs.gui.BrowserLauncher;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.List;
import java.util.logging.Level;
import javax.swing.JFormattedTextField;
import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.text.NumberFormatter;
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
public final class TargetForm extends javax.swing.JPanel implements PropertyChangeListener, TreeSelectionListener {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.TargetForm";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** SimBad URL (query by identifier) */
  private static final String SIMBAD_QUERY_ID = "http://simbad.u-strasbg.fr/simbad/sim-id?Ident=";
  /** custom number field formatter */
  private static NumberFormatter numberFieldFormatter = null;

  /* members */
  /** list of edited targets (clone) */
  private List<Target> editTargets;
  /** list of edited target user informations (clone) */
  private TargetUserInformations editTargetUserInfos;
  /** current edited target */
  private Target currentTarget = null;
  /** flag to enable / disable the automatic update of the target when any swing component changes */
  private boolean doAutoUpdateTarget = true;

  /**
   * Creates new form TargetForm
   */
  public TargetForm() {
    initComponents();

    postInit();
  }

  /**
   * This method is useful to set the specific features of initialized swing components.
   */
  private void postInit() {

    // single tree selection :
    this.jTreeModels.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    // tree selection listener :
    this.jTreeModels.addTreeSelectionListener(this);

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

  }

  /**
   * Initialize the internal model (tree) from the given list of targets
   * @param targets list of targets to edit
   * @param targetUserInfos target user informations
   * @param targetName target name to select
   */
  protected void initialize(final List<Target> targets, final TargetUserInformations targetUserInfos, final String targetName) {
    this.editTargets = targets;
    this.editTargetUserInfos = targetUserInfos;

    this.generateTree(targets);
    this.selectTarget(targetName);
  }
  /* Tree related methods */
  /**
   * Return the tree model
   * @return tree model
   */
  private DefaultTreeModel getTreeTargetsModel() {
    return ((DefaultTreeModel) this.jTreeModels.getModel());
  }

  /**
   * Return the root node (Models)
   * @return root node
   */
  private DefaultMutableTreeNode getRootNode() {
    return (DefaultMutableTreeNode) getTreeTargetsModel().getRoot();
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
  private void generateTree(final List<Target> targets) {

    final DefaultMutableTreeNode rootNode = getRootNode();

    DefaultMutableTreeNode targetNode;
    for (Target target : targets) {

      targetNode = new DefaultMutableTreeNode(target);

      // TODO : add calibrators as children of the target Node

      rootNode.add(targetNode);
    }
    // fire node structure changed :
    getTreeTargetsModel().nodeStructureChanged(rootNode);
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

          // expand target node if there is at least one child :
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

    // expand target node if there is at least one child :
    if (!firstChild.isLeaf()) {
      final DefaultMutableTreeNode secondChild = (DefaultMutableTreeNode) firstChild.getFirstChild();

      this.jTreeModels.scrollPathToVisible(new TreePath(secondChild.getPath()));
    }
  }

  /**
   * Change the selected path in the target tree
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
      this.jTextAreaTargetInfos.setText(this.editTargetUserInfos.getTargetUserInformation(target).getDescription());

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

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("field " + field.getName() + " new: " + value + " old: " + oldValue);
      }

      if (value != null) {

        // check the new value :
        final double val = value.doubleValue();

        if (field == this.jFieldParaErr) {
          // check if error is negative :
          if (val < 0d) {
            if (logger.isLoggable(Level.FINE)) {
              logger.fine("Parallax Error negative : " + val);
            }

            field.setValue(oldValue);
            return;
          }
        } else if (field.getName().startsWith("FLUX")) {
          // check if magnitudes are in range [-30;100]
          if (val < -30d || val > 100d) {
            if (logger.isLoggable(Level.FINE)) {
              logger.fine("Magnitude " + field.getName() + " invalid : " + val);
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
        logger.severe("unsupported field : " + field);
      }
    }
  }

  /**
   * Process the document change event for the target user information
   * @param text new content as a string
   */
  protected void targetInfosChanged(final String text) {
    // check if the automatic update flag is enabled :
    if (this.doAutoUpdateTarget) {

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("user infos : " + text);
      }

      this.editTargetUserInfos.getTargetUserInformation(this.currentTarget).setDescription((text.length() > 0) ? text : null);
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
    jScrollPaneTreeTargets = new javax.swing.JScrollPane();
    jTreeModels = createJTree();
    jPanelTargetActions = new javax.swing.JPanel();
    jButtonUp = new javax.swing.JButton();
    jButtonDown = new javax.swing.JButton();
    jToggleButtonMarkCal = new javax.swing.JToggleButton();
    jToggleButtonAssociateCal = new javax.swing.JToggleButton();
    jSeparatorButtons = new javax.swing.JSeparator();
    jPanelTarget = new javax.swing.JPanel();
    jLabelName = new javax.swing.JLabel();
    jFieldName = new javax.swing.JTextField();
    jLabelRA = new javax.swing.JLabel();
    jFieldRA = new javax.swing.JTextField();
    jLabelDEC = new javax.swing.JLabel();
    jFieldDEC = new javax.swing.JTextField();
    jSeparator2 = new javax.swing.JSeparator();
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
    jPanelDescription = new javax.swing.JPanel();
    jScrollPaneTargetInfos = new javax.swing.JScrollPane();
    jTextAreaTargetInfos = new javax.swing.JTextArea();

    setLayout(new java.awt.GridBagLayout());

    jPanelTargets.setMinimumSize(new java.awt.Dimension(300, 100));
    jPanelTargets.setPreferredSize(new java.awt.Dimension(300, 100));
    jPanelTargets.setLayout(new java.awt.GridBagLayout());

    jScrollPaneTreeTargets.setMinimumSize(new java.awt.Dimension(80, 100));
    jScrollPaneTreeTargets.setPreferredSize(new java.awt.Dimension(130, 100));

    javax.swing.tree.DefaultMutableTreeNode treeNode1 = new javax.swing.tree.DefaultMutableTreeNode("Targets");
    jTreeModels.setModel(new javax.swing.tree.DefaultTreeModel(treeNode1));
    jScrollPaneTreeTargets.setViewportView(jTreeModels);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.gridheight = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.2;
    gridBagConstraints.weighty = 1.0;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTargets.add(jScrollPaneTreeTargets, gridBagConstraints);

    jPanelTargetActions.setBorder(javax.swing.BorderFactory.createTitledBorder("Target actions"));
    jPanelTargetActions.setEnabled(false);
    jPanelTargetActions.setMinimumSize(new java.awt.Dimension(200, 80));
    jPanelTargetActions.setPreferredSize(new java.awt.Dimension(200, 80));
    jPanelTargetActions.setLayout(new java.awt.GridBagLayout());

    jButtonUp.setText("Up");
    jButtonUp.setEnabled(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTargetActions.add(jButtonUp, gridBagConstraints);

    jButtonDown.setText("Down");
    jButtonDown.setEnabled(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTargetActions.add(jButtonDown, gridBagConstraints);

    jToggleButtonMarkCal.setText("Mark as calibrator");
    jToggleButtonMarkCal.setEnabled(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTargetActions.add(jToggleButtonMarkCal, gridBagConstraints);

    jToggleButtonAssociateCal.setText("Associate ...");
    jToggleButtonAssociateCal.setEnabled(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTargetActions.add(jToggleButtonAssociateCal, gridBagConstraints);

    jSeparatorButtons.setOrientation(javax.swing.SwingConstants.VERTICAL);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.gridheight = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
    gridBagConstraints.insets = new java.awt.Insets(0, 20, 0, 20);
    jPanelTargetActions.add(jSeparatorButtons, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weighty = 0.1;
    jPanelTargets.add(jPanelTargetActions, gridBagConstraints);

    jPanelTarget.setBorder(javax.swing.BorderFactory.createTitledBorder("Target"));
    jPanelTarget.setLayout(new java.awt.GridBagLayout());

    jLabelName.setText("Name");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelName, gridBagConstraints);

    jFieldName.setColumns(10);
    jFieldName.setEditable(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldName, gridBagConstraints);

    jLabelRA.setText("RA [HMS]");
    jLabelRA.setToolTipText("RA coordinate (J2000) (HMS)");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.weightx = 0.1;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelRA, gridBagConstraints);

    jFieldRA.setColumns(10);
    jFieldRA.setEditable(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.weightx = 0.2;
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

    jFieldDEC.setColumns(10);
    jFieldDEC.setEditable(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.weightx = 0.2;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldDEC, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 4;
    gridBagConstraints.gridwidth = 4;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.weighty = 0.05;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jSeparator2, gridBagConstraints);

    jLabelSysVel.setText("Radial Velocity");
    jLabelSysVel.setToolTipText("radial velocity in km/s");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 3;
    gridBagConstraints.gridwidth = 3;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelSysVel, gridBagConstraints);

    jFieldSysVel.setColumns(6);
    jFieldSysVel.setName("SYSVEL"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 3;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldSysVel, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.gridwidth = 4;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.weighty = 0.05;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jSeparator3, gridBagConstraints);

    jLabelPMRA.setText("PMRA");
    jLabelPMRA.setToolTipText("proper motion in RA (mas/yr)");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 5;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelPMRA, gridBagConstraints);

    jFieldPMRA.setColumns(6);
    jFieldPMRA.setName("PMRA"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 5;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldPMRA, gridBagConstraints);

    jLabelRMDEC.setText("PMDEC");
    jLabelRMDEC.setToolTipText("proper motion in DEC (mas/yr)");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 5;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelRMDEC, gridBagConstraints);

    jFieldPMDEC.setColumns(6);
    jFieldPMDEC.setName("PMDEC"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 5;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldPMDEC, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 7;
    gridBagConstraints.gridwidth = 4;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.weighty = 0.05;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jSeparator4, gridBagConstraints);

    jLabelMag.setText("Magnitudes :");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 8;
    gridBagConstraints.gridwidth = 4;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
    jPanelTarget.add(jLabelMag, gridBagConstraints);

    jLabelMagV.setText("V");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 9;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelMagV, gridBagConstraints);

    jFieldMagV.setColumns(6);
    jFieldMagV.setName("FLUXV"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 9;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldMagV, gridBagConstraints);

    jLabelMagI.setText("I");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 9;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelMagI, gridBagConstraints);

    jFieldMagI.setColumns(6);
    jFieldMagI.setName("FLUXI"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 9;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldMagI, gridBagConstraints);

    jLabelMagJ.setText("J");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 10;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelMagJ, gridBagConstraints);

    jFieldMagJ.setColumns(6);
    jFieldMagJ.setName("FLUXJ"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 10;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldMagJ, gridBagConstraints);

    jLabelMagH.setText("H");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 10;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelMagH, gridBagConstraints);

    jFieldMagH.setColumns(6);
    jFieldMagH.setName("FLUXH"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 10;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldMagH, gridBagConstraints);

    jLabelMagK.setText("K");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 11;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelMagK, gridBagConstraints);

    jFieldMagK.setColumns(6);
    jFieldMagK.setName("FLUXK"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 11;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldMagK, gridBagConstraints);

    jLabelMagN.setText("N");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 11;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelMagN, gridBagConstraints);

    jFieldMagN.setColumns(6);
    jFieldMagN.setName("FLUXN"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 11;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldMagN, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 12;
    gridBagConstraints.gridwidth = 4;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.weighty = 0.05;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jSeparator5, gridBagConstraints);

    jLabelObjTypes.setText("Object types");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 14;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelObjTypes, gridBagConstraints);

    jLabelSpecTypes.setText("Spectral type");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 13;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelSpecTypes, gridBagConstraints);

    jFieldSpecType.setColumns(10);
    jFieldSpecType.setEditable(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 13;
    gridBagConstraints.gridwidth = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldSpecType, gridBagConstraints);

    jFieldObjTypes.setColumns(10);
    jFieldObjTypes.setEditable(false);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 14;
    gridBagConstraints.gridwidth = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldObjTypes, gridBagConstraints);

    jLabelParallax.setText("Parallax");
    jLabelParallax.setToolTipText("parallax in mas");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 6;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelParallax, gridBagConstraints);

    jFieldParallax.setColumns(6);
    jFieldParallax.setName("PARALLAX"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 6;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldParallax, gridBagConstraints);

    jLabelParaErr.setText("Error");
    jLabelParaErr.setToolTipText("Error in parallax (mas/yr)");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 6;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelParaErr, gridBagConstraints);

    jFieldParaErr.setColumns(6);
    jFieldParaErr.setName("PARA_ERR"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 6;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jFieldParaErr, gridBagConstraints);

    jLabelIds.setText("Identifiers");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 15;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jLabelIds, gridBagConstraints);

    jTextAreaIds.setColumns(10);
    jTextAreaIds.setEditable(false);
    jTextAreaIds.setLineWrap(true);
    jTextAreaIds.setRows(2);
    jTextAreaIds.setTabSize(2);
    jTextAreaIds.setWrapStyleWord(true);
    jScrollPaneIds.setViewportView(jTextAreaIds);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 15;
    gridBagConstraints.gridwidth = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weighty = 0.1;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jScrollPaneIds, gridBagConstraints);

    jButtonSimbad.setText("open Simbad");
    jButtonSimbad.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonSimbadActionPerformed(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelTarget.add(jButtonSimbad, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.8;
    gridBagConstraints.weighty = 0.8;
    jPanelTargets.add(jPanelTarget, gridBagConstraints);

    jPanelDescription.setBorder(javax.swing.BorderFactory.createTitledBorder("Target information"));
    jPanelDescription.setMinimumSize(new java.awt.Dimension(10, 50));
    jPanelDescription.setPreferredSize(new java.awt.Dimension(100, 80));
    jPanelDescription.setLayout(new java.awt.GridBagLayout());

    jTextAreaTargetInfos.setBackground(new java.awt.Color(255, 255, 153));
    jTextAreaTargetInfos.setColumns(20);
    jTextAreaTargetInfos.setFont(new java.awt.Font("Monospaced", 0, 10));
    jTextAreaTargetInfos.setRows(2);
    jTextAreaTargetInfos.addFocusListener(new java.awt.event.FocusAdapter() {
      public void focusLost(java.awt.event.FocusEvent evt) {
        jTextAreaTargetInfosFocusLost(evt);
      }
    });
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
    gridBagConstraints.weighty = 0.1;
    jPanelTargets.add(jPanelDescription, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.weighty = 0.3;
    add(jPanelTargets, gridBagConstraints);
  }// </editor-fold>//GEN-END:initComponents

  private void jTextAreaTargetInfosFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_jTextAreaTargetInfosFocusLost
    // TODO add your handling code here:
    targetInfosChanged(this.jTextAreaTargetInfos.getText());
  }//GEN-LAST:event_jTextAreaTargetInfosFocusLost

  private void jButtonSimbadActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonSimbadActionPerformed

    try {
      final String url = SIMBAD_QUERY_ID + URLEncoder.encode(this.currentTarget.getName(), "UTF-8");

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("Simbad url = " + url);
      }

      BrowserLauncher.openURL(url);
    } catch (UnsupportedEncodingException uee) {
      logger.log(Level.SEVERE, "unsupported encoding : ", uee);
    }

  }//GEN-LAST:event_jButtonSimbadActionPerformed

  /**
   * Validate the form
   * @return true only if the data are valid
   */
  protected boolean validateForm() {
    // TODO : is there something to validate ?
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
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JButton jButtonDown;
  private javax.swing.JButton jButtonSimbad;
  private javax.swing.JButton jButtonUp;
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
  private javax.swing.JPanel jPanelDescription;
  private javax.swing.JPanel jPanelTarget;
  private javax.swing.JPanel jPanelTargetActions;
  private javax.swing.JPanel jPanelTargets;
  private javax.swing.JScrollPane jScrollPaneIds;
  private javax.swing.JScrollPane jScrollPaneTargetInfos;
  private javax.swing.JScrollPane jScrollPaneTreeTargets;
  private javax.swing.JSeparator jSeparator2;
  private javax.swing.JSeparator jSeparator3;
  private javax.swing.JSeparator jSeparator4;
  private javax.swing.JSeparator jSeparator5;
  private javax.swing.JSeparator jSeparatorButtons;
  private javax.swing.JTextArea jTextAreaIds;
  private javax.swing.JTextArea jTextAreaTargetInfos;
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
}
