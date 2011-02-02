/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: TargetEditorDialog.java,v 1.14 2011-02-02 17:43:21 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.13  2011/01/26 17:21:11  bourgesl
 * use deepClone (target/models ...)
 *
 * Revision 1.12  2011/01/10 12:45:37  bourgesl
 * added targetEditorActive flag to disable SearchCal Samp action
 *
 * Revision 1.11  2010/12/17 15:17:41  bourgesl
 * comment
 *
 * Revision 1.10  2010/12/14 09:24:12  bourgesl
 * use ObservationManager.updateTargets to update data model and fire target and observation change events
 *
 * Revision 1.9  2010/12/08 17:04:53  bourgesl
 * refresh the complete model tree when the tab changes
 *
 * Revision 1.8  2010/12/03 16:11:52  bourgesl
 * refactoring to use new JTree classes
 *
 * Revision 1.7  2010/11/30 17:20:25  bourgesl
 * fixed NPE when targetUserInformations is undefined
 *
 * Revision 1.6  2010/11/30 17:03:34  bourgesl
 * use new cloned ObservationSetting to define model (targets + target user informations)
 *
 * Revision 1.5  2010/11/29 13:51:43  bourgesl
 * larger height for dialog window
 *
 * Revision 1.4  2010/11/25 07:59:59  bourgesl
 * synchronize the selected target between tabs
 *
 * Revision 1.3  2010/11/23 16:56:29  bourgesl
 * fixed main (locale)
 *
 * Revision 1.2  2010/11/19 16:57:04  bourgesl
 * always open full editor with selected target
 * added target name, RA/DEC, magnitudes
 *
 * Revision 1.1  2010/11/18 17:19:35  bourgesl
 * new target and model editor using tabs (modal dialog)
 *
 */
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.gui.util.ComponentResizeAdapter;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.mcs.gui.App;
import java.awt.Component;
import java.awt.Dimension;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * This dialog contains tabs to edit both target properties and models
 * @author bourgesl
 */
public final class TargetEditorDialog extends javax.swing.JPanel {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.TargetEditorDialog";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** flag indicating that the target editor dialog is active */
  private static boolean targetEditorActive = false;

  /* members */
  /** editor result = true if the user validates the inputs */
  private boolean result = false;
  /** list of edited targets (clone) */
  private final List<Target> editTargets;
  /** list of edited target user informations (clone) */
  private final TargetUserInformations editTargetUserInfos;
  /* Swing */
  /** dialog window */
  private JDialog dialog;

  /**
   * Return the flag indicating that the target editor dialog is active
   * @return flag indicating that the target editor dialog is active
   */
  public static boolean isTargetEditorActive() {
    return targetEditorActive;
  }

  /**
   * Display the model editor using the given target name as the initial selected target
   * @param targetName optional target name to display only the models of this target
   * @return true if the model editor changed anything
   */
  public static boolean showEditor(final String targetName) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("showing Editor : " + targetName);
    }
    boolean result = false;

    JDialog dialog = null;

    try {
      // update flag to indicate that the editor is enabled :
      targetEditorActive = true;

      final ObservationManager om = ObservationManager.getInstance();

      // use deep copy of the current main observation to allow OK/cancel actions :
      final ObservationSetting cloned = om.getMainObservation().deepClone();

      // Prepare the data model (editable targets and user infos) :
      final List<Target> targets = cloned.getTargets();
      final TargetUserInformations targetUserInfos = cloned.getOrCreateTargetUserInfos();

      final TargetEditorDialog form = new TargetEditorDialog(targets, targetUserInfos);

      // initialise the editor and select the target :
      form.initialize(targetName);

      // 1. Create the dialog
      dialog = new JDialog(App.getFrame(), "Target Editor", true);

      final Dimension dim = new Dimension(600, 700);
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

      if (result) {
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("update the targets ...");
        }

        // update the complete list of targets and force to update references :
        // and fire target and observation change events :
        om.updateTargets(form.getEditTargets(), form.getEditTargetUserInfos());
      }

    } finally {
      // update flag to indicate that the editor is disabled :
      targetEditorActive = false;

      if (dialog != null) {
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("dispose Model Editor ...");
        }
        dialog.dispose();
      }
    }

    return result;
  }

  /** 
   * Creates new form TargetEditorDialog (used to test swing interface in NetBeans)
   */
  public TargetEditorDialog() {
    this(new ArrayList<Target>(0), new TargetUserInformations());
  }

  /**
   * Creates new form TargetEditorDialog with the given list of targets and user information
   * @param targets list of targets to edit
   * @param targetUserInfos target user informations
   */
  protected TargetEditorDialog(final List<Target> targets, final TargetUserInformations targetUserInfos) {
    super();

    // Define shared data model with tabbed forms :
    this.editTargets = targets;
    this.editTargetUserInfos = targetUserInfos;

    // initialize swing components :
    initComponents();

    // Register a change listener for the tabbed panel :
    this.jTabbedPane.addChangeListener(new ChangeListener() {

      /**
       * This method is called whenever the selected tab changes
       * @param evt change event
       */
      public final void stateChanged(final ChangeEvent ce) {

        final Component selected = jTabbedPane.getSelectedComponent();

        if (selected == targetModelForm) {
          // refresh the tree according to the new target / calibrator list
          // and select the target :
          targetModelForm.initialize(targetForm.getCurrentTarget().getName());
        } else if (selected == targetForm) {
          // select the target :
          targetForm.selectTarget(targetModelForm.getCurrentTarget());
        }
      }
    });
  }

  /**
   * Initialize the editor forms and the selected target
   * @param targetName target name to select
   */
  protected void initialize(final String targetName) {

    // Generate trees and select the target :

    this.targetModelForm.initialize(targetName);

    this.targetForm.initialize(targetName);
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

    jTabbedPane = new javax.swing.JTabbedPane();
    targetModelForm = new fr.jmmc.aspro.gui.TargetModelForm(this.editTargets, this.editTargetUserInfos);
    targetForm = new fr.jmmc.aspro.gui.TargetForm(this.editTargets, this.editTargetUserInfos);
    jPanelButtons = new javax.swing.JPanel();
    jButtonOK = new javax.swing.JButton();
    jButtonCancel = new javax.swing.JButton();

    setLayout(new java.awt.GridBagLayout());

    jTabbedPane.addTab("Models", null, targetModelForm, "edit target models");
    jTabbedPane.addTab("Targets", null, targetForm, "edit target information and associate calibrators");

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.weighty = 1.0;
    add(jTabbedPane, gridBagConstraints);

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
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    add(jPanelButtons, gridBagConstraints);
  }// </editor-fold>//GEN-END:initComponents

    private void jButtonOKActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonOKActionPerformed

      // delegate the validation to inner child panels :
      if (!this.targetModelForm.validateForm()) {
        return;
      }
      if (!this.targetForm.validateForm()) {
        return;
      }

      // update the validation flag :
      this.result = true;

      if (this.dialog != null) {
        this.dialog.setVisible(false);
      }
}//GEN-LAST:event_jButtonOKActionPerformed

    private void jButtonCancelActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonCancelActionPerformed
      if (this.dialog != null) {
        this.dialog.setVisible(false);
      }
}//GEN-LAST:event_jButtonCancelActionPerformed
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JButton jButtonCancel;
  private javax.swing.JButton jButtonOK;
  private javax.swing.JPanel jPanelButtons;
  private javax.swing.JTabbedPane jTabbedPane;
  private fr.jmmc.aspro.gui.TargetForm targetForm;
  private fr.jmmc.aspro.gui.TargetModelForm targetModelForm;
  // End of variables declaration//GEN-END:variables

  /**
   * Return the editor result
   * @return true if the user validated (ok button)
   */
  protected boolean isResult() {
    return result;
  }

  /**
   * Return the list of target that is edited by this editor
   * @return list of target
   */
  protected List<Target> getEditTargets() {
    return editTargets;
  }

  /**
   * Return the target user informations that is edited by this editor
   * @return target user informations
   */
  protected TargetUserInformations getEditTargetUserInfos() {
    return editTargetUserInfos;
  }

  /**
   * Define the JDialog for this form
   * @param dialog JDialog instance
   */
  protected void setDialog(final JDialog dialog) {
    this.dialog = dialog;
  }

  // --- TESTS -----------------------------------------------------------------
  /**
   * Simple GUI test
   * @param args unused
   */
  public static void main(String[] args) {
    // Set the default locale to en-US locale (for Numerical Fields "." ",")
    Locale.setDefault(Locale.US);

    SwingUtilities.invokeLater(new Runnable() {

      public void run() {

        try {
          ObservationManager.getInstance().load(new File("/home/bourgesl/VLTI_FUN2.asprox"));
          logger.info("result = " + showEditor("HIP32768"));
          System.exit(0);
        } catch (Exception e) {
          logger.log(Level.SEVERE, "runtime exception", e);
        }
      }
    });

  }
}
