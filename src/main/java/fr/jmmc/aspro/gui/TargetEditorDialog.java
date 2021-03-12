/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.jmcs.App;
import fr.jmmc.jmcs.gui.component.ComponentResizeAdapter;
import fr.jmmc.jmcs.gui.component.Disposable;
import fr.jmmc.jmcs.gui.util.WindowUtils;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This dialog contains tabs to edit both target properties and models
 * @author bourgesl
 */
public final class TargetEditorDialog extends javax.swing.JPanel implements Disposable {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(TargetEditorDialog.class.getName());
    /** Tab Targets */
    public static final String TAB_TARGETS = "Targets";
    /** Tab Models */
    public static final String TAB_MODELS = "Models";
    /** Tab Targets */
    public static final String TAB_GROUPS = "Groups";
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
    /** current form */
    private Component currentComponent = null;

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
     * @param selectedTab name of the selected tab (see TAB_xxx constants)
     * @return true if the model editor changed anything
     */
    public static boolean showEditor(final String targetName, final String selectedTab) {
        logger.debug("showing Editor : {}", targetName);

        boolean result = false;

        JDialog dialog = null;
        TargetEditorDialog form = null;

        try {
            // update flag to indicate that the editor is enabled :
            targetEditorActive = true;

            final ObservationManager om = ObservationManager.getInstance();

            // use deep copy of the current main observation to allow OK/cancel actions :
            final ObservationSetting cloned = om.getMainObservation().deepClone();

            // Prepare the data model (editable targets and user infos) :
            final List<Target> targets = cloned.getTargets();
            final TargetUserInformations targetUserInfos = cloned.getOrCreateTargetUserInfos();

            form = new TargetEditorDialog(targets, targetUserInfos, selectedTab);

            // initialise the editor and select the target :
            form.initialize(targetName);

            // 1. Create the dialog
            dialog = new JDialog(App.getFrame(), "Target Editor", true);

            // get screen size to adjust minimum window size :
            final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

            logger.debug("screen size = {} x {}", screenSize.getWidth(), screenSize.getHeight());

            // hack for screens smaller than 1152x864 screens :
            final int h = (screenSize.getHeight() >= 768) ? 800 : 700;

            final Dimension dim = new Dimension(800, h);
            dialog.setMinimumSize(dim);
            dialog.addComponentListener(new ComponentResizeAdapter(dim));

            // 2. Optional: What happens when the dialog closes ?
            dialog.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

            // 3. Create components and put them in the dialog
            dialog.add(form);

            // Associate the dialog to the form :
            form.setDialog(dialog);

            // 4. Size the dialog.
            WindowUtils.setClosingKeyboardShortcuts(dialog);
            dialog.pack();

            // Center it :
            dialog.setLocationRelativeTo(dialog.getOwner());

            // 5. Show it and waits until dialog is not visible or disposed :
            dialog.setVisible(true);

            // get editor result :
            result = form.isResult();

            if (result) {
                logger.debug("update the targets ...");

                // update the complete list of targets and force to update references :
                // and fire target and observation change events :
                om.updateTargets(form.getEditTargets(), form.getEditTargetUserInfos());
            }

        } finally {
            // update flag to indicate that the editor is disabled :
            targetEditorActive = false;

            if (form != null) {
                logger.debug("dispose TargetEditorDialog ...");

                form.dispose();
            }
            if (dialog != null) {
                logger.debug("dispose Model Editor ...");

                dialog.dispose();
            }
        }

        return result;
    }

    /** 
     * Creates new form TargetEditorDialog (used to test swing interface in NetBeans)
     */
    public TargetEditorDialog() {
        this(new ArrayList<Target>(0), new TargetUserInformations(), TAB_TARGETS);
    }

    /**
     * Creates new form TargetEditorDialog with the given list of targets and user information
     * @param targets list of targets to edit
     * @param targetUserInfos target user informations
     * @param selectedTab name of the selected tab (see TAB_xxx constants)
     */
    protected TargetEditorDialog(final List<Target> targets, final TargetUserInformations targetUserInfos,
                                 final String selectedTab) {
        super();

        // Define shared data model with tabbed forms :
        this.editTargets = targets;
        this.editTargetUserInfos = targetUserInfos;

        // initialize swing components :
        initComponents();

        // Select tab:
        if (selectedTab != null) {
            for (int i = 0, size = this.jTabbedPane.getTabCount(); i < size; i++) {
                if (selectedTab.equals(this.jTabbedPane.getTitleAt(i))) {
                    this.jTabbedPane.setSelectedIndex(i);
                    break;
                }
            }
        }

        // Register a change listener for the tabbed panel :
        this.jTabbedPane.addChangeListener(new ChangeListener() {
            /**
             * This method is called whenever the selected tab changes
             * @param evt change event
             */
            @Override
            public final void stateChanged(final ChangeEvent ce) {
                final Component selected = jTabbedPane.getSelectedComponent();

                if (selected != targetModelForm) {
                    targetModelForm.disableForm();
                }

                final Target target = getCurrentTarget();

                if (selected == targetForm) {
                    // select the target :
                    targetForm.selectTarget(target);
                } else if (selected == targetModelForm) {
                    // refresh the tree according to the new target / calibrator list
                    // and select the target :
                    targetModelForm.initialize((target != null) ? target.getName() : null);
                } else if (selected == targetGroupForm) {
                    // refresh the tree according to the new target / calibrator list
                    // and select the target :
                    targetGroupForm.initialize((target != null) ? target.getName() : null);
                }
                currentComponent = selected;
            }

            private Target getCurrentTarget() {
                if (currentComponent == targetForm) {
                    return targetForm.getCurrentTarget();
                } else if (currentComponent == targetModelForm) {
                    return targetModelForm.getCurrentTarget();
                } else if (currentComponent == targetGroupForm) {
                    return targetGroupForm.getCurrentTarget();
                }
                return null;
            }
        });
    }

    /**
     * Initialize the editor forms and the selected target
     * @param targetName target name to select
     */
    void initialize(final String targetName) {
        this.currentComponent = jTabbedPane.getSelectedComponent();

        // Generate trees and select the target :
        if (currentComponent == targetModelForm) {
            // only initialize Model form (model animator) if displayed:
            this.targetModelForm.initialize(targetName);
        }

        this.targetForm.initialize(targetName);

        this.targetGroupForm.initialize(targetName);
    }

    /**
     * Free any resource or reference to this instance
     */
    @Override
    public void dispose() {
        this.jTabbedPane.removeAll();
        this.targetModelForm.dispose();
        // GC
        this.targetForm = null;
        this.targetModelForm = null;
        this.targetGroupForm = null;
        this.currentComponent = null;
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
        targetForm = new TargetForm(this.editTargets, this.editTargetUserInfos);
        targetModelForm = new TargetModelForm(this.editTargets, this.editTargetUserInfos);
        targetGroupForm = new TargetGroupForm(this.editTargets, this.editTargetUserInfos);
        jPanelButtons = new javax.swing.JPanel();
        jButtonCancel = new javax.swing.JButton();
        jButtonOK = new javax.swing.JButton();

        setLayout(new java.awt.GridBagLayout());

        jTabbedPane.addTab("Targets", null, targetForm, "edit target information and associate calibrators");
        jTabbedPane.addTab("Models", null, targetModelForm, "edit target models");
        jTabbedPane.addTab("Groups", targetGroupForm);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jTabbedPane, gridBagConstraints);

        jPanelButtons.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 12, 6));

        jButtonCancel.setText("Cancel");
        jButtonCancel.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonCancelActionPerformed(evt);
            }
        });
        jPanelButtons.add(jButtonCancel);

        jButtonOK.setText("OK");
        jButtonOK.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonOKActionPerformed(evt);
            }
        });
        jPanelButtons.add(jButtonOK);

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
        if (!this.targetGroupForm.validateForm()) {
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
    private fr.jmmc.aspro.gui.TargetGroupForm targetGroupForm;
    private fr.jmmc.aspro.gui.TargetModelForm targetModelForm;
    // End of variables declaration//GEN-END:variables

    /**
     * Return the editor result
     * @return true if the user validated (ok button)
     */
    private boolean isResult() {
        return result;
    }

    /**
     * Return the list of target that is edited by this editor
     * @return list of target
     */
    private List<Target> getEditTargets() {
        return editTargets;
    }

    /**
     * Return the target user informations that is edited by this editor
     * @return target user informations
     */
    private TargetUserInformations getEditTargetUserInfos() {
        return editTargetUserInfos;
    }

    /**
     * Define the JDialog for this form
     * @param dialog JDialog instance
     */
    private void setDialog(final JDialog dialog) {
        this.dialog = dialog;
    }
}
