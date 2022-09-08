/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.TargetEditContext;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.jmcs.App;
import fr.jmmc.jmcs.gui.component.ComponentResizeAdapter;
import fr.jmmc.jmcs.gui.component.Disposable;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.gui.util.WindowUtils;
import java.awt.Component;
import java.awt.Dimension;
import java.io.File;
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
    /** dev GC flag */
    private final static boolean SHOW_GC_BUTTON = false;
    /** Target Editor key to remember file dialog dimensions */
    private final static String TARGET_EDITOR_DIMENSION_KEY = "___ASPRO2_TARGET_EDITOR_DIMENSION";
    /** Tab Targets */
    public static final String TAB_TARGETS = "Targets";
    /** Tab Models */
    public static final String TAB_MODELS = "Models";
    /** Tab Targets */
    public static final String TAB_GROUPS = "Groups";
    /** target editor instance defined when the target editor dialog is active */
    private static TargetEditorDialog targetEditor = null;

    /* members */
    /** target edit context */
    private final TargetEditContext targetEditCtx;
    /** editor result = true if the user validates the inputs */
    private boolean result = false;
    /* Swing */
    /** dialog window */
    private JDialog dialog;
    /** current form */
    private Component currentComponent = null;

    /**
     * Return the target editor instance
     * @return target editor or null if the editor is not active
     */
    public static TargetEditorDialog getTargetEditor() {
        return targetEditor;
    }

    /**
     * Display the model editor using the given target name as the initial selected target
     * @param targetName optional target name to display only the models of this target
     * @param selectedTab name of the selected tab (see TAB_xxx constants)
     * @return true if the model editor changed anything
     */
    public static boolean showEditor(final String targetName, final String selectedTab) {
        return showEditor(targetName, selectedTab, null);
    }

    /**
     * Display the model editor using the given target name as the initial selected target
     * @param targetName optional target name to display only the models of this target
     * @param selectedTab name of the selected tab (see TAB_xxx constants)
     * @param userModelFile optional user model file to associate with the current target
     * @return true if the model editor changed anything
     */
    public static boolean showEditor(final String targetName, final String selectedTab,
                                     final File userModelFile) {

        logger.debug("showing Editor : {}", targetName);

        boolean result = false;

        JDialog dialog = null;
        TargetEditorDialog form = null;

        try {
            final ObservationManager om = ObservationManager.getInstance();

            // Prepare the data model (editable targets and user infos) :
            final TargetEditContext targetEditCtx = om.getMainObservation().createTargetEditContext();

            form = new TargetEditorDialog(targetEditCtx, selectedTab);
            // set target editor instance:
            targetEditor = form;

            // initialise the editor and select the target :
            form.initialize(targetName);

            if (userModelFile != null) {
                form.defineUserModel(userModelFile);
            }

            // 1. Create the dialog
            dialog = new JDialog(App.getFrame(), "Target Editor", true);

            final Dimension dim = new Dimension(800, 700);
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

            // Restore, then automatically save window size changes:
            WindowUtils.rememberWindowSize(dialog, TARGET_EDITOR_DIMENSION_KEY);

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
                om.updateTargets(form.getTargetEditCtx());
            }
        } finally {
            // reset the target editor instance to indicate that the editor is disabled:
            targetEditor = null;

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
        this(new TargetEditContext(), TAB_TARGETS);
    }

    /**
     * Creates new TargetEditorDialog form with the given list of targets and user information
     * @param targetEditCtx target editor context
     * @param selectedTab name of the selected tab (see TAB_xxx constants)
     */
    protected TargetEditorDialog(final TargetEditContext targetEditCtx, final String selectedTab) {
        super();

        // Define shared data model with tabbed forms :
        this.targetEditCtx = targetEditCtx;

        // initialize swing components :
        initComponents();

        selectTab(selectedTab);

        // Register a change listener for the tabbed panel :
        this.jTabbedPane.addChangeListener(new ChangeListener() {
            /**
             * This method is called whenever the selected tab changes
             * @param evt change event
             */
            @Override
            public final void stateChanged(final ChangeEvent ce) {
                refreshDialog();
            }
        });
        
        this.jButtonGC.setVisible(SHOW_GC_BUTTON);
    }

    /**
     * Initialize the editor forms and the selected target
     * @param targetName target name to select
     */
    void initialize(final String targetName) {
        this.currentComponent = jTabbedPane.getSelectedComponent();

        logger.warn("initialize: {}", targetName);

        // Generate trees and select the target:
        if (currentComponent == targetModelForm) {
            // only initialize Model form (model animator) if displayed:
            this.targetModelForm.initialize(targetName);
        }

        this.targetForm.initialize(targetName);
        this.targetGroupForm.initialize(targetName);
    }

    public void closeDialog() {
        // cancel:
        if (this.dialog != null) {
            this.dialog.setVisible(false);
        }
    }

    public void defineUserModel(final File userModelFile) {
        if (userModelFile != null) {
            selectTab(TAB_MODELS);

            // Use invokeLater to selection change issues with form:
            SwingUtils.invokeLaterEDT(new Runnable() {
                /**
                 * Update tree selection
                 */
                @Override
                public void run() {
                    targetModelForm.defineUserModel(userModelFile);
                }
            });
        }
    }

    private void selectTab(final String selectedTab) {
        // Select tab:
        if (selectedTab != null) {
            for (int i = 0, size = this.jTabbedPane.getTabCount(); i < size; i++) {
                if (selectedTab.equals(this.jTabbedPane.getTitleAt(i))) {
                    this.jTabbedPane.setSelectedIndex(i);
                    break;
                }
            }
        }
        // will call refreshDialog via change listener if needed
    }

    public void refreshDialog() {
        // Get target from the current component:
        final Target target = getCurrentTarget();
        logger.debug("refreshDialog: {}", target);

        // Change the current component:
        final Component selected = jTabbedPane.getSelectedComponent();
        this.currentComponent = selected;

        if (selected != targetModelForm) {
            targetModelForm.disableForm();
        }

        if (selected == targetForm) {
            // select the target :
            // targetForm.selectTarget(target);
            targetForm.initialize((target != null) ? target.getName() : null);
        } else if (selected == targetModelForm) {
            // refresh the tree according to the new target / calibrator list
            // and select the target :
            targetModelForm.initialize((target != null) ? target.getName() : null);
        } else if (selected == targetGroupForm) {
            // refresh the tree according to the new target / calibrator list
            // and select the target :
            targetGroupForm.initialize((target != null) ? target.getName() : null);
        }
    }

    /**
     * Free any resource or reference to this instance
     */
    @Override
    public void dispose() {
        this.jTabbedPane.removeAll();
        this.targetModelForm.dispose();
        this.targetGroupForm.dispose();
        // GC
        this.targetForm = null;
        this.targetModelForm = null;
        this.targetGroupForm = null;
        this.currentComponent = null;
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
        targetForm = new TargetForm(this.getEditTargets(), this.getEditTargetUserInfos());
        targetModelForm = new TargetModelForm(this.getEditTargets(), this.getEditTargetUserInfos());
        targetGroupForm = new TargetGroupForm(this.getEditTargets(), this.getEditTargetUserInfos());
        jPanelButtons = new javax.swing.JPanel();
        jButtonCancel = new javax.swing.JButton();
        jButtonOK = new javax.swing.JButton();
        jButtonGC = new javax.swing.JButton();

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

        jButtonGC.setText("[GC]");
        jButtonGC.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonGCActionPerformed(evt);
            }
        });
        jPanelButtons.add(jButtonGC);

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

        final Target target = getCurrentTarget();
        if (target != null) {
            this.targetEditCtx.setSelectedTarget(target);
        }
        
        // update the validation flag :
        this.result = true;

        if (this.dialog != null) {
            this.dialog.setVisible(false);
        }
}//GEN-LAST:event_jButtonOKActionPerformed

    private void jButtonCancelActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonCancelActionPerformed
        closeDialog();
}//GEN-LAST:event_jButtonCancelActionPerformed

    private void jButtonGCActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonGCActionPerformed
        System.gc();
        System.gc();
    }//GEN-LAST:event_jButtonGCActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonCancel;
    private javax.swing.JButton jButtonGC;
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
     * Return the target edit context
     * @return target edit context
     */
    public TargetEditContext getTargetEditCtx() {
        return targetEditCtx;
    }

    /**
     * Return the list of target that is edited by this editor
     * @return list of target
     */
    private List<Target> getEditTargets() {
        return targetEditCtx.getTargets();
    }

    /**
     * Return the target user informations that is edited by this editor
     * @return target user informations
     */
    private TargetUserInformations getEditTargetUserInfos() {
        return targetEditCtx.getTargetUserInfos();
    }

    /**
     * Define the JDialog for this form
     * @param dialog JDialog instance
     */
    private void setDialog(final JDialog dialog) {
        this.dialog = dialog;
    }
}
