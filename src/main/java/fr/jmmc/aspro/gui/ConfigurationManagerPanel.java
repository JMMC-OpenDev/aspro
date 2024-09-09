/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.InterferometerFile;
import fr.jmmc.jmcs.App;
import fr.jmmc.jmcs.data.MimeType;
import fr.jmmc.jmcs.gui.component.ComponentResizeAdapter;
import fr.jmmc.jmcs.gui.component.FileChooser;
import fr.jmmc.jmcs.gui.component.GenericListModel;
import fr.jmmc.jmcs.gui.util.ResourceImage;
import fr.jmmc.jmcs.gui.util.WindowUtils;
import fr.jmmc.jmcs.service.BrowserLauncher;
import fr.jmmc.jmcs.util.StringUtils;
import java.awt.Component;
import java.awt.Dimension;
import java.io.File;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Simple form to handle user configuration files (interferometer description & configuration)
 * @author bourgesl
 */
public final class ConfigurationManagerPanel extends javax.swing.JPanel {

    private static final long serialVersionUID = 1L;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(ConfigurationManagerPanel.class.getName());
    /** link to aspro-conf-contrib repository */
    private static final String ASPRO_CONF_CONTRIB = "https://github.com/JMMC-OpenDev/aspro-conf-contrib";

    /** Target Editor key to remember file dialog dimensions */
    private final static String CONF_EDITOR_DIMENSION_KEY = "___ASPRO2_CONF_EDITOR_DIMENSION";
    /** configuration manager */
    private static final ConfigurationManager cm = ConfigurationManager.getInstance();

    /* members */
    /** dialog window */
    private JDialog dialog;
    /** selected file reference */
    private InterferometerFile selectedFileRef = null;
    /** flag to enable / disable the automatic update when any swing component changes */
    private boolean doAutoUpdate = true;

    /**
     * Display the Configuration Manager
     */
    public static void showDialog() {
        JDialog dialog = null;

        try {
            final ConfigurationManagerPanel form = new ConfigurationManagerPanel();

            // initialise the editor:
            form.initialize();

            // 1. Create the dialog
            dialog = new JDialog(App.getFrame(), "Configuration Manager", true);

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
            WindowUtils.rememberWindowSize(dialog, CONF_EDITOR_DIMENSION_KEY);

            // Center it :
            dialog.setLocationRelativeTo(dialog.getOwner());

            // 5. Show it and waits until dialog is not visible or disposed :
            dialog.setVisible(true);

            // once the editor is closed:
            // update observation and fire GUI update:
            ObservationManager.getInstance().handleConfigurationChanged();

        } finally {
            if (dialog != null) {
                logger.debug("dispose Configuration Manager ...");

                dialog.dispose();
            }
        }
    }

    /** Creates new form ConfigurationManagerPanel */
    public ConfigurationManagerPanel() {
        initComponents();

        postInit();
    }

    private void postInit() {
        this.jListUserConfig.setCellRenderer(InterferometerFileRenderer.INSTANCE);
    }

    /**
     * Initialize the form
     */
    private void initialize() {
        logger.debug("initialize");

        try {
            doAutoUpdate = false;

            // initialize files:
            final InterferometerFile oldValue = this.jListUserConfig.getSelectedValue();

            this.jListUserConfig.setModel(new GenericListModel<InterferometerFile>(cm.getUserConfigurationFiles()));

            // restore previous selected item :
            if (oldValue != null) {
                this.jListUserConfig.setSelectedValue(oldValue, true);
            }
            // ensure one config is selected :
            checkUserConfigSelection();

            if (this.jListUserConfig.getModel().getSize() == 0) {
                showUserConfig(null);
            }
        } finally {
            doAutoUpdate = true;
        }
    }

    /**
     * Check if the selected file is empty, then restore the last selected file
     * or select the first file
     */
    private void checkUserConfigSelection() {
        checkListSelection(jListUserConfig, selectedFileRef);
    }

    private void showUserConfig(final InterferometerFile fileRef) {
        if (fileRef != null) {
            this.selectedFileRef = fileRef;
            this.jTextFieldFileReference.setText(fileRef.getFile());
            setInterferometerName(fileRef.getInterferometerName());

            if (fileRef.isReallyEnabled()) {
                this.jRadioButtonStateOn.setSelected(true);
            } else {
                this.jRadioButtonStateOff.setSelected(true);
            }
        } else {
            this.selectedFileRef = null;
            this.jTextFieldFileReference.setText(null);
            setInterferometerName(null);
            this.jRadioButtonStateOff.setSelected(true);
        }
        final boolean enabled = (fileRef != null);
        this.jButtonReload.setEnabled(enabled);
        this.jButtonRemove.setEnabled(enabled);

        final boolean valid = (fileRef != null) && fileRef.isValid();
        this.jRadioButtonStateOn.setEnabled(valid);
        this.jRadioButtonStateOff.setEnabled(valid);
    }

    private void setInterferometerName(final String name) {
        final String text = StringUtils.isEmpty(name) ? "[INVALID]" : name;
        this.jTextFieldName.setText(text);
    }

    /**
     * Return the currently selected user config
     * @return user config
     */
    public InterferometerFile getSelectedUserConfig() {
        return selectedFileRef;
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

        buttonGroupState = new javax.swing.ButtonGroup();
        jPanelMain = new javax.swing.JPanel();
        jLabelID = new javax.swing.JLabel();
        jTextFieldName = new javax.swing.JTextField();
        jScrollPaneUserConfigs = new javax.swing.JScrollPane();
        jListUserConfig = new javax.swing.JList<>();
        jLabelFile = new javax.swing.JLabel();
        jLabelValid = new javax.swing.JLabel();
        jRadioButtonStateOn = new javax.swing.JRadioButton();
        jRadioButtonStateOff = new javax.swing.JRadioButton();
        jTextFieldFileReference = new javax.swing.JTextField();
        jPanelButtons = new javax.swing.JPanel();
        jButtonLoad = new javax.swing.JButton();
        jButtonReload = new javax.swing.JButton();
        jButtonRemove = new javax.swing.JButton();
        jButtonOpenAsproConfContrib = new javax.swing.JButton();
        jPanelBottom = new javax.swing.JPanel();
        jButtonOK = new javax.swing.JButton();

        setLayout(new java.awt.GridBagLayout());

        jPanelMain.setBorder(javax.swing.BorderFactory.createTitledBorder("User Configurations"));
        jPanelMain.setLayout(new java.awt.GridBagLayout());

        jLabelID.setText("Name");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        jPanelMain.add(jLabelID, gridBagConstraints);

        jTextFieldName.setEditable(false);
        jTextFieldName.setColumns(20);
        jTextFieldName.setText("Name");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelMain.add(jTextFieldName, gridBagConstraints);

        jListUserConfig.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        jListUserConfig.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
            public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
                jListUserConfigValueChanged(evt);
            }
        });
        jScrollPaneUserConfigs.setViewportView(jListUserConfig);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.1;
        gridBagConstraints.weighty = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        jPanelMain.add(jScrollPaneUserConfigs, gridBagConstraints);

        jLabelFile.setText("File");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        jPanelMain.add(jLabelFile, gridBagConstraints);

        jLabelValid.setText("State");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        gridBagConstraints.weightx = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 6);
        jPanelMain.add(jLabelValid, gridBagConstraints);

        buttonGroupState.add(jRadioButtonStateOn);
        jRadioButtonStateOn.setSelected(true);
        jRadioButtonStateOn.setText("enabled");
        jRadioButtonStateOn.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonStateOnActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.weightx = 0.4;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelMain.add(jRadioButtonStateOn, gridBagConstraints);

        buttonGroupState.add(jRadioButtonStateOff);
        jRadioButtonStateOff.setText("disabled");
        jRadioButtonStateOff.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonStateOffActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.weightx = 0.4;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelMain.add(jRadioButtonStateOff, gridBagConstraints);

        jTextFieldFileReference.setEditable(false);
        jTextFieldFileReference.setColumns(20);
        jTextFieldFileReference.setText("FileReference");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.6;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelMain.add(jTextFieldFileReference, gridBagConstraints);

        jPanelButtons.setLayout(new java.awt.GridBagLayout());

        jButtonLoad.setText("Load");
        jButtonLoad.setToolTipText("Load a local configuration file");
        jButtonLoad.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonLoadActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 10;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        jPanelButtons.add(jButtonLoad, gridBagConstraints);

        jButtonReload.setText("Reload");
        jButtonReload.setToolTipText("Reload the selected configuration file");
        jButtonReload.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonReloadActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        jPanelButtons.add(jButtonReload, gridBagConstraints);

        jButtonRemove.setText("Remove");
        jButtonRemove.setToolTipText("Remove the selected configuration file");
        jButtonRemove.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonRemoveActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanelButtons.add(jButtonRemove, gridBagConstraints);

        jButtonOpenAsproConfContrib.setText("<html>Open aspro-conf<br>contrib...</html>");
        jButtonOpenAsproConfContrib.setMargin(new java.awt.Insets(2, 20, 2, 20));
        jButtonOpenAsproConfContrib.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonOpenAsproConfContribActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.PAGE_END;
        gridBagConstraints.weighty = 0.8;
        jPanelButtons.add(jButtonOpenAsproConfContrib, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 5, 0);
        jPanelMain.add(jPanelButtons, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.1;
        gridBagConstraints.weighty = 0.1;
        add(jPanelMain, gridBagConstraints);

        jPanelBottom.setLayout(new java.awt.GridBagLayout());

        jButtonOK.setText("OK");
        jButtonOK.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonOKActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        jPanelBottom.add(jButtonOK, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        add(jPanelBottom, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void jButtonOKActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonOKActionPerformed
        logger.debug("jButtonOKActionPerformed");

        if (this.dialog != null) {
            this.dialog.setVisible(false);
        }
    }//GEN-LAST:event_jButtonOKActionPerformed

    private void jButtonLoadActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonLoadActionPerformed
        logger.debug("jButtonLoadActionPerformed");

        final File file = FileChooser.showOpenFileChooser("Load interferometer settings", null, MimeType.ASPRO_CONFIGURATION);

        if (file != null) {
            cm.addUserConfiguration(file.getAbsolutePath());
            // anyway refresh:
            initialize();
        }
    }//GEN-LAST:event_jButtonLoadActionPerformed

    private void jButtonReloadActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonReloadActionPerformed
        logger.debug("jButtonReloadActionPerformed");

        final InterferometerFile fileRef = getSelectedUserConfig();

        if (fileRef != null) {
            cm.reloadUserConfiguration(fileRef.getFile());
            // anyway refresh:            
            initialize();
        }
    }//GEN-LAST:event_jButtonReloadActionPerformed

    private void jListUserConfigValueChanged(javax.swing.event.ListSelectionEvent evt) {//GEN-FIRST:event_jListUserConfigValueChanged
        // skip events when the user selection is adjusting:
        if (evt.getValueIsAdjusting()) {
            return;
        }

        final InterferometerFile userConfig = this.jListUserConfig.getSelectedValue();

        // ensure at least one item is selected:
        if (userConfig == null) {
            checkUserConfigSelection();
            return;
        }

        logger.debug("userConfig: {}", userConfig);

        showUserConfig(userConfig);
    }//GEN-LAST:event_jListUserConfigValueChanged

    private void jButtonRemoveActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonRemoveActionPerformed
        logger.debug("jButtonRemoveActionPerformed");

        final InterferometerFile fileRef = getSelectedUserConfig();

        if (fileRef != null) {
            cm.removeUserConfiguration(fileRef.getFile());
            // anyway refresh:
            initialize();
        }
    }//GEN-LAST:event_jButtonRemoveActionPerformed

    private void jRadioButtonStateOffActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButtonStateOffActionPerformed
        logger.debug("jRadioButtonStateOffActionPerformed");
        changeState();
    }//GEN-LAST:event_jRadioButtonStateOffActionPerformed

    private void jRadioButtonStateOnActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButtonStateOnActionPerformed
        logger.debug("jRadioButtonStateOnActionPerformed");
        changeState();
    }//GEN-LAST:event_jRadioButtonStateOnActionPerformed

    private void jButtonOpenAsproConfContribActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonOpenAsproConfContribActionPerformed
        BrowserLauncher.openURL(ASPRO_CONF_CONTRIB);
    }//GEN-LAST:event_jButtonOpenAsproConfContribActionPerformed

    private void changeState() {
        if (doAutoUpdate) {
            final InterferometerFile fileRef = getSelectedUserConfig();

            if (fileRef != null) {
                if (cm.setUserConfigurationEnabled(fileRef.getFile(), this.jRadioButtonStateOn.isSelected())) {
                    initialize();
                }
            }
        }
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroupState;
    private javax.swing.JButton jButtonLoad;
    private javax.swing.JButton jButtonOK;
    private javax.swing.JButton jButtonOpenAsproConfContrib;
    private javax.swing.JButton jButtonReload;
    private javax.swing.JButton jButtonRemove;
    private javax.swing.JLabel jLabelFile;
    private javax.swing.JLabel jLabelID;
    private javax.swing.JLabel jLabelValid;
    private javax.swing.JList<InterferometerFile> jListUserConfig;
    private javax.swing.JPanel jPanelBottom;
    private javax.swing.JPanel jPanelButtons;
    private javax.swing.JPanel jPanelMain;
    private javax.swing.JRadioButton jRadioButtonStateOff;
    private javax.swing.JRadioButton jRadioButtonStateOn;
    private javax.swing.JScrollPane jScrollPaneUserConfigs;
    private javax.swing.JTextField jTextFieldFileReference;
    private javax.swing.JTextField jTextFieldName;
    // End of variables declaration//GEN-END:variables

    /**
     * Define the JDialog for this form
     * @param dialog JDialog instance
     */
    private void setDialog(final JDialog dialog) {
        this.dialog = dialog;
    }

    /**
     * Check if the given list selection is empty, then restore the last selected item
     * or select the first item (if exist)
     * @param list JList to use
     * @param lastValue last selected value for the given list
     * @param <K> type of every list item
     */
    @SuppressWarnings("unchecked")
    private static <K> void checkListSelection(final JList list, final K lastValue) {
        // ensure at least one item is selected :
        if (list.getSelectionModel().isSelectionEmpty()) {
            // previously an item was selected - select it back (if possible) :
            K selection = lastValue;

            final GenericListModel<K> model = (GenericListModel<K>) list.getModel();

            if (selection == null || !model.contains(selection)) {
                // Select first item (if exist) :
                selection = (model.isEmpty()) ? null : model.get(0);
            }
            if (selection != null) {
                logger.debug("list selection empty - select: {}", selection);

                list.setSelectedValue(selection, true);
            } else {
                logger.debug("list selection empty - nothing to select !");
            }
        }
    }

    final static class InterferometerFileRenderer extends DefaultListCellRenderer {

        /** default serial UID for Serializable interface */
        private static final long serialVersionUID = 1;

        static final InterferometerFileRenderer INSTANCE = new InterferometerFileRenderer();

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
            } else if (value instanceof InterferometerFile) {
                final InterferometerFile fileRef = (InterferometerFile) value;
                if (fileRef.isValid()) {
                    val = "[" + fileRef.getInterferometerName() + "] " + fileRef.getFile();
                } else {
                    val = fileRef.getFile();
                }
            } else {
                val = value.toString();
            }

            super.getListCellRendererComponent(
                    list, val, index,
                    isSelected, cellHasFocus);

            if (value instanceof InterferometerFile) {
                setIcon((((InterferometerFile) value).isReallyEnabled()
                        ? ResourceImage.OK_MARK.icon()
                        : ResourceImage.KO_MARK.icon())
                );
            }

            return this;
        }
    }
}
