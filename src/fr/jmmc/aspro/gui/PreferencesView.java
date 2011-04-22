/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: PreferencesView.java,v 1.9 2011-04-22 15:38:41 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.8  2010/10/22 13:31:37  bourgesl
 * added preference for Time LST/UTC
 *
 * Revision 1.7  2010/09/08 16:00:31  bourgesl
 * unregister Preference Observers when the widget is released (Preference View, UV Coverage Panel)
 *
 * Revision 1.6  2010/06/17 10:02:50  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.5  2010/06/08 12:32:46  bourgesl
 * javadoc
 *
 * Revision 1.4  2010/05/21 14:27:34  bourgesl
 * added preferences for Model Image Lut & Size
 *
 * Revision 1.3  2010/05/19 09:08:10  mella
 * dispose on close
 *
 * Revision 1.2  2010/05/17 16:09:03  mella
 * Add the preference view as preference observer
 *
 * Revision 1.1  2010/05/12 08:44:10  mella
 * Add one preferences window first to choose the default style of display for positions
 *
 *
 */
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.mcs.image.ColorModels;
import fr.jmmc.mcs.util.PreferencesException;
import java.util.Observable;
import java.util.Observer;
import java.util.logging.Level;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JFrame;

/**
 * Preferences GUI
 */
public final class PreferencesView extends JFrame implements Observer {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.PreferencesView";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /* members */
  /** preference singleton */
  private final Preferences myPreferences = Preferences.getInstance();

  /** 
   * Creates a new PreferencesView
   */
  public PreferencesView() {
    initComponents();

    postInit();
  }

  /**
   * This method is useful to set the models and specific features of initialized swing components :
   * Update the combo boxes with their models
   */
  private void postInit() {
    // define custom models :
    this.jComboBoxLUT.setModel(new DefaultComboBoxModel(ColorModels.getColorModelNames()));
    this.jComboBoxImageSize.setModel(new DefaultComboBoxModel(AsproConstants.IMAGE_SIZES));

    // register this instance as a Preference Observer :
    this.myPreferences.addObserver(this);

    // update GUI
    update(null, null);

    // pack and center window
    pack();
    setLocationRelativeTo(null);
  }

  /**
   * Free any ressource or reference to this instance :
   * remove this instance form Preference Observers
   */
  @Override
  public void dispose() {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("dispose : " + this);
    }

    // unregister this instance as a Preference Observer :
    this.myPreferences.deleteObserver(this);

    // dispose Frame :
    super.dispose();
  }

  /**
   * Overriden method to give object identifier
   * @return string identifier
   */
  @Override
  public String toString() {
    return "PreferencesView@" + Integer.toHexString(hashCode());
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

    buttonGroupPositionStyle = new javax.swing.ButtonGroup();
    buttonGroupTimeRef = new javax.swing.ButtonGroup();
    buttonGroupTimeAxis = new javax.swing.ButtonGroup();
    jPanel1 = new javax.swing.JPanel();
    jLabelTimeRef = new javax.swing.JLabel();
    jRadioButtonTimeLST = new javax.swing.JRadioButton();
    jRadioButtonTimeUTC = new javax.swing.JRadioButton();
    jLabelCenterNight = new javax.swing.JLabel();
    jRadioButtonCenterNightYes = new javax.swing.JRadioButton();
    jRadioButtonCenterNightNo = new javax.swing.JRadioButton();
    jPanelModelEditor = new javax.swing.JPanel();
    jLabelPositionStyle = new javax.swing.JLabel();
    jRadioButtonXY = new javax.swing.JRadioButton();
    jRadioButtonRhoTheta = new javax.swing.JRadioButton();
    jPanelModelImage = new javax.swing.JPanel();
    jLabelLutTable = new javax.swing.JLabel();
    jComboBoxLUT = new javax.swing.JComboBox();
    jLabelImageSize = new javax.swing.JLabel();
    jComboBoxImageSize = new javax.swing.JComboBox();
    jPanelButtons = new javax.swing.JPanel();
    jButtonDefault = new javax.swing.JButton();
    jButtonSave = new javax.swing.JButton();

    setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
    setTitle("Preferences");
    getContentPane().setLayout(new javax.swing.BoxLayout(getContentPane(), javax.swing.BoxLayout.PAGE_AXIS));

    jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Observability"));
    jPanel1.setLayout(new java.awt.GridBagLayout());

    jLabelTimeRef.setText("Time reference");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanel1.add(jLabelTimeRef, gridBagConstraints);

    buttonGroupTimeRef.add(jRadioButtonTimeLST);
    jRadioButtonTimeLST.setSelected(true);
    jRadioButtonTimeLST.setText(AsproConstants.TIME_LST);
    jRadioButtonTimeLST.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jRadioButtonTimeRefActionPerformed(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    jPanel1.add(jRadioButtonTimeLST, gridBagConstraints);

    buttonGroupTimeRef.add(jRadioButtonTimeUTC);
    jRadioButtonTimeUTC.setText(AsproConstants.TIME_UTC);
    jRadioButtonTimeUTC.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jRadioButtonTimeRefActionPerformed(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    jPanel1.add(jRadioButtonTimeUTC, gridBagConstraints);

    jLabelCenterNight.setText("Center plot arround night");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanel1.add(jLabelCenterNight, gridBagConstraints);

    buttonGroupTimeAxis.add(jRadioButtonCenterNightYes);
    jRadioButtonCenterNightYes.setSelected(true);
    jRadioButtonCenterNightYes.setText("yes");
    jRadioButtonCenterNightYes.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jRadioButtonCenterNightActionPerformed(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    jPanel1.add(jRadioButtonCenterNightYes, gridBagConstraints);

    buttonGroupTimeAxis.add(jRadioButtonCenterNightNo);
    jRadioButtonCenterNightNo.setText("no");
    jRadioButtonCenterNightNo.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jRadioButtonCenterNightActionPerformed(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
    jPanel1.add(jRadioButtonCenterNightNo, gridBagConstraints);

    getContentPane().add(jPanel1);

    jPanelModelEditor.setBorder(javax.swing.BorderFactory.createTitledBorder("Model Editor"));
    jPanelModelEditor.setLayout(new java.awt.GridBagLayout());

    jLabelPositionStyle.setText("<html>Default style to <br/>edit model positions</htmL>");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    jPanelModelEditor.add(jLabelPositionStyle, gridBagConstraints);

    buttonGroupPositionStyle.add(jRadioButtonXY);
    jRadioButtonXY.setText("x / y (mas)");
    jRadioButtonXY.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jRadioButtonPositionStyleActionPerformed(evt);
      }
    });
    jPanelModelEditor.add(jRadioButtonXY, new java.awt.GridBagConstraints());

    buttonGroupPositionStyle.add(jRadioButtonRhoTheta);
    jRadioButtonRhoTheta.setText("rho (mas) / theta");
    jRadioButtonRhoTheta.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jRadioButtonPositionStyleActionPerformed(evt);
      }
    });
    jPanelModelEditor.add(jRadioButtonRhoTheta, new java.awt.GridBagConstraints());

    getContentPane().add(jPanelModelEditor);

    jPanelModelImage.setBorder(javax.swing.BorderFactory.createTitledBorder("Model Image"));
    jPanelModelImage.setLayout(new java.awt.GridBagLayout());

    jLabelLutTable.setText("LUT table");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelModelImage.add(jLabelLutTable, gridBagConstraints);

    jComboBoxLUT.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jComboBoxLUTActionPerformed(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    jPanelModelImage.add(jComboBoxLUT, gridBagConstraints);

    jLabelImageSize.setText("Image size");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelModelImage.add(jLabelImageSize, gridBagConstraints);

    jComboBoxImageSize.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jComboBoxImageSizeActionPerformed(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    jPanelModelImage.add(jComboBoxImageSize, gridBagConstraints);

    getContentPane().add(jPanelModelImage);

    jButtonDefault.setText("Restore Default Settings");
    jButtonDefault.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonDefaultActionPerformed(evt);
      }
    });
    jPanelButtons.add(jButtonDefault);

    jButtonSave.setText("Save Modifications");
    jButtonSave.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonSaveActionPerformed(evt);
      }
    });
    jPanelButtons.add(jButtonSave);

    getContentPane().add(jPanelButtons);
  }// </editor-fold>//GEN-END:initComponents

    private void jRadioButtonPositionStyleActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButtonPositionStyleActionPerformed
      try {
        // will fire triggerObserversNotification so update() will be called
        this.myPreferences.setPreference(Preferences.MODELEDITOR_PREFERXY, this.jRadioButtonXY.isSelected());
      } catch (PreferencesException pe) {
        logger.log(Level.SEVERE, null, pe);
      }
    }//GEN-LAST:event_jRadioButtonPositionStyleActionPerformed

    private void jButtonSaveActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonSaveActionPerformed
      try {
        this.myPreferences.saveToFile();
      } catch (PreferencesException pe) {
        // this try catch should not be solved here
        // one feedback report could be thrown on error into Preference code
        logger.log(Level.SEVERE, null, pe);
      }
    }//GEN-LAST:event_jButtonSaveActionPerformed

    private void jButtonDefaultActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonDefaultActionPerformed
      this.myPreferences.resetToDefaultPreferences();
    }//GEN-LAST:event_jButtonDefaultActionPerformed

    private void jComboBoxImageSizeActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBoxImageSizeActionPerformed
      try {
        // will fire triggerObserversNotification so update() will be called
        this.myPreferences.setPreference(Preferences.MODEL_IMAGE_SIZE, this.jComboBoxImageSize.getSelectedItem());
      } catch (PreferencesException pe) {
        logger.log(Level.SEVERE, null, pe);
      }
    }//GEN-LAST:event_jComboBoxImageSizeActionPerformed

    private void jComboBoxLUTActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBoxLUTActionPerformed
      try {
        // will fire triggerObserversNotification so update() will be called
        this.myPreferences.setPreference(Preferences.MODEL_IMAGE_LUT, this.jComboBoxLUT.getSelectedItem());
      } catch (PreferencesException pe) {
        logger.log(Level.SEVERE, null, pe);
      }
    }//GEN-LAST:event_jComboBoxLUTActionPerformed

    private void jRadioButtonTimeRefActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButtonTimeRefActionPerformed
      try {
        final String value;
        if (this.jRadioButtonTimeUTC.isSelected()) {
          value = AsproConstants.TIME_UTC;
        } else {
          value = AsproConstants.TIME_LST;
        }

        // will fire triggerObserversNotification so update() will be called
        this.myPreferences.setPreference(Preferences.TIME_REFERENCE, value);
      } catch (PreferencesException pe) {
        logger.log(Level.SEVERE, null, pe);
      }
    }//GEN-LAST:event_jRadioButtonTimeRefActionPerformed

    private void jRadioButtonCenterNightActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButtonCenterNightActionPerformed
      try {
        final String value;
        if (this.jRadioButtonCenterNightYes.isSelected()) {
          value = "true";
        } else {
          value = "false";
        }

        // will fire triggerObserversNotification so update() will be called
        this.myPreferences.setPreference(Preferences.CENTER_NIGHT, value);
      } catch (PreferencesException pe) {
        logger.log(Level.SEVERE, null, pe);
      }
    }//GEN-LAST:event_jRadioButtonCenterNightActionPerformed

  /**
   * Listen to preferences changes
   * @param o Preferences
   * @param arg unused
   */
  public void update(final Observable o, final Object arg) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Preferences updated on : " + this);
    }

    // read prefs to set states of GUI elements
    final boolean preferXyMode = this.myPreferences.getPreferenceAsBoolean(Preferences.MODELEDITOR_PREFERXY);
    this.jRadioButtonXY.setSelected(preferXyMode);
    this.jRadioButtonRhoTheta.setSelected(!preferXyMode);

    this.jComboBoxLUT.setSelectedItem(this.myPreferences.getPreference(Preferences.MODEL_IMAGE_LUT));

    this.jComboBoxImageSize.setSelectedItem(this.myPreferences.getPreferenceAsInt(Preferences.MODEL_IMAGE_SIZE));

    final boolean preferTimeLst = AsproConstants.TIME_LST.equals(this.myPreferences.getTimeReference());
    this.jRadioButtonTimeLST.setSelected(preferTimeLst);
    this.jRadioButtonTimeUTC.setSelected(!preferTimeLst);

    final boolean preferCenterNight = this.myPreferences.isCenterNight();
    this.jRadioButtonCenterNightYes.setSelected(preferCenterNight);
    this.jRadioButtonCenterNightNo.setSelected(!preferCenterNight);

  }
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.ButtonGroup buttonGroupPositionStyle;
  private javax.swing.ButtonGroup buttonGroupTimeAxis;
  private javax.swing.ButtonGroup buttonGroupTimeRef;
  private javax.swing.JButton jButtonDefault;
  private javax.swing.JButton jButtonSave;
  private javax.swing.JComboBox jComboBoxImageSize;
  private javax.swing.JComboBox jComboBoxLUT;
  private javax.swing.JLabel jLabelCenterNight;
  private javax.swing.JLabel jLabelImageSize;
  private javax.swing.JLabel jLabelLutTable;
  private javax.swing.JLabel jLabelPositionStyle;
  private javax.swing.JLabel jLabelTimeRef;
  private javax.swing.JPanel jPanel1;
  private javax.swing.JPanel jPanelButtons;
  private javax.swing.JPanel jPanelModelEditor;
  private javax.swing.JPanel jPanelModelImage;
  private javax.swing.JRadioButton jRadioButtonCenterNightNo;
  private javax.swing.JRadioButton jRadioButtonCenterNightYes;
  private javax.swing.JRadioButton jRadioButtonRhoTheta;
  private javax.swing.JRadioButton jRadioButtonTimeLST;
  private javax.swing.JRadioButton jRadioButtonTimeUTC;
  private javax.swing.JRadioButton jRadioButtonXY;
  // End of variables declaration//GEN-END:variables
}
