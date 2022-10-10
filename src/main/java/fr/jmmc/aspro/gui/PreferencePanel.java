/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.model.TimeRef;
import fr.jmmc.aspro.model.observability.SunTimeInterval.SunType;
import fr.jmmc.aspro.service.pops.BestPopsEstimatorFactory.Algorithm;
import fr.jmmc.aspro.service.pops.Criteria;
import fr.jmmc.jmal.image.ColorModels;
import fr.jmmc.jmal.image.ColorScale;
import fr.jmmc.jmal.image.ImageUtils.ImageInterpolation;
import fr.jmmc.jmcs.data.preference.PreferencesException;
import fr.jmmc.jmcs.gui.component.ComponentResizeAdapter;
import fr.jmmc.oiexplorer.core.gui.IconComboBoxRenderer;
import java.awt.Dimension;
import java.awt.Image;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Arrays;
import java.util.Observable;
import java.util.Observer;
import javax.swing.DefaultComboBoxModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Preferences GUI
 */
public final class PreferencePanel extends javax.swing.JPanel implements Observer {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(PreferencePanel.class.getName());
    /** twilight choices */
    private final static String[] TWILIGHTS = new String[]{"Astronomical (-18°)", "Nautical (-12°)", "Civil (-6°)", "Sun (0°)"};

    /* members */
    /** preference singleton */
    private final Preferences myPreferences = Preferences.getInstance();

    /**
     * Creates a new PreferencePanel
     */
    public PreferencePanel() {
        initComponents();

        postInit();
    }

    /**
     * This method is useful to set the models and specific features of initialized swing components :
     * Update the combo boxes with their models
     */
    private void postInit() {

        // define custom models :
        this.jComboBoxImageSize.setModel(new DefaultComboBoxModel(AsproConstants.IMAGE_SIZES));
        this.jComboBoxLUT.setModel(new DefaultComboBoxModel(ColorModels.getColorModelNames()));
        this.jComboBoxColorScale.setModel(new DefaultComboBoxModel(ColorScale.values()));
        this.jComboBoxInterpolation.setModel(new DefaultComboBoxModel(ImageInterpolation.values()));

        this.jComboBoxBestPopsAlgorithm.setModel(new DefaultComboBoxModel(Algorithm.values()));
        this.jComboBoxBestPopsCriteriaSigma.setModel(new DefaultComboBoxModel(Criteria.values()));
        this.jComboBoxBestPopsCriteriaAverageWeight.setModel(new DefaultComboBoxModel(Criteria.values()));

        this.jComboBoxFastError.setModel(new DefaultComboBoxModel(AsproConstants.FAST_ERROR));
        this.jComboBoxSuperSampling.setModel(new DefaultComboBoxModel(AsproConstants.SUPER_SAMPLING));

        // Custom renderer for LUT:
        this.jComboBoxLUT.setRenderer(new IconComboBoxRenderer() {
            private static final long serialVersionUID = 1L;

            @Override
            protected Image getImage(final String name) {
                return ColorModels.getColorModelImage(name);
            }
        });

        // Set the Aspro Preferences:
        this.chartPreferencesView.setPreferences(myPreferences);

        // register this instance as a Preference Observer :
        this.myPreferences.addObserver(this);

        // update GUI
        update(null, null);

        this.jFieldMinElev.addPropertyChangeListener("value", new PropertyChangeListener() {
            @Override
            public void propertyChange(final PropertyChangeEvent evt) {
                final double minElevNew = ((Number) jFieldMinElev.getValue()).doubleValue();

                if (minElevNew <= 0.0 || minElevNew >= 90.0) {
                    // invalid value :
                    jFieldMinElev.setValue(myPreferences.getPreferenceAsDouble(Preferences.MIN_ELEVATION));
                }
                try {
                    // will fire triggerObserversNotification so update() will be called
                    myPreferences.setPreference(Preferences.MIN_ELEVATION, Double.valueOf(((Number) jFieldMinElev.getValue()).doubleValue()));
                } catch (PreferencesException pe) {
                    logger.error("property failure : ", pe);
                }
            }
        });

        this.jFieldSNRTh.addPropertyChangeListener("value", new PropertyChangeListener() {
            @Override
            public void propertyChange(final PropertyChangeEvent evt) {
                final double snrNew = ((Number) jFieldSNRTh.getValue()).doubleValue();

                if (snrNew <= 0.0) {
                    // invalid value :
                    jFieldSNRTh.setValue(myPreferences.getPreferenceAsDouble(Preferences.OIFITS_SNR_THRESHOLD));
                }
                try {
                    // will fire triggerObserversNotification so update() will be called
                    myPreferences.setPreference(Preferences.OIFITS_SNR_THRESHOLD, Double.valueOf(((Number) jFieldSNRTh.getValue()).doubleValue()));
                } catch (PreferencesException pe) {
                    logger.error("property failure : ", pe);
                }
            }
        });
        
        final Dimension dim = new Dimension(500, 500);
        setMinimumSize(dim);
        addComponentListener(new ComponentResizeAdapter(dim));
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
        buttonGroupFastUserModel = new javax.swing.ButtonGroup();
        buttonGroupNightOnly = new javax.swing.ButtonGroup();
        buttonGroupAddNoise = new javax.swing.ButtonGroup();
        buttonGroupImageNoise = new javax.swing.ButtonGroup();
        buttonGroupGuiRestrictions = new javax.swing.ButtonGroup();
        buttonGroupApodization = new javax.swing.ButtonGroup();
        jScrollPane = new javax.swing.JScrollPane();
        jPanelLayout = new javax.swing.JPanel();
        jPanelObservability = new javax.swing.JPanel();
        jLabelTimeRef = new javax.swing.JLabel();
        jPanelTimeRef = new javax.swing.JPanel();
        jRadioButtonTimeLST = new javax.swing.JRadioButton();
        jRadioButtonTimeUTC = new javax.swing.JRadioButton();
        jRadioButtonTimeLOCAL = new javax.swing.JRadioButton();
        jLabelCenterNight = new javax.swing.JLabel();
        jRadioButtonCenterNightYes = new javax.swing.JRadioButton();
        jRadioButtonCenterNightNo = new javax.swing.JRadioButton();
        jLabelNightOnly = new javax.swing.JLabel();
        jRadioButtonNightOnlyYes = new javax.swing.JRadioButton();
        jRadioButtonNightOnlyNo = new javax.swing.JRadioButton();
        jLabelMinElev = new javax.swing.JLabel();
        jFieldMinElev = new javax.swing.JFormattedTextField();
        jLabelTwilight = new javax.swing.JLabel();
        jComboBoxTwilight = new javax.swing.JComboBox();
        jSeparator = new javax.swing.JSeparator();
        jLabelBestPopsAlgorithm = new javax.swing.JLabel();
        jComboBoxBestPopsAlgorithm = new javax.swing.JComboBox();
        jLabelBestPopsCriteriaSigma = new javax.swing.JLabel();
        jComboBoxBestPopsCriteriaSigma = new javax.swing.JComboBox();
        jLabelBestPopsCriteriaAverageWeight = new javax.swing.JLabel();
        jComboBoxBestPopsCriteriaAverageWeight = new javax.swing.JComboBox();
        jPanelModelEditor = new javax.swing.JPanel();
        jLabelPositionStyle = new javax.swing.JLabel();
        jRadioButtonXY = new javax.swing.JRadioButton();
        jRadioButtonSepPosAngle = new javax.swing.JRadioButton();
        jPanelModelImage = new javax.swing.JPanel();
        jLabelLutTable = new javax.swing.JLabel();
        jComboBoxLUT = new javax.swing.JComboBox();
        jLabelImageSize = new javax.swing.JLabel();
        jComboBoxImageSize = new javax.swing.JComboBox();
        jLabelColorScale = new javax.swing.JLabel();
        jComboBoxColorScale = new javax.swing.JComboBox();
        jLabelImageNoise = new javax.swing.JLabel();
        jRadioButtonImageNoiseYes = new javax.swing.JRadioButton();
        jRadioButtonImageNoiseNo = new javax.swing.JRadioButton();
        jLabelInterpolation = new javax.swing.JLabel();
        jComboBoxInterpolation = new javax.swing.JComboBox();
        jPanelUserModel = new javax.swing.JPanel();
        jLabelFastUserModel = new javax.swing.JLabel();
        jRadioButtonFastUserModelYes = new javax.swing.JRadioButton();
        jRadioButtonFastUserModelNo = new javax.swing.JRadioButton();
        jComboBoxFastError = new javax.swing.JComboBox();
        jLabelApodization = new javax.swing.JLabel();
        jLabelFastError = new javax.swing.JLabel();
        jRadioButtonApodizationYes = new javax.swing.JRadioButton();
        jRadioButtonApodizationNo = new javax.swing.JRadioButton();
        jPanelOIFits = new javax.swing.JPanel();
        jLabelSuperSampling = new javax.swing.JLabel();
        jComboBoxSuperSampling = new javax.swing.JComboBox();
        jLabelAddNoise = new javax.swing.JLabel();
        jRadioButtonAddNoiseYes = new javax.swing.JRadioButton();
        jRadioButtonAddNoiseNo = new javax.swing.JRadioButton();
        jFieldSNRTh = new javax.swing.JFormattedTextField();
        jLabelSNRTh = new javax.swing.JLabel();
        jPanelGui = new javax.swing.JPanel();
        jLabelGuiRestrictions = new javax.swing.JLabel();
        jRadioButtonBypassGuiRestrictionsYes = new javax.swing.JRadioButton();
        jRadioButtonBypassGuiRestrictionsNo = new javax.swing.JRadioButton();
        chartPreferencesView = new fr.jmmc.oiexplorer.core.gui.ChartPreferencesView();
        jPanelCommonPreferencesView = new fr.jmmc.jmcs.gui.component.CommonPreferencesView();

        setLayout(new javax.swing.BoxLayout(this, javax.swing.BoxLayout.LINE_AXIS));

        jPanelLayout.setLayout(new javax.swing.BoxLayout(jPanelLayout, javax.swing.BoxLayout.Y_AXIS));

        jPanelObservability.setBorder(javax.swing.BorderFactory.createTitledBorder("Observability"));
        jPanelObservability.setLayout(new java.awt.GridBagLayout());

        jLabelTimeRef.setText("Time reference");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelObservability.add(jLabelTimeRef, gridBagConstraints);

        buttonGroupTimeRef.add(jRadioButtonTimeLST);
        jRadioButtonTimeLST.setText(TimeRef.LST.getDisplayName());
        jRadioButtonTimeLST.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonTimeLSTjRadioButtonTimeRefActionPerformed(evt);
            }
        });
        jPanelTimeRef.add(jRadioButtonTimeLST);

        buttonGroupTimeRef.add(jRadioButtonTimeUTC);
        jRadioButtonTimeUTC.setText(TimeRef.UTC.getDisplayName());
        jRadioButtonTimeUTC.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonTimeUTCjRadioButtonTimeRefActionPerformed(evt);
            }
        });
        jPanelTimeRef.add(jRadioButtonTimeUTC);

        buttonGroupTimeRef.add(jRadioButtonTimeLOCAL);
        jRadioButtonTimeLOCAL.setText(TimeRef.LOCAL.getDisplayName());
        jRadioButtonTimeLOCAL.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonTimeLOCALjRadioButtonTimeRefActionPerformed(evt);
            }
        });
        jPanelTimeRef.add(jRadioButtonTimeLOCAL);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_START;
        gridBagConstraints.weightx = 0.5;
        jPanelObservability.add(jPanelTimeRef, gridBagConstraints);

        jLabelCenterNight.setText("Center plot around night");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 0.2;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelObservability.add(jLabelCenterNight, gridBagConstraints);

        buttonGroupTimeAxis.add(jRadioButtonCenterNightYes);
        jRadioButtonCenterNightYes.setText("yes");
        jRadioButtonCenterNightYes.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonCenterNightActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 0.4;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelObservability.add(jRadioButtonCenterNightYes, gridBagConstraints);

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
        gridBagConstraints.weightx = 0.4;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelObservability.add(jRadioButtonCenterNightNo, gridBagConstraints);

        jLabelNightOnly.setText("Night only");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelObservability.add(jLabelNightOnly, gridBagConstraints);

        buttonGroupNightOnly.add(jRadioButtonNightOnlyYes);
        jRadioButtonNightOnlyYes.setText("yes");
        jRadioButtonNightOnlyYes.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonNightOnlyActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelObservability.add(jRadioButtonNightOnlyYes, gridBagConstraints);

        buttonGroupNightOnly.add(jRadioButtonNightOnlyNo);
        jRadioButtonNightOnlyNo.setText("no");
        jRadioButtonNightOnlyNo.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonNightOnlyActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelObservability.add(jRadioButtonNightOnlyNo, gridBagConstraints);

        jLabelMinElev.setText("Default min. Elevation");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelObservability.add(jLabelMinElev, gridBagConstraints);

        jFieldMinElev.setColumns(2);
        jFieldMinElev.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0"))));
        jFieldMinElev.setName("jFieldMinElev"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_START;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 2);
        jPanelObservability.add(jFieldMinElev, gridBagConstraints);

        jLabelTwilight.setText("Twilight used as Night limit");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelObservability.add(jLabelTwilight, gridBagConstraints);

        jComboBoxTwilight.setModel(new DefaultComboBoxModel(TWILIGHTS));
        jComboBoxTwilight.setSelectedItem(TWILIGHTS[0]);
        jComboBoxTwilight.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBoxTwilightActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 4, 2);
        jPanelObservability.add(jComboBoxTwilight, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 6, 0, 6);
        jPanelObservability.add(jSeparator, gridBagConstraints);

        jLabelBestPopsAlgorithm.setText("Best PoPs algorithm");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelObservability.add(jLabelBestPopsAlgorithm, gridBagConstraints);

        jComboBoxBestPopsAlgorithm.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBoxBestPopsAlgorithmActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(4, 2, 2, 2);
        jPanelObservability.add(jComboBoxBestPopsAlgorithm, gridBagConstraints);

        jLabelBestPopsCriteriaSigma.setText("Gaussian sigma");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelObservability.add(jLabelBestPopsCriteriaSigma, gridBagConstraints);

        jComboBoxBestPopsCriteriaSigma.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBoxBestPopsCriteriaSigmaActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelObservability.add(jComboBoxBestPopsCriteriaSigma, gridBagConstraints);

        jLabelBestPopsCriteriaAverageWeight.setText("Average weight % Min");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelObservability.add(jLabelBestPopsCriteriaAverageWeight, gridBagConstraints);

        jComboBoxBestPopsCriteriaAverageWeight.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBoxBestPopsCriteriaAverageWeightActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelObservability.add(jComboBoxBestPopsCriteriaAverageWeight, gridBagConstraints);

        jPanelLayout.add(jPanelObservability);

        jPanelModelEditor.setBorder(javax.swing.BorderFactory.createTitledBorder("Model Editor"));
        jPanelModelEditor.setLayout(new java.awt.GridBagLayout());

        jLabelPositionStyle.setText("<html>Default style to<br>edit model positions</html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 6);
        jPanelModelEditor.add(jLabelPositionStyle, gridBagConstraints);

        buttonGroupPositionStyle.add(jRadioButtonXY);
        jRadioButtonXY.setText("x / y (mas)");
        jRadioButtonXY.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonXYjRadioButtonPositionStyleActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelModelEditor.add(jRadioButtonXY, gridBagConstraints);

        buttonGroupPositionStyle.add(jRadioButtonSepPosAngle);
        jRadioButtonSepPosAngle.setText("sep. (mas) / pos. angle");
        jRadioButtonSepPosAngle.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonSepPosAnglejRadioButtonPositionStyleActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelModelEditor.add(jRadioButtonSepPosAngle, gridBagConstraints);

        jPanelLayout.add(jPanelModelEditor);

        jPanelModelImage.setBorder(javax.swing.BorderFactory.createTitledBorder("Model Image"));
        jPanelModelImage.setLayout(new java.awt.GridBagLayout());

        jLabelLutTable.setText("LUT table");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelModelImage.add(jLabelLutTable, gridBagConstraints);

        jComboBoxLUT.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBoxLUTActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 2, 4);
        jPanelModelImage.add(jComboBoxLUT, gridBagConstraints);

        jLabelImageSize.setText("Image size");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelModelImage.add(jLabelImageSize, gridBagConstraints);

        jComboBoxImageSize.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBoxImageSizeActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 2, 4);
        jPanelModelImage.add(jComboBoxImageSize, gridBagConstraints);

        jLabelColorScale.setText("Color scale");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelModelImage.add(jLabelColorScale, gridBagConstraints);

        jComboBoxColorScale.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBoxColorScaleActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 2, 4);
        jPanelModelImage.add(jComboBoxColorScale, gridBagConstraints);

        jLabelImageNoise.setText("Add error noise to image");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelModelImage.add(jLabelImageNoise, gridBagConstraints);

        buttonGroupImageNoise.add(jRadioButtonImageNoiseYes);
        jRadioButtonImageNoiseYes.setText("yes");
        jRadioButtonImageNoiseYes.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonImageNoiseActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelModelImage.add(jRadioButtonImageNoiseYes, gridBagConstraints);

        buttonGroupImageNoise.add(jRadioButtonImageNoiseNo);
        jRadioButtonImageNoiseNo.setText("no");
        jRadioButtonImageNoiseNo.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonImageNoiseActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelModelImage.add(jRadioButtonImageNoiseNo, gridBagConstraints);

        jLabelInterpolation.setText("Interpolation");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 4);
        jPanelModelImage.add(jLabelInterpolation, gridBagConstraints);

        jComboBoxInterpolation.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBoxInterpolationActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 2, 4);
        jPanelModelImage.add(jComboBoxInterpolation, gridBagConstraints);

        jPanelLayout.add(jPanelModelImage);

        jPanelUserModel.setBorder(javax.swing.BorderFactory.createTitledBorder("User Model"));
        jPanelUserModel.setLayout(new java.awt.GridBagLayout());

        jLabelFastUserModel.setText("Fast mode (optimize image)");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelUserModel.add(jLabelFastUserModel, gridBagConstraints);

        buttonGroupFastUserModel.add(jRadioButtonFastUserModelYes);
        jRadioButtonFastUserModelYes.setText("yes");
        jRadioButtonFastUserModelYes.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonFastUserModelActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelUserModel.add(jRadioButtonFastUserModelYes, gridBagConstraints);

        buttonGroupFastUserModel.add(jRadioButtonFastUserModelNo);
        jRadioButtonFastUserModelNo.setText("no");
        jRadioButtonFastUserModelNo.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonFastUserModelActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelUserModel.add(jRadioButtonFastUserModelNo, gridBagConstraints);

        jComboBoxFastError.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBoxFastErrorActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 0.1;
        jPanelUserModel.add(jComboBoxFastError, gridBagConstraints);

        jLabelApodization.setText("Apodization (telescope)");
        jLabelApodization.setToolTipText("");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelUserModel.add(jLabelApodization, gridBagConstraints);

        jLabelFastError.setText("Fast mode Error (%)");
        jLabelFastError.setToolTipText("");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelUserModel.add(jLabelFastError, gridBagConstraints);

        buttonGroupApodization.add(jRadioButtonApodizationYes);
        jRadioButtonApodizationYes.setText("yes");
        jRadioButtonApodizationYes.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonApodizationActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        jPanelUserModel.add(jRadioButtonApodizationYes, gridBagConstraints);

        buttonGroupApodization.add(jRadioButtonApodizationNo);
        jRadioButtonApodizationNo.setText("no");
        jRadioButtonApodizationNo.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonApodizationActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        jPanelUserModel.add(jRadioButtonApodizationNo, gridBagConstraints);

        jPanelLayout.add(jPanelUserModel);

        jPanelOIFits.setBorder(javax.swing.BorderFactory.createTitledBorder("OIFits data"));
        jPanelOIFits.setLayout(new java.awt.GridBagLayout());

        jLabelSuperSampling.setText("<html>Supersampling model<br>in spectral channels</html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelOIFits.add(jLabelSuperSampling, gridBagConstraints);

        jComboBoxSuperSampling.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBoxSuperSamplingActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 0.1;
        jPanelOIFits.add(jComboBoxSuperSampling, gridBagConstraints);

        jLabelAddNoise.setText("Add error noise to data");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelOIFits.add(jLabelAddNoise, gridBagConstraints);

        buttonGroupAddNoise.add(jRadioButtonAddNoiseYes);
        jRadioButtonAddNoiseYes.setText("yes");
        jRadioButtonAddNoiseYes.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonAddNoiseActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelOIFits.add(jRadioButtonAddNoiseYes, gridBagConstraints);

        buttonGroupAddNoise.add(jRadioButtonAddNoiseNo);
        jRadioButtonAddNoiseNo.setText("no");
        jRadioButtonAddNoiseNo.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonAddNoiseActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelOIFits.add(jRadioButtonAddNoiseNo, gridBagConstraints);

        jFieldSNRTh.setColumns(5);
        jFieldSNRTh.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0.0"))));
        jFieldSNRTh.setName("jFieldMinElev"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 2);
        jPanelOIFits.add(jFieldSNRTh, gridBagConstraints);

        jLabelSNRTh.setText("SNR Threshold (V2)");
        jLabelSNRTh.setToolTipText("All related values below this threshold will be flagged out (V2, T3...)");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelOIFits.add(jLabelSNRTh, gridBagConstraints);

        jPanelLayout.add(jPanelOIFits);

        jPanelGui.setBorder(javax.swing.BorderFactory.createTitledBorder("Gui settings"));
        jPanelGui.setLayout(new java.awt.GridBagLayout());

        jLabelGuiRestrictions.setText("Bypass GUI restrictions");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 0.2;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 6);
        jPanelGui.add(jLabelGuiRestrictions, gridBagConstraints);

        buttonGroupGuiRestrictions.add(jRadioButtonBypassGuiRestrictionsYes);
        jRadioButtonBypassGuiRestrictionsYes.setText("yes");
        jRadioButtonBypassGuiRestrictionsYes.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonBypassGuiRestrictionsActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelGui.add(jRadioButtonBypassGuiRestrictionsYes, gridBagConstraints);

        buttonGroupGuiRestrictions.add(jRadioButtonBypassGuiRestrictionsNo);
        jRadioButtonBypassGuiRestrictionsNo.setText("no");
        jRadioButtonBypassGuiRestrictionsNo.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButtonBypassGuiRestrictionsActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelGui.add(jRadioButtonBypassGuiRestrictionsNo, gridBagConstraints);

        jPanelLayout.add(jPanelGui);
        jPanelLayout.add(chartPreferencesView);
        jPanelLayout.add(jPanelCommonPreferencesView);

        jScrollPane.setViewportView(jPanelLayout);

        add(jScrollPane);
    }// </editor-fold>//GEN-END:initComponents

    private void jRadioButtonTimeLSTjRadioButtonTimeRefActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButtonTimeLSTjRadioButtonTimeRefActionPerformed
        try {
            final String value;
            if (this.jRadioButtonTimeLOCAL.isSelected()) {
                value = TimeRef.LOCAL.getDisplayName();
            } else if (this.jRadioButtonTimeUTC.isSelected()) {
                value = TimeRef.UTC.getDisplayName();
            } else {
                value = TimeRef.LST.getDisplayName();
            }

            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.TIME_REFERENCE, value);
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jRadioButtonTimeLSTjRadioButtonTimeRefActionPerformed

    private void jRadioButtonTimeUTCjRadioButtonTimeRefActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButtonTimeUTCjRadioButtonTimeRefActionPerformed
        try {
            final String value;
            if (this.jRadioButtonTimeLOCAL.isSelected()) {
                value = TimeRef.LOCAL.getDisplayName();
            } else if (this.jRadioButtonTimeUTC.isSelected()) {
                value = TimeRef.UTC.getDisplayName();
            } else {
                value = TimeRef.LST.getDisplayName();
            }

            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.TIME_REFERENCE, value);
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jRadioButtonTimeUTCjRadioButtonTimeRefActionPerformed

    private void jRadioButtonTimeLOCALjRadioButtonTimeRefActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButtonTimeLOCALjRadioButtonTimeRefActionPerformed
        try {
            final String value;
            if (this.jRadioButtonTimeLOCAL.isSelected()) {
                value = TimeRef.LOCAL.getDisplayName();
            } else if (this.jRadioButtonTimeUTC.isSelected()) {
                value = TimeRef.UTC.getDisplayName();
            } else {
                value = TimeRef.LST.getDisplayName();
            }

            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.TIME_REFERENCE, value);
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jRadioButtonTimeLOCALjRadioButtonTimeRefActionPerformed

    private void jComboBoxTwilightActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBoxTwilightActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.TWILIGHT_NIGHT, getTwilight((String) this.jComboBoxTwilight.getSelectedItem()).toString());
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jComboBoxTwilightActionPerformed

    private void jComboBoxBestPopsAlgorithmActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBoxBestPopsAlgorithmActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.BEST_POPS_ALGORITHM, this.jComboBoxBestPopsAlgorithm.getSelectedItem().toString());
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jComboBoxBestPopsAlgorithmActionPerformed

    private void jComboBoxBestPopsCriteriaSigmaActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBoxBestPopsCriteriaSigmaActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.BEST_POPS_CRITERIA_SIGMA, this.jComboBoxBestPopsCriteriaSigma.getSelectedItem().toString());
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jComboBoxBestPopsCriteriaSigmaActionPerformed

    private void jComboBoxBestPopsCriteriaAverageWeightActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBoxBestPopsCriteriaAverageWeightActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.BEST_POPS_CRITERIA_AVERAGE_WEIGHT, this.jComboBoxBestPopsCriteriaAverageWeight.getSelectedItem().toString());
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jComboBoxBestPopsCriteriaAverageWeightActionPerformed

    private void jRadioButtonXYjRadioButtonPositionStyleActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButtonXYjRadioButtonPositionStyleActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.MODELEDITOR_PREFERXY, Boolean.valueOf(this.jRadioButtonXY.isSelected()));
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jRadioButtonXYjRadioButtonPositionStyleActionPerformed

    private void jRadioButtonSepPosAnglejRadioButtonPositionStyleActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButtonSepPosAnglejRadioButtonPositionStyleActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.MODELEDITOR_PREFERXY, Boolean.valueOf(this.jRadioButtonXY.isSelected()));
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jRadioButtonSepPosAnglejRadioButtonPositionStyleActionPerformed

    private void jComboBoxLUTActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBoxLUTActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.MODEL_IMAGE_LUT, this.jComboBoxLUT.getSelectedItem());
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jComboBoxLUTActionPerformed

    private void jComboBoxImageSizeActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBoxImageSizeActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.MODEL_IMAGE_SIZE, this.jComboBoxImageSize.getSelectedItem());
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jComboBoxImageSizeActionPerformed

    private void jComboBoxColorScaleActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBoxColorScaleActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.MODEL_IMAGE_SCALE, this.jComboBoxColorScale.getSelectedItem().toString());
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jComboBoxColorScaleActionPerformed

    private void jComboBoxSuperSamplingActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBoxSuperSamplingActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.OIFITS_SUPER_SAMPLING, this.jComboBoxSuperSampling.getSelectedItem());
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jComboBoxSuperSamplingActionPerformed

    private void jRadioButtonCenterNightActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButtonCenterNightActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.CENTER_NIGHT, Boolean.valueOf(this.jRadioButtonCenterNightYes.isSelected()));
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jRadioButtonCenterNightActionPerformed

    private void jRadioButtonNightOnlyActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButtonNightOnlyActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.ONLY_NIGHT, Boolean.valueOf(this.jRadioButtonNightOnlyYes.isSelected()));
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jRadioButtonNightOnlyActionPerformed

    private void jRadioButtonImageNoiseActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButtonImageNoiseActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.MODEL_IMAGE_NOISE, Boolean.valueOf(this.jRadioButtonImageNoiseYes.isSelected()));
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jRadioButtonImageNoiseActionPerformed

    private void jRadioButtonFastUserModelActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButtonFastUserModelActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.MODEL_USER_FAST, Boolean.valueOf(this.jRadioButtonFastUserModelYes.isSelected()));
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jRadioButtonFastUserModelActionPerformed

    private void jRadioButtonAddNoiseActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButtonAddNoiseActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.OIFITS_ADD_NOISE, Boolean.valueOf(this.jRadioButtonAddNoiseYes.isSelected()));
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jRadioButtonAddNoiseActionPerformed

    private void jRadioButtonBypassGuiRestrictionsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButtonBypassGuiRestrictionsActionPerformed
        // Inverse logical:
        final boolean useGuiRestrictions = !this.jRadioButtonBypassGuiRestrictionsYes.isSelected();
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.GUI_RESTRICTIONS, Boolean.valueOf(useGuiRestrictions));
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jRadioButtonBypassGuiRestrictionsActionPerformed

    private void jComboBoxInterpolationActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBoxInterpolationActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.MODEL_IMAGE_INTERPOLATION, this.jComboBoxInterpolation.getSelectedItem().toString());
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jComboBoxInterpolationActionPerformed

    private void jComboBoxFastErrorActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBoxFastErrorActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.MODEL_USER_FAST_ERROR, (Double) this.jComboBoxFastError.getSelectedItem());
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jComboBoxFastErrorActionPerformed

    private void jRadioButtonApodizationActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButtonApodizationActionPerformed
        try {
            // will fire triggerObserversNotification so update() will be called
            this.myPreferences.setPreference(Preferences.MODEL_USER_APODIZE, Boolean.valueOf(this.jRadioButtonApodizationYes.isSelected()));
        } catch (PreferencesException pe) {
            logger.error("property failure : ", pe);
        }
    }//GEN-LAST:event_jRadioButtonApodizationActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroupAddNoise;
    private javax.swing.ButtonGroup buttonGroupApodization;
    private javax.swing.ButtonGroup buttonGroupFastUserModel;
    private javax.swing.ButtonGroup buttonGroupGuiRestrictions;
    private javax.swing.ButtonGroup buttonGroupImageNoise;
    private javax.swing.ButtonGroup buttonGroupNightOnly;
    private javax.swing.ButtonGroup buttonGroupPositionStyle;
    private javax.swing.ButtonGroup buttonGroupTimeAxis;
    private javax.swing.ButtonGroup buttonGroupTimeRef;
    private fr.jmmc.oiexplorer.core.gui.ChartPreferencesView chartPreferencesView;
    private javax.swing.JComboBox jComboBoxBestPopsAlgorithm;
    private javax.swing.JComboBox jComboBoxBestPopsCriteriaAverageWeight;
    private javax.swing.JComboBox jComboBoxBestPopsCriteriaSigma;
    private javax.swing.JComboBox jComboBoxColorScale;
    private javax.swing.JComboBox jComboBoxFastError;
    private javax.swing.JComboBox jComboBoxImageSize;
    private javax.swing.JComboBox jComboBoxInterpolation;
    private javax.swing.JComboBox jComboBoxLUT;
    private javax.swing.JComboBox jComboBoxSuperSampling;
    private javax.swing.JComboBox jComboBoxTwilight;
    private javax.swing.JFormattedTextField jFieldMinElev;
    private javax.swing.JFormattedTextField jFieldSNRTh;
    private javax.swing.JLabel jLabelAddNoise;
    private javax.swing.JLabel jLabelApodization;
    private javax.swing.JLabel jLabelBestPopsAlgorithm;
    private javax.swing.JLabel jLabelBestPopsCriteriaAverageWeight;
    private javax.swing.JLabel jLabelBestPopsCriteriaSigma;
    private javax.swing.JLabel jLabelCenterNight;
    private javax.swing.JLabel jLabelColorScale;
    private javax.swing.JLabel jLabelFastError;
    private javax.swing.JLabel jLabelFastUserModel;
    private javax.swing.JLabel jLabelGuiRestrictions;
    private javax.swing.JLabel jLabelImageNoise;
    private javax.swing.JLabel jLabelImageSize;
    private javax.swing.JLabel jLabelInterpolation;
    private javax.swing.JLabel jLabelLutTable;
    private javax.swing.JLabel jLabelMinElev;
    private javax.swing.JLabel jLabelNightOnly;
    private javax.swing.JLabel jLabelPositionStyle;
    private javax.swing.JLabel jLabelSNRTh;
    private javax.swing.JLabel jLabelSuperSampling;
    private javax.swing.JLabel jLabelTimeRef;
    private javax.swing.JLabel jLabelTwilight;
    private fr.jmmc.jmcs.gui.component.CommonPreferencesView jPanelCommonPreferencesView;
    private javax.swing.JPanel jPanelGui;
    private javax.swing.JPanel jPanelLayout;
    private javax.swing.JPanel jPanelModelEditor;
    private javax.swing.JPanel jPanelModelImage;
    private javax.swing.JPanel jPanelOIFits;
    private javax.swing.JPanel jPanelObservability;
    private javax.swing.JPanel jPanelTimeRef;
    private javax.swing.JPanel jPanelUserModel;
    private javax.swing.JRadioButton jRadioButtonAddNoiseNo;
    private javax.swing.JRadioButton jRadioButtonAddNoiseYes;
    private javax.swing.JRadioButton jRadioButtonApodizationNo;
    private javax.swing.JRadioButton jRadioButtonApodizationYes;
    private javax.swing.JRadioButton jRadioButtonBypassGuiRestrictionsNo;
    private javax.swing.JRadioButton jRadioButtonBypassGuiRestrictionsYes;
    private javax.swing.JRadioButton jRadioButtonCenterNightNo;
    private javax.swing.JRadioButton jRadioButtonCenterNightYes;
    private javax.swing.JRadioButton jRadioButtonFastUserModelNo;
    private javax.swing.JRadioButton jRadioButtonFastUserModelYes;
    private javax.swing.JRadioButton jRadioButtonImageNoiseNo;
    private javax.swing.JRadioButton jRadioButtonImageNoiseYes;
    private javax.swing.JRadioButton jRadioButtonNightOnlyNo;
    private javax.swing.JRadioButton jRadioButtonNightOnlyYes;
    private javax.swing.JRadioButton jRadioButtonSepPosAngle;
    private javax.swing.JRadioButton jRadioButtonTimeLOCAL;
    private javax.swing.JRadioButton jRadioButtonTimeLST;
    private javax.swing.JRadioButton jRadioButtonTimeUTC;
    private javax.swing.JRadioButton jRadioButtonXY;
    private javax.swing.JScrollPane jScrollPane;
    private javax.swing.JSeparator jSeparator;
    // End of variables declaration//GEN-END:variables

    /**
     * Listen to preferences changes
     * @param o Preferences
     * @param arg unused
     */
    @Override
    public void update(final Observable o, final Object arg) {
        logger.debug("Preferences updated on : {}", this);

        // read prefs to set states of GUI elements
        // Observability:
        final TimeRef timeRef = TimeRef.findByDisplayName(this.myPreferences.getPreference(Preferences.TIME_REFERENCE));
        switch (timeRef) {
            default:
            case LST:
                this.jRadioButtonTimeLST.setSelected(true);
                break;
            case UTC:
                this.jRadioButtonTimeUTC.setSelected(true);
                break;
            case LOCAL:
                this.jRadioButtonTimeLOCAL.setSelected(true);
                break;
        }

        final boolean preferCenterNight = this.myPreferences.getPreferenceAsBoolean(Preferences.CENTER_NIGHT);
        this.jRadioButtonCenterNightYes.setSelected(preferCenterNight);
        this.jRadioButtonCenterNightNo.setSelected(!preferCenterNight);

        final boolean preferNightOnly = this.myPreferences.getPreferenceAsBoolean(Preferences.ONLY_NIGHT);
        this.jRadioButtonNightOnlyYes.setSelected(preferNightOnly);
        this.jRadioButtonNightOnlyNo.setSelected(!preferNightOnly);

        this.jFieldMinElev.setValue(this.myPreferences.getPreferenceAsDouble(Preferences.MIN_ELEVATION));

        this.jComboBoxTwilight.setSelectedItem(getTwilight(this.myPreferences.getTwilightAsNightLimit()));

        // Observability (best pops):
        this.jComboBoxBestPopsAlgorithm.setSelectedItem(this.myPreferences.getBestPopsAlgorithm());
        this.jComboBoxBestPopsCriteriaSigma.setSelectedItem(this.myPreferences.getBestPopsCriteriaSigma());
        this.jComboBoxBestPopsCriteriaAverageWeight.setSelectedItem(this.myPreferences.getBestPopsCriteriaAverageWeight());

        // Model editor:
        final boolean preferXyMode = this.myPreferences.getPreferenceAsBoolean(Preferences.MODELEDITOR_PREFERXY);
        this.jRadioButtonXY.setSelected(preferXyMode);
        this.jRadioButtonSepPosAngle.setSelected(!preferXyMode);

        // Model image:
        this.jComboBoxImageSize.setSelectedItem(this.myPreferences.getPreferenceAsInt(Preferences.MODEL_IMAGE_SIZE));
        this.jComboBoxLUT.setSelectedItem(this.myPreferences.getPreference(Preferences.MODEL_IMAGE_LUT));
        this.jComboBoxColorScale.setSelectedItem(this.myPreferences.getImageColorScale());
        this.jComboBoxInterpolation.setSelectedItem(this.myPreferences.getImageInterpolation());

        final boolean preferImageNoise = this.myPreferences.getPreferenceAsBoolean(Preferences.MODEL_IMAGE_NOISE);
        this.jRadioButtonImageNoiseYes.setSelected(preferImageNoise);
        this.jRadioButtonImageNoiseNo.setSelected(!preferImageNoise);

        // User model:
        final boolean useFastUserModel = this.myPreferences.isFastUserModel();
        this.jRadioButtonFastUserModelYes.setSelected(useFastUserModel);
        this.jRadioButtonFastUserModelNo.setSelected(!useFastUserModel);
        this.jComboBoxFastError.setSelectedItem(this.myPreferences.getPreferenceAsDouble(Preferences.MODEL_USER_FAST_ERROR));
        final boolean useApodization = this.myPreferences.isDoUserModelApodization();
        this.jRadioButtonApodizationYes.setSelected(useApodization);
        this.jRadioButtonApodizationNo.setSelected(!useApodization);

        // OIFits:
        this.jComboBoxSuperSampling.setSelectedItem(this.myPreferences.getPreferenceAsInt(Preferences.OIFITS_SUPER_SAMPLING));

        final boolean preferAddNoise = this.myPreferences.getPreferenceAsBoolean(Preferences.OIFITS_ADD_NOISE);
        this.jRadioButtonAddNoiseYes.setSelected(preferAddNoise);
        this.jRadioButtonAddNoiseNo.setSelected(!preferAddNoise);
        
        this.jFieldSNRTh.setValue(this.myPreferences.getPreferenceAsDouble(Preferences.OIFITS_SNR_THRESHOLD));

        // Gui settings:
        // Inverse logical:
        final boolean bypassGuiRestrictions = !this.myPreferences.getPreferenceAsBoolean(Preferences.GUI_RESTRICTIONS);
        this.jRadioButtonBypassGuiRestrictionsYes.setSelected(bypassGuiRestrictions);
        this.jRadioButtonBypassGuiRestrictionsNo.setSelected(!bypassGuiRestrictions);
    }

    /**
     * Return the string choice corresponding to the given SunType instance
     * @param type SunType instance
     * @return string choice
     */
    private String getTwilight(final SunType type) {
        switch (type) {
            default:
            case Night:
                return TWILIGHTS[0];
            case AstronomicalTwilight:
                return TWILIGHTS[1];
            case NauticalTwilight:
                return TWILIGHTS[2];
            case CivilTwilight:
                return TWILIGHTS[3];
        }
    }

    /**
     * Return the SunType instance corresponding to the given string choice
     * @param choice string choice
     * @return SunType instance
     */
    private SunType getTwilight(final String choice) {
        int pos = -1;
        for (int i = 0; i < TWILIGHTS.length; i++) {
            if (TWILIGHTS[i].equals(choice)) {
                pos = i;
                break;
            }
        }
        if (pos == -1) {
            logger.warn("choice[{}] not found in {}", choice, Arrays.toString(TWILIGHTS));
        }
        switch (pos) {
            default:
            case 0:
                return SunType.Night;
            case 1:
                return SunType.AstronomicalTwilight;
            case 2:
                return SunType.NauticalTwilight;
            case 3:
                return SunType.CivilTwilight;
        }
    }

}
