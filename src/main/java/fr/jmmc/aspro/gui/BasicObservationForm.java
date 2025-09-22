/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import edu.dartmouth.AstroSkyCalc;
import edu.dartmouth.AstroSkyCalcObservation;
import edu.dartmouth.JSkyCalc;
import edu.dartmouth.Site;
import fr.jmmc.aspro.Aspro2;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.gui.action.GetStarAction;
import fr.jmmc.aspro.gui.util.PopListRenderer;
import fr.jmmc.aspro.gui.util.TargetList;
import fr.jmmc.aspro.gui.util.TargetListRenderer;
import fr.jmmc.aspro.gui.util.TargetRenderer;
import fr.jmmc.aspro.gui.util.WindWidget;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.TargetImporter;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.WarningMessage;
import fr.jmmc.aspro.model.WarningMessage.Level;
import fr.jmmc.aspro.model.event.ObservabilityEvent;
import fr.jmmc.aspro.model.event.ObservationEvent;
import fr.jmmc.aspro.model.event.ObservationListener;
import fr.jmmc.aspro.model.event.TargetSelectionEvent;
import fr.jmmc.aspro.model.event.UpdateObservationEvent;
import fr.jmmc.aspro.model.event.WarningContainerEvent;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.observability.PopCombination;
import fr.jmmc.aspro.model.oi.BestPopsMode;
import fr.jmmc.aspro.model.oi.FocalInstrumentConfiguration;
import fr.jmmc.aspro.model.oi.FocalInstrumentConfigurationChoice;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerConfigurationChoice;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.ObservationCollection;
import fr.jmmc.aspro.model.oi.ObservationContext;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.ObservationVariant;
import fr.jmmc.aspro.model.oi.Pop;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.jmal.star.EditableStarResolverWidget;
import fr.jmmc.jmal.star.StarResolver;
import fr.jmmc.jmal.star.StarResolverListener;
import fr.jmmc.jmal.star.StarResolverResult;
import fr.jmmc.jmcs.gui.component.GenericListModel;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.component.SearchPanel;
import fr.jmmc.jmcs.gui.util.ResourceImage;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.gui.util.SwingUtils.ComponentSizeVariant;
import fr.jmmc.jmcs.logging.LogbackGui;
import fr.jmmc.jmcs.logging.LoggingService;
import fr.jmmc.jmcs.util.CollectionUtils;
import fr.jmmc.jmcs.util.ImageUtils;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.ObjectUtils;
import fr.jmmc.jmcs.util.StringUtils;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Vector;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListSelectionModel;
import javax.swing.JComboBox;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JSpinner.DateEditor;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.text.JTextComponent;
import javax.swing.text.NumberFormatter;
import javax.xml.datatype.XMLGregorianCalendar;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This form allows the user to select main observation settings : date, interferometer, configuration, stations and targets ...
 * @author bourgesl
 */
public final class BasicObservationForm extends javax.swing.JPanel implements ChangeListener, ActionListener,
                                                                              StarResolverListener<StarResolverResult>, ObservationListener {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(BasicObservationForm.class.getName());
    /** Logger */
    private static final Logger _warningLogger = LoggingService.getInstance().getLogger(AsproConstants.ASPRO_WARNING_LOG);
    /** flag to log a stack trace in method updateObservation() to detect multiple calls */
    private final static boolean DEBUG_UPDATE_EVENT = false;
    /** blanking value to indicate that PoPs are in use but in multi-configuration */
    private final static String POPS_MULTI_CONF = "PoPs_MULTI_CONF";
    /** blanking value to indicate that PoPs are defined manually by the user */
    private final static String POPS_MANUAL = "[Manual]";
    /** blanking value to indicate that PoPs are determined using best PoPs algorithm */
    private final static String POPS_AUTO = "[Auto]";
    /** BestPopsMode ALL */
    private final static String POPS_MODE_ALL = "All";
    /** BestPopsMode SELECTED */
    private final static String POPS_MODE_SEL = "Sel";
    /** maximum of warnings shown in tooltips */
    private final static int MAX_TOOLTIP_WARNINGS = 100;
    /** fixed font */
    private final static Font FIXED_FONT = new Font(Font.MONOSPACED, Font.PLAIN, SwingUtils.adjustUISize(12));
    /** fixed font (smaller) */
    private final static Font FIXED_FONT_SMALLER = new Font(Font.MONOSPACED, Font.PLAIN, SwingUtils.adjustUISize(11));
    /** configuration manager */
    private final static ConfigurationManager cm = ConfigurationManager.getInstance();
    /** observation manager */
    private final static ObservationManager om = ObservationManager.getInstance();

    /* members */
    /** flag to enable / disable the automatic update of the observation when any swing component changes */
    private boolean doAutoUpdateObservation = true;
    /** flag to enable / disable the automatic selection check of the target list */
    private boolean doAutoCheckTargets = true;
    /** flag to enable / disable the automatic selection check of the instrument configuration */
    private boolean doAutoCheckConfigurations = true;
    /** default list model for the PoPs combo box */
    private final ComboBoxModel<String> defaultModelPops = new DefaultComboBoxModel<>(new String[]{POPS_MANUAL, POPS_AUTO});
    /** Wind widget */
    private WindWidget windWidget = null;
    /** Dedicated panel for target quick search */
    private SearchPanel _searchPanel = null;

    /* cached data */
    /** loaded observation setup (Interferometer + Instrument + Config) */
    private String loadedObsSetup = null;
    /** current selected target to avoid empty list selection */
    private Target currentTarget = null;
    /** current selected instrument configuration (S1 S2 E1) to avoid empty list selection */
    private String currentInsConfiguration = null;
    /** last Pop config given by the interferometer configuration (12455) */
    private String lastConfPopConfig = null;
    /** last instrument configuration (S1 S2 E1) compatible with manual PoPs */
    private String lastInsConfCompatibleWithPopConfig = null;
    /** last instrument configuration key ('S1 S2 E1@T/F') used by the best PoPs algorithm */
    private String lastInsConfKeyForBestPops = null;

    // Fixed Pops handling (2022.3):
    /** current Pops (from observability data) as (station = Pop) */
    private Map<String, Pop> popsCurrentMap = null;
    /** fixed Pops (from observability data) as (station = Pop) */
    private TreeMap<String, Pop> popsFixed = null;
    /** last stations for Pops used */
    private List<String> lastStationsForPops = null;
    /** default stations for Pops */
    private final List<String> defStationsForPops = new ArrayList<>();
    /** station labels (ordered like the interferometer pops list) */
    private final List<JLabel> popsStationLabels = new ArrayList<>();
    /** pops combo boxes (ordered like the interferometer pops list) */
    private final List<JComboBox> popsComboBoxes = new ArrayList<>();
    /** map of station to list index in previous fields */
    private final Map<String, Integer> stationToPopsWidgetIndex = new HashMap<>();

    /** Creates new form BasicObservationForm */
    public BasicObservationForm() {
        initComponents();
        postInit();
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
        jPanelTargetsLeft = new javax.swing.JPanel();
        starSearchField = new EditableStarResolverWidget(true);
        jScrollPaneTargets = new javax.swing.JScrollPane();
        jListTargets = new TargetList();
        jButtonGetStar = new javax.swing.JButton();
        jButtonTargetEditor = new javax.swing.JButton();
        jButtonDeleteTarget = new javax.swing.JButton();
        jButtonSkyCalc = new javax.swing.JButton();
        jPanelTargetsRight = new javax.swing.JPanel();
        jPanelMain = new javax.swing.JPanel();
        jPanelObsLeft = new javax.swing.JPanel();
        jLabelInterferometer = new javax.swing.JLabel();
        jComboBoxInterferometer = new javax.swing.JComboBox();
        jLabelPeriod = new javax.swing.JLabel();
        jComboBoxInterferometerConfiguration = new javax.swing.JComboBox();
        jLabelInstrument = new javax.swing.JLabel();
        jComboBoxInstrument = new javax.swing.JComboBox();
        jLabelPops = new javax.swing.JLabel();
        jTextPoPs = new JFormattedTextField(getPopsFormatter());
        jComboBoxPops = new javax.swing.JComboBox<>();
        jButtonPopsSet = new javax.swing.JButton();
        jButtonPopsClear = new javax.swing.JButton();
        jPanelFixedPops = new javax.swing.JPanel();
        jLabelFixedPops = new javax.swing.JLabel();
        jPanelFixedPopsGen = new javax.swing.JPanel();
        jLabelCurrentPops = new javax.swing.JLabel();
        jTextFieldCurrentPops = new javax.swing.JTextField();
        jPanelFixedPopsSpacer = new javax.swing.JPanel();
        jPanelObsRight = new javax.swing.JPanel();
        jPanelObsBottom = new javax.swing.JPanel();
        jComboBoxPopsMode = new javax.swing.JComboBox<>();
        jPanelConfigurations = new javax.swing.JPanel();
        jScrollPaneInstrumentConfigurations = new javax.swing.JScrollPane();
        jListInstrumentConfigurations = createConfigurationList();
        jPanelConfLeft = new javax.swing.JPanel();
        jPanelConfRight = new javax.swing.JPanel();
        jPanelOptions = new javax.swing.JPanel();
        jCheckBoxNightLimit = new javax.swing.JCheckBox();
        jLabelDate = new javax.swing.JLabel();
        jDateSpinner = new javax.swing.JSpinner();
        jLabelMinElev = new javax.swing.JLabel();
        jFieldMinElev = new javax.swing.JFormattedTextField();
        jCheckBoxWind = new javax.swing.JCheckBox();
        jPanelOptBottom = new javax.swing.JPanel();
        jPanelConsLeft = new javax.swing.JPanel();
        jPanelStatus = new javax.swing.JPanel();
        jLabelState = new javax.swing.JLabel();
        jLabelStatus = new javax.swing.JLabel();

        setMinimumSize(new java.awt.Dimension(700, 100));
        setPreferredSize(new java.awt.Dimension(900, getPreferredHeight()));
        setLayout(new java.awt.GridBagLayout());

        jPanelTargets.setBorder(javax.swing.BorderFactory.createTitledBorder("Targets"));
        jPanelTargets.setName("jPanelTargets"); // NOI18N
        jPanelTargets.setLayout(new java.awt.GridBagLayout());

        jPanelTargetsLeft.setMinimumSize(new java.awt.Dimension(4, 4));
        jPanelTargetsLeft.setPreferredSize(new java.awt.Dimension(4, 4));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.01;
        jPanelTargets.add(jPanelTargetsLeft, gridBagConstraints);

        starSearchField.setToolTipText("<html>\nEnter targets here :<br>\nTarget identifier (CDS Simbad or JMMC GetStar service)<br>\nor RA / DEC coordinates (J2000) with an optional identifier:<br>\n'H:M:S [+/-]D:M:S [identifier]'<br>\n<b>Use the semicolon separator ';' for multiple targets</b>\n</html>");
        starSearchField.setName("starSearchField"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 0.8;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 6, 6);
        jPanelTargets.add(starSearchField, gridBagConstraints);

        jListTargets.setToolTipText("Target list : use the Simbad field to enter your targets\n");
        jListTargets.setName("jListTargets"); // NOI18N
        jListTargets.setVisibleRowCount(2);
        jListTargets.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
            public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
                jListTargetsValueChanged(evt);
            }
        });
        jScrollPaneTargets.setViewportView(jListTargets);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.8;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 4);
        jPanelTargets.add(jScrollPaneTargets, gridBagConstraints);

        jButtonGetStar.setText("GetStar");
        jButtonGetStar.setToolTipText("Open JMMC GetStar web page for this target");
        jButtonGetStar.setMargin(new java.awt.Insets(0, 4, 0, 4));
        jButtonGetStar.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonGetStarActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 6, 0);
        jPanelTargets.add(jButtonGetStar, gridBagConstraints);

        jButtonTargetEditor.setText("Editor");
        jButtonTargetEditor.setToolTipText("Open the Target Editor");
        jButtonTargetEditor.setMargin(new java.awt.Insets(0, 4, 0, 4));
        jButtonTargetEditor.setName("jButtonTargetEditor"); // NOI18N
        jButtonTargetEditor.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonTargetEditorActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        jPanelTargets.add(jButtonTargetEditor, gridBagConstraints);

        jButtonDeleteTarget.setIcon(ImageUtils.loadResourceIcon("fr/jmmc/aspro/gui/icons/delete.png"));
        jButtonDeleteTarget.setToolTipText("delete the selected target(s)");
        jButtonDeleteTarget.setMargin(new java.awt.Insets(0, 4, 0, 4));
        jButtonDeleteTarget.setName("jButtonDeleteTarget"); // NOI18N
        jButtonDeleteTarget.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonDeleteTargetActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 2);
        jPanelTargets.add(jButtonDeleteTarget, gridBagConstraints);

        jButtonSkyCalc.setText("Sky");
        jButtonSkyCalc.setToolTipText("Open the JSkyCalc Window");
        jButtonSkyCalc.setMargin(new java.awt.Insets(0, 4, 0, 4));
        jButtonSkyCalc.setName("jButtonSkyCalc"); // NOI18N
        jButtonSkyCalc.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonSkyCalcActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        jPanelTargets.add(jButtonSkyCalc, gridBagConstraints);

        jPanelTargetsRight.setMinimumSize(new java.awt.Dimension(4, 4));
        jPanelTargetsRight.setPreferredSize(new java.awt.Dimension(4, 4));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.01;
        jPanelTargets.add(jPanelTargetsRight, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.3;
        gridBagConstraints.weighty = 1.0;
        add(jPanelTargets, gridBagConstraints);

        jPanelMain.setBorder(javax.swing.BorderFactory.createTitledBorder("Main settings"));
        jPanelMain.setName("jPanelMain"); // NOI18N
        jPanelMain.setLayout(new java.awt.GridBagLayout());

        jPanelObsLeft.setMinimumSize(new java.awt.Dimension(4, 4));
        jPanelObsLeft.setPreferredSize(new java.awt.Dimension(4, 4));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.01;
        jPanelMain.add(jPanelObsLeft, gridBagConstraints);

        jLabelInterferometer.setLabelFor(jComboBoxInterferometer);
        jLabelInterferometer.setText("Interferometer");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 4);
        jPanelMain.add(jLabelInterferometer, gridBagConstraints);

        jComboBoxInterferometer.setName("jComboBoxInterferometer"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 0.5;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        jPanelMain.add(jComboBoxInterferometer, gridBagConstraints);

        jLabelPeriod.setText("Period");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 4);
        jPanelMain.add(jLabelPeriod, gridBagConstraints);

        jComboBoxInterferometerConfiguration.setName("jComboBoxInterferometerConfiguration"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        jPanelMain.add(jComboBoxInterferometerConfiguration, gridBagConstraints);

        jLabelInstrument.setLabelFor(jComboBoxInstrument);
        jLabelInstrument.setText("Instrument");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 4);
        jPanelMain.add(jLabelInstrument, gridBagConstraints);

        jComboBoxInstrument.setName("jComboBoxInstrument"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        jPanelMain.add(jComboBoxInstrument, gridBagConstraints);

        jLabelPops.setLabelFor(jTextPoPs);
        jLabelPops.setText("PoPs");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 4);
        jPanelMain.add(jLabelPops, gridBagConstraints);

        jTextPoPs.setColumns(6);
        jTextPoPs.setToolTipText("<html>\ndefine a specific PoPs combination (PoP 1 to 5) by giving the list of PoP numbers<br>\nin the same order than stations of the selected base line. For example:<ul>\n<li>CLASSIC (2T) with baseline S1-S2<br>'34' means PoP3 on S1 and PoP4 on S2</li>\n<li>MIRC (4T) with baseline S1-S2-E1-W2<br>'1255' means PoP1 on S1, PoP2 on S2 and Pop5 on E1 and W2</li>\n</ul>\n<b>If you leave this field blank, ASPRO 2 will compute the 'best PoP' combination<br>\nmaximizing the observability of your complete list of targets</b>\n<br>The Pop value '8' is the wildcard value (any Pop)\n</html>");
        jTextPoPs.setMinimumSize(new java.awt.Dimension(60, 20));
        jTextPoPs.setName("jTextPoPs"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanelMain.add(jTextPoPs, gridBagConstraints);

        jComboBoxPops.setMaximumRowCount(6);
        jComboBoxPops.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] { "[Manual]", "[Auto]" }));
        jComboBoxPops.setName("jComboBoxPops"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 2);
        jPanelMain.add(jComboBoxPops, gridBagConstraints);

        jButtonPopsSet.setText("set");
        jButtonPopsSet.setMargin(new java.awt.Insets(0, 0, 0, 0));
        jButtonPopsSet.setName("jButtonPopsSet"); // NOI18N
        jButtonPopsSet.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonPopsSetActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanelMain.add(jButtonPopsSet, gridBagConstraints);

        jButtonPopsClear.setText("clear");
        jButtonPopsClear.setToolTipText("Reset all fixed Pops (any)");
        jButtonPopsClear.setMargin(new java.awt.Insets(0, 0, 0, 0));
        jButtonPopsClear.setName("jButtonPopsClear"); // NOI18N
        jButtonPopsClear.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonPopsClearActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 6;
        gridBagConstraints.gridy = 3;
        jPanelMain.add(jButtonPopsClear, gridBagConstraints);

        jPanelFixedPops.setLayout(new java.awt.GridBagLayout());

        jLabelFixedPops.setText("Fixed:");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        jPanelFixedPops.add(jLabelFixedPops, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanelFixedPops.add(jPanelFixedPopsGen, gridBagConstraints);

        jLabelCurrentPops.setText("Current:");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        jPanelFixedPops.add(jLabelCurrentPops, gridBagConstraints);

        jTextFieldCurrentPops.setEditable(false);
        jTextFieldCurrentPops.setColumns(36);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_START;
        gridBagConstraints.weightx = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanelFixedPops.add(jTextFieldCurrentPops, gridBagConstraints);

        jPanelFixedPopsSpacer.setMinimumSize(new java.awt.Dimension(4, 4));
        jPanelFixedPopsSpacer.setPreferredSize(new java.awt.Dimension(4, 4));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.01;
        jPanelFixedPops.add(jPanelFixedPopsSpacer, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_START;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        jPanelMain.add(jPanelFixedPops, gridBagConstraints);

        jPanelObsRight.setMinimumSize(new java.awt.Dimension(4, 4));
        jPanelObsRight.setPreferredSize(new java.awt.Dimension(4, 4));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 7;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.01;
        jPanelMain.add(jPanelObsRight, gridBagConstraints);

        jPanelObsBottom.setPreferredSize(new java.awt.Dimension(1, 1));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 8;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weighty = 0.01;
        jPanelMain.add(jPanelObsBottom, gridBagConstraints);

        jComboBoxPopsMode.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] { "All", "Sel" }));
        jComboBoxPopsMode.setToolTipText("<html>Best Pops algorithm applied to:\n<ul>\n<li><b>All</b>: all targets</li>\n<li><b>Sel</b>: selected targets</li>\n</ul></html>");
        jComboBoxPopsMode.setName("jComboBoxPopsMode"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 2);
        jPanelMain.add(jComboBoxPopsMode, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.1;
        add(jPanelMain, gridBagConstraints);

        jPanelConfigurations.setBorder(javax.swing.BorderFactory.createTitledBorder("Configuration(s)"));
        jPanelConfigurations.setName("jPanelConfigurations"); // NOI18N
        jPanelConfigurations.setLayout(new java.awt.GridBagLayout());

        jScrollPaneInstrumentConfigurations.setPreferredSize(new java.awt.Dimension(150, 50));

        jListInstrumentConfigurations.setModel(new javax.swing.AbstractListModel() {
            String[] strings = { "UT1 UT2 UT3 UT4" };
            public int getSize() { return strings.length; }
            public Object getElementAt(int i) { return strings[i]; }
        });
        jListInstrumentConfigurations.setName("jListInstrumentConfigurations"); // NOI18N
        jListInstrumentConfigurations.setVisibleRowCount(2);
        jListInstrumentConfigurations.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
            public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
                jListInstrumentConfigurationsValueChanged(evt);
            }
        });
        jScrollPaneInstrumentConfigurations.setViewportView(jListInstrumentConfigurations);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanelConfigurations.add(jScrollPaneInstrumentConfigurations, gridBagConstraints);

        jPanelConfLeft.setMinimumSize(new java.awt.Dimension(6, 6));
        jPanelConfLeft.setPreferredSize(new java.awt.Dimension(4, 4));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.01;
        jPanelConfigurations.add(jPanelConfLeft, gridBagConstraints);

        jPanelConfRight.setMinimumSize(new java.awt.Dimension(6, 6));
        jPanelConfRight.setPreferredSize(new java.awt.Dimension(4, 4));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.01;
        jPanelConfigurations.add(jPanelConfRight, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.3;
        gridBagConstraints.weighty = 0.5;
        add(jPanelConfigurations, gridBagConstraints);

        jPanelOptions.setBorder(javax.swing.BorderFactory.createTitledBorder("Constraints"));
        jPanelOptions.setName("jPanelOptions"); // NOI18N
        jPanelOptions.setLayout(new java.awt.GridBagLayout());

        jCheckBoxNightLimit.setText("Night restriction");
        jCheckBoxNightLimit.setHorizontalTextPosition(javax.swing.SwingConstants.LEADING);
        jCheckBoxNightLimit.setIconTextGap(6);
        jCheckBoxNightLimit.setName("jCheckBoxNightLimit"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 4);
        jPanelOptions.add(jCheckBoxNightLimit, gridBagConstraints);

        jLabelDate.setHorizontalAlignment(javax.swing.SwingConstants.RIGHT);
        jLabelDate.setLabelFor(jDateSpinner);
        jLabelDate.setText("Date");
        jLabelDate.setToolTipText("<html>the given date is used to determine the <b>coming night</b>\n<br/>at the observatory in the [DD; DD+1] range.\n<br/>for example, '2014/4/4' corresponds to the night \n<br/>between April 4th and 5th.</html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 4);
        jPanelOptions.add(jLabelDate, gridBagConstraints);

        jDateSpinner.setModel(new javax.swing.SpinnerDateModel());
        jDateSpinner.setEditor(new javax.swing.JSpinner.DateEditor(jDateSpinner, "yyyy/MM/dd"));
        jDateSpinner.setName("jDateSpinner"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.ipadx = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_START;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 6);
        jPanelOptions.add(jDateSpinner, gridBagConstraints);

        jLabelMinElev.setLabelFor(jFieldMinElev);
        jLabelMinElev.setText("Min. Elevation");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 4);
        jPanelOptions.add(jLabelMinElev, gridBagConstraints);

        jFieldMinElev.setColumns(2);
        jFieldMinElev.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0"))));
        jFieldMinElev.setName("jFieldMinElev"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_START;
        gridBagConstraints.weightx = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 2);
        jPanelOptions.add(jFieldMinElev, gridBagConstraints);

        jCheckBoxWind.setText("Wind");
        jCheckBoxWind.setHorizontalTextPosition(javax.swing.SwingConstants.LEADING);
        jCheckBoxWind.setIconTextGap(6);
        jCheckBoxWind.setName("jCheckBoxWind"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 4);
        jPanelOptions.add(jCheckBoxWind, gridBagConstraints);

        jPanelOptBottom.setMinimumSize(new java.awt.Dimension(1, 1));
        jPanelOptBottom.setPreferredSize(new java.awt.Dimension(1, 1));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weighty = 0.01;
        jPanelOptions.add(jPanelOptBottom, gridBagConstraints);

        jPanelConsLeft.setMinimumSize(new java.awt.Dimension(6, 6));
        jPanelConsLeft.setPreferredSize(new java.awt.Dimension(4, 4));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.01;
        jPanelOptions.add(jPanelConsLeft, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.1;
        gridBagConstraints.weighty = 0.8;
        add(jPanelOptions, gridBagConstraints);

        jPanelStatus.setName("jPanelStatus"); // NOI18N
        jPanelStatus.setLayout(new java.awt.GridBagLayout());

        jLabelState.setText("Status: ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 2, 0);
        jPanelStatus.add(jLabelState, gridBagConstraints);

        jLabelStatus.setText("Ok");
        jLabelStatus.setName("jLabelStatus"); // NOI18N
        jLabelStatus.setPreferredSize(new java.awt.Dimension(32, 32));
        jLabelStatus.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jLabelStatusMouseClicked(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 2);
        jPanelStatus.add(jLabelStatus, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weighty = 0.2;
        add(jPanelStatus, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

  private void jButtonSkyCalcActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonSkyCalcActionPerformed
      final Target selectedTarget = getSelectedTarget();
      if (selectedTarget != null) {
          final ObservationSetting observation = om.getMainObservation();
          final InterferometerDescription interferometer = observation.getInterferometerConfiguration().getInterferometerConfiguration().getInterferometer();

          final Site site = AstroSkyCalc.createSite(interferometer.getName(), interferometer.getPosSph());

          final List<Target> displayTargets = om.getDisplayTargets();

          final int size = displayTargets.size();
          final String[] name = new String[size];
          final String[] ra = new String[size];
          final String[] dec = new String[size];

          for (int i = 0; i < size; i++) {
              final Target target = displayTargets.get(i);
              name[i] = target.getName();

              // convert RA/DEC in HH:MM:SS.sss or DD:MM:SS.sss :
              final String[] raDec = AstroSkyCalcObservation.toString(target.getRADeg(), target.getDECDeg());

              ra[i] = raDec[0];
              dec[i] = raDec[1];
          }

          JSkyCalc.showJSkyCalc(site, name, ra, dec, selectedTarget.getName(), observation.getWhen().getDate());
      }
  }//GEN-LAST:event_jButtonSkyCalcActionPerformed

    private void jButtonPopsSetActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonPopsSetActionPerformed
        if (popsCurrentMap != null) {
            // disable the automatic update observation :
            final boolean prevAutoUpdateObservation = setAutoUpdateObservation(false);
            try {
                // Define fixed Pops combo boxes (only Pops used):
                setFixedPopsComboBox(popsCurrentMap);
            } finally {
                // restore the automatic update observation :
                setAutoUpdateObservation(prevAutoUpdateObservation);
            }
            // group multiple calls into a single observation update event :
            fireObservationUpdateEvent();
        }
    }//GEN-LAST:event_jButtonPopsSetActionPerformed

    private void jButtonPopsClearActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonPopsClearActionPerformed
        // disable the automatic update observation :
        final boolean prevAutoUpdateObservation = setAutoUpdateObservation(false);
        try {
            // Reset all fixed Pops combo boxes:
            for (JComboBox comboBox : popsComboBoxes) {
                comboBox.setSelectedItem(Pop.ANY_POP);
            }
        } finally {
            // restore the automatic update observation :
            setAutoUpdateObservation(prevAutoUpdateObservation);
        }
        // group multiple calls into a single observation update event :
        fireObservationUpdateEvent();
    }//GEN-LAST:event_jButtonPopsClearActionPerformed

    private void jButtonGetStarActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonGetStarActionPerformed
        String targetId = starSearchField.getRealText();

        if (StringUtils.isEmpty(targetId)) {
            targetId = null;

            final Target target = this.currentTarget;
            if (target != null) {
                targetId = target.getName();

                if (StringUtils.isEmpty(targetId)) {
                    targetId = null;
                }
            }
        } else {
            // convert ';' separator (aspro2) by ',' (GetStar):
            targetId = targetId.replace(';', ',');
        }
        if (targetId != null) {
            logger.info("Querying GetStar for identifier '{}'...", targetId);

            GetStarAction.openGetStarInBrowser(targetId);
        }
    }//GEN-LAST:event_jButtonGetStarActionPerformed

    /**
     * Process the remove target action
     * @param evt action event
     */
    private void jButtonDeleteTargetActionPerformed(java.awt.event.ActionEvent evt) {

        final List<Target> selectedTargets = getSelectedTargets();
        final int size = selectedTargets.size();

        if (size != 0) {
            // Single selection mode:
            if (size == 1) {
                final Target selectedTarget = selectedTargets.get(0);

                if (om.isCalibrator(selectedTarget)) {
                    if (MessagePane.showConfirmMessage(jButtonDeleteTarget,
                            "Do you want to delete the calibrator target [" + selectedTarget.getName() + "] and all associations ?")) {

                        // update the data model and fire change events :
                        om.removeCalibrator(selectedTarget);
                    }
                } else if (MessagePane.showConfirmMessage(jButtonDeleteTarget,
                        "Do you want to delete the science target [" + selectedTarget.getName() + "] ?")) {

                    // update the data model and fire change events :
                    om.removeTarget(selectedTarget);
                }
            } else {
                // Count calibrators:
                int calibrators = 0;
                for (Target selectedTarget : selectedTargets) {
                    if (om.isCalibrator(selectedTarget)) {
                        calibrators++;
                    }
                }

                if (MessagePane.showConfirmMessage(jButtonDeleteTarget,
                        (calibrators == size) ? "Do you want to delete all selected calibrator targets and associations ?"
                                : ((calibrators != 0) ? "Do you want to delete all selected science & calibrator targets and associations ?"
                                        : "Do you want to delete all selected science targets ?"))) {

                    // update the data model and fire change events :
                    om.removeTargetAndCalibrators(selectedTargets);
                }
            }
        }
    }

    /**
     * Handle click on the target editor button: show the target editor
     * @param ae unused
     */
    private void jButtonTargetEditorActionPerformed(ActionEvent ae) {
        showTargetEditor();
    }

    /**
     * Handle the instrument selection
     * @param lse unused
     */
    private void jListInstrumentConfigurationsValueChanged(ListSelectionEvent lse) {
        processInstrumentConfigurationValueChanged(lse);
    }

    /**
     * Handle the (single) target selection
     * @param lse unused
     */
    private void jListTargetsValueChanged(ListSelectionEvent lse) {
        processTargetValueChanged(lse);
    }

    /**
     * Handle click on the status panel: show warning log console
     * @param me unused
     */
    private void jLabelStatusMouseClicked(MouseEvent me) {
        LogbackGui.showLogConsoleForLogger(AsproConstants.ASPRO_WARNING_LOG);
    }

    /**
     * Open the target editor using the selected target
     */
    public void showTargetEditor() {
        if (isTargetEditable()) {
            final Target target = getSelectedTarget();

            if (target != null) {
                final String selectedTab = (Aspro2.getInstance().getSettingPanel().isSelectedTabUsingTargetModel())
                        ? TargetEditorDialog.TAB_MODELS : TargetEditorDialog.TAB_TARGETS;

                // show model editor :
                TargetEditorDialog.showEditor(target.getName(), selectedTab);
            }
        }
    }

    /**
     * Return the Pops custom formatter : number format that accepts null values
     * @return number formatter
     */
    private static NumberFormatter getPopsFormatter() {
        final NumberFormatter nf = new NumberFormatter(new DecimalFormat("######")) {
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
        nf.setValueClass(Integer.class);
        // reject invalid characters (digits only)
        nf.setAllowsInvalid(false);
        nf.setCommitsOnValidEdit(false);
        return nf;
    }

    /**
     * This method is useful to set the models and specific features of initialized swing components.
     * Add the star search field and refresh content of the combo boxes.
     * Finally update the observation according to the form state
     */
    private void postInit() {
        // Search Panel
        _searchPanel = new SearchPanel(new TargetListSearchPanelDelegate(jListTargets));

        // register the StarResolverListener for Simbad:
        starSearchField.setListener(StarResolverResult.class, this);
        // register the StarResolverListener for GetStar:
        starSearchField.setListener(String.class, GetStarAction.GETSTAR_LISTENER);

        // update component models :
        final DateEditor de = (DateEditor) jDateSpinner.getEditor();

        // custom focus listener to let the user change the day field by default :
        de.getTextField().addFocusListener(new FocusAdapter() {
            @Override
            public void focusGained(final FocusEvent fe) {
                if (fe.getSource() instanceof JTextComponent) {
                    final JTextComponent textComponent = ((JTextComponent) fe.getSource());

                    SwingUtils.invokeLaterEDT(new Runnable() {
                        @Override
                        public void run() {
                            final int last = textComponent.getDocument().getLength();
                            if (last >= 2) {
                                // select the day field to force the spinner to use it
                                textComponent.setCaretPosition(last - 2);
                                textComponent.moveCaretPosition(last);
                            }
                        }
                    });
                }
            }

            @Override
            public void focusLost(final FocusEvent fe) {
                // nothing to do
            }
        });

        // define change listeners :
        jDateSpinner.addChangeListener(this);
        jComboBoxInterferometer.addActionListener(this);
        jComboBoxInterferometerConfiguration.addActionListener(this);
        jComboBoxInstrument.addActionListener(this);
        jComboBoxPops.addActionListener(this);
        jComboBoxPopsMode.addActionListener(this);

        jTextPoPs.addPropertyChangeListener("value", new PropertyChangeListener() {
            @Override
            public void propertyChange(final PropertyChangeEvent evt) {
                jTextPoPsPropertyChange(evt);
            }
        });

        jFieldMinElev.addPropertyChangeListener("value", new PropertyChangeListener() {
            @Override
            public void propertyChange(final PropertyChangeEvent evt) {
                final double minElevNew = ((Number) jFieldMinElev.getValue()).doubleValue();

                if (minElevNew < 0d || minElevNew >= 90d) {
                    // invalid value :
                    jFieldMinElev.setValue(Preferences.getInstance().getPreferenceAsDouble(Preferences.MIN_ELEVATION));
                }
                fireObservationUpdateEvent();
            }
        });

        jCheckBoxNightLimit.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(final ItemEvent e) {
                updateWindRestriction();
                fireObservationUpdateEvent();
            }
        });

        // define interferometer names:
        updateComboInterferometer();

        // reset status :
        resetStatus();

        windWidget = WindWidget.create();

        final GridBagConstraints gridBagConstraints = new GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 4; // 4 rows (including spacer)
        gridBagConstraints.fill = GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 2);
        jPanelOptions.add(windWidget, gridBagConstraints);

        windWidget.addPropertyChangeListener(WindWidget.PROPERTY_VALUE, new PropertyChangeListener() {
            @Override
            public void propertyChange(final PropertyChangeEvent pe) {
                fireObservationUpdateEvent();
            }
        });

        jCheckBoxWind.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(final ItemEvent e) {
                windWidget.setEnabled(jCheckBoxWind.isSelected());
                fireObservationUpdateEvent();
            }
        });

        jCheckBoxWind.setSelected(false);
        windWidget.setEnabled(false);

        // Adjust fonts:
        jLabelStatus.setFont(new Font(Font.DIALOG, Font.BOLD, SwingUtils.adjustUISize(18)));

        jListTargets.setFont(FIXED_FONT);
        jListInstrumentConfigurations.setFont(FIXED_FONT);
        jComboBoxPops.setFont(FIXED_FONT);
        jTextPoPs.setFont(FIXED_FONT);
        jTextFieldCurrentPops.setFont(FIXED_FONT_SMALLER);

        // use small variant:
        SwingUtils.adjustSize(this.jButtonGetStar, ComponentSizeVariant.small);
        SwingUtils.adjustSize(this.jButtonTargetEditor, ComponentSizeVariant.small);
        SwingUtils.adjustSize(this.jButtonDeleteTarget, ComponentSizeVariant.small);
        SwingUtils.adjustSize(this.jButtonSkyCalc, ComponentSizeVariant.small);

        SwingUtils.adjustSize(this.jComboBoxPopsMode, ComponentSizeVariant.small);
        SwingUtils.adjustSize(this.jButtonPopsSet, ComponentSizeVariant.small);
        SwingUtils.adjustSize(this.jButtonPopsClear, ComponentSizeVariant.small);

        // update button UI:
        SwingUtilities.updateComponentTreeUI(this);
    }

    /**
     * Process the date spinner change event.
     * Update the observation according to the form state
     * @param ce change event
     */
    @Override
    public void stateChanged(final ChangeEvent ce) {
        if (ce.getSource() == jDateSpinner) {
            if (logger.isDebugEnabled()) {
                logger.debug("Date changed: {}", jDateSpinner.getModel().getValue());
            }
            fireObservationUpdateEvent();
        }
    }

    /**
     * Refresh the list of interferometer names : depends on the configuration changes
     */
    private void updateComboInterferometer() {
        jComboBoxInterferometer.setModel(new DefaultComboBoxModel(cm.getInterferometerNames()));
    }

    /**
     * Refresh the list of interferometer configurations : depends on the chosen interferometer
     */
    private void updateComboInterferometerConfiguration() {
        final Vector<String> v = cm.getInterferometerConfigurationNames((String) jComboBoxInterferometer.getSelectedItem());
        jComboBoxInterferometerConfiguration.setModel(new DefaultComboBoxModel(v));
        final boolean visible = (v.size() > 1);
        jLabelPeriod.setVisible(visible);
        jComboBoxInterferometerConfiguration.setVisible(visible);
    }

    /**
     * Refresh the list of instruments : depends on the chosen interferometer configuration
     */
    private void updateComboInstrument() {
        final Object oldValue = jComboBoxInstrument.getSelectedItem();

        final Vector<String> v = cm.getInterferometerInstrumentNames((String) jComboBoxInterferometerConfiguration.getSelectedItem());
        jComboBoxInstrument.setModel(new DefaultComboBoxModel(v));

        // restore previous selected item :
        if (oldValue != null) {
            jComboBoxInstrument.setSelectedItem(oldValue);
        }
    }

    /**
     * Refresh the list of instrument configurations : depends on the chosen instrument (also the interferometer configuration)
     */
    private void updateComboInstrumentConfiguration() {
        final String configurationName = (String) jComboBoxInterferometerConfiguration.getSelectedItem();
        final String instrumentAlias = (String) jComboBoxInstrument.getSelectedItem();

        final Vector<String> names = cm.getInstrumentConfigurationNames(configurationName, instrumentAlias);

        final Object[] oldValues = getInstrumentConfigurations();

        // disable the automatic selection check of the instrument configuration :
        final boolean prevAutoCheckConfigurations = setAutoCheckConfigurations(false);
        try {
            jListInstrumentConfigurations.setModel(new GenericListModel<String>(names));

            // define alternative names for tooltips:
            ((ConfigurationJList) jListInstrumentConfigurations).setAltNames(
                    cm.getInstrumentConfigurationDisplayAltNames(configurationName, instrumentAlias)
            );

            FocalInstrumentConfiguration insConf = null;

            // Fix alternatives for previous selected item(s):
            for (int i = 0; i < oldValues.length; i++) {
                final String oldStations = (String) oldValues[i];

                if (!names.contains(oldStations)) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("Missing prev conf {}", oldStations);
                    }
                    if (insConf == null) {
                        insConf = cm.getInterferometerInstrumentConfiguration(configurationName, instrumentAlias);
                    }
                    // insConf can not be null now:
                    final String stationIds = ConfigurationManager.findInstrumentConfigurationStations(insConf, oldStations);
                    if (logger.isDebugEnabled()) {
                        logger.debug("Fixed conf {}", stationIds);
                    }
                    oldValues[i] = (stationIds != null) ? stationIds : "";
                }
            }

            // restore previous selected item(s) :
            selectInstrumentConfigurations(oldValues);

        } finally {
            // restore the automatic selection check of the instrument configuration :
            setAutoCheckConfigurations(prevAutoCheckConfigurations);
        }
        // ensure one configuration is selected :
        checkInstrumentConfigurationSelection();
    }

    /**
     * Return the list model of the instrument configuration list
     * @return list model
     */
    @SuppressWarnings("unchecked")
    private GenericListModel<String> getInstrumentConfigurationModel() {
        return (GenericListModel<String>) jListInstrumentConfigurations.getModel();
    }

    /**
     * Refresh the target list
     */
    private void updateListTargets() {
        final Target selectedTarget = getSelectedTarget();

        final List<Target> displayTargets = om.getDisplayTargets();
        final TargetUserInformations targetUserInfos = om.getTargetUserInfos();

        // disable the automatic selection check of the target list :
        final boolean prevAutoCheckTargets = setAutoCheckTargets(false);
        try {
            jListTargets.setModel(new GenericListModel<Target>(displayTargets));
            ((TargetList) this.jListTargets).setTargetUserInfos(targetUserInfos);
            jListTargets.setCellRenderer(new TargetListRenderer(new TargetRenderer(targetUserInfos)));

            // restore previous selected item :
            if (selectedTarget != null) {
                // may use an old Target instance (modified since):
                jListTargets.setSelectedValue(selectedTarget, true);
            }

            if (isTargetEditable()) {
                // disable buttons if the target list is empty :
                jButtonDeleteTarget.setEnabled(!displayTargets.isEmpty());
                jButtonTargetEditor.setEnabled(!displayTargets.isEmpty());
            }

            // (Dis)enable Find widgets according to data availability
            _searchPanel.enableMenus(!displayTargets.isEmpty());

        } finally {
            // restore the automatic selection check of the target list :
            setAutoCheckTargets(prevAutoCheckTargets);
        }
        // ensure one target is selected :
        checkTargetSelection();

        if (logger.isDebugEnabled()) {
            logger.debug("jListTargets updated: {}", getSelectedTarget());
        }
    }

    /**
     * Return the currently selected target
     * @return target
     */
    public Target getSelectedTarget() {
        return currentTarget;
    }

    /**
     * Return the currently selected targets
     * @return target list
     */
    public List<Target> getSelectedTargets() {
        final ListSelectionModel lsm = jListTargets.getSelectionModel();

        final int iMin = lsm.getMinSelectionIndex();
        final int iMax = lsm.getMaxSelectionIndex();

        if ((iMin < 0) || (iMax < 0)) {
            return Collections.emptyList();
        }

        final ListModel model = jListTargets.getModel();
        final List<Target> selectedItems = new ArrayList<Target>(iMax - iMin);
        for (int i = iMin; i <= iMax; i++) {
            if (lsm.isSelectedIndex(i)) {
                selectedItems.add((Target) model.getElementAt(i));
            }
        }
        return selectedItems;
    }

    /**
     * Called whenever the target selection changes.
     * @param e the event that characterizes the change.
     */
    private void processTargetValueChanged(final ListSelectionEvent e) {
        // skip events when the user selection is adjusting :
        if (e.getValueIsAdjusting()) {
            return;
        }

        final List<Target> selectedTargets = getSelectedTargets();

        // ensure at least one item is selected :
        if (selectedTargets.isEmpty()) {
            checkTargetSelection();
            return;
        }

        logger.debug("Selected targets: {}", selectedTargets);

        // check if the current selected target changed:
        if ((currentTarget == null) || om.isSelectedTargetsChanged(selectedTargets)) {
            // memorize the first selected item :
            currentTarget = selectedTargets.get(0);

            // handle single selection :
            logger.debug("Selected Target changed: {}", currentTarget);

            // update observation :
            om.fireTargetSelectionChanged(selectedTargets);
        }
    }

    /**
     * Check if the selected target is empty, then restore the last selected target
     * or select the first target
     */
    private void checkTargetSelection() {
        // check if the automatic configuration check flag is enabled :
        if (doAutoCheckTargets) {
            checkListSelection(jListTargets, getSelectedTarget());
        }
    }

    /**
     * Update the selected target in the target list
     * @param target selected target (not null)
     */
    private void showSelectedTarget(final Target target) {
        final String targetName = target.getName();

        if (logger.isDebugEnabled()) {
            logger.debug("showSelectedTarget: {}", targetName);
        }

        // update current target to avoid event loop:
        currentTarget = target;

        jListTargets.setSelectedValue(target, true);
    }

    /**
     * Process any comboBox change event (interferometer, interferometer configuration, instrument, instrument configuration).
     * Refresh the dependent combo boxes and update the observation according to the form state
     * @param e action event
     */
    @Override
    public void actionPerformed(final ActionEvent e) {
        // disable the automatic update observation :
        final boolean prevAutoUpdateObservation = setAutoUpdateObservation(false);
        try {
            if (e.getSource() == jComboBoxInterferometer) {
                if (logger.isDebugEnabled()) {
                    logger.debug("Interferometer changed: {}", jComboBoxInterferometer.getSelectedItem());
                }
                updateComboInterferometerConfiguration();
                updateComboInstrument();
                updateComboInstrumentConfiguration();
                checkPops();
                synchronizePops(false);
                updateOrderedFixedPops();
                updateWindRestriction();

            } else if (e.getSource() == jComboBoxInterferometerConfiguration) {
                if (logger.isDebugEnabled()) {
                    logger.debug("Interferometer Configuration changed: {}", jComboBoxInterferometerConfiguration.getSelectedItem());
                }
                updateComboInstrument();
                updateComboInstrumentConfiguration();
                updateOrderedFixedPops();
            } else if (e.getSource() == jComboBoxInstrument) {
                if (logger.isDebugEnabled()) {
                    logger.debug("Instrument changed: {}", jComboBoxInstrument.getSelectedItem());
                }
                updateComboInstrumentConfiguration();
                updateOrderedFixedPops();
                validateCurrentPoPs();
            } else if (e.getSource() == jComboBoxPops) {
                final String selectedPops = jComboBoxPops.getSelectedItem().toString();
                if (logger.isDebugEnabled()) {
                    logger.debug("Pops changed: {}", selectedPops);
                }
                // update pops field:
                if (POPS_MANUAL.equals(selectedPops)) {
                    // manual combination:
                    updatePops(jTextPoPs.getText(), false);
                } else {
                    // auto or specific Pops combination
                    updatePops((POPS_AUTO.equals(selectedPops)) ? null : selectedPops, false);
                }
            } else if (e.getSource() == jComboBoxPopsMode) {
                if (logger.isDebugEnabled()) {
                    logger.debug("PopsMode changed: {}", jComboBoxPopsMode.getSelectedItem());
                }
            } else {
                logger.warn("Unsupported source component: {}", e.getSource());
            }

        } finally {
            // restore the automatic update observation :
            setAutoUpdateObservation(prevAutoUpdateObservation);
        }
        // group multiple calls into a single observation update event :
        fireObservationUpdateEvent();
    }

    /**
     * Called whenever the instrument configuration selection changes.
     * @param e the event that characterizes the change.
     */
    private void processInstrumentConfigurationValueChanged(final ListSelectionEvent e) {
        // skip events when the user selection is adjusting :
        if (e.getValueIsAdjusting()) {
            return;
        }

        final ListSelectionModel lsm = jListInstrumentConfigurations.getSelectionModel();

        // ensure at least one item is selected :
        if (lsm.isSelectionEmpty()) {
            checkInstrumentConfigurationSelection();
            return;
        }

        // memorize the first selected item :
        currentInsConfiguration = (String) jListInstrumentConfigurations.getSelectedValue();

        if (logger.isDebugEnabled()) {
            logger.debug("Instrument Configuration changed: {}", Arrays.toString(getInstrumentConfigurations()));
        }

        // update PoPs text field if the selected instrument configurations have associated PoPs in the configuration:
        updatePops();
        validateCurrentPoPs();

        // group multiple calls into a single observation update event :
        fireObservationUpdateEvent();
    }

    /**
     * Return the selected instrument configurations
     * @return selected instrument configurations
     */
    private Object[] getInstrumentConfigurations() {
        return jListInstrumentConfigurations.getSelectedValues();
    }

    /**
     * Check if the selected instrument configuration is empty, then restore the last selected configuration
     * or select the first configuration
     */
    private void checkInstrumentConfigurationSelection() {
        // check if the automatic configuration check flag is enabled :
        if (doAutoCheckConfigurations) {
            checkListSelection(jListInstrumentConfigurations, currentInsConfiguration);
        }
    }

    /**
     * Select all given values in the instrument configuration list
     * @param values configurations to select
     */
    private void selectInstrumentConfigurations(final Object[] values) {
        if (values != null && values.length > 0) {
            if (logger.isDebugEnabled()) {
                logger.debug("selectInstrumentConfigurations: {}", Arrays.toString(values));
            }

            final GenericListModel<String> lm = getInstrumentConfigurationModel();
            final DefaultListSelectionModel lsm = (DefaultListSelectionModel) jListInstrumentConfigurations.getSelectionModel();

            int index = -1;
            for (Object selection : values) {
                index = lm.indexOf((String) selection);
                if (index != -1) {
                    lsm.addSelectionInterval(index, index);
                    jListInstrumentConfigurations.ensureIndexIsVisible(index);
                }
            }
            if (index != -1) {
                // scroll to last selected value :
                jListInstrumentConfigurations.ensureIndexIsVisible(index - 1);
                jListInstrumentConfigurations.ensureIndexIsVisible(index);
                jListInstrumentConfigurations.ensureIndexIsVisible(index + 1);
            }
            if (logger.isDebugEnabled()) {
                logger.debug("selectInstrumentConfigurations : selectedValues: {}", Arrays.toString(getInstrumentConfigurations()));
            }
        }
    }

    /**
     * Check if this interferometer has PoPs:
     * enable or disable Pops field and combo box
     * If false, reset the PoPs text field
     */
    private void checkPops() {
        final String interferometerName = (String) jComboBoxInterferometer.getSelectedItem();

        List<Pop> popList = cm.getPoPs(interferometerName);
        logger.debug("popList: {}", popList);

        final boolean hasPops = !CollectionUtils.isEmpty(popList);

        // reset on interferometer change:
        popsCurrentMap = null;
        popsFixed = null;

        // note: label pops is only visible if the interferometer has Pops:
        jLabelPops.setVisible(hasPops);
        jTextPoPs.setVisible(hasPops);
        jComboBoxPops.setVisible(hasPops);
        jComboBoxPopsMode.setVisible(hasPops);
        jButtonPopsSet.setVisible(hasPops);
        jButtonPopsClear.setVisible(hasPops);
        jPanelFixedPops.setVisible(hasPops);

        jPanelFixedPopsGen.removeAll();
        resetFixedPops();

        if (hasPops) {
            // Add any Pop (null):
            popList = new ArrayList<>(popList);
            popList.add(0, Pop.ANY_POP);

            final List<Station> stationsWithPops = cm.getStationsWithPoPs(interferometerName);
            logger.debug("stationsWithPops: {}", stationsWithPops);

            int i = 0;
            for (final Station station : stationsWithPops) {
                final String name = station.getName();
                stationToPopsWidgetIndex.put(name, i++);
                defStationsForPops.add(name);

                final JLabel label = new JLabel(" " + name);
                label.setFont(FIXED_FONT_SMALLER);
                popsStationLabels.add(label);

                @SuppressWarnings("unchecked")
                final JComboBox comboBox = new JComboBox(new GenericListModel<Pop>(popList, true));
                comboBox.setPrototypeDisplayValue("X");
                comboBox.setRenderer(PopListRenderer.INSTANCE);
                comboBox.setFont(FIXED_FONT_SMALLER);
                comboBox.setSelectedItem(Pop.ANY_POP); // any
                comboBox.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(final ActionEvent e) {
                        actionPerformedFixedPopsComboBox(name);
                    }
                });
                popsComboBoxes.add(comboBox);

                // same order as stations in interferometer configuration:
                jPanelFixedPopsGen.add(label);
                jPanelFixedPopsGen.add(comboBox);
            }
        }
    }

    private void resetFixedPops() {
        lastStationsForPops = null;
        defStationsForPops.clear();
        popsStationLabels.clear();
        popsComboBoxes.clear();
        stationToPopsWidgetIndex.clear();
    }

    private void updateOrderedFixedPops() {
        // enable PoPs only if the interferometer support it:
        if (jLabelPops.isVisible()) {
            // Get possible 6T configuration (first):
            final String firstConf = (String) jListInstrumentConfigurations.getModel().getElementAt(0);
            logger.debug("firstConf: {}", firstConf);

            String[] stationNames = firstConf.split(" "); // ordered

            // check first station has Pops (skip CHARA_7T):
            if ((stationNames.length != defStationsForPops.size())
                    || (stationToPopsWidgetIndex.get(stationNames[0]) == null)) {
                stationNames = null;
            }

            final List<String> stationsForPops = (stationNames != null) ? Arrays.asList(stationNames) : defStationsForPops;
            logger.debug("stationsForPops: {}", stationsForPops);

            if (!ObjectUtils.areEquals(stationsForPops, lastStationsForPops)) {
                lastStationsForPops = new ArrayList<>(stationsForPops); // copy needed (defStationsForPops)
                jPanelFixedPopsGen.removeAll();

                for (final String name : stationsForPops) {
                    final Integer index = stationToPopsWidgetIndex.get(name);
                    if (index != null) {
                        final JLabel label = popsStationLabels.get(index);
                        final JComboBox comboBox = popsComboBoxes.get(index);
                        // same order as given stations:
                        jPanelFixedPopsGen.add(label);
                        jPanelFixedPopsGen.add(comboBox);
                    }
                }
            }
        }
    }

    private void actionPerformedFixedPopsComboBox(final String name) {
        final Integer index = stationToPopsWidgetIndex.get(name);
        if (index != null) {
            final JComboBox comboBox = popsComboBoxes.get(index);
            final Pop selectedPop = (Pop) comboBox.getSelectedItem();

            logger.debug("actionPerformedFixedPopsComboBox: {} = {}", name, selectedPop);

            if (selectedPop == Pop.ANY_POP) {
                if (popsFixed != null) {
                    popsFixed.remove(name);
                    if (popsFixed.isEmpty()) {
                        popsFixed = null;
                    }
                }
            } else {
                if (popsFixed == null) {
                    popsFixed = new TreeMap<>();
                }
                popsFixed.put(name, selectedPop);
            }
            logger.debug("popsFixed: {}", popsFixed);

            fireObservationUpdateEvent();
        }
    }

    private void setFixedPopsComboBox(final Map<String, Pop> fixedPops) {
        if (jLabelPops.isVisible() && (fixedPops != null)) {
            logger.debug("setFixedPopsComboBox: {}", fixedPops);

            // already reset
            for (Map.Entry<String, Pop> entry : fixedPops.entrySet()) {
                final String name = entry.getKey();
                final Pop pop = entry.getValue();

                final Integer index = stationToPopsWidgetIndex.get(name);
                if (index != null) {
                    final JComboBox comboBox = popsComboBoxes.get(index);
                    comboBox.setSelectedItem(pop);
                }
            }
        }
    }

    /**
     * Make both Pops field and combo box enabled
     * @param enabled true to enable them; false otherwise
     */
    private void makePopsEditable(final boolean enabled) {
        jTextPoPs.setEnabled(enabled);
        jComboBoxPops.setEnabled(enabled);
        jComboBoxPopsMode.setEnabled(enabled);
        jButtonPopsSet.setEnabled(enabled);
        jButtonPopsClear.setEnabled(enabled);
    }

    /**
     * Process the change event for the PoPs configuration text field.
     * Validates the new input (digits corresponds to valid PoPs indices)
     * @param evt property change event
     */
    public void jTextPoPsPropertyChange(final PropertyChangeEvent evt) {
        final Object value = evt.getNewValue();

        logger.debug("jTextPoPsPropertyChange: value: {}", value);

        validateTextPoPs((value != null) ? value.toString() : null);

        // then update the observation :
        fireObservationUpdateEvent();
    }

    private void validateCurrentPoPs() {
        validateTextPoPs(jTextPoPs.getText());
    }

    private void validateTextPoPs(final String value) {
        String popConfig = value;
        if (value != null) {
            // 1st selected configuration:
            final String lastInstrumentConfiguration = lastInsConfCompatibleWithPopConfig;
            logger.debug("Last Instrument Config: {}", lastInstrumentConfiguration);

            // parse the configuration (instrument = number of channels) + (interferometer = pop indexes [1-5]) :
            final List<Pop> listPoPs = cm.parseInstrumentPoPs((String) jComboBoxInterferometerConfiguration.getSelectedItem(),
                    currentInsConfiguration, popConfig, lastInstrumentConfiguration, popsFixed);

            if (listPoPs == null) {
                // invalid, reset the field to empty :
                synchronizePops(true);
                popConfig = jTextPoPs.getText();
            } else {
                popConfig = Pop.toString(listPoPs);
                logger.debug("fixed popConfig: {}", popConfig);
            }
        }
        // update combo box:
        updatePops(popConfig, false);
    }

    /**
     * Update the Pops field and combo box
     * @param value String or Integer value
     * @param notify true to notify changes (Integer value); false otherwise (String value)
     */
    private void updatePops(final Object value, final boolean notify) {
        logger.debug("updatePops: {} ({})", value, notify);

        if (notify) {
            // 2 cases: value == null or value == integer value (pop config):
            // test against PoPs integer value:
            if (!ObjectUtils.areEquals(value, jTextPoPs.getValue())) {
                // fire a property change event:
                jTextPoPs.setValue(value);
            } else if (value == null) {
                // clear text value:
                jTextPoPs.setText(null);
            }
            return;
        }

        // note : setText() does not fire a property change event:
        jTextPoPs.setText((value != null) ? value.toString() : null);

        if (value != null) {
            this.lastInsConfCompatibleWithPopConfig = currentInsConfiguration;
            logger.debug("Last Config set: {} with PoPs: {}", lastInsConfCompatibleWithPopConfig, value);
        }

        final String text = jTextPoPs.getText();

        final String selected;

        if (!(StringUtils.isEmpty(text))) {
            // check if the combo box has this value:
            final ComboBoxModel<String> comboModel = jComboBoxPops.getModel();

            boolean found = false;
            for (int i = 0, len = comboModel.getSize(); i < len; i++) {
                if (text.equals(comboModel.getElementAt(i))) {
                    found = true;
                    break;
                }
            }
            selected = (found) ? text : POPS_MANUAL;
        } else {
            selected = POPS_AUTO;
        }
        // avoid firing multiple actionPerformed:
        if (!ObjectUtils.areEquals(selected, jComboBoxPops.getSelectedItem())) {
            jComboBoxPops.setSelectedItem(selected);
        }
    }

    /**
     * Synchronize Pops (text field / Combo box) with configuration pops
     * and optionally reset PoPs text field
     * @param forceReset true to force clearing pops
     */
    private void synchronizePops(final boolean forceReset) {
        logger.debug("synchronizePops: forceReset = {}", forceReset);

        // disable the automatic update observation :
        final boolean prevAutoUpdateObservation = setAutoUpdateObservation(false);
        try {
            // enable PoPs only if the interferometer support it:
            if (jLabelPops.isVisible()) {
                final String popConfig = getConfigurationPops();
                final boolean popMulti = POPS_MULTI_CONF.equals(popConfig);

                final Integer value = (popConfig != null && !popMulti) ? NumberUtils.valueOf(popConfig) : null;

                if (value != null || forceReset) {
                    // fire a property change event:
                    updatePops(value, true);
                }

                // allow user inputs when no PoPs are defined in the configuration and not in multi-conf:
                makePopsEditable(!isGuiRestrictionEnabled() || isPopsEditable() && ((value == null) && !popMulti));
            } else {
                // fire a property change event:
                updatePops(null, true);
                makePopsEditable(false);
            }
            lastConfPopConfig = null;

        } finally {
            // restore the automatic update observation :
            setAutoUpdateObservation(prevAutoUpdateObservation);
        }
    }

    /**
     * Update the PoPs text field using the PoPs defined in the current instrument configurations
     * only if one and only one configuration is selected.
     */
    private void updatePops() {
        // handle PoPs only if the interferometer support it:
        if (jLabelPops.isVisible()) {
            final String popConfig = getConfigurationPops();

            // get last Pops defined by the instrument configuration:
            final String lastPopConfig = lastConfPopConfig;
            lastConfPopConfig = null;

            logger.debug("updatePops: {}, last: {}", popConfig, lastPopConfig);

            if (popConfig != null) {
                // update the selected pops (pops) :
                if (POPS_MULTI_CONF.equals(popConfig)) {
                    // fire a property change event:
                    updatePops(null, true);
                    makePopsEditable(isPopsEditable());
                } else {
                    updatePops(popConfig, false);
                    makePopsEditable(!isGuiRestrictionEnabled());
                    lastConfPopConfig = popConfig;
                }
            } else if (lastPopConfig != null) {
                // reset the predefined pops
                // fire a property change event:
                updatePops(null, true);
                makePopsEditable(isPopsEditable());
            }
        }
    }

    /**
     * Return the PoP identifiers defined for the current instrument configuration
     * only if one and only one configuration is selected.
     * @return PoP identifiers or null or POPS_MULTI if PoPs are in use but in multi-configuration
     */
    private String getConfigurationPops() {
        final Object[] instConfs = getInstrumentConfigurations();
        if (instConfs.length > 1) {
            // multi configuration:
            return jLabelPops.isVisible() ? POPS_MULTI_CONF : null;
        }
        if (instConfs.length == 1) {
            // determine if there are PoPs defined in the instrument configuration :
            final String instrumentConfiguration = (String) instConfs[0];
            final List<Pop> popList = cm.getInstrumentConfigurationPoPs((String) jComboBoxInterferometerConfiguration.getSelectedItem(),
                    (String) jComboBoxInstrument.getSelectedItem(), instrumentConfiguration);

            if (popList != null && !popList.isEmpty()) {
                // PoPs are defined in the instrument configuration :
                return Pop.toString(popList);
            }
        }
        return null;
    }

    private void setPopsModeComboBox(final BestPopsMode popsMode) {
        if (jLabelPops.isVisible()) {
            logger.debug("setPopsModeComboBox: {}", popsMode);
            switch (popsMode) {
                case ALL:
                    this.jComboBoxPopsMode.setSelectedItem(POPS_MODE_ALL);
                    break;
                case SELECTED:
                    this.jComboBoxPopsMode.setSelectedItem(POPS_MODE_SEL);
                    break;
                default:
                    logger.warn("Unsupported case: {}", popsMode);
            }
        }
    }

    private BestPopsMode getBestPopsMode() {
        BestPopsMode popsMode = null;

        if (jLabelPops.isVisible()) {
            switch ((String) this.jComboBoxPopsMode.getSelectedItem()) {
                case POPS_MODE_ALL:
                    popsMode = BestPopsMode.ALL;
                    break;
                case POPS_MODE_SEL:
                    popsMode = BestPopsMode.SELECTED;
                    break;
                default:
                    logger.warn("Unsupported case: {}", popsMode);
            }
        }
        return popsMode;
    }

    /**
     * Enable or disable the wind restriction depending on the chosen interferometer
     */
    private void updateWindRestriction() {
        final Double windRestriction = cm.getWindPointingRestriction((String) jComboBoxInterferometer.getSelectedItem());

        // wind restriction is enabled only if night restriction are enabled and interferometer support it:
        final boolean useWind = jCheckBoxNightLimit.isSelected()
                && (windRestriction != null && windRestriction > 0d && windRestriction < 180d);

        if (!useWind) {
            // reset
            jCheckBoxWind.setSelected(false);
        }

        jCheckBoxWind.setEnabled(useWind);
    }

    /**
     * Handle the star resolver result (EDT) to
     * create new Target(s) object with the retrieved data from Simbad and
     * fire a single ObservationTargetsChanged event
     * @param result star resolver result
     */
    @Override
    public void handleResult(final StarResolverResult result) {
        logger.debug("star resolver result:\n{}", result);
        if (!result.isEmpty()) {
            // get names (read only):
            List<String> validNames = result.getNames();

            // Handle multiple matches per identifier:
            if (result.isMultipleMatches()) {
                // clone names:
                validNames = new ArrayList<String>(validNames);
                // Remove all identifiers having multiple matches:
                validNames.removeAll(result.getNamesForMultipleMatches());
            }

            if (!validNames.isEmpty()) {
                final boolean doConfirm = (validNames.size() < 100);

                final List<Target> editTargets = om.getTargets();
                final StringBuilder sb = new StringBuilder(64);
                boolean isTargetChanged = false;
                try {
                    for (String name : validNames) {
                        isTargetChanged |= TargetImporter.addTarget(result.getSingleStar(name), editTargets, doConfirm, sb);
                    }
                } finally {
                    if (isTargetChanged) {
                        // fire change events :
                        om.fireTargetChangedEvents();
                    }
                    if (sb.length() != 0) {
                        MessagePane.showWarning(sb.toString(), "Simbad resolver");
                    }
                }
            }
        }
    }

    /**
     * Fire an Observation Change event when a Swing component changed
     * ONLY if the automatic update flag is enabled
     */
    private void fireObservationUpdateEvent() {
        // check if the automatic update flag is enabled :
        if (doAutoUpdateObservation) {
            logger.debug("fireObservationUpdateEvent");

            if (DEBUG_UPDATE_EVENT) {
                logger.warn("FIRE_UPDATE", new Throwable());
            }

            om.fireObservationUpdate();
        }
    }

    /**
     * Update the UI widgets from the given observation when configuration changed
     *
     * @param observation observation
     */
    private void onConfigChanged(final ObservationSetting observation) {
        if (logger.isDebugEnabled()) {
            logger.debug("onConfigChanged:\n{}", ObservationManager.toString(observation));
        }
        // disable the automatic update observation :
        final boolean prevAutoUpdateObservation = setAutoUpdateObservation(false);
        // disable the automatic selection check of the instrument configuration :
        final boolean prevAutoCheckConfigurations = setAutoCheckConfigurations(false);
        try {
            // reset partially cached values :
            currentInsConfiguration = null;
            lastConfPopConfig = null;
            lastInsConfCompatibleWithPopConfig = null;
            lastInsConfKeyForBestPops = null;
            popsCurrentMap = null;
            popsFixed = null;
            resetFixedPops();

            // configuration changed so update interferometer names:
            updateComboInterferometer();

            // observation :
            // update the interferometer and interferometer configuration :
            final InterferometerConfigurationChoice interferometerChoice = observation.getInterferometerConfiguration();

            final InterferometerConfiguration ic = interferometerChoice.getInterferometerConfiguration();

            if (ic != null) {
                // update the selected interferometer :
                jComboBoxInterferometer.setSelectedItem(ic.getInterferometer().getName());
                // update the selected interferometer configuration :
                jComboBoxInterferometerConfiguration.setSelectedItem(ic.getName());
            }

            final FocalInstrumentConfigurationChoice instrumentChoice = observation.getInstrumentConfiguration();

            // update the selected instrument :
            jComboBoxInstrument.setSelectedItem(instrumentChoice.getName());

            // update the selected instrument configurations :
            final List<ObservationVariant> obsVariants = observation.getVariants();
            final int len = obsVariants.size();
            final Object[] stationConfs = new Object[len];
            for (int i = 0; i < len; i++) {
                stationConfs[i] = obsVariants.get(i).getStations();
            }

            jListInstrumentConfigurations.clearSelection();
            selectInstrumentConfigurations(stationConfs);

            // update the selected pops (pops) :
            updatePops(instrumentChoice.getPops(), false);

            // restore Pops widgets
            setFixedPopsComboBox(interferometerChoice.getFixedPops());

        } finally {
            // restore the automatic selection check of the instrument configuration :
            setAutoCheckConfigurations(prevAutoCheckConfigurations);
            // restore the automatic update observation :
            setAutoUpdateObservation(prevAutoUpdateObservation);
        }
        // ensure one configuration is selected :
        checkInstrumentConfigurationSelection();
    }

    /**
     * Update the UI widgets from the given loaded observation
     *
     * @param observation observation
     */
    private void onLoadObservation(final ObservationSetting observation) {
        if (logger.isDebugEnabled()) {
            logger.debug("onLoadObservation:\n{}", ObservationManager.toString(observation));
        }
        // disable the automatic update observation :
        final boolean prevAutoUpdateObservation = setAutoUpdateObservation(false);
        // disable the automatic selection check of the instrument configuration :
        final boolean prevAutoCheckConfigurations = setAutoCheckConfigurations(false);
        // disable the automatic selection check of the target list :
        final boolean prevAutoCheckTargets = setAutoCheckTargets(false);
        try {
            // clear selected target :
            jListTargets.clearSelection();
            // reset cached values :
            loadedObsSetup = null;
            currentInsConfiguration = null;
            lastConfPopConfig = null;
            lastInsConfCompatibleWithPopConfig = null;
            lastInsConfKeyForBestPops = null;
            popsCurrentMap = null;
            popsFixed = null;
            resetFixedPops();

            // use observation context to enable/disable POPS FIRST (event ordering issue):
            makePopsEditable(isPopsEditable());

            // observation :
            // Get selected target:
            currentTarget = observation.getSelectedTarget();

            // update the interferometer and interferometer configuration :
            final InterferometerConfigurationChoice interferometerChoice = observation.getInterferometerConfiguration();

            final InterferometerConfiguration ic = interferometerChoice.getInterferometerConfiguration();

            if (ic != null) {
                // update the selected interferometer :
                jComboBoxInterferometer.setSelectedItem(ic.getInterferometer().getName());
                // update the selected interferometer configuration :
                jComboBoxInterferometerConfiguration.setSelectedItem(ic.getName());
            }

            final FocalInstrumentConfigurationChoice instrumentChoice = observation.getInstrumentConfiguration();

            // update the selected instrument :
            jComboBoxInstrument.setSelectedItem(instrumentChoice.getName());

            // update the selected instrument configurations :
            final List<ObservationVariant> obsVariants = observation.getVariants();
            final int len = obsVariants.size();
            final Object[] stationConfs = new Object[len];
            for (int i = 0; i < len; i++) {
                stationConfs[i] = obsVariants.get(i).getStations();
            }

            jListInstrumentConfigurations.clearSelection();
            selectInstrumentConfigurations(stationConfs);

            // update the selected pops (pops) :
            updatePops(instrumentChoice.getPops(), false);

            // restore Pops widgets
            setFixedPopsComboBox(interferometerChoice.getFixedPops());

            // restore BestPopsMode:
            setPopsModeComboBox(interferometerChoice.getBestPopsModeOrDefault());

            // constraints :
            // update the night restriction :
            jCheckBoxNightLimit.setSelected(observation.getWhen().isNightRestriction());

            // update the date spinner :
            final XMLGregorianCalendar date = observation.getWhen().getDate();
            if (date != null) {
                jDateSpinner.setValue(date.toGregorianCalendar().getTime());
            }

            // Update wind direction:
            windWidget.setEnabled(true); // to update its value
            final Double windAz = observation.getWhen().getWindAzimuth();
            final boolean useWind = (windAz != null);
            windWidget.setValue((useWind) ? windAz.doubleValue() : 0d);
            windWidget.setEnabled(useWind);
            jCheckBoxWind.setSelected(useWind);

            // update the min elevation :
            jFieldMinElev.setValue(interferometerChoice.getMinElevation());

        } finally {
            // restore the automatic selection check of the target list :
            setAutoCheckTargets(prevAutoCheckTargets);
            // restore the automatic selection check of the instrument configuration :
            setAutoCheckConfigurations(prevAutoCheckConfigurations);
            // restore the automatic update observation :
            setAutoUpdateObservation(prevAutoUpdateObservation);
        }
        // ensure one configuration is selected :
        checkInstrumentConfigurationSelection();

        // Memorize the loaded config:
        updateInitialSetup();

        // use observation context to enable/disable GUI features:
        if (!isGuiRestrictionEnabled()) {
            logger.warn("Advanced user mode: {} disabled.", Preferences.GUI_RESTRICTIONS);
        }
        final ObservationContext ctx = getObservationContext();

        if (ctx != null) {
            // Main settings:
            jComboBoxInterferometer.setEnabled(ctx.isInterferometerEditable());
            jComboBoxInterferometerConfiguration.setEnabled(ctx.isPeriodEditable());
            jComboBoxInstrument.setEnabled(ctx.isInstrumentEditable());

            // Configuration(s):
            jListInstrumentConfigurations.setEnabled(ctx.isConfigurationsEditable());

            // Constraints:
            jCheckBoxNightLimit.setEnabled(ctx.isNightEditable());
            jDateSpinner.setEnabled(ctx.isDateEditable());
            jFieldMinElev.setEnabled(ctx.isMinElevationEditable());

        } else {
            // reset GUI:

            // Main settings:
            jComboBoxInterferometer.setEnabled(true);
            jComboBoxInterferometerConfiguration.setEnabled(true);
            jComboBoxInstrument.setEnabled(true);

            // Configuration(s):
            jListInstrumentConfigurations.setEnabled(true);

            // Constraints:
            jCheckBoxNightLimit.setEnabled(true);
            jDateSpinner.setEnabled(true);
            jFieldMinElev.setEnabled(true);
        }

        // TARGETS:
        final boolean targetEditable = isTargetEditable();

        starSearchField.setEnabled(targetEditable);
        jListTargets.setEnabled(targetEditable);
        jButtonTargetEditor.setEnabled(targetEditable);
        jButtonDeleteTarget.setEnabled(targetEditable);

        // auto-update coords when loading an asprox file:
        SwingUtils.invokeLaterEDT(new Runnable() {
            @Override
            public void run() {
                updateWithStarResolver(true);
            }
        });
    }

    private void updateInitialSetup() {
        this.loadedObsSetup = ((String) jComboBoxInterferometer.getSelectedItem())
                + " " + ((String) jComboBoxInstrument.getSelectedItem())
                + " " + Arrays.toString(getInstrumentConfigurations())
                + (!StringUtils.isEmpty(jTextPoPs.getText()) ? (" PoPs: " + jTextPoPs.getText()) : "");

        logger.debug("initialSetup: {}", loadedObsSetup);
    }

    /**
     * @return true if the pops are editable
     */
    private boolean isPopsEditable() {
        return (getObservationContext() != null) ? getObservationContext().isPopsEditable() : true;
    }

    /**
     * @return true if the target(s) is editable
     */
    private boolean isTargetEditable() {
        return (getObservationContext() != null) ? getObservationContext().isTargetsEditable() : true;
    }

    /**
     * Update the current observation (via the ObservationManager) with state of UI widgets
     * ONLY if the automatic update flag is enabled.
     *
     * If the observation changes, it updates the event's changed flag (MAIN | UV | NONE)
     * to fire an observation refresh event.
     *
     * @param event update event
     */
    private void onUpdateObservation(final UpdateObservationEvent event) {
        // check if the automatic update flag is enabled :
        if (doAutoUpdateObservation) {
            if (DEBUG_UPDATE_EVENT) {
                logger.warn("UPDATE", new Throwable());
            }

            boolean changed = false;

            // observation :
            changed |= om.setInterferometerConfigurationName((String) jComboBoxInterferometerConfiguration.getSelectedItem());
            changed |= om.setInterferometerPopsFixed(this.popsFixed);
            changed |= om.setInterferometerPopsMode(getBestPopsMode());

            changed |= om.setInstrumentConfigurationName((String) jComboBoxInstrument.getSelectedItem());
            changed |= om.setInstrumentConfigurationStations(getInstrumentConfigurations());
            changed |= om.setInstrumentConfigurationPoPs(jTextPoPs.getText());

            // constraints :
            changed |= om.setWhen((Date) jDateSpinner.getModel().getValue());
            changed |= om.setMinElevation(((Number) jFieldMinElev.getValue()).doubleValue());
            changed |= om.setNightRestriction(jCheckBoxNightLimit.isSelected());

            if (jCheckBoxWind.isSelected()) {
                changed |= om.setWindAzimuth(Double.valueOf(windWidget.getValue()));
            } else {
                changed |= om.setWindAzimuth(null);
            }

            logger.debug("onUpdateObservation: changed: {}", changed);

            if (changed) {
                // update change flag to make the ObservationManager fire an observation refresh event later
                event.setChanged(UpdateObservationEvent.ChangeType.MAIN);
            }
        }
    }

    /**
     * Handle the given event on the given observation.
     * Refresh the UI component according to the loaded observation settings
     *
     * @param event event
     */
    @Override
    public void onProcess(final ObservationEvent event) {
        if (logger.isDebugEnabled()) {
            logger.debug("event [{}] process IN", event.getType());
        }
        switch (event.getType()) {
            case CONFIG_CHANGED:
                onConfigChanged(event.getObservation());
                break;
            case LOADED:
                onLoadObservation(event.getObservation());
                break;
            case TARGET_CHANGED:
                updateListTargets();
                break;
            case TARGET_SELECTION_CHANGED:
                if (event instanceof TargetSelectionEvent) {
                    showSelectedTarget(((TargetSelectionEvent) event).getTarget());
                }
                break;
            case DO_UPDATE:
                if (event instanceof UpdateObservationEvent) {
                    onUpdateObservation((UpdateObservationEvent) event);
                }
                break;
            case REFRESH:
                resetStatus();
                break;
            case REFRESH_UV:
                resetStatus();
                break;
            case OBSERVABILITY_DONE:
                if (event instanceof ObservabilityEvent) {
                    updateObservabilityData(event.getObservationCollection(),
                            ((ObservabilityEvent) event).getObservabilityData());
                }
                break;
            case WARNINGS_READY:
                if (event instanceof WarningContainerEvent) {
                    updateStatus(((WarningContainerEvent) event).getWarningContainer());
                }
                break;
            default:
        }
        if (logger.isDebugEnabled()) {
            logger.debug("event [{}] process OUT", event.getType());
        }
    }

    /**
     * Update the observability Data
     * @param obsCollection observation collection to use
     * @param obsDataList observability data
     */
    private void updateObservabilityData(final ObservationCollection obsCollection,
                                         final List<ObservabilityData> obsDataList) {
        // are PoPs in use ?
        if (!jComboBoxPops.isVisible()) {
            resetComboBoxPops(POPS_AUTO);
        } else {
            // reset
            this.popsCurrentMap = null;
            this.jButtonPopsSet.setToolTipText(null);
            this.jTextFieldCurrentPops.setText(null);

            final int nObs = obsDataList.size();

            if (nObs == 1) {
                final ObservationSetting observation = obsCollection.getFirstObservation();
                final ObservabilityData obsData = obsDataList.get(0);

                final String insConfKey = observation.getInstrumentConfiguration().getStations()
                        + "@" + obsData.isDoBaseLineLimits();

                logger.debug("updateObservabilityData: used PoPs = {}", obsData.getUsedPops());

                if (obsData.getUsedPops() != null) {
                    // Update current Pops:
                    final List<Station> stations = obsData.getStationList();
                    final Pop[] pops = obsData.getUsedPops().getPops();

                    final Map<String, Pop> currentPops = new TreeMap<>();

                    for (int i = 0; i < pops.length; i++) {
                        currentPops.put(stations.get(i).getName(), pops[i]);
                    }

                    this.popsCurrentMap = currentPops;
                    logger.debug("popsCurrentMap: {}", popsCurrentMap);

                    final StringBuilder sb = new StringBuilder(32);
                    obsData.getStationNameWithPops(sb);
                    this.jButtonPopsSet.setToolTipText("Set fixed Pops with " + sb.toString());

                    if (lastStationsForPops != null) {
                        // Custom format:
                        sb.setLength(0);
                        sb.append(' ');
                        for (String name : lastStationsForPops) {
                            sb.append(name);
                            sb.append('(');
                            final Pop p = currentPops.get(name);
                            if (p != null) {
                                sb.append(p.getIndex());
                            } else {
                                sb.append('-');
                            }
                            sb.append(") ");
                        }
                        sb.deleteCharAt(sb.length() - 1);
                        this.jTextFieldCurrentPops.setText(sb.toString());
                    }
                }

                final List<PopCombination> bestPopList = obsData.getBestPopList();
                final List<PopCombination> betterPopList = obsData.getBetterPopList();

                // PoPs list are null if [Manual]:
                if (bestPopList != null && betterPopList != null) {
                    final Vector<String> orderedPops = new Vector<String>(bestPopList.size() + betterPopList.size() + 2);
                    orderedPops.add(POPS_MANUAL);
                    orderedPops.add(POPS_AUTO);

                    for (PopCombination p : bestPopList) {
                        orderedPops.add(p.toString());
                    }
                    for (PopCombination p : betterPopList) {
                        orderedPops.add(p.toString());
                    }

                    // single observation results:
                    final ComboBoxModel<String> comboPopsModel = new DefaultComboBoxModel<>(orderedPops);
                    comboPopsModel.setSelectedItem(POPS_AUTO);
                    jComboBoxPops.setModel(comboPopsModel);

                    lastInsConfKeyForBestPops = insConfKey;
                } else {
                    // check [Manual] ie is Pops combo box still valid ? (same stations)
                    if ((lastInsConfKeyForBestPops != null) && !lastInsConfKeyForBestPops.equals(insConfKey)) {
                        resetComboBoxPops((obsData.isUserPops() ? POPS_MANUAL : POPS_AUTO));
                        lastInsConfKeyForBestPops = null;
                    }
                }
            } else if (nObs > 1) {
                // multiple observation results but how to merge best PoPs solutions ?
                // reset and restore auto or manual mode:
                resetComboBoxPops((obsDataList.get(0).getBestPopList() != null) ? POPS_AUTO : POPS_MANUAL);
                lastInsConfKeyForBestPops = null;
            }
        }
    }

    private void resetComboBoxPops(final String selectedItem) {
        defaultModelPops.setSelectedItem(selectedItem);
        jComboBoxPops.setModel(defaultModelPops);
    }

    /**
     * Reset status panel
     */
    private void resetStatus() {
        updateStatus(null);
    }

    /**
     * Update status panel
     * @param warningContainer warning container or null to reset content
     */
    private void updateStatus(final WarningContainer warningContainer) {
        if (warningContainer == null || !warningContainer.hasWarning()) {
            // reset
            if (jLabelStatus.getIcon() != null) {
                jLabelStatus.setIcon(null);
                jLabelStatus.setText("Ok");
                jLabelStatus.setToolTipText(null);
            }
        } else {
            logger.debug("updateStatus: {}", warningContainer.getVersion());

            final Level level = warningContainer.getLevel();

            jLabelStatus.setIcon((level == Level.Warning) ? ResourceImage.WARNING_ICON.icon() : ResourceImage.INFO_ICON.icon());
            jLabelStatus.setText(level.toString());

            final int maxLen = Math.min(MAX_TOOLTIP_WARNINGS, warningContainer.getWarnings().size());
            final StringBuilder sb = new StringBuilder(100 * maxLen);
            sb.append("<html>");

            int n = 0;

            // Add initial setup:
            if (this.loadedObsSetup != null) {
                n++;
                sb.append("(loaded obs. setup: ").append(loadedObsSetup).append(")<br>");
            }

            _warningLogger.info("-------------------");

            if (level == Level.Information) {
                _warningLogger.info("Observation status : " + level);
            } else {
                _warningLogger.warn("Observation status : " + level);
            }

            for (final WarningMessage message : warningContainer.getWarnings()) {
                String msg = message.getMessage();

                if (n < MAX_TOOLTIP_WARNINGS) {
                    if (message.getLevel() == Level.Html) {
                        sb.append(msg);
                        n++;
                    } else if (message.getLevel() != Level.Trace) {
                        if (message.getLevel() == Level.Warning) {
                            sb.append("<b>");
                        }
                        sb.append(StringUtils.encodeTagContent(msg));
                        if (message.getLevel() == Level.Warning) {
                            sb.append("</b>");
                        }
                        sb.append("<br>");
                        n++;
                    }
                    if (n == MAX_TOOLTIP_WARNINGS) {
                        sb.append("...");
                    }
                }

                if (message.isLogged()) {
                    logger.debug("skipped warning: {}", message);
                } else if (message.getLevel() != Level.Html) {
                    message.setLogged(true);
                    msg = StringUtils.removeTags(msg);

                    // add message to the warning log:
                    switch (message.getLevel()) {
                        case Trace:
                        case Information:
                            _warningLogger.info(msg);
                            break;
                        case Warning:
                            _warningLogger.warn(msg);
                            break;
                        default:
                    }
                }
            }
            sb.append("</html>");

            jLabelStatus.setToolTipText(sb.toString());
        }
    }

    public void updateWithStarResolver() {
        updateWithStarResolver(false);
    }

    private void updateWithStarResolver(final boolean onLoad) {
        final List<Target> editTargets = om.getTargets();

        if (editTargets.isEmpty()) {
            // set initial target version (avoid repeats):
            om.updateTargetVersion();
        } else {
            if (onLoad) {
                // check version:
                if (om.checkTargetVersion()) {
                    // version is OK, skip update
                    return;
                }

                if (!MessagePane.showConfirmMessage(this, "Do you want to update targets with the latest star resolver information (coordinates ...) ?")) {
                    return;
                }
            }

            // update target version (avoid repeats):
            om.updateTargetVersion();

            try {
                final StringBuilder sb = new StringBuilder(256);

                for (Target t : editTargets) {
                    sb.append(t.getName()).append(StarResolver.SEPARATOR_SEMI_COLON);
                }
                sb.setLength(sb.length() - 1);

                starSearchField.setText(sb.toString());

                // set flag to ignore fixing star name:
                starSearchField.setFlags(StarResolver.FLAG_SKIP_FIX_NAME);
                // asynchronous event:
                starSearchField.postActionEvent();
            } finally {
                // anyway reset flags & text:
                starSearchField.resetFlags();
                starSearchField.setText(null);
            }
        }
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonDeleteTarget;
    private javax.swing.JButton jButtonGetStar;
    private javax.swing.JButton jButtonPopsClear;
    private javax.swing.JButton jButtonPopsSet;
    private javax.swing.JButton jButtonSkyCalc;
    private javax.swing.JButton jButtonTargetEditor;
    private javax.swing.JCheckBox jCheckBoxNightLimit;
    private javax.swing.JCheckBox jCheckBoxWind;
    private javax.swing.JComboBox jComboBoxInstrument;
    private javax.swing.JComboBox jComboBoxInterferometer;
    private javax.swing.JComboBox jComboBoxInterferometerConfiguration;
    private javax.swing.JComboBox<String> jComboBoxPops;
    private javax.swing.JComboBox<String> jComboBoxPopsMode;
    private javax.swing.JSpinner jDateSpinner;
    private javax.swing.JFormattedTextField jFieldMinElev;
    private javax.swing.JLabel jLabelCurrentPops;
    private javax.swing.JLabel jLabelDate;
    private javax.swing.JLabel jLabelFixedPops;
    private javax.swing.JLabel jLabelInstrument;
    private javax.swing.JLabel jLabelInterferometer;
    private javax.swing.JLabel jLabelMinElev;
    private javax.swing.JLabel jLabelPeriod;
    private javax.swing.JLabel jLabelPops;
    private javax.swing.JLabel jLabelState;
    private javax.swing.JLabel jLabelStatus;
    private javax.swing.JList jListInstrumentConfigurations;
    private javax.swing.JList jListTargets;
    private javax.swing.JPanel jPanelConfLeft;
    private javax.swing.JPanel jPanelConfRight;
    private javax.swing.JPanel jPanelConfigurations;
    private javax.swing.JPanel jPanelConsLeft;
    private javax.swing.JPanel jPanelFixedPops;
    private javax.swing.JPanel jPanelFixedPopsGen;
    private javax.swing.JPanel jPanelFixedPopsSpacer;
    private javax.swing.JPanel jPanelMain;
    private javax.swing.JPanel jPanelObsBottom;
    private javax.swing.JPanel jPanelObsLeft;
    private javax.swing.JPanel jPanelObsRight;
    private javax.swing.JPanel jPanelOptBottom;
    private javax.swing.JPanel jPanelOptions;
    private javax.swing.JPanel jPanelStatus;
    private javax.swing.JPanel jPanelTargets;
    private javax.swing.JPanel jPanelTargetsLeft;
    private javax.swing.JPanel jPanelTargetsRight;
    private javax.swing.JScrollPane jScrollPaneInstrumentConfigurations;
    private javax.swing.JScrollPane jScrollPaneTargets;
    private javax.swing.JTextField jTextFieldCurrentPops;
    private javax.swing.JFormattedTextField jTextPoPs;
    private fr.jmmc.jmal.star.EditableStarResolverWidget starSearchField;
    // End of variables declaration//GEN-END:variables

    static abstract class ConfigurationJList extends JList {

        /** default serial UID for Serializable interface */
        protected static final long serialVersionUID = 1;

        abstract void setAltNames(final List<String> altNames);
    };

    /**
     * Create the custom JList to support tooltips for targets
     * @return JList
     */
    public static JList createConfigurationList() {
        final JList list = new ConfigurationJList() {
            /* members */
            private List<String> altNames;

            void setAltNames(final List<String> altNames) {
                this.altNames = altNames;
            }

            /** 
             * This method is called as the cursor moves within the component
             * @param me mouse event
             * @return tooltip text
             */
            @Override
            public String getToolTipText(final MouseEvent me) {
                final Point pt = me.getPoint();
                // Get item index :
                final int index = locationToIndex(pt);
                if (index != -1) {
                    // check cell bounds:
                    if (getCellBounds(index, index + 1).contains(pt)) {
                        if (altNames != null && altNames.size() > index) {
                            // Get alternative name:
                            final String altName = altNames.get(index);
                            if (altName != null) {
                                return "Configuration '" + altName + "' (" + getModel().getElementAt(index) + ")";
                            }
                        }
                    }
                }
                return getToolTipText();
            }
        };

        return list;
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

    /**
     * Enable / Disable the automatic update of the observation when any swing component changes.
     * Return its previous value.
     *
     * Typical use is as following :
     * // disable the automatic update observation :
     * final boolean prevAutoUpdateObservation = setAutoUpdateObservation(false);
     * try {
     *   // operations ...
     *
     * } finally {
     *   // restore the automatic update observation :
     *   setAutoUpdateObservation(prevAutoUpdateObservation);
     * }
     *
     * @param value new value
     * @return previous value
     */
    private boolean setAutoUpdateObservation(final boolean value) {
        // first backup the state of the automatic update observation :
        final boolean previous = doAutoUpdateObservation;

        // then change its state :
        doAutoUpdateObservation = value;

        // return previous state :
        return previous;
    }

    /**
     * Enable / Disable the automatic automatic selection check of the instrument configuration.
     * Return its previous value.
     *
     * Typical use is as following :
     * // disable the automatic selection check of the instrument configuration :
     * final boolean prevAutoCheckConfigurations = setAutoCheckConfigurations(false);
     * try {
     *   // operations ...
     *
     * } finally {
     *   // restore the automatic selection check of the instrument configuration :
     *   setAutoCheckConfigurations(prevAutoCheckConfiguration);
     * }
     *
     * @param value new value
     * @return previous value
     */
    private boolean setAutoCheckConfigurations(final boolean value) {
        // first backup the state of the automatic selection check :
        final boolean previous = doAutoCheckConfigurations;

        // then change its state :
        doAutoCheckConfigurations = value;

        // return previous state :
        return previous;
    }

    /**
     * Enable / Disable the automatic automatic selection check of the target list.
     * Return its previous value.
     *
     * Typical use is as following :
     * // disable the automatic selection check of the target list :
     * final boolean prevAutoCheckTargets = setAutoCheckTargets(false);
     * try {
     *   // operations ...
     *
     * } finally {
     *   // restore the automatic selection check of the target list :
     *   setAutoCheckTargets(prevAutoCheckTargets);
     * }
     *
     * @param value new value
     * @return previous value
     */
    private boolean setAutoCheckTargets(final boolean value) {
        // first backup the state of the automatic selection check :
        final boolean previous = doAutoCheckTargets;

        // then change its state :
        doAutoCheckTargets = value;

        // return previous state :
        return previous;
    }

    /**
     * Return the optional observation context of the main observation
     * @return observation context or null
     */
    private ObservationContext getObservationContext() {
        if (isGuiRestrictionEnabled()) {
            return om.getMainObservation().getContext();
        }
        return null;
    }

    private boolean isGuiRestrictionEnabled() {
        return Preferences.getInstance().getPreferenceAsBoolean(Preferences.GUI_RESTRICTIONS);
    }

    static int getPreferredHeight() {
        return Math.max(160, Math.min(320, SwingUtils.adjustUISize(160)));
    }
}
