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
import fr.jmmc.aspro.gui.util.TargetListRenderer;
import fr.jmmc.aspro.gui.util.TargetRenderer;
import fr.jmmc.aspro.gui.util.WindWidget;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.event.ObservationEvent;
import fr.jmmc.aspro.model.event.ObservationListener;
import fr.jmmc.aspro.model.event.UpdateObservationEvent;
import fr.jmmc.aspro.model.event.WarningContainerEvent;
import fr.jmmc.aspro.model.oi.FocalInstrumentConfigurationChoice;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerConfigurationChoice;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.ObservationContext;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.ObservationVariant;
import fr.jmmc.aspro.model.oi.Pop;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.jmal.star.Star;
import fr.jmmc.jmcs.App;
import fr.jmmc.jmcs.gui.component.GenericListModel;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.resource.image.ResourceImage;
import fr.jmmc.jmcs.util.StringUtils;
import fr.jmmc.jmcs.util.logging.ApplicationLogSingleton;
import java.awt.GridBagConstraints;
import java.awt.Insets;
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
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import java.util.Vector;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListSelectionModel;
import javax.swing.ImageIcon;
import javax.swing.JFormattedTextField;
import javax.swing.JList;
import javax.swing.JSpinner.DateEditor;
import javax.swing.ListSelectionModel;
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
public final class BasicObservationForm extends javax.swing.JPanel implements ChangeListener, ActionListener, Observer, ObservationListener {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(BasicObservationForm.class.getName());
  /** Logger */
  private static final Logger _warningLogger = ApplicationLogSingleton.getInstance().getLogger(AsproConstants.ASPRO_WARNING_LOG);
  /** flag to log a stack trace in method updateObservation() to detect multiple calls */
  private final static boolean DEBUG_UPDATE_EVENT = false;
  /** blanking value to indicate that PoPs are in use but in multi-configuration */
  private final static String POPS_MULTI_CONF = "PoPs_MULTI_CONF";
  /** configuration manager */
  private final static ConfigurationManager cm = ConfigurationManager.getInstance();
  /** observation manager */
  private final static ObservationManager om = ObservationManager.getInstance();

  /* members */
  /** Warning image icon */
  private ImageIcon warningIcon = null;
  /** flag to enable / disable the automatic update of the observation when any swing component changes */
  private boolean doAutoUpdateObservation = true;
  /** flag to enable / disable the automatic selection check of the target list */
  private boolean doAutoCheckTargets = true;
  /** flag to enable / disable the automatic selection check of the instrument configuration */
  private boolean doAutoCheckConfigurations = true;
  /** current selected target to avoid empty list selection */
  private Target currentTarget = null;
  /** current selected instrument configuration to avoid empty list selection */
  private String currentInstrumentConfiguration = null;
  /** last Pop config given by the interferometer configuration */
  private String lastConfPopConfig = null;
  /** Wind widget */
  private WindWidget windWidget = null;

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
    starSearchField = new fr.jmmc.jmal.star.EditableStarResolverWidget();
    jScrollPaneTargets = new javax.swing.JScrollPane();
    jListTargets = createTargetList();
    jButtonDeleteTarget = new javax.swing.JButton();
    jButtonTargetEditor = new javax.swing.JButton();
    jPanelTargetsRight = new javax.swing.JPanel();
    jButtonSkyCalc = new javax.swing.JButton();
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
    jPanelObsRight = new javax.swing.JPanel();
    jPanelObsBottom = new javax.swing.JPanel();
    jPanelConfigurations = new javax.swing.JPanel();
    jScrollPaneInstrumentConfigurations = new javax.swing.JScrollPane();
    jListInstrumentConfigurations = new javax.swing.JList();
    jPanelConfLeft = new javax.swing.JPanel();
    jPanelConfRight = new javax.swing.JPanel();
    jPanelOptions = new javax.swing.JPanel();
    jCheckBoxNightLimit = new javax.swing.JCheckBox();
    jLabelDate = new javax.swing.JLabel();
    jDateSpinner = new javax.swing.JSpinner();
    jLabelMinElev = new javax.swing.JLabel();
    jFieldMinElev = new javax.swing.JFormattedTextField();
    jCheckBoxWind = new javax.swing.JCheckBox();
    jPanelStatus = new javax.swing.JPanel();
    jLabelState = new javax.swing.JLabel();
    jLabelStatus = new javax.swing.JLabel();

    setLayout(new java.awt.GridBagLayout());

    jPanelTargets.setBorder(javax.swing.BorderFactory.createTitledBorder("Targets"));
    jPanelTargets.setName("jPanelTargets"); // NOI18N
    jPanelTargets.setLayout(new java.awt.GridBagLayout());
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.gridheight = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.1;
    jPanelTargets.add(jPanelTargetsLeft, gridBagConstraints);

    starSearchField.setToolTipText("<html>\nEnter targets here :<br>\nTarget identifier (CDS Simbad service)<br>\nor RA / DEC coordinates (J2000) with optional star name:<br>\n'H:M:S [+/-]D:M:S [star name]'\n</html>");
    starSearchField.setName("starSearchField"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.gridwidth = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.weightx = 0.8;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
    jPanelTargets.add(starSearchField, gridBagConstraints);

    jListTargets.setModel(new javax.swing.AbstractListModel() {
      String[] strings = { "Samples:", "HD 1234", "HIP 1234" };
      public int getSize() { return strings.length; }
      public Object getElementAt(int i) { return strings[i]; }
    });
    jListTargets.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
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
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 6);
    jPanelTargets.add(jScrollPaneTargets, gridBagConstraints);

    jButtonDeleteTarget.setIcon(new javax.swing.ImageIcon(getClass().getResource("/fr/jmmc/aspro/gui/icons/delete.png"))); // NOI18N
    jButtonDeleteTarget.setToolTipText("delete the selected target");
    jButtonDeleteTarget.setMargin(new java.awt.Insets(0, 0, 0, 0));
    jButtonDeleteTarget.setName("jButtonDeleteTarget"); // NOI18N
    jButtonDeleteTarget.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonDeleteTargetActionPerformed(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHWEST;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
    jPanelTargets.add(jButtonDeleteTarget, gridBagConstraints);

    jButtonTargetEditor.setText("Target editor");
    jButtonTargetEditor.setMargin(new java.awt.Insets(0, 0, 0, 0));
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
    jPanelTargets.add(jButtonTargetEditor, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.gridheight = java.awt.GridBagConstraints.RELATIVE;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.1;
    jPanelTargets.add(jPanelTargetsRight, gridBagConstraints);

    jButtonSkyCalc.setText("sky");
    jButtonSkyCalc.setName("jButtonSkyCalc"); // NOI18N
    jButtonSkyCalc.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonSkyCalcActionPerformed(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
    jPanelTargets.add(jButtonSkyCalc, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridheight = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.3;
    gridBagConstraints.weighty = 1.0;
    add(jPanelTargets, gridBagConstraints);

    jPanelMain.setBorder(javax.swing.BorderFactory.createTitledBorder("Main settings"));
    jPanelMain.setName("jPanelMain"); // NOI18N
    jPanelMain.setLayout(new java.awt.GridBagLayout());
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.gridheight = 4;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.1;
    jPanelMain.add(jPanelObsLeft, gridBagConstraints);

    jLabelInterferometer.setLabelFor(jComboBoxInterferometer);
    jLabelInterferometer.setText("Interferometer");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 6);
    jPanelMain.add(jLabelInterferometer, gridBagConstraints);

    jComboBoxInterferometer.setName("jComboBoxInterferometer"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.weightx = 0.5;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
    jPanelMain.add(jComboBoxInterferometer, gridBagConstraints);

    jLabelPeriod.setText("Period");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 6);
    jPanelMain.add(jLabelPeriod, gridBagConstraints);

    jComboBoxInterferometerConfiguration.setName("jComboBoxInterferometerConfiguration"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
    jPanelMain.add(jComboBoxInterferometerConfiguration, gridBagConstraints);

    jLabelInstrument.setLabelFor(jComboBoxInstrument);
    jLabelInstrument.setText("Instrument");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 6);
    jPanelMain.add(jLabelInstrument, gridBagConstraints);

    jComboBoxInstrument.setName("jComboBoxInstrument"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
    jPanelMain.add(jComboBoxInstrument, gridBagConstraints);

    jLabelPops.setLabelFor(jTextPoPs);
    jLabelPops.setText("PoPs");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 3;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 6);
    jPanelMain.add(jLabelPops, gridBagConstraints);

    jTextPoPs.setColumns(6);
    jTextPoPs.setToolTipText("<html>\ndefine a specific PoPs combination (PoP 1 to 5) by giving the list of PoP numbers<br>\nin the same order than stations of the selected base line. For example:<ul>\n<li>VEGA_2T with baseline S1-S2<br>'34' means PoP3 on S1 and PoP4 on S2</li>\n<li>MIRC (4T) with baseline S1-S2-E1-W2<br>'1255' means PoP1 on S1, PoP2 on S2 and Pop5 on E1 and W2</li>\n</ul>\n<b>If you leave this field blank, ASPRO 2 will compute the 'best PoP' combination<br>\nmaximizing the observability of your complete list of targets</b>\n</html>");
    jTextPoPs.setMinimumSize(new java.awt.Dimension(60, 20));
    jTextPoPs.setName("jTextPoPs"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 3;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
    jPanelMain.add(jTextPoPs, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.gridheight = 4;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.1;
    jPanelMain.add(jPanelObsRight, gridBagConstraints);

    jPanelObsBottom.setPreferredSize(new java.awt.Dimension(1, 1));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 4;
    gridBagConstraints.gridwidth = 4;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weighty = 1.0;
    jPanelMain.add(jPanelObsBottom, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.gridheight = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.2;
    add(jPanelMain, gridBagConstraints);

    jPanelConfigurations.setBorder(javax.swing.BorderFactory.createTitledBorder("Configuration(s)"));
    jPanelConfigurations.setName("jPanelConfigurations"); // NOI18N
    jPanelConfigurations.setLayout(new java.awt.GridBagLayout());

    jScrollPaneInstrumentConfigurations.setPreferredSize(new java.awt.Dimension(120, 50));

    jListInstrumentConfigurations.setModel(new javax.swing.AbstractListModel() {
      String[] strings = { "Samples:", "UT1 UT2 UT3 UT4", "E1 E2 W1 W2" };
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
    gridBagConstraints.weightx = 0.8;
    gridBagConstraints.weighty = 1.0;
    jPanelConfigurations.add(jScrollPaneInstrumentConfigurations, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.1;
    jPanelConfigurations.add(jPanelConfLeft, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.1;
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
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.gridwidth = 3;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_END;
    jPanelOptions.add(jCheckBoxNightLimit, gridBagConstraints);

    jLabelDate.setLabelFor(jDateSpinner);
    jLabelDate.setText("Date");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 6);
    jPanelOptions.add(jLabelDate, gridBagConstraints);

    jDateSpinner.setModel(new javax.swing.SpinnerDateModel());
    jDateSpinner.setEditor(new javax.swing.JSpinner.DateEditor(jDateSpinner, "yyyy/MM/dd"));
    jDateSpinner.setName("jDateSpinner"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.ipadx = 2;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
    jPanelOptions.add(jDateSpinner, gridBagConstraints);

    jLabelMinElev.setLabelFor(jFieldMinElev);
    jLabelMinElev.setText("Min. Elevation");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 6);
    jPanelOptions.add(jLabelMinElev, gridBagConstraints);

    jFieldMinElev.setColumns(2);
    jFieldMinElev.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0"))));
    jFieldMinElev.setName("jFieldMinElev"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.anchor = java.awt.GridBagConstraints.LINE_START;
    jPanelOptions.add(jFieldMinElev, gridBagConstraints);

    jCheckBoxWind.setText("Wind");
    jCheckBoxWind.setHorizontalTextPosition(javax.swing.SwingConstants.LEADING);
    jCheckBoxWind.setIconTextGap(6);
    jCheckBoxWind.setName("jCheckBoxWind"); // NOI18N
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 2;
    gridBagConstraints.gridy = 2;
    jPanelOptions.add(jCheckBoxWind, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 0.2;
    gridBagConstraints.weighty = 1.0;
    add(jPanelOptions, gridBagConstraints);

    jPanelStatus.setMinimumSize(new java.awt.Dimension(57, 30));
    jPanelStatus.setName("jPanelStatus"); // NOI18N
    jPanelStatus.setPreferredSize(new java.awt.Dimension(100, 30));
    jPanelStatus.setLayout(new java.awt.GridBagLayout());

    jLabelState.setText("Status : ");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 4);
    jPanelStatus.add(jLabelState, gridBagConstraints);

    jLabelStatus.setText("Ok");
    jLabelStatus.setName("jLabelStatus"); // NOI18N
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
    gridBagConstraints.weighty = 1.0;
    jPanelStatus.add(jLabelStatus, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weighty = 0.1;
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

  /**
   * Process the remove target action
   * @param evt action event
   */
  private void jButtonDeleteTargetActionPerformed(java.awt.event.ActionEvent evt) {

    // TODO : multi selection of targets to delete multiple targets at the same time :

    final Target selectedTarget = getSelectedTarget();

    if (selectedTarget != null) {

      if (om.isCalibrator(selectedTarget)) {
        if (MessagePane.showConfirmMessage(this.jButtonDeleteTarget,
                "Do you want to delete the calibrator target [" + selectedTarget.getName() + "] and all associations ?")) {

          // update the data model and fire change events :
          om.removeCalibrator(selectedTarget);
        }
      } else if (MessagePane.showConfirmMessage(this.jButtonDeleteTarget,
              "Do you want to delete the science target [" + selectedTarget.getName() + "] ?")) {

        // update the data model and fire change events :
        om.removeTarget(selectedTarget);
      }
    }
  }

  private void jButtonTargetEditorActionPerformed(java.awt.event.ActionEvent evt) {
    showTargetEditor();
  }

  private void jListInstrumentConfigurationsValueChanged(javax.swing.event.ListSelectionEvent evt) {
    this.processInstrumentConfigurationValueChanged(evt);
  }

  private void jListTargetsValueChanged(javax.swing.event.ListSelectionEvent evt) {
    this.processTargetValueChanged(evt);
  }

  private void jLabelStatusMouseClicked(java.awt.event.MouseEvent evt) {
    App.showLogConsole(AsproConstants.ASPRO_WARNING_LOG);
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

    Preferences.getInstance().addObserver(this);

    this.warningIcon = ResourceImage.WARNING_ICON.icon();

    // add observer to the StarResolverWidget :
    this.starSearchField.getStar().addObserver(this);

    // update component models :
    final DateEditor de = (DateEditor) this.jDateSpinner.getEditor();

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
              // select the day field to force the spinner to use it
              textComponent.setCaretPosition(last - 2);
              textComponent.moveCaretPosition(last);
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
    this.jDateSpinner.addChangeListener(this);
    this.jComboBoxInterferometer.addActionListener(this);
    this.jComboBoxInterferometerConfiguration.addActionListener(this);
    this.jComboBoxInstrument.addActionListener(this);

    this.jTextPoPs.addPropertyChangeListener("value", new PropertyChangeListener() {
      @Override
      public void propertyChange(final PropertyChangeEvent evt) {
        jTextPoPsPropertyChange(evt);
      }
    });

    this.jFieldMinElev.addPropertyChangeListener("value", new PropertyChangeListener() {
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

    this.jCheckBoxNightLimit.addItemListener(new ItemListener() {
      @Override
      public void itemStateChanged(final ItemEvent e) {
        updateWindRestriction();
        fireObservationUpdateEvent();
      }
    });

    // define interferometer names (constant) :
    this.jComboBoxInterferometer.setModel(new DefaultComboBoxModel(cm.getInterferometerNames()));

    // reset status :
    this.resetStatus();

    this.windWidget = WindWidget.create();

    final GridBagConstraints gridBagConstraints = new GridBagConstraints();
    gridBagConstraints.gridx = 3;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.gridheight = 3;
    gridBagConstraints.fill = GridBagConstraints.BOTH;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.insets = new Insets(2, 2, 2, 2);
    gridBagConstraints.weightx = 0.9;
    gridBagConstraints.weighty = 0.9;
    this.jPanelOptions.add(this.windWidget, gridBagConstraints);

    this.windWidget.addPropertyChangeListener(WindWidget.PROPERTY_VALUE, new PropertyChangeListener() {
      public void propertyChange(final PropertyChangeEvent pe) {
        fireObservationUpdateEvent();
      }
    });

    this.jCheckBoxWind.addItemListener(new ItemListener() {
      @Override
      public void itemStateChanged(final ItemEvent e) {
        windWidget.setEnabled(jCheckBoxWind.isSelected());
        fireObservationUpdateEvent();
      }
    });

    this.jCheckBoxWind.setSelected(false);
    this.windWidget.setEnabled(false);
  }

  /**
   * Process the date spinner change event.
   * Update the observation according to the form state
   * @param ce change event
   */
  @Override
  public void stateChanged(final ChangeEvent ce) {
    if (ce.getSource() == this.jDateSpinner) {
      if (logger.isDebugEnabled()) {
        logger.debug("Date changed: {}", this.jDateSpinner.getModel().getValue());
      }
      fireObservationUpdateEvent();
    }
  }

  /**
   * Refresh the list of interferometer configurations : depends on the chosen interferometer
   */
  private void updateComboInterferometerConfiguration() {
    final Vector<String> v = cm.getInterferometerConfigurationNames((String) this.jComboBoxInterferometer.getSelectedItem());
    this.jComboBoxInterferometerConfiguration.setModel(new DefaultComboBoxModel(v));
    final boolean visible = (v.size() > 1);
    this.jLabelPeriod.setVisible(visible);
    this.jComboBoxInterferometerConfiguration.setVisible(visible);
  }

  /**
   * Refresh the list of instruments : depends on the chosen interferometer configuration
   */
  private void updateComboInstrument() {
    final Object oldValue = this.jComboBoxInstrument.getSelectedItem();

    final Vector<String> v = cm.getInterferometerInstrumentNames((String) this.jComboBoxInterferometerConfiguration.getSelectedItem());
    this.jComboBoxInstrument.setModel(new DefaultComboBoxModel(v));

    // restore previous selected item :
    if (oldValue != null) {
      this.jComboBoxInstrument.setSelectedItem(oldValue);
    }
  }

  /**
   * Refresh the list of instrument configurations : depends on the chosen instrument (also the interferometer configuration)
   */
  private void updateComboInstrumentConfiguration() {
    final Vector<String> v = cm.getInstrumentConfigurationNames((String) this.jComboBoxInterferometerConfiguration.getSelectedItem(),
            (String) this.jComboBoxInstrument.getSelectedItem());

    final Object[] oldValues = getInstrumentConfigurations();

    // disable the automatic selection check of the instrument configuration :
    final boolean prevAutoCheckConfigurations = this.setAutoCheckConfigurations(false);
    try {
      this.jListInstrumentConfigurations.setModel(new GenericListModel<String>(v));

      // restore previous selected item(s) :
      this.selectInstrumentConfigurations(oldValues);

    } finally {
      // restore the automatic selection check of the instrument configuration :
      this.setAutoCheckConfigurations(prevAutoCheckConfigurations);
    }
    // ensure one configuration is selected :
    this.checkInstrumentConfigurationSelection();
  }

  /**
   * Return the list model of the instrument configuration list
   * @return list model
   */
  @SuppressWarnings("unchecked")
  private GenericListModel<String> getInstrumentConfigurationModel() {
    return (GenericListModel<String>) this.jListInstrumentConfigurations.getModel();
  }

  /**
   * Refresh the target list
   */
  private void updateListTargets() {
    final Target selectedTarget = getSelectedTarget();

    final List<Target> displayTargets = om.getDisplayTargets();
    final TargetUserInformations targetUserInfos = om.getTargetUserInfos();

    // disable the automatic selection check of the target list :
    final boolean prevAutoCheckTargets = this.setAutoCheckTargets(false);
    try {
      this.jListTargets.setModel(new GenericListModel<Target>(displayTargets));
      this.jListTargets.setCellRenderer(new TargetListRenderer(new TargetRenderer(targetUserInfos)));

      // restore previous selected item :
      if (selectedTarget != null) {
        this.jListTargets.setSelectedValue(selectedTarget, true);
      }

      if (isTargetEditable()) {
        // disable buttons if the target list is empty :
        this.jButtonDeleteTarget.setEnabled(!displayTargets.isEmpty());
        this.jButtonTargetEditor.setEnabled(!displayTargets.isEmpty());
      }
    } finally {
      // restore the automatic selection check of the target list :
      this.setAutoCheckTargets(prevAutoCheckTargets);
    }
    // ensure one target is selected :
    this.checkTargetSelection();

    if (logger.isDebugEnabled()) {
      logger.debug("jListTargets updated: {}", getSelectedTarget());
    }
  }

  /**
   * Return the currently selected target
   * @return target
   */
  public Target getSelectedTarget() {
    // TODO : multi selection of targets to delete multiple targets at the same time :
    return (Target) this.jListTargets.getSelectedValue();
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

    // ensure at least one item is selected :
    if (this.jListTargets.getSelectionModel().isSelectionEmpty()) {
      this.checkTargetSelection();
      return;
    }

    // check if selection changes :
    if (this.currentTarget == null || this.currentTarget != getSelectedTarget()) {
      // memorize the selected item :
      this.currentTarget = getSelectedTarget();

      // handle single selection :

      if (logger.isDebugEnabled()) {
        logger.debug("Selected Target changed: {}", getSelectedTarget());
      }

      // update observation :
      fireTargetSelectionChangeEvent();
    }
  }

  /**
   * Check if the selected target is empty, then restore the last selected target
   * or select the first target
   */
  private void checkTargetSelection() {
    // check if the automatic configuration check flag is enabled :
    if (this.doAutoCheckTargets) {
      checkListSelection(this.jListTargets, this.currentTarget);
    }
  }

  /**
   * Process any comboBox change event (interferometer, interferometer configuration, instrument, instrument configuration).
   * Refresh the dependent combo boxes and update the observation according to the form state
   * @param e action event
   */
  @Override
  public void actionPerformed(final ActionEvent e) {
    // disable the automatic update observation :
    final boolean prevAutoUpdateObservation = this.setAutoUpdateObservation(false);
    try {
      if (e.getSource() == this.jComboBoxInterferometer) {
        if (logger.isDebugEnabled()) {
          logger.debug("Interferometer changed: {}", this.jComboBoxInterferometer.getSelectedItem());
        }
        updateComboInterferometerConfiguration();
        updateComboInstrument();
        updateComboInstrumentConfiguration();
        checkPops();
        updateWindRestriction();

      } else if (e.getSource() == this.jComboBoxInterferometerConfiguration) {
        if (logger.isDebugEnabled()) {
          logger.debug("Interferometer Configuration changed: {}", this.jComboBoxInterferometerConfiguration.getSelectedItem());
        }
        updateComboInstrument();
        updateComboInstrumentConfiguration();
        checkPops();
      } else if (e.getSource() == this.jComboBoxInstrument) {
        if (logger.isDebugEnabled()) {
          logger.debug("Instrument changed: {}", this.jComboBoxInstrument.getSelectedItem());
        }
        updateComboInstrumentConfiguration();
        checkPops();
      }

    } finally {
      // restore the automatic update observation :
      this.setAutoUpdateObservation(prevAutoUpdateObservation);
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

    final ListSelectionModel lsm = this.jListInstrumentConfigurations.getSelectionModel();

    // ensure at least one item is selected :
    if (lsm.isSelectionEmpty()) {
      this.checkInstrumentConfigurationSelection();
      return;
    }

    // memorize the first selected item :
    this.currentInstrumentConfiguration = (String) this.jListInstrumentConfigurations.getSelectedValue();

    if (logger.isDebugEnabled()) {
      logger.debug("Instrument Configuration changed: {}", Arrays.toString(getInstrumentConfigurations()));
    }

    // update PoPs text field if the selected instrument configurations have associated PoPs in the configuration:
    updatePops();

    // group multiple calls into a single observation update event :
    fireObservationUpdateEvent();
  }

  /**
   * Return the selected instrument configurations
   * @return selected instrument configurations
   */
  private Object[] getInstrumentConfigurations() {
    return this.jListInstrumentConfigurations.getSelectedValues();
  }

  /**
   * Check if the selected instrument configuration is empty, then restore the last selected configuration
   * or select the first configuration
   */
  private void checkInstrumentConfigurationSelection() {
    // check if the automatic configuration check flag is enabled :
    if (this.doAutoCheckConfigurations) {
      checkListSelection(this.jListInstrumentConfigurations, this.currentInstrumentConfiguration);
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
      final DefaultListSelectionModel lsm = (DefaultListSelectionModel) this.jListInstrumentConfigurations.getSelectionModel();

      int index = -1;
      for (Object selection : values) {
        index = lm.indexOf((String) selection);
        if (index != -1) {
          lsm.addSelectionInterval(index, index);
          this.jListInstrumentConfigurations.ensureIndexIsVisible(index);
        }
      }
      if (index != -1) {
        // scroll to last selected value :
        this.jListInstrumentConfigurations.ensureIndexIsVisible(index - 1);
        this.jListInstrumentConfigurations.ensureIndexIsVisible(index);
        this.jListInstrumentConfigurations.ensureIndexIsVisible(index + 1);
      }
      if (logger.isDebugEnabled()) {
        logger.debug("selectInstrumentConfigurations : selectedValues: {}", Arrays.toString(getInstrumentConfigurations()));
      }
    }
  }

  /**
   * Check if this interferometer has PoPs. If false, disable and reset the PoPs text field
   */
  private void checkPops() {
    final boolean hasPops = cm.hasPoPs((String) this.jComboBoxInterferometer.getSelectedItem());

    // note: label pops is only visible if the interferometer has Pops:
    this.jLabelPops.setVisible(hasPops);

    this.jTextPoPs.setVisible(hasPops);

    // reset the pops configuration anyway because it can be invalid because of the chosen instrument:
    resetPops();
  }

  /**
   * Process the change event for the PoPs configuration text field.
   * Validates the new input (digits corresponds to valid PoPs indices)
   * @param evt property change event
   */
  public void jTextPoPsPropertyChange(final PropertyChangeEvent evt) {
    List<Pop> listPoPs = null;
    final Object value = evt.getNewValue();

    if (value != null) {
      final String popConfig = value.toString();

      // parse the configuration (instrument = number of channels) + (interferometer = pop indexes [1-5]) :
      listPoPs = cm.parseInstrumentPoPs((String) this.jComboBoxInterferometerConfiguration.getSelectedItem(),
              (String) this.jComboBoxInstrument.getSelectedItem(), popConfig);
    }

    logger.debug("Pops changed = {} : {}", value, listPoPs);

    if (listPoPs == null && value != null) {
      // invalid, reset the field to empty :
      resetPops();
    }

    // then update the observation :
    fireObservationUpdateEvent();
  }

  /**
   * Reset PoPs text field
   */
  private void resetPops() {
    // disable the automatic update observation :
    final boolean prevAutoUpdateObservation = this.setAutoUpdateObservation(false);
    try {
      // enable PoPs only if the interferometer support it:
      if (this.jLabelPops.isVisible()) {
        final String popConfig = getConfigurationPops();
        final boolean popMulti = POPS_MULTI_CONF.equals(popConfig);

        final Integer value = (popConfig != null && !popMulti) ? Integer.valueOf(popConfig) : null;

        // note : setValue() can fire a property change event :
        this.jTextPoPs.setValue(value);
        // allow user inputs when no PoPs are defined in the configuration and not in multi-conf:
        this.jTextPoPs.setEnabled(isPopsEditable() && (value == null || popMulti));
      } else {
        // note : setValue() can fire a property change event :
        this.jTextPoPs.setValue(null);
        this.jTextPoPs.setEnabled(false);
      }
      this.lastConfPopConfig = null;

    } finally {
      // restore the automatic update observation :
      this.setAutoUpdateObservation(prevAutoUpdateObservation);
    }
  }

  /**
   * Update the PoPs text field using the PoPs defined in the current instrument configurations
   * only if one and only one configuration is selected.
   */
  private void updatePops() {
    // handle PoPs only if the interferometer support it:
    if (this.jLabelPops.isVisible()) {
      final String popConfig = getConfigurationPops();

      // get last Pops defined by the instrument configuration:
      final String lastPopConfig = this.lastConfPopConfig;
      this.lastConfPopConfig = null;

      logger.debug("updatePops: {}, last: {}", popConfig, lastPopConfig);

      if (popConfig != null) {
        // update the selected pops (pops) :
        if (POPS_MULTI_CONF.equals(popConfig)) {
          // note : setValue() can fire a property change event :
          this.jTextPoPs.setValue(null);
          this.jTextPoPs.setEnabled(isPopsEditable());
        } else {
          // note : setText() does not fire a property change event :
          this.jTextPoPs.setText(popConfig);
          this.jTextPoPs.setEnabled(false);
          this.lastConfPopConfig = popConfig;
        }
      } else {
        // reset the predefined pops:
        if (lastPopConfig != null) {
          // note : setValue() can fire a property change event :
          this.jTextPoPs.setValue(null);
          this.jTextPoPs.setEnabled(isPopsEditable());
        }
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
      return this.jLabelPops.isVisible() ? POPS_MULTI_CONF : null;
    }
    if (instConfs.length == 1) {
      // determine if there are PoPs defined in the instrument configuration :
      final String instrumentConfiguration = (String) instConfs[0];
      final List<Pop> popList = cm.getInstrumentConfigurationPoPs((String) this.jComboBoxInterferometerConfiguration.getSelectedItem(),
              (String) this.jComboBoxInstrument.getSelectedItem(), instrumentConfiguration);

      if (popList != null && !popList.isEmpty()) {
        // PoPs are defined in the instrument configuration :
        return Pop.toString(popList);
      }
    }
    return null;
  }

  /**
   * Enable or disable the wind restriction depending on the chosen interferometer
   */
  private void updateWindRestriction() {
    final Double windRestriction = cm.getWindPointingRestriction((String) this.jComboBoxInterferometer.getSelectedItem());

    // wind restriction is enabled only if night restriction are enabled and interferometer support it:
    final boolean useWind = this.jCheckBoxNightLimit.isSelected()
            && (windRestriction != null && windRestriction > 0d && windRestriction < 180d);

    if (!useWind) {
      // reset
      this.jCheckBoxWind.setSelected(false);
    }

    this.jCheckBoxWind.setEnabled(useWind);
  }

  /**
   * Observer implementation used for the StarResolver (called by EDT)
   * Create a new Target object with the retrieved data from Simbad and
   * fire an observation change event
   * @param o Observable instance i.e. Star instance
   * @param arg Star.Notification instance
   */
  @Override
  public void update(final Observable o, final Object arg) {
    if (o instanceof Star) {
      final Star star = (Star) o;

      final Star.Notification notification = (Star.Notification) arg;

      if (notification == Star.Notification.QUERY_COMPLETE) {
        logger.debug("Star resolved: \n{}", star);

        // update the data model and fire change events :
        om.addTarget(Target.formatName(star.getName()), star);
      }
    } else if (o instanceof Preferences) {
      // means Preferences:
// disabled because it is also updated when any preference changes !!!
//      this.jFieldMinElev.setValue(Preferences.getInstance().getPreferenceAsDouble(Preferences.MIN_ELEVATION));
    }
  }

  /**
   * Fire an Observation Change event when a Swing component changed
   * ONLY if the automatic update flag is enabled
   */
  private void fireObservationUpdateEvent() {
    // check if the automatic update flag is enabled :
    if (this.doAutoUpdateObservation) {
      logger.debug("fireObservationUpdateEvent");

      if (DEBUG_UPDATE_EVENT) {
        logger.warn("FIRE_UPDATE", new Throwable());
      }

      ObservationManager.getInstance().fireObservationUpdate();
    }
  }

  /**
   * Fire a Target Selection Change event when the target selection changes.
   */
  private void fireTargetSelectionChangeEvent() {

    final Target selected = getSelectedTarget();

    logger.debug("fireTargetSelectionChangeEvent : target = {}", selected);

    ObservationManager.getInstance().fireTargetSelectionChanged(selected);
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
    final boolean prevAutoUpdateObservation = this.setAutoUpdateObservation(false);
    // disable the automatic selection check of the instrument configuration :
    final boolean prevAutoCheckConfigurations = this.setAutoCheckConfigurations(false);
    // disable the automatic selection check of the target list :
    final boolean prevAutoCheckTargets = this.setAutoCheckTargets(false);
    try {
      // clear selected target :
      this.jListTargets.clearSelection();
      // reset cached values :
      this.currentTarget = null;
      this.currentInstrumentConfiguration = null;
      this.lastConfPopConfig = null;

      // use observation context to enable/disable POPS FIRST (event ordering issue):
      this.jTextPoPs.setEnabled(isPopsEditable());

      // observation :

      // update the interferometer and interferometer configuration :
      final InterferometerConfigurationChoice interferometerChoice = observation.getInterferometerConfiguration();

      final InterferometerConfiguration ic = interferometerChoice.getInterferometerConfiguration();

      if (ic != null) {
        // update the selected interferometer :
        this.jComboBoxInterferometer.setSelectedItem(ic.getInterferometer().getName());
        // update the selected interferometer configuration :
        this.jComboBoxInterferometerConfiguration.setSelectedItem(ic.getName());
      }

      final FocalInstrumentConfigurationChoice instrumentChoice = observation.getInstrumentConfiguration();

      // update the selected instrument :
      this.jComboBoxInstrument.setSelectedItem(instrumentChoice.getName());

      // update the selected instrument configurations :
      final List<ObservationVariant> obsVariants = observation.getVariants();
      final int len = obsVariants.size();
      final Object[] stationConfs = new Object[len];
      for (int i = 0; i < len; i++) {
        stationConfs[i] = obsVariants.get(i).getStations();
      }

      this.jListInstrumentConfigurations.clearSelection();
      this.selectInstrumentConfigurations(stationConfs);

      // update the selected pops (pops) :
      // note : setText() does not fire a property change event :
      this.jTextPoPs.setText(instrumentChoice.getPops());

      // constraints :
      // update the night restriction :
      this.jCheckBoxNightLimit.setSelected(observation.getWhen().isNightRestriction());

      // update the date spinner :
      final XMLGregorianCalendar date = observation.getWhen().getDate();
      if (date != null) {
        this.jDateSpinner.setValue(date.toGregorianCalendar().getTime());
      }

      // Update wind direction:
      this.windWidget.setEnabled(true); // to update its value
      final Double windAz = observation.getWhen().getWindAzimuth();
      final boolean useWind = windAz != null;
      this.windWidget.setValue((useWind) ? windAz.doubleValue() : 0d);
      this.windWidget.setEnabled(useWind);
      this.jCheckBoxWind.setSelected(useWind);

      // update the min elevation :
      this.jFieldMinElev.setValue(interferometerChoice.getMinElevation());

    } finally {
      // restore the automatic selection check of the target list :
      this.setAutoCheckTargets(prevAutoCheckTargets);
      // restore the automatic selection check of the instrument configuration :
      this.setAutoCheckConfigurations(prevAutoCheckConfigurations);
      // restore the automatic update observation :
      this.setAutoUpdateObservation(prevAutoUpdateObservation);
    }
    // ensure one configuration is selected :
    this.checkInstrumentConfigurationSelection();

    // use observation context to enable/disable GUI features:
    final ObservationContext ctx = getObservationContext();

    if (ctx != null) {
      // Main settings:
      this.jComboBoxInterferometer.setEnabled(ctx.isInterferometerEditable());
      this.jComboBoxInterferometerConfiguration.setEnabled(ctx.isPeriodEditable());
      this.jComboBoxInstrument.setEnabled(ctx.isInstrumentEditable());

      // Configuration(s):
      this.jListInstrumentConfigurations.setEnabled(ctx.isConfigurationsEditable());

      // Constraints:
      this.jCheckBoxNightLimit.setEnabled(ctx.isNightEditable());
      this.jDateSpinner.setEnabled(ctx.isDateEditable());
      this.jFieldMinElev.setEnabled(ctx.isMinElevationEditable());

    } else {
      // reset GUI:

      // Main settings:
      this.jComboBoxInterferometer.setEnabled(true);
      this.jComboBoxInterferometerConfiguration.setEnabled(true);
      this.jComboBoxInstrument.setEnabled(true);

      // Configuration(s):
      this.jListInstrumentConfigurations.setEnabled(true);

      // Constraints:
      this.jCheckBoxNightLimit.setEnabled(true);
      this.jDateSpinner.setEnabled(true);
      this.jFieldMinElev.setEnabled(true);
    }

    // TARGETS:
    final boolean targetEditable = isTargetEditable();

    this.starSearchField.setEnabled(targetEditable);
    this.jListTargets.setEnabled(targetEditable);
    this.jButtonTargetEditor.setEnabled(targetEditable);
    this.jButtonDeleteTarget.setEnabled(targetEditable);
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
    if (this.doAutoUpdateObservation) {
      if (DEBUG_UPDATE_EVENT) {
        logger.warn("UPDATE", new Throwable());
      }

      boolean changed = false;

      // observation :
      changed |= om.setInterferometerConfigurationName((String) this.jComboBoxInterferometerConfiguration.getSelectedItem());
      changed |= om.setInstrumentConfigurationName((String) this.jComboBoxInstrument.getSelectedItem());

      changed |= om.setInstrumentConfigurationStations(getInstrumentConfigurations());

      changed |= om.setInstrumentConfigurationPoPs(this.jTextPoPs.getText());

      // constraints :
      changed |= om.setWhen((Date) this.jDateSpinner.getModel().getValue());
      changed |= om.setMinElevation(((Number) this.jFieldMinElev.getValue()).doubleValue());
      changed |= om.setNightRestriction(this.jCheckBoxNightLimit.isSelected());

      if (this.jCheckBoxWind.isSelected()) {
        changed |= om.setWindAzimuth(Double.valueOf(this.windWidget.getValue()));
      } else {
        changed |= om.setWindAzimuth(null);
      }

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
      case LOADED:
        this.onLoadObservation(event.getObservation());
        break;
      case TARGET_CHANGED:
        this.updateListTargets();
        break;
      case DO_UPDATE:
        if (event instanceof UpdateObservationEvent) {
          this.onUpdateObservation((UpdateObservationEvent) event);
        }
        break;
      case REFRESH:
        this.resetStatus();
        break;
      case REFRESH_UV:
        this.resetStatus();
        break;
      case WARNINGS_READY:
        if (event instanceof WarningContainerEvent) {
          this.updateStatus(((WarningContainerEvent) event).getWarningContainer());
        }
        break;
      default:
    }
    if (logger.isDebugEnabled()) {
      logger.debug("event [{}] process OUT", event.getType());
    }
  }

  /**
   * Reset status panel
   */
  private void resetStatus() {
    this.updateStatus(null);
  }

  /**
   * Update status panel
   * @param warningContainer warning container or null to reset content
   */
  private void updateStatus(final WarningContainer warningContainer) {
    if (warningContainer == null || !warningContainer.hasWarningMessages()) {
      // reset
      if (this.jLabelStatus.getIcon() != null) {
        this.jLabelStatus.setIcon(null);
        this.jLabelStatus.setText("Ok");
        this.jLabelStatus.setToolTipText(null);
      }
    } else {
      this.jLabelStatus.setIcon(this.warningIcon);
      this.jLabelStatus.setText("Warning");

      final StringBuilder sb = new StringBuilder(256);
      sb.append("<html>");
      for (String msg : warningContainer.getWarningMessages()) {
        sb.append(msg).append("<br>");

        // add warning to the warning log:
        _warningLogger.info(StringUtils.removeTags(msg));
      }
      sb.append("</html>");
      this.jLabelStatus.setToolTipText(sb.toString());
    }
  }
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JButton jButtonDeleteTarget;
  private javax.swing.JButton jButtonSkyCalc;
  private javax.swing.JButton jButtonTargetEditor;
  private javax.swing.JCheckBox jCheckBoxNightLimit;
  private javax.swing.JCheckBox jCheckBoxWind;
  private javax.swing.JComboBox jComboBoxInstrument;
  private javax.swing.JComboBox jComboBoxInterferometer;
  private javax.swing.JComboBox jComboBoxInterferometerConfiguration;
  private javax.swing.JSpinner jDateSpinner;
  private javax.swing.JFormattedTextField jFieldMinElev;
  private javax.swing.JLabel jLabelDate;
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
  private javax.swing.JPanel jPanelMain;
  private javax.swing.JPanel jPanelObsBottom;
  private javax.swing.JPanel jPanelObsLeft;
  private javax.swing.JPanel jPanelObsRight;
  private javax.swing.JPanel jPanelOptions;
  private javax.swing.JPanel jPanelStatus;
  private javax.swing.JPanel jPanelTargets;
  private javax.swing.JPanel jPanelTargetsLeft;
  private javax.swing.JPanel jPanelTargetsRight;
  private javax.swing.JScrollPane jScrollPaneInstrumentConfigurations;
  private javax.swing.JScrollPane jScrollPaneTargets;
  private javax.swing.JFormattedTextField jTextPoPs;
  private fr.jmmc.jmal.star.EditableStarResolverWidget starSearchField;
  // End of variables declaration//GEN-END:variables

  /**
   * Create the custom JList to support tooltips for targets
   * @return JList
   */
  private static JList createTargetList() {
    final JList list = new JList() {
      /** default serial UID for Serializable interface */
      private static final long serialVersionUID = 1;

      /** This method is called as the cursor moves within the list */
      @Override
      public String getToolTipText(final MouseEvent evt) {
        logger.trace("getToolTipText: {}", evt);

        // Get item index :
        final int index = locationToIndex(evt.getPoint());
        if (index != -1) {
          // Get target :
          final Target target = (Target) getModel().getElementAt(index);
          if (target != null) {
            // Return the tool tip text :
            return target.toHtml();
          }
        }
        return getToolTipText();
      }
    };

    final Target defTarget = new Target();
    defTarget.setName("HIP 1234");

    // Useful to define the empty list width and height :
    list.setPrototypeCellValue(defTarget);

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
   * final boolean prevAutoUpdateObservation = this.setAutoUpdateObservation(false);
   * try {
   *   // operations ...
   *
   * } finally {
   *   // restore the automatic update observation :
   *   this.setAutoUpdateObservation(prevAutoUpdateObservation);
   * }
   *
   * @param value new value
   * @return previous value
   */
  private boolean setAutoUpdateObservation(final boolean value) {
    // first backup the state of the automatic update observation :
    final boolean previous = this.doAutoUpdateObservation;

    // then change its state :
    this.doAutoUpdateObservation = value;

    // return previous state :
    return previous;
  }

  /**
   * Enable / Disable the automatic automatic selection check of the instrument configuration.
   * Return its previous value.
   *
   * Typical use is as following :
   * // disable the automatic selection check of the instrument configuration :
   * final boolean prevAutoCheckConfigurations = this.setAutoCheckConfigurations(false);
   * try {
   *   // operations ...
   *
   * } finally {
   *   // restore the automatic selection check of the instrument configuration :
   *   this.setAutoCheckConfigurations(prevAutoCheckConfiguration);
   * }
   *
   * @param value new value
   * @return previous value
   */
  private boolean setAutoCheckConfigurations(final boolean value) {
    // first backup the state of the automatic selection check :
    final boolean previous = this.doAutoCheckConfigurations;

    // then change its state :
    this.doAutoCheckConfigurations = value;

    // return previous state :
    return previous;
  }

  /**
   * Enable / Disable the automatic automatic selection check of the target list.
   * Return its previous value.
   *
   * Typical use is as following :
   * // disable the automatic selection check of the target list :
   * final boolean prevAutoCheckTargets = this.setAutoCheckTargets(false);
   * try {
   *   // operations ...
   *
   * } finally {
   *   // restore the automatic selection check of the target list :
   *   this.setAutoCheckTargets(prevAutoCheckTargets);
   * }
   *
   * @param value new value
   * @return previous value
   */
  private boolean setAutoCheckTargets(final boolean value) {
    // first backup the state of the automatic selection check :
    final boolean previous = this.doAutoCheckTargets;

    // then change its state :
    this.doAutoCheckTargets = value;

    // return previous state :
    return previous;
  }

  /**
   * Return the optional observation context of the main observation
   * @return observation context or null
   */
  private ObservationContext getObservationContext() {
    return om.getMainObservation().getContext();
  }
}
