/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: UVCoveragePanel.java,v 1.30 2010-04-08 14:06:51 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.29  2010/04/06 14:40:47  bourgesl
 * minor UI changes for mac os (II)
 *
 * Revision 1.28  2010/04/06 13:58:37  bourgesl
 * minor UI changes for mac os & other LAF
 *
 * Revision 1.27  2010/04/02 10:05:24  bourgesl
 * added OB for AMBER
 *
 * Revision 1.26  2010/03/30 12:10:33  bourgesl
 * disable model image widgets for targets without any defined model
 *
 * Revision 1.25  2010/02/19 16:06:08  bourgesl
 * added image size & LUT combo boxes
 *
 * Revision 1.24  2010/02/18 15:52:38  bourgesl
 * added parameter argument validation with an user message
 *
 * Revision 1.23  2010/02/17 15:13:18  bourgesl
 * image mode disabled if plot image unchecked
 *
 * Revision 1.22  2010/02/16 14:48:26  bourgesl
 * if the model editor was successfull (ok), update the plots
 *
 * Revision 1.21  2010/02/15 16:47:26  bourgesl
 * model editor supports add / remove model
 *
 * Revision 1.20  2010/02/12 15:53:18  bourgesl
 * added target model editor
 *
 * Revision 1.19  2010/02/09 16:51:09  bourgesl
 * added change listener for image modes
 *
 * Revision 1.18  2010/02/08 17:00:16  bourgesl
 * added U-V max selector + checkboxes
 *
 * Revision 1.17  2010/02/05 16:17:01  bourgesl
 * added widgets for UV Model
 *
 * Revision 1.16  2010/02/05 13:13:30  bourgesl
 * fixed NPE
 *
 * Revision 1.15  2010/02/04 17:05:06  bourgesl
 * UV bounds are coming from UVCoverageService
 *
 * Revision 1.14  2010/02/04 14:54:11  bourgesl
 * UVMapData refactoring (uvRect, min/max values) to keep the color mapping consistent when zooming
 * Compute an sub Image when a zoom occurs while the correct model is computed in the background
 *
 * Revision 1.13  2010/02/03 16:07:49  bourgesl
 * refactoring to use the custom swing worker executor
 * when zomming uv map is computed asynchronously
 *
 * Revision 1.12  2010/02/03 09:48:53  bourgesl
 * target model uvmap added on the uv coverage with zooming supported
 *
 * Revision 1.11  2010/01/22 14:25:29  bourgesl
 * fixed log level
 *
 * Revision 1.10  2010/01/22 13:17:20  bourgesl
 * change color association to plots
 *
 * Revision 1.9  2010/01/21 16:41:30  bourgesl
 * added HA min / max sliders and used only to constraint the UV tracks
 *
 * Revision 1.8  2010/01/20 16:18:38  bourgesl
 * observation form refactoring
 *
 * Revision 1.7  2010/01/19 13:20:20  bourgesl
 * NPE fixed when the observability displays the baseline limits
 *
 * Revision 1.6  2010/01/15 16:14:16  bourgesl
 * added computation of UV points compatible with observability ranges, bandpass and sampling periodicity
 *
 * Revision 1.5  2010/01/15 13:52:14  bourgesl
 * instrumentMode synchronized properly between the observation and the UI widgets (load/change/reset)
 *
 * Revision 1.4  2010/01/14 17:03:37  bourgesl
 * refactoring for observation LOAD / CHANGE events
 *
 * Revision 1.3  2010/01/13 16:12:31  bourgesl
 * added export to PDF button
 *
 * Revision 1.2  2010/01/12 16:54:19  bourgesl
 * added PoPs in title + several changes on charts
 *
 * Revision 1.1  2010/01/11 13:58:43  bourgesl
 * bad class name for UV Coverage Panel
 *
 * Revision 1.1  2010/01/08 16:48:30  bourgesl
 * package refactoring
 *
 */
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.gui.action.ExportOBAmberAction;
import fr.jmmc.aspro.gui.action.ExportPDFAction;
import fr.jmmc.aspro.gui.chart.ChartUtils;
import fr.jmmc.aspro.gui.chart.SquareChartPanel;
import fr.jmmc.aspro.gui.chart.SquareXYPlot;
import fr.jmmc.aspro.gui.chart.ZoomEvent;
import fr.jmmc.aspro.gui.chart.ZoomEventListener;
import fr.jmmc.aspro.gui.util.ColorPalette;
import fr.jmmc.aspro.gui.util.FieldSliderAdapter;
import fr.jmmc.aspro.gui.util.SwingWorkerExecutor;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.ObservationListener;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.uvcoverage.UVCoverageData;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Pop;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.uvcoverage.UVBaseLineData;
import fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData;
import fr.jmmc.aspro.service.UVCoverageService;
import fr.jmmc.mcs.gui.FeedbackReport;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.image.ColorModels;
import fr.jmmc.mcs.model.ModelUVMapService;
import fr.jmmc.mcs.model.ModelUVMapService.ImageMode;
import fr.jmmc.mcs.model.UVMapData;
import fr.jmmc.mcs.model.targetmodel.Model;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.geom.Rectangle2D;
import java.awt.image.IndexColorModel;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.ParseException;
import java.util.List;
import java.util.Vector;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JFormattedTextField;
import javax.swing.JOptionPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.jdesktop.swingworker.SwingWorker;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.event.ChartProgressEvent;
import org.jfree.chart.event.ChartProgressListener;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 * This panel presents the UV coverage plot with its parameters (target, instrument mode ...)
 * @author bourgesl
 */
public class UVCoveragePanel extends javax.swing.JPanel implements ChartProgressListener, ZoomEventListener,
        ActionListener, ChangeListener, ObservationListener {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.UVChartPanel";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** image size choices */
  private final static Integer[] IMAGE_SIZES = {256, 512, 1024};
  /** scaling factor to Mega Lambda for U,V points */
  private final static double MEGA_LAMBDA_SCALE = 1e-6;

  /* members */
  /** observation manager */
  private ObservationManager om = ObservationManager.getInstance();
  /** jFreeChart instance */
  private JFreeChart localJFreeChart;
  /** xy plot instance */
  private SquareXYPlot localXYPlot;
  /** uv coordinates scaling factor */
  private double uvPlotScalingFactor = MEGA_LAMBDA_SCALE;
  /* cached data */
  /* last computed Observability Data to get star data */
  private ObservabilityData currentObsData = null;
  /** last zoom event to check if the zoom area changed */
  private ZoomEvent lastZoomEvent = null;
  /** last computed UV Map Data to have a reference UV Map */
  private UVMapData currentUVMapData = null;
  /** current interferometer configuration name to track changes */
  private String interferometerConfigurationName = null;
  /* swing */
  /** chart panel */
  private SquareChartPanel chartPanel;
  /** flag to enable / disable the automatic refresh of the plot when any swing component changes */
  private boolean doAutoRefresh = true;
  /** flag to enable / disable the automatic update of the observation when any swing component changes */
  private boolean doAutoUpdateObservation = true;
  /** custom adapter for HA min fields */
  private FieldSliderAdapter haMinAdapter = null;
  /** custom adapter for HA max fields */
  private FieldSliderAdapter haMaxAdapter = null;
  /** custom adapter for UV max fields */
  private FieldSliderAdapter uvMaxAdapter = null;

  /**
   * Constructor
   */
  public UVCoveragePanel() {
    initComponents();

    postInit();

    // register this as an observation listener :
    om.register(this);
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

    jSplitPane = new javax.swing.JSplitPane();
    jPanelRight = new javax.swing.JPanel();
    jLabel1 = new javax.swing.JLabel();
    jPanelButtons = new javax.swing.JPanel();
    jButtonModelEditor = new javax.swing.JButton();
    jButtonPDF = new javax.swing.JButton();
    jButtonOB = new javax.swing.JButton();
    jComboBoxTarget = new javax.swing.JComboBox();
    jLabel2 = new javax.swing.JLabel();
    jComboBoxInstrumentMode = new javax.swing.JComboBox();
    jLabel3 = new javax.swing.JLabel();
    jFieldSamplingPeriod = new javax.swing.JFormattedTextField();
    jLabel4 = new javax.swing.JLabel();
    jSliderHAMin = new javax.swing.JSlider();
    jLabel5 = new javax.swing.JLabel();
    jSliderHAMax = new javax.swing.JSlider();
    jFieldHAMin = new javax.swing.JFormattedTextField();
    jFieldHAMax = new javax.swing.JFormattedTextField();
    jTargetHAMin = new javax.swing.JLabel();
    jTargetHAMax = new javax.swing.JLabel();
    jComboBoxImageMode = new javax.swing.JComboBox();
    jCheckBoxModelImage = new javax.swing.JCheckBox();
    jSeparator1 = new javax.swing.JSeparator();
    jCheckBoxPlotUVSupport = new javax.swing.JCheckBox();
    jLabel6 = new javax.swing.JLabel();
    jSeparator3 = new javax.swing.JSeparator();
    jSliderUVMax = new javax.swing.JSlider();
    jFieldUVMax = new javax.swing.JFormattedTextField();
    jLabel7 = new javax.swing.JLabel();
    jLabel8 = new javax.swing.JLabel();
    jLabel9 = new javax.swing.JLabel();
    jComboBoxLUT = new javax.swing.JComboBox();
    jComboBoxImageSize = new javax.swing.JComboBox();
    jLabelAtmQual = new javax.swing.JLabel();
    jComboBoxAtmQual = new javax.swing.JComboBox();
    jLabelFTMode = new javax.swing.JLabel();
    jComboBoxFTMode = new javax.swing.JComboBox();

    setLayout(new java.awt.BorderLayout());

    jSplitPane.setResizeWeight(0.1);

    jPanelRight.setMinimumSize(new java.awt.Dimension(200, 500));
    jPanelRight.setPreferredSize(new java.awt.Dimension(200, 500));
    jPanelRight.setLayout(new java.awt.GridBagLayout());

    jLabel1.setText("Target");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.ipadx = 2;
    gridBagConstraints.ipady = 2;
    jPanelRight.add(jLabel1, gridBagConstraints);

    jPanelButtons.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 2, 2));

    jButtonModelEditor.setText("Model Editor");
    jButtonModelEditor.setMargin(new java.awt.Insets(0, 0, 0, 0));
    jButtonModelEditor.setMinimumSize(new java.awt.Dimension(50, 25));
    jButtonModelEditor.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonModelEditorActionPerformed(evt);
      }
    });
    jPanelButtons.add(jButtonModelEditor);

    jButtonPDF.setIcon(new javax.swing.ImageIcon(getClass().getResource("/fr/jmmc/aspro/gui/icons/icon_pdf.gif"))); // NOI18N
    jButtonPDF.setMargin(new java.awt.Insets(0, 0, 0, 0));
    jButtonPDF.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonPDFActionPerformed(evt);
      }
    });
    jPanelButtons.add(jButtonPDF);

    jButtonOB.setText("OB");
    jButtonOB.setToolTipText("Only AMBER is supported");
    jButtonOB.setMargin(new java.awt.Insets(0, 0, 0, 0));
    jButtonOB.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonOBActionPerformed(evt);
      }
    });
    jPanelButtons.add(jButtonOB);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
    jPanelRight.add(jPanelButtons, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    jPanelRight.add(jComboBoxTarget, gridBagConstraints);

    jLabel2.setText("Instrument mode");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 4;
    gridBagConstraints.gridwidth = 2;
    jPanelRight.add(jLabel2, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 5;
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    jPanelRight.add(jComboBoxInstrumentMode, gridBagConstraints);

    jLabel3.setText("Sampling Periodicity (min)");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 14;
    gridBagConstraints.gridwidth = 2;
    jPanelRight.add(jLabel3, gridBagConstraints);

    jFieldSamplingPeriod.setColumns(3);
    jFieldSamplingPeriod.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter()));
    jFieldSamplingPeriod.setMinimumSize(new java.awt.Dimension(40, 19));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 15;
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    jPanelRight.add(jFieldSamplingPeriod, gridBagConstraints);

    jLabel4.setText("HA min");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 16;
    jPanelRight.add(jLabel4, gridBagConstraints);

    jSliderHAMin.setMajorTickSpacing(30);
    jSliderHAMin.setMaximum(240);
    jSliderHAMin.setPaintTicks(true);
    jSliderHAMin.setMaximumSize(new java.awt.Dimension(80, 27));
    jSliderHAMin.setPreferredSize(new java.awt.Dimension(80, 27));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 17;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    jPanelRight.add(jSliderHAMin, gridBagConstraints);

    jLabel5.setText("HA max");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 18;
    jPanelRight.add(jLabel5, gridBagConstraints);

    jSliderHAMax.setMajorTickSpacing(30);
    jSliderHAMax.setMaximum(240);
    jSliderHAMax.setPaintTicks(true);
    jSliderHAMax.setMaximumSize(new java.awt.Dimension(80, 32767));
    jSliderHAMax.setPreferredSize(new java.awt.Dimension(80, 27));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 19;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    jPanelRight.add(jSliderHAMax, gridBagConstraints);

    jFieldHAMin.setColumns(6);
    jFieldHAMin.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0.00"))));
    jFieldHAMin.setMinimumSize(new java.awt.Dimension(50, 19));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 17;
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    jPanelRight.add(jFieldHAMin, gridBagConstraints);

    jFieldHAMax.setColumns(6);
    jFieldHAMax.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0.00"))));
    jFieldHAMax.setMinimumSize(new java.awt.Dimension(50, 19));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 19;
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    jPanelRight.add(jFieldHAMax, gridBagConstraints);

    jTargetHAMin.setText("targetHAMin");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 16;
    jPanelRight.add(jTargetHAMin, gridBagConstraints);

    jTargetHAMax.setText("targetHAMax");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 18;
    jPanelRight.add(jTargetHAMax, gridBagConstraints);

    jComboBoxImageMode.setModel(new DefaultComboBoxModel(ModelUVMapService.ImageMode.values()));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 24;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    jPanelRight.add(jComboBoxImageMode, gridBagConstraints);

    jCheckBoxModelImage.setSelected(true);
    jCheckBoxModelImage.setText("Underplot a model image");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 23;
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    jPanelRight.add(jCheckBoxModelImage, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 22;
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelRight.add(jSeparator1, gridBagConstraints);

    jCheckBoxPlotUVSupport.setSelected(true);
    jCheckBoxPlotUVSupport.setText("Plot rise/set uv tracks");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 20;
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    jPanelRight.add(jCheckBoxPlotUVSupport, gridBagConstraints);

    jLabel6.setText("Plot what ...");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 24;
    jPanelRight.add(jLabel6, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 11;
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelRight.add(jSeparator3, gridBagConstraints);

    jSliderUVMax.setMajorTickSpacing(10);
    jSliderUVMax.setPaintTicks(true);
    jSliderUVMax.setValue(100);
    jSliderUVMax.setMaximumSize(new java.awt.Dimension(80, 32767));
    jSliderUVMax.setPreferredSize(new java.awt.Dimension(80, 27));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 13;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    jPanelRight.add(jSliderUVMax, gridBagConstraints);

    jFieldUVMax.setColumns(6);
    jFieldUVMax.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0.00"))));
    jFieldUVMax.setMinimumSize(new java.awt.Dimension(50, 19));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 13;
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    jPanelRight.add(jFieldUVMax, gridBagConstraints);

    jLabel7.setText("U-V range to plot");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 12;
    gridBagConstraints.gridwidth = 2;
    jPanelRight.add(jLabel7, gridBagConstraints);

    jLabel8.setText("LUT table");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 25;
    jPanelRight.add(jLabel8, gridBagConstraints);

    jLabel9.setText("Image size");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 26;
    jPanelRight.add(jLabel9, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 25;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    jPanelRight.add(jComboBoxLUT, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 26;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    jPanelRight.add(jComboBoxImageSize, gridBagConstraints);

    jLabelAtmQual.setText("Atmosphere quality");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 6;
    gridBagConstraints.gridwidth = 2;
    jPanelRight.add(jLabelAtmQual, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 7;
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    jPanelRight.add(jComboBoxAtmQual, gridBagConstraints);

    jLabelFTMode.setText("Fringe tracker mode");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 8;
    gridBagConstraints.gridwidth = 2;
    jPanelRight.add(jLabelFTMode, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 9;
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
    jPanelRight.add(jComboBoxFTMode, gridBagConstraints);

    jSplitPane.setLeftComponent(jPanelRight);

    add(jSplitPane, java.awt.BorderLayout.CENTER);
  }// </editor-fold>//GEN-END:initComponents

  /**
   * Export the current chart as a PDF document
   * @param evt action event
   */
  private void jButtonPDFActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonPDFActionPerformed

    // set the source with the chart :
    evt.setSource(this.localJFreeChart);

    ExportPDFAction.getInstance().actionPerformed(evt);
  }//GEN-LAST:event_jButtonPDFActionPerformed

  private void jButtonModelEditorActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonModelEditorActionPerformed

    final String targetName = getSelectedTargetName();

    // show model editor :
    if (TargetModelForm.showModelEditor(targetName)) {
      changeStateForModelImageWidgets();
      refreshPlot();
    }
}//GEN-LAST:event_jButtonModelEditorActionPerformed

  private void jButtonOBActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonOBActionPerformed

    final ObservationSetting observation = ObservationManager.getInstance().getObservation();
    final String instrumentName = observation.getInstrumentConfiguration().getName();

    if (AsproConstants.INS_AMBER.equals(instrumentName)) {
      // set the source with this instance :
      evt.setSource(this);

      ExportOBAmberAction.getInstance().actionPerformed(evt);
    } else {
      JOptionPane.showMessageDialog(null, "The application can not generate an Observing Block for this instrument !",
              "Error", JOptionPane.INFORMATION_MESSAGE);
    }

  }//GEN-LAST:event_jButtonOBActionPerformed

  /**
   * This method is useful to set the models and specific features of initialized swing components :
   */
  private void postInit() {

    this.localJFreeChart = ChartUtils.createSquareXYLineChart("U (M\u03BB)", "V (M\u03BB)");
    this.localXYPlot = (SquareXYPlot) localJFreeChart.getPlot();

    // Adjust background settings :
    this.localXYPlot.setBackgroundImageAlpha(1.0f);
    // Adjust outline :
    this.localXYPlot.setOutlineStroke(new BasicStroke(1.f));

    // add listener :
    this.localJFreeChart.addProgressListener(this);

    this.chartPanel = new SquareChartPanel(this.localJFreeChart,
            400, 400, /* prefered size */
            200, 200, /* minimum size before scaling */
            1600, 1600, /* maximum size before scaling */
            true, /* use buffer */
            false, /* properties */
            true, /* copy */
            true, /* save */
            true, /* print */
            false, /* zoom */
            false /* tooltips */);

    // zoom options :
    this.chartPanel.setDomainZoomable(AsproConstants.ENABLE_ZOOM);
    this.chartPanel.setRangeZoomable(AsproConstants.ENABLE_ZOOM);
    this.chartPanel.setMouseWheelEnabled(false);

    // define zoom listener :
    this.chartPanel.setZoomEventListener(this);

    this.chartPanel.setMinimumSize(new Dimension(650, 500));
    this.jSplitPane.setRightComponent(this.chartPanel);

    // define change listeners :
    this.jComboBoxTarget.addActionListener(this);
    this.jComboBoxInstrumentMode.addActionListener(this);

    this.uvMaxAdapter = new FieldSliderAdapter(jSliderUVMax, jFieldUVMax, 0d, 0d, 0d);
    this.uvMaxAdapter.addChangeListener(this);

    // default sampling Period and property change listener :
    this.jFieldSamplingPeriod.setValue(AsproConstants.DEFAULT_SAMPLING_PERIOD);
    this.jFieldSamplingPeriod.addPropertyChangeListener("value", new PropertyChangeListener() {

      public void propertyChange(final PropertyChangeEvent evt) {
        final double samplingNew = ((Number) jFieldSamplingPeriod.getValue()).doubleValue();

        if (samplingNew < 0d) {
          // invalid value :
          jFieldSamplingPeriod.setValue(AsproConstants.DEFAULT_SAMPLING_PERIOD);
        }

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("samplingPeriod changed : " + samplingNew);
        }
        updateObservation();
        refreshPlot();
      }
    });

    this.haMinAdapter = new FieldSliderAdapter(jSliderHAMin, jFieldHAMin, -12D, 12D, -12D);
    this.haMinAdapter.addChangeListener(this);

    this.haMaxAdapter = new FieldSliderAdapter(jSliderHAMax, jFieldHAMax, -12D, 12D, 12D);
    this.haMaxAdapter.addChangeListener(this);

    this.jTargetHAMin.setText(null);
    this.jTargetHAMax.setText(null);

    this.jCheckBoxPlotUVSupport.addItemListener(new ItemListener() {

      public void itemStateChanged(final ItemEvent e) {
        refreshPlot();
      }
    });

    this.jCheckBoxModelImage.addItemListener(new ItemListener() {

      public void itemStateChanged(final ItemEvent e) {
        final boolean enabled = jCheckBoxModelImage.isSelected();
        jComboBoxImageMode.setEnabled(enabled);
        jComboBoxLUT.setEnabled(enabled);
        jComboBoxImageSize.setEnabled(enabled);

        refreshPlot();
      }
    });

    this.jComboBoxImageMode.addActionListener(this);

    this.jComboBoxLUT.setModel(new DefaultComboBoxModel(ColorModels.getColorModelNames()));
    this.jComboBoxLUT.setSelectedItem("aspro");

    this.jComboBoxLUT.addActionListener(this);

    this.jComboBoxImageSize.setModel(new DefaultComboBoxModel(IMAGE_SIZES));
    this.jComboBoxImageSize.setSelectedIndex(1);

    this.jComboBoxImageSize.addActionListener(this);

    // disable Atmosphere quality :
    this.jLabelAtmQual.setVisible(false);
    this.jComboBoxAtmQual.setVisible(false);
  }

  /**
   * Refresh the target list
   */
  private void updateComboTarget() {
    final Object oldValue = getSelectedTargetName();

    final Vector<String> v = this.om.getTargetNames();
    this.jComboBoxTarget.setModel(new DefaultComboBoxModel(v));

    // restore previous selected item :
    if (oldValue != null) {
      this.jComboBoxTarget.setSelectedItem(oldValue);
    }
    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("jComboBoxTarget updated : " + getSelectedTargetName());
    }
  }

  /**
   * Refresh the instrument modes
   * @param observation current observation settings
   */
  private void updateComboInstrumentModes(final ObservationSetting observation) {
    final Object oldValue = this.jComboBoxInstrumentMode.getSelectedItem();

    final Vector<String> v = ConfigurationManager.getInstance().getInstrumentModes(
            observation.getInterferometerConfiguration().getName(),
            observation.getInstrumentConfiguration().getName());
    this.jComboBoxInstrumentMode.setModel(new DefaultComboBoxModel(v));

    // restore previous selected item :
    if (oldValue != null) {
      this.jComboBoxInstrumentMode.setSelectedItem(oldValue);
    }
    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("jComboBoxInstrumentMode updated : " + this.jComboBoxInstrumentMode.getSelectedItem());
    }
  }

  /**
   * Refresh the fringe tracker modes
   * @param observation current observation settings
   */
  private void updateComboFTModes(final ObservationSetting observation) {
    final Object oldValue = this.jComboBoxFTMode.getSelectedItem();

    final Vector<String> modes = ConfigurationManager.getInstance().getFringeTrackerModes(
            observation.getInterferometerConfiguration().getName());

    // modes can be empty :
    this.jComboBoxFTMode.setModel(new DefaultComboBoxModel(modes));

    // restore previous selected item :
    if (oldValue != null) {
      this.jComboBoxFTMode.setSelectedItem(oldValue);
    }
    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("jComboBoxFTMode updated : " + this.jComboBoxFTMode.getSelectedItem());
    }
    final boolean visible = !modes.isEmpty();

    this.jComboBoxFTMode.setVisible(visible);
    this.jLabelFTMode.setVisible(visible);
  }

  /**
   * Process any comboBox change event (target, instrument mode, image mode ...).
   * Refresh the dependent combo boxes and update the observation according to the form state
   * @param e action event
   */
  public void actionPerformed(final ActionEvent e) {
    if (e.getSource() == this.jComboBoxTarget) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("target changed : " + getSelectedTargetName());
      }
      changeStateForModelImageWidgets();
      updateTargetHA();
      refreshPlot();
    } else if (e.getSource() == this.jComboBoxInstrumentMode) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("instrument mode changed : " + this.jComboBoxInstrumentMode.getSelectedItem());
      }
      updateObservation();
      refreshPlot();
    } else if (e.getSource() == this.jComboBoxImageMode) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("image mode changed : " + this.jComboBoxImageMode.getSelectedItem());
      }
      refreshPlot();
    } else if (e.getSource() == this.jComboBoxLUT) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("LUT changed : " + this.jComboBoxLUT.getSelectedItem());
      }
      refreshPlot();
    } else if (e.getSource() == this.jComboBoxImageSize) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("image size changed : " + this.jComboBoxImageSize.getSelectedItem());
      }
      refreshPlot();
    }
  }

  /**
   * Handle the stateChanged event from the FieldSliderAdapter instances
   * @param ce change event
   */
  public void stateChanged(final ChangeEvent ce) {
    final FieldSliderAdapter source = (FieldSliderAdapter) ce.getSource();

    if (source == this.haMinAdapter) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("haMin changed : " + source.getValue());
      }
      this.haMaxAdapter.setMinValue(source.getValue());
      refreshPlot();

    } else if (source == this.haMaxAdapter) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("haMax changed : " + source.getValue());
      }
      this.haMinAdapter.setMaxValue(source.getValue());
      refreshPlot();
    } else if (source == this.uvMaxAdapter) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("U-V Max changed : " + source.getValue());
      }
      refreshPlot();
    }
  }

  /**
   * Update the observation with the form fields if the automatic update flag is enabled.
   */
  private void updateObservation() {
    // check if the automatic update flag is enabled :
    if (this.doAutoUpdateObservation) {

      final String targetName = getSelectedTargetName();

      if (targetName != null) {
        // Change the instrument mode :
        this.om.setInstrumentMode((String) this.jComboBoxInstrumentMode.getSelectedItem());

        // Update the sampling period :
        final Number samplingPeriod = (Number) this.jFieldSamplingPeriod.getValue();
        this.om.setInstrumentSamplingPeriod(Double.valueOf(samplingPeriod.doubleValue()));

      } else {
        // clean up i.e. the panel is then invalid :

        this.om.setInstrumentMode(null);
        this.om.setInstrumentSamplingPeriod(null);
      }

      // TODO : fire event ??
      // NOTE : the onChange event is already handled : risk of cyclic loop !
    }
  }

  /**
   * Update the UI widgets from the given changed observation
   * @param observation observation
   */
  private void onChangeObservation(final ObservationSetting observation) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("observation :\n" + ObservationManager.toString(observation));
    }

    // When the observation changes, it means that the observability will be computed in background,
    // and soon an ObservabilityDone event will be sent.

    // Only refresh the UI widgets and NOT the plot :

    try {
      // first disable the automatic refresh from field changes :
      this.doAutoRefresh = false;

      // refresh the targets :
      updateComboTarget();
      changeStateForModelImageWidgets();

      // refresh the instrument modes :
      updateComboInstrumentModes(observation);

      // refresh the fringe tracker modes :
      updateComboFTModes(observation);

      // update the data related to the interferometer :
      updateInteferometerData(observation);

      updateObservation();

    } finally {
      // restore the automatic refresh from field changes :
      this.doAutoRefresh = true;
    }
  }

  /**
   * Update the UI widgets from the given loaded observation
   *
   * @param observation observation
   */
  private void onLoadObservation(final ObservationSetting observation) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("onLoadObservation :\n" + ObservationManager.toString(observation));
    }
    try {
      // first disable the automatic update observation from field changes :
      this.doAutoUpdateObservation = false;
      // first disable the automatic refresh from field changes :
      this.doAutoRefresh = false;

      // refresh the targets :
      updateComboTarget();
      changeStateForModelImageWidgets();

      // refresh the instrument modes :
      updateComboInstrumentModes(observation);

      // update the selected instrument mode :
      this.jComboBoxInstrumentMode.setSelectedItem(observation.getInstrumentConfiguration().getInstrumentMode());

      // refresh the fringe tracker modes :
      updateComboFTModes(observation);

      // reset HA limits :
      this.haMinAdapter.setValue(-12D);
      this.haMaxAdapter.setValue(12D);

      // reset defaults :
      this.jCheckBoxPlotUVSupport.setSelected(true);
      this.jCheckBoxModelImage.setSelected(true);
      this.jComboBoxImageMode.setSelectedItem(ImageMode.AMP);

      // reset cached data :
      this.currentObsData = null;
      this.lastZoomEvent = null;
      this.currentUVMapData = null;
      this.interferometerConfigurationName = null;

      // update the data related to the interferometer :
      updateInteferometerData(observation);

    } finally {
      // restore the automatic refresh from field changes :
      this.doAutoRefresh = true;
      // restore the automatic update observation from field changes :
      this.doAutoUpdateObservation = true;
    }
  }

  /**
   * Handle the given event on the given observation =
   * 1/ If the observation changed, refresh the UI widgets (targets ...)
   * 2/ If the observability is computed, then refresh the plot
   * @param type event type
   * @param observation observation
   */
  public void onProcess(final ObservationEventType type, final ObservationSetting observation) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("event [" + type + "] process IN");
    }
    switch (type) {
      case CHANGED:
        this.onChangeObservation(observation);
        break;
      case LOADED:
        this.onLoadObservation(observation);
        break;
      case OBSERVABILITY_DONE:
        this.updateObservabilityData(observation.getObservabilityData());
        this.plot(observation);
        break;
      default:
    }
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("event [" + type + "] process OUT");
    }
  }

  private void updateObservabilityData(final ObservabilityData obsData) {
    this.currentObsData = obsData;
    updateTargetHA();
  }

  private void updateTargetHA() {
    if (this.currentObsData != null) {
      final String targetName = getSelectedTargetName();

      final StarData starData = this.currentObsData.getStarData(targetName);
      if (starData != null) {
        final Double min = Range.getMinimum(starData.getObsRangesHA());
        final Double max = Range.getMaximum(starData.getObsRangesHA());

        this.jTargetHAMin.setText(format(jFieldHAMin, min));
        this.jTargetHAMax.setText(format(jFieldHAMin, max));

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("target HA min : " + min);
          logger.fine("target HA max : " + min);
        }
      } else {
        // baseline limits case :
        this.jTargetHAMin.setText(null);
        this.jTargetHAMax.setText(null);
      }
    }
  }

  /**
   * If the current target has no model defined, then disable model options widgets
   */
  private void changeStateForModelImageWidgets() {
    final String targetName = getSelectedTargetName();

    final Target target = ObservationManager.getInstance().getTarget(targetName);
    if (target != null) {
      final List<Model> models = target.getModels();

      final boolean hasModel = (models != null && !models.isEmpty());

      this.jCheckBoxModelImage.setEnabled(hasModel);
      this.jComboBoxImageMode.setEnabled(hasModel);
      this.jComboBoxImageSize.setEnabled(hasModel);
      this.jComboBoxLUT.setEnabled(hasModel);
    }
  }

  private void updateInteferometerData(final ObservationSetting observation) {
    // note : can not be null :
    final String intConfName = observation.getInterferometerConfiguration().getName();
    // test if the interferometer changed :
    boolean changed = !intConfName.equals(this.interferometerConfigurationName);
    if (changed) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("interferometer configuration changed : " + intConfName);
      }
      this.interferometerConfigurationName = intConfName;

      final InterferometerConfiguration intConf = observation.getInterferometerConfiguration().getInterferometerConfiguration();

      // update the UV Max :
      final double maxBaseLine = intConf.getInterferometer().getMaxBaseLine();
      this.uvMaxAdapter.reset(0, maxBaseLine, maxBaseLine);
    }
  }

  /**
   * Refresh the plot when an UI widget changes.
   * Check the doAutoRefresh flag to avoid unwanted refresh (resetOptions)
   */
  protected void refreshPlot() {
    if (this.doAutoRefresh) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("refreshPlot");
      }
      this.plot(ObservationManager.getInstance().getObservation());
    }
  }

  /**
   * Plot the UV Coverage using a SwingWorker to do the computation in the background.
   * This code is executed by the Swing Event Dispatcher thread (EDT)
   * @param observation observation data to use
   */
  protected void plot(final ObservationSetting observation) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("plot : " + ObservationManager.toString(observation));
    }

    final String targetName = getSelectedTargetName();

    // HA Limits :
    final double haMin = this.haMinAdapter.getValue();
    final double haMax = this.haMaxAdapter.getValue();

    final double uvMax = this.uvMaxAdapter.getValue();

    final boolean doUVSupport = this.jCheckBoxPlotUVSupport.isSelected();
    final boolean doModelImage = this.jCheckBoxModelImage.isSelected();

    // model image options :
    final ImageMode imageMode = (ImageMode) this.jComboBoxImageMode.getSelectedItem();
    final Integer imageSize = (Integer) this.jComboBoxImageSize.getSelectedItem();
    final IndexColorModel colorModel = ColorModels.getColorModel((String) this.jComboBoxLUT.getSelectedItem());

    // check if observability data are available :
    final ObservabilityData obsData = observation.getObservabilityData();

    if (obsData != null) {
      /*
       * Use the SwingWorker backport for Java 5 = swing-worker-1.2.jar (org.jdesktop.swingworker.SwingWorker)
       */
      final SwingWorker<UVCoverageData, Void> worker = new SwingWorker<UVCoverageData, Void>() {

        /**
         * Compute the UV Coverage data in background
         * @return UV Coverage data
         */
        @Override
        public UVCoverageData doInBackground() {
          logger.fine("SwingWorker[UV].doInBackground : IN");

          UVCoverageData uvData = new UVCoverageService(observation, targetName, haMin, haMax, uvMax,
                  doUVSupport, doModelImage, imageMode, imageSize.intValue(), colorModel).compute();

          if (isCancelled()) {
            logger.fine("SwingWorker[UV].doInBackground : CANCELLED");
            // no result if task is cancelled :
            uvData = null;
          } else {
            logger.fine("SwingWorker[UV].doInBackground : OUT");
          }
          return uvData;
        }

        /**
         * Reset the plot in case of baseline limits or model exception
         */
        public void reset() {
          lastZoomEvent = null;
          currentUVMapData = null;

          // reset bounds to [-1;1] (before setDataset) :
          localXYPlot.defineBounds(1d);
          // reset dataset for baseline limits :
          localXYPlot.setDataset(null);

          // update the background image :
          updateUVMap(null);
        }

        /**
         * Refresh the plot using the computed UV Coverage data.
         * This code is executed by the Swing Event Dispatcher thread (EDT)
         */
        @Override
        public void done() {
          // check if the worker was cancelled :
          if (!isCancelled()) {
            logger.fine("SwingWorker[UV].done : IN");
            try {
              // Get the computation results with all data necessary to draw the plot :
              final UVCoverageData uvData = get();

              if (uvData != null) {
                logger.fine("SwingWorker[UV].done : refresh Chart");

                lastZoomEvent = null;
                currentUVMapData = null;

                ChartUtils.clearTextSubTitle(localJFreeChart);

                if (uvData.getName() == null) {
                  // Baseline limits case :
                  reset();
                } else {

                  // title :
                  final StringBuilder title = new StringBuilder(observation.getInterferometerConfiguration().getName());
                  title.append(" - ").append(observation.getInstrumentConfiguration().getStations());

                  if (obsData.getBestPops() != null) {
                    title.append(" + ");
                    for (Pop pop : obsData.getBestPops().getPopList()) {
                      title.append(pop.getName()).append(" ");
                    }
                  }
                  ChartUtils.addSubtitle(localJFreeChart, title.toString());
                  ChartUtils.addSubtitle(localJFreeChart, "Source : " + uvData.getName());

                  if (observation.getWhen().isNightRestriction()) {
                    // date :
                    ChartUtils.addSubtitle(localJFreeChart, "Day : " + observation.getWhen().getDate().toString());
                  }

                  // change the scaling factor : (???)
                  // lambda ??
                  setUvPlotScalingFactor(MEGA_LAMBDA_SCALE);

                  // computed data are valid :
                  updateChart(uvData);

                  // update the uv map data :
                  currentUVMapData = uvData.getUvMapData();

                  // update the background image :
                  if (currentUVMapData == null) {
                    updateUVMap(null);
                  } else {
                    updateUVMap(currentUVMapData.getUvMap());
                  }
                }

                // update theme at end :
                ChartUtilities.applyCurrentTheme(localJFreeChart);
              }

            } catch (InterruptedException ignore) {
            } catch (ExecutionException ee) {
              reset();
              if (ee.getCause() instanceof IllegalArgumentException) {
                JOptionPane.showMessageDialog(null, ee.getCause().getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
              } else {
                logger.log(Level.SEVERE, "Error : ", ee);
                new FeedbackReport(null, true, (Exception) ee.getCause());
              }
            }

            // update the status bar :
            StatusBar.show("uv coverage done.");

            logger.fine("SwingWorker[UV].done : OUT");
          }
        }
      };

      // update the status bar :
      StatusBar.show("computing uv coverage ... (please wait, this may take a while)");

      // Cancel other uv map task and execute this new uv map task :
      SwingWorkerExecutor.getInstance().execute("UVCoverage", worker);

    } // observability data check
  }

  /**
   * Process the zoom event to refresh the model UV map according to the new coordinates
   * @param ze zoom event
   */
  public void chartChanged(final ZoomEvent ze) {
    // check if the zoom changed :
    if (this.currentUVMapData != null && !ze.equals(this.lastZoomEvent)) {
      this.lastZoomEvent = ze;

      final String targetName = getSelectedTargetName();

      final List<Model> models = ObservationManager.getTarget(ObservationManager.getInstance().getObservation(), targetName).getModels();

      if (models.size() > 0) {

        final Rectangle2D.Float uvRect = new Rectangle2D.Float();
        uvRect.setFrameFromDiagonal(
                fromUVPlotScale(ze.getDomainLowerBound()), fromUVPlotScale(ze.getRangeLowerBound()),
                fromUVPlotScale(ze.getDomainUpperBound()), fromUVPlotScale(ze.getRangeUpperBound()));

        // compute an approximated uv map from the reference UV Map :
        computeSubUVMap(uvRect);

        // visibility reference extrema :
        final Float refMin = Float.valueOf(this.currentUVMapData.getMin());
        final Float refMax = Float.valueOf(this.currentUVMapData.getMax());

        // model image options :
        final ImageMode imageMode = (ImageMode) this.jComboBoxImageMode.getSelectedItem();
        final Integer imageSize = (Integer) this.jComboBoxImageSize.getSelectedItem();
        final IndexColorModel colorModel = ColorModels.getColorModel((String) this.jComboBoxLUT.getSelectedItem());

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("computing model uv map ...");
        }

        /*
         * Use the SwingWorker backport for Java 5 = swing-worker-1.2.jar (org.jdesktop.swingworker.SwingWorker)
         */
        final SwingWorker<UVMapData, Void> worker = new SwingWorker<UVMapData, Void>() {

          /**
           * Compute the UV Map in background
           * @return Image
           */
          @Override
          public UVMapData doInBackground() {
            logger.fine("SwingWorker[UVMap].doInBackground : IN");

            UVMapData uvMapData = ModelUVMapService.computeUVMap(
                    models, uvRect, refMin, refMax,
                    imageMode, imageSize.intValue(), colorModel);

            if (isCancelled()) {
              logger.fine("SwingWorker[UVMap].doInBackground : CANCELLED");
              // no result if task is cancelled :
              uvMapData = null;
            } else {
              logger.fine("SwingWorker[UVMap].doInBackground : OUT");
            }
            return uvMapData;
          }

          /**
           * Refresh the plot using the computed UV Map.
           * This code is executed by the Swing Event Dispatcher thread (EDT)
           */
          @Override
          public void done() {
            // check if the worker was cancelled :
            if (!isCancelled()) {
              logger.fine("SwingWorker[UVMap].done : IN");
              try {
                // Get the computation results with all data necessary to draw the plot :
                final UVMapData uvMapData = get();

                if (uvMapData != null) {
                  logger.fine("SwingWorker[UVMap].done : refresh Chart");

                  // update the background image :
                  updateUVMap(uvMapData.getUvMap());
                }

              } catch (InterruptedException ignore) {
              } catch (ExecutionException ee) {
                if (ee.getCause() instanceof IllegalArgumentException) {
                  JOptionPane.showMessageDialog(null, ee.getCause().getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                } else {
                  logger.log(Level.SEVERE, "Error : ", ee);
                  new FeedbackReport(null, true, (Exception) ee.getCause());
                }
              }

              // update the status bar :
              StatusBar.show("uv map done.");

              logger.fine("SwingWorker[UVMap].done : OUT");
            }
          }
        };

        // update the status bar :
        StatusBar.show("computing uv map ... (please wait, this may take a while)");

        // Cancel other uv map task and execute this new uv map task :
        SwingWorkerExecutor.getInstance().execute("UVMap", worker);
      }
    }
  }

  /**
   * Compute a sub image for the UV Map given the new uv area
   * @param uvRect uv area
   */
  private void computeSubUVMap(final Rectangle2D.Float uvRect) {
    if (this.currentUVMapData != null) {
      final int imageSize = this.currentUVMapData.getImageSize();
      // uv area reference :
      final Rectangle2D.Float uvRectRef = this.currentUVMapData.getUvRect();

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("uv map rect     = " + uvRect);
        logger.fine("uv map rect REF = " + uvRectRef);
      }

      // note : floor/ceil to be sure to have at least 1x1 pixel image
      final int x = (int) Math.floor(imageSize * (uvRect.getX() - uvRectRef.getX()) / uvRectRef.getWidth());
      int y = (int) Math.floor(imageSize * (uvRect.getY() - uvRectRef.getY()) / uvRectRef.getHeight());
      final int w = (int) Math.ceil(imageSize * uvRect.getWidth() / uvRectRef.getWidth());
      final int h = (int) Math.ceil(imageSize * uvRect.getHeight() / uvRectRef.getHeight());

      // Note : the image is produced from an array where 0,0 corresponds to the upper left corner
      // whereas it corresponds in UV to the lower U and Upper V coordinates => inverse the V axis

      // Inverse V axis issue :
      y = imageSize - y - h;

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("sub uvMap [" + x + ", " + y + " - " + w + ", " + h + "]");
      }

      Image subUVMap = null;
      try {
        // crop a small sub image waiting for the correct model to be computed :
        subUVMap = this.currentUVMapData.getUvMap().getSubimage(x, y, w, h);

      } catch (RuntimeException re) {
        logger.log(Level.SEVERE, "subImage failure : ", re);
      }

      // update the background image :
      updateUVMap(subUVMap);
    }
  }

  /**
   * Update the background image of the chart with the UV Map
   * @param uvMap image or null
   */
  private void updateUVMap(final Image uvMap) {
    if (uvMap != null) {
      this.localXYPlot.setBackgroundPaint(null);
      this.localXYPlot.setBackgroundImage(uvMap);
    } else {
      this.localXYPlot.setBackgroundPaint(Color.lightGray);
      this.localXYPlot.setBackgroundImage(null);
    }
  }

  /**
   * Update the datasets
   * @param uvData uv coverage data
   */
  private void updateChart(final UVCoverageData uvData) {
    // renderer :
    final XYLineAndShapeRenderer renderer = (XYLineAndShapeRenderer) this.localXYPlot.getRenderer();

    // reset colors :
    renderer.clearSeriesPaints(false);
    // side effect with chart theme :
    renderer.setAutoPopulateSeriesPaint(false);

    final XYSeriesCollection dataset = new XYSeriesCollection();

    this.updateUVTracks(dataset, uvData);
    this.updateUVTracksRiseSet(dataset, uvData);

    // define bounds to the uv maximum value (before setDataset) :
    this.localXYPlot.defineBounds(toUVPlotScale(uvData.getUvMax()));

    // set the main data set :
    this.localXYPlot.setDataset(dataset);
  }

  /**
   * Update the dataset with UV rise/set tracks
   * @param dataset dataset to use
   * @param uvData uv coverage data
   */
  private void updateUVTracksRiseSet(final XYSeriesCollection dataset, final UVCoverageData uvData) {
    final ColorPalette palette = ColorPalette.getDefaultColorPalette();

    // renderer :
    final XYLineAndShapeRenderer renderer = (XYLineAndShapeRenderer) this.localXYPlot.getRenderer();

    // process uv rise/set :
    final List<UVBaseLineData> targetUVRiseSet = uvData.getTargetUVRiseSet();

    if (targetUVRiseSet != null) {
      // target is visible :

      XYSeries xySeriesBL;

      double[] u;
      double[] v;
      double x, y;
      int n = 0;
      // serie offset :
      final int offset = dataset.getSeriesCount();


      for (UVBaseLineData uvBL : targetUVRiseSet) {
        xySeriesBL = new XYSeries("Rise/Set " + uvBL.getName(), false);
        xySeriesBL.setNotify(false);

        u = uvBL.getU();
        v = uvBL.getV();

        // first ellipse line :
        for (int i = 0, size = uvBL.getNPoints(); i < size; i++) {
          x = toUVPlotScale(u[i]);
          y = toUVPlotScale(v[i]);

          xySeriesBL.add(x, y);
        } // points

        // add an invalid point to break the line between the 2 segments :
        xySeriesBL.add(Double.NaN, Double.NaN);

        // second symetric ellipse line :
        for (int i = 0, size = uvBL.getNPoints(); i < size; i++) {
          x = toUVPlotScale(-u[i]);
          y = toUVPlotScale(-v[i]);

          xySeriesBL.add(x, y);
        } // points

        xySeriesBL.setNotify(true);
        dataset.addSeries(xySeriesBL);

        // color :
        renderer.setSeriesPaint(n + offset, palette.getColor(n), false);

        n++;
      } // BL
    }
  }

  /**
   * Update the dataset with UV observable tracks
   * @param dataset dataset to use
   * @param uvData uv coverage data
   */
  private void updateUVTracks(final XYSeriesCollection dataset, final UVCoverageData uvData) {
    final ColorPalette palette = ColorPalette.getDefaultColorPalette();

    // renderer :
    final XYLineAndShapeRenderer renderer = (XYLineAndShapeRenderer) this.localXYPlot.getRenderer();

    // process observable uv ranges :
    final List<UVRangeBaseLineData> targetUVObservability = uvData.getTargetUVObservability();

    if (targetUVObservability != null) {
      // target is observable :

      XYSeries xySeriesBL;

      double[] u;
      double[] v;
      double[] u2;
      double[] v2;
      double x1, y1, x2, y2;
      int n = 0;

      for (UVRangeBaseLineData uvBL : targetUVObservability) {
        xySeriesBL = new XYSeries("Observable " + uvBL.getName(), false);
        xySeriesBL.setNotify(false);

        u = uvBL.getU();
        v = uvBL.getV();
        u2 = uvBL.getU2();
        v2 = uvBL.getV2();

        for (int i = 0, size = uvBL.getNPoints(); i < size; i++) {
          x1 = toUVPlotScale(u[i]);
          y1 = toUVPlotScale(v[i]);

          x2 = toUVPlotScale(u2[i]);
          y2 = toUVPlotScale(v2[i]);

          // first segment :
          xySeriesBL.add(x1, y1);
          xySeriesBL.add(x2, y2);

          // add an invalid point to break the line between the 2 segments :
          xySeriesBL.add(Double.NaN, Double.NaN);

          // second symetric segment :
          xySeriesBL.add(-x1, -y1);
          xySeriesBL.add(-x2, -y2);

          // add an invalid point to break the line between the 2 segments :
          xySeriesBL.add(Double.NaN, Double.NaN);

        } // points

        xySeriesBL.setNotify(true);
        dataset.addSeries(xySeriesBL);

        // color :
        renderer.setSeriesPaint(n, palette.getColor(n), false);

        n++;
      } // BL
    }
  }
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JButton jButtonModelEditor;
  private javax.swing.JButton jButtonOB;
  private javax.swing.JButton jButtonPDF;
  private javax.swing.JCheckBox jCheckBoxModelImage;
  private javax.swing.JCheckBox jCheckBoxPlotUVSupport;
  private javax.swing.JComboBox jComboBoxAtmQual;
  private javax.swing.JComboBox jComboBoxFTMode;
  private javax.swing.JComboBox jComboBoxImageMode;
  private javax.swing.JComboBox jComboBoxImageSize;
  private javax.swing.JComboBox jComboBoxInstrumentMode;
  private javax.swing.JComboBox jComboBoxLUT;
  private javax.swing.JComboBox jComboBoxTarget;
  private javax.swing.JFormattedTextField jFieldHAMax;
  private javax.swing.JFormattedTextField jFieldHAMin;
  private javax.swing.JFormattedTextField jFieldSamplingPeriod;
  private javax.swing.JFormattedTextField jFieldUVMax;
  private javax.swing.JLabel jLabel1;
  private javax.swing.JLabel jLabel2;
  private javax.swing.JLabel jLabel3;
  private javax.swing.JLabel jLabel4;
  private javax.swing.JLabel jLabel5;
  private javax.swing.JLabel jLabel6;
  private javax.swing.JLabel jLabel7;
  private javax.swing.JLabel jLabel8;
  private javax.swing.JLabel jLabel9;
  private javax.swing.JLabel jLabelAtmQual;
  private javax.swing.JLabel jLabelFTMode;
  private javax.swing.JPanel jPanelButtons;
  private javax.swing.JPanel jPanelRight;
  private javax.swing.JSeparator jSeparator1;
  private javax.swing.JSeparator jSeparator3;
  private javax.swing.JSlider jSliderHAMax;
  private javax.swing.JSlider jSliderHAMin;
  private javax.swing.JSlider jSliderUVMax;
  private javax.swing.JSplitPane jSplitPane;
  private javax.swing.JLabel jTargetHAMax;
  private javax.swing.JLabel jTargetHAMin;
  // End of variables declaration//GEN-END:variables
  /** drawing started time value */
  private long lastTime = 0l;

  public void chartProgress(final ChartProgressEvent event) {
    if (logger.isLoggable(Level.FINE)) {
      switch (event.getType()) {
        case ChartProgressEvent.DRAWING_STARTED:
          this.lastTime = System.currentTimeMillis();
          break;
        case ChartProgressEvent.DRAWING_FINISHED:
          logger.fine("Drawing chart time : " + (System.currentTimeMillis() - lastTime) + " ms.");
          this.lastTime = 0l;
          break;
        default:
      }
    }
  }

  private static String format(final JFormattedTextField field, final Double value) {
    String res = "";
    try {
      res = field.getFormatter().valueToString(value);
    } catch (ParseException pe) {
      logger.log(Level.SEVERE, "parsing exception", pe);
    }
    return res;
  }

  public void setUvPlotScalingFactor(double uvPlotScalingFactor) {
    this.uvPlotScalingFactor = uvPlotScalingFactor;
  }

  /**
   * Convert the given value (u or v) to the plot scale
   * @param value u or v coordinate in rad-1
   * @return u or v coordinate in the plot unit
   */
  private final double toUVPlotScale(final double value) {
    return uvPlotScalingFactor * value;
  }

  /**
   * Convert the given plot value (u or v) to the standard unit (rad-1)
   * @param value u or v coordinate in the plot unit
   * @return u or v coordinate in rad-1
   */
  private final double fromUVPlotScale(final double value) {
    return value / uvPlotScalingFactor;
  }

  /**
   * Return the currently selected target name
   * @return target name
   */
  public String getSelectedTargetName() {
    return (String) this.jComboBoxTarget.getSelectedItem();
  }
}
