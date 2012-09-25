/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.jmcs.gui.component.Disposable;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.gui.action.ExportOBVLTIAction;
import fr.jmmc.aspro.gui.action.ExportOBVegaAction;
import fr.jmmc.aspro.gui.action.AsproExportPDFAction;
import fr.jmmc.oiexplorer.core.gui.chart.BoundedNumberAxis;
import fr.jmmc.oiexplorer.core.gui.chart.ChartUtils;
import fr.jmmc.aspro.gui.chart.ColorModelPaintScale;
import fr.jmmc.oiexplorer.core.gui.chart.PDFOptions;
import fr.jmmc.aspro.gui.chart.PaintLogScaleLegend;
import fr.jmmc.oiexplorer.core.gui.chart.SquareChartPanel;
import fr.jmmc.oiexplorer.core.gui.chart.SquareXYPlot;
import fr.jmmc.oiexplorer.core.gui.chart.ZoomEvent;
import fr.jmmc.oiexplorer.core.gui.chart.ZoomEventListener;
import fr.jmmc.aspro.gui.task.AsproTaskRegistry;
import fr.jmmc.aspro.gui.task.ObservationCollectionTaskSwingWorker;
import fr.jmmc.oiexplorer.core.gui.chart.ColorPalette;
import fr.jmmc.aspro.gui.util.FieldSliderAdapter;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.ObservationCollectionUVData;
import fr.jmmc.aspro.model.event.ObservationListener;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.event.ObservabilityEvent;
import fr.jmmc.aspro.model.event.ObservationEvent;
import fr.jmmc.aspro.model.event.TargetSelectionEvent;
import fr.jmmc.aspro.model.event.UpdateObservationEvent;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.oi.AtmosphereQuality;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.ObservationCollection;
import fr.jmmc.aspro.model.uvcoverage.UVCoverageData;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.model.oi.UserModel;
import fr.jmmc.aspro.model.util.AtmosphereQualityUtils;
import fr.jmmc.aspro.model.uvcoverage.UVBaseLineData;
import fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData;
import fr.jmmc.aspro.ob.ExportOBMode;
import fr.jmmc.aspro.service.NoiseService;
import fr.jmmc.aspro.service.OIFitsCreatorService;
import fr.jmmc.aspro.service.UVCoverageService;
import fr.jmmc.aspro.service.UserModelService;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.gui.task.TaskSwingWorker;
import fr.jmmc.jmal.image.ColorModels;
import fr.jmmc.jmal.image.ColorScale;
import fr.jmmc.jmal.model.ModelUVMapService;
import fr.jmmc.jmal.model.ImageMode;
import fr.jmmc.jmal.model.UVMapData;
import fr.jmmc.jmal.model.VisNoiseService;
import fr.jmmc.jmal.model.targetmodel.Model;
import fr.jmmc.jmcs.gui.task.TaskSwingWorkerExecutor;
import fr.jmmc.jmcs.util.ObjectUtils;
import fr.jmmc.jmcs.util.concurrent.InterruptedJobException;
import fr.jmmc.oiexplorer.core.gui.PDFExportable;
import fr.jmmc.oiexplorer.core.util.Constants;
import fr.jmmc.oitools.image.FitsImage;
import fr.jmmc.oitools.model.OIFitsFile;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import java.util.Vector;
import java.util.concurrent.ExecutionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JFormattedTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.axis.AxisLocation;
import org.jfree.chart.axis.LogarithmicAxis;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.block.BlockBorder;
import org.jfree.chart.event.ChartProgressEvent;
import org.jfree.chart.event.ChartProgressListener;
import org.jfree.chart.renderer.AbstractRenderer;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.chart.title.PaintScaleLegend;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.ui.Layer;
import org.jfree.ui.RectangleEdge;

/**
 * This panel presents the UV coverage plot with its parameters (target, instrument mode ...)
 * @author bourgesl
 */
public final class UVCoveragePanel extends javax.swing.JPanel implements ChartProgressListener, ZoomEventListener,
        ActionListener, ChangeListener, ObservationListener, Observer, PDFExportable, Disposable {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1L;
  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(UVCoveragePanel.class.getName());
  /** message indicating computations */
  private static final String MSG_COMPUTING_COVERAGE = "computing uv coverage ...";
  /** message indicating computations */
  private static final String MSG_COMPUTING_MAP = "computing uv map ...";
  /** message indicating computations */
  private static final String MSG_COMPUTING_OIFITS = "computing OIFits data ...";
  /** flag to log a stack trace in method updateObservation() to detect multiple calls */
  private final static boolean DEBUG_UPDATE_EVENT = false;
  /** flag to log a stack trace in method plot() to detect multiple calls */
  private final static boolean DEBUG_PLOT_EVENT = false;
  /** flag to log version checking */
  private final static boolean DEBUG_VERSIONS = false;
  /** scaling factor to Mega Lambda for U,V points */
  private final static double MEGA_LAMBDA_SCALE = 1e-6d;
  /** observation manager */
  private final static ObservationManager om = ObservationManager.getInstance();

  /* members */
  /** preference singleton */
  private final Preferences myPreferences = Preferences.getInstance();
  /** jFreeChart instance */
  private JFreeChart chart;
  /** xy plot instance */
  private SquareXYPlot xyPlot;
  /** JMMC annotation */
  private XYTextAnnotation aJMMC = null;
  /** uv map image scale legend */
  private PaintScaleLegend mapLegend = null;
  /** uv coordinates scaling factor */
  private double uvPlotScalingFactor = MEGA_LAMBDA_SCALE;

  /* cached data */
  /** current interferometer configuration name to track changes */
  private String interferometerConfigurationName = null;
  /** current instrument name to track changes */
  private String instrumentName = null;
  /** current selected target */
  private Target currentTarget = null;
  /* cached computed data */
  /** last computed Observability Data */
  private List<ObservabilityData> currentObsData = null;

  /* plot data */
  /** last zoom event to check if the zoom area changed */
  private ZoomEvent lastZoomEvent = null;
  /** chart data */
  private ObservationCollectionUVData chartData = null;
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
        jScrollPaneForm = new javax.swing.JScrollPane();
        jPanelLeft = new javax.swing.JPanel();
        jLabelInstrumentMode = new javax.swing.JLabel();
        jComboBoxInstrumentMode = new javax.swing.JComboBox();
        jLabelAtmQual = new javax.swing.JLabel();
        jComboBoxAtmQual = new javax.swing.JComboBox();
        jLabelFTMode = new javax.swing.JLabel();
        jComboBoxFTMode = new javax.swing.JComboBox();
        jSeparator3 = new javax.swing.JSeparator();
        jLabelUVMax = new javax.swing.JLabel();
        jSliderUVMax = new javax.swing.JSlider();
        jFieldUVMax = new javax.swing.JFormattedTextField();
        jLabelSamplingPeriod = new javax.swing.JLabel();
        jFieldSamplingPeriod = new javax.swing.JFormattedTextField();
        jLabelObsDuration = new javax.swing.JLabel();
        jFieldObsDuration = new javax.swing.JFormattedTextField();
        jLabelHAMin = new javax.swing.JLabel();
        jSliderHAMin = new javax.swing.JSlider();
        jTargetHAMin = new javax.swing.JLabel();
        jFieldHAMin = new javax.swing.JFormattedTextField();
        jLabelHAMax = new javax.swing.JLabel();
        jSliderHAMax = new javax.swing.JSlider();
        jTargetHAMax = new javax.swing.JLabel();
        jFieldHAMax = new javax.swing.JFormattedTextField();
        jCheckBoxPlotUVSupport = new javax.swing.JCheckBox();
        jSeparator1 = new javax.swing.JSeparator();
        jCheckBoxModelImage = new javax.swing.JCheckBox();
        jLabelImageMode = new javax.swing.JLabel();
        jComboBoxImageMode = new javax.swing.JComboBox();
        jSeparator2 = new javax.swing.JSeparator();
        jCheckBoxAddNoise = new javax.swing.JCheckBox();
        jPanelSpacer = new javax.swing.JPanel();
        jCheckBoxDoOIFits = new javax.swing.JCheckBox();

        setLayout(new java.awt.BorderLayout());

        jSplitPane.setDividerSize(5);
        jSplitPane.setResizeWeight(0.05);
        jSplitPane.setContinuousLayout(true);
        jSplitPane.setMinimumSize(new java.awt.Dimension(320, 400));
        jSplitPane.setPreferredSize(new java.awt.Dimension(320, 400));

        jScrollPaneForm.setMinimumSize(new java.awt.Dimension(200, 400));
        jScrollPaneForm.setPreferredSize(new java.awt.Dimension(220, 400));

        jPanelLeft.setMinimumSize(new java.awt.Dimension(185, 550));
        jPanelLeft.setLayout(new java.awt.GridBagLayout());

        jLabelInstrumentMode.setText("Instrument mode");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.insets = new java.awt.Insets(6, 0, 0, 0);
        jPanelLeft.add(jLabelInstrumentMode, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
        jPanelLeft.add(jComboBoxInstrumentMode, gridBagConstraints);

        jLabelAtmQual.setText("Atmosphere quality");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 2;
        jPanelLeft.add(jLabelAtmQual, gridBagConstraints);

        jComboBoxAtmQual.setModel(new DefaultComboBoxModel(AtmosphereQualityUtils.getAtmosphereQualityList()));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
        jPanelLeft.add(jComboBoxAtmQual, gridBagConstraints);

        jLabelFTMode.setText("Fringe tracker mode");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.gridwidth = 2;
        jPanelLeft.add(jLabelFTMode, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelLeft.add(jComboBoxFTMode, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weighty = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelLeft.add(jSeparator3, gridBagConstraints);

        jLabelUVMax.setText("U-V range to plot (m)");
        jLabelUVMax.setToolTipText("");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.gridwidth = 2;
        jPanelLeft.add(jLabelUVMax, gridBagConstraints);

        jSliderUVMax.setMajorTickSpacing(10);
        jSliderUVMax.setPaintTicks(true);
        jSliderUVMax.setValue(100);
        jSliderUVMax.setMaximumSize(new java.awt.Dimension(80, 32767));
        jSliderUVMax.setPreferredSize(new java.awt.Dimension(80, 30));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
        jPanelLeft.add(jSliderUVMax, gridBagConstraints);

        jFieldUVMax.setColumns(6);
        jFieldUVMax.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0.00"))));
        jFieldUVMax.setMinimumSize(new java.awt.Dimension(50, 20));
        jFieldUVMax.setName("jFieldUVMax"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
        jPanelLeft.add(jFieldUVMax, gridBagConstraints);

        jLabelSamplingPeriod.setText("Sampling Periodicity (min)");
        jLabelSamplingPeriod.setToolTipText("One set of calibrated visibilities in the u-v plane is taken at this interval (minutes)"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 13;
        gridBagConstraints.gridwidth = 2;
        jPanelLeft.add(jLabelSamplingPeriod, gridBagConstraints);

        jFieldSamplingPeriod.setColumns(3);
        jFieldSamplingPeriod.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter()));
        jFieldSamplingPeriod.setMinimumSize(new java.awt.Dimension(40, 20));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 14;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
        jPanelLeft.add(jFieldSamplingPeriod, gridBagConstraints);

        jLabelObsDuration.setText("Total Integration time (s)");
        jLabelObsDuration.setToolTipText("Time REALLY spent on-source, in seconds, per calibrated point");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 15;
        gridBagConstraints.gridwidth = 2;
        jPanelLeft.add(jLabelObsDuration, gridBagConstraints);

        jFieldObsDuration.setColumns(3);
        jFieldObsDuration.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter()));
        jFieldObsDuration.setMinimumSize(new java.awt.Dimension(40, 20));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 16;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
        jPanelLeft.add(jFieldObsDuration, gridBagConstraints);

        jLabelHAMin.setText("HA min");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 17;
        jPanelLeft.add(jLabelHAMin, gridBagConstraints);

        jSliderHAMin.setMajorTickSpacing(30);
        jSliderHAMin.setMaximum(240);
        jSliderHAMin.setPaintTicks(true);
        jSliderHAMin.setMaximumSize(new java.awt.Dimension(80, 32767));
        jSliderHAMin.setPreferredSize(new java.awt.Dimension(80, 30));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 18;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
        jPanelLeft.add(jSliderHAMin, gridBagConstraints);

        jTargetHAMin.setText("targetHAMin");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 17;
        jPanelLeft.add(jTargetHAMin, gridBagConstraints);

        jFieldHAMin.setColumns(6);
        jFieldHAMin.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0.00"))));
        jFieldHAMin.setMinimumSize(new java.awt.Dimension(50, 20));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 18;
        gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
        jPanelLeft.add(jFieldHAMin, gridBagConstraints);

        jLabelHAMax.setText("HA max");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 19;
        jPanelLeft.add(jLabelHAMax, gridBagConstraints);

        jSliderHAMax.setMajorTickSpacing(30);
        jSliderHAMax.setMaximum(240);
        jSliderHAMax.setPaintTicks(true);
        jSliderHAMax.setMaximumSize(new java.awt.Dimension(80, 32767));
        jSliderHAMax.setPreferredSize(new java.awt.Dimension(80, 30));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
        jPanelLeft.add(jSliderHAMax, gridBagConstraints);

        jTargetHAMax.setText("targetHAMax");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 19;
        jPanelLeft.add(jTargetHAMax, gridBagConstraints);

        jFieldHAMax.setColumns(6);
        jFieldHAMax.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0.00"))));
        jFieldHAMax.setMinimumSize(new java.awt.Dimension(50, 20));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
        jPanelLeft.add(jFieldHAMax, gridBagConstraints);

        jCheckBoxPlotUVSupport.setSelected(true);
        jCheckBoxPlotUVSupport.setText("<html>Plot rise/set uv tracks</html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 21;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanelLeft.add(jCheckBoxPlotUVSupport, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 26;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weighty = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelLeft.add(jSeparator1, gridBagConstraints);

        jCheckBoxModelImage.setSelected(true);
        jCheckBoxModelImage.setText("<html>Underplot a model image</html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 24;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanelLeft.add(jCheckBoxModelImage, gridBagConstraints);

        jLabelImageMode.setText("Plot what ...");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 25;
        jPanelLeft.add(jLabelImageMode, gridBagConstraints);

        jComboBoxImageMode.setModel(new DefaultComboBoxModel(ImageMode.values()));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 25;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
        jPanelLeft.add(jComboBoxImageMode, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 23;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weighty = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelLeft.add(jSeparator2, gridBagConstraints);

        jCheckBoxAddNoise.setSelected(true);
        jCheckBoxAddNoise.setText("Add error noise to data");
        jCheckBoxAddNoise.setName("jCheckBoxAddNoise"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 28;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
        jPanelLeft.add(jCheckBoxAddNoise, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 30;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weighty = 0.1;
        jPanelLeft.add(jPanelSpacer, gridBagConstraints);

        jCheckBoxDoOIFits.setSelected(true);
        jCheckBoxDoOIFits.setText("Compute OIFits data");
        jCheckBoxDoOIFits.setName("jCheckBoxDoOIFits"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 27;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
        jPanelLeft.add(jCheckBoxDoOIFits, gridBagConstraints);

        jScrollPaneForm.setViewportView(jPanelLeft);

        jSplitPane.setLeftComponent(jScrollPaneForm);

        add(jSplitPane, java.awt.BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents

  /**
   * Export Observing Block(s) (OB)
   * @param evt action event
   * @param mode export OB mode
   */
  public void performOBAction(final ActionEvent evt, final ExportOBMode mode) {

    // Use main observation to check instrument :
    final ObservationSetting observation = om.getMainObservation();

    if (!observation.isSingle()) {
      MessagePane.showMessage("Aspro 2 can not generate an Observing Block when multiple configurations are selected !");
      return;
    }

    final String insName = observation.getInstrumentConfiguration().getName();

    if (AsproConstants.INS_AMBER.equals(insName)
            || AsproConstants.INS_MIDI.equals(insName)
            || AsproConstants.INS_PIONIER.equals(insName)) {

      // set the source with this instance :
      evt.setSource(this);

      switch (mode) {
        case ALL:
          ExportOBVLTIAction.getInstance().process(observation.getTargets());
          break;
        case SINGLE:
          ExportOBVLTIAction.getInstance().process(Arrays.asList(new Target[]{getSelectedTarget()}));
          break;
        default:
      }

    } else if (insName.startsWith(AsproConstants.INS_VEGA)) {

      ExportOBVegaAction.getInstance().process();

    } else {
      MessagePane.showMessage("Aspro 2 can not generate an Observing Block for this instrument [" + insName + "] !");
    }
  }

  /**
   * Export the current chart as a PDF document
   * @param evt action event
   */
  /**
   * Export the chart component as a PDF document
   */
  @Override
  public void performPDFAction() {
    AsproExportPDFAction.exportPDF(this);
  }

  /**
   * Return the PDF default file name
   * @return PDF default file name
   */
  @Override
  public String getPDFDefaultFileName() {
    if (this.getChartData() != null) {
      final ObservationSetting observation = this.getChartData().getFirstObservation();

      final StringBuilder sb = new StringBuilder(32);
      sb.append("UV_");
      sb.append(this.getChartData().getTargetName().replaceAll(fr.jmmc.aspro.AsproConstants.REGEXP_INVALID_TEXT_CHARS, "_")).append('_');
      sb.append(observation.getInstrumentConfiguration().getName()).append('_');
      sb.append(this.getChartData().getDisplayConfigurations("_", true));
      if (observation.getWhen().isNightRestriction()) {
        sb.append('_');
        sb.append(observation.getWhen().getDate().toString());
      }
      sb.append('.').append(PDF_EXT);

      return sb.toString();
    }
    return null;
  }

  /**
   * Return the PDF options
   * @return PDF options
   */
  @Override
  public PDFOptions getPDFOptions() {
    return PDFOptions.DEFAULT_PDF_OPTIONS;
  }

  /**
   * Return the chart to export as a PDF document
   * @return chart
   */
  @Override
  public JFreeChart prepareChart() {
    return this.chart;
  }

  /**
   * Callback indicating the chart was processed by the PDF engine
   */
  @Override
  public void postPDFExport() {
    // no-op
  }

  /**
   * This method is useful to set the models and specific features of initialized swing components :
   */
  private void postInit() {

    this.chart = ChartUtils.createSquareXYLineChart("U (M\u03BB)", "V (M\u03BB)", true);
    this.xyPlot = (SquareXYPlot) this.chart.getPlot();

    // Adjust background settings :
    this.xyPlot.setBackgroundImageAlpha(1.0f);

    // create new JMMC annotation (moving position):
    this.aJMMC = ChartUtils.createJMMCAnnotation(AsproConstants.JMMC_ANNOTATION);
    this.xyPlot.getRenderer().addAnnotation(this.aJMMC, Layer.BACKGROUND);

    // add UV axes in meters:
    final BoundedNumberAxis uAxisMeter = new BoundedNumberAxis("U (m)");
    uAxisMeter.setAutoRangeIncludesZero(false);
    uAxisMeter.setTickLabelFont(ChartUtils.DEFAULT_TITLE_FONT);
    this.xyPlot.setDomainAxis(1, uAxisMeter);

    final BoundedNumberAxis vAxisMeter = new BoundedNumberAxis("V (m)");
    vAxisMeter.setAutoRangeIncludesZero(false);
    vAxisMeter.setTickLabelFont(ChartUtils.DEFAULT_TITLE_FONT);
    this.xyPlot.setRangeAxis(1, vAxisMeter);

    // add listener :
    this.chart.addProgressListener(this);
    this.chartPanel = ChartUtils.createSquareChartPanel(this.chart);

    // zoom options :
    this.chartPanel.setDomainZoomable(Constants.ENABLE_ZOOM);
    this.chartPanel.setRangeZoomable(Constants.ENABLE_ZOOM);

    // define zoom listener :
    this.chartPanel.setZoomEventListener(this);

    // define min and prefered size for chart panel used by the split pane container :
    this.chartPanel.setMinimumSize(new Dimension(650, 500));
    this.jSplitPane.setRightComponent(this.chartPanel);

    // define change listeners :
    this.jComboBoxInstrumentMode.addActionListener(this);
    this.jComboBoxFTMode.addActionListener(this);
    this.jComboBoxAtmQual.addActionListener(this);

    this.uvMaxAdapter = new FieldSliderAdapter(jSliderUVMax, jFieldUVMax, 0d, 0d, 0d);
    this.uvMaxAdapter.addChangeListener(this);

    // define property change listener :
    this.jFieldSamplingPeriod.addPropertyChangeListener("value", new PropertyChangeListener() {
      @Override
      public void propertyChange(final PropertyChangeEvent evt) {
        final double newValue = ((Number) jFieldSamplingPeriod.getValue()).doubleValue();

        if (newValue <= 0d) {
          // invalid value :
          resetSamplingPeriod(om.getMainObservation());
        }

        // Check that obs duration is less than jFieldSamplingPeriod / 2:
        final double halfSamplingSec = ((Number) jFieldSamplingPeriod.getValue()).doubleValue() * 60d * 0.5d;
        final double obsDurationSec = ((Number) jFieldObsDuration.getValue()).doubleValue();

        if (obsDurationSec > halfSamplingSec) {
          // update obs duration:
          jFieldObsDuration.setValue(Double.valueOf(halfSamplingSec));
        }

        if (logger.isDebugEnabled()) {
          logger.debug("samplingPeriod changed: {}", newValue);
        }
        fireObservationUpdateEvent();
      }
    });

    // default obs duration and property change listener :
    this.jFieldObsDuration.setValue(AsproConstants.DEFAULT_OBSERVATION_DURATION);
    this.jFieldObsDuration.addPropertyChangeListener("value", new PropertyChangeListener() {
      @Override
      public void propertyChange(final PropertyChangeEvent evt) {
        final double newValue = ((Number) jFieldObsDuration.getValue()).doubleValue();

        if (newValue <= 0d) {
          // invalid value :
          jFieldObsDuration.setValue(AsproConstants.DEFAULT_OBSERVATION_DURATION);
        }

        if (logger.isDebugEnabled()) {
          logger.debug("obsDuration changed: {}", newValue);
        }
        fireObservationUpdateEvent();
      }
    });

    this.haMinAdapter = new FieldSliderAdapter(jSliderHAMin, jFieldHAMin, AsproConstants.HA_MIN, AsproConstants.HA_MAX, AsproConstants.HA_MIN);
    this.haMinAdapter.addChangeListener(this);

    this.haMaxAdapter = new FieldSliderAdapter(jSliderHAMax, jFieldHAMax, AsproConstants.HA_MIN, AsproConstants.HA_MAX, AsproConstants.HA_MAX);
    this.haMaxAdapter.addChangeListener(this);

    this.jTargetHAMin.setText(null);
    this.jTargetHAMax.setText(null);

    this.jCheckBoxPlotUVSupport.addItemListener(new ItemListener() {
      @Override
      public void itemStateChanged(final ItemEvent e) {
        refreshPlot();
      }
    });

    this.jCheckBoxModelImage.addItemListener(new ItemListener() {
      @Override
      public void itemStateChanged(final ItemEvent e) {
        final boolean enabled = jCheckBoxModelImage.isSelected();
        jComboBoxImageMode.setEnabled(enabled);

        refreshPlot();
      }
    });

    this.jComboBoxImageMode.addActionListener(this);

    this.jCheckBoxDoOIFits.addActionListener(this);
    this.jCheckBoxAddNoise.addActionListener(this);

    // register this instance as a Preference Observer :
    this.myPreferences.addObserver(this);
  }

  /**
   * Free any ressource or reference to this instance :
   * remove this instance form Preference Observers
   */
  @Override
  public void dispose() {
    if (logger.isDebugEnabled()) {
      logger.debug("dispose: {}", ObjectUtils.getObjectInfo(this));
    }

    // unregister this instance as a Preference Observer :
    this.myPreferences.deleteObserver(this);
  }

  /**
   * Overriden method to give object identifier
   * @return string identifier
   */
  @Override
  public String toString() {
    return "UVCoveragePanel@" + Integer.toHexString(hashCode());
  }

  /**
   * Listen to preferences changes
   * @param o Preferences
   * @param arg unused
   */
  @Override
  public void update(final Observable o, final Object arg) {
    logger.debug("Preferences updated on : {}", this);

    this.refreshPlot();
  }

  /**
   * Update the information relative to the interferometer (configuration) : UV max length and fringe tracker modes
   * @param observation current observation settings
   */
  private void updateInteferometerData(final ObservationSetting observation) {
    final String intConfName = observation.getInterferometerConfiguration().getName();
    // test if the interferometer changed :
    final boolean changed = intConfName != null && !intConfName.equals(this.interferometerConfigurationName);
    if (changed) {
      this.interferometerConfigurationName = intConfName;

      final InterferometerConfiguration intConf = observation.getInterferometerConfiguration().getInterferometerConfiguration();

      // update the UV range :
      final double minBaseLine = intConf.getMinBaseLine();
      final double maxBaseLine = intConf.getMaxBaseLine();

      if (logger.isDebugEnabled()) {
        logger.debug("interferometer configuration changed: {}; baseline min= {}, max= {}",
                intConfName, minBaseLine, maxBaseLine);
      }

      // adjust uv max range to [0.5 * minBaseLine; 2 * maxBaseLine] and
      // set value to maxBaseLine + 5% (margin):
      this.uvMaxAdapter.reset(0.5 * minBaseLine, 2.0d * maxBaseLine, 1.05d * maxBaseLine);
    }
  }

  /**
   * Refresh the information relative to the instrument : sampling time and instrument modes
   * @param observation current observation settings
   */
  private void updateInstrumentData(final ObservationSetting observation) {
    final String insName = observation.getInstrumentConfiguration().getName();
    // test if the instrument changed :
    final boolean changed = insName != null && !insName.equals(this.instrumentName);
    if (changed) {
      this.instrumentName = insName;

      logger.debug("instrument changed : {}", insName);

      resetSamplingPeriod(observation);

      // update instrument modes :
      final Vector<String> v = ConfigurationManager.getInstance().getInstrumentModes(
              observation.getInterferometerConfiguration().getName(),
              observation.getInstrumentConfiguration().getName());
      this.jComboBoxInstrumentMode.setModel(new DefaultComboBoxModel(v));

      if (logger.isTraceEnabled()) {
        logger.trace("jComboBoxInstrumentMode updated: {}", this.jComboBoxInstrumentMode.getSelectedItem());
      }

      // refresh the fringe tracker modes that depends on the interferometer :
      this.updateComboFTModes(observation);
    }
  }

  /**
   * Reset the sampling period to the default sampling time of the selected instrument
   * And reset the total integration time to defaults
   * @param observation observation to use
   */
  private void resetSamplingPeriod(final ObservationSetting observation) {
    // reset the sampling time to the default sampling time of the instrument :
    final int defaultSamplingTime = getInstrumentDefaultSamplingTime(observation);

    this.jFieldSamplingPeriod.setValue(Double.valueOf(defaultSamplingTime));
    this.jFieldObsDuration.setValue(AsproConstants.DEFAULT_OBSERVATION_DURATION);

    if (logger.isDebugEnabled()) {
      logger.debug("defaultSamplingTime: {}", defaultSamplingTime);
    }
  }

  /**
   * Return the default sampling time of the selected instrument
   * @param observation observation to use
   * @return default sampling time
   */
  private int getInstrumentDefaultSamplingTime(final ObservationSetting observation) {
    // get the default sampling time of the instrument :
    return ConfigurationManager.getInstance().getInstrumentSamplingTime(
            observation.getInterferometerConfiguration().getName(),
            observation.getInstrumentConfiguration().getName());
  }

  /**
   * Refresh the fringe tracker modes
   * @param observation current observation settings
   */
  private void updateComboFTModes(final ObservationSetting observation) {
    final Object oldValue = this.jComboBoxFTMode.getSelectedItem();

    final Vector<String> modes = ConfigurationManager.getInstance().getFringeTrackerModes(
            observation.getInterferometerConfiguration().getName(),
            observation.getInstrumentConfiguration().getName());

    // modes can be empty :
    this.jComboBoxFTMode.setModel(new DefaultComboBoxModel(modes));

    // restore previous selected item :
    if (oldValue != null) {
      this.jComboBoxFTMode.setSelectedItem(oldValue);
    }
    if (logger.isTraceEnabled()) {
      logger.trace("jComboBoxFTMode updated: {}", this.jComboBoxFTMode.getSelectedItem());
    }

    final boolean visible = !modes.isEmpty();
    this.jComboBoxFTMode.setVisible(visible);
    this.jLabelFTMode.setVisible(visible);
  }

  /**
   * Update UI according to the target configuration
   */
  private void updateTargetConfiguration() {
    final String targetName = getSelectedTargetName();

    final TargetConfiguration targetConf = om.getTargetConfiguration(targetName);
    if (targetConf != null) {

      logger.trace("updateTargetConfiguration : {}", targetName);

      // disable the automatic update observation :
      final boolean prevAutoUpdateObservation = this.setAutoUpdateObservation(false);
      try {
        // update HA Min / Max :
        final Double haMin = targetConf.getHAMin();
        this.haMinAdapter.setValue((haMin != null) ? haMin.doubleValue() : AsproConstants.HA_MIN);

        final Double haMax = targetConf.getHAMax();
        this.haMaxAdapter.setValue((haMax != null) ? haMax.doubleValue() : AsproConstants.HA_MAX);

        // update ft mode :
        if (this.jComboBoxFTMode.getModel().getSize() > 0) {
          final String ftMode = targetConf.getFringeTrackerMode();
          this.jComboBoxFTMode.setSelectedItem((ftMode != null) ? ftMode : AsproConstants.NONE);
        }
      } finally {
        // restore the automatic update observation :
        this.setAutoUpdateObservation(prevAutoUpdateObservation);
      }
    }
  }

  /**
   * Update the HA min / max according to the selected target and computed observability data (star data)
   */
  private void updateTargetHA() {
    boolean reset = true;

    // HA min / max are only relevant for a single observation :
    if (this.getObservabilityData() != null
            && this.getObservabilityData().size() == 1) {

      final String targetName = getSelectedTargetName();
      final StarData starData = this.getFirstObservabilityData().getStarData(targetName);
      if (starData != null) {
        final Double min = Range.getMinimum(starData.getObsRangesHA());
        final Double max = Range.getMaximum(starData.getObsRangesHA());

        this.jTargetHAMin.setText(format(this.jFieldHAMin, min));
        this.jTargetHAMax.setText(format(this.jFieldHAMax, max));

        if (logger.isDebugEnabled()) {
          logger.debug("target HA min: {}", min);
          logger.debug("target HA max: {}", min);
        }
        reset = false;
      }
    }
    if (reset) {
      this.jTargetHAMin.setText(null);
      this.jTargetHAMax.setText(null);
    }
  }

  /**
   * If the current target has no model defined, then disable model options widgets
   */
  private void changeStateForModelImageWidgets() {
    final Target target = getSelectedTarget();
    if (target != null) {
      final boolean hasModel = target.hasModel();

      this.jCheckBoxModelImage.setEnabled(hasModel);
      this.jComboBoxImageMode.setEnabled(hasModel);
    }
  }

  /**
   * Process any comboBox change event (target, instrument mode, image mode ...).
   * Refresh the dependent combo boxes and update the observation according to the form state
   * @param e action event
   */
  @Override
  public void actionPerformed(final ActionEvent e) {
    if (e.getSource() == this.jComboBoxInstrumentMode) {
      if (logger.isDebugEnabled()) {
        logger.debug("instrument mode changed: {}", this.jComboBoxInstrumentMode.getSelectedItem());
      }
      fireObservationUpdateEvent();

    } else if (e.getSource() == this.jComboBoxFTMode) {
      if (logger.isDebugEnabled()) {
        logger.debug("ft mode changed: {}", this.jComboBoxFTMode.getSelectedItem());
      }
      fireObservationUpdateEvent();

    } else if (e.getSource() == this.jComboBoxAtmQual) {
      if (logger.isDebugEnabled()) {
        logger.debug("atmQuality changed: {}", this.jComboBoxAtmQual.getSelectedItem());
      }
      fireObservationUpdateEvent();

    } else if (e.getSource() == this.jComboBoxImageMode) {
      if (logger.isDebugEnabled()) {
        logger.debug("image mode changed: {}", this.jComboBoxImageMode.getSelectedItem());
      }
      refreshPlot();
    } else if (e.getSource() == this.jCheckBoxAddNoise) {
      if (logger.isDebugEnabled()) {
        logger.debug("do noise: {}", this.jCheckBoxAddNoise.isSelected());
      }
      refreshPlot();
    } else if (e.getSource() == this.jCheckBoxDoOIFits) {
      if (logger.isDebugEnabled()) {
        logger.debug("do OIFits: {}", this.jCheckBoxDoOIFits.isSelected());
      }
      if (this.getChartData() != null) {
        if (this.jCheckBoxDoOIFits.isSelected()) {
          computeOIFits(this.getChartData());
        } else {
          // cancel anyway currently running OIFitsSwingWorker:
          if (TaskSwingWorkerExecutor.cancelTask(AsproTaskRegistry.TASK_OIFITS)) {
            // update the status bar:
            StatusBar.showIfPrevious(MSG_COMPUTING_OIFITS, "OIFits data cancelled.");

            // reset the OIFits structure in the current observation - No OIFitsSwingWorker running:
            ObservationManager.getInstance().setOIFitsList(null);
          }
        }
      }
    }
  }

  /**
   * Handle the stateChanged event from the FieldSliderAdapter instances
   * @param ce change event
   */
  @Override
  public void stateChanged(final ChangeEvent ce) {
    final FieldSliderAdapter source = (FieldSliderAdapter) ce.getSource();

    if (source == this.haMinAdapter) {
      if (logger.isDebugEnabled()) {
        logger.debug("haMin changed: {}", source.getValue());
      }
      this.haMaxAdapter.setMinValue(source.getValue());

      fireObservationUpdateEvent();

    } else if (source == this.haMaxAdapter) {
      if (logger.isDebugEnabled()) {
        logger.debug("haMax changed: {}", source.getValue());
      }
      this.haMinAdapter.setMaxValue(source.getValue());

      fireObservationUpdateEvent();

    } else if (source == this.uvMaxAdapter) {
      if (logger.isDebugEnabled()) {
        logger.debug("U-V Max changed: {}", source.getValue());
      }
      refreshPlot();
    }
  }

  /**
   * Fire an Observation Change event when a Swing component changed
   * ONLY if the automatic update flag is enabled
   */
  private void fireObservationUpdateEvent() {
    // check if the automatic update flag is enabled :
    if (this.doAutoUpdateObservation) {
      ObservationManager.getInstance().fireObservationUpdate();
    }
  }

  /**
   * Update the UI widgets from the given loaded observation
   *
   * @param observation observation
   */
  private void onLoadObservation(final ObservationSetting observation) {
    if (logger.isDebugEnabled()) {
      logger.debug("onLoadObservation :\n{}", ObservationManager.toString(observation));
    }
    // disable the automatic update observation :
    final boolean prevAutoUpdateObservation = this.setAutoUpdateObservation(false);
    // disable the automatic refresh :
    final boolean prevAutoRefresh = this.setAutoRefresh(false);
    try {

      // update the data related to the interferometer :
      this.interferometerConfigurationName = null;
      this.updateInteferometerData(observation);

      // refresh data related to the instrument :
      this.instrumentName = null;
      this.updateInstrumentData(observation);

      // update the selected instrument mode :
      if (observation.getInstrumentConfiguration().getInstrumentMode() != null) {
        this.jComboBoxInstrumentMode.setSelectedItem(observation.getInstrumentConfiguration().getInstrumentMode());
      }

      // update the sampling period :
      if (observation.getInstrumentConfiguration().getSamplingPeriod() != null) {
        this.jFieldSamplingPeriod.setValue(observation.getInstrumentConfiguration().getSamplingPeriod());
      }

      // update the acquisition time :
      if (observation.getInstrumentConfiguration().getAcquisitionTime() != null) {
        this.jFieldObsDuration.setValue(observation.getInstrumentConfiguration().getAcquisitionTime());
      } else {
        this.jFieldObsDuration.setValue(AsproConstants.DEFAULT_OBSERVATION_DURATION);
      }

      // update atmQuality :
      if (observation.getWhen().getAtmosphereQuality() != null) {
        this.jComboBoxAtmQual.setSelectedItem(observation.getWhen().getAtmosphereQuality().value());
      } else {
        this.jComboBoxAtmQual.setSelectedItem(AtmosphereQuality.AVERAGE);
      }

      // reset HA limits :
      this.haMinAdapter.setValue(AsproConstants.HA_MIN);
      this.haMaxAdapter.setValue(AsproConstants.HA_MAX);

      // reset to defaults :
      this.jCheckBoxPlotUVSupport.setSelected(true);
      this.jCheckBoxModelImage.setSelected(true);
      this.jComboBoxImageMode.setSelectedItem(ImageMode.AMP);

      // reset cached data :
      setObservabilityData(null);
      this.currentTarget = null;
      this.lastZoomEvent = null;
      this.chartData = null;

    } finally {
      // restore the automatic refresh :
      this.setAutoRefresh(prevAutoRefresh);
      // restore the automatic update observation :
      this.setAutoUpdateObservation(prevAutoUpdateObservation);
    }

    // Event chain : Load -> Changed :
    // Do not call updateObservation() as it will done soon by onChangeObservation()
  }

  /**
   * Update the selected target for the given observation
   * @param target selected target
   */
  private void onTargetSelectionChange(final Target target) {
    logger.debug("onTargetSelectionChange : {}", target);

    // disable the automatic update observation :
    final boolean prevAutoUpdateObservation = this.setAutoUpdateObservation(false);
    // disable the automatic refresh :
    final boolean prevAutoRefresh = this.setAutoRefresh(false);
    try {

      // update the current selected target :
      this.setSelectedTarget(target);

      updateTargetConfiguration();
      updateTargetHA();
      changeStateForModelImageWidgets();

    } finally {
      // restore the automatic refresh :
      this.setAutoRefresh(prevAutoRefresh);
      // restore the automatic update observation :
      this.setAutoUpdateObservation(prevAutoUpdateObservation);
    }
    refreshPlot();
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
    if (logger.isDebugEnabled()) {
      logger.debug("observation :\n{}", ObservationManager.toString(event.getObservation()));
    }

    // Refresh the UI widgets related to Observation Main changes :

    // disable the automatic update observation :
    final boolean prevAutoUpdateObservation = this.setAutoUpdateObservation(false);
    // disable the automatic refresh :
    final boolean prevAutoRefresh = this.setAutoRefresh(false);
    try {

      // update data related to the interferometer :
      this.updateInteferometerData(event.getObservation());

      // refresh data related to the instrument :
      this.updateInstrumentData(event.getObservation());

    } finally {
      // restore the automatic refresh :
      this.setAutoRefresh(prevAutoRefresh);
      // restore the automatic update observation :
      this.setAutoUpdateObservation(prevAutoUpdateObservation);
    }

    // Update Observation :

    // check if the automatic update flag is enabled :
    if (this.doAutoUpdateObservation) {
      if (DEBUG_UPDATE_EVENT) {
        logger.warn("UPDATE", new Throwable());
      }

      boolean changed = false;

      // Change the instrument mode :
      changed |= om.setInstrumentMode((String) this.jComboBoxInstrumentMode.getSelectedItem());

      // Update the sampling period :
      final Number samplingPeriod = (Number) this.jFieldSamplingPeriod.getValue();
      changed |= om.setInstrumentSamplingPeriod(Double.valueOf(samplingPeriod.doubleValue()));

      // Update the acquisition time :
      final Number obsDuration = (Number) this.jFieldObsDuration.getValue();
      changed |= om.setInstrumentAcquisitionTime(Double.valueOf(obsDuration.doubleValue()));

      // update atmQuality :
      changed |= om.setAtmosphereQuality(AtmosphereQualityUtils.getAtmosphereQuality((String) this.jComboBoxAtmQual.getSelectedItem()));

      final String targetName = getSelectedTargetName();

      if (targetName != null) {
        logger.debug("onUpdateObservation : {}", targetName);

        // Update target HA Min/Max :
        changed |= om.setTargetHAMin(targetName, Double.valueOf(this.haMinAdapter.getValue()));
        changed |= om.setTargetHAMax(targetName, Double.valueOf(this.haMaxAdapter.getValue()));

        // update ft mode :
        changed |= om.setTargetFTMode(targetName, (String) this.jComboBoxFTMode.getSelectedItem());
      }

      if (changed) {
        // update change flag to make the ObservationManager fire an observation refresh event later
        event.setChanged(UpdateObservationEvent.ChangeType.UV);
      }
    }
  }

  /**
   * Handle the given event on the given observation =
   * 1/ If the observation changed, refresh the UI widgets (targets ...)
   * 2/ If the observability is computed, then refresh the plot
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
      case TARGET_SELECTION_CHANGED:
        if (event instanceof TargetSelectionEvent) {
          this.onTargetSelectionChange(((TargetSelectionEvent) event).getTarget());
        }
        break;
      case DO_UPDATE:
        if (event instanceof UpdateObservationEvent) {
          this.onUpdateObservation((UpdateObservationEvent) event);
        }
        break;
      case REFRESH_UV:
        this.refreshPlot(event.getObservationCollection());
        break;
      case OBSERVABILITY_DONE:
        if (event instanceof ObservabilityEvent) {
          this.updateObservabilityData(((ObservabilityEvent) event).getObservabilityData());
          this.plot(event.getObservationCollection());
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
   * and update star data (HA min / max)
   * @param obsDataList observability data
   */
  private void updateObservabilityData(final List<ObservabilityData> obsDataList) {
    setObservabilityData(obsDataList);
    updateTargetHA();
  }

  /**
   * Define the observability Data
   * @param obsDataList observability Data
   */
  private void setObservabilityData(final List<ObservabilityData> obsDataList) {
    this.currentObsData = obsDataList;
  }

  /**
   * Return the observability Data
   * @return observability Data
   */
  private List<ObservabilityData> getObservabilityData() {
    return this.currentObsData;
  }

  /**
   * Return the first observability Data
   * @return first observability Data
   */
  private ObservabilityData getFirstObservabilityData() {
    return this.getObservabilityData().get(0);
  }

  /**
   * Refresh the plot when an UI widget changes that is not related to the observation.
   * Check the doAutoRefresh flag to avoid unwanted refresh (resetOptions)
   */
  private void refreshPlot() {
    if (this.doAutoRefresh) {
      logger.debug("refreshPlot");

      // use the latest observation for computations :
      this.refreshPlot(om.getObservationCollection());
    }
  }

  /**
   * Refresh the plot using the given observation.
   * Check if observation and observability data are consistent.
   * Used by refreshPlot() (UV widget change not related to observation)
   *  and by onProcess(REFRESH_UV) (UV widget change related to observation)
   *
   * @param obsCollection observation collection to use
   */
  private void refreshPlot(final ObservationCollection obsCollection) {
    if (obsCollection != null && this.getObservabilityData() != null) {
      // versions are the same for all observability data :
      final ObservabilityData obsData = this.getFirstObservabilityData();

      // avoid to mix inconsistent observation and observability data :
      // Next plot (observability done event) will take into account UI widget changes.

      if (obsData.getVersion().isSameMainVersion(obsCollection.getVersion())) {
        if (logger.isDebugEnabled()) {
          logger.debug("refreshPlot: main version equals: {} :: {}", obsData.getVersion(), obsCollection.getVersion());
        }
        if (DEBUG_VERSIONS) {
          logger.warn("refreshPlot: main version equals: {} :: {}", obsData.getVersion(), obsCollection.getVersion());
        }

        this.plot(obsCollection);
      } else {
        if (logger.isDebugEnabled()) {
          logger.debug("refreshPlot: main version mismatch: {} :: {}", obsData.getVersion(), obsCollection.getVersion());
        }
        if (DEBUG_VERSIONS) {
          logger.warn("refreshPlot: main version mismatch: {} :: {}", obsData.getVersion(), obsCollection.getVersion());
        }
      }
    }
  }

  /**
   * Plot the UV Coverage using a SwingWorker to do the computation in the background.
   * This code is executed by the Swing Event Dispatcher thread (EDT)
   * @param obsCollection observation collection to use
   */
  private void plot(final ObservationCollection obsCollection) {
    if (logger.isDebugEnabled()) {
      logger.debug("plot: {}", ObservationManager.toString(obsCollection));
    }
    if (DEBUG_PLOT_EVENT) {
      logger.warn("PLOT", new Throwable());
    }

    // Note : version checks are already done in refreshPlot(observation) :

    // check if observability data are available :
    if (this.getObservabilityData() != null) {

      /* get plot options from swing components */

      final String targetName = getSelectedTargetName();

      final double uvMax = this.uvMaxAdapter.getValue();
      final boolean doUVSupport = this.jCheckBoxPlotUVSupport.isSelected();
      final boolean doOIFits = this.jCheckBoxDoOIFits.isSelected();
      final boolean doDataNoise = this.jCheckBoxAddNoise.isSelected();
      final boolean doModelImage = this.jCheckBoxModelImage.isSelected();

      // model image options :
      final ImageMode imageMode = (ImageMode) this.jComboBoxImageMode.getSelectedItem();

      // Use model image Preferences :
      final int imageSize = this.myPreferences.getPreferenceAsInt(Preferences.MODEL_IMAGE_SIZE);
      final IndexColorModel colorModel = ColorModels.getColorModel(this.myPreferences.getPreference(Preferences.MODEL_IMAGE_LUT));
      final ColorScale colorScale = this.myPreferences.getImageColorScale();
      final boolean doImageNoise = this.myPreferences.getPreferenceAsBoolean(Preferences.MODEL_IMAGE_NOISE);

      // update the status bar :
      StatusBar.show(MSG_COMPUTING_COVERAGE);

      // Get previously computed UV Map Data (can be null) :
      final UVMapData currentUVMapData = (getChartData() != null) ? getChartData().getUVMapData() : null;

      // Create uv coverage task worker :
      // Cancel other tasks and execute this new task :
      new UVCoverageSwingWorker(this, obsCollection, this.getObservabilityData(), targetName,
              uvMax, doUVSupport, doOIFits, doDataNoise, doModelImage, imageMode, imageSize, colorModel, colorScale, doImageNoise,
              currentUVMapData).executeTask();

    } // observability data check
  }

  /**
   * TaskSwingWorker child class to compute uv coverage data and refresh the uv coverage plot
   */
  private final static class UVCoverageSwingWorker extends ObservationCollectionTaskSwingWorker<ObservationCollectionUVData> {

    /* members */
    /** uv panel used for refreshUI callback */
    private final UVCoveragePanel uvPanel;
    /** computed observability data */
    private final List<ObservabilityData> obsDataList;
    /** target name */
    private final String targetName;
    /** maximum U or V coordinate (lambda scale) */
    private double uvMax;
    /** flag to compute the UV support */
    private final boolean doUVSupport;
    /** flag to compute OIFits */
    private final boolean doOIFits;
    /** flag to add gaussian noise to OIFits data */
    private final boolean doDataNoise;
    /** flag to compute the model image */
    private final boolean doModelImage;
    /** image mode (amplitude or phase) */
    private final ImageMode imageMode;
    /** image size */
    private final int imageSize;
    /** image color model */
    private final IndexColorModel colorModel;
    /** color scaling method */
    private final ColorScale colorScale;
    /** flag to add gaussian noise to UV map */
    private final boolean doImageNoise;
    /** previously computed UV Map Data (or null) */
    private final UVMapData currentUVMapData;

    /**
     * Hidden constructor
     *
     * @param uvPanel observability panel
     * @param obsCollection observation collection to use
     * @param obsDataList observability data to use
     * @param targetName target name
     * @param uvMax U-V max in meter
     * @param doUVSupport flag to compute the UV support
     * @param doOIFits flag to compute OIFits
     * @param doDataNoise enable data noise
     * @param doModelImage flag to compute the model image
     * @param imageMode image mode (amplitude or phase)
     * @param imageSize expected number of pixels for both width and height of the generated image
     * @param colorModel color model to use
     * @param colorScale color scaling method
     * @param doImageNoise enable image noise
     * @param currentUVMapData previously computed UV Map Data
     */
    private UVCoverageSwingWorker(final UVCoveragePanel uvPanel, final ObservationCollection obsCollection,
            final List<ObservabilityData> obsDataList, final String targetName,
            final double uvMax, final boolean doUVSupport, final boolean doOIFits, final boolean doDataNoise,
            final boolean doModelImage, final ImageMode imageMode, final int imageSize,
            final IndexColorModel colorModel, final ColorScale colorScale, final boolean doImageNoise,
            final UVMapData currentUVMapData) {
      // get current observation version :
      super(AsproTaskRegistry.TASK_UV_COVERAGE, obsCollection);
      this.uvPanel = uvPanel;
      this.obsDataList = obsDataList;
      this.targetName = targetName;
      this.uvMax = uvMax;
      this.doUVSupport = doUVSupport;
      this.doOIFits = doOIFits;
      this.doDataNoise = doDataNoise;
      this.doModelImage = doModelImage;
      this.imageMode = imageMode;
      this.imageSize = imageSize;
      this.colorModel = colorModel;
      this.colorScale = colorScale;
      this.doImageNoise = doImageNoise;
      this.currentUVMapData = currentUVMapData;
    }

    /**
     * Compute the UV Coverage data in background
     * This code is executed by a Worker thread (Not Swing EDT)
     * @return UV Coverage data
     */
    @Override
    public ObservationCollectionUVData computeInBackground() {

      // TODO: externalize NoiseService and OIFitsCreatorService from UVCoverageService (IMPORTANT)

      // Start the computations :
      final long start = System.nanoTime();

      // get target from the first observation for consistency :
      final Target target = getObservationCollection().getFirstObservation().getTarget(this.targetName);

      final List<ObservationSetting> observations = getObservationCollection().getObservations();
      final List<UVCoverageData> uvDataList = new ArrayList<UVCoverageData>(observations.size());

      ObservationSetting observation;
      ObservabilityData obsData;

      for (int i = 0, len = observations.size(); i < len; i++) {
        observation = observations.get(i);
        obsData = this.obsDataList.get(i);

        // compute the uv coverage data :
        uvDataList.add(new UVCoverageService(observation, obsData, targetName, this.uvMax, this.doUVSupport, this.doDataNoise).compute());

        // fast interrupt :
        if (Thread.currentThread().isInterrupted()) {
          return null;
        }
      }

      final ObservationCollectionUVData uvDataCollection = new ObservationCollectionUVData(getObservationCollection(), this.obsDataList, uvDataList);

      // first UVCoverageData i.e. corresponding to the first observation:
      final UVCoverageData uvDataFirst = uvDataCollection.getFirstUVData();

      if (this.doModelImage) {
        // compute the uv map data :

        // update uvMax according to UVCoverage Service (wavelength correction):
        this.uvMax = uvDataFirst.getUvMax();

        // Get the noise service if enabled:
        // note: it depends on telescopes so it is enabled only for single configuration:
        final NoiseService noiseService = (this.doImageNoise && uvDataCollection.isSingle()) ? uvDataFirst.getNoiseService() : null;

        final Rectangle2D.Double uvRect = new Rectangle2D.Double();
        uvRect.setFrameFromDiagonal(-this.uvMax, -this.uvMax, this.uvMax, this.uvMax);

        if (target != null && target.hasModel()) {

          // get observation target version :
          final int targetVersion = uvDataCollection.getVersion().getTargetVersion();

          // Check if the previously computed UV Map Data is still valid :
          if (this.currentUVMapData != null
                  && this.currentUVMapData.isValid(targetName, targetVersion, uvRect,
                  this.imageMode, this.imageSize, this.colorModel, this.colorScale, noiseService)) {

            logger.debug("Reuse model image.");

            // reuse computed UV Map Data :
            uvDataCollection.setUvMapData(this.currentUVMapData);
          } else {
            try {
              UVMapData uvMapData;

              if (target.hasAnalyticalModel()) {
                // Analytical model:
                final List<Model> models = target.getModels();

                // Check if the previously computed visiblity Data is still valid :
                if (this.currentUVMapData != null
                        && this.currentUVMapData.isDataValid(targetName, targetVersion, uvRect, this.imageSize)) {

                  logger.debug("Reuse model visiblity.");

                  // Compute only image using existing complex visibility data :
                  uvMapData = ModelUVMapService.computeUVMap(models,
                          uvRect, null, null, this.currentUVMapData.getVisData(),
                          this.imageMode, this.imageSize, this.colorModel, this.colorScale, noiseService);

                } else {
                  logger.debug("Computing model image ...");

                  // Compute Target Model for the UV coverage limits ONCE :
                  uvMapData = ModelUVMapService.computeUVMap(models, uvRect,
                          this.imageMode, this.imageSize, this.colorModel, this.colorScale, noiseService);
                }
              } else {
                // User Model:

                // Get preloaded and prepared fits image:
                final FitsImage fitsImage = target.getUserModel().getFitsImage();

                if (fitsImage != null) {

                  // Check if the previously computed visiblity Data is still valid :
                  if (this.currentUVMapData != null
                          && this.currentUVMapData.isDataValid(targetName, targetVersion, uvRect, this.imageSize)) {

                    logger.debug("Reuse model visiblity.");

                    // Compute only image using existing complex visibility data :
                    uvMapData = UserModelService.computeUVMap(fitsImage,
                            uvRect, this.imageMode, this.imageSize, this.colorModel, this.colorScale, noiseService,
                            null, null, this.currentUVMapData.getVisData());

                  } else {
                    logger.debug("Computing model image ...");

                    try {
                      // Compute Target Model for the UV coverage limits ONCE :
                      // Note: throws IllegalArgumentException if the fits image is invalid:
                      uvMapData = UserModelService.computeUVMap(fitsImage,
                              uvRect, this.imageMode, this.imageSize, this.colorModel, this.colorScale, noiseService);

                    } catch (IllegalArgumentException iae) {
                      logger.warn("Incorrect fits image in file [{}]", target.getUserModel().getFile(), iae);

                      // disable model:
                      target.getUserModel().setFileValid(false);
                      uvMapData = null;
                    }
                  }
                } else {
                  uvMapData = null;
                }
              }

              if (uvMapData != null) {
                // define target name and version :
                uvMapData.setTargetName(targetName);
                uvMapData.setTargetVersion(targetVersion);

                uvDataCollection.setUvMapData(uvMapData);
              }

            } catch (InterruptedJobException ije) {
              logger.debug("Computing model image interrupted: ", ije);
              return null;
            }
          }
        }
      }

      // fast interrupt :
      if (Thread.currentThread().isInterrupted()) {
        return null;
      }

      // merged warning container :
      final WarningContainer mergedWarningContainer = new WarningContainer();

      // merge warning messages:
      if (uvDataCollection.isSingle()) {
        // ObservabilityService warnings:
        mergedWarningContainer.addWarningMessages(uvDataCollection.getFirstObsData().getWarningContainer());
        // UVCoverageService warnings:
        mergedWarningContainer.addWarningMessages(uvDataFirst.getWarningContainer());

      } else {

        if (uvDataCollection.getFirstObservation().getWhen().isNightRestriction()) {
          mergedWarningContainer.addWarningMessage(
                  "Multiple configurations cannot be done in one night (night restrictions are only valid for "
                  + uvDataCollection.getFirstObservation().getWhen().getDate().toString() + ")");
        }

        // ObservabilityService warnings:
        for (int i = 0, len = observations.size(); i < len; i++) {
          obsData = this.obsDataList.get(i);
          mergedWarningContainer.addWarningMessages(obsData.getWarningContainer());
        }

        // UVCoverageService warnings:
        for (UVCoverageData uvData : uvDataList) {
          mergedWarningContainer.addWarningMessages(uvData.getWarningContainer());
        }
      }

      // add warning if the user model is disabled:
      if (this.doModelImage) {
        if (target != null && !target.hasAnalyticalModel()) {
          final UserModel userModel = target.getUserModel();
          if (userModel != null && !userModel.isFileValid()) {
            mergedWarningContainer.addWarningMessage("User model [" + userModel.getName() + "] is disabled");
          }
        }
      }

      uvDataCollection.setWarningContainer(mergedWarningContainer);

      if (logger.isInfoEnabled()) {
        logger.info("compute[ObservationCollectionUVData]: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
      }

      return uvDataCollection;
    }

    /**
     * Refresh the plot using the computed UV Coverage data.
     * This code is executed by the Swing Event Dispatcher thread (EDT)
     * @param uvDataCollection computed UV Coverage data
     */
    @Override
    public void refreshUI(final ObservationCollectionUVData uvDataCollection) {
      // Start computing OIFits ...
      final boolean resetOIFits = (doOIFits) ? !this.uvPanel.computeOIFits(uvDataCollection) : true;

      if (!doOIFits) {
        // add warning to indicate that OIFits are disabled:
        uvDataCollection.getWarningContainer().addWarningMessage("OIFits data computation is disabled");
      }

      // reset the OIFits structure in the current observation - No OIFitsSwingWorker running:
      if (resetOIFits) {
        ObservationManager.getInstance().setOIFitsList(null);
      }

      // Fire a warnings ready event :
      ObservationManager.getInstance().fireWarningsReady(uvDataCollection.getWarningContainer());

      // Refresh the GUI using coherent data :
      this.uvPanel.updatePlot(uvDataCollection);
    }

    /**
     * Handle the execution exception that occured in the compute operation @see #computeInBackground()
     * This implementation resets the plot and opens a message dialog or the feedback report depending on the cause.
     *
     * @param ee execution exception
     */
    @Override
    public void handleException(final ExecutionException ee) {
      this.uvPanel.resetPlot();
      if (ee.getCause() instanceof IllegalArgumentException) {
        MessagePane.showErrorMessage(ee.getCause().getMessage());
      } else {
        super.handleException(ee);
      }
    }
  }

  /**
   * Start computing OIFits
   * @param uvDataCollection computed UV Coverage data
   * @return true if computation started
   */
  private boolean computeOIFits(final ObservationCollectionUVData uvDataCollection) {
    // check if OIFits computation already done ?
    if (uvDataCollection.isOIFitsDone()) {
      return true;
    }

    boolean computing = false;

    // check if the OIFits data are still available:
    if (ObservationManager.getInstance().getOIFitsList() != null) {
      // Check if the previously computed UV Data is still valid :
      final ObservationCollectionUVData currentUVData = getChartData();

      if (currentUVData != null) {
        // note: OIFitsCreatorService parameter dependencies:
        // observation {target, instrumentMode {lambdaMin, lambdaMax, nSpectralChannels}}
        // obsData {beams, baseLines, starData, sc (DateCalc)}
        // parameter: doDataNoise
        // results: computeObservableUV {HA, targetUVObservability} {obsData + observation{haMin/haMax, instrumentMode {lambdaMin, lambdaMax}}}
        // and warning container

        // check if computation needed:
        // - check observation (UV version includes main version)
        // - check obsData (main version)
        // - check doNoise: noiseService.isDoNoise()

        if (currentUVData.isOIFitsValid(uvDataCollection)) {
          // means no reset and current OIFits data are correct:
          return true;
        }
      }
    }

    // Note : the main observation can have changed while computation

    final ObservationCollection taskObsCollection = uvDataCollection;

    // use the latest observation for computations to check versions :
    final ObservationCollection lastObsCollection = om.getObservationCollection();

    // avoid to mix inconsistent observation and uv coverage data :
    // Next plot (uv coverage done event) will take into account UI widget changes.

    if (taskObsCollection.getVersion().isSameUVVersion(lastObsCollection.getVersion())) {
      if (logger.isDebugEnabled()) {
        logger.debug("computeOIFits: uv version equals: {} :: {}", taskObsCollection.getVersion(), lastObsCollection.getVersion());
      }
      if (DEBUG_VERSIONS) {
        logger.warn("computeOIFits: uv version equals: {} :: {}", taskObsCollection.getVersion(), lastObsCollection.getVersion());
      }

      final List<OIFitsCreatorService> oiFitsCreatorList = new ArrayList<OIFitsCreatorService>(uvDataCollection.size());

      OIFitsCreatorService oiFitsCreator;

      for (UVCoverageData uvData : uvDataCollection.getUVDataList()) {
        oiFitsCreator = uvData.getOiFitsCreator();

        // check if OIFits data available:
        if (oiFitsCreator != null) {
          oiFitsCreatorList.add(oiFitsCreator);
        }
      }

      if (oiFitsCreatorList.size() > 0) {
        computing = true;

        // update the status bar :
        StatusBar.show(MSG_COMPUTING_OIFITS);

        // Create OIFits task worker :
        // Cancel other tasks and execute this new task :
        new OIFitsSwingWorker(uvDataCollection, oiFitsCreatorList).executeTask();
      }
    }

    return computing;
  }

  /**
   * Reset the plot in case of model exception
   */
  private void resetPlot() {
    // disable chart & plot notifications:
    this.chart.setNotify(false);
    this.xyPlot.setNotify(false);
    try {
      ChartUtils.clearTextSubTitle(this.chart);

      this.lastZoomEvent = null;
      this.chartData = null;

      // reset bounds to [-1;1] (before setDataset) :
      this.xyPlot.defineBounds(1d);
      // reset dataset for baseline limits :
      this.xyPlot.setDataset(null);

      // update the background image :
      this.updateUVMap(null);

      // update theme at end :
      ChartUtilities.applyCurrentTheme(this.chart);

    } finally {
      // restore chart & plot notifications:
      this.xyPlot.setNotify(true);
      this.chart.setNotify(true);
    }
    // update the status bar:
    StatusBar.showIfPrevious(MSG_COMPUTING_COVERAGE, "uv coverage done.");
  }

  /**
   * Return the chart data
   * @return chart data
   */
  private ObservationCollectionUVData getChartData() {
    return this.chartData;
  }

  /**
   * Define the chart data
   * @param chartData chart data
   */
  private void setChartData(final ObservationCollectionUVData chartData) {
    this.chartData = chartData;
  }

  /**
   * Refresh the plot using chart data.
   * This code is executed by the Swing Event Dispatcher thread (EDT)
   *
   * @param chartData chart data
   */
  private void updatePlot(final ObservationCollectionUVData chartData) {
    // memorize chart data (used by export PDF and zoom handling) :
    setChartData(chartData);

    if (chartData.getTargetName() == null) {
      // Baseline limits case :
      resetPlot();

    } else {
      // reset zoom cache :
      this.lastZoomEvent = null;

      final ObservationSetting observation = chartData.getFirstObservation();
      final ObservabilityData obsData = chartData.getFirstObsData();
      final UVMapData uvMapData = chartData.getUVMapData();

      // disable chart & plot notifications:
      this.chart.setNotify(false);
      this.xyPlot.setNotify(false);
      try {
        // title :
        ChartUtils.clearTextSubTitle(this.chart);

        final StringBuilder sb = new StringBuilder(32);
        sb.append(chartData.getInterferometerConfiguration(false)).append(" - ");
        sb.append(observation.getInstrumentConfiguration().getName()).append(" - ");
        sb.append(chartData.getDisplayConfigurations(" / "));
        if ((chartData.isSingle() || obsData.isUserPops()) && obsData.getBestPops() != null) {
          obsData.getBestPops().toString(sb);
        }
        ChartUtils.addSubtitle(this.chart, sb.toString());

        if (observation.getWhen().isNightRestriction()) {
          // date - Source:
          ChartUtils.addSubtitle(this.chart, "Day: " + observation.getWhen().getDate().toString()
                  + " - Source: " + chartData.getTargetName());
        } else {
          // Source only:
          ChartUtils.addSubtitle(this.chart, "Source: " + chartData.getTargetName());
        }

        // change the scaling factor ?
        setUvPlotScalingFactor(MEGA_LAMBDA_SCALE);

        // computed data are valid :
        updateChart(chartData, uvMapData);

        // update the background image and legend:
        updateUVMapData(uvMapData);

        // update theme at end :
        ChartUtilities.applyCurrentTheme(this.chart);

      } finally {
        // restore chart & plot notifications:
        this.xyPlot.setNotify(true);
        this.chart.setNotify(true);
      }

      // update the status bar:
      StatusBar.showIfPrevious(MSG_COMPUTING_COVERAGE, "uv coverage done.");
    }
  }

  /**
   * Process the zoom event to refresh the model UV map according to the new coordinates
   * @param ze zoom event
   */
  @Override
  public void chartChanged(final ZoomEvent ze) {
    // check if the zoom changed :
    if (!ze.equals(this.lastZoomEvent)) {
      this.lastZoomEvent = ze;

      if (this.getChartData() != null && this.getChartData().getUVMapData() != null) {
        // Update model uv map :

        final UVMapData uvMapData = this.getChartData().getUVMapData();

        // get target from observation for consistency :
        final Target target = this.getChartData().getFirstObservation().getTarget(this.getChartData().getTargetName());

        if (target != null && target.hasModel()) {

          final Rectangle2D.Double uvRect = new Rectangle2D.Double();
          uvRect.setFrameFromDiagonal(
                  fromUVPlotScale(ze.getDomainLowerBound()), fromUVPlotScale(ze.getRangeLowerBound()),
                  fromUVPlotScale(ze.getDomainUpperBound()), fromUVPlotScale(ze.getRangeUpperBound()));

          // compute an approximated uv map from the reference UV Map :
          if (computeSubUVMap(uvMapData, uvRect)) {

            // visibility reference extrema :
            final Float refMin = uvMapData.getMin();
            final Float refMax = uvMapData.getMax();

            // model image options :
            final ImageMode imageMode = uvMapData.getImageMode();
            final int imageSize = uvMapData.getImageSize();
            final IndexColorModel colorModel = uvMapData.getColorModel();
            final ColorScale colorScale = uvMapData.getColorScale();
            final VisNoiseService noiseService = uvMapData.getNoiseService();

            // update the status bar :
            StatusBar.show(MSG_COMPUTING_MAP);

            // Compute a correct uv map for analytical models ONLY:
            if (target.hasAnalyticalModel()) {

              // Create uv map task worker :
              // Cancel other tasks and execute this new task :

              new UVMapSwingWorker(this, target.getModels(), uvRect, refMin, refMax,
                      imageMode, imageSize, colorModel, colorScale, noiseService).executeTask();

            } else if (false) {
              // TODO: support custom rectangular area ie extract area in centered FFT
              // User model
              new UVMapSwingWorker(this, target.getUserModel().getFitsImage(), uvRect, refMin, refMax,
                      imageMode, imageSize, colorModel, colorScale, noiseService).executeTask();
            }
          } else {
            // cancel anyway currently running UVMapSwingWorker:
            if (TaskSwingWorkerExecutor.cancelTask(AsproTaskRegistry.TASK_UV_MAP)) {
              // update the status bar:
              StatusBar.showIfPrevious(MSG_COMPUTING_MAP, "uv map cancelled.");
            }
          }
        }
      }
    }
  }

  /**
   * TaskSwingWorker child class to compute UV Map data and refresh the uv coverage plot
   */
  private final static class UVMapSwingWorker extends TaskSwingWorker<UVMapData> {

    /* members */
    /** uv panel used for refreshUI callback */
    private final UVCoveragePanel uvPanel;
    /** list of models to use */
    private final List<Model> models;
    /** user-defined  model */
    private final FitsImage fitsImage;
    /** UV frequency area in rad-1 */
    private final Rectangle2D.Double uvRect;
    /** minimum reference double value used only for sub images */
    private final Float refMin;
    /** maximum reference double value used only for sub images */
    private final Float refMax;
    /** image mode (amplitude or phase) */
    private final ImageMode imageMode;
    /** image size */
    private final int imageSize;
    /** image color model */
    private final IndexColorModel colorModel;
    /** color scaling method */
    private final ColorScale colorScale;
    /** optional Complex visibility Noise Service ready to use to compute noise on model images */
    private final VisNoiseService noiseService;

    /**
     * Hidden constructor
     *
     * @param uvPanel observability panel
     * @param models list of models to use
     * @param uvRect expected UV frequency area in rad-1
     * @param refMin minimum reference double value used only for sub images
     * @param refMax maximum reference double value used only for sub images
     * @param imageMode image mode (amplitude or phase)
     * @param imageSize expected number of pixels for both width and height of the generated image
     * @param colorModel color model to use
     * @param colorScale color scaling method
     * @param noiseService optional Complex visibility Noise Service ready to use to compute noise on model images
     */
    private UVMapSwingWorker(final UVCoveragePanel uvPanel, final List<Model> models,
            final Rectangle2D.Double uvRect,
            final Float refMin, final Float refMax,
            final ImageMode imageMode, final int imageSize,
            final IndexColorModel colorModel, final ColorScale colorScale,
            final VisNoiseService noiseService) {
      super(AsproTaskRegistry.TASK_UV_MAP);
      this.uvPanel = uvPanel;
      this.models = models;
      this.fitsImage = null;
      this.uvRect = uvRect;
      this.refMin = refMin;
      this.refMax = refMax;
      this.imageMode = imageMode;
      this.imageSize = imageSize;
      this.colorModel = colorModel;
      this.colorScale = colorScale;
      this.noiseService = noiseService;
    }

    /**
     * Hidden constructor
     *
     * @param uvPanel observability panel
     * @param fitsImage fits image to compute user-defined model
     * @param uvRect expected UV frequency area in rad-1
     * @param refMin minimum reference double value used only for sub images
     * @param refMax maximum reference double value used only for sub images
     * @param imageMode image mode (amplitude or phase)
     * @param imageSize expected number of pixels for both width and height of the generated image
     * @param colorModel color model to use
     * @param colorScale color scaling method
     * @param noiseService optional Complex visibility Noise Service ready to use to compute noise on model images
     */
    private UVMapSwingWorker(final UVCoveragePanel uvPanel, final FitsImage fitsImage,
            final Rectangle2D.Double uvRect,
            final Float refMin, final Float refMax,
            final ImageMode imageMode, final int imageSize,
            final IndexColorModel colorModel, final ColorScale colorScale,
            final VisNoiseService noiseService) {
      super(AsproTaskRegistry.TASK_UV_MAP);
      this.uvPanel = uvPanel;
      this.models = null;
      this.fitsImage = fitsImage;
      this.uvRect = uvRect;
      this.refMin = refMin;
      this.refMax = refMax;
      this.imageMode = imageMode;
      this.imageSize = imageSize;
      this.colorModel = colorModel;
      this.colorScale = colorScale;
      this.noiseService = noiseService;
    }

    /**
     * Compute the UV Map data in background
     * This code is executed by a Worker thread (Not Swing EDT)
     * @return UV Map data
     */
    @Override
    public UVMapData computeInBackground() {
      logger.debug("Computing model image ...");
      try {
        if (this.models != null) {
          // compute anyway the uv map data :
          return ModelUVMapService.computeUVMap(this.models, this.uvRect, this.refMin, this.refMax, null,
                  this.imageMode, this.imageSize, this.colorModel, this.colorScale, this.noiseService);

        } else if (fitsImage != null) {
          // User Model:

          // Compute Target Model for the UV coverage limits ONCE :
          // Note: throws IllegalArgumentException if the fits image is invalid:
          return UserModelService.computeUVMap(this.fitsImage, this.uvRect, this.imageMode,
                  this.imageSize, this.colorModel, this.colorScale, noiseService, this.refMin, this.refMax, null);
        }

      } catch (InterruptedJobException ije) {
        logger.debug("Computing model image interrupted: ", ije);
      }
      return null;
    }

    /**
     * Refresh the plot using the computed UV Map data.
     * This code is executed by the Swing Event Dispatcher thread (EDT)
     * @param uvMapData computed UV Map data
     */
    @Override
    public void refreshUI(final UVMapData uvMapData) {
      // delegates to uv coverage panel :
      this.uvPanel.updateUVMap(uvMapData.getUvMap());

      // update the status bar:
      StatusBar.showIfPrevious(MSG_COMPUTING_MAP, "uv map done.");
    }

    /**
     * Handle the execution exception that occured in the compute operation @see #computeInBackground()
     * This implementation resets the plot and opens a message dialog or the feedback report depending on the cause.
     *
     * @param ee execution exception
     */
    @Override
    public void handleException(final ExecutionException ee) {
      this.uvPanel.updateUVMap(null);
      if (ee.getCause() instanceof IllegalArgumentException) {
        MessagePane.showErrorMessage(ee.getCause().getMessage());
      } else {
        super.handleException(ee);
      }
    }
  }

  /**
   * Compute a sub image for the UV Map given the new uv area
   * @param uvMapData UV Map data
   * @param uvRect uv area
   * @return true if the given uvRect is smaller than uvRect of the reference image
   */
  private boolean computeSubUVMap(final UVMapData uvMapData, final Rectangle2D.Double uvRect) {
    boolean doCrop = false;

    final int imageSize = uvMapData.getUvMapSize();

    // uv area reference :
    final Rectangle2D.Double uvRectRef = uvMapData.getUvMapRect();

    if (logger.isDebugEnabled()) {
      logger.debug("uv map rect     = {}", uvRect);
      logger.debug("uv map rect REF = {}", uvRectRef);
    }

    // note : floor/ceil to be sure to have at least 1x1 pixel image
    int x = (int) Math.floor(imageSize * (uvRect.getX() - uvRectRef.getX()) / uvRectRef.getWidth());
    int y = (int) Math.floor(imageSize * (uvRect.getY() - uvRectRef.getY()) / uvRectRef.getHeight());
    int w = (int) Math.ceil(imageSize * uvRect.getWidth() / uvRectRef.getWidth());
    int h = (int) Math.ceil(imageSize * uvRect.getHeight() / uvRectRef.getHeight());

    // Note : the image is produced from an array where 0,0 corresponds to the upper left corner
    // whereas it corresponds in UV to the lower U and Upper V coordinates => inverse the V axis

    // Inverse V axis issue :
    y = imageSize - y - h;

    // check bounds:
    x = checkBounds(x, 0, imageSize - 1);
    y = checkBounds(y, 0, imageSize - 1);
    w = checkBounds(w, 1, imageSize - x);
    h = checkBounds(h, 1, imageSize - y);

    doCrop = ((x != 0) || (y != 0) || (w != imageSize) || (h != imageSize));

    if (logger.isDebugEnabled()) {
      logger.debug("sub image [{}, {} - {}, {}] - doCrop = {}", x, y, w, h, doCrop);
    }

    // crop a small sub image displayed while the correct model image is computed:

    // check reset zoom to avoid computing sub image == ref image:
    final Image subUVMap = (doCrop) ? uvMapData.getUvMap().getSubimage(x, y, w, h) : uvMapData.getUvMap();

    // TODO: adjust axis bounds to exact viewed rectangle (i.e. avoid rounding errors) !!

    // update the background image :
    updateUVMap(subUVMap);

    return doCrop;
  }

  /**
   * Return the value or the closest bound
   * @param value value to check
   * @param min minimum value
   * @param max maximum value
   * @return value or the closest bound
   */
  private static int checkBounds(final int value, final int min, final int max) {
    if (value < min) {
      return min;
    }
    if (value > max) {
      return max;
    }
    return value;
  }

  /**
   * Update the background image of the chart with the UV Map and its legend
   * @param uvMapData uv map data or null
   */
  private void updateUVMapData(final UVMapData uvMapData) {

    if (mapLegend != null) {
      this.chart.removeSubtitle(mapLegend);
    }

    if (uvMapData != null && uvMapData.getUvMap() != null) {
      final double min = uvMapData.getMin();
      final double max = uvMapData.getMax();
      final IndexColorModel colorModel = uvMapData.getColorModel();
      final ColorScale colorScale = uvMapData.getColorScale();

      final NumberAxis uvMapAxis;
      if (colorScale == ColorScale.LINEAR) {
        uvMapAxis = new NumberAxis();
        mapLegend = new PaintScaleLegend(new ColorModelPaintScale(min, max, colorModel, colorScale), uvMapAxis);
      } else {
        uvMapAxis = new LogarithmicAxis(null);
        ((LogarithmicAxis) uvMapAxis).setExpTickLabelsFlag(true);
        mapLegend = new PaintLogScaleLegend(new ColorModelPaintScale(min, max, colorModel, colorScale), uvMapAxis);
      }

      uvMapAxis.setTickLabelFont(ChartUtils.DEFAULT_FONT);
      uvMapAxis.setAxisLinePaint(Color.BLACK);
      uvMapAxis.setTickMarkPaint(Color.BLACK);

      mapLegend.setPosition(RectangleEdge.LEFT);
      mapLegend.setStripWidth(15d);
      mapLegend.setStripOutlinePaint(Color.BLACK);
      mapLegend.setStripOutlineVisible(true);
      mapLegend.setSubdivisionCount(colorModel.getMapSize());
      mapLegend.setAxisLocation(AxisLocation.BOTTOM_OR_LEFT);
      mapLegend.setFrame(new BlockBorder(Color.BLACK));
      mapLegend.setMargin(1d, 1d, 1d, 1d);
      mapLegend.setPadding(10d, 10d, 10d, 10d);

      this.chart.addSubtitle(mapLegend);

      updateUVMap(uvMapData.getUvMap());

    } else {
      updateUVMap(null);
    }
  }

  /**
   * Update the background image of the chart with the UV Map
   * @param uvMap image or null
   */
  private void updateUVMap(final Image uvMap) {
    if (uvMap != null) {
      this.xyPlot.setBackgroundPaint(null);
      this.xyPlot.setBackgroundImage(uvMap);
    } else {
      this.xyPlot.setBackgroundPaint(Color.lightGray);
      this.xyPlot.setBackgroundImage(null);
    }
  }

  /**
   * Update the datasets
   * @param chartData chart data
   * @param uvMapData optional uvMap data
   */
  private void updateChart(final ObservationCollectionUVData chartData, final UVMapData uvMapData) {

    // renderer :
    final AbstractRenderer renderer = (AbstractRenderer) this.xyPlot.getRenderer();

    // reset colors :
    renderer.clearSeriesPaints(false);
    // side effect with chart theme :
    renderer.setAutoPopulateSeriesPaint(false);

    // Create dataset with UV coverage data :
    final XYSeriesCollection dataset = prepareDataset(chartData);
    this.updateUVTracks(dataset, chartData);
    this.updateUVTracksRiseSet(dataset, chartData);

    // define bounds to the uv maximum value (before setDataset which calls restoreAxisBounds()) :
    final UVCoverageData uvData = chartData.getFirstUVData();

    final double uvMaxInLambda = (uvMapData != null) ? -uvMapData.getUvMapRect().getMinX() : uvData.getUvMax();
    final double boxSize = toUVPlotScale(uvMaxInLambda);
    this.xyPlot.defineBounds(boxSize);

    // uv in meters (not corrected by uv map area):
    this.xyPlot.defineAxisBounds(1, uvData.getUvMaxInMeter());

    // set the main data set :
    this.xyPlot.setDataset(dataset);
  }

  /**
   * Prepare the dataset i.e. create all XYSeries once for all
   * @param chartData chart data
   * @return dataset
   */
  private XYSeriesCollection prepareDataset(final ObservationCollectionUVData chartData) {
    final ColorPalette palette = ColorPalette.getDefaultColorPalette();

    // renderer :
    final XYLineAndShapeRenderer renderer = (XYLineAndShapeRenderer) this.xyPlot.getRenderer();

    final XYSeriesCollection dataset = new XYSeriesCollection();

    XYSeries xySeries = null;
    int n;

    final boolean single = chartData.isSingle();

    // Iterate over UV Coverage data (multi conf) :
    for (int c = 0, len = chartData.size(); c < len; c++) {
      final UVCoverageData uvData = chartData.getUVDataList().get(c);

      if (!single) {
        // 1 color per configuration (i.e. per XYSeries) :
        xySeries = new XYSeries(chartData.getConfigurationNames().get(c), false);
        xySeries.setNotify(false);

        dataset.addSeries(xySeries);

        n = dataset.getSeriesCount() - 1;
        renderer.setSeriesPaint(n, palette.getColor(n), false);

      } else {
        for (BaseLine bl : uvData.getBaseLines()) {

          // 1 color per base line (i.e. per XYSeries) :
          xySeries = new XYSeries(bl.getName(), false);
          xySeries.setNotify(false);

          dataset.addSeries(xySeries);

          n = dataset.getSeriesCount() - 1;
          renderer.setSeriesPaint(n, palette.getColor(n), false);
        }
      } // BL
    }

    return dataset;
  }

  /**
   * Update the dataset with UV observable tracks
   * @param dataset dataset to use
   * @param chartData chart data
   */
  private void updateUVTracks(final XYSeriesCollection dataset, final ObservationCollectionUVData chartData) {

    List<UVRangeBaseLineData> targetUVObservability;

    XYSeries xySeries = null;

    double[] uWMin;
    double[] vWMin;
    double[] uWMax;
    double[] vWMax;
    double x1, y1, x2, y2;

    final boolean single = chartData.isSingle();

    // Iterate over UV Coverage data (multi conf) :
    for (int c = 0, len = chartData.size(); c < len; c++) {
      final UVCoverageData uvData = chartData.getUVDataList().get(c);

      // process observable uv ranges :
      targetUVObservability = uvData.getTargetUVObservability();

      if (targetUVObservability != null) {
        // target is observable :

        if (!single) {
          // 1 color per configuration (i.e. per XYSeries) :
          xySeries = dataset.getSeries(chartData.getConfigurationNames().get(c));
        }

        for (UVRangeBaseLineData uvBL : targetUVObservability) {

          if (single) {
            // 1 color per base line (i.e. per XYSeries) :
            xySeries = dataset.getSeries(uvBL.getName());
          }

          uWMin = uvBL.getUWMin();
          vWMin = uvBL.getVWMin();
          uWMax = uvBL.getUWMax();
          vWMax = uvBL.getVWMax();

          for (int i = 0, size = uvBL.getNPoints(); i < size; i++) {
            x1 = toUVPlotScale(uWMax[i]);
            y1 = toUVPlotScale(vWMax[i]);

            x2 = toUVPlotScale(uWMin[i]);
            y2 = toUVPlotScale(vWMin[i]);

            // first segment :
            xySeries.add(x1, y1, false);
            xySeries.add(x2, y2, false);

            // add an invalid point to break the line between the 2 segments :
            xySeries.add(Double.NaN, Double.NaN, false);

            // second symetric segment :
            xySeries.add(-x1, -y1, false);
            xySeries.add(-x2, -y2, false);

            // add an invalid point to break the line between the 2 segments :
            xySeries.add(Double.NaN, Double.NaN, false);

          } // points
        } // BL
      }
    }
  }

  /**
   * Update the dataset with UV rise/set tracks
   * @param dataset dataset to use
   * @param chartData chart data
   */
  private void updateUVTracksRiseSet(final XYSeriesCollection dataset, final ObservationCollectionUVData chartData) {

    List<UVBaseLineData> targetUVRiseSet;

    XYSeries xySeries = null;

    double[] u;
    double[] v;
    double x, y;

    final boolean single = chartData.isSingle();

    // Iterate over UV Coverage data (multi conf) :
    for (int c = 0, len = chartData.size(); c < len; c++) {
      final UVCoverageData uvData = chartData.getUVDataList().get(c);

      // process uv rise/set :
      targetUVRiseSet = uvData.getTargetUVRiseSet();

      if (targetUVRiseSet != null) {
        // target is visible :

        if (!single) {
          // 1 color per configuration (i.e. per XYSeries) :
          xySeries = dataset.getSeries(chartData.getConfigurationNames().get(c));
        }

        for (UVBaseLineData uvBL : targetUVRiseSet) {

          if (single) {
            // 1 color per base line (i.e. per XYSeries) :
            xySeries = dataset.getSeries(uvBL.getName());
          }

          u = uvBL.getU();
          v = uvBL.getV();

          // first ellipse line :
          for (int i = 0, size = uvBL.getNPoints(); i < size; i++) {
            x = toUVPlotScale(u[i]);
            y = toUVPlotScale(v[i]);

            xySeries.add(x, y, false);
          } // points

          // add an invalid point to break the line between the 2 segments :
          xySeries.add(Double.NaN, Double.NaN, false);

          // second symetric ellipse line :
          for (int i = 0, size = uvBL.getNPoints(); i < size; i++) {
            x = toUVPlotScale(-u[i]);
            y = toUVPlotScale(-v[i]);

            xySeries.add(x, y, false);
          } // points

          // add an invalid point to break the line between the 2 segments :
          xySeries.add(Double.NaN, Double.NaN, false);

        } // BL
      }
    }
  }

  /**
   * TaskSwingWorker child class to compute OIFits and send OIFits done events
   *
   * TODO: create OIFits creator services automatically using UVCoverageData + observation directly ...
   * + use a single NoiseService instance ...
   */
  private final static class OIFitsSwingWorker extends TaskSwingWorker<List<OIFitsFile>> {

    /* members */
    /** uv coverage data collection */
    private final ObservationCollectionUVData uvDataCollection;
    /** list of oiFitsCreator services to execute */
    private final List<OIFitsCreatorService> oiFitsCreatorList;

    /**
     * Hidden constructor
     *
     * @param uvDataCollection uv coverage data collection
     * @param oiFitsCreatorList list of oiFitsCreator services to execute
     */
    private OIFitsSwingWorker(final ObservationCollectionUVData uvDataCollection, final List<OIFitsCreatorService> oiFitsCreatorList) {
      super(AsproTaskRegistry.TASK_OIFITS);
      this.uvDataCollection = uvDataCollection;
      this.oiFitsCreatorList = oiFitsCreatorList;
    }

    /**
     * Compute the OIFits structures in background
     * This code is executed by a Worker thread (Not Swing EDT)
     * @return OIFitsFiles OIFitsFile results
     */
    @Override
    public List<OIFitsFile> computeInBackground() {
      logger.debug("Computing oifits data ...");

      List<OIFitsFile> result = null;
      try {
        // Start the computations :
        final long start = System.nanoTime();

        final List<OIFitsFile> oiFitsList = new ArrayList<OIFitsFile>(this.oiFitsCreatorList.size());

        for (OIFitsCreatorService oiFitsCreator : this.oiFitsCreatorList) {
          // Create the OIFits structure :
          oiFitsList.add(oiFitsCreator.createOIFits());
        }

        result = oiFitsList;

        if (logger.isInfoEnabled()) {
          logger.info("compute[OIFitsFiles]: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        }

      } catch (InterruptedJobException ije) {
        logger.debug("Computing oifits data interrupted: ", ije);
      }
      return result;
    }

    /**
     * Dispatch the computed OIFits to the current observation (which triggers events)
     * This code is executed by the Swing Event Dispatcher thread (EDT)
     * @param oiFitsList OIFitsFile results
     */
    @Override
    public void refreshUI(final List<OIFitsFile> oiFitsList) {

      // update OIFits done flag on uv data collection:
      this.uvDataCollection.setOIFitsDone(true);

      // update the OIFits structure in the current observation :
      ObservationManager.getInstance().setOIFitsList(oiFitsList);

      // update the status bar:
      StatusBar.showIfPrevious(MSG_COMPUTING_OIFITS, "OIFits done.");
    }
  }
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBox jCheckBoxAddNoise;
    private javax.swing.JCheckBox jCheckBoxDoOIFits;
    private javax.swing.JCheckBox jCheckBoxModelImage;
    private javax.swing.JCheckBox jCheckBoxPlotUVSupport;
    private javax.swing.JComboBox jComboBoxAtmQual;
    private javax.swing.JComboBox jComboBoxFTMode;
    private javax.swing.JComboBox jComboBoxImageMode;
    private javax.swing.JComboBox jComboBoxInstrumentMode;
    private javax.swing.JFormattedTextField jFieldHAMax;
    private javax.swing.JFormattedTextField jFieldHAMin;
    private javax.swing.JFormattedTextField jFieldObsDuration;
    private javax.swing.JFormattedTextField jFieldSamplingPeriod;
    private javax.swing.JFormattedTextField jFieldUVMax;
    private javax.swing.JLabel jLabelAtmQual;
    private javax.swing.JLabel jLabelFTMode;
    private javax.swing.JLabel jLabelHAMax;
    private javax.swing.JLabel jLabelHAMin;
    private javax.swing.JLabel jLabelImageMode;
    private javax.swing.JLabel jLabelInstrumentMode;
    private javax.swing.JLabel jLabelObsDuration;
    private javax.swing.JLabel jLabelSamplingPeriod;
    private javax.swing.JLabel jLabelUVMax;
    private javax.swing.JPanel jPanelLeft;
    private javax.swing.JPanel jPanelSpacer;
    private javax.swing.JScrollPane jScrollPaneForm;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JSeparator jSeparator3;
    private javax.swing.JSlider jSliderHAMax;
    private javax.swing.JSlider jSliderHAMin;
    private javax.swing.JSlider jSliderUVMax;
    private javax.swing.JSplitPane jSplitPane;
    private javax.swing.JLabel jTargetHAMax;
    private javax.swing.JLabel jTargetHAMin;
    // End of variables declaration//GEN-END:variables
  /** drawing started time value */
  private long chartDrawStartTime = 0l;

  /**
   * Handle the chart progress event to log the chart rendering delay
   * @param event chart progress event
   */
  @Override
  public void chartProgress(final ChartProgressEvent event) {
    if (logger.isDebugEnabled()) {
      switch (event.getType()) {
        case ChartProgressEvent.DRAWING_STARTED:
          this.chartDrawStartTime = System.nanoTime();
          break;
        case ChartProgressEvent.DRAWING_FINISHED:
          logger.debug("Drawing chart time = {} ms.", 1e-6d * (System.nanoTime() - this.chartDrawStartTime));
          this.chartDrawStartTime = 0l;
          break;
        default:
      }
    }

    // Perform custom operations before/after chart rendering:
    // move JMMC annotation:
    this.aJMMC.setX(this.xyPlot.getDomainAxis().getUpperBound());
    this.aJMMC.setY(this.xyPlot.getRangeAxis().getLowerBound());
  }

  /**
   * Format the given double value using the text field formatter
   * @param field formatted text field
   * @param value value to use
   * @return formatted string
   */
  private static String format(final JFormattedTextField field, final Double value) {
    String res = "";
    try {
      res = field.getFormatter().valueToString(value);
    } catch (ParseException pe) {
      logger.error("parsing exception", pe);
    }
    return res;
  }

  /**
   * Enable / Disable the automatic update of the observation when any swing component changes.
   * Return its previous value.
   *
   * Typical use is as following :
   * // first disable the automatic update observation :
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
   * Enable / Disable the automatic refresh of the plot when any swing component changes.
   * Return its previous value.
   *
   * Typical use is as following :
   * // disable the automatic refresh :
   * final boolean prevAutoRefresh = this.setAutoRefresh(false);
   * try {
   *   // operations ...
   *
   * } finally {
   *   // restore the automatic refresh :
   *   this.setAutoRefresh(prevAutoRefresh);
   * }
   *
   * @param value new value
   * @return previous value
   */
  private boolean setAutoRefresh(final boolean value) {
    // first backup the state of the automatic update observation :
    final boolean previous = this.doAutoRefresh;

    // then change its state :
    this.doAutoRefresh = value;

    // return previous state :
    return previous;
  }

  /**
   * Define the uv scaling factor
   * @param uvPlotScalingFactor new value
   */
  private void setUvPlotScalingFactor(final double uvPlotScalingFactor) {
    this.uvPlotScalingFactor = uvPlotScalingFactor;
  }

  /**
   * Convert the given value (u or v) to the plot scale
   * @param value u or v coordinate in rad-1
   * @return u or v coordinate in the plot unit
   */
  private double toUVPlotScale(final double value) {
    return uvPlotScalingFactor * value;
  }

  /**
   * Convert the given plot value (u or v) to the standard unit (rad-1)
   * @param value u or v coordinate in the plot unit
   * @return u or v coordinate in rad-1
   */
  private double fromUVPlotScale(final double value) {
    return value / uvPlotScalingFactor;
  }

  /**
   * Return the currently selected target name
   * @return target name
   */
  private String getSelectedTargetName() {
    final Target target = getSelectedTarget();
    if (target != null) {
      return target.getName();
    }
    return null;
  }

  /**
   * Return the currently selected target
   * @return target
   */
  public Target getSelectedTarget() {
    return this.currentTarget;
  }

  /**
   * Define the currently selected target
   * @param target target to use
   */
  private void setSelectedTarget(final Target target) {
    this.currentTarget = target;
  }
}
