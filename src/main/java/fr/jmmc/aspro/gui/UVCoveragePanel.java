/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.Aspro2;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.gui.action.ExportOBVLTIAction;
import fr.jmmc.aspro.gui.action.ExportOBVegaAction;
import fr.jmmc.aspro.gui.action.ExportOBXmlAction;
import fr.jmmc.aspro.gui.chart.AsproChartUtils;
import fr.jmmc.aspro.gui.chart.EnhancedXYLineAnnotation;
import fr.jmmc.aspro.gui.chart.ExtendedXYTextAnnotation;
import fr.jmmc.aspro.gui.task.AsproTaskRegistry;
import fr.jmmc.aspro.gui.task.ObservationCollectionTaskSwingWorker;
import fr.jmmc.aspro.gui.util.AnimatorPanel;
import fr.jmmc.aspro.gui.util.UserModelAnimator;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.OIFitsData;
import fr.jmmc.aspro.model.ObservationCollectionUVData;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.event.ObservabilityEvent;
import fr.jmmc.aspro.model.event.ObservationEvent;
import fr.jmmc.aspro.model.event.ObservationListener;
import fr.jmmc.aspro.model.event.TargetSelectionEvent;
import fr.jmmc.aspro.model.event.UpdateObservationEvent;
import fr.jmmc.aspro.model.event.UpdateObservationEvent.ChangeType;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.observability.TargetPointInfo;
import fr.jmmc.aspro.model.oi.AtmosphereQuality;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.ObservationCollection;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.model.oi.UserModel;
import fr.jmmc.aspro.model.util.AtmosphereQualityUtils;
import fr.jmmc.aspro.model.uvcoverage.UVBaseLineData;
import fr.jmmc.aspro.model.uvcoverage.UVCoverageData;
import fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData;
import fr.jmmc.aspro.ob.ExportOBMode;
import fr.jmmc.aspro.service.NoiseService;
import fr.jmmc.aspro.service.OIFitsCreatorService;
import fr.jmmc.aspro.service.UVCoverageService;
import fr.jmmc.aspro.service.UserModelData;
import fr.jmmc.aspro.service.UserModelService;
import fr.jmmc.aspro.service.UserModelService.MathMode;
import fr.jmmc.jmal.image.ColorModels;
import fr.jmmc.jmal.image.ColorScale;
import fr.jmmc.jmal.image.FloatArrayCache;
import fr.jmmc.jmal.image.ImageUtils;
import fr.jmmc.jmal.model.ImageMode;
import fr.jmmc.jmal.model.ModelUVMapService;
import fr.jmmc.jmal.model.UVMapData;
import fr.jmmc.jmal.model.VisNoiseService;
import fr.jmmc.jmal.model.targetmodel.Model;
import fr.jmmc.jmal.util.MathUtils;
import fr.jmmc.jmcs.gui.component.Disposable;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.gui.task.TaskSwingWorker;
import fr.jmmc.jmcs.gui.task.TaskSwingWorkerExecutor;
import fr.jmmc.jmcs.gui.util.FieldSliderAdapter;
import fr.jmmc.jmcs.util.FormatterUtils;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.ObjectUtils;
import fr.jmmc.jmcs.util.SpecialChars;
import fr.jmmc.jmcs.util.StringUtils;
import fr.jmmc.jmcs.util.concurrent.InterruptedJobException;
import fr.jmmc.oiexplorer.core.export.DocumentExportable;
import fr.jmmc.oiexplorer.core.export.DocumentOptions;
import fr.jmmc.oiexplorer.core.gui.action.ExportDocumentAction;
import fr.jmmc.oiexplorer.core.gui.chart.BoundedNumberAxis;
import fr.jmmc.oiexplorer.core.gui.chart.ChartUtils;
import fr.jmmc.oiexplorer.core.gui.chart.ColorModelPaintScale;
import fr.jmmc.oiexplorer.core.gui.chart.FastXYLineAndShapeRenderer;
import fr.jmmc.oiexplorer.core.gui.chart.PaintLogScaleLegend;
import fr.jmmc.oiexplorer.core.gui.chart.SquareChartPanel;
import fr.jmmc.oiexplorer.core.gui.chart.SquareXYPlot;
import fr.jmmc.oiexplorer.core.gui.chart.ZoomEvent;
import fr.jmmc.oiexplorer.core.gui.chart.ZoomEventListener;
import fr.jmmc.oiexplorer.core.gui.chart.dataset.SharedSeriesAttributes;
import fr.jmmc.oiexplorer.core.util.Constants;
import static fr.jmmc.oiexplorer.core.util.FitsImageUtils.checkBounds;
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
import java.awt.image.BufferedImage;
import java.awt.image.IndexColorModel;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;
import java.util.Vector;
import java.util.concurrent.ExecutionException;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JFormattedTextField;
import javax.swing.Timer;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import net.jafama.FastMath;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.axis.AxisLocation;
import org.jfree.chart.axis.LogarithmicAxis;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.block.BlockBorder;
import org.jfree.chart.event.ChartProgressEvent;
import org.jfree.chart.event.ChartProgressListener;
import org.jfree.chart.labels.XYToolTipGenerator;
import org.jfree.chart.renderer.AbstractRenderer;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.chart.title.PaintScaleLegend;
import org.jfree.chart.ui.Drawable;
import org.jfree.chart.ui.Layer;
import org.jfree.chart.ui.RectangleEdge;
import org.jfree.chart.ui.TextAnchor;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This panel presents the UV coverage plot with its parameters (target, instrument mode ...)
 * @author bourgesl
 */
public final class UVCoveragePanel extends javax.swing.JPanel implements XYToolTipGenerator, ChartProgressListener, ZoomEventListener,
                                                                         ActionListener, ChangeListener, ObservationListener, Observer,
                                                                         UserModelAnimator.UserModelAnimatorListener,
                                                                         DocumentExportable, Disposable {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1L;
    /** Class _logger */
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
    /** shadow color */
    private final static Color SHADOW_COLOR = new Color(0, 0, 0, 128);
    /** dataset index for UV points */
    private final static int DATASET_UV_POINTS = 0;
    /** dataset index for UV points shadows */
    private final static int DATASET_UV_POINTS_SHADOW = 1;
    /** dataset index for UV tracks */
    private final static int DATASET_UV_TRACKS = 2;
    /** dataset index for UV tracks shadows */
    private final static int DATASET_UV_TRACKS_SHADOW = 3;
    /** observation manager */
    private final static ObservationManager om = ObservationManager.getInstance();
    /** user model animator singleton */
    private final static UserModelAnimator animator = UserModelAnimator.getInstance();
    /** default timeline refresh period = 1 minutes */
    private static final int REFRESH_PERIOD = 60 * 1000;
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
    PaintScaleLegend mapLegend = null;
    /** uv coordinates scaling factor */
    private double uvPlotScalingFactor = MEGA_LAMBDA_SCALE;
    /** current image index (user model only) */
    private int imageIndex = -1;
    /** precomputed tooltips for uv observable ranges */
    private final Map<String, Map<Integer, String>> seriesTooltips = new HashMap<String, Map<Integer, String>>(32);
    /** tooltip buffer */
    private final StringBuffer sbToolTip = new StringBuffer(200);
    /** 24h date formatter like in france */
    private final DateFormat timeFormatter = DateFormat.getTimeInstance(DateFormat.SHORT, Locale.FRANCE);
    /** double formatter for HA */
    private final NumberFormat df1 = new DecimalFormat("0.0");
    /** timeline refresh Swing timer */
    private final Timer timerTimeRefresh;
    /** flag to indicate that the plot is rendered for PDF output */
    private boolean renderingPDF = false;

    /* cached data */
    /** current interferometer configuration name to track changes */
    private String interferometerConfigurationCacheKey = null;
    /** current instrument name to track changes */
    private String instrumentCacheKey = null;
    /** selected target name  */
    private String selectedTargetName = null;
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

        // Create the timeline refresh timer:
        this.timerTimeRefresh = new Timer(REFRESH_PERIOD, new ActionListener() {
            /**
             * Invoked when the timer action occurs.
             */
            @Override
            public void actionPerformed(final ActionEvent ae) {
                updateTimeAnnotations();
            }
        });
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
        jSeparator2 = new javax.swing.JSeparator();
        jCheckBoxModelImage = new javax.swing.JCheckBox();
        jLabelImageMode = new javax.swing.JLabel();
        jComboBoxImageMode = new javax.swing.JComboBox();
        animatorPanel = new AnimatorPanel(this, false);
        jSeparator1 = new javax.swing.JSeparator();
        jCheckBoxDoOIFits = new javax.swing.JCheckBox();
        jPanelSpacer = new javax.swing.JPanel();
        jCheckBoxAddNoise = new javax.swing.JCheckBox();
        jCheckBoxUseBias = new javax.swing.JCheckBox();

        setLayout(new java.awt.BorderLayout());

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
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
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
        gridBagConstraints.insets = new java.awt.Insets(1, 1, 0, 1);
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

        jSliderUVMax.setMajorTickSpacing(100);
        jSliderUVMax.setMaximum(1000);
        jSliderUVMax.setPaintTicks(true);
        jSliderUVMax.setValue(500);
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
        jLabelSamplingPeriod.setToolTipText("One set of calibrated visibilities in the u-v plane is taken at this interval (minutes)");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 13;
        gridBagConstraints.gridwidth = 2;
        jPanelLeft.add(jLabelSamplingPeriod, gridBagConstraints);

        jFieldSamplingPeriod.setColumns(5);
        jFieldSamplingPeriod.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0"))));
        jFieldSamplingPeriod.setMinimumSize(new java.awt.Dimension(40, 20));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 14;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
        jPanelLeft.add(jFieldSamplingPeriod, gridBagConstraints);

        jLabelObsDuration.setText("Total Integration time (s)");
        jLabelObsDuration.setToolTipText("<html><b>Time REALLY spent on source</b>, in seconds, per calibrated point\n<br>Default values corresponds to the typical time according to the instrument (1 OB)\n</html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 15;
        gridBagConstraints.gridwidth = 2;
        jPanelLeft.add(jLabelObsDuration, gridBagConstraints);

        jFieldObsDuration.setColumns(5);
        jFieldObsDuration.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0"))));
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
        gridBagConstraints.gridy = 22;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weighty = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelLeft.add(jSeparator2, gridBagConstraints);

        jCheckBoxModelImage.setSelected(true);
        jCheckBoxModelImage.setText("<html>Underplot a model image</html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 23;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanelLeft.add(jCheckBoxModelImage, gridBagConstraints);

        jLabelImageMode.setText("Plot what ...");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 24;
        jPanelLeft.add(jLabelImageMode, gridBagConstraints);

        jComboBoxImageMode.setModel(new DefaultComboBoxModel(ImageMode.values()));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 24;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(1, 1, 0, 1);
        jPanelLeft.add(jComboBoxImageMode, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 25;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.insets = new java.awt.Insets(1, 1, 0, 1);
        jPanelLeft.add(animatorPanel, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 26;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weighty = 0.1;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelLeft.add(jSeparator1, gridBagConstraints);

        jCheckBoxDoOIFits.setSelected(true);
        jCheckBoxDoOIFits.setText("Compute OIFits data");
        jCheckBoxDoOIFits.setName("jCheckBoxDoOIFits"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 27;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanelLeft.add(jCheckBoxDoOIFits, gridBagConstraints);

        jPanelSpacer.setMinimumSize(new java.awt.Dimension(1, 1));
        jPanelSpacer.setPreferredSize(new java.awt.Dimension(1, 1));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 30;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weighty = 0.1;
        jPanelLeft.add(jPanelSpacer, gridBagConstraints);

        jCheckBoxAddNoise.setSelected(true);
        jCheckBoxAddNoise.setText("Add error noise to data");
        jCheckBoxAddNoise.setToolTipText("add gaussian noise to simulated data according to the estimated error");
        jCheckBoxAddNoise.setName("jCheckBoxAddNoise"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 28;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanelLeft.add(jCheckBoxAddNoise, gridBagConstraints);

        jCheckBoxUseBias.setText("Use inst. & cal. error bias");
        jCheckBoxUseBias.setToolTipText("if enabled, correct error with both instrumental visibility / phase biases and calibration bias");
        jCheckBoxUseBias.setName("jCheckBoxUseBias"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 29;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanelLeft.add(jCheckBoxUseBias, gridBagConstraints);

        jScrollPaneForm.setViewportView(jPanelLeft);

        jSplitPane.setLeftComponent(jScrollPaneForm);

        add(jSplitPane, java.awt.BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents

    /**
     * Export Observing Block(s) (OB)
     * @param evt action event
     * @param mode export OB mode
     * @param xml true to use new OB output
     */
    public void performOBAction(final ActionEvent evt, final ExportOBMode mode, final boolean xml) {

        // Use main observation to check instrument :
        final ObservationSetting observation = om.getMainObservation();

        if (!observation.isSingle()) {
            MessagePane.showMessage("Aspro 2 can not generate an Observing Block when multiple configurations are selected !");
            return;
        }

        if (xml) {
            switch (mode) {
                case ALL:
                    ExportOBXmlAction.getInstance().process(observation.getTargets());
                    break;
                case SINGLE:
                    final Target target = getSelectedTarget();
                    if (target != null) {
                        ExportOBXmlAction.getInstance().process(Arrays.asList(new Target[]{target}));
                    }
                    break;
                default:
            }
        } else {
            final String insName = observation.getInstrumentConfiguration().getName();

            if (AsproConstants.INS_AMBER.equals(insName)
                    || AsproConstants.INS_MIDI.equals(insName)
                    || insName.startsWith(AsproConstants.INS_PIONIER)
                    || insName.startsWith(AsproConstants.INS_GRAVITY)) {

                switch (mode) {
                    case ALL:
                        ExportOBVLTIAction.getInstance().process(observation.getTargets());
                        break;
                    case SINGLE:
                        final Target target = getSelectedTarget();
                        if (target != null) {
                            ExportOBVLTIAction.getInstance().process(Arrays.asList(new Target[]{target}));
                        }
                        break;
                    default:
                }
            } else if (insName.startsWith(AsproConstants.INS_VEGA)) {
                ExportOBVegaAction.getInstance().process();
            } else {
                MessagePane.showMessage("Aspro 2 can not generate an Observing Block for this instrument [" + insName + "] !");
            }
        }
    }

    /**
     * Export the component as a document using the given action:
     * the component should check if there is something to export ?
     * @param action export action to perform the export action
     */
    @Override
    public void performAction(final ExportDocumentAction action) {
        action.process(this);
    }

    /**
     * Return the default file name
     * @param fileExtension  document's file extension
     * @return default file name
     */
    @Override
    public String getDefaultFileName(final String fileExtension) {
        if (this.getChartData() != null) {
            final ObservationSetting observation = this.getChartData().getFirstObservation();

            final StringBuilder sb = new StringBuilder(32);
            sb.append("UV_");
            sb.append(StringUtils.replaceNonAlphaNumericCharsByUnderscore(this.getChartData().getTargetName())).append('_');
            sb.append(observation.getInstrumentConfiguration().getName()).append('_');
            sb.append(this.getChartData().getDisplayConfigurations("_", true));
            if (observation.getWhen().isNightRestriction()) {
                sb.append('_');
                sb.append(observation.getWhen().getDate().toString());
            }
            sb.append('.').append(fileExtension);

            return sb.toString();
        }
        return null;
    }

    /**
     * Prepare the page layout before doing the export:
     * Performs layout and modifies the given options
     * @param options document options used to prepare the document
     */
    @Override
    public void prepareExport(final DocumentOptions options) {
        // Enable the PDF rendering flag:
        this.renderingPDF = true;

        // update the time annotations to disable it:
        updateTimeAnnotations();

        options.setSmallDefaults();
    }

    /**
     * Return the page to export given its page index
     * @param pageIndex page index (1..n)
     * @return Drawable array to export on this page
     */
    @Override
    public Drawable[] preparePage(final int pageIndex) {
        return new Drawable[]{this.chart};
    }

    /**
     * Callback indicating the export is done to reset the component's state
     */
    @Override
    public void postExport() {
        // Disable the PDF rendering flag:
        this.renderingPDF = false;

        // update the time annotations to enable it:
        updateTimeAnnotations();
    }

    /**
     * This method is useful to set the models and specific features of initialized swing components :
     */
    private void postInit() {

        this.chart = ChartUtils.createSquareXYLineChart("U (" + SpecialChars.UNIT_MEGA_LAMBDA + ")", "V (" + SpecialChars.UNIT_MEGA_LAMBDA + ")", true);
        this.xyPlot = (SquareXYPlot) this.chart.getPlot();

        this.xyPlot.getDomainAxis().setPositiveArrowVisible(true);
        this.xyPlot.getRangeAxis().setPositiveArrowVisible(true);

        // Use FastXYLineAndShapeRenderer to have tooltip on line segments:
        final FastXYLineAndShapeRenderer rendererPoints = new FastXYLineAndShapeRenderer(true, false); // DATASET_UV_POINTS
        // force to use the large stroke :
        rendererPoints.setAutoPopulateSeriesStroke(false);
        rendererPoints.setDefaultStroke(ChartUtils.LARGE_STROKE);
        rendererPoints.setDefaultToolTipGenerator(this);
        this.xyPlot.setRenderer(DATASET_UV_POINTS, rendererPoints);

        final XYLineAndShapeRenderer rendererPointsShadow = new XYLineAndShapeRenderer(true, false); // DATASET_UV_POINTS_SHADOW
        // force to use the very large stroke :
        rendererPointsShadow.setAutoPopulateSeriesStroke(false);
        rendererPointsShadow.setDefaultStroke(ChartUtils.VERY_LARGE_STROKE);
        this.xyPlot.setRenderer(DATASET_UV_POINTS_SHADOW, rendererPointsShadow);

        final XYLineAndShapeRenderer rendererTracks = new XYLineAndShapeRenderer(true, false); // DATASET_UV_TRACKS
        rendererTracks.setDrawSeriesLineAsPath(true);
        // force to use the large stroke :
        rendererTracks.setAutoPopulateSeriesStroke(false);
        rendererTracks.setDefaultStroke(ChartUtils.LARGE_STROKE);
        this.xyPlot.setRenderer(DATASET_UV_TRACKS, rendererTracks);

        final XYLineAndShapeRenderer rendererTracksShadow = new XYLineAndShapeRenderer(true, false); // DATASET_UV_TRACKS_SHADOW
        rendererTracksShadow.setDrawSeriesLineAsPath(true);
        // force to use the very large stroke :
        rendererTracksShadow.setAutoPopulateSeriesStroke(false);
        rendererTracksShadow.setDefaultStroke(ChartUtils.VERY_LARGE_STROKE);
        this.xyPlot.setRenderer(DATASET_UV_TRACKS_SHADOW, rendererTracksShadow);

        // Adjust background settings :
        this.xyPlot.setBackgroundImageAlpha(1.0f);

        // create new JMMC annotation (moving position):
        this.aJMMC = AsproChartUtils.createJMMCAnnotation();
        this.xyPlot.getRenderer().addAnnotation(this.aJMMC, Layer.BACKGROUND);

        // add UV axes in meters:
        final BoundedNumberAxis uAxisMeter = new BoundedNumberAxis("U (m) - North");
        uAxisMeter.setAutoRangeIncludesZero(false);
        uAxisMeter.setTickLabelFont(ChartUtils.DEFAULT_TITLE_FONT);
        uAxisMeter.setTickMarkPaint(Color.BLACK);
        this.xyPlot.setDomainAxis(1, uAxisMeter);

        final BoundedNumberAxis vAxisMeter = new BoundedNumberAxis("V (m) - East");
        vAxisMeter.setAutoRangeIncludesZero(false);
        vAxisMeter.setTickLabelFont(ChartUtils.DEFAULT_TITLE_FONT);
        vAxisMeter.setTickMarkPaint(Color.BLACK);
        this.xyPlot.setRangeAxis(1, vAxisMeter);

        // add listener :
        this.chart.addProgressListener(this);
        this.chartPanel = ChartUtils.createSquareChartPanel(this.chart, true); // show tooltips

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
                final int newValue = ((Number) jFieldSamplingPeriod.getValue()).intValue();

                if (newValue <= 0) {
                    // invalid value :
                    resetSamplingPeriod(om.getMainObservation());
                }

                if (jFieldObsDuration.getValue() != null) {
                    // Check that obs duration is less than jFieldSamplingPeriod / 2:
                    final double halfSamplingSec = ((Number) jFieldSamplingPeriod.getValue()).doubleValue() * 60d * 0.5d;
                    final double obsDurationSec = ((Number) jFieldObsDuration.getValue()).doubleValue();

                    if (obsDurationSec > halfSamplingSec) {
                        // update obs duration:
                        jFieldObsDuration.setValue(Double.valueOf(halfSamplingSec));
                    }
                }

                if (logger.isDebugEnabled()) {
                    logger.debug("samplingPeriod changed: {}", jFieldSamplingPeriod.getValue());
                }
                fireObservationUpdateEvent();
            }
        });

        // define property change listener :
        this.jFieldObsDuration.addPropertyChangeListener("value", new PropertyChangeListener() {
            @Override
            public void propertyChange(final PropertyChangeEvent evt) {
                final double newValue = ((Number) jFieldObsDuration.getValue()).doubleValue();

                if (newValue <= 0d) {
                    // invalid value :
                    resetTotalIntegrationTime(om.getMainObservation());
                }

                // TODO: check again: obs duration is less than jFieldSamplingPeriod / 2:
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

        this.jCheckBoxAddNoise.setSelected(this.myPreferences.getPreferenceAsBoolean(Preferences.OIFITS_ADD_NOISE));
        this.jCheckBoxAddNoise.addActionListener(this);

        this.jCheckBoxUseBias.addActionListener(this);

        // hide animator panel at startup:
        this.animatorPanel.setVisible(false);

        // register this instance as a Preference Observer :
        this.myPreferences.addObserver(this);
    }

    /**
     * Free any ressource or reference to this instance :
     * remove this instance form Preference Observers, UserModelAnimator and ObservationManager listeners
     */
    @Override
    public void dispose() {
        if (logger.isDebugEnabled()) {
            logger.debug("dispose: {}", ObjectUtils.getObjectInfo(this));
        }
        // disable model animation:
        animator.unregister(this);

        // unregister this instance as a Preference Observer :
        this.myPreferences.deleteObserver(this);

        // unregister the uv panel for the next event :
        om.unregister(this);
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

        // disable the automatic refresh:
        final boolean prevAutoRefresh = this.setAutoRefresh(false);
        try {
            this.jCheckBoxAddNoise.setSelected(this.myPreferences.getPreferenceAsBoolean(Preferences.OIFITS_ADD_NOISE));
        } finally {
            // restore the automatic refresh :
            this.setAutoRefresh(prevAutoRefresh);
        }
        this.refreshPlot();
    }

    /**
     * Update the information relative to the interferometer (configuration) : UV max length and fringe tracker modes
     * @param observation current observation settings
     */
    private void updateInteferometerData(final ObservationSetting observation) {
        final String intConfName = observation.getInterferometerConfiguration().getName();
        // test if the interferometer changed :
        final boolean changed = intConfName != null && !intConfName.equals(this.interferometerConfigurationCacheKey);
        if (changed) {
            this.interferometerConfigurationCacheKey = intConfName;

            final InterferometerConfiguration intConf = observation.getInterferometerConfiguration().getInterferometerConfiguration();

            // update the UV range :
            final double minBaseLine = intConf.getMinBaseLine();
            final double maxBaseLine = intConf.getMaxBaseLine();

            if (logger.isDebugEnabled()) {
                logger.debug("interferometer configuration changed: {}; baseline min= {}, max= {}",
                        intConfName, minBaseLine, maxBaseLine);
            }

            // adjust uv max range to [0.1 * minBaseLine; 4 * maxBaseLine] and
            // set value to maxBaseLine + 5% (margin):
            this.uvMaxAdapter.reset(Math.floor(0.1 * minBaseLine), Math.ceil(4.0 * maxBaseLine), 1.05 * maxBaseLine);
        }
    }

    /**
     * Refresh the information relative to the instrument : sampling time and instrument modes
     * @param observation current observation settings
     */
    private void updateInstrumentData(final ObservationSetting observation) {
        // use the real instrument name (not alias):
        final String insName = observation.getInstrumentConfiguration().getInstrumentConfiguration().getFocalInstrument().getName();
        final String insKey = observation.getInterferometerConfiguration().getName() + '@' + insName;

        // test if the instrument changed :
        final boolean changed = (insName != null) && !insKey.equals(this.instrumentCacheKey);
        if (changed) {
            this.instrumentCacheKey = insKey;

            logger.debug("instrument changed : {}", insName);

            resetSamplingPeriod(observation);

            // always update instrument modes (may depend on selected period):
            final Vector<String> v = ConfigurationManager.getInstance().getInstrumentModes(
                    observation.getInterferometerConfiguration().getName(),
                    observation.getInstrumentConfiguration().getName());
            this.jComboBoxInstrumentMode.setModel(new DefaultComboBoxModel(v));

            // try restoring the selected instrument mode :
            if (observation.getInstrumentConfiguration().getInstrumentMode() != null) {
                this.jComboBoxInstrumentMode.setSelectedItem(observation.getInstrumentConfiguration().getInstrumentMode());
            }

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

        if (logger.isDebugEnabled()) {
            logger.debug("defaultSamplingTime: {}", defaultSamplingTime);
        }
        resetTotalIntegrationTime(observation);
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
     * Reset the total integration time to the default total integration time of the selected instrument
     * @param observation observation to use
     */
    private void resetTotalIntegrationTime(final ObservationSetting observation) {
        // reset the total integration time to the default total integration time of the instrument :
        final int defaultTotalIntegrationTime = getInstrumentTotalIntegrationTime(observation);

        this.jFieldObsDuration.setValue(Double.valueOf(defaultTotalIntegrationTime));

        if (logger.isDebugEnabled()) {
            logger.debug("defaultTotalIntegrationTime: {}", defaultTotalIntegrationTime);
        }
    }

    /**
     * Return the default total integration time of the selected instrument
     * @param observation observation to use
     * @return default total integration time
     */
    private int getInstrumentTotalIntegrationTime(final ObservationSetting observation) {
        // get the default sampling time of the instrument :
        return ConfigurationManager.getInstance().getInstrumentTotalIntegrationTime(
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
        } else if (e.getSource() == this.jCheckBoxDoOIFits) {
            if (logger.isDebugEnabled()) {
                logger.debug("do OIFits: {}", this.jCheckBoxDoOIFits.isSelected());
            }
            if (this.getChartData() != null) {
                if (this.jCheckBoxDoOIFits.isSelected()) {
                    computeOIFits(this.getChartData());
                } else // cancel anyway currently running OIFitsSwingWorker:
                {
                    if (TaskSwingWorkerExecutor.cancelTask(AsproTaskRegistry.TASK_OIFITS)) {
                        // update the status bar:
                        StatusBar.showIfPrevious(MSG_COMPUTING_OIFITS, "OIFits data cancelled.");

                        // reset the OIFits structure in the current observation - No OIFitsSwingWorker running:
                        om.setOIFitsData(null);
                    }
                }
            }
        } else if (e.getSource() == this.jCheckBoxAddNoise) {
            if (logger.isDebugEnabled()) {
                logger.debug("do noise: {}", this.jCheckBoxAddNoise.isSelected());
            }
            refreshPlot();
        } else if (e.getSource() == this.jCheckBoxUseBias) {
            if (logger.isDebugEnabled()) {
                logger.debug("use bias: {}", this.jCheckBoxUseBias.isSelected());
            }
            refreshPlot();
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
            om.fireObservationUpdate();
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
            this.interferometerConfigurationCacheKey = null;
            this.updateInteferometerData(observation);

            // refresh data related to the instrument :
            this.instrumentCacheKey = null;
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
            this.selectedTargetName = null;
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
            this.setSelectedTargetName((target != null) ? target.getName() : null);

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

            ChangeType changeType = ChangeType.UV;
            if (targetName != null) {
                logger.debug("onUpdateObservation : {}", targetName);

                // Update target HA Min/Max :
                if (om.setTargetHAMin(targetName, Double.valueOf(this.haMinAdapter.getValue()))
                        || om.setTargetHAMax(targetName, Double.valueOf(this.haMaxAdapter.getValue()))) {
                    // Special case to force computing observability (HA restriction are also used):
                    changed = true;
                    changeType = ChangeType.MAIN;
                }

                // update ft mode :
                changed |= om.setTargetFTMode(targetName, (String) this.jComboBoxFTMode.getSelectedItem());
            }

            if (changed) {
                // update change flag to make the ObservationManager fire an observation refresh event later
                event.setChanged(changeType);
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
                    // reset:
                    this.imageIndex = -1;

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

                // reset:
                this.imageIndex = -1;

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
            final boolean useInstrumentBias = this.jCheckBoxUseBias.isSelected();
            final boolean doDataNoise = this.jCheckBoxAddNoise.isSelected();
            final boolean doModelImage = this.jCheckBoxModelImage.isSelected();

            // model image options :
            final ImageMode imageMode = (ImageMode) this.jComboBoxImageMode.getSelectedItem();

            // Use model image Preferences :
            final int imageSize = this.myPreferences.getPreferenceAsInt(Preferences.MODEL_IMAGE_SIZE);
            final String modelImageLut = this.myPreferences.getPreference(Preferences.MODEL_IMAGE_LUT);
            final IndexColorModel colorModel = (imageMode != ImageMode.PHASE) ? ColorModels.getColorModel(modelImageLut)
                    : ColorModels.getCyclicColorModel(modelImageLut);
            final ColorScale colorScale = this.myPreferences.getImageColorScale();
            final boolean doImageNoise = this.myPreferences.getPreferenceAsBoolean(Preferences.MODEL_IMAGE_NOISE);

            // Use OIFits preferences:
            final int supersamplingOIFits = this.myPreferences.getPreferenceAsInt(Preferences.OIFITS_SUPER_SAMPLING);
            final MathMode mathModeOIFits = this.myPreferences.getOIFitsMathMode();

            // update the status bar :
            StatusBar.show(MSG_COMPUTING_COVERAGE);

            // Get previously computed UV Map Data (can be null) :
            final UVMapData currentUVMapData = (getChartData() != null) ? getChartData().getUVMapData() : null;

            // Create uv coverage task worker :
            // Cancel other tasks and execute this new task :
            new UVCoverageSwingWorker(this, obsCollection, this.getObservabilityData(), targetName, uvMax,
                    doUVSupport, doOIFits, useInstrumentBias, doDataNoise,
                    doModelImage, imageMode, imageSize, colorModel, colorScale, doImageNoise,
                    this.imageIndex, supersamplingOIFits, mathModeOIFits, currentUVMapData).executeTask();

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
        /** maximum U or V coordinate */
        private double uvMax;
        /** flag to compute the UV support */
        private final boolean doUVSupport;
        /** flag to compute OIFits */
        private final boolean doOIFits;
        /** true to use instrument bias; false to compute only theoretical error */
        private final boolean useInstrumentBias;
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
        /** image index used (user model only) */
        private final int imageIndex;
        /** OIFits supersampling preference */
        private final int supersamplingOIFits;
        /** OIFits MathMode preference */
        private final UserModelService.MathMode mathModeOIFits;

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
         * @param useInstrumentBias true to use instrument bias; false to compute only theoretical error
         * @param doDataNoise enable data noise
         * @param doModelImage flag to compute the model image
         * @param imageMode image mode (amplitude or phase)
         * @param imageSize expected number of pixels for both width and height of the generated image
         * @param colorModel color model to use
         * @param colorScale color scaling method
         * @param doImageNoise enable image noise
         * @param imageIndex image index used (user model only)
         * @param supersamplingOIFits OIFits supersampling preference
         * @param mathModeOIFits OIFits MathMode preference
         * @param currentUVMapData previously computed UV Map Data
         */
        private UVCoverageSwingWorker(final UVCoveragePanel uvPanel, final ObservationCollection obsCollection,
                                      final List<ObservabilityData> obsDataList, final String targetName,
                                      final double uvMax, final boolean doUVSupport, final boolean doOIFits,
                                      final boolean useInstrumentBias, final boolean doDataNoise,
                                      final boolean doModelImage, final ImageMode imageMode, final int imageSize,
                                      final IndexColorModel colorModel, final ColorScale colorScale, final boolean doImageNoise,
                                      final int imageIndex, final int supersamplingOIFits, final UserModelService.MathMode mathModeOIFits,
                                      final UVMapData currentUVMapData) {

            // get current observation version :
            super(AsproTaskRegistry.TASK_UV_COVERAGE, obsCollection);
            this.uvPanel = uvPanel;
            this.obsDataList = obsDataList;
            this.targetName = targetName;
            this.uvMax = uvMax;
            this.doUVSupport = doUVSupport;
            this.doOIFits = doOIFits;
            this.useInstrumentBias = useInstrumentBias;
            this.doDataNoise = doDataNoise;
            this.doModelImage = doModelImage;
            this.imageMode = imageMode;
            this.imageSize = imageSize;
            this.colorModel = colorModel;
            this.colorScale = colorScale;
            this.doImageNoise = doImageNoise;
            this.imageIndex = imageIndex;
            this.supersamplingOIFits = supersamplingOIFits;
            this.mathModeOIFits = mathModeOIFits;
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
                uvDataList.add(new UVCoverageService(observation, obsData, targetName, this.uvMax, this.doUVSupport,
                        this.useInstrumentBias, this.doDataNoise, this.supersamplingOIFits, this.mathModeOIFits).compute());

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

                // update uvMaxFreq according to UVCoverage Service (wavelength correction):
                final double uvMaxFreq = uvDataFirst.getUvMaxFreq();

                if (target != null && target.hasModel()) {

                    // Get the noise service if enabled:
                    // note: it depends on telescopes so it is enabled only for single configuration:
                    final NoiseService noiseService = (this.doImageNoise && uvDataCollection.isSingle()) ? uvDataFirst.getNoiseService() : null;

                    final Rectangle2D.Double uvRect = new Rectangle2D.Double();
                    uvRect.setFrameFromDiagonal(-uvMaxFreq, -uvMaxFreq, uvMaxFreq, uvMaxFreq);

                    // Fix image index:
                    final List<UserModelData> modelDataList = (target.hasAnalyticalModel()) ? null : target.getUserModel().getModelDataList();

                    // Should only show images within instrument mode's wavelength range ???
                    // ie. gray model => only first image !!
                    // TODO: be consistent with OIFits logic rules
                    final int imageIdx = (modelDataList == null) ? -1 : (this.imageIndex < 0 || this.imageIndex >= modelDataList.size()) ? 0 : this.imageIndex;

                    logger.debug("imageIdx: {}", imageIdx);

                    // get observation target version :
                    final int targetVersion = uvDataCollection.getVersion().getTargetVersion();

                    // Check if the previously computed UV Map Data is still valid :
                    if (this.currentUVMapData != null
                            && this.currentUVMapData.isValid(targetName, targetVersion, uvRect,
                                    this.imageMode, this.imageSize, this.colorModel, this.colorScale, imageIdx, noiseService)) {

                        _logger.debug("Reuse model image.");

                        // User Model:
                        if (modelDataList != null) {
                            // flag indicating not to recycle currentUVMapData.getData():
                            this.currentUVMapData.setDataReused(true);
                        }

                        // reuse computed UV Map Data :
                        uvDataCollection.setUvMapData(this.currentUVMapData);
                    } else {
                        UVMapData uvMapData = null;
                        try {
                            if (target.hasAnalyticalModel()) {
                                // Analytical model:
                                final List<Model> models = target.getModels();

                                // Check if the previously computed visiblity Data is still valid :
                                if (this.currentUVMapData != null
                                        && this.currentUVMapData.isDataValid(targetName, targetVersion, uvRect, this.imageSize, imageIdx)) {

                                    _logger.debug("Reuse model complex visibility.");

                                    // Compute only image using existing complex visibility data :
                                    uvMapData = ModelUVMapService.computeUVMap(models,
                                            uvRect, null, null, this.currentUVMapData.getData(),
                                            this.imageMode, this.imageSize, this.colorModel, this.colorScale, noiseService);

                                } else {
                                    _logger.debug("Computing model image ...");

                                    // Compute Target Model for the UV coverage limits ONCE :
                                    uvMapData = ModelUVMapService.computeUVMap(models, uvRect,
                                            this.imageMode, this.imageSize, this.colorModel, this.colorScale, noiseService);
                                }
                            } else // User Model:
                            {
                                if (modelDataList != null) {
                                    // Get preloaded and prepared fits image at given index:
                                    final FitsImage fitsImage = modelDataList.get(imageIdx).getFitsImage();

                                    if (fitsImage != null) {
                                        // Check if the previously computed visiblity Data is still valid :
                                        if (this.currentUVMapData != null
                                                && this.currentUVMapData.isDataValid(targetName, targetVersion, uvRect, this.imageSize, imageIdx)) {

                                            _logger.debug("Reuse model visiblity.");

                                            // flag indicating not to recycle currentUVMapData.getData():
                                            this.currentUVMapData.setDataReused(true);

                                            // Compute only image using existing complex visibility data :
                                            uvMapData = UserModelService.computeUVMap(fitsImage,
                                                    uvRect, this.imageMode, this.imageSize, this.colorModel, this.colorScale, noiseService,
                                                    null, null, this.currentUVMapData.getData());

                                        } else {
                                            _logger.debug("Computing model image ...");

                                            try {
                                                // Compute Target Model for the UV coverage limits ONCE :
                                                // Note: throws IllegalArgumentException if the fits image is invalid:
                                                uvMapData = UserModelService.computeUVMap(fitsImage,
                                                        uvRect, this.imageMode, this.imageSize, this.colorModel, this.colorScale, noiseService);

                                            } catch (IllegalArgumentException iae) {
                                                _logger.warn("Incorrect fits image in file [{}]", target.getUserModel().getFile(), iae);

                                                // disable model:
                                                target.getUserModel().setFileValid(false);
                                                uvMapData = null;
                                            }
                                        }
                                    }

                                    if (uvMapData != null) {
                                        // update image index and count:
                                        uvMapData.setImageIndex(imageIdx);
                                        uvMapData.setImageCount(modelDataList.size());
                                        uvMapData.setWaveLength(fitsImage.getWaveLength());
                                        uvMapData.setUserModel(target.getUserModel());
                                    }

                                }
                            }

                            if (uvMapData != null) {
                                // define target name and version :
                                uvMapData.setTargetName(targetName);
                                uvMapData.setTargetVersion(targetVersion);

                                uvDataCollection.setUvMapData(uvMapData);
                            }

                        } catch (InterruptedJobException ije) {
                            _logger.debug("Computing model image interrupted: ", ije);
                            // recycle arrays:
                            if ((uvMapData != null)
                                    && !target.hasAnalyticalModel()
                                    && ((this.currentUVMapData == null)
                                    || (this.currentUVMapData.getData() != uvMapData.getData()))) {
                                // recycle array:
                                FloatArrayCache.recycleArray(uvMapData.getData());
                            }
                            return null;
                        }
                    }
                }
            }

            // fast interrupt :
            if (Thread.currentThread().isInterrupted()) {
                return null;
            }

            // merged warning container:
            final WarningContainer mergedWarningContainer = new WarningContainer();

            // merge warning messages:
            if (uvDataCollection.isSingle()) {
                // ObservabilityService warnings:
                mergedWarningContainer.addWarningMessages(uvDataCollection.getFirstObsData().getWarningContainer());
                // UVCoverageService warnings:
                mergedWarningContainer.addWarningMessages(uvDataFirst.getWarningContainer());

            } else {
                if (uvDataCollection.getFirstObservation().getWhen().isNightRestriction()) {
                    mergedWarningContainer.addWarningMessage("Multiple configurations cannot be done in one night"
                            + " (night restrictions are only valid for "
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

            _logger.info("compute[ObservationCollectionUVData]: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

            return uvDataCollection;
        }

        /**
         * Refresh the plot using the computed UV Coverage data.
         * This code is executed by the Swing Event Dispatcher thread (EDT)
         * @param uvDataCollection computed UV Coverage data
         */
        @Override
        public void refreshUI(final ObservationCollectionUVData uvDataCollection) {

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

            } else {
                if (_logger.isDebugEnabled()) {
                    _logger.debug("refreshUI: uv version mismatch: {} :: {}", taskObsCollection.getVersion(), lastObsCollection.getVersion());
                }
                if (DEBUG_VERSIONS) {
                    _logger.warn("refreshUI: uv version mismatch: {} :: {}", taskObsCollection.getVersion(), lastObsCollection.getVersion());
                }

                // Skip = ignore these results
                // next iteration will see changes ...
                // Note: this is necessary as SharedSeriesAttributes (color) is global (consistency issue)
                return;
            }

            // Start computing OIFits ...
            final boolean resetOIFits = (doOIFits) ? !this.uvPanel.computeOIFits(uvDataCollection) : true;

            if (!doOIFits) {
                // add warning to indicate that OIFits are disabled:
                uvDataCollection.getWarningContainer().addWarningMessage("OIFits data computation is disabled");
            }

            // reset the OIFits structure in the current observation - No OIFitsSwingWorker running:
            if (resetOIFits) {
                om.setOIFitsData(null);
            }

            // Fire a warnings ready event :
            om.fireWarningsReady(uvDataCollection.getWarningContainer());

            // Refresh the GUI using coherent data :
            this.uvPanel.updatePlot(uvDataCollection);

            // anyway enable or disable timer:
            final UVMapData uvMapData = uvDataCollection.getUVMapData();

            final boolean enableAnimator = (uvMapData != null && uvMapData.getImageIndex() != -1 && uvMapData.getImageCount() > 1);

            this.uvPanel.updateAnimatorPanel(enableAnimator ? (UserModel) uvMapData.getUserModel() : null);
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
     * Define the current user model (may register/unregister the listener in user model animator)
     * @param userModel user model
     */
    private void updateAnimatorPanel(final UserModel userModel) {
        // animator is not running until animate button is clicked.
        this.animatorPanel.setVisible(userModel != null);

        // update user model in animator panel to enable/disable animator:
        this.animatorPanel.setUserModel(userModel);
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
        if (om.getOIFitsData() != null) {
            // Check if the previously computed UV Data is still valid :
            final ObservationCollectionUVData currentUVData = getChartData();

            if (currentUVData != null) {
                // note: OIFitsCreatorService parameter dependencies:
                // observation {target, instrumentMode {lambdaMin, lambdaMax, nSpectralChannels}}
                // obsData {beams, baseLines, starData, sc (DateCalc)}
                // parameter: supersamplingOIFits, doDataNoise, useInstrumentBias
                // results: computeObservableUV {HA, targetUVObservability} {obsData + observation{haMin/haMax, instrumentMode {lambdaMin, lambdaMax}}}
                // and warning container

                // check if computation needed:
                // - check observation (UV version includes main version)
                // - check obsData (main version)
                // - check supersamplingOIFits
                // - check doNoise: noiseService.isDoNoise()
                // - check useInstrumentBias: noiseService.isUseInstrumentBias()
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

            // reset datasets:
            this.xyPlot.setDataset(DATASET_UV_POINTS, null);
            this.xyPlot.setDataset(DATASET_UV_POINTS_SHADOW, null);
            this.xyPlot.setDataset(DATASET_UV_TRACKS, null);
            this.xyPlot.setDataset(DATASET_UV_TRACKS_SHADOW, null);

            // update the background image :
            this.resetUVMap();

            // update theme at end :
            org.jfree.chart.ChartUtils.applyCurrentTheme(this.chart);

            this.xyPlot.setBackgroundPaint(Color.WHITE);

            // reset cached tooltips:
            this.seriesTooltips.clear();

            // remove annotations anyway:
            this.xyPlot.clearAnnotations();

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
        if (this.chartData != null) {
            // recycle old uv map data:
            final UVMapData uvMapData = this.chartData.getUVMapData();
            if (uvMapData != null && uvMapData.getUserModel() != null) {
                boolean same = false;

                if (chartData.getUVMapData() != null) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("new uvMapData: {}", chartData.getUVMapData().hashCode());
                    }
                    if (chartData.getUVMapData().getData() != null) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("new uvMapData.data: {}", chartData.getUVMapData().getData().hashCode());
                        }

                        same = (chartData.getUVMapData().getData() == uvMapData.getData());

                        if (logger.isDebugEnabled()) {
                            logger.debug("same = {}", same);
                        }
                    }
                }
                if (logger.isDebugEnabled()) {
                    logger.debug("old uvMapData: {}", uvMapData.hashCode());
                }

                if (uvMapData.isDataReused()) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("visData in use");
                    }
                } else if (!same) {
                    // recycle arrays:
                    if (logger.isDebugEnabled()) {
                        logger.debug("old uvMapData.data: {}", uvMapData.getData().hashCode());
                    }
                    FloatArrayCache.recycleArray(uvMapData.getData());
                }
            }
        }

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

                sb.setLength(0);

                if (observation.getWhen().isNightRestriction()) {
                    // date:
                    sb.append("Day: ").append(observation.getWhen().getDate().toString()).append(" - ");
                }
                // Source only:
                sb.append("Source: ").append(chartData.getTargetName());

                if (uvMapData != null && uvMapData.getWaveLength() != null && !Double.isNaN(uvMapData.getWaveLength())) {
                    // Polychromatic user model only:
                    sb.append(" - Model ").append(SpecialChars.LAMBDA_LOWER).append(": ");
                    sb.append(NumberUtils.trimTo3Digits(uvMapData.getWaveLength() / AsproConstants.MICRO_METER)).append(' ').append(SpecialChars.UNIT_MICRO_METER);
                }
                ChartUtils.addSubtitle(this.chart, sb.toString());

                // change the scaling factor ?
                setUvPlotScalingFactor(MEGA_LAMBDA_SCALE);

                // computed data are valid :
                updateChart(chartData, uvMapData);

                // update the background image and legend:
                updateUVMapData(uvMapData);

                // update theme at end :
                org.jfree.chart.ChartUtils.applyCurrentTheme(this.chart);

                // update the time annotations:
                updateTimeAnnotations();

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
                            fromUVPlotScale(ze.getDomainLowerBound()),
                            fromUVPlotScale(ze.getRangeLowerBound()),
                            fromUVPlotScale(ze.getDomainUpperBound()),
                            fromUVPlotScale(ze.getRangeUpperBound()));

                    // compute an approximated uv map from the reference UV Map :
                    if (computeSubUVMap(uvMapData, uvRect)) {
                        {
                            // adjust axis bounds to exact viewed rectangle (i.e. avoid rounding errors)
                            final UVCoverageData uvData = this.getChartData().getFirstUVData();

                            // uv in megalambda:
                            ValueAxis axis = this.xyPlot.getDomainAxis();
                            axis.setRange(toUVPlotScale(uvRect.getMinX()), toUVPlotScale(uvRect.getMaxX()));

                            axis = this.xyPlot.getRangeAxis();
                            axis.setRange(toUVPlotScale(uvRect.getMinY()), toUVPlotScale(uvRect.getMaxY()));

                            // uv in meters (megalambda to meter conversion):
                            axis = this.xyPlot.getDomainAxis(1);
                            axis.setRange(uvRect.getMinX() * uvData.getLambda(), uvRect.getMaxX() * uvData.getLambda());

                            axis = this.xyPlot.getRangeAxis(1);
                            axis.setRange(uvRect.getMinY() * uvData.getLambda(), uvRect.getMaxY() * uvData.getLambda());
                        }

                        // visibility reference extrema :
                        final Float refMin = uvMapData.getMin();
                        final Float refMax = uvMapData.getMax();

                        // model image options :
                        final ImageMode imageMode = uvMapData.getImageMode();
                        final int imageSize = uvMapData.getImageSize();
                        final IndexColorModel colorModel = uvMapData.getColorModel();
                        final ColorScale colorScale = uvMapData.getColorScale();
                        final VisNoiseService noiseService = uvMapData.getNoiseService();

                        // Compute a correct uv map for analytical models ONLY:
                        if (target.hasAnalyticalModel()) {
                            // update the status bar :
                            StatusBar.show(MSG_COMPUTING_MAP);

                            // Create uv map task worker :
                            // Cancel other tasks and execute this new task :
                            new UVMapSwingWorker(this, target.getModels(), uvRect, refMin, refMax,
                                    imageMode, imageSize, colorModel, colorScale, noiseService).executeTask();

                        } else if (false) {
                            // not working: sub region (uvRect) not supported

                            // Fix image index:
                            final List<UserModelData> modelDataList = (target.hasAnalyticalModel()) ? null : target.getUserModel().getModelDataList();

                            if (modelDataList != null) {
                                final int imageIdx = uvMapData.getImageIndex();

                                // Get preloaded and prepared fits image at given index:
                                final FitsImage fitsImage = modelDataList.get(imageIdx).getFitsImage();

                                if (fitsImage != null) {
                                    new UVMapSwingWorker(this, fitsImage, uvRect, refMin, refMax,
                                            imageMode, imageSize, colorModel, colorScale, noiseService).executeTask();
                                }
                            }
                        }
                    } else // cancel anyway currently running UVMapSwingWorker:
                    {
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
            _logger.debug("Computing model image ...");
            try {
                if (this.models != null) {
                    // compute anyway the uv map data :
                    return ModelUVMapService.computeUVMap(this.models, this.uvRect, this.refMin, this.refMax, null,
                            this.imageMode, this.imageSize, this.colorModel, this.colorScale, this.noiseService);

                } else if (fitsImage != null) {
                    // NOT working: sub region (uvRect) not supported
                    // Note: throws IllegalArgumentException if the fits image is invalid:
                    return UserModelService.computeUVMap(this.fitsImage, this.uvRect, this.imageMode,
                            this.imageSize, this.colorModel, this.colorScale, noiseService, this.refMin, this.refMax, null);
                }

            } catch (InterruptedJobException ije) {
                _logger.debug("Computing model image interrupted: ", ije);
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
            // check that rectangle area corresponds to the current UV area:
            // It may happen that GUI changed while the asynchronously computed image is ready !
            final Rectangle2D.Double uvRectCurrent = this.uvPanel.getCurrentUVRect();
            if (this.uvRect.equals(uvRectCurrent)) {

                // delegates to uv coverage panel :
                this.uvPanel.updateUVMap(uvMapData.getUvMap(), uvMapData.getDataMin(), uvMapData.getDataMax(), uvMapData.getColorScale());

                // update the status bar:
                StatusBar.showIfPrevious(MSG_COMPUTING_MAP, "uv map done.");
            } else {
                logger.debug("Incompatible uv rectangles: {} vs {}", uvRect, uvRectCurrent);
            }
        }

        /**
         * Handle the execution exception that occured in the compute operation @see #computeInBackground()
         * This implementation resets the plot and opens a message dialog or the feedback report depending on the cause.
         *
         * @param ee execution exception
         */
        @Override
        public void handleException(final ExecutionException ee) {
            this.uvPanel.resetUVMap();
            if (ee.getCause() instanceof IllegalArgumentException) {
                MessagePane.showErrorMessage(ee.getCause().getMessage());
            } else {
                super.handleException(ee);
            }
        }
    }

    private Rectangle2D.Double getCurrentUVRect() {
        final ValueAxis domainAxis = this.xyPlot.getDomainAxis();
        final ValueAxis rangeAxis = this.xyPlot.getRangeAxis();

        final Rectangle2D.Double uvRect = new Rectangle2D.Double();
        uvRect.setFrameFromDiagonal(
                fromUVPlotScale(domainAxis.getLowerBound()),
                fromUVPlotScale(rangeAxis.getLowerBound()),
                fromUVPlotScale(domainAxis.getUpperBound()),
                fromUVPlotScale(rangeAxis.getUpperBound()));

        return uvRect;
    }

    /**
     * Compute a sub image for the UV Map given the new uv area
     * @param uvMapData UV Map data
     * @param uvRect uv area
     * @return true if the given uvRect is smaller than uvRect of the reference image
     */
    private boolean computeSubUVMap(final UVMapData uvMapData, final Rectangle2D.Double uvRect) {
        final int imageSize = uvMapData.getUvMapSize();

        // uv area reference :
        final Rectangle2D.Double uvRectRef = uvMapData.getUvMapRect();

        if (logger.isDebugEnabled()) {
            logger.debug("uv map rect     = {}", uvRect);
            logger.debug("uv map rect REF = {}", uvRectRef);
        }

        final double pixRatioX = ((double) imageSize) / uvRectRef.getWidth();
        final double pixRatioY = ((double) imageSize) / uvRectRef.getHeight();

        // note : floor/ceil to be sure to have at least 1x1 pixel image
        int x = (int) Math.floor(pixRatioX * (uvRect.getX() - uvRectRef.getX()));
        int y = (int) Math.floor(pixRatioY * (uvRect.getY() - uvRectRef.getY()));
        int w = (int) Math.ceil(pixRatioX * uvRect.getWidth());
        int h = (int) Math.ceil(pixRatioY * uvRect.getHeight());

        // check bounds:
        x = checkBounds(x, 0, imageSize - 1);
        y = checkBounds(y, 0, imageSize - 1);
        w = checkBounds(w, 1, imageSize - x);
        h = checkBounds(h, 1, imageSize - y);

        final boolean doCrop = ((x != 0) || (y != 0) || (w != imageSize) || (h != imageSize));

        if (logger.isDebugEnabled()) {
            logger.debug("sub image [{}, {} - {}, {}] - doCrop = {}", new Object[]{x, y, w, h, doCrop});
        }

        // check reset zoom to avoid computing sub image == ref image:
        if (doCrop) {
            // adjust rounded data coords:
            logger.debug("uv rect (IN) = {}", uvRect);

            uvRect.setRect(
                    ((double) x) / pixRatioX + uvRectRef.getX(),
                    ((double) y) / pixRatioY + uvRectRef.getY(),
                    ((double) w) / pixRatioX,
                    ((double) h) / pixRatioY
            );
            logger.debug("uv rect (OUT) = {}", uvRect);

            // Note : the image is produced from an array where 0,0 corresponds to the upper left corner
            // whereas it corresponds in UV to the lower U and Upper V coordinates => inverse the V axis
            // Inverse V axis issue :
            y = imageSize - y - h;

            // crop a small sub image displayed while the correct model image is computed:
            final Image subUVMap = uvMapData.getUvMap().getSubimage(x, y, w, h);

            // update the background image only:
            updateUVMap(subUVMap, null, null, null);

        } else {
            // restore the background image and legend scale:
            updateUVMap(uvMapData.getUvMap(), uvMapData.getDataMin(), uvMapData.getDataMax(), uvMapData.getColorScale());
        }

        return doCrop;
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

            updateUVMap(uvMapData.getUvMap(), uvMapData.getDataMin(), uvMapData.getDataMax(), colorScale);

        } else {
            resetUVMap();
        }
    }

    /**
     * Reset the background image of the chart
     */
    private void resetUVMap() {
        updateUVMap(null, null, null, null);
    }

    /**
     * Update the background image of the chart with the UV Map
     * @param image image or null
     * @param dataMin minimum value to update color scale bounds
     * @param dataMax maximum value to update color scale bounds
     * @param colorScale color scaling method
     */
    private void updateUVMap(final Image image, final Float dataMin, final Float dataMax, final ColorScale colorScale) {
        if (image != null) {
            // check that the uvMap is different than currently displayed one:
            final Image bckgImg = this.xyPlot.getBackgroundImage();
            if (image != bckgImg) {
                if (dataMin != null && dataMax != null
                        && dataMin.floatValue() != dataMax.floatValue() && !Float.isInfinite(dataMin) && !Float.isInfinite(dataMax)) {
                    logger.debug("data min = {} - max = {}", dataMin, dataMax);

                    double min = dataMin.doubleValue();
                    double max = dataMax.doubleValue();

                    if (colorScale == ColorScale.LOGARITHMIC) {
                        final double log10Min = Math.log10(min);
                        final double log10Max = Math.log10(max);

                        if ((int) log10Max - (int) log10Min == 0) {
                            // fix data range to lower and upper pow(10):
                            min = FastMath.pow(10d, Math.floor(log10Min));
                            max = FastMath.pow(10d, Math.ceil(log10Max));
                        }
                    }
                    if (logger.isDebugEnabled()) {
                        logger.debug("min = {} - max = {}", min, max);
                    }

                    // fix axis boundaries:
                    this.mapLegend.getAxis().setRange(min, max);
                }
                // Recycle previous image:
                if (bckgImg instanceof BufferedImage) {
                    final BufferedImage bi = (BufferedImage) bckgImg;
                    // avoid sub images (child raster):
                    if (bi.getRaster().getParent() == null
                            && this.chartData != null && this.chartData.getUVMapData() != null) {
                        // check if this is the reference image:
                        if (bckgImg != this.chartData.getUVMapData().getUvMap()) {
                            // recycle previous images:
                            ImageUtils.recycleImage(bi);
                        }
                    }
                }
                if (logger.isDebugEnabled() && image instanceof BufferedImage) {
                    final BufferedImage bi = (BufferedImage) image;
                    logger.debug("display Image[{} x {}] @ {}", bi.getWidth(), bi.getHeight(), bi.hashCode());
                }
                this.xyPlot.setBackgroundPaint(null);
                this.xyPlot.setBackgroundImage(image);
            }
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

        // points renderer:
        final AbstractRenderer rendererPoints = (AbstractRenderer) this.xyPlot.getRenderer(); // DATASET_UV_POINTS
        // show series in legend:
        rendererPoints.setDefaultSeriesVisibleInLegend(true);
        // side effect with chart theme :
        rendererPoints.setAutoPopulateSeriesPaint(false);
        // reset colors :
        rendererPoints.clearSeriesPaints(false);

        // tracks renderer:
        final AbstractRenderer rendererTracks = (AbstractRenderer) this.xyPlot.getRenderer(DATASET_UV_TRACKS);
        // hide series in legend:
        rendererTracks.setDefaultSeriesVisibleInLegend(false);
        // side effect with chart theme :
        rendererTracks.setAutoPopulateSeriesPaint(false);
        // reset colors :
        rendererTracks.clearSeriesPaints(false);

        // points shadow renderer:
        final AbstractRenderer rendererPointsShadow = (AbstractRenderer) this.xyPlot.getRenderer(DATASET_UV_POINTS_SHADOW);
        // hide series in legend:
        rendererPointsShadow.setDefaultSeriesVisibleInLegend(false);
        // side effect with chart theme :
        rendererPointsShadow.setAutoPopulateSeriesPaint(false);
        rendererPointsShadow.setDefaultPaint(SHADOW_COLOR);

        // points shadow renderer:
        final AbstractRenderer rendererTracksShadow = (AbstractRenderer) this.xyPlot.getRenderer(DATASET_UV_TRACKS_SHADOW);
        // hide series in legend:
        rendererTracksShadow.setDefaultSeriesVisibleInLegend(false);
        // side effect with chart theme :
        rendererTracksShadow.setAutoPopulateSeriesPaint(false);
        rendererTracksShadow.setDefaultPaint(SHADOW_COLOR);

        // Create dataset with UV coverage data :
        final XYSeriesCollection datasetPoints = prepareDataset(chartData, rendererPoints);
        this.updateUVObservableRanges(datasetPoints, chartData);

        final XYSeriesCollection datasetTracks = prepareDataset(chartData, rendererTracks);
        this.updateUVTracksRiseSet(datasetTracks, chartData);

        // define bounds to the uv maximum value (before setDataset which calls restoreAxisBounds()) :
        final UVCoverageData uvData = chartData.getFirstUVData();

        // Get the correct uv max from the model image because FFTs have gridding issue => smaller max frequency
        final double uvMaxFreq = (uvMapData != null) ? -uvMapData.getUvMapRect().getMinX() : uvData.getUvMaxFreq();
        // uv in megalambda:
        this.xyPlot.defineBounds(toUVPlotScale(uvMaxFreq));
        // uv in meters (megalambda to meter conversion):
        this.xyPlot.defineAxisBounds(1, uvMaxFreq * uvData.getLambda());

        // Reset Fixed legend items:
        this.xyPlot.setFixedLegendItems(null);

        // set the main data set :
        this.xyPlot.setDataset(DATASET_UV_POINTS, datasetPoints);

        // Collect legend items now to avoid duplicates:        
        this.xyPlot.setFixedLegendItems(this.xyPlot.getLegendItems());

        // hide series in legend:
        rendererPoints.setDefaultSeriesVisibleInLegend(false);

        this.xyPlot.setDataset(DATASET_UV_POINTS_SHADOW, datasetPoints);
        this.xyPlot.setDataset(DATASET_UV_TRACKS, datasetTracks);
        this.xyPlot.setDataset(DATASET_UV_TRACKS_SHADOW, datasetTracks);
    }

    /**
     * Prepare the dataset i.e. create all XYSeries once for all
     * @param chartData chart data
     * @param renderer optional renderer to use to setSeriesPaint
     * @return dataset
     */
    private static XYSeriesCollection prepareDataset(final ObservationCollectionUVData chartData, final AbstractRenderer renderer) {
        // Get Global SharedSeriesAttributes:
        final SharedSeriesAttributes globalAttrs = SharedSeriesAttributes.INSTANCE;

        final XYSeriesCollection dataset = new XYSeriesCollection();

        XYSeries xySeries;
        String label;

        final boolean single = chartData.isSingle();

        // Iterate over UV Coverage data (multi conf) :
        for (int c = 0, len = chartData.size(); c < len; c++) {
            final UVCoverageData uvData = chartData.getUVDataList().get(c);

            if (!single) {
                // 1 color per configuration (i.e. per XYSeries) :
                label = chartData.getConfigurationLabel().get(c);
                xySeries = new XYSeries(label, false);
                xySeries.setNotify(false);

                dataset.addSeries(xySeries);

            } else {
                for (BaseLine bl : uvData.getBaseLines()) {
                    // 1 color per base line (i.e. per XYSeries) :
                    label = bl.getName();
                    xySeries = new XYSeries(label, false);
                    xySeries.setNotify(false);

                    dataset.addSeries(xySeries);
                }
            } // BL
        }

        // Apply attributes to dataset:
        for (int serie = 0, seriesCount = dataset.getSeriesCount(); serie < seriesCount; serie++) {
            label = (String) dataset.getSeriesKey(serie);
            renderer.setSeriesPaint(serie, globalAttrs.getColorAlpha(label), false);
        }

        return dataset;
    }

    /**
     * Update the dataset with UV observable ranges
     * @param dataset dataset to use
     * @param chartData chart data
     */
    private void updateUVObservableRanges(final XYSeriesCollection dataset, final ObservationCollectionUVData chartData) {

        List<UVRangeBaseLineData> targetUVObservability;
        UVCoverageData uvData;
        int nPoints;
        TargetPointInfo[] targetPointInfos;
        TargetPointInfo targetPointInfo;

        XYSeries xySeries = null;
        String serieKey, blName, confName;
        Map<Integer, String> tooltipMap = null;

        double[] u;
        double[] v;
        double[] uWMin;
        double[] vWMin;
        double[] uWMax;
        double[] vWMax;
        double x1, y1, x2, y2;

        final String timeRef = chartData.getFirstObsData().getTimeRef().getDisplayName();

        final boolean single = chartData.isSingle();

        // Iterate over UV Coverage data (multi conf) :
        for (int c = 0, len = chartData.size(); c < len; c++) {
            uvData = chartData.getUVDataList().get(c);

            // process observable uv ranges :
            targetUVObservability = uvData.getTargetUVObservability();

            if (targetUVObservability != null) {
                // target is observable:
                nPoints = uvData.getNPoints();
                targetPointInfos = uvData.getTargetPointInfos();

                confName = chartData.getConfigurationLabel().get(c);

                if (!single) {
                    serieKey = confName;

                    // 1 color per configuration (i.e. per XYSeries) :
                    xySeries = dataset.getSeries(serieKey);

                    tooltipMap = this.seriesTooltips.get(serieKey);
                    if (tooltipMap == null) {
                        tooltipMap = new HashMap<Integer, String>(2 * nPoints * targetUVObservability.size());
                        this.seriesTooltips.put(serieKey, tooltipMap);
                    }
                }

                for (UVRangeBaseLineData uvBL : targetUVObservability) {

                    blName = uvBL.getName();

                    if (single) {
                        serieKey = blName;

                        // 1 color per base line (i.e. per XYSeries) :
                        xySeries = dataset.getSeries(serieKey);

                        tooltipMap = this.seriesTooltips.get(serieKey);
                        if (tooltipMap == null) {
                            tooltipMap = new HashMap<Integer, String>(2 * nPoints);
                            this.seriesTooltips.put(serieKey, tooltipMap);
                        }
                    }

                    u = uvBL.getU();
                    v = uvBL.getV();
                    uWMin = uvBL.getUWMin();
                    vWMin = uvBL.getVWMin();
                    uWMax = uvBL.getUWMax();
                    vWMax = uvBL.getVWMax();

                    for (int i = 0, size = uvBL.getNPoints(); i < size; i++) {
                        targetPointInfo = targetPointInfos[i];

                        x1 = toUVPlotScale(uWMax[i]);
                        y1 = toUVPlotScale(vWMax[i]);

                        x2 = toUVPlotScale(uWMin[i]);
                        y2 = toUVPlotScale(vWMin[i]);

                        // first segment :
                        xySeries.add(x1, y1, false);

                        // line tooltip use index of (x2, y2) point:
                        tooltipMap.put(NumberUtils.valueOf(xySeries.getItemCount()),
                                generateTooltip(blName, confName, targetPointInfo, timeRef, u[i], v[i]));

                        xySeries.add(x2, y2, false);

                        // add an invalid point to break the line between the 2 segments :
                        xySeries.add(NumberUtils.DBL_NAN, NumberUtils.DBL_NAN, false);

                        // second symetric segment :
                        xySeries.add(-x1, -y1, false);

                        // line tooltip use index of (x2, y2) point:
                        tooltipMap.put(NumberUtils.valueOf(xySeries.getItemCount()),
                                generateTooltip(blName, confName, targetPointInfo, timeRef, -u[i], -v[i]));

                        xySeries.add(-x2, -y2, false);

                        // add an invalid point to break the line between the 2 segments :
                        xySeries.add(NumberUtils.DBL_NAN, NumberUtils.DBL_NAN, false);

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
                    xySeries = dataset.getSeries(chartData.getConfigurationLabel().get(c));
                }

                // TODO: adjust capacity of xySeries !
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
                    xySeries.add(NumberUtils.DBL_NAN, NumberUtils.DBL_NAN, false);

                    // second symetric ellipse line :
                    for (int i = 0, size = uvBL.getNPoints(); i < size; i++) {
                        x = toUVPlotScale(-u[i]);
                        y = toUVPlotScale(-v[i]);

                        xySeries.add(x, y, false);
                    } // points

                    // add an invalid point to break the line between the 2 segments :
                    xySeries.add(NumberUtils.DBL_NAN, NumberUtils.DBL_NAN, false);

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
            _logger.debug("Computing oifits data ...");

            List<OIFitsFile> result = null;
            try {
                // Start the computations :
                final long start = System.nanoTime();

                final List<OIFitsFile> oiFitsList = new ArrayList<OIFitsFile>(this.oiFitsCreatorList.size());

                for (OIFitsCreatorService oiFitsCreator : this.oiFitsCreatorList) {
                    // Create the OIFits structure and compute its values:
                    final OIFitsFile oiFitsFile = oiFitsCreator.createOIFits();

                    if (oiFitsFile != null) {
                        oiFitsList.add(oiFitsFile);
                    }

                    // fast interrupt :
                    if (Thread.currentThread().isInterrupted()) {
                        return null;
                    }
                }

                // note: even if the list is empty, return it to call refreshGUI(list)
                result = oiFitsList;

                _logger.info("compute[OIFitsFiles]: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

            } catch (InterruptedJobException ije) {
                _logger.debug("Computing oifits data interrupted: ", ije);
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
            om.setOIFitsData(
                    new OIFitsData(oiFitsList, this.uvDataCollection.getWarningContainer())
            );

            // update the status bar:
            StatusBar.showIfPrevious(MSG_COMPUTING_OIFITS, "OIFits done.");
        }
    }
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private fr.jmmc.aspro.gui.util.AnimatorPanel animatorPanel;
    private javax.swing.JCheckBox jCheckBoxAddNoise;
    private javax.swing.JCheckBox jCheckBoxDoOIFits;
    private javax.swing.JCheckBox jCheckBoxModelImage;
    private javax.swing.JCheckBox jCheckBoxPlotUVSupport;
    private javax.swing.JCheckBox jCheckBoxUseBias;
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

        if (event.getType() == ChartProgressEvent.DRAWING_STARTED) {
            // Perform custom operations before chart rendering:
            // move JMMC annotation:
            this.aJMMC.setX(this.xyPlot.getDomainAxis().getUpperBound());
            this.aJMMC.setY(this.xyPlot.getRangeAxis().getLowerBound());
        }
    }

    /**
     * Format the given double value using the text field formatter
     * @param field formatted text field
     * @param value value to use
     * @return formatted string
     */
    private static String format(final JFormattedTextField field, final Double value) {
        String res = "";
        if (value != null) {
            try {
                res = field.getFormatter().valueToString(value);
            } catch (ParseException pe) {
                logger.error("parsing exception", pe);
            }
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
        return this.selectedTargetName;
    }

    /**
     * Define the currently selected target name
     * @param targetName selected target name
     */
    private void setSelectedTargetName(final String targetName) {
        this.selectedTargetName = targetName;
    }

    /**
     * Return the currently selected target from the main observation
     * @return target or null if not found
     */
    private Target getSelectedTarget() {
        final String targetName = getSelectedTargetName();
        if (targetName != null) {
            // Use main observation to get the latest revision of the Target:
            return om.getMainObservation().getTarget(targetName);
        }
        return null;
    }

    /**
     * Perform image refresh on the given user model and image index (always on [0; modelDataList.size - 1]
     * @param userModelFile user model file to use
     * @param imageIndex image index to display
     */
    @Override
    public void perform(final String userModelFile, final int imageIndex) {
        // Ensure this panel is visible and no swing task is running:
        // note: FitsImagePanel can start swing worker tasks but UV Coverage panel should be the first listener called !
        if (Aspro2.getInstance().getSettingPanel().isUVCoveragePanelSelected()
                && !TaskSwingWorkerExecutor.isTaskRunning()) {

            // update local image index:
            this.imageIndex = imageIndex;

            // use the latest observation for computations :
            plot(om.getObservationCollection());
        }
    }

    /**
     * Generates the tooltip text for the specified item.
     *
     * @param dataset  the dataset (<code>null</code> not permitted).
     * @param series  the series index (zero-based).
     * @param item  the item index (zero-based).
     *
     * @return The tooltip text (possibly <code>null</code>).
     */
    @Override
    public String generateToolTip(final XYDataset dataset, final int series, final int item) {
        // note: series corresponds to the baseline or configuration, item to its UV segment (or symetric one)
        final String serieKey = (String) dataset.getSeriesKey(series);

        final Map<Integer, String> tooltipMap = this.seriesTooltips.get(serieKey);

        if (tooltipMap == null) {
            return null;
        }

        return tooltipMap.get(NumberUtils.valueOf(item));
    }

    /**
     * Generate the tooltip's text for the given UV point
     * @param baseline corresponding base line
     * @param confName configuration name
     * @param targetPointInfo target point information for the given UV point
     * @param timeRef time reference LST or UTC
     * @param u u coordinate in meters
     * @param v v coordinate in meters
     * @return tooltip's text for an UV point
     */
    public String generateTooltip(final String baseline, final String confName, final TargetPointInfo targetPointInfo, final String timeRef,
                                  final double u, final double v) {

        final StringBuffer sb = this.sbToolTip;
        sb.setLength(0); // clear

        sb.append("<html><b>").append(confName);
        sb.append("<br>Base line: ").append(baseline);

        sb.append("<br>Time</b>: ");
        FormatterUtils.format(this.timeFormatter, sb, targetPointInfo.getDate());
        sb.append(" [").append(timeRef).append(']');

        sb.append("<br><b>HA</b>: ");
        FormatterUtils.format(this.df1, sb, targetPointInfo.getHa());
        sb.append(" (az ").append((int) Math.round(targetPointInfo.getAzimuth()));
        sb.append(", el ").append((int) Math.round(targetPointInfo.getElevation())).append(')');

        sb.append("<br><b>Airmass</b>: ");
        FormatterUtils.format(this.df1, sb, targetPointInfo.getAirmass());

        // use U and V to display radius and position angle:
        sb.append("<br><b>Radius</b>: ");
        FormatterUtils.format(this.df1, sb, MathUtils.carthesianNorm(u, v));
        sb.append(" m<br><b>Pos. angle</b>: ");
        FormatterUtils.format(this.df1, sb, FastMath.toDegrees(FastMath.atan2(u, v)));
        sb.append(" deg</html>");

        return sb.toString();
    }

    /**
     * Add UV Points at current time (timeline marker in red) as annotations
     */
    private void updateTimeAnnotations() {
        // remove annotations anyway:
        this.xyPlot.clearAnnotations();

        boolean enableTimer = false;

        // do not export time marker in PDF output:
        if (!this.renderingPDF && getChartData() != null && getChartData().isSingle()) {

            final ObservabilityData obsData = getChartData().getFirstObsData();

            // Get AstroSkyCalc instance :
            final AstroSkyCalc sc = obsData.getDateCalc();

            // Get jd of current date/time:
            final double jd = sc.getCurrentJd();

            // check if the current jd is within the good night:
            if (jd >= obsData.getJdMin() && jd <= obsData.getJdMax()) {
                // enable timeline refresh timer:
                enableTimer = true;

                // convert JD to LST/UT date/time and
                // roll +/- 1 day to be within plot range:
                final Date now = sc.toDate(jd, obsData.getTimeRef(), obsData.getDateMin(), obsData.getDateMax());

                if (logger.isDebugEnabled()) {
                    logger.debug("current date/time is: {}", now);
                }

                // Compute UV Points:
                // Get starData for the selected target name :
                final StarData starData = obsData.getStarData(getSelectedTargetName());

                final double ha = AstroSkyCalc.checkHA(sc.convertJDToHA(jd, starData.getPrecRA()));

                final List<UVRangeBaseLineData> targetUVPoints
                                                = UVCoverageService.computeUVPoints(getChartData().getFirstObservation(), obsData, starData, ha);

                if (targetUVPoints != null) {
                    final String textNow = FormatterUtils.format(this.timeFormatter, now);

                    double u1, v1, u2, v2;

                    for (UVRangeBaseLineData uvBL : targetUVPoints) {

                        u1 = toUVPlotScale(uvBL.getUWMax()[0]);
                        v1 = toUVPlotScale(uvBL.getVWMax()[0]);

                        u2 = toUVPlotScale(uvBL.getUWMin()[0]);
                        v2 = toUVPlotScale(uvBL.getVWMin()[0]);

                        // uv point:
                        this.xyPlot.addAnnotation(new EnhancedXYLineAnnotation(u1, v1, u2, v2, ChartUtils.THIN_STROKE, Color.RED), false);
                        this.xyPlot.addAnnotation(createTimeAnnotation(textNow, u2, v2), false);

                        // symetric uv point:
                        this.xyPlot.addAnnotation(new EnhancedXYLineAnnotation(-u1, -v1, -u2, -v2, ChartUtils.THIN_STROKE, Color.RED), false);
                        this.xyPlot.addAnnotation(createTimeAnnotation(textNow, -u2, -v2), false);
                    }
                }
            }
        }
        // anyway enable or disable timer:
        enableTimelineRefreshTimer(enableTimer);
    }

    /**
     * Create time annotation with correct text anchor
     * @param text time to display
     * @param u u coordinate in mega lambda
     * @param v v coordinate in mega lambda
     * @return xy text annotation
     */
    public static ExtendedXYTextAnnotation createTimeAnnotation(final String text, final double u, final double v) {
        final ExtendedXYTextAnnotation annotation = new ExtendedXYTextAnnotation(text, u, v);
        annotation.setFont(ChartUtils.DEFAULT_TEXT_SMALL_FONT);
        annotation.setPaint(Color.RED);

        if (v >= 0.0) {
            if (u >= 0.0) {
                annotation.setTextAnchor(TextAnchor.BOTTOM_LEFT);
            } else {
                // u negative:
                annotation.setTextAnchor(TextAnchor.BOTTOM_RIGHT);
            }
        } else // v negative:
        {
            if (u >= 0.0) {
                annotation.setTextAnchor(TextAnchor.TOP_LEFT);
            } else {
                // u negative:
                annotation.setTextAnchor(TextAnchor.TOP_RIGHT);
            }
        }

        return annotation;
    }

    /**
     * Start/Stop the internal timeline Refresh timer
     * @param enable true to enable it, false otherwise
     */
    private void enableTimelineRefreshTimer(final boolean enable) {
        if (enable) {
            if (!this.timerTimeRefresh.isRunning()) {
                logger.debug("Starting timer: {}", this.timerTimeRefresh);

                this.timerTimeRefresh.start();
            }
        } else if (this.timerTimeRefresh.isRunning()) {
            logger.debug("Stopping timer: {}", this.timerTimeRefresh);

            this.timerTimeRefresh.stop();
        }
    }
}
