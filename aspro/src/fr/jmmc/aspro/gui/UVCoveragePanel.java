/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: UVCoveragePanel.java,v 1.90 2011-03-09 14:18:29 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.89  2011/03/04 16:57:36  bourgesl
 * minor
 *
 * Revision 1.88  2011/03/01 17:11:38  bourgesl
 * minor changes
 *
 * Revision 1.87  2011/02/28 17:13:48  bourgesl
 * fixed dataset (always defined) that use CONF + PoPs in the legend
 *
 * Revision 1.86  2011/02/25 16:50:48  bourgesl
 * simplify title / file name via observation collection API
 * multi conf support on plot
 *
 * Revision 1.85  2011/02/24 17:14:12  bourgesl
 * Major refactoring to support / handle observation collection (multi-conf)
 *
 * Revision 1.84  2011/02/22 18:11:30  bourgesl
 * Major UI changes : configuration multi-selection, unique target selection in main form
 *
 * Revision 1.83  2011/02/08 15:34:16  bourgesl
 * changed proposed OB file names to include instrument mode and FT/noFT
 *
 * Revision 1.82  2011/02/07 16:07:36  bourgesl
 * performance : use current chart data to avoid computing UV Map (model image) if it is still valid !
 *
 * Revision 1.81  2011/02/04 17:18:30  bourgesl
 * new ChartData inner class to have chart state used by PDF export AND to handle zoom event (consistent with uv map image displayed)
 * use ModelUVMapService directly in UVCoverageWorker
 *
 * Revision 1.80  2011/02/03 17:25:42  bourgesl
 * minor clean up
 *
 * Revision 1.79  2011/02/02 17:44:12  bourgesl
 * added observation version checkings
 * comments / to do
 *
 * Revision 1.78  2011/01/31 15:29:08  bourgesl
 * use WarningContainerEvent instead of shared warning in observation
 * modified fireWarningsReady(warningContainer) to use WarningContainerEvent
 *
 * Revision 1.77  2011/01/31 13:27:16  bourgesl
 * updated java doc
 *
 * Revision 1.76  2011/01/28 16:32:35  mella
 * Add new observationEvents (CHANGED replaced by DO_UPDATE, REFRESH and REFRESH_UV)
 * Modify the observationListener interface
 *
 * Revision 1.75  2011/01/27 17:10:34  bourgesl
 * use target changed event to update the target combo box
 * use current observability data to compute UV Coverage (service)
 * renamed chart vars
 *
 * Revision 1.74  2011/01/26 17:23:41  bourgesl
 * comments + use Observability data (parameters)
 *
 * Revision 1.73  2011/01/25 12:29:37  bourgesl
 * fixed javadoc errors
 *
 * Revision 1.72  2011/01/25 10:41:19  bourgesl
 * added comments
 * moved OM.setComputedResult in refreshUI
 *
 * Revision 1.71  2011/01/21 16:28:03  bourgesl
 * import ObservationEventType
 * use AsproTaskRegistry instead of task family
 * extracted TaskSwingWorker classes to see clearly what are inputs/outputs
 * reset computed results using Swing EDT (plot) and not in background
 *
 * Revision 1.70  2011/01/07 13:20:59  bourgesl
 * getSelectedTargetName made private
 *
 * Revision 1.69  2010/12/17 15:17:21  bourgesl
 * major change : target combo box use display targets instead of target names
 *
 * Revision 1.68  2010/12/14 09:25:01  bourgesl
 * target change event refactored
 *
 * Revision 1.67  2010/12/10 17:14:03  bourgesl
 * hack to update target list
 *
 * Revision 1.66  2010/12/01 16:35:42  bourgesl
 * 'Model editor' renamed to 'Target editor'
 *
 * Revision 1.65  2010/11/18 17:18:23  bourgesl
 * use new TargetEditorDialog
 *
 * Revision 1.64  2010/10/22 13:31:10  bourgesl
 * code convention
 *
 * Revision 1.63  2010/10/21 16:51:01  bourgesl
 * JMMC trademark made less important
 *
 * Revision 1.62  2010/10/15 16:59:43  bourgesl
 * new PDF options (page size and orientation)
 * PDFExportable refactoring to include prepareChart, postPDF and getPDFOptions methods
 *
 * Revision 1.61  2010/10/14 10:58:03  bourgesl
 * Fixed bug related to sampling periodicity : use the instrument default sampling time when an invalid value is detected
 *
 * Revision 1.60  2010/10/08 09:39:03  bourgesl
 * removed log when the selected target changes
 *
 * Revision 1.59  2010/10/05 15:06:22  bourgesl
 * sycnhronize target selection from UV coverage panel to observation form
 *
 * Revision 1.58  2010/10/04 14:31:46  bourgesl
 * use the minimum baseline as a minimum for the UV Max coverage in order to avoid zero value (bug)
 *
 * Revision 1.57  2010/10/01 15:30:52  bourgesl
 * define warning container filled in UVCoverageService (including noise service messages)
 * use MessagePane
 *
 * Revision 1.56  2010/09/26 12:00:07  bourgesl
 * do not catch runtime exceptions
 *
 * Revision 1.55  2010/09/24 15:49:48  bourgesl
 * use MessagePane
 *
 * Revision 1.54  2010/09/23 19:46:35  bourgesl
 * comments when calling FeedBackReport
 *
 * Revision 1.53  2010/09/20 14:46:02  bourgesl
 * minor refactoring changes
 *
 * Revision 1.52  2010/09/15 13:56:31  bourgesl
 * added JMMC copyright on plot
 *
 * Revision 1.51  2010/09/08 16:00:31  bourgesl
 * unregister Preference Observers when the widget is released (Preference View, UV Coverage Panel)
 *
 * Revision 1.50  2010/09/06 13:39:34  bourgesl
 * small changes on Panels (scrollbar added as needed) in order to solve widget display on small screens
 *
 * Revision 1.49  2010/07/22 15:45:43  bourgesl
 * added acquisition time in UV coverage and observation
 *
 * Revision 1.48  2010/07/22 14:34:23  bourgesl
 * sampling time is updated with the default sampling time of the instrument when the instrument is changed
 * atmosphere quality is displayed and updated when an observation is loaded
 *
 * Revision 1.47  2010/07/07 15:12:15  bourgesl
 * fixed NPE on optional fields (load)
 *
 * Revision 1.46  2010/07/05 14:52:26  bourgesl
 * corrected comments
 *
 * Revision 1.45  2010/06/25 14:16:51  bourgesl
 * refactoring to use UV for WMin / WMax
 *
 * Revision 1.44  2010/06/23 12:52:08  bourgesl
 * ObservationManager regsitration for observation events moved in SettingPanel (external)
 *
 * Revision 1.43  2010/06/17 10:02:51  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.42  2010/06/10 08:54:06  bourgesl
 * rename variable
 *
 * Revision 1.41  2010/06/09 12:51:09  bourgesl
 * new interface PDFExportable to define a standard method performPDFAction() that use ExportPDFAction to export the chart to PDF
 *
 * Revision 1.40  2010/06/08 12:39:06  bourgesl
 * minor UI changes : pdf button moved to bottom, changed weight constraint (resizing issues)
 * javadoc
 *
 * Revision 1.39  2010/06/08 10:20:42  bourgesl
 * minor UI changes (layout / margins)
 *
 * Revision 1.38  2010/06/07 16:03:29  bourgesl
 * changed tooltip on 'OB' button
 *
 * Revision 1.37  2010/05/26 15:30:24  bourgesl
 * added CHARA Vega Star List generation (OB like)
 *
 * Revision 1.36  2010/05/21 14:27:48  bourgesl
 * use preferences for Model Image Lut & Size
 * removed previous widgets
 *
 * Revision 1.35  2010/05/11 12:04:56  bourgesl
 * minor changes due to ChartUtils & chart duration
 *
 * Revision 1.34  2010/05/06 15:40:20  bourgesl
 * added updateObservation and plot debug logs
 * added better auto update/refresh flag handling
 * HA Min/Max + FT Mode are related to the target (onLoad, onChange, updateObservation)
 *
 * Revision 1.33  2010/05/05 14:28:48  bourgesl
 * on load : restore sampling period + image defaults (size, lut)
 * added ha Min / Max to generate OB with correct LST intervals
 *
 * Revision 1.32  2010/04/14 13:09:59  bourgesl
 * first minimal OB for MIDI
 *
 * Revision 1.31  2010/04/13 14:18:27  bourgesl
 * uniform sizes for sliders and text fields
 *
 * Revision 1.30  2010/04/08 14:06:51  bourgesl
 * javadoc
 *
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
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.gui.action.ExportOBVLTIAction;
import fr.jmmc.aspro.gui.action.ExportOBVegaAction;
import fr.jmmc.aspro.gui.action.ExportPDFAction;
import fr.jmmc.aspro.gui.chart.ChartUtils;
import fr.jmmc.aspro.gui.chart.PDFOptions;
import fr.jmmc.aspro.gui.chart.SquareChartPanel;
import fr.jmmc.aspro.gui.chart.SquareXYPlot;
import fr.jmmc.aspro.gui.chart.ZoomEvent;
import fr.jmmc.aspro.gui.chart.ZoomEventListener;
import fr.jmmc.aspro.gui.task.AsproTaskRegistry;
import fr.jmmc.aspro.gui.task.ObservationCollectionTaskSwingWorker;
import fr.jmmc.aspro.gui.util.ColorPalette;
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
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.ObservationCollection;
import fr.jmmc.aspro.model.uvcoverage.UVCoverageData;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.model.util.AtmosphereQualityUtils;
import fr.jmmc.aspro.model.uvcoverage.UVBaseLineData;
import fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData;
import fr.jmmc.aspro.service.UVCoverageService;
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.gui.task.TaskSwingWorker;
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
import java.util.ArrayList;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import java.util.Vector;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JFormattedTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.event.ChartProgressEvent;
import org.jfree.chart.event.ChartProgressListener;
import org.jfree.chart.renderer.AbstractRenderer;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.ui.Layer;
import org.jfree.ui.TextAnchor;

/**
 * This panel presents the UV coverage plot with its parameters (target, instrument mode ...)
 * @author bourgesl
 */
public final class UVCoveragePanel extends javax.swing.JPanel implements ChartProgressListener, ZoomEventListener,
                                                                         ActionListener, ChangeListener, ObservationListener, Observer, PDFExportable, Disposable {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.UVChartPanel";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** flag to log a stack trace in method updateObservation() to detect multiple calls */
  private final static boolean DEBUG_UPDATE_EVENT = false;
  /** flag to log a stack trace in method plot() to detect multiple calls */
  private final static boolean DEBUG_PLOT_EVENT = false;
  /** flag to log version checking */
  private final static boolean DEBUG_VERSIONS = false;
  /** scaling factor to Mega Lambda for U,V points */
  private final static double MEGA_LAMBDA_SCALE = 1e-6;
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
    jPanelSpacer = new javax.swing.JPanel();

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

    jLabelUVMax.setText("U-V range to plot");
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
    gridBagConstraints.gridy = 23;
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

    jComboBoxImageMode.setModel(new DefaultComboBoxModel(ModelUVMapService.ImageMode.values()));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 25;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(1, 1, 1, 1);
    jPanelLeft.add(jComboBoxImageMode, gridBagConstraints);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 26;
    gridBagConstraints.gridwidth = 2;
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weighty = 0.1;
    jPanelLeft.add(jPanelSpacer, gridBagConstraints);

    jScrollPaneForm.setViewportView(jPanelLeft);

    jSplitPane.setLeftComponent(jScrollPaneForm);

    add(jSplitPane, java.awt.BorderLayout.CENTER);
  }// </editor-fold>//GEN-END:initComponents

  /**
   * Export the selected target as an Observing Block (OB)
   * @param evt action event
   */
  public void performOBAction(final ActionEvent evt) {

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

      ExportOBVLTIAction.getInstance().process(evt);

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
  public void performPDFAction() {
    ExportPDFAction.exportPDF(this);
  }

  /**
   * Return the PDF default file name (without extension)
   * @return PDF default file name
   */
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
  public PDFOptions getPDFOptions() {
    return PDFOptions.DEFAULT_PDF_OPTIONS;
  }

  /**
   * Return the chart to export as a PDF document
   * @return chart
   */
  public JFreeChart prepareChart() {
    return this.chart;
  }

  /**
   * Callback indicating the chart was processed by the PDF engine
   */
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
    // Adjust outline :
    this.xyPlot.setOutlineStroke(new BasicStroke(1.f));

    // add listener :
    this.chart.addProgressListener(this);
    this.chartPanel = ChartUtils.createSquareChartPanel(this.chart);

    // zoom options :
    this.chartPanel.setDomainZoomable(AsproConstants.ENABLE_ZOOM);
    this.chartPanel.setRangeZoomable(AsproConstants.ENABLE_ZOOM);

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

      public void propertyChange(final PropertyChangeEvent evt) {
        final double newValue = ((Number) jFieldSamplingPeriod.getValue()).doubleValue();

        if (newValue <= 0d) {
          // invalid value :
          resetSamplingPeriod(om.getMainObservation());
        }

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("samplingPeriod changed : " + newValue);
        }
        fireObservationUpdateEvent();
      }
    });

    // default obs duration and property change listener :
    this.jFieldObsDuration.setValue(AsproConstants.DEFAULT_OBSERVATION_DURATION);
    this.jFieldObsDuration.addPropertyChangeListener("value", new PropertyChangeListener() {

      public void propertyChange(final PropertyChangeEvent evt) {
        final double newValue = ((Number) jFieldObsDuration.getValue()).doubleValue();

        if (newValue <= 0d) {
          // invalid value :
          jFieldObsDuration.setValue(AsproConstants.DEFAULT_OBSERVATION_DURATION);
        }

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("obsDuration changed : " + newValue);
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

      public void itemStateChanged(final ItemEvent e) {
        refreshPlot();
      }
    });

    this.jCheckBoxModelImage.addItemListener(new ItemListener() {

      public void itemStateChanged(final ItemEvent e) {
        final boolean enabled = jCheckBoxModelImage.isSelected();
        jComboBoxImageMode.setEnabled(enabled);

        refreshPlot();
      }
    });

    this.jComboBoxImageMode.addActionListener(this);

    // register this instance as a Preference Observer :
    this.myPreferences.addObserver(this);
  }

  /**
   * Free any ressource or reference to this instance :
   * remove this instance form Preference Observers
   */
  public void dispose() {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("dispose : " + this);
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
  public void update(final Observable o, final Object arg) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Preferences updated on : " + this);
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
    final boolean changed = intConfName != null && !intConfName.equals(this.interferometerConfigurationName);
    if (changed) {
      this.interferometerConfigurationName = intConfName;
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("interferometer configuration changed : " + intConfName);
      }

      final InterferometerConfiguration intConf = observation.getInterferometerConfiguration().getInterferometerConfiguration();

      // update the UV range :
      final double minBaseLine = intConf.getInterferometer().getMinBaseLine();
      final double maxBaseLine = intConf.getInterferometer().getMaxBaseLine();
      this.uvMaxAdapter.reset(minBaseLine, maxBaseLine, maxBaseLine);

      // refresh the fringe tracker modes that depends on the interferometer :
      this.updateComboFTModes(observation);
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
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("instrument changed : " + insName);
      }

      resetSamplingPeriod(observation);

      // update instrument modes :
      final Vector<String> v = ConfigurationManager.getInstance().getInstrumentModes(
              observation.getInterferometerConfiguration().getName(),
              observation.getInstrumentConfiguration().getName());
      this.jComboBoxInstrumentMode.setModel(new DefaultComboBoxModel(v));

      if (logger.isLoggable(Level.FINEST)) {
        logger.finest("jComboBoxInstrumentMode updated : " + this.jComboBoxInstrumentMode.getSelectedItem());
      }
    }
  }

  /**
   * Reset the sampling period to the default sampling time of the selected instrument
   * @param observation observation to use
   */
  private void resetSamplingPeriod(final ObservationSetting observation) {
    // reset the sampling time to the default sampling time of the instrument :
    final int defaultSamplingTime = getInstrumentDefaultSamplingTime(observation);

    this.jFieldSamplingPeriod.setValue(Double.valueOf(defaultSamplingTime));

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("defaultSamplingTime : " + defaultSamplingTime);
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
   * Update UI according to the target configuration
   */
  private void updateTargetConfiguration() {
    final String targetName = getSelectedTargetName();

    final TargetConfiguration targetConf = om.getTargetConfiguration(targetName);
    if (targetConf != null) {

      if (logger.isLoggable(Level.FINEST)) {
        logger.finest("updateTargetConfiguration : " + targetName);
      }

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

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("target HA min : " + min);
          logger.fine("target HA max : " + min);
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
      final List<Model> models = target.getModels();

      final boolean hasModel = (models != null && !models.isEmpty());

      this.jCheckBoxModelImage.setEnabled(hasModel);
      this.jComboBoxImageMode.setEnabled(hasModel);
    }
  }

  /**
   * Process any comboBox change event (target, instrument mode, image mode ...).
   * Refresh the dependent combo boxes and update the observation according to the form state
   * @param e action event
   */
  public void actionPerformed(final ActionEvent e) {
    if (e.getSource() == this.jComboBoxInstrumentMode) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("instrument mode changed : " + this.jComboBoxInstrumentMode.getSelectedItem());
      }
      fireObservationUpdateEvent();

    } else if (e.getSource() == this.jComboBoxFTMode) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("ft mode changed : " + this.jComboBoxFTMode.getSelectedItem());
      }
      fireObservationUpdateEvent();

    } else if (e.getSource() == this.jComboBoxAtmQual) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("atmQuality changed : " + this.jComboBoxAtmQual.getSelectedItem());
      }
      fireObservationUpdateEvent();

    } else if (e.getSource() == this.jComboBoxImageMode) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("image mode changed : " + this.jComboBoxImageMode.getSelectedItem());
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

      fireObservationUpdateEvent();

    } else if (source == this.haMaxAdapter) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("haMax changed : " + source.getValue());
      }
      this.haMinAdapter.setMaxValue(source.getValue());

      fireObservationUpdateEvent();

    } else if (source == this.uvMaxAdapter) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("U-V Max changed : " + source.getValue());
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
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("onLoadObservation :\n" + ObservationManager.toString(observation));
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
      this.jComboBoxInstrumentMode.setSelectedItem(observation.getInstrumentConfiguration().getInstrumentMode());

      // update the sampling period :
      if (observation.getInstrumentConfiguration().getSamplingPeriod() != null) {
        this.jFieldSamplingPeriod.setValue(Double.valueOf(observation.getInstrumentConfiguration().getSamplingPeriod()));
      }

      // update the acquisition time :
      if (observation.getInstrumentConfiguration().getAcquisitionTime() != null) {
        this.jFieldObsDuration.setValue(Double.valueOf(observation.getInstrumentConfiguration().getAcquisitionTime()));
      }

      // update atmQuality :
      if (observation.getWhen().getAtmosphereQuality() != null) {
        this.jComboBoxAtmQual.setSelectedItem(observation.getWhen().getAtmosphereQuality().value());
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
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("onTargetSelectionChange : " + target);
    }

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
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("observation :\n" + ObservationManager.toString(event.getObservation()));
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
        logger.log(Level.SEVERE, "UPDATE", new Throwable());
      }
      final String targetName = getSelectedTargetName();

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("onUpdateObservation : " + targetName);
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

      if (targetName != null) {

        // Update target HA Min/Max :
        changed |= om.setTargetHAMin(targetName, Double.valueOf(this.haMinAdapter.getValue()));
        changed |= om.setTargetHAMax(targetName, Double.valueOf(this.haMaxAdapter.getValue()));

        // update ft mode :
        changed |= om.setTargetFTMode(targetName, (String) this.jComboBoxFTMode.getSelectedItem());

      }

      // update atmQuality :
      changed |= om.setAtmosphereQuality(AtmosphereQualityUtils.getAtmosphereQuality((String) this.jComboBoxAtmQual.getSelectedItem()));

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
  public void onProcess(final ObservationEvent event) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("event [" + event.getType() + "] process IN");
    }
    switch (event.getType()) {
      case LOADED:
        this.onLoadObservation(event.getObservation());
        break;
      case TARGET_SELECTION_CHANGED:
        this.onTargetSelectionChange(((TargetSelectionEvent) event).getTarget());
        break;
      case DO_UPDATE:
        this.onUpdateObservation((UpdateObservationEvent) event);
        break;
      case REFRESH_UV:
        this.refreshPlot(event.getObservationCollection());
        break;
      case OBSERVABILITY_DONE:
        this.updateObservabilityData(((ObservabilityEvent) event).getObservabilityData());
        this.plot(event.getObservationCollection());
        break;
      default:
    }
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("event [" + event.getType() + "] process OUT");
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
  protected void refreshPlot() {
    if (this.doAutoRefresh) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("refreshPlot");
      }
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
  protected void refreshPlot(final ObservationCollection obsCollection) {
    if (obsCollection != null && this.getObservabilityData() != null) {
      // versions are the same for all observability data :
      final ObservabilityData obsData = this.getFirstObservabilityData();

      // avoid to mix inconsistent observation and observability data :
      // Next plot (observability done event) will take into account UI widget changes.

      if (obsData.getVersion().isSameMainVersion(obsCollection.getVersion())) {
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("refreshPlot : main version equals : " + obsData.getVersion() + " :: " + obsCollection.getVersion());
        }
        if (DEBUG_VERSIONS) {
          logger.severe("refreshPlot : main version equals : " + obsData.getVersion() + " :: " + obsCollection.getVersion());
        }

        this.plot(obsCollection);
      } else {
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("refreshPlot : main version mismatch : " + obsData.getVersion() + " :: " + obsCollection.getVersion());
        }
        if (DEBUG_VERSIONS) {
          logger.severe("refreshPlot : main version mismatch : " + obsData.getVersion() + " :: " + obsCollection.getVersion());
        }
      }
    }
  }

  /**
   * Plot the UV Coverage using a SwingWorker to do the computation in the background.
   * This code is executed by the Swing Event Dispatcher thread (EDT)
   * @param obsCollection observation collection to use
   */
  protected void plot(final ObservationCollection obsCollection) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("plot : " + ObservationManager.toString(obsCollection));
    }
    if (DEBUG_PLOT_EVENT) {
      logger.log(Level.SEVERE, "PLOT", new Throwable());
    }

    // Note : version checks are already done in refreshPlot(observation) :

    // check if observability data are available :
    if (this.getObservabilityData() != null) {

      /* get plot options from swing components */

      final String targetName = getSelectedTargetName();

      final double uvMax = this.uvMaxAdapter.getValue();

      final boolean doUVSupport = this.jCheckBoxPlotUVSupport.isSelected();
      final boolean doModelImage = this.jCheckBoxModelImage.isSelected();

      // model image options :
      final ImageMode imageMode = (ImageMode) this.jComboBoxImageMode.getSelectedItem();

      // Use model image Preferences :
      final int imageSize = this.myPreferences.getPreferenceAsInt(Preferences.MODEL_IMAGE_SIZE);
      final IndexColorModel colorModel = ColorModels.getColorModel(this.myPreferences.getPreference(Preferences.MODEL_IMAGE_LUT));

      // update the status bar :
      StatusBar.show("computing uv coverage ...");

      // Get previously computed UV Map Data (can be null) :
      final UVMapData currentUVMapData = (getChartData() != null) ? getChartData().getUVMapData() : null;

      // Create uv coverage task worker :
      // Cancel other tasks and execute this new task :
      new UVCoverageSwingWorker(this, obsCollection, this.getObservabilityData(), targetName,
              uvMax, doUVSupport, doModelImage, imageMode, imageSize, colorModel,
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
    /** maximum U or V coordinate (corrected by the minimal wavelength) */
    private double uvMax;
    /** flag to compute the UV support */
    private final boolean doUVSupport;
    /** flag to compute the model image */
    private final boolean doModelImage;
    /** image mode (amplitude or phase) */
    private final ImageMode imageMode;
    /** image size */
    private final int imageSize;
    /** image color model */
    private final IndexColorModel colorModel;
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
     * @param doModelImage flag to compute the model image
     * @param imageMode image mode (amplitude or phase)
     * @param imageSize number of pixels for both width and height of the generated image
     * @param colorModel color model to use
     * @param currentUVMapData previously computed UV Map Data
     */
    private UVCoverageSwingWorker(final UVCoveragePanel uvPanel, final ObservationCollection obsCollection,
                                  final List<ObservabilityData> obsDataList, final String targetName,
                                  final double uvMax, final boolean doUVSupport,
                                  final boolean doModelImage, final ImageMode imageMode, final int imageSize, final IndexColorModel colorModel,
                                  final UVMapData currentUVMapData) {
      // get current observation version :
      super(AsproTaskRegistry.TASK_UV_COVERAGE, obsCollection);
      this.uvPanel = uvPanel;
      this.obsDataList = obsDataList;
      this.targetName = targetName;
      this.uvMax = uvMax;
      this.doUVSupport = doUVSupport;
      this.doModelImage = doModelImage;
      this.imageMode = imageMode;
      this.imageSize = imageSize;
      this.colorModel = colorModel;
      this.currentUVMapData = currentUVMapData;
    }

    /**
     * Compute the UV Coverage data in background
     * This code is executed by a Worker thread (Not Swing EDT)
     * @return UV Coverage data
     */
    @Override
    public ObservationCollectionUVData computeInBackground() {

      // Start the computations :
      final long start = System.nanoTime();

      final List<ObservationSetting> observations = getObservationCollection().getObservations();
      final List<UVCoverageData> uvDataList = new ArrayList<UVCoverageData>(observations.size());

      ObservationSetting observation;
      ObservabilityData obsData;
      for (int i = 0, len = observations.size(); i < len; i++) {
        observation = observations.get(i);
        obsData = this.obsDataList.get(i);

        // compute the uv coverage data :
        uvDataList.add(
                new UVCoverageService(observation, obsData, this.targetName, this.uvMax, this.doUVSupport).compute());

        // fast interrupt :
        if (Thread.currentThread().isInterrupted()) {
          return null;
        }
      }

      final ObservationCollectionUVData uvDataCollection = new ObservationCollectionUVData(getObservationCollection(), this.obsDataList, uvDataList);

      if (this.doModelImage) {
        // compute the uv map data :

        // update uvMax according to UVCoverage Service (wavelength correction) :
        this.uvMax = uvDataCollection.getFirstUVData().getUvMax();

        final Rectangle2D.Double uvRect = new Rectangle2D.Double();
        uvRect.setFrameFromDiagonal(-this.uvMax, -this.uvMax, this.uvMax, this.uvMax);

        // get target from the first observation for consistency :
        final Target target = uvDataCollection.getFirstObservation().getTarget(this.targetName);

        if (target != null) {
          final List<Model> models = target.getModels();

          if (models != null && models.size() > 0) {

            // get observation target version :
            final int targetVersion = uvDataCollection.getVersion().getTargetVersion();

            // Check if the previously computed UV Map Data is still valid :
            if (this.currentUVMapData != null
                    && this.currentUVMapData.isValid(this.targetName, targetVersion, uvRect, this.imageMode, this.imageSize, this.colorModel)) {

              if (logger.isLoggable(Level.FINE)) {
                logger.fine("Reuse model image.");
              }

              // reuse computed UV Map Data :
              uvDataCollection.setUvMapData(this.currentUVMapData);
            } else {

              if (logger.isLoggable(Level.FINE)) {
                logger.fine("Computing model image ...");
              }

              // Compute Target Model for the UV coverage limits ONCE :
              final UVMapData uvMapData = ModelUVMapService.computeUVMap(models,
                      uvRect, null, null, this.imageMode, this.imageSize, this.colorModel);

              if (uvMapData != null) {
                // define target name and version :
                uvMapData.setTargetName(this.targetName);
                uvMapData.setTargetVersion(targetVersion);

                uvDataCollection.setUvMapData(uvMapData);
              }
            }
          }
        }
      }

      // fast interrupt :
      if (Thread.currentThread().isInterrupted()) {
        return null;
      }

      // merge results (warning, fits ...) :
      if (uvDataCollection.isSingle()) {
        final UVCoverageData uvData1 = uvDataCollection.getFirstUVData();

        uvDataCollection.setWarningContainer(uvData1.getWarningContainer());
        uvDataCollection.setOiFitsFile(uvData1.getOiFitsFile());

      } else {

        // merged warning container :
        final WarningContainer mergedWarningContainer = new WarningContainer();

        if (uvDataCollection.getFirstObservation().getWhen().isNightRestriction()) {
          mergedWarningContainer.addWarningMessage("Multiple configurations cannot be done in one night (night restrictions are only valid for "
                  + uvDataCollection.getFirstObservation().getWhen().getDate().toString() + ")");
        }

        for (UVCoverageData uvData : uvDataList) {
          mergedWarningContainer.addWarningMessages(uvData.getWarningContainer());
        }

        uvDataCollection.setWarningContainer(mergedWarningContainer);

        // No merged OIFITS still :
        uvDataCollection.setOiFitsFile(null);
      }

      if (logger.isLoggable(Level.INFO)) {
        logger.info("compute : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");
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

      // Note : the main observation can have changed while computation
      // i.e. the this.observation.getVersion() != this.getVersion()

      // Fire a warnings ready event :
      ObservationManager.getInstance().fireWarningsReady(uvDataCollection.getWarningContainer());

      // update the OIFits structure in the current observation :
      ObservationManager.getInstance().setOIFitsFile(uvDataCollection.getOiFitsFile());

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
   * Reset the plot in case of model exception
   */
  private void resetPlot() {

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

    // update the status bar :
    StatusBar.show("uv coverage done.");
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
      ChartUtils.addSubtitle(this.chart, "Source : " + chartData.getTargetName());

      if (observation.getWhen().isNightRestriction()) {
        // date :
        ChartUtils.addSubtitle(this.chart, "Day : " + observation.getWhen().getDate().toString());
      }

      // change the scaling factor ?
      setUvPlotScalingFactor(MEGA_LAMBDA_SCALE);

      // computed data are valid :
      updateChart(chartData);

      // update the background image :
      if (uvMapData == null) {
        updateUVMap(null);
      } else {
        updateUVMap(uvMapData.getUvMap());
      }

      // update theme at end :
      ChartUtilities.applyCurrentTheme(this.chart);

      // update the status bar :
      StatusBar.show("uv coverage done.");
    }
  }

  /**
   * Process the zoom event to refresh the model UV map according to the new coordinates
   * @param ze zoom event
   */
  public void chartChanged(final ZoomEvent ze) {
    // check if the zoom changed :
    if (!ze.equals(this.lastZoomEvent)) {
      this.lastZoomEvent = ze;

      if (this.aJMMC != null) {
        this.xyPlot.getRenderer(0).removeAnnotations();
        this.aJMMC.setX(ze.getDomainUpperBound());
        this.aJMMC.setY(ze.getRangeLowerBound());

        this.xyPlot.getRenderer(0).addAnnotation(this.aJMMC, Layer.BACKGROUND);
      }

      if (this.getChartData() != null && this.getChartData().getUVMapData() != null) {
        // Update model uv map :

        final UVMapData uvMapData = this.getChartData().getUVMapData();

        // get target from observation for consistency :
        final Target target = this.getChartData().getFirstObservation().getTarget(this.getChartData().getTargetName());

        if (target != null) {
          final List<Model> models = target.getModels();

          if (models != null && models.size() > 0) {

            final Rectangle2D.Double uvRect = new Rectangle2D.Double();
            uvRect.setFrameFromDiagonal(
                    fromUVPlotScale(ze.getDomainLowerBound()), fromUVPlotScale(ze.getRangeLowerBound()),
                    fromUVPlotScale(ze.getDomainUpperBound()), fromUVPlotScale(ze.getRangeUpperBound()));

            // compute an approximated uv map from the reference UV Map :
            computeSubUVMap(uvMapData, uvRect);

            // visibility reference extrema :
            final Float refMin = uvMapData.getMin();
            final Float refMax = uvMapData.getMax();

            // model image options :
            final ImageMode imageMode = uvMapData.getImageMode();
            final int imageSize = uvMapData.getImageSize();
            final IndexColorModel colorModel = uvMapData.getColorModel();

            // update the status bar :
            StatusBar.show("computing uv map ...");

            // Create uv map task worker :
            // Cancel other tasks and execute this new task :

            new UVMapSwingWorker(this, models, uvRect,
                    refMin, refMax, imageMode, imageSize, colorModel).executeTask();
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

    /**
     * Hidden constructor
     *
     * @param uvPanel observability panel
     * @param models list of models to use
     * @param uvRect UV frequency area in rad-1
     * @param refMin minimum reference double value used only for sub images
     * @param refMax maximum reference double value used only for sub images
     * @param imageMode image mode (amplitude or phase)
     * @param imageSize number of pixels for both width and height of the generated image
     * @param colorModel color model to use
     */
    private UVMapSwingWorker(final UVCoveragePanel uvPanel, final List<Model> models,
                             final Rectangle2D.Double uvRect,
                             final Float refMin, final Float refMax,
                             final ImageMode imageMode, final int imageSize, final IndexColorModel colorModel) {
      super(AsproTaskRegistry.TASK_UV_MAP);
      this.uvPanel = uvPanel;
      this.models = models;
      this.uvRect = uvRect;
      this.refMin = refMin;
      this.refMax = refMax;
      this.imageMode = imageMode;
      this.imageSize = imageSize;
      this.colorModel = colorModel;
    }

    /**
     * Compute the UV Map data in background
     * This code is executed by a Worker thread (Not Swing EDT)
     * @return UV Map data
     */
    @Override
    public UVMapData computeInBackground() {
      // compute the uv map data :
      return ModelUVMapService.computeUVMap(
              this.models, this.uvRect, this.refMin, this.refMax, this.imageMode, this.imageSize, this.colorModel);
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

      // update the status bar :
      StatusBar.show("uv map done.");
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
   */
  private void computeSubUVMap(final UVMapData uvMapData, final Rectangle2D.Double uvRect) {
    final int imageSize = uvMapData.getImageSize();

    // uv area reference :
    final Rectangle2D.Double uvRectRef = uvMapData.getUvRect();

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

    // crop a small sub image waiting for the correct model to be computed :
    final Image subUVMap = uvMapData.getUvMap().getSubimage(x, y, w, h);

    // update the background image :
    updateUVMap(subUVMap);
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
   */
  private void updateChart(final ObservationCollectionUVData chartData) {

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

    // define bounds to the uv maximum value (before setDataset) :
    final double boxSize = toUVPlotScale(chartData.getFirstUVData().getUvMax());
    this.xyPlot.defineBounds(boxSize);

    // set the main data set :
    this.xyPlot.setDataset(dataset);

    // annotation JMMC (moving position) :
    this.xyPlot.getRenderer(0).removeAnnotations();
    if (this.aJMMC == null) {
      this.aJMMC = ChartUtils.createXYTextAnnotation(AsproConstants.JMMC_ANNOTATION, boxSize, -boxSize);
      this.aJMMC.setFont(ChartUtils.SMALL_TEXT_ANNOTATION_FONT);
      this.aJMMC.setTextAnchor(TextAnchor.BOTTOM_RIGHT);
      this.aJMMC.setPaint(Color.DARK_GRAY);
    } else {
      this.aJMMC.setX(boxSize);
      this.aJMMC.setY(-boxSize);
    }
    this.xyPlot.getRenderer(0).addAnnotation(this.aJMMC, Layer.BACKGROUND);
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
            xySeries.add(x1, y1);
            xySeries.add(x2, y2);

            // add an invalid point to break the line between the 2 segments :
            xySeries.add(Double.NaN, Double.NaN);

            // second symetric segment :
            xySeries.add(-x1, -y1);
            xySeries.add(-x2, -y2);

            // add an invalid point to break the line between the 2 segments :
            xySeries.add(Double.NaN, Double.NaN);

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

            xySeries.add(x, y);
          } // points

          // add an invalid point to break the line between the 2 segments :
          xySeries.add(Double.NaN, Double.NaN);

          // second symetric ellipse line :
          for (int i = 0, size = uvBL.getNPoints(); i < size; i++) {
            x = toUVPlotScale(-u[i]);
            y = toUVPlotScale(-v[i]);

            xySeries.add(x, y);
          } // points

          // add an invalid point to break the line between the 2 segments :
          xySeries.add(Double.NaN, Double.NaN);
        } // BL
      }
    }
  }
  // Variables declaration - do not modify//GEN-BEGIN:variables
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

  /**
   * Handle the chart progress event to log the chart rendering delay
   * @param event chart progress event
   */
  public void chartProgress(final ChartProgressEvent event) {
    if (logger.isLoggable(Level.FINE)) {
      switch (event.getType()) {
        case ChartProgressEvent.DRAWING_STARTED:
          this.lastTime = System.nanoTime();
          break;
        case ChartProgressEvent.DRAWING_FINISHED:
          logger.fine("Drawing chart time : " + 1e-6d * (System.nanoTime() - this.lastTime) + " ms.");
          this.lastTime = 0l;
          break;
        default:
      }
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
    try {
      res = field.getFormatter().valueToString(value);
    } catch (ParseException pe) {
      logger.log(Level.SEVERE, "parsing exception", pe);
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
