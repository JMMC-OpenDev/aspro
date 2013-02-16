/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import com.jidesoft.swing.CheckBoxList;
import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.gui.action.AsproExportPDFAction;
import fr.jmmc.aspro.gui.chart.AsproChartUtils;
import fr.jmmc.aspro.gui.chart.EnhancedXYBoxAnnotation;
import fr.jmmc.aspro.gui.chart.ObservabilityPlotContext;
import fr.jmmc.aspro.gui.chart.SlidingXYPlotAdapter;
import fr.jmmc.aspro.gui.chart.SlidingXYPlotAdapter.State;
import fr.jmmc.aspro.gui.chart.XYDiamondAnnotation;
import fr.jmmc.aspro.gui.chart.XYTickAnnotation;
import fr.jmmc.aspro.gui.task.AsproTaskRegistry;
import fr.jmmc.aspro.gui.task.ObservationCollectionTaskSwingWorker;
import fr.jmmc.aspro.model.ObservationCollectionObsData;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.event.ObservationEvent;
import fr.jmmc.aspro.model.event.ObservationListener;
import fr.jmmc.aspro.model.event.TargetSelectionEvent;
import fr.jmmc.aspro.model.observability.DateTimeInterval;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.observability.StarObservabilityData;
import fr.jmmc.aspro.model.observability.SunTimeInterval;
import fr.jmmc.aspro.model.observability.SunTimeInterval.SunType;
import fr.jmmc.aspro.model.observability.TargetPositionDate;
import fr.jmmc.aspro.model.oi.ObservationCollection;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.service.ObservabilityService;
import fr.jmmc.aspro.service.pops.BestPopsEstimatorFactory.Algorithm;
import fr.jmmc.aspro.service.pops.Criteria;
import fr.jmmc.jmcs.gui.component.Disposable;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.ObjectUtils;
import fr.jmmc.jmcs.util.concurrent.InterruptedJobException;
import fr.jmmc.jmcs.util.concurrent.ParallelJobExecutor;
import fr.jmmc.oiexplorer.core.gui.PDFExportable;
import fr.jmmc.oiexplorer.core.gui.chart.BoundedDateAxis;
import fr.jmmc.oiexplorer.core.gui.chart.ChartUtils;
import fr.jmmc.oiexplorer.core.gui.chart.ColorPalette;
import fr.jmmc.oiexplorer.core.gui.chart.PDFOptions;
import fr.jmmc.oiexplorer.core.gui.chart.PDFOptions.Orientation;
import fr.jmmc.oiexplorer.core.gui.chart.PDFOptions.PageSize;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Insets;
import java.awt.Paint;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;
import java.util.Set;
import java.util.concurrent.Callable;
import javax.swing.BorderFactory;
import javax.swing.BoundedRangeModel;
import javax.swing.DefaultBoundedRangeModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.Timer;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartRenderingInfo;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.LegendItemCollection;
import org.jfree.chart.annotations.XYAnnotation;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.axis.TickUnitSource;
import org.jfree.chart.event.ChartProgressEvent;
import org.jfree.chart.event.ChartProgressListener;
import org.jfree.chart.plot.IntervalMarker;
import org.jfree.chart.plot.PlotRenderingInfo;
import org.jfree.chart.plot.ValueMarker;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYBarRenderer;
import org.jfree.data.gantt.Task;
import org.jfree.data.gantt.TaskSeries;
import org.jfree.data.gantt.TaskSeriesCollection;
import org.jfree.data.time.DateRange;
import org.jfree.ui.Layer;
import org.jfree.ui.RectangleAnchor;
import org.jfree.ui.RectangleInsets;
import org.jfree.ui.TextAnchor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This panel represents the observability plot
 * @author bourgesl
 */
public final class ObservabilityPanel extends javax.swing.JPanel implements ChartProgressListener,
        ObservationListener, Observer, PDFExportable, Disposable {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class _logger */
    private static final Logger logger = LoggerFactory.getLogger(ObservabilityPanel.class.getName());
    /** message indicating computations */
    private static final String MSG_COMPUTING = "computing observability ...";
    /** flag to log version checking */
    private final static boolean DEBUG_VERSIONS = false;
    /** background color corresponding to the DAY zone */
    public static final Color DAY_COLOR = new Color(224, 224, 224);
    /** background color corresponding to the TWILIGHT zone */
    public static final Color CIVIL_TWILIGHT_COLOR = new Color(206, 206, 206);
    /** background color corresponding to the TWILIGHT zone */
    public static final Color NAUTIC_TWILIGHT_COLOR = new Color(188, 188, 188);
    /** background color corresponding to the TWILIGHT zone */
    public static final Color ASTRO_TWILIGHT_COLOR = new Color(170, 170, 170);
    /** background color corresponding to the NIGHT zone */
    public static final Color NIGHT_COLOR = new Color(150, 150, 150);
    /** annotation rotation angle = 90 degrees */
    private static final double HALF_PI = Math.PI / 2d;
    /** milliseconds threshold to consider the date too close to date axis limits = 3 minutes */
    private static final long DATE_LIMIT_THRESHOLD = 3 * 60 * 1000;
    /** night margin in milliseconds = 15 minutes */
    private static final long NIGHT_MARGIN = 15 * 60 * 1000;
    /** hour angle tick units */
    private final static TickUnitSource HA_TICK_UNITS = ChartUtils.createHourAngleTickUnits();
    /** hour:minute units */
    private final static TickUnitSource HH_MM_TICK_UNITS = ChartUtils.createTimeTickUnits();
    /** max items printed before using A3 format */
    private final static int MAX_PRINTABLE_ITEMS_A4 = 10;
    /** max items printed before using A2 format */
    private final static int MAX_PRINTABLE_ITEMS_A3 = MAX_PRINTABLE_ITEMS_A4 * 4;
    /** max items displayed before scrolling */
    private final static int MAX_VIEW_ITEMS = MAX_PRINTABLE_ITEMS_A4;
    /** default item size to determine the max view items dynamically */
    private final static int ITEM_SIZE = 40;
    /** default timeline refresh period = 1 minutes */
    private static final int REFRESH_PERIOD = 60 * 1000;
    /* Filters */
    /** calibrator filter */
    private static final Filter CALIBRATOR_FILTER = new Filter("CalibratorFilter", "Hide calibrators") {
        @Override
        protected boolean apply(final Target target, final boolean calibrator, final StarObservabilityData so) {
            // if calibrator, filter item:
            return calibrator;
        }
    };
    /** unobservable filter */
    private static final Filter UNOBSERVABLE_FILTER = new Filter("UnobservableFilter", "Hide unobservable") {
        @Override
        protected boolean apply(final Target target, final boolean calibrator, final StarObservabilityData so) {
            if (so != null && so.getVisible().isEmpty()) {
                return true;
            }
            return false;
        }
    };

    /* default plot options */
    /** default value for the checkbox BaseLine Limits */
    private static final boolean DEFAULT_DO_BASELINE_LIMITS = false;
    /** default value for the checkbox Details */
    private static final boolean DEFAULT_DO_DETAILED_OUTPUT = false;

    /* members */
    /** preference singleton */
    private final Preferences myPreferences = Preferences.getInstance();
    /** preference: flag to center JD range arround midnight */
    private boolean prefCenterNight = myPreferences.getPreferenceAsBoolean(Preferences.CENTER_NIGHT);
    /** preference: twilight considered as night limit */
    private SunType prefTwilightNightLimit = myPreferences.getTwilightAsNightLimit();
    /** Best Pops algorithm */
    private Algorithm prefBestPopsAlgorithm = myPreferences.getBestPopsAlgorithm();
    /** optional Best Pops criteria on sigma */
    private Criteria prefBestPopEstimatorCriteriaSigma = myPreferences.getBestPopsCriteriaSigma();
    /** optional Best Pops criteria on average weight */
    private Criteria prefBestPopEstimatorCriteriaAverageWeight = myPreferences.getBestPopsCriteriaAverageWeight();
    /** jFreeChart instance */
    private JFreeChart chart;
    /** xy plot instance */
    private XYPlot xyPlot;
    /** JMMC annotation */
    private XYTextAnnotation aJMMC = null;
    /** sliding adapter to display a subset of targets */
    private SlidingXYPlotAdapter slidingXYPlotAdapter = null;
    /** plot rendering context */
    private final ObservabilityPlotContext renderContext = ObservabilityPlotContext.getInstance();
    /** height (in pixels) corresponding to non data area (title / axis / legend) */
    private int plotNonDataHeight = 100;
    /** 24h date formatter like in france */
    private final DateFormat timeFormatter = DateFormat.getTimeInstance(DateFormat.SHORT, Locale.FRANCE);
    /** timeline marker */
    private ValueMarker timeMarker = null;
    /** flag to enable / disable the automatic refresh of the plot when any swing component changes */
    private boolean doAutoRefresh = true;
    /** flag to indicate that the plot is rendered for PDF output */
    private boolean renderingPDF = false;
    /** backup state of slidingXYAdapter before PDF */
    private State stateBeforePDF = null;
    /** timeline refresh Swing timer */
    private final Timer timerTimeRefresh;
    /** current target name (selected target or given by scroller position) */
    private String currentTargetName = null;
    /** selected target name (selected target only) for highlight */
    private String selectedTargetName = null;

    /* plot data */
    /** chart data */
    private ObservationCollectionObsData chartData = null;
    /** sky calc instance */
    private AstroSkyCalc sc = null;
    /** night lower bound */
    private long nightLower = 0l;
    /** night upper bound */
    private long nightUpper = 0l;

    /* swing */
    /** chart panel */
    private ChartPanel chartPanel;
    /** panel dedicated to the scrollbar to define margins */
    private JPanel scrollerPanel = null;
    /** optional scrollbar to navigate through targets */
    private JScrollBar scroller = null;
    /** checkbox Scroll View */
    private JCheckBox jCheckBoxScrollView;
    /** time reference combo box */
    private JComboBox jComboTimeRef;
    /** checkbox night only*/
    private JCheckBox jCheckBoxNightOnly;
    /** checkbox BaseLine Limits */
    private JCheckBox jCheckBoxBaseLineLimits;
    /** checkbox Detailed output */
    private JCheckBox jCheckBoxDetailedOutput;
    /** checkbox list (JIDE) filters */
    private CheckBoxList jCheckBoxListFilters;

    /**
     * Constructor
     */
    public ObservabilityPanel() {
        super(new BorderLayout());
        initComponents();

        // Create the timeline refresh timer:
        this.timerTimeRefresh = new Timer(REFRESH_PERIOD, new ActionListener() {
            /**
             * Invoked when the timer action occurs.
             */
            @Override
            public void actionPerformed(final ActionEvent ae) {
                updateTimeMarker();
            }
        });
    }

    /**
     * Initialize the components (once)
     */
    private void initComponents() {

        this.chart = AsproChartUtils.createXYBarChart();
        this.xyPlot = (XYPlot) this.chart.getPlot();

        // create new JMMC annotation (moving position):
        this.aJMMC = ChartUtils.createJMMCAnnotation(AsproConstants.JMMC_ANNOTATION);

        // define sliding adapter :
        this.slidingXYPlotAdapter = new SlidingXYPlotAdapter(this.chart, this.xyPlot, MAX_VIEW_ITEMS, this.aJMMC);

        // add listener :
        this.chart.addProgressListener(this);
        this.chartPanel = ChartUtils.createChartPanel(this.chart, true);

        // intercept component resize events:
        this.chartPanel.addComponentListener(new PanelResizeAdapter());

        // zoom options :
        this.chartPanel.setDomainZoomable(true);
        // date axis :
        this.chartPanel.setRangeZoomable(true);
        // disable mouse wheel as it is already used when scrolling view:
        this.chartPanel.setMouseWheelEnabled(false);

        this.add(this.chartPanel, BorderLayout.CENTER);

        this.scroller = new JScrollBar(JScrollBar.VERTICAL, 0, 0, 0, 0);
        this.scroller.setName("scroller");

        this.scroller.getModel().addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(final ChangeEvent paramChangeEvent) {
                final DefaultBoundedRangeModel model = (DefaultBoundedRangeModel) paramChangeEvent.getSource();
                // update position and repaint the plot:
                slidingXYPlotAdapter.setPosition(model.getValue());

                // update current target:
                setCurrentTargetName(slidingXYPlotAdapter.getCurrentTargetName());
            }
        });

        // add the mouse wheel listener to the complete observability panel :
        this.addMouseWheelListener(new MouseWheelListener() {
            @Override
            public void mouseWheelMoved(final MouseWheelEvent e) {
                if (scroller.isEnabled()) {
                    logger.debug("mouseWheelMoved: {}", e);

                    final DefaultBoundedRangeModel model = (DefaultBoundedRangeModel) scroller.getModel();

                    final int clicks = e.getWheelRotation();
                    if (clicks != 0) {
                        // update value in min (0) / max (size - max viewed items) range:
                        model.setValue(model.getValue() + clicks);
                    }
                }
            }
        });

        // Use a panel to define custom margin arround the scroll bar:
        this.scrollerPanel = new JPanel(new BorderLayout());
        this.scrollerPanel.add(this.scroller);
        this.scrollerPanel.setBorder(BorderFactory.createEmptyBorder(20, 0, 20, 0));
        this.scrollerPanel.setBackground(Color.WHITE);

        this.add(this.scrollerPanel, BorderLayout.EAST);

        final JPanel panelOptions = new JPanel(new FlowLayout(FlowLayout.CENTER, 10, 1));

        panelOptions.add(new JLabel("Time:"));

        this.jComboTimeRef = new JComboBox(AsproConstants.TIME_CHOICES);
        this.jComboTimeRef.setName("jComboTimeRef");

        this.jComboTimeRef.setSelectedItem(this.myPreferences.getPreference(Preferences.TIME_REFERENCE));
        this.jComboTimeRef.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                refreshPlot();
            }
        });
        panelOptions.add(this.jComboTimeRef);

        this.jCheckBoxNightOnly = new JCheckBox("Night only");
        this.jCheckBoxNightOnly.setName("jCheckBoxNightOnly");

        this.jCheckBoxNightOnly.setSelected(myPreferences.getPreferenceAsBoolean(Preferences.ONLY_NIGHT));
        this.jCheckBoxNightOnly.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(final ItemEvent e) {
                if (getChartData() != null && jCheckBoxNightOnly.isEnabled()) {
                    if (e.getStateChange() == ItemEvent.SELECTED) {
                        // Update date axis = zoom on night bounds:
                        if (nightLower != 0d && nightUpper != 0d) {
                            updateDateAxisBounds(nightLower, nightUpper);
                        }
                    } else {
                        // full range:
                        final ObservabilityData obsData = getChartData().getFirstObsData();
                        updateDateAxisBounds(obsData.getDateMin().getTime(), obsData.getDateMax().getTime());
                    }
                }
            }
        });

        panelOptions.add(this.jCheckBoxNightOnly);

        this.jCheckBoxBaseLineLimits = new JCheckBox("BaseLine limits");
        this.jCheckBoxBaseLineLimits.setName("jCheckBoxBaseLineLimits");

        this.jCheckBoxBaseLineLimits.setSelected(DEFAULT_DO_BASELINE_LIMITS);
        this.jCheckBoxBaseLineLimits.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(final ItemEvent e) {
                final boolean doBaseLineLimits = e.getStateChange() == ItemEvent.SELECTED;

                // disable the automatic refresh :
                final boolean prevAutoRefresh = setAutoRefresh(false);
                try {
                    if (doBaseLineLimits) {
                        // force LST to compute correctly base line limits :
                        jComboTimeRef.setSelectedItem(AsproConstants.TIME_LST);
                        jCheckBoxDetailedOutput.setSelected(false);
                    } else {
                        // restore user preference :
                        jComboTimeRef.setSelectedItem(myPreferences.getPreference(Preferences.TIME_REFERENCE));
                    }

                    jComboTimeRef.setEnabled(!doBaseLineLimits);
                    jCheckBoxDetailedOutput.setEnabled(!doBaseLineLimits);

                } finally {
                    // restore the automatic refresh :
                    setAutoRefresh(prevAutoRefresh);
                }
                refreshPlot();
            }
        });

        panelOptions.add(this.jCheckBoxBaseLineLimits);

        this.jCheckBoxDetailedOutput = new JCheckBox("Details");
        this.jCheckBoxDetailedOutput.setName("jCheckBoxDetailedOutput");

        this.jCheckBoxDetailedOutput.setSelected(DEFAULT_DO_DETAILED_OUTPUT);
        this.jCheckBoxDetailedOutput.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(final ItemEvent e) {
                refreshPlot();
            }
        });

        panelOptions.add(this.jCheckBoxDetailedOutput);

        this.jCheckBoxListFilters = new CheckBoxList(new Object[]{CALIBRATOR_FILTER, UNOBSERVABLE_FILTER});
        this.jCheckBoxListFilters.setName("jCheckBoxListFilters");
        this.jCheckBoxListFilters.setVisibleRowCount(1); // 1 or 2 max

        this.jCheckBoxListFilters.getCheckBoxListSelectionModel().addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(final ListSelectionEvent e) {
                if (chartData != null) {
                    updatePlot(chartData);
                }
            }
        });

        panelOptions.add(new JSeparator(SwingConstants.VERTICAL));
        panelOptions.add(new JLabel("Filters:"));
        panelOptions.add(new JScrollPane(this.jCheckBoxListFilters));

        this.jCheckBoxScrollView = new JCheckBox("Scroll view");
        this.jCheckBoxScrollView.setName("jCheckBoxScrollView");

        this.jCheckBoxScrollView.setSelected(true);
        this.jCheckBoxScrollView.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(final ItemEvent e) {
                // update scrollbar state and repaint the plot:
                updateSliderProperties(false);
            }
        });

        panelOptions.add(new JSeparator(SwingConstants.VERTICAL));
        panelOptions.add(this.jCheckBoxScrollView);

        this.add(panelOptions, BorderLayout.PAGE_END);

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

        // disable timeline refresh timer:
        enableTimelineRefreshTimer(false);
    }

    /**
     * Listen to preferences changes
     * @param o Preferences
     * @param arg unused
     */
    @Override
    public void update(final Observable o, final Object arg) {
        logger.debug("Preferences updated on : {}", this);

        boolean changed = false;

        // disable the automatic refresh :
        final boolean prevAutoRefresh = this.setAutoRefresh(false);
        try {
            final String timeRef = this.myPreferences.getPreference(Preferences.TIME_REFERENCE);
            if (!ObjectUtils.areEquals(this.jComboTimeRef.getSelectedItem(), timeRef)) {
                changed |= true;
                this.jComboTimeRef.setSelectedItem(timeRef);
            }

            final boolean centerNight = myPreferences.getPreferenceAsBoolean(Preferences.CENTER_NIGHT);
            if (centerNight != this.prefCenterNight) {
                changed |= true;
                this.prefCenterNight = centerNight;
            }

            final SunType twilightNightLimit = myPreferences.getTwilightAsNightLimit();
            if (twilightNightLimit != this.prefTwilightNightLimit) {
                changed |= true;
                this.prefTwilightNightLimit = twilightNightLimit;
            }

            final boolean nightOnly = myPreferences.getPreferenceAsBoolean(Preferences.ONLY_NIGHT);
            if (nightOnly != this.jCheckBoxNightOnly.isSelected()) {
                changed |= true;
                this.jCheckBoxNightOnly.setSelected(nightOnly);
            }

            if (chartData != null
                    && !chartData.getFirstObservation().getInterferometerConfiguration().getInterferometerConfiguration().getInterferometer().getPops().isEmpty()) {

                // Best Pops algorithm
                final Algorithm bestPopsAlgorithm = myPreferences.getBestPopsAlgorithm();
                if (bestPopsAlgorithm != this.prefBestPopsAlgorithm) {
                    changed |= true;
                    this.prefBestPopsAlgorithm = bestPopsAlgorithm;
                }

                final Criteria bestPopEstimatorCriteriaSigma = myPreferences.getBestPopsCriteriaSigma();
                if (bestPopEstimatorCriteriaSigma != this.prefBestPopEstimatorCriteriaSigma) {
                    changed |= true;
                    this.prefBestPopEstimatorCriteriaSigma = bestPopEstimatorCriteriaSigma;
                }

                final Criteria bestPopsCriteriaAverageWeight = myPreferences.getBestPopsCriteriaAverageWeight();
                if (bestPopsCriteriaAverageWeight != this.prefBestPopEstimatorCriteriaAverageWeight) {
                    changed |= true;
                    this.prefBestPopEstimatorCriteriaAverageWeight = bestPopsCriteriaAverageWeight;
                }
            }

        } finally {
            // restore the automatic refresh :
            this.setAutoRefresh(prevAutoRefresh);
        }

        if (changed) {
            refreshPlot();
        }
    }

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

            // flags used by the plot :
            final ObservabilityData obsData = this.getChartData().getFirstObsData();
            final boolean doBaseLineLimits = obsData.isDoBaseLineLimits();
            final boolean doDetailedOutput = obsData.isDoDetailedOutput();

            final StringBuilder sb = new StringBuilder(32);
            sb.append("OBS_");

            final String baseLine = this.getChartData().getDisplayConfigurations("_", true);

            if (doBaseLineLimits) {
                sb.append("LIMITS_");
                sb.append(this.getChartData().getInterferometerConfiguration(true));
                sb.append('_').append(baseLine);

            } else {
                if (doDetailedOutput) {
                    sb.append("DETAILS_");
                }
                sb.append(observation.getInstrumentConfiguration().getName());
                sb.append('_').append(baseLine);
                if (observation.getWhen().isNightRestriction()) {
                    sb.append('_');
                    sb.append(observation.getWhen().getDate().toString());
                }
            }
            sb.append('.').append(PDF_EXT);

            return sb.toString();
        }
        return null;
    }

    /**
     * Prepare the chart(s) before exporting them as a PDF document:
     * Performs layout and return PDF options
     * @return PDF options
     */
    public PDFOptions preparePDFExport() {
        // Enable the PDF rendering flag:
        this.renderingPDF = true;

        // Backup slidingXYPlotAdapter state before rendering PDF :
        this.stateBeforePDF = this.slidingXYPlotAdapter.backupState();

        // Render text even if do not fit in block size:
        this.renderContext.setHideAnnotationTooSmall(false);
        // Use smaller fonts (print):
        this.renderContext.setMinSizeFont(true);

        // update the time marker to disable it:
        updateTimeMarker();

        boolean useSubset = false;
        PDFOptions options = PDFOptions.DEFAULT_PDF_OPTIONS;

        if (this.getChartData() != null) {
            // baseline limits flag used by the plot :
            if (!this.getChartData().getFirstObsData().isDoBaseLineLimits()) {

                final int size = this.slidingXYPlotAdapter.getSize();

                if (logger.isDebugEnabled()) {
                    logger.debug("row count: {}", size);
                }

                if (this.slidingXYPlotAdapter.isUseSubset() && size > MAX_PRINTABLE_ITEMS_A3) {
                    // multi page mode
                    useSubset = true;
                    // update max items:
                    this.slidingXYPlotAdapter.setMaxViewItems(MAX_PRINTABLE_ITEMS_A3);

//                    options = new PDFOptions(PageSize.A2, Orientation.Portait);
                    options = new PDFOptions(PageSize.A3, Orientation.Portait, 1 + size / MAX_PRINTABLE_ITEMS_A3);

                } else if (size > MAX_PRINTABLE_ITEMS_A4) {
                    options = new PDFOptions(PageSize.A3, Orientation.Portait);
                }
            }
        }

        // Note: if scroll view is disabled, user can zoom on plot and export it as displayed (highlight is then present)
        // difficult to keep plot as displayed without hilight (requires plot update ie reset zoom ...)

        if (useSubset) {
            // disable highlight:
            this.slidingXYPlotAdapter.setSelectedPosition(-1);

            // start at the beginning:
            this.slidingXYPlotAdapter.setPosition(0);
        }

        // Adapt the chart to print all targets on 1 page or multiple pages:
        if (this.slidingXYPlotAdapter.isUseSubset() != useSubset) {
            this.slidingXYPlotAdapter.setUseSubset(useSubset);
        }

        return options;
    }

    /**
     * Return the chart to export on the given page index
     * @param pageIndex page index (1..n)
     * @return chart
     */
    public JFreeChart prepareChart(final int pageIndex) {
        if (logger.isDebugEnabled()) {
            logger.debug("prepareChart: page {}", pageIndex);
        }

        // If only one page: export as displayed

        if (pageIndex > 1) {
            final int maxViewItems = this.slidingXYPlotAdapter.getMaxViewItems();
            final int pos = (pageIndex - 1) * maxViewItems;

            // TODO: fix last page (number of items => empty space ?)
            if (pos + maxViewItems > this.slidingXYPlotAdapter.getSize()) {
                this.slidingXYPlotAdapter.setMaxViewItems(this.slidingXYPlotAdapter.getSize() - pos);
            }
            this.slidingXYPlotAdapter.setPosition(pos);
        }

        return this.chart;
    }

    /**
     * Callback indicating the chart was processed by the PDF engine
     */
    @Override
    public void postPDFExport() {
        // Disable the PDF rendering flag:
        this.renderingPDF = false;

        // Restore slidingXYPlotAdapter state after rendering PDF :
        this.slidingXYPlotAdapter.restoreState(this.stateBeforePDF);
        this.stateBeforePDF = null;

        // Reset hideTextDontFit:
        this.renderContext.setHideAnnotationTooSmall(ObservabilityPlotContext.DEFAULT_HIDE_TEXT_DONT_FIT);
        // Use larger fonts (display):
        this.renderContext.setMinSizeFont(false);

        // update the time marker to enable it:
        updateTimeMarker();
    }

    /**
     * This method is called by the SettingPanel when the selected tabbed panel is different from this
     * to disable the 'BaseLine Limits' checkbox in order to have correct results in the UV Coverage Panel.
     */
    void disableBaseLineLimits() {
        if (this.jCheckBoxBaseLineLimits.isSelected()) {
            // this will send a refresh plot event ...
            this.jCheckBoxBaseLineLimits.setSelected(false);
        }
    }

    /**
     * Update the UI widgets from the given loaded observation
     *
     * @param observation observation (unused)
     */
    private void onLoadObservation(final ObservationSetting observation) {
        if (logger.isDebugEnabled()) {
            logger.debug("onLoadObservation:\n{}", ObservationManager.toString(observation));
        }
        // disable the automatic refresh :
        final boolean prevAutoRefresh = this.setAutoRefresh(false);
        try {
            // restore user preference :
            this.jComboTimeRef.setSelectedItem(this.myPreferences.getPreference(Preferences.TIME_REFERENCE));

            this.jCheckBoxBaseLineLimits.setSelected(DEFAULT_DO_BASELINE_LIMITS);
            this.jCheckBoxDetailedOutput.setSelected(DEFAULT_DO_DETAILED_OUTPUT);

            // reset current target:
            setCurrentTargetName(null);

        } finally {
            // restore the automatic refresh :
            this.setAutoRefresh(prevAutoRefresh);
        }
    }

    /**
     * Handle the given event on the given observation =
     * compute observability data and refresh the plot
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
                    this.showSelectedTarget(((TargetSelectionEvent) event).getTarget());
                }
                break;
            case REFRESH:
                this.plot(event.getObservationCollection());
                break;
            default:
        }
        if (logger.isDebugEnabled()) {
            logger.debug("event [{}] process OUT", event.getType());
        }
    }

    /**
     * Refresh the plot when an UI widget changes that is not related to the observation.
     * Check the doAutoRefresh flag to avoid unwanted refresh (onLoadObservation)
     */
    private void refreshPlot() {
        if (this.doAutoRefresh) {
            logger.debug("refreshPlot");

            // use the latest observation collection used by computations :
            this.plot(ObservationManager.getInstance().getObservationCollection());
        }
    }

    /**
     * Plot the observability using a SwingWorker to do the computation in the background.
     * This code is executed by the Swing Event Dispatcher thread (EDT)
     * @param obsCollection observation collection to use
     */
    private void plot(final ObservationCollection obsCollection) {
        if (logger.isDebugEnabled()) {
            logger.debug("plot: {}", ObservationManager.toString(obsCollection));
        }

        final boolean isSingle = obsCollection.isSingle();

        // disable the automatic refresh :
        final boolean prevAutoRefresh = this.setAutoRefresh(false);
        try {
            // if multiple configurations, disable baseline limits and detailed output :
            if (!isSingle) {
                this.jCheckBoxBaseLineLimits.setSelected(false);
                this.jCheckBoxDetailedOutput.setSelected(false);
            }
            this.jCheckBoxBaseLineLimits.setEnabled(isSingle);
            this.jCheckBoxDetailedOutput.setEnabled(isSingle && !this.jCheckBoxBaseLineLimits.isSelected());

        } finally {
            // restore the automatic refresh :
            this.setAutoRefresh(prevAutoRefresh);
        }

        /* get plot options from swing components */

        // indicates if the timestamps are expressed in LST or in UTC:
        final boolean useLST = AsproConstants.TIME_LST.equals(this.jComboTimeRef.getSelectedItem());

        // flag to find baseline limits:
        final boolean doBaseLineLimits = this.jCheckBoxBaseLineLimits.isSelected();

        // flag to produce detailed output with all BL / horizon / rise intervals per target:
        final boolean doDetailedOutput = this.jCheckBoxDetailedOutput.isSelected();

        // update the status bar :
        StatusBar.show(MSG_COMPUTING);

        // Create Observability task worker
        // Cancel other tasks and execute this new task :
        new ObservabilitySwingWorker(this,
                obsCollection, useLST, doDetailedOutput, doBaseLineLimits,
                this.prefCenterNight, this.prefTwilightNightLimit, this.prefBestPopsAlgorithm,
                this.prefBestPopEstimatorCriteriaSigma, this.prefBestPopEstimatorCriteriaAverageWeight).executeTask();
    }

    /**
     * TaskSwingWorker child class to compute observability data and refresh the observability plot
     */
    private final static class ObservabilitySwingWorker extends ObservationCollectionTaskSwingWorker<List<ObservabilityData>> {

        /** Jmcs Parallel Job executor */
        private static final ParallelJobExecutor jobExecutor = ParallelJobExecutor.getInstance();

        /* members */
        /** observability panel used for refreshUI callback */
        private final ObservabilityPanel obsPanel;
        /** indicates if the timestamps are expressed in LST or in UTC */
        private final boolean useLST;
        /** flag to find baseline limits */
        private final boolean doBaseLineLimits;
        /** flag to produce detailed output with all BL / horizon / rise intervals per target */
        private final boolean doDetailedOutput;
        /** flag to center the plot arround midnight */
        private final boolean doCenterMidnight;
        /** twilight considered as night limit */
        private final SunType twilightNightLimit;
        /** Best Pops algorithm */
        private final Algorithm bestPopsAlgorithm;
        /** optional Best Pops criteria on sigma */
        private final Criteria bestPopEstimatorCriteriaSigma;
        /** optional Best Pops criteria on average weight */
        private final Criteria bestPopEstimatorCriteriaAverageWeight;

        /**
         * Hidden constructor
         *
         * @param obsPanel observability panel
         * @param obsCollection observation collection to use
         * @param useLST indicates if the timestamps are expressed in LST or in UTC
         * @param doDetailedOutput flag to produce detailed output with all BL / horizon / rise intervals per target
         * @param doBaseLineLimits flag to find base line limits
         * @param doCenterMidnight flag to center JD range arround midnight
         * @param twilightNightLimit twilight considered as night limit
         * @param bestPopsAlgorithm Best Pops algorithm
         * @param bestPopEstimatorCriteriaSigma optional Best Pops criteria on sigma
         * @param bestPopEstimatorCriteriaAverageWeight optional Best Pops criteria on average weight
         */
        private ObservabilitySwingWorker(final ObservabilityPanel obsPanel, final ObservationCollection obsCollection,
                final boolean useLST, final boolean doDetailedOutput, final boolean doBaseLineLimits,
                final boolean doCenterMidnight, final SunType twilightNightLimit,
                final Algorithm bestPopsAlgorithm,
                final Criteria bestPopEstimatorCriteriaSigma, final Criteria bestPopEstimatorCriteriaAverageWeight) {

            // get current observation version :
            super(AsproTaskRegistry.TASK_OBSERVABILITY, obsCollection);
            this.obsPanel = obsPanel;
            this.useLST = useLST;
            this.doDetailedOutput = doDetailedOutput;
            this.doBaseLineLimits = doBaseLineLimits;
            this.doCenterMidnight = doCenterMidnight;
            this.twilightNightLimit = twilightNightLimit;
            this.bestPopsAlgorithm = bestPopsAlgorithm;
            this.bestPopEstimatorCriteriaSigma = bestPopEstimatorCriteriaSigma;
            this.bestPopEstimatorCriteriaAverageWeight = bestPopEstimatorCriteriaAverageWeight;
        }

        /**
         * Compute the observability data in background
         * This code is executed by a Worker thread (Not Swing EDT)
         * @return observability data
         */
        @Override
        public List<ObservabilityData> computeInBackground() {

            try {
                // Start the computations :
                final long start = System.nanoTime();

                final List<ObservationSetting> observations = getObservationCollection().getObservations();

                final int nObs = observations.size();

                // computation tasks = 1 job per observation (work stealing):
                final Callable<?>[] jobs = new Callable<?>[nObs];

                // create observation tasks:
                for (int i = 0; i < nObs; i++) {
                    final ObservationSetting observation = observations.get(i);

                    jobs[i] = new Callable<ObservabilityData>() {
                        /**
                         * Called by the ParallelJobExecutor to perform task computation
                         */
                        @Override
                        public ObservabilityData call() {

                            // compute the observability data :
                            final ObservabilityData obsData = new ObservabilityService(observation, useLST, doDetailedOutput, doBaseLineLimits,
                                    doCenterMidnight, twilightNightLimit, bestPopsAlgorithm,
                                    bestPopEstimatorCriteriaSigma, bestPopEstimatorCriteriaAverageWeight).compute();

                            // fast interrupt:
                            if (Thread.currentThread().isInterrupted()) {
                                return null;
                            }

                            return obsData;
                        }
                    };
                }

                // Is is better to use or not best Pops parallelism (use more threads ?) but compromise because best pops is only a part of observability (moon...)
                // so fully parallel may be better !

                final boolean useThreads = nObs > 1;

                // execute jobs in parallel:
                @SuppressWarnings("unchecked")
                final List<ObservabilityData> obsDataList = (List<ObservabilityData>) jobExecutor.forkAndJoin("ObservabilitySwingWorker.computeInBackground", jobs, useThreads);

                // fast interrupt :
                if (Thread.currentThread().isInterrupted()) {
                    return null;
                }

                _logger.info("compute[ObservabilityData]: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

                return obsDataList;

            } catch (InterruptedJobException ije) {
                _logger.debug("compute[ObservabilityData]: interrupted: ", ije);
            }
            return null;
        }

        /**
         * Refresh the plot using the computed observability data.
         * This code is executed by the Swing Event Dispatcher thread (EDT)
         * @param obsDataList computed observability data
         */
        @Override
        public void refreshUI(final List<ObservabilityData> obsDataList) {

            final ObservationCollection taskObsCollection = this.getObservationCollection();

            // TODO : if do baseline limits => compute also observation observability ...

            // skip baseline limits case :
            if (!this.doBaseLineLimits) {
                // Fire the event ObservabilityDone and call UVCoveragePanel to refresh the UV Coverage plot :

                final ObservationManager om = ObservationManager.getInstance();

                // use the latest observation for computations to check versions :
                final ObservationCollection lastObsCollection = om.getObservationCollection();

                if (taskObsCollection.getVersion().isSameMainVersion(lastObsCollection.getVersion())) {
                    if (_logger.isDebugEnabled()) {
                        _logger.debug("refreshUI: main version equals: {} :: {}", taskObsCollection.getVersion(), lastObsCollection.getVersion());
                    }
                    if (DEBUG_VERSIONS) {
                        _logger.warn("refreshUI: main version equals: {} :: {}", taskObsCollection.getVersion(), lastObsCollection.getVersion());
                    }

                    // use latest observation collection to see possible UV widget changes :
                    // note: observability data is also valid for any UV version :

                    om.fireObservabilityDone(lastObsCollection, obsDataList);

                } else {
                    if (_logger.isDebugEnabled()) {
                        _logger.debug("refreshUI: main version mismatch: {} :: {}", taskObsCollection.getVersion(), lastObsCollection.getVersion());
                    }
                    if (DEBUG_VERSIONS) {
                        _logger.warn("refreshUI: main version mismatch: {} :: {}", taskObsCollection.getVersion(), lastObsCollection.getVersion());
                    }

                    // use consistent observation and observability data :
                    // next iteration will see changes ...
                    om.fireObservabilityDone(taskObsCollection, obsDataList);
                }
            }

            // Refresh the GUI using coherent data :
            this.obsPanel.updatePlot(new ObservationCollectionObsData(taskObsCollection, obsDataList));
        }
    }

    /**
     * Return the chart data
     * @return chart data
     */
    private ObservationCollectionObsData getChartData() {
        return this.chartData;
    }

    /**
     * Define the chart data
     * @param chartData chart data
     */
    private void setChartData(final ObservationCollectionObsData chartData) {
        this.chartData = chartData;

        // Get AstroSkyCalc instance :
        this.sc = this.chartData.getFirstObsData().getDateCalc();
    }

    /**
     * Refresh the plot using chart data.
     * This code is executed by the Swing Event Dispatcher thread (EDT)
     *
     * @param chartData chart data
     */
    private void updatePlot(final ObservationCollectionObsData chartData) {
        // memorize chart data (used by export PDF) :
        setChartData(chartData);

        final ObservationSetting observation = chartData.getFirstObservation();
        final ObservabilityData obsData = chartData.getFirstObsData();

        final boolean useLST = obsData.isUseLST();
        final boolean doBaseLineLimits = obsData.isDoBaseLineLimits();

        // Enable or disable the 'Night only' option:
        this.jCheckBoxNightOnly.setEnabled(!doBaseLineLimits && observation.getWhen().isNightRestriction());

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

            if (!doBaseLineLimits && (observation.getWhen().isNightRestriction() || !useLST)) {
                // date and moon FLI :
                ChartUtils.addSubtitle(this.chart, "Day: " + observation.getWhen().getDate().toString()
                        + (observation.getWhen().isNightRestriction()
                        ? " - Moon = " + (int) Math.round(obsData.getMoonIllumPercent()) + "%"
                        : ""));
            }

            final String dateAxisLabel;
            if (doBaseLineLimits) {
                dateAxisLabel = AsproConstants.TIME_HA;
            } else {
                if (useLST) {
                    dateAxisLabel = AsproConstants.TIME_LST;
                } else {
                    dateAxisLabel = AsproConstants.TIME_UTC;
                }
            }

            // define the date axis (bounds and current range):
            updateDateAxis(dateAxisLabel, obsData.getDateMin(), obsData.getDateMax(), doBaseLineLimits);

            // only valid for single observation :
            updateSunMarkers(obsData.getSunIntervals(), obsData.getDateMin(), obsData.getDateMax());

            // update the time marker:
            updateTimeMarker();

            // computed data are valid :
            updateChart(observation.getDisplayTargets(),
                    observation.getOrphanCalibrators(),
                    observation.getOrCreateTargetUserInfos(),
                    chartData,
                    obsData.getDateMin(), obsData.getDateMax(),
                    doBaseLineLimits);

        } finally {
            // restore chart & plot notifications:
            this.xyPlot.setNotify(true);
            this.chart.setNotify(true);
        }

        // update the status bar:
        StatusBar.showIfPrevious(MSG_COMPUTING, "observability done.");
    }

    /**
     * Update the datasets and the symbol axis given the star observability data
     * @param displayTargets list of display targets
     * @param orphanCalibrators set of orphan calibrators
     * @param targetUserInfos target user informations
     * @param chartData chart data
     * @param min lower date of the plot
     * @param max upper date of the plot
     * @param doBaseLineLimits flag to plot baseline limits
     */
    @SuppressWarnings("unchecked")
    private void updateChart(final List<Target> displayTargets,
            final Set<Target> orphanCalibrators,
            final TargetUserInformations targetUserInfos,
            final ObservationCollectionObsData chartData,
            final Date min, final Date max,
            final boolean doBaseLineLimits) {

        final ColorPalette palette = ColorPalette.getDefaultColorPalette();

        final XYBarRenderer xyBarRenderer = (XYBarRenderer) this.xyPlot.getRenderer();

        final boolean single = chartData.isSingle();
        final int obsLen = chartData.size();

        final Map<Integer, List<XYAnnotation>> annotations;
        // Target list :
        final List<Target> targets;
        if (doBaseLineLimits) {
            // Use generated targets for baseline limits :
            targets = chartData.getFirstObsData().getTargets();
            annotations = null;
        } else {
            // Use display target to get correct ordering and calibrator associations :
            targets = displayTargets;
            annotations = new HashMap<Integer, List<XYAnnotation>>(obsLen * targets.size());
        }

        // Get filters:
        boolean hasFilters = (doBaseLineLimits) ? false : true; // disable filters for baseline limits
        Filter[] selectedFilters = null;

        if (hasFilters) {
            selectedFilters = getSelectedFilters();

            hasFilters = (selectedFilters.length != 0);

            if (hasFilters) {
                if (logger.isDebugEnabled()) {
                    logger.debug("selectedFilters: {}", Arrays.toString(selectedFilters));
                }
            }
        }

        // Prepare chart information used by SlidingXYPlotAdapter :
        final int initialSize = obsLen * targets.size();
        final TaskSeriesCollection taskSeriesCollection = new TaskSeriesCollection();
        final List<String> symbolList = new ArrayList<String>(initialSize);
        final List<Color> colorList = new ArrayList<Color>(initialSize);
        final Map<String, Paint> legendItems = new LinkedHashMap<String, Paint>(initialSize);

        // for tooltip information:
        final List<Target> targetList = (doBaseLineLimits) ? null : new ArrayList<Target>(initialSize);
        final List<String> labelList = new ArrayList<String>(initialSize);
        final List<StarObservabilityData> soTargetList = new ArrayList<StarObservabilityData>(initialSize);

        String name;
        TaskSeries taskSeries;
        Task task;
        int colorIndex;
        boolean calibrator;
        Integer pos;
        int n = 0;
        String legendLabel;
        Color paint;

        final StringBuilder sb = new StringBuilder(4);

        ObservabilityData obsData;
        // current StarObservabilityData used in loops :
        List<StarObservabilityData> soList;

        boolean isCalibrator;
        boolean filtered;

        // Iterate over objects targets :
        for (Target target : targets) {

            isCalibrator = targetUserInfos.isCalibrator(target);

            if (hasFilters) {
                filtered = false;
                for (Filter filter : selectedFilters) {
                    if (filter.apply(target, isCalibrator, null)) {
                        filtered = true;
                        break;
                    }
                }
                if (filtered) {
                    // skip target
                    continue;
                }
            }

            // Iterate over Observability data (multi conf) :
            for (int c = 0; c < obsLen; c++) {
                obsData = chartData.getObsDataList().get(c);

                // get StarObservabilityData results :
                soList = obsData.getMapStarVisibilities().get(target.getName());

                if (soList != null) {

                    // Iterate over StarObservabilityData :
                    for (StarObservabilityData so : soList) {

                        if (hasFilters) {
                            filtered = false;
                            for (Filter filter : selectedFilters) {
                                if (filter.apply(target, isCalibrator, so)) {
                                    filtered = true;
                                    break;
                                }
                            }
                            if (filtered) {
                                // skip target
                                continue;
                            }
                        }

                        if (doBaseLineLimits) {
                            name = so.getTargetName();
                            // note: targetlist is null in such case
                        } else {
                            // TODO: compute name (outside loops)
                            // display name :
                            name = targetUserInfos.getTargetDisplayName(target);

                            // add information character:
                            if (targetUserInfos.getDescription(target) != null) {
                                name += " [\u2139]";
                            }

                            targetList.add(target);
                        }
                        symbolList.add(name);

                        // add tooltip info:
                        soTargetList.add(so);

                        // use the target name as the name of the serie :
                        taskSeries = new TaskSeries(name);
                        taskSeries.setNotify(false);

                        int j = 1;
                        for (DateTimeInterval interval : so.getVisible()) {
                            task = new Task(sb.append('T').append(j).toString(), interval.getStartDate(), interval.getEndDate());
                            sb.setLength(0); // recycle buffer
                            taskSeries.add(task);
                            j++;
                        }

                        taskSeriesCollection.add(taskSeries);

                        // color :
                        colorIndex = so.getType();
                        calibrator = false;

                        if (!doBaseLineLimits && colorIndex == StarObservabilityData.TYPE_STAR && isCalibrator) {
                            // use different color for calibrators :
                            colorIndex = StarObservabilityData.TYPE_CALIBRATOR;
                            calibrator = true;
                        }

                        if (single) {
                            // 1 color per StarObservabilityData type (star, calibrator, rise_set, horizon, baselines ...) :
                            // note : uses so.getInfo() to get baseline ...
                            legendLabel = so.getLegendLabel(colorIndex);
                        } else {
                            legendLabel = chartData.getConfigurationNames().get(c);

                            // 1 color per configuration (incompatible with Detailed output : too complex i.e. unreadable) :
                            colorIndex = c;
                        }

                        // display differently orphan calibrators:
                        if (calibrator && orphanCalibrators.contains(target)) {
                            legendLabel = "Orphan calibrator";
                            paint = Color.ORANGE;
                        } else {
                            paint = palette.getColor(colorIndex);
                        }
                        colorList.add(paint);

                        if (doBaseLineLimits) {
                            // no tooltip:
                            labelList.add(null);
                        } else {
                            // add legend to tooltip:
                            labelList.add(legendLabel);

                            // define legend :
                            legendItems.put(legendLabel, paint);

                            // add the Annotations :
                            // 24h date formatter like in france :

                            pos = NumberUtils.valueOf(n);

                            // transit annotation :
                            if (so.getType() == StarObservabilityData.TYPE_STAR) {
                                addAnnotation(annotations, pos, new XYDiamondAnnotation(n, so.getTransitDate().getTime()));

                                // azimuth and elevation ticks:
                                XYTickAnnotation a;
                                for (TargetPositionDate ed : so.getTargetPositions().values()) {
                                    if (checkDateAxisLimits(ed.getDate(), min, max)) {
                                        a = AsproChartUtils.createXYTickAnnotation(Integer.toString(ed.getAzimuth()), n, ed.getDate().getTime(), -HALF_PI);
                                        a.setTextAnchor(TextAnchor.BOTTOM_CENTER);
                                        a.setRotationAnchor(TextAnchor.BOTTOM_CENTER);
                                        addAnnotation(annotations, pos, a);

                                        a = AsproChartUtils.createXYTickAnnotation(Integer.toString(ed.getElevation()), n, ed.getDate().getTime(), HALF_PI);
                                        a.setTextAnchor(TextAnchor.TOP_CENTER);
                                        a.setRotationAnchor(TextAnchor.TOP_CENTER);
                                        addAnnotation(annotations, pos, a);
                                    }
                                }
                            }

                            // time annotations at range boundaries:
                            for (DateTimeInterval interval : so.getVisible()) {
                                if (checkDateAxisLimits(interval.getStartDate(), min, max)) {
                                    final XYTextAnnotation aStart = AsproChartUtils.createFitXYTextAnnotation(this.timeFormatter.format(interval.getStartDate()), n, interval.getStartDate().getTime());
                                    aStart.setRotationAngle(HALF_PI);
                                    addAnnotation(annotations, pos, aStart);
                                }

                                if (checkDateAxisLimits(interval.getEndDate(), min, max)) {
                                    final XYTextAnnotation aEnd = AsproChartUtils.createFitXYTextAnnotation(this.timeFormatter.format(interval.getEndDate()), n, interval.getEndDate().getTime());
                                    aEnd.setRotationAngle(HALF_PI);
                                    addAnnotation(annotations, pos, aEnd);
                                }
                            }

                            // Observable range limits without HA restrictions:
                            // TODO: rename WIND / MOON TOO:
                            if (so.getVisibleNoHaLimits() != null) {
                                final Paint fillPaint = new Color(paint.getRed(), paint.getGreen(), paint.getBlue(), 48); // 80% transparent
                                // final Paint fillPaint = ImageUtils.createHatchedTexturePaint(15, new Color(0, true), paint, new BasicStroke(2.0f));

                                for (DateTimeInterval interval : so.getVisibleNoHaLimits()) {
                                    addAnnotation(annotations, pos,
                                            new EnhancedXYBoxAnnotation(n, interval.getStartDate().getTime(), n, interval.getEndDate().getTime(),
                                            ChartUtils.DOTTED_STROKE, Color.BLACK, fillPaint));
                                }
                            }
                        }
                        n++;
                    } // loop on StarObservabilityData
                } // soList !== null
            } // loop on Observability data (multi conf)
        } // loop on targets (display targets or baseline limits)

        // update plot data :
        this.slidingXYPlotAdapter.setData(taskSeriesCollection, symbolList, colorList, annotations, targetList, labelList, soTargetList,
                !chartData.isDoBaseLineLimits() && chartData.getFirstObservation().getWhen().isNightRestriction());

        // force a plot refresh:
        this.updateSliderProperties(true);

        // define fixed Legend :
        final LegendItemCollection legendCollection = new LegendItemCollection();
        if (!legendItems.isEmpty()) {
            for (Map.Entry<String, Paint> legend : legendItems.entrySet()) {
                legendCollection.add(ChartUtils.createLegendItem(xyBarRenderer, legend.getKey(), legend.getValue()));
            }
        }
        this.xyPlot.setFixedLegendItems(legendCollection);
    }

    /**
     * Update the scrollbar model and define the SlidingXYPlotAdapter.useSubset flag to force a plot refresh
     * @param forceRefresh to repaint the plot anyway
     */
    private void updateSliderProperties(final boolean forceRefresh) {
        // baseline limits flag used by the plot :
        final boolean doBaseLineLimits =
                (this.getChartData() != null) ? this.getChartData().getFirstObsData().isDoBaseLineLimits() : false;

        final int size = this.slidingXYPlotAdapter.getSize();

        final BoundedRangeModel rangeModel = this.scroller.getModel();

        final boolean useSubset;
        if (doBaseLineLimits || !this.jCheckBoxScrollView.isSelected() || size <= this.slidingXYPlotAdapter.getMaxViewItems()) {
            // disable scrollbar:
            rangeModel.setRangeProperties(0, 0, 0, 0, false);
            this.scroller.setEnabled(false);
            useSubset = false;
        } else {
            // refresh scrollbar position:
            final int lastPos = rangeModel.getValue();

            // find target to find first target occurence in slidingXYPlotAdapter:
            final int pos =
                    (this.currentTargetName != null && this.currentTargetName.equals(slidingXYPlotAdapter.getTargetName(lastPos))) ? lastPos
                    : slidingXYPlotAdapter.findTargetPosition(this.currentTargetName);

            if (logger.isDebugEnabled()) {
                logger.debug("target position: {}", pos);
            }

            // refresh scrollbar maximum value:
            rangeModel.setRangeProperties((pos != -1) ? pos : lastPos, 0, 0, size - this.slidingXYPlotAdapter.getMaxViewItems(), false);
            this.scroller.setEnabled(true);
            useSubset = true;
        }

        // update selected target (target may have different position now):
        updateSelectedTarget();

        // repaint the plot:
        if (forceRefresh || useSubset != this.slidingXYPlotAdapter.isUseSubset()) {
            this.slidingXYPlotAdapter.setUseSubset(useSubset);
        }
    }

    /**
     * Update the scrollbar model to show the given target
     * @param target 
     */
    private void showSelectedTarget(final Target target) {
        final String targetName = target.getName();

        if (logger.isDebugEnabled()) {
            logger.debug("showSelectedTarget: {}", targetName);
        }

        // scroller is enabled if useSubset mode:
        if (this.scroller.isEnabled()) {
            // find target to find first target occurence in slidingXYPlotAdapter:
            final int pos = slidingXYPlotAdapter.findTargetPosition(targetName);

            if (logger.isDebugEnabled()) {
                logger.debug("showSelectedTarget: target position: {}", pos);
            }

            if (pos != -1) {
                // update scroller model to update slidingXYPlotAdapter (may redraw plot):
                this.scroller.getModel().setValue(pos);
            }
        }

        // update current target:
        setCurrentTargetName(targetName);

        // update selected target:
        this.selectedTargetName = targetName;
        updateSelectedTarget();
    }

    /**
     * Update the selected position (target) on the plot
     */
    private void updateSelectedTarget() {
        // find target to find first target occurence in slidingXYPlotAdapter:
        final int pos = this.slidingXYPlotAdapter.findTargetPosition(this.selectedTargetName);

        if (logger.isDebugEnabled()) {
            logger.debug("updateSelectedTarget: target position: {}", pos);
        }
        // note: pos can be -1 if not found (so no highlited target):
        this.slidingXYPlotAdapter.setSelectedPosition(pos);
    }

    /**
     * Add the given annotation to the map of annotations keyed by position
     * @param annotations map of annotations keyed by position
     * @param pos position
     * @param annotation annotation to add
     */
    private void addAnnotation(final Map<Integer, List<XYAnnotation>> annotations, final Integer pos, final XYAnnotation annotation) {
        List<XYAnnotation> list = annotations.get(pos);
        if (list == null) {
            list = new ArrayList<XYAnnotation>(10);
            annotations.put(pos, list);
        }
        list.add(annotation);
    }

    /**
     * Check if the given date is too close to date axis limits
     * @param date date to check
     * @param min min date of date axis
     * @param max max date of date axis
     * @return true if the given date is NOT close to date axis limits
     */
    private boolean checkDateAxisLimits(final Date date, final Date min, final Date max) {
        // if date is too close to date min:
        if (date.getTime() - min.getTime() < DATE_LIMIT_THRESHOLD) {
            return false;
        }
        // if date is too close to date min:
        if (max.getTime() - date.getTime() < DATE_LIMIT_THRESHOLD) {
            return false;
        }
        return true;
    }

    /**
     * Update the date axis i.e. the horizontal axis
     * @param label axis label with units
     * @param from starting date
     * @param to ending date
     * @param doBaseLineLimits flag to plot baseline limits
     */
    private void updateDateAxis(final String label, final Date from, final Date to, final boolean doBaseLineLimits) {
        // change the Range axis (horizontal) :
        final BoundedDateAxis dateAxis = new BoundedDateAxis(label);

        if (doBaseLineLimits) {
            dateAxis.setStandardTickUnits(HA_TICK_UNITS);
        } else {
            dateAxis.setStandardTickUnits(HH_MM_TICK_UNITS);
        }
        dateAxis.setTickLabelInsets(ChartUtils.TICK_LABEL_INSETS);

        this.xyPlot.setRangeAxis(dateAxis);

        // use the range [0;24]:
        updateDateAxisBounds(from.getTime(), to.getTime());
    }

    /**
     * Update the data axis range i.e. zoom on date range
     * @param lower lower value in milliseconds
     * @param upper upper value in milliseconds
     */
    private void updateDateAxisBounds(final long lower, final long upper) {
        final BoundedDateAxis dateAxis = (BoundedDateAxis) this.xyPlot.getRangeAxis();
        // add a margin of 1 ms :
        dateAxis.setBounds(new DateRange(lower - 1l, upper + 1l));
        dateAxis.setRange(lower - 1l, upper + 1l);
    }

    /**
     * Update the sun zones : twilight and night zones
     * @param intervals sun time intervals
     * @param from starting date
     * @param to ending date
     */
    private void updateSunMarkers(final List<SunTimeInterval> intervals, final Date from, final Date to) {
        // remove Markers :
        this.xyPlot.clearRangeMarkers();

        // reset the night boundaries:
        this.nightLower = 0L;
        this.nightUpper = 0L;

        // add the Markers :
        if (intervals != null) {

            long nightMin = Long.MAX_VALUE;
            long nightMax = Long.MIN_VALUE;

            long startTime, endTime;

            Color col;

            for (SunTimeInterval interval : intervals) {
                switch (interval.getType()) {
                    case Day:
                        col = DAY_COLOR;
                        break;
                    case Night:
                        col = NIGHT_COLOR;
                        break;
                    case CivilTwilight:
                        col = CIVIL_TWILIGHT_COLOR;
                        break;
                    case NauticalTwilight:
                        col = NAUTIC_TWILIGHT_COLOR;
                        break;
                    case AstronomicalTwilight:
                        col = ASTRO_TWILIGHT_COLOR;
                        break;
                    default:
                        col = Color.RED;
                        break;
                }

                startTime = interval.getStartDate().getTime();
                endTime = interval.getEndDate().getTime();

                // Update night limits:
                if (col != DAY_COLOR) {
                    if (nightMin > startTime) {
                        nightMin = startTime;
                    }
                    if (nightMax < endTime) {
                        nightMax = endTime;
                    }
                }

                // force Alpha to 1.0 to avoid PDF rendering problems (alpha layer ordering) :
                this.xyPlot.addRangeMarker(new IntervalMarker(startTime, endTime, col, ChartUtils.THIN_STROKE, null, null, 1f), Layer.BACKGROUND);
            }

            // Add 15 minutes margin:
            nightMin -= NIGHT_MARGIN;
            nightMax += NIGHT_MARGIN;

            if (nightMin < from.getTime()) {
                nightMin = from.getTime();
            }

            if (nightMax > to.getTime()) {
                nightMax = to.getTime();
            }

            // update the night boundaries:
            this.nightLower = nightMin;
            this.nightUpper = nightMax;

            if (logger.isDebugEnabled()) {
                logger.debug("nightLower: {}", new Date(this.nightLower));
                logger.debug("nightUpper: {}", new Date(this.nightUpper));
            }

            if (this.jCheckBoxNightOnly.isEnabled() && this.jCheckBoxNightOnly.isSelected()) {
                // Update date axis = zoom on night bounds:
                updateDateAxisBounds(nightLower, nightUpper);
            }
        }
    }

    /**
     * Create or update the timeline marker (red)
     */
    private void updateTimeMarker() {

        // remove time marker anyway:
        if (this.timeMarker != null) {
            this.xyPlot.removeRangeMarker(this.timeMarker, Layer.BACKGROUND);
        }

        boolean enableTimer = false;

        // do not export time marker in PDF output:
        if (!this.renderingPDF) {

            final ObservabilityData obsData = chartData.getFirstObsData();

            final boolean doBaseLineLimits = obsData.isDoBaseLineLimits();

            if (!doBaseLineLimits) {

                if (this.sc != null) {
                    final boolean useLST = obsData.isUseLST();

                    // Get jd of current date/time:
                    final double jd = this.sc.getCurrentJd();

                    // check if the current jd is within the good night:
                    if (jd >= obsData.getJdMin() && jd <= obsData.getJdMax()) {
                        // enable timeline refresh timer:
                        enableTimer = true;

                        // convert JD to LST/UT date/time:
                        final Calendar cal = this.sc.toCalendar(jd, useLST);

                        // roll +/- 1 day to be within plot range:
                        final Date now = convertCalendarToDate(cal, obsData.getDateMin(), obsData.getDateMax());

                        if (logger.isDebugEnabled()) {
                            logger.debug("timeMarker set at: {}", now);
                        }

                        final double timeValue = now.getTime();

                        if (timeMarker == null) {
                            // force Alpha to 1.0 to avoid PDF rendering problems (alpha layer ordering) :
                            this.timeMarker = new ValueMarker(timeValue, Color.RED, ChartUtils.THIN_STROKE, Color.GRAY, ChartUtils.THIN_STROKE, 1.0f);
                            this.timeMarker.setLabelFont(ChartUtils.DEFAULT_TEXT_SMALL_FONT);
                            this.timeMarker.setLabelPaint(Color.RED);
                            this.timeMarker.setLabelOffset(new RectangleInsets(1d, 0d, 1d, 0d));
                            this.timeMarker.setLabelAnchor(RectangleAnchor.TOP);
                        } else {
                            this.timeMarker.setValue(timeValue);
                        }
                        // update displayed time:
                        this.timeMarker.setLabel(this.timeFormatter.format(now));

                        this.xyPlot.addRangeMarker(this.timeMarker, Layer.BACKGROUND);
                    }
                }
            }
        }
        // anyway enable or disable timer:
        enableTimelineRefreshTimer(enableTimer);
    }

    /**
     * Convert the given calendar to a date within LST/UT range [0;24]
     *
     * @param cal date to convert
     * @param min lower date of plot
     * @param max upper date of plot
     * @return date
     */
    private Date convertCalendarToDate(final Calendar cal, final Date min, final Date max) {

        // Note: use Calendar.roll to only fix date field

        if (cal.getTimeInMillis() >= min.getTime()) {

            if (cal.getTimeInMillis() > max.getTime()) {
                // after date max :

                // return [cal - 1 day]
                cal.roll(Calendar.DATE, false);
            }

        } else {
            // before date min:

            // return [cal + 1 day]
            cal.roll(Calendar.DATE, true);
        }
        return cal.getTime();
    }
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
        this.aJMMC.setY(this.xyPlot.getRangeAxis().getUpperBound()); // upper bound instead of other plots

        if (event.getType() == ChartProgressEvent.DRAWING_STARTED) {
            if (isTimelineEnabled()) {
                // set time marker label anchor:
                final BoundedDateAxis dateAxis = (BoundedDateAxis) this.xyPlot.getRangeAxis();

                double left = this.timeMarker.getValue() - dateAxis.getRange().getLowerBound();
                if (left < 0d) {
                    left = 0d;
                }

                double right = dateAxis.getRange().getUpperBound() - this.timeMarker.getValue();
                if (right < 0d) {
                    right = 0d;
                }

                final TextAnchor anchor = (left > right) ? TextAnchor.TOP_RIGHT : TextAnchor.TOP_LEFT;

                // check values to avoid refresh loop:
                if (anchor != this.timeMarker.getLabelTextAnchor()) {
                    // triggers MarkerChangeEvent which causes repaint loop:
                    this.timeMarker.setLabelTextAnchor(anchor);
                }
            }
        }

        // reset context state:
        this.renderContext.reset();

        // adjust scrollbar properties:
        if (event.getType() == ChartProgressEvent.DRAWING_FINISHED) {
            // get chart rendering area:
            final ChartRenderingInfo info = this.chartPanel.getChartRenderingInfo();
            final PlotRenderingInfo pinfo = info.getPlotInfo();

            if (logger.isDebugEnabled()) {
                logger.debug("chartArea = {}", info.getChartArea());
                logger.debug("dataArea  = {}", pinfo.getDataArea());
            }

            final int top = (int) Math.floor(pinfo.getDataArea().getY());
            final int bottom = (int) Math.floor(info.getChartArea().getHeight() - pinfo.getDataArea().getMaxY());

            final Insets current = this.scrollerPanel.getBorder().getBorderInsets(null);
            if (current.top != top || current.bottom != bottom) {
                // adjust scrollbar margins :
                this.scrollerPanel.setBorder(BorderFactory.createEmptyBorder(top, 0, bottom, 0));
            }

            this.plotNonDataHeight = top + bottom;
        }
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
     * Handle resize event to determine the maximum viewable items
     * @param width width of the chart panel
     * @param height height of the chart panel
     */
    private void handleResize(final int width, final int height) {
        // use non data height (title + axis + legend):
        final int maxViewItems = (height - this.plotNonDataHeight) / ITEM_SIZE;

        if (logger.isDebugEnabled()) {
            logger.debug("size = {} x {} => maxViewItems = {}", width, height, maxViewItems);
        }

        if (maxViewItems != this.slidingXYPlotAdapter.getMaxViewItems()) {
            // repaint the plot:
            this.slidingXYPlotAdapter.setMaxViewItems(maxViewItems);

            // update scrollbar state and repaint the plot:
            this.updateSliderProperties(true);
        }
    }

    /**
     * Custom Component adapter that handle componentResized events
     */
    private final class PanelResizeAdapter extends ComponentAdapter {

        /**
         * Protected Constructor
         */
        PanelResizeAdapter() {
            super();
        }

        /**
         * Invoked when the component's size changes.
         * This overriden method checks that the new size is greater than the minimal dimension
         * @param e event to process
         */
        @Override
        public void componentResized(final ComponentEvent e) {
            final Dimension d = e.getComponent().getSize();
            handleResize(d.width, d.height);
        }
    }

    /**
     * Return true if the timeline (timer) is enabled
     * @return true if the timeline (timer) is enabled
     */
    private boolean isTimelineEnabled() {
        return this.timerTimeRefresh.isRunning();
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
        } else {
            if (this.timerTimeRefresh.isRunning()) {
                logger.debug("Stopping timer: {}", this.timerTimeRefresh);

                this.timerTimeRefresh.stop();
            }
        }
    }

    /**
     * Update the current target name (selected target or given by scroller position)
     * @param targetName target name
     */
    private void setCurrentTargetName(final String targetName) {
        this.currentTargetName = targetName;
    }

    /**
     * Abstract Filter observability results
     */
    private static class Filter {

        /** filter name */
        private final String name;
        /** filter label (JList) */
        private final String label;

        /**
         * Filter constructor
         * @param name filter name
         * @param label filter label
         */
        Filter(final String name, final String label) {
            this.name = name;
            this.label = label;
        }

        /**
         * Return the filter name
         * @return filter name
         */
        public final String getName() {
            return name;
        }

        /**
         * Return the filter label
         * @return filter label
         */
        public final String getLabel() {
            return label;
        }

        /**
         * Return the filter label (JList)
         * @return filter label
         */
        @Override
        public final String toString() {
            return label;
        }

        /**
         * Apply the filter on the given target, its calibrator flag and star observability data
         * @param target target instance
         * @param calibrator calibrator flag
         * @param so star observability data
         * @return true if that item should be filtered; false otherwise
         */
        protected boolean apply(final Target target, final boolean calibrator, final StarObservabilityData so) {
            // default; do not filter
            return false;
        }
    }

    /**
     * Get selected filters (ie enabled)
     * @return Filter array
     */
    private Filter[] getSelectedFilters() {
        final Object[] selectedValues = jCheckBoxListFilters.getCheckBoxListSelectedValues();

        final int len = selectedValues.length;

        final Filter[] filters = new Filter[len];

        for (int i = 0; i < len; i++) {
            filters[i] = (Filter) selectedValues[i];
        }

        return filters;
    }
}
