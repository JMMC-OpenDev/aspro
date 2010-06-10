/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityPanel.java,v 1.35 2010-06-10 08:54:06 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.34  2010/06/09 12:51:09  bourgesl
 * new interface PDFExportable to define a standard method performPDFAction() that use ExportPDFAction to export the chart to PDF
 *
 * Revision 1.33  2010/06/08 14:48:39  bourgesl
 * moved pdf button against the left side
 *
 * Revision 1.32  2010/06/08 12:32:31  bourgesl
 * javadoc + pdf button moved to left side
 *
 * Revision 1.31  2010/05/11 09:49:28  bourgesl
 * plot duration use nanoseconds()
 *
 * Revision 1.30  2010/05/07 11:35:31  bourgesl
 * detail mode always available
 *
 * Revision 1.29  2010/04/08 14:06:06  bourgesl
 * javadoc
 *
 * Revision 1.28  2010/04/02 14:40:39  bourgesl
 * added elevation data and transit date
 *
 * Revision 1.27  2010/04/02 10:05:08  bourgesl
 * minor visual changes
 *
 * Revision 1.26  2010/02/18 15:52:38  bourgesl
 * added parameter argument validation with an user message
 *
 * Revision 1.25  2010/02/03 16:07:49  bourgesl
 * refactoring to use the custom swing worker executor
 * when zomming uv map is computed asynchronously
 *
 * Revision 1.24  2010/02/03 09:48:18  bourgesl
 * minor chart style corrections
 *
 * Revision 1.23  2010/01/22 13:17:20  bourgesl
 * change color association to plots
 *
 * Revision 1.22  2010/01/21 16:39:24  bourgesl
 * smaller margins
 *
 * Revision 1.21  2010/01/20 16:18:38  bourgesl
 * observation form refactoring
 *
 * Revision 1.20  2010/01/19 13:20:35  bourgesl
 * no message
 *
 * Revision 1.19  2010/01/14 17:03:06  bourgesl
 * No more gradient paint + smaller bar width
 *
 * Revision 1.18  2010/01/13 16:12:31  bourgesl
 * added export to PDF button
 *
 * Revision 1.17  2010/01/12 16:54:19  bourgesl
 * added PoPs in title + several changes on charts
 *
 * Revision 1.16  2010/01/08 16:51:17  bourgesl
 * initial uv coverage
 *
 * Revision 1.15  2010/01/05 17:18:56  bourgesl
 * syntax changes
 *
 * Revision 1.14  2009/12/16 16:47:24  bourgesl
 * comments
 *
 * Revision 1.13  2009/12/08 11:30:35  bourgesl
 * when an observation is loaded, reset plot options to defaults
 *
 * Revision 1.12  2009/12/07 15:18:00  bourgesl
 * Load observation action now refreshes the observation form completely
 *
 * Revision 1.11  2009/12/04 15:38:27  bourgesl
 * Added Save action in the menu bar
 *
 * Revision 1.10  2009/12/02 17:23:51  bourgesl
 * fixed several bugs on pop finder + refactoring
 *
 * Revision 1.9  2009/11/27 16:38:17  bourgesl
 * added minElev to GUI + fixed horizon profiles
 *
 * Revision 1.8  2009/11/26 17:04:11  bourgesl
 * added observability plots options (night/detail / UTC/LST)
 * added base line limits
 *
 * Revision 1.7  2009/11/25 17:14:32  bourgesl
 * fixed bugs on HA limits + merge JD intervals
 *
 * Revision 1.6  2009/11/20 16:55:47  bourgesl
 * Added Beam / Delay Line definition
 * ObservabilityService is stateless to simplify coding
 *
 * Revision 1.5  2009/11/20 10:17:02  mella
 * force the use of the swingworker backport
 *
 * Revision 1.4  2009/11/17 17:00:28  bourgesl
 * chosen instrument configuration propagated to observation
 *
 * Revision 1.3  2009/11/16 14:47:46  bourgesl
 * determine the hour angle for a target over a min elevation to get the simple observability
 *
 * Revision 1.2  2009/11/05 12:59:39  bourgesl
 * first simple source observability (only min elevation condition)
 *
 * Revision 1.1  2009/11/03 16:57:55  bourgesl
 * added observability plot with LST/UTC support containing only day/night/twilight zones
 *
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.gui.action.ExportPDFAction;
import fr.jmmc.aspro.gui.chart.ChartUtils;
import fr.jmmc.aspro.gui.chart.XYDiamondAnnotation;
import fr.jmmc.aspro.gui.chart.XYTickAnnotation;
import fr.jmmc.aspro.gui.util.ColorPalette;
import fr.jmmc.aspro.gui.util.SwingWorkerExecutor;
import fr.jmmc.aspro.model.observability.DateTimeInterval;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.ObservationListener;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.observability.ElevationDate;
import fr.jmmc.aspro.model.observability.StarObservabilityData;
import fr.jmmc.aspro.model.observability.SunTimeInterval;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Pop;
import fr.jmmc.aspro.service.ObservabilityService;
import fr.jmmc.mcs.gui.FeedbackReport;
import fr.jmmc.mcs.gui.StatusBar;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.text.DateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import org.jdesktop.swingworker.SwingWorker;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.axis.DateAxis;
import org.jfree.chart.axis.SymbolAxis;
import org.jfree.chart.event.ChartProgressEvent;
import org.jfree.chart.event.ChartProgressListener;
import org.jfree.chart.plot.IntervalMarker;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYBarRenderer;
import org.jfree.data.gantt.Task;
import org.jfree.data.gantt.TaskSeries;
import org.jfree.data.gantt.TaskSeriesCollection;
import org.jfree.data.gantt.XYTaskDataset;
import org.jfree.ui.Layer;
import org.jfree.ui.TextAnchor;

/**
 * This panel represents the observability plot
 * @author bourgesl
 */
public class ObservabilityPanel extends javax.swing.JPanel implements ChartProgressListener,
        ObservationListener, PDFExportable {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.ObservabilityPanel";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** background color corresponding to the DAY zone */
  public static final Color DAY_COLOR = new Color(224, 224, 224);
  /** background color corresponding to the TWILIGHT zone */
  public static final Color TWILIGHT_COLOR = new Color(192, 192, 192);
  /** background color corresponding to the NIGHT zone */
  public static final Color NIGHT_COLOR = new Color(128, 128, 128);

  /* time references */
  /** LST time reference */
  private static final String TIME_LST = "LST";
  /** UTC time reference */
  private static final String TIME_UTC = "UTC";

  /* default plot options */
  /** default value for the checkbox BaseLine Limits */
  private static final boolean DEFAULT_DO_BASELINE_LIMITS = false;
  /** default value for the checkbox Details */
  private static final boolean DEFAULT_DO_DETAILED_OUTPUT = false;

  /* members */
  /** jFreeChart instance */
  private JFreeChart localJFreeChart;
  /** xy plot instance */
  private XYPlot localXYPlot;
  /* swing */
  /** chart panel */
  private ChartPanel chartPanel;
  /** time reference combo box */
  private JComboBox jComboTimeRef;
  /** checkbox BaseLine Limits */
  private JCheckBox jCheckBoxBaseLineLimits;
  /** checkbox Detailed output */
  private JCheckBox jCheckBoxDetailedOutput;
  /** pdf export button */
  private JButton jButtonPDF;
  /** flag to enable / disable the automatic refresh of the plot when any swing component changes */
  private boolean doAutoRefresh = true;

  /**
   * Constructor
   */
  public ObservabilityPanel() {
    super(new BorderLayout());
    initComponents();

    // register this as an observation listener :
    ObservationManager.getInstance().register(this);
  }

  /**
   * Initialize the components (once)
   */
  private void initComponents() {

    this.localJFreeChart = ChartUtils.createXYBarChart();
    this.localXYPlot = (XYPlot) this.localJFreeChart.getPlot();

    // add listener :
    this.localJFreeChart.addProgressListener(this);

    this.chartPanel = new ChartPanel(this.localJFreeChart,
            600, 400, /* prefered size */
            300, 200, /* minimum size before scaling */
            1900, 1200, /* maximum size before scaling */
            true, /* use buffer */
            false, /* properties */
            true, /* copy */
            true, /* save */
            true, /* print */
            false, /* zoom */
            false /* tooltips */);

    // zoom options :
    this.chartPanel.setDomainZoomable(false);
    this.chartPanel.setRangeZoomable(false);
    this.chartPanel.setMouseWheelEnabled(false);

    this.add(this.chartPanel, BorderLayout.CENTER);

    final JPanel panelBottom = new JPanel(new BorderLayout());

    // same generated code in UVCoveragePanel :
    this.jButtonPDF = new javax.swing.JButton();
    this.jButtonPDF.setIcon(new javax.swing.ImageIcon(getClass().getResource("/fr/jmmc/aspro/gui/icons/icon_pdf.gif"))); // NOI18N
    this.jButtonPDF.setMargin(new java.awt.Insets(0, 0, 0, 0));
    this.jButtonPDF.addActionListener(new java.awt.event.ActionListener() {

      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonPDFActionPerformed(evt);
      }
    });
    panelBottom.add(this.jButtonPDF, BorderLayout.WEST);

    final JPanel panelOptions = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 1));

    panelOptions.add(new JLabel("Time :"));

    this.jComboTimeRef = new JComboBox(new String[]{TIME_LST, TIME_UTC});
    this.jComboTimeRef.addActionListener(new ActionListener() {

      public void actionPerformed(final ActionEvent e) {
        refreshPlot();
      }
    });
    panelOptions.add(this.jComboTimeRef);

    this.jCheckBoxBaseLineLimits = new JCheckBox("BaseLine limits");
    this.jCheckBoxBaseLineLimits.setSelected(DEFAULT_DO_BASELINE_LIMITS);
    this.jCheckBoxBaseLineLimits.addItemListener(new ItemListener() {

      public void itemStateChanged(final ItemEvent e) {
        final boolean doBaseLineLimits = e.getStateChange() == ItemEvent.SELECTED;
        if (doBaseLineLimits) {
          jCheckBoxDetailedOutput.setSelected(false);
        }

        jCheckBoxDetailedOutput.setEnabled(!doBaseLineLimits);
        refreshPlot();
      }
    });

    panelOptions.add(this.jCheckBoxBaseLineLimits);

    this.jCheckBoxDetailedOutput = new JCheckBox("Details");
    this.jCheckBoxDetailedOutput.setSelected(DEFAULT_DO_DETAILED_OUTPUT);
    this.jCheckBoxDetailedOutput.addItemListener(new ItemListener() {

      public void itemStateChanged(final ItemEvent e) {
        refreshPlot();
      }
    });

    panelOptions.add(this.jCheckBoxDetailedOutput);

    panelBottom.add(panelOptions, BorderLayout.CENTER);

    this.add(panelBottom, BorderLayout.PAGE_END);
  }

  /**
   * Export the current chart as a PDF document
   * @param evt action event
   */
  private void jButtonPDFActionPerformed(java.awt.event.ActionEvent evt) {
    this.performPDFAction();
  }

  /**
   * Export the current chart as a PDF document
   */
  public void performPDFAction() {
    ExportPDFAction.exportPDF(this.localJFreeChart);
  }

  /**
   * This method is called by the SettingPanel when the selected tabbed panel is different from this
   * to disable the 'BaseLine Limits' checkbox in order to have correct results in the UV Coverage Panel.
   */
  protected void disableBaseLineLimits() {
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
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("onLoadObservation :\n" + ObservationManager.toString(observation));
    }
    try {
      // first disable the automatic refresh from field changes :
      this.doAutoRefresh = false;

      this.jComboTimeRef.setSelectedItem(TIME_LST);

      this.jCheckBoxBaseLineLimits.setSelected(DEFAULT_DO_BASELINE_LIMITS);
      this.jCheckBoxDetailedOutput.setSelected(DEFAULT_DO_DETAILED_OUTPUT);
    } finally {
      // restore the automatic refresh from field changes :
      this.doAutoRefresh = true;
    }
  }

  /**
   * Handle the given event on the given observation =
   * compute observability data and refresh the plot
   * @param type event type
   * @param observation observation
   */
  public void onProcess(final ObservationEventType type, final ObservationSetting observation) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("event [" + type + "] process IN");
    }

    switch (type) {
      case CHANGED:
        this.plot(observation);
        break;
      case LOADED:
        this.onLoadObservation(observation);
        break;
      default:
    }
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("event [" + type + "] process OUT");
    }
  }

  /**
   * Refresh the plot when an UI widget changes.
   * Check the doAutoRefresh flag to avoid unwanted refresh (onLoadObservation)
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
   * Plot the observability using a SwingWorker to do the computation in the background.
   * This code is executed by the Swing Event Dispatcher thread (EDT)
   * @param observation observation data to use
   */
  protected void plot(final ObservationSetting observation) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("plot : " + ObservationManager.toString(observation));
    }

    /* get plot options from swing components */

    /** indicates if the timestamps are expressed in LST or in UTC */
    final boolean useLST = TIME_LST.equals(this.jComboTimeRef.getSelectedItem());

    /** flag to find baseline limits */
    final boolean doBaseLineLimits = this.jCheckBoxBaseLineLimits.isSelected();

    /** flag to produce detailed output with all BL / horizon / rise intervals per target */
    final boolean doDetailedOutput = this.jCheckBoxDetailedOutput.isSelected();

    /*
     * Use the SwingWorker backport for Java 5 = swing-worker-1.2.jar (org.jdesktop.swingworker.SwingWorker)
     */
    final SwingWorker<ObservabilityData, Void> worker = new SwingWorker<ObservabilityData, Void>() {

      /**
       * Compute the observability data in background
       * @return observability data
       */
      @Override
      public ObservabilityData doInBackground() {
        logger.fine("SwingWorker[Observability].doInBackground : IN");

        ObservabilityData obsData = null;

        // first reset the observability data in the current observation :
        ObservationManager.getInstance().setObservabilityData(null);

        // compute the observability data with the UI parameters :
        obsData = new ObservabilityService(observation, useLST, doDetailedOutput, doBaseLineLimits).compute();

        if (isCancelled()) {
          logger.fine("SwingWorker[Observability].doInBackground : CANCELLED");
          // no result if task is cancelled :
          obsData = null;
        } else {
          logger.fine("SwingWorker[Observability].doInBackground : OUT");
        }
        return obsData;
      }

      /**
       * Refresh the plot using the computed observability data.
       * This code is executed by the Swing Event Dispatcher thread (EDT)
       */
      @Override
      public void done() {
        // check if the worker was cancelled :
        if (!isCancelled()) {
          logger.fine("SwingWorker[Observability].done : IN");
          try {
            // Get the computation results with all data necessary to draw the plot :
            final ObservabilityData obsData = get();

            if (obsData != null) {
              logger.fine("SwingWorker[Observability].done : refresh Chart");

              // update the observability data in the current observation :
              ObservationManager.getInstance().setObservabilityData(obsData);

              localJFreeChart.clearSubtitles();

              // title :
              final StringBuilder sb = new StringBuilder(observation.getInterferometerConfiguration().getName());
              sb.append(" - ").append(observation.getInstrumentConfiguration().getStations());

              if (obsData.getBestPops() != null) {
                sb.append(" + ");
                for (Pop pop : obsData.getBestPops().getPopList()) {
                  sb.append(pop.getName()).append(" ");
                }
              }

              ChartUtils.addSubtitle(localJFreeChart, sb.toString());

              if (observation.getWhen().isNightRestriction() || !useLST) {
                // date :
                ChartUtils.addSubtitle(localJFreeChart, "Day : " + observation.getWhen().getDate().toString());
              }

              // computed data are valid :
              updateChart(obsData.getStarVisibilities(), obsData.getDateMin(), obsData.getDateMax());

              updateDateAxis((useLST) ? "LST" : "UTC", obsData.getDateMin(), obsData.getDateMax());

              updateSunMarkers(obsData.getSunIntervals());

              // tick color :
              localXYPlot.getRangeAxis().setTickMarkPaint(Color.BLACK);
              localXYPlot.getDomainAxis().setTickMarkPaint(Color.BLACK);

              // update theme at end :
              ChartUtilities.applyCurrentTheme(localJFreeChart);
            }

          } catch (InterruptedException ignore) {
          } catch (ExecutionException ee) {
            logger.log(Level.SEVERE, "Error : ", ee);
            new FeedbackReport(null, true, (Exception) ee.getCause());
          }

          // update the status bar :
          StatusBar.show("observability done.");

          logger.fine("SwingWorker[Observability].done : OUT");
        }
      }
    };

    // update the status bar :
    StatusBar.show("computing observability ... (please wait, this may take a while)");

    // Cancel other uv map task and execute this new uv map task :
    SwingWorkerExecutor.getInstance().execute("Observability", worker);
  }

  /**
   * Update the datasets and the symbol axis given the star observability data
   * @param starVis star observability data
   * @param min lower date of the plot
   * @param max upper date of the plot
   */
  private void updateChart(final List<StarObservabilityData> starVis, final Date min, final Date max) {
    final ColorPalette palette = ColorPalette.getDefaultColorPalette();

    // renderer :
    final XYBarRenderer renderer = (XYBarRenderer) this.localXYPlot.getRenderer();

    // reset colors :
    renderer.clearSeriesPaints(false);
    // side effect with chart theme :
    renderer.setAutoPopulateSeriesPaint(false);

    final String[] targetNames = new String[starVis.size()];
    final TaskSeriesCollection localTaskSeriesCollection = new TaskSeriesCollection();

    int n = 0;
    int j;
    String name;
    TaskSeries taskSeries;
    Task task;

    for (StarObservabilityData so : starVis) {
      name = so.getName();
      // use the target name as the name of the serie :
      targetNames[n] = name;
      taskSeries = new TaskSeries(name);
      taskSeries.setNotify(false);

      j = 1;

      for (DateTimeInterval interval : so.getVisible()) {
        task = new Task("T" + j, interval.getStartDate(), interval.getEndDate());
        taskSeries.add(task);
        j++;
      }

      taskSeries.setNotify(true);
      localTaskSeriesCollection.add(taskSeries);

      // color :
      renderer.setSeriesPaint(n, palette.getColor(so.getType()), false);

      n++;
    }

    // set the main data set :
    final XYTaskDataset dataset = new XYTaskDataset(localTaskSeriesCollection);

    // adjust bar width :
    if (targetNames.length > 1) {
      dataset.setSeriesWidth(0.5d);
    } else {
      dataset.setSeriesWidth(0.25d);
    }

    this.localXYPlot.setDataset(dataset);

    // change the Domain axis (vertical) :
    final SymbolAxis localSymbolAxis = new SymbolAxis(null, targetNames);
    localSymbolAxis.setInverted(true);
    localSymbolAxis.setGridBandsVisible(false);
    localSymbolAxis.setAutoRange(false);
    if (targetNames.length > 0) {
      localSymbolAxis.setRange(-0.5d, targetNames.length - 0.5d);
    }
    this.localXYPlot.setDomainAxis(localSymbolAxis);

    // remove Annotations :
    renderer.removeAnnotations();

    if (!this.jCheckBoxBaseLineLimits.isSelected()) {
      // add the Annotations :
      // 24h date formatter like in france :
      final DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT, Locale.FRANCE);

      n = 0;
      for (StarObservabilityData so : starVis) {

        // transit annotation :
        if (so.getType() == StarObservabilityData.TYPE_STAR) {
          renderer.addAnnotation(new XYDiamondAnnotation(n, so.getTransitDate().getTime(), 10, 10));

          for (ElevationDate ed : so.getElevations()) {
            renderer.addAnnotation(new XYTickAnnotation(Integer.toString(ed.getElevation()) + "°", n, ed.getDate().getTime(), Math.PI / 2d));
          }
        }

        for (DateTimeInterval interval : so.getVisible()) {

          if (!interval.getStartDate().equals(min)) {
            final XYTextAnnotation aStart = new XYTextAnnotation(df.format(interval.getStartDate()), n, interval.getStartDate().getTime());
            aStart.setTextAnchor(TextAnchor.BASELINE_CENTER);
            aStart.setPaint(Color.BLACK);
            aStart.setRotationAngle(Math.PI / 2);
            renderer.addAnnotation(aStart);
          }

          if (!interval.getEndDate().equals(max)) {
            final XYTextAnnotation aEnd = new XYTextAnnotation(df.format(interval.getEndDate()), n, interval.getEndDate().getTime());
            aEnd.setTextAnchor(TextAnchor.BASELINE_CENTER);
            aEnd.setPaint(Color.BLACK);
            aEnd.setRotationAngle(Math.PI / 2);
            renderer.addAnnotation(aEnd);
          }
        }

        n++;
      }
    }
  }

  /**
   * Update the date axis i.e. the horizontal axis
   * @param label axis label with units
   * @param from starting date
   * @param to ending date
   */
  private void updateDateAxis(final String label, final Date from, final Date to) {
    // change the Range axis (horizontal) :
    final DateAxis dateAxis = new DateAxis(label);
    dateAxis.setAutoRange(false);
    dateAxis.setRange(from.getTime(), to.getTime());
    this.localXYPlot.setRangeAxis(dateAxis);
  }

  /**
   * Update the sun zones : twilight and night zones
   * @param intervals sun time intervals
   */
  private void updateSunMarkers(final List<SunTimeInterval> intervals) {
    // remove Markers :
    this.localXYPlot.clearRangeMarkers();

    // add the Markers :
    if (intervals != null) {
      Color col;
      IntervalMarker localIntervalMarker;
      for (SunTimeInterval interval : intervals) {
        switch (interval.getType()) {
          case Day:
            col = DAY_COLOR;
            break;
          case Night:
            col = NIGHT_COLOR;
            break;
          default:
          case Twilight:
            col = TWILIGHT_COLOR;
            break;
        }
        localIntervalMarker = new IntervalMarker(interval.getStartDate().getTime(),
                interval.getEndDate().getTime(), col);

        // force Alpha to 1.0 to avoid PDF rendering problems (alpha layer ordering) :
        localIntervalMarker.setAlpha(1.0f);

        this.localXYPlot.addRangeMarker(localIntervalMarker, Layer.BACKGROUND);
      }
    }
  }
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
}
