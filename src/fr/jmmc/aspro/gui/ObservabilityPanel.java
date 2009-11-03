/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityPanel.java,v 1.1 2009-11-03 16:57:55 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 *
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.model.ObservabilityData;
import fr.jmmc.aspro.model.ObservationListener;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.SunTimeInterval;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.service.ObservabilityService;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.annotations.XYPointerAnnotation;
import org.jfree.chart.axis.DateAxis;
import org.jfree.chart.axis.SymbolAxis;
import org.jfree.chart.event.ChartProgressEvent;
import org.jfree.chart.event.ChartProgressListener;
import org.jfree.chart.plot.IntervalMarker;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYBarRenderer;
import org.jfree.data.gantt.Task;
import org.jfree.data.gantt.TaskSeries;
import org.jfree.data.gantt.TaskSeriesCollection;
import org.jfree.data.gantt.XYTaskDataset;
import org.jfree.data.time.Day;
import org.jfree.data.time.Hour;
import org.jfree.data.xy.IntervalXYDataset;
import org.jfree.ui.Layer;
import org.jfree.ui.RectangleAnchor;
import org.jfree.ui.TextAnchor;

/**
 * This panel represents the observability plot
 * @author bourgesl
 */
public class ObservabilityPanel extends javax.swing.JPanel implements ChartProgressListener, ObservationListener {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.ObservabilityPanel";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /* members */
  /** observation manager */
  private ObservationManager om = ObservationManager.getInstance();
  /** chart panel */
  private ChartPanel chartPanel;
  /** jFreeChart instance */
  private JFreeChart localJFreeChart;
  /** xy plot instance */
  private XYPlot localXYPlot;

  /**
   * Constructor
   */
  public ObservabilityPanel() {
    super(new BorderLayout());
    initComponents();

    // register this as an observation listener :
    om.register(this);
  }

  /**
   * Initialize the components (once)
   */
  private void initComponents() {

    this.localJFreeChart = createChart();
    this.localXYPlot = (XYPlot) localJFreeChart.getPlot();

    // add listener :
    this.localJFreeChart.addProgressListener(this);

    // disable all zoom functions :
    this.chartPanel = new ChartPanel(this.localJFreeChart, false, true, false, false, false);
    this.chartPanel.setPreferredSize(new Dimension(800, 600));
    this.chartPanel.setDomainZoomable(false);
    this.chartPanel.setRangeZoomable(false);

    this.add(chartPanel);
  }

  /**
   * Create the basic XYBarChart
   * @return jFreeChart instance
   */
  private static JFreeChart createChart() {
    final JFreeChart localJFreeChart = ChartFactory.createXYBarChart("Source Observability", null, false, null, null, PlotOrientation.HORIZONTAL, false, false, false);

    final XYPlot localXYPlot = (XYPlot) localJFreeChart.getPlot();
    localXYPlot.setDomainPannable(false);
    localXYPlot.setRangePannable(false);

    localXYPlot.getDomainAxis().setVisible(false);
    localXYPlot.getRangeAxis().setVisible(false);

    final XYBarRenderer localXYBarRenderer = (XYBarRenderer) localXYPlot.getRenderer();
    localXYBarRenderer.setUseYInterval(true);

    ChartUtilities.applyCurrentTheme(localJFreeChart);
    return localJFreeChart;
  }

  /**
   * OnChange implementation to compute observability data and refresh the plot
   * @param observation updated observation
   */
  public void onChange(final ObservationSetting observation) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("onChange occured : " + observation);
    }

    // Use a swing worker to compute the data :

    // Get the computation results with all data necessary to produce the plot :
    // -

    boolean useLst = true;
    double minElev = 20d;

    final ObservabilityData data = ObservabilityService.calcObservability(observation, useLst, minElev);

    updateChart(createDataset());

    updateDateAxis((useLst) ? "L.S.T" : "U.T.C", data.getDateMin(), data.getDateMax());

    updateSunMarkers(data.getSunIntervals());

    // update theme :
    ChartUtilities.applyCurrentTheme(this.localJFreeChart);
  }

  private static IntervalXYDataset createDataset() {
    final TaskSeriesCollection localTaskSeriesCollection = new TaskSeriesCollection();

    final TaskSeries localTaskSeries1 = new TaskSeries("Star 1");

//    Task localTask1 = new Task("Write Proposal", date(1, 3, 2001), date(5, 3, 2001));


    localTaskSeries1.add(new Task("T1a", new Hour(11, new Day())));
    localTaskSeries1.add(new Task("T1b", new Hour(14, new Day())));
    localTaskSeries1.add(new Task("T1c", new Hour(16, new Day())));

    localTaskSeriesCollection.add(localTaskSeries1);

    return new XYTaskDataset(localTaskSeriesCollection);
  }

  private void updateChart(final IntervalXYDataset paramIntervalXYDataset) {

    // set the main data set :
    localXYPlot.setDataset(paramIntervalXYDataset);

    // change the Domain axis (vertical) :
    final SymbolAxis localSymbolAxis = new SymbolAxis("Source", new String[]{"Star 1"});
    localSymbolAxis.setGridBandsVisible(false);
    localSymbolAxis.setAutoRange(false);
    localSymbolAxis.setRangeWithMargins(-1d, 1d);
    localXYPlot.setDomainAxis(localSymbolAxis);

    final XYBarRenderer localXYBarRenderer = (XYBarRenderer) localXYPlot.getRenderer();

    // remove Annotations :
    localXYBarRenderer.removeAnnotations();

    // add the Annotations :

    // 0D corresponds to the first Star :
    final XYPointerAnnotation localXYPointerAnnotation1 = new XYPointerAnnotation("TEST", 0D, new Hour(13, new Day()).getFirstMillisecond(), 45D);
    localXYPointerAnnotation1.setTextAnchor(TextAnchor.BOTTOM_CENTER);
    localXYPointerAnnotation1.setPaint(Color.black);
    localXYPointerAnnotation1.setArrowPaint(Color.black);
    localXYBarRenderer.addAnnotation(localXYPointerAnnotation1);
  }

  private void updateDateAxis(final String label, final Date from, final Date to) {
    // change the Range axis (horizontal) :
    final DateAxis dateAxis = new DateAxis(label);
    dateAxis.setAutoRange(false);
    dateAxis.setRange(from.getTime(), to.getTime());
    this.localXYPlot.setRangeAxis(dateAxis);
  }

  private void updateSunMarkers(final List<SunTimeInterval> intervals) {
    // remove Markers :
    localXYPlot.clearRangeMarkers();

    // add the Markers :
    if (intervals != null) {
      Color col;
      IntervalMarker localIntervalMarker;
      for (SunTimeInterval interval : intervals) {
        switch (interval.getType()) {
          case Day:
            col = Color.WHITE;
            break;
          case Night:
            col = Color.BLACK;
            break;
          default:
          case Twilight:
            col = Color.LIGHT_GRAY;
            break;
        }
        localIntervalMarker = new IntervalMarker(interval.getStartDate().getTime(),
                interval.getEndDate().getTime(), col);
        localIntervalMarker.setAlpha(0.4f);
        localXYPlot.addRangeMarker(localIntervalMarker, Layer.BACKGROUND);
      }
    }
  }
  /** drawing started time value */
  private long lastTime = 0l;

  public void chartProgress(final ChartProgressEvent event) {
    if (logger.isLoggable(Level.FINE)) {
      switch (event.getType()) {
        case ChartProgressEvent.DRAWING_STARTED:
          lastTime = System.currentTimeMillis();
          break;
        case ChartProgressEvent.DRAWING_FINISHED:
          logger.fine("Drawing chart time : " + (System.currentTimeMillis() - lastTime) + " ms.");
          lastTime = 0l;
        default:
      }
    }
  }
}
