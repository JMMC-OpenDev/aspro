/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityPanel.java,v 1.4 2009-11-17 17:00:28 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.3  2009/11/16 14:47:46  bourgesl
 * determine the hour angle for a target over a min elevation to get the simple observability
 *
 * Revision 1.2  2009/11/05 12:59:39  bourgesl
 * first simple source observability (only min elevation condition)
 *
 * Revision 1.1  2009/11/03 16:57:55  bourgesl
 * added observability plot with LST/UTC support containing only day/night/twilight zones
 *
 *
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.model.DateTimeInterval;
import fr.jmmc.aspro.model.ObservabilityData;
import fr.jmmc.aspro.model.ObservationListener;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.StarObservability;
import fr.jmmc.aspro.model.SunTimeInterval;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.service.ObservabilityService;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import javax.swing.SwingWorker;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
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
import org.jfree.ui.Layer;

/**
 * This panel represents the observability plot
 * @author bourgesl
 */
public class ObservabilityPanel extends javax.swing.JPanel implements ChartProgressListener, ObservationListener {
  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;

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
   * Current (or old) worker reference
   * (can be a leaked reference if the computation is done)
   */
  private SwingWorker<ObservabilityData, Void> currentWorker = null;

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

    /* plot options */
    final boolean useLst = true;
    final double minElev = 20d;

    /*
     * Requires the java 5 SwingWorker backport = swing-worker-1.2.jar
     */
    final SwingWorker<ObservabilityData, Void> worker = new SwingWorker<ObservabilityData, Void>() {

      /**
       * Compute the observability data in background
       * @return observability data
       */
      @Override
      public ObservabilityData doInBackground() {
        logger.fine("SwingWorker.doInBackground : IN");

        ObservabilityData data = ObservabilityService.calcObservability(observation, useLst, minElev);

        if (isCancelled()) {
          logger.fine("SwingWorker.doInBackground : CANCELLED");
          // no result if task is cancelled :
          data = null;
        } else {
          logger.fine("SwingWorker.doInBackground : OUT");
        }
        return data;
      }

      /**
       * Refresh the plot using the computed observability data
       */
      @Override
      public void done() {
        if (!isCancelled()) {
          logger.fine("SwingWorker.done : IN");
          try {
            // Get the computation results with all data necessary to draw the plot :
            final ObservabilityData data = get();

            if (data != null) {
              // computed data are valid :
              updateChart(data.getStarVisibilities());

              updateDateAxis((useLst) ? "L.S.T" : "U.T.C", data.getDateMin(), data.getDateMax());

              updateSunMarkers(data.getSunIntervals());

              // update theme at end :
              ChartUtilities.applyCurrentTheme(localJFreeChart);
            }

          } catch (InterruptedException ignore) {
          } catch (ExecutionException ee) {
            logger.log(Level.SEVERE, "Error : ", ee);
          }
          logger.fine("SwingWorker.done : OUT");
        }
      }
    };

    if (this.currentWorker != null && !this.currentWorker.isDone()) {
      // interrupt if running :
      this.currentWorker.cancel(true);
    }
    // memorize the reference to the new worker before execution :
    this.currentWorker = worker;

    // start the new worker :
    worker.execute();
  }

  /**
   * Update the dataset and the symbol axis given the star observability data
   * @param starVis star observability data
   */
  private void updateChart(final List<StarObservability> starVis) {
    final String[] targetNames = new String[starVis.size()];

    final TaskSeriesCollection localTaskSeriesCollection = new TaskSeriesCollection();

    int i = 0;
    int j;
    String name;
    TaskSeries taskSeries;
    for (StarObservability so : starVis) {
      name = so.getName();
      // use the target name as the name of the serie :
      targetNames[i++] = name;
      taskSeries = new TaskSeries(name);

      j = 1;

      for (DateTimeInterval interval : so.getVisible()) {
        taskSeries.add(new Task("T" + j, interval.getStartDate(), interval.getEndDate()));
      }

      localTaskSeriesCollection.add(taskSeries);
    }

    // set the main data set :
    localXYPlot.setDataset(new XYTaskDataset(localTaskSeriesCollection));

    // change the Domain axis (vertical) :
    final SymbolAxis localSymbolAxis = new SymbolAxis("Source", targetNames);
    localSymbolAxis.setGridBandsVisible(false);
    localSymbolAxis.setAutoRange(false);
    localSymbolAxis.setRangeWithMargins(-1d, targetNames.length);
    localXYPlot.setDomainAxis(localSymbolAxis);

/*
    final XYBarRenderer localXYBarRenderer = (XYBarRenderer) localXYPlot.getRenderer();
    // remove Annotations :
    localXYBarRenderer.removeAnnotations();

    // add the Annotations :

    // 0D corresponds to the first Star :
    final XYPointerAnnotation localXYPointerAnnotation1 = new XYPointerAnnotation("TEST", 0D, new Hour(13, new Day()).getFirstMillisecond(), Math.PI / 2d);
    localXYPointerAnnotation1.setTextAnchor(TextAnchor.BOTTOM_LEFT);
    localXYPointerAnnotation1.setPaint(Color.black);
    localXYPointerAnnotation1.setArrowPaint(Color.black);
    localXYBarRenderer.addAnnotation(localXYPointerAnnotation1);
     */
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
        break;
        default:
      }
    }
  }
}
