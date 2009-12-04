/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityPanel.java,v 1.11 2009-12-04 15:38:27 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.DateTimeInterval;
import fr.jmmc.aspro.model.ObservabilityData;
import fr.jmmc.aspro.model.ObservationListener;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.StarObservability;
import fr.jmmc.aspro.model.SunTimeInterval;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.service.ObservabilityService;
import fr.jmmc.mcs.gui.StatusBar;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import org.jdesktop.swingworker.SwingWorker;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.StandardChartTheme;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.axis.DateAxis;
import org.jfree.chart.axis.SymbolAxis;
import org.jfree.chart.event.ChartProgressEvent;
import org.jfree.chart.event.ChartProgressListener;
import org.jfree.chart.plot.IntervalMarker;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYBarRenderer;
import org.jfree.chart.title.TextTitle;
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
public class ObservabilityPanel extends javax.swing.JPanel implements ChartProgressListener, ObservationListener {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.ObservabilityPanel";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** debug flag : enables the zoom in / out */
  private static boolean ENABLE_ZOOM = true;
  /** The default font for titles. */
  private static final Font DEFAULT_TITLE_FONT = new Font("SansSerif", Font.BOLD, 14);

  /* members */
  /** jFreeChart instance */
  private JFreeChart localJFreeChart;
  /** xy plot instance */
  private XYPlot localXYPlot;
  /* swing */
  /** chart panel */
  private ChartPanel chartPanel;
  /* min elevation field */
  private JFormattedTextField jFieldMinElev;
  /* checkbox Night Limit */
  private JCheckBox jCheckBoxNightLimit;
  /* checkbox BaseLine Limits */
  private JCheckBox jCheckBoxBaseLineLimits;
  /* checkbox Details */
  private JCheckBox jCheckBoxDetails;

  /* plot options */
  /** minimum of elevation to observe any target (rad) */
  private double minElev;
  /** flag to enable the observability restriction due to the night */
  private boolean useNightLimit = true;
  /** indicates if the timestamps are expressed in LST or in UTC */
  private boolean useLST = true;
  /** flag to find baseline limits */
  private boolean doBaseLineLimits = false;
  /** flag to produce detailed output with all BL / horizon / rise intervals per target */
  private boolean doDetailedOutput = false;
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
    ObservationManager.getInstance().register(this);
  }

  /**
   * Initialize the components (once)
   */
  private void initComponents() {

    // Disable bar shadows before creating any chart :
    if (ChartFactory.getChartTheme() instanceof StandardChartTheme) {
      final StandardChartTheme theme = (StandardChartTheme) ChartFactory.getChartTheme();
      theme.setShadowVisible(false);
    }

    this.localJFreeChart = createChart();
    this.localXYPlot = (XYPlot) localJFreeChart.getPlot();

    // add listener :
    this.localJFreeChart.addProgressListener(this);

    this.chartPanel = new ChartPanel(this.localJFreeChart,
            800, 500, /* prefered size */
            300, 200, /* minimum size before scaling */
            1900, 1200, /* maximum size before scaling */
            true, /* use buffer */
            false, true, false, false, false);

    /*
    public ChartPanel(JFreeChart chart, int width, int height,
    int minimumDrawWidth, int minimumDrawHeight, int maximumDrawWidth,
    int maximumDrawHeight, boolean useBuffer, boolean properties,
    boolean save, boolean print, boolean zoom, boolean tooltips)
     */

    this.chartPanel.setDomainZoomable(ENABLE_ZOOM);
    this.chartPanel.setRangeZoomable(ENABLE_ZOOM);

    this.setLayout(new BorderLayout());

    this.add(this.chartPanel, BorderLayout.CENTER);

    final JPanel panelOptions = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 2));

    panelOptions.add(new JLabel("min. Elevation :"));

    final double defaultMinElev = 20d;
    this.minElev = Math.toRadians(defaultMinElev);

    this.jFieldMinElev = new JFormattedTextField(NumberFormat.getNumberInstance());
    this.jFieldMinElev.setColumns(3);
    this.jFieldMinElev.setValue(defaultMinElev);
    this.jFieldMinElev.addPropertyChangeListener("value", new PropertyChangeListener() {

      public void propertyChange(final PropertyChangeEvent evt) {
        double minElevNew = ((Number) jFieldMinElev.getValue()).doubleValue();

        if (minElevNew >= 0d && minElevNew < 90d) {
          minElev = Math.toRadians(minElevNew);
        } else {
          jFieldMinElev.setValue(defaultMinElev);
        }
        refreshPlot();
      }
    });

    panelOptions.add(this.jFieldMinElev);

    this.jCheckBoxNightLimit = new JCheckBox("Night restriction");
    this.jCheckBoxNightLimit.setSelected(this.useNightLimit);
    this.jCheckBoxNightLimit.addItemListener(new ItemListener() {

      public void itemStateChanged(final ItemEvent e) {
        useNightLimit = e.getStateChange() == ItemEvent.SELECTED;
        refreshPlot();
      }
    });

    panelOptions.add(this.jCheckBoxNightLimit);

    panelOptions.add(new JLabel("Time :"));

    final JComboBox jComboTime = new JComboBox(new String[]{"LST", "UTC"});
    jComboTime.addActionListener(new ActionListener() {

      public void actionPerformed(final ActionEvent e) {
        useLST = jComboTime.getSelectedItem().toString().equals("LST");
        refreshPlot();
      }
    });
    panelOptions.add(jComboTime);

    jCheckBoxBaseLineLimits = new JCheckBox("BaseLine limits");
    jCheckBoxBaseLineLimits.setSelected(this.doBaseLineLimits);
    jCheckBoxBaseLineLimits.addItemListener(new ItemListener() {

      public void itemStateChanged(final ItemEvent e) {
        doBaseLineLimits = e.getStateChange() == ItemEvent.SELECTED;
        if (doBaseLineLimits) {
          useNightLimit = false;
          jCheckBoxNightLimit.setSelected(useNightLimit);
          doDetailedOutput = false;
          jCheckBoxDetails.setSelected(doDetailedOutput);
        }

        jCheckBoxNightLimit.setEnabled(!doBaseLineLimits);
        jCheckBoxDetails.setEnabled(!doBaseLineLimits);
        refreshPlot();
      }
    });

    panelOptions.add(this.jCheckBoxBaseLineLimits);

    if (AsproConstants.DEBUG_MODE) {

      jCheckBoxDetails = new JCheckBox("Details");
      jCheckBoxDetails.setSelected(this.doDetailedOutput);
      jCheckBoxDetails.addItemListener(new ItemListener() {

        public void itemStateChanged(final ItemEvent e) {
          doDetailedOutput = e.getStateChange() == ItemEvent.SELECTED;
          refreshPlot();
        }
      });

      panelOptions.add(this.jCheckBoxDetails);
    }

    this.add(panelOptions, BorderLayout.PAGE_END);
  }

  /**
   * Create the basic XYBarChart
   * @return jFreeChart instance
   */
  private static JFreeChart createChart() {
    // no title :
    final JFreeChart localJFreeChart = ChartFactory.createXYBarChart("", null, false, null, null, PlotOrientation.HORIZONTAL, false, false, false);

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

  protected void refreshPlot() {
    this.plot(ObservationManager.getInstance().getObservation());
  }

  /**
   * Handle the given event on the given observation =
   * compute observability data and refresh the plot
   * @param type event type
   * @param observation observation
   */
  public void onProcess(final ObservationEventType type, final ObservationSetting observation) {
    if (type == ObservationEventType.CHANGED) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("onChange occured : " + observation.getName());
      }
      this.plot(observation);
    }
  }

  protected void plot(final ObservationSetting observation) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("plot : " + observation);
    }

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
        logger.fine("SwingWorker.doInBackground : IN");

        ObservabilityData data = new ObservabilityService(observation, minElev, useNightLimit, useLST, doDetailedOutput, doBaseLineLimits).calcObservability();

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
              // update the status bar :
              StatusBar.show("observability done.");

              final String title = observation.getInterferometerConfiguration().getName() +
                      " - " + observation.getInstrumentConfiguration().getStations();

              localJFreeChart.clearSubtitles();
              // interferometer + stations :
              localJFreeChart.addSubtitle(new TextTitle(title, DEFAULT_TITLE_FONT));
              if (useNightLimit) {
                // date :
                localJFreeChart.addSubtitle(new TextTitle("Day : " + observation.getWhen().getDate().toString(), DEFAULT_TITLE_FONT));
              }

              // computed data are valid :
              updateChart(data.getStarVisibilities(), data.getDateMin(), data.getDateMax());

              updateDateAxis((useLST) ? "LST" : "UTC", data.getDateMin(), data.getDateMax());

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

    // update the status bar :
    StatusBar.show("computing observability ... (please wait, this may take a while)");

    // start the new worker :
    worker.execute();
  }

  /**
   * Update the dataset and the symbol axis given the star observability data
   * @param starVis star observability data
   */
  private void updateChart(final List<StarObservability> starVis, final Date min, final Date max) {
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
    final SymbolAxis localSymbolAxis = new SymbolAxis("", targetNames);
    localSymbolAxis.setInverted(true);
    localSymbolAxis.setGridBandsVisible(false);
    localSymbolAxis.setAutoRange(false);
    localSymbolAxis.setRangeWithMargins(-1d, targetNames.length);
    localXYPlot.setDomainAxis(localSymbolAxis);

    final XYBarRenderer localXYBarRenderer = (XYBarRenderer) localXYPlot.getRenderer();
    // remove Annotations :
    localXYBarRenderer.removeAnnotations();

    if (!this.doBaseLineLimits) {
      // add the Annotations :
      // 24h date formatter :
      final DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT, Locale.FRANCE);

      i = 0;
      for (StarObservability so : starVis) {

        for (DateTimeInterval interval : so.getVisible()) {

          if (!interval.getStartDate().equals(min)) {
            final XYTextAnnotation aStart = new XYTextAnnotation(df.format(interval.getStartDate()), i, interval.getStartDate().getTime());
            aStart.setTextAnchor(TextAnchor.BASELINE_CENTER);
            aStart.setPaint(Color.BLACK);
            aStart.setRotationAngle(Math.PI / 2);
            localXYBarRenderer.addAnnotation(aStart);
          }

          if (!interval.getEndDate().equals(max)) {
            final XYTextAnnotation aEnd = new XYTextAnnotation(df.format(interval.getEndDate()), i, interval.getEndDate().getTime());
            aEnd.setTextAnchor(TextAnchor.BASELINE_CENTER);
            aEnd.setPaint(Color.BLACK);
            aEnd.setRotationAngle(Math.PI / 2);
            localXYBarRenderer.addAnnotation(aEnd);
          }
        }

        i++;
      }
    }
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
