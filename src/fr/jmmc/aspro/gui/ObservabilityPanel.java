/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityPanel.java,v 1.18 2010-01-13 16:12:31 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.gui.action.ExportPDFAction;
import fr.jmmc.aspro.gui.chart.ChartUtils;
import fr.jmmc.aspro.model.observability.DateTimeInterval;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.ObservationListener;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.observability.StarObservabilityData;
import fr.jmmc.aspro.model.observability.SunTimeInterval;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Pop;
import fr.jmmc.aspro.service.ObservabilityService;
import fr.jmmc.mcs.gui.StatusBar;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
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
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFormattedTextField;
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
public class ObservabilityPanel extends javax.swing.JPanel implements ChartProgressListener, ObservationListener {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.ObservabilityPanel";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /* time references */
  /** LST time reference */
  private static final String TIME_LST = "LST";
  /** UTC time reference */
  private static final String TIME_UTC = "UTC";

  /* default plot options */
  /** default value for checkbox Night Limit = true */
  private static final boolean DEFAULT_USE_NIGHT_LIMITS = true;
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
  /* min elevation field */
  private JFormattedTextField jFieldMinElev;
  /** time reference combo box */
  private JComboBox jComboTimeRef;
  /* checkbox Night Limit */
  private JCheckBox jCheckBoxNightLimit;
  /* checkbox BaseLine Limits */
  private JCheckBox jCheckBoxBaseLineLimits;
  /* checkbox Detailed output */
  private JCheckBox jCheckBoxDetailedOutput;
  /** pdf export button */
  private JButton jButtonPDF;

  /** flag to enable / disable the automatic refresh of the plot when any swing component changes */
  private boolean doAutoRefresh = true;
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

    final JPanel panelOptions = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 2));

    panelOptions.add(new JLabel("min. Elevation :"));

    this.jFieldMinElev = new JFormattedTextField(NumberFormat.getNumberInstance());
    this.jFieldMinElev.setColumns(3);
    this.jFieldMinElev.setValue(AsproConstants.DEFAULT_MIN_ELEVATION);
    this.jFieldMinElev.addPropertyChangeListener("value", new PropertyChangeListener() {

      public void propertyChange(final PropertyChangeEvent evt) {
        final double minElevNew = ((Number) jFieldMinElev.getValue()).doubleValue();

        if (minElevNew < 0d || minElevNew >= 90d) {
          // invalid value :
          jFieldMinElev.setValue(AsproConstants.DEFAULT_MIN_ELEVATION);
        }
        refreshPlot();
      }
    });

    panelOptions.add(this.jFieldMinElev);

    this.jCheckBoxNightLimit = new JCheckBox("Night restriction");
    this.jCheckBoxNightLimit.setSelected(DEFAULT_USE_NIGHT_LIMITS);
    this.jCheckBoxNightLimit.addItemListener(new ItemListener() {

      public void itemStateChanged(final ItemEvent e) {
        refreshPlot();
      }
    });

    panelOptions.add(this.jCheckBoxNightLimit);

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
          jCheckBoxNightLimit.setSelected(false);
          jCheckBoxDetailedOutput.setSelected(false);
        }

        jCheckBoxNightLimit.setEnabled(!doBaseLineLimits);
        jCheckBoxDetailedOutput.setEnabled(!doBaseLineLimits);
        refreshPlot();
      }
    });

    panelOptions.add(this.jCheckBoxBaseLineLimits);

    if (AsproConstants.DEBUG_MODE) {

      this.jCheckBoxDetailedOutput = new JCheckBox("Details");
      this.jCheckBoxDetailedOutput.setSelected(DEFAULT_DO_DETAILED_OUTPUT);
      this.jCheckBoxDetailedOutput.addItemListener(new ItemListener() {

        public void itemStateChanged(final ItemEvent e) {
          refreshPlot();
        }
      });

      panelOptions.add(this.jCheckBoxDetailedOutput);
    }

    this.jButtonPDF = new javax.swing.JButton();
    this.jButtonPDF.setIcon(new javax.swing.ImageIcon(getClass().getResource("/fr/jmmc/aspro/gui/icons/icon_pdf.gif"))); // NOI18N
    this.jButtonPDF.setMargin(new java.awt.Insets(0, 0, 0, 0));
    this.jButtonPDF.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonPDFActionPerformed(evt);
      }
    });
    panelOptions.add(this.jButtonPDF);

    this.add(panelOptions, BorderLayout.PAGE_END);
  }

  private void jButtonPDFActionPerformed(java.awt.event.ActionEvent evt) {

    // set the source with the chart :
    evt.setSource(this.localJFreeChart);

    ExportPDFAction.getInstance().actionPerformed(evt);
  }

  private void resetOptions() {
    try {
      // first disable the automatic refresh from field changes :
      this.doAutoRefresh = false;

      this.jFieldMinElev.setValue(AsproConstants.DEFAULT_MIN_ELEVATION);

      this.jComboTimeRef.setSelectedItem(TIME_LST);

      this.jCheckBoxNightLimit.setSelected(DEFAULT_USE_NIGHT_LIMITS);
      this.jCheckBoxBaseLineLimits.setSelected(DEFAULT_DO_BASELINE_LIMITS);
      this.jCheckBoxDetailedOutput.setSelected(DEFAULT_DO_DETAILED_OUTPUT);
    } finally {
      // restore the automatic refresh from field changes :
      this.doAutoRefresh = true;
    }
  }

  protected void refreshPlot() {
    if (this.doAutoRefresh) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("refreshPlot");
      }
      this.plot(ObservationManager.getInstance().getObservation());
    }
  }

  /**
   * Handle the given event on the given observation =
   * compute observability data and refresh the plot
   * @param type event type
   * @param observation observation
   */
  public void onProcess(final ObservationEventType type, final ObservationSetting observation) {
    switch (type) {
      case CHANGED:
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("onChange occured : " + ObservationManager.toString(observation));
        }
        this.plot(observation);
        break;
      case LOADED:
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("onLoad occured : " + ObservationManager.toString(observation));
        }
        resetOptions();
        break;
      default:
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

    /** minimum of elevation to observe any target (rad) */
    final double minElev = Math.toRadians(((Number) this.jFieldMinElev.getValue()).doubleValue());

    /** flag to enable the observability restriction due to the night */
    final boolean useNightLimit = this.jCheckBoxNightLimit.isSelected();

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
        obsData = new ObservabilityService(observation, minElev, useNightLimit, useLST, doDetailedOutput, doBaseLineLimits).compute();

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
              final StringBuilder title = new StringBuilder(observation.getInterferometerConfiguration().getName());
              title.append(" - ").append(observation.getInstrumentConfiguration().getStations());

              if (obsData.getBestPops() != null) {
                title.append(" + ");
                for (Pop pop : obsData.getBestPops().getPopList()) {
                  title.append(pop.getName()).append(" ");
                }
              }

              ChartUtils.addSubtitle(localJFreeChart, title.toString());

              if (useNightLimit || !useLST) {
                // date :
                ChartUtils.addSubtitle(localJFreeChart, "Day : " + observation.getWhen().getDate().toString());
              }

              // computed data are valid :
              updateChart(obsData.getStarVisibilities(), obsData.getDateMin(), obsData.getDateMax());

              updateDateAxis((useLST) ? "LST" : "UTC", obsData.getDateMin(), obsData.getDateMax());

              updateSunMarkers(obsData.getSunIntervals());

              // update theme at end :
              ChartUtilities.applyCurrentTheme(localJFreeChart);
            }

          } catch (InterruptedException ignore) {
          } catch (ExecutionException ee) {
            logger.log(Level.SEVERE, "Error : ", ee);
          }

          // update the status bar :
          StatusBar.show("observability done.");

          logger.fine("SwingWorker[Observability].done : OUT");
        }
      }
    };

    // update the status bar :
    StatusBar.show("computing observability ... (please wait, this may take a while)");

    // first, cancel the current running worker :
    if (this.currentWorker != null) {
      // note : if the worker was previously cancelled, it has no effect.
      // interrupt the thread to have Thread.isInterrupted() == true :
      this.currentWorker.cancel(true);
    }

    // memorize the reference to the new worker before execution :
    this.currentWorker = worker;

    // start the new worker :
    worker.execute();
  }

  /**
   * Update the datasets and the symbol axis given the star observability data
   * @param starVis star observability data
   */
  private void updateChart(final List<StarObservabilityData> starVis, final Date min, final Date max) {
    final String[] targetNames = new String[starVis.size()];

    final TaskSeriesCollection localTaskSeriesCollection = new TaskSeriesCollection();

    int i = 0;
    int j;
    String name;
    TaskSeries taskSeries;
    Task task;
    for (StarObservabilityData so : starVis) {
      name = so.getName();
      // use the target name as the name of the serie :
      targetNames[i++] = name;
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
    }

    // set the main data set :
    this.localXYPlot.setDataset(new XYTaskDataset(localTaskSeriesCollection));

    // change the Domain axis (vertical) :
    final SymbolAxis localSymbolAxis = new SymbolAxis("", targetNames);
    localSymbolAxis.setInverted(true);
    localSymbolAxis.setGridBandsVisible(false);
    localSymbolAxis.setAutoRange(false);
    localSymbolAxis.setRangeWithMargins(-1d, targetNames.length);
    this.localXYPlot.setDomainAxis(localSymbolAxis);

    final XYBarRenderer localXYBarRenderer = (XYBarRenderer) this.localXYPlot.getRenderer();
    // remove Annotations :
    localXYBarRenderer.removeAnnotations();

    if (!this.jCheckBoxBaseLineLimits.isSelected()) {
      // add the Annotations :
      // 24h date formatter like in france :
      final DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT, Locale.FRANCE);

      i = 0;
      for (StarObservabilityData so : starVis) {

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
    this.localXYPlot.clearRangeMarkers();

    // add the Markers :
    if (intervals != null) {
      Color col;
      IntervalMarker localIntervalMarker;
      for (SunTimeInterval interval : intervals) {
        switch (interval.getType()) {
          case Day:
            col = new Color(224,224,224);
            break;
          case Night:
            col = new Color(96,96,96);
            break;
          default:
          case Twilight:
            col = new Color(192,192,192);
            break;
        }
        localIntervalMarker = new IntervalMarker(interval.getStartDate().getTime(),
                interval.getEndDate().getTime(), col);
        localIntervalMarker.setAlpha(1.0f);
        this.localXYPlot.addRangeMarker(localIntervalMarker, Layer.BACKGROUND);
      }
    }
  }
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
}
