/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: UVCoveragePanel.java,v 1.5 2010-01-15 13:52:14 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
import fr.jmmc.aspro.gui.action.ExportPDFAction;
import fr.jmmc.aspro.gui.chart.ChartUtils;
import fr.jmmc.aspro.gui.chart.SquareChartPanel;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.ObservationListener;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.uvcoverage.UVCoverageData;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Pop;
import fr.jmmc.aspro.model.uvcoverage.UVBaseLineData;
import fr.jmmc.aspro.service.UVCoverageService;
import fr.jmmc.mcs.gui.StatusBar;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.Vector;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import javax.swing.DefaultComboBoxModel;
import org.jdesktop.swingworker.SwingWorker;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.event.ChartProgressEvent;
import org.jfree.chart.event.ChartProgressListener;
import org.jfree.chart.plot.XYPlot;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 * This panel presents the UV coverage plot with its parameters (target, instrument mode ...)
 * @author bourgesl
 */
public class UVCoveragePanel extends javax.swing.JPanel implements ChartProgressListener, ActionListener, ObservationListener {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.UVChartPanel";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /* members */
  /** observation manager */
  private ObservationManager om = ObservationManager.getInstance();
  /** jFreeChart instance */
  private JFreeChart localJFreeChart;
  /** xy plot instance */
  private XYPlot localXYPlot;
  /* swing */
  /** chart panel */
  private ChartPanel chartPanel;
  /** flag to enable / disable the automatic refresh of the plot when any swing component changes */
  private boolean doAutoRefresh = true;
  /** flag to enable / disable the automatic update of the observation when any swing component changes */
  private boolean doAutoUpdateObservation = true;
  /**
   * Current (or old) worker reference
   * (can be a leaked reference if the computation is done)
   */
  private SwingWorker<UVCoverageData, Void> currentWorker = null;

  /**
   * Constructor
   */
  public UVCoveragePanel() {
    initComponents();

    postInit();

    // register this as an observation listener :
    om.register(this);
  }

  /** This method is called from within the constructor to
   * initialize the form.
   * WARNING: Do NOT modify this code. The content of this method is
   * always regenerated by the Form Editor.
   */
  @SuppressWarnings("unchecked")
  // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
  private void initComponents() {
    java.awt.GridBagConstraints gridBagConstraints;

    jPanelRight = new javax.swing.JPanel();
    jLabel1 = new javax.swing.JLabel();
    jComboBoxTarget = new javax.swing.JComboBox();
    jLabel2 = new javax.swing.JLabel();
    jComboBoxInstrumentMode = new javax.swing.JComboBox();
    jLabel3 = new javax.swing.JLabel();
    jFormattedTextField1 = new javax.swing.JFormattedTextField();
    jButtonPDF = new javax.swing.JButton();

    setLayout(new javax.swing.BoxLayout(this, javax.swing.BoxLayout.X_AXIS));

    jPanelRight.setMaximumSize(new java.awt.Dimension(200, 2147483647));
    jPanelRight.setMinimumSize(new java.awt.Dimension(200, 300));
    jPanelRight.setPreferredSize(new java.awt.Dimension(200, 300));
    jPanelRight.setLayout(new java.awt.GridBagLayout());

    jLabel1.setText("Target");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.ipadx = 2;
    gridBagConstraints.ipady = 2;
    jPanelRight.add(jLabel1, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    jPanelRight.add(jComboBoxTarget, gridBagConstraints);

    jLabel2.setText("Instrument mode");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.insets = new java.awt.Insets(10, 0, 0, 0);
    jPanelRight.add(jLabel2, gridBagConstraints);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 3;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    jPanelRight.add(jComboBoxInstrumentMode, gridBagConstraints);

    jLabel3.setText("<html>Sampling<br>Periodicity (min)</html>");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 4;
    gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
    gridBagConstraints.insets = new java.awt.Insets(10, 0, 0, 0);
    jPanelRight.add(jLabel3, gridBagConstraints);

    jFormattedTextField1.setColumns(3);
    jFormattedTextField1.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter()));
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 5;
    jPanelRight.add(jFormattedTextField1, gridBagConstraints);

    jButtonPDF.setIcon(new javax.swing.ImageIcon(getClass().getResource("/fr/jmmc/aspro/gui/icons/icon_pdf.gif"))); // NOI18N
    jButtonPDF.setMargin(new java.awt.Insets(0, 0, 0, 0));
    jButtonPDF.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jButtonPDFActionPerformed(evt);
      }
    });
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 6;
    gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
    jPanelRight.add(jButtonPDF, gridBagConstraints);

    add(jPanelRight);
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

  /**
   * This method is useful to set the models and specific features of initialized swing components :
   */
  private void postInit() {

    this.localJFreeChart = ChartUtils.createSquareXYLineChart("U (m)", "V (m)");
    this.localXYPlot = (XYPlot) localJFreeChart.getPlot();

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
    this.chartPanel.setMouseWheelEnabled(AsproConstants.ENABLE_ZOOM);

    add(chartPanel, java.awt.BorderLayout.CENTER);

    // define change listeners :
    this.jComboBoxTarget.addActionListener(this);
    this.jComboBoxInstrumentMode.addActionListener(this);
  }

  /**
   * Refresh the target list
   */
  private void updateComboTarget() {
    final Object oldValue = this.jComboBoxTarget.getSelectedItem();

    final Vector<String> v = this.om.getTargetNames();
    this.jComboBoxTarget.setModel(new DefaultComboBoxModel(v));

    // restore previous selected item :
    if (oldValue != null) {
      this.jComboBoxTarget.setSelectedItem(oldValue);
    }
    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("jComboBoxTarget updated : "+ this.jComboBoxTarget.getSelectedItem());
    }
  }

  /**
   * Refresh the instrument modes
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
      logger.finest("jComboBoxInstrumentMode updated : "+ this.jComboBoxInstrumentMode.getSelectedItem());
    }
  }

  /**
   * Process any comboBox change event (interferometer, interferometer configuration, instrument, instrument configuration).
   * Refresh the dependent combo boxes and update the observation according to the form state
   * @param e action event
   */
  public void actionPerformed(final ActionEvent e) {
    if (e.getSource() == this.jComboBoxTarget) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("target changed : " + this.jComboBoxTarget.getSelectedItem());
      }
      refreshPlot();
    } else if (e.getSource() == this.jComboBoxInstrumentMode) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("instrument mode changed : " + this.jComboBoxInstrumentMode.getSelectedItem());
      }
      updateObservation();
      refreshPlot();
    }
  }

  /**
   * Update the observation with the form fields if the automatic update flag is enabled.
   */
  private void updateObservation() {
    // check if the automatic update flag is enabled :
    if (this.doAutoUpdateObservation) {

      final String targetName = (String) this.jComboBoxTarget.getSelectedItem();

      if (targetName != null) {
        // Change the instrument mode :

        this.om.setInstrumentMode((String) this.jComboBoxInstrumentMode.getSelectedItem());
      } else {
        // clean up i.e. the panel is then invalid :

        this.om.setInstrumentMode(null);
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

      // refresh the instrument modes :
      updateComboInstrumentModes(observation);

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

      // refresh the instrument modes :
      updateComboInstrumentModes(observation);

      // update the selected instrument mode :
      this.jComboBoxInstrumentMode.setSelectedItem(observation.getInstrumentConfiguration().getInstrumentMode());

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
        this.plot(observation);
        break;
      default:
    }
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("event [" + type + "] process OUT");
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

    final String targetName = (String) this.jComboBoxTarget.getSelectedItem();

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

          UVCoverageData uvData = new UVCoverageService(observation, targetName).compute();

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

                ChartUtils.clearTextSubTitle(localJFreeChart);

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

                // source :
                ChartUtils.addSubtitle(localJFreeChart, "Source : " + uvData.getName());

                // computed data are valid :
                updateChart(uvData);

                // update theme at end :
                ChartUtilities.applyCurrentTheme(localJFreeChart);
              }

            } catch (InterruptedException ignore) {
            } catch (ExecutionException ee) {
              logger.log(Level.SEVERE, "Error : ", ee);
            }

            // update the status bar :
            StatusBar.show("uv coverage done.");

            logger.fine("SwingWorker[UV].done : OUT");
          }
        }
      };

      // update the status bar :
      StatusBar.show("computing uv coverage ... (please wait, this may take a while)");

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

    } // observability data check
  }

  /**
   * Update the datasets
   * @param uvData uv coverage data
   */
  private void updateChart(final UVCoverageData uvData) {
    final XYSeriesCollection dataset = new XYSeriesCollection();

    // process uv rise/set :
    final List<UVBaseLineData> targetUVRiseSet = uvData.getTargetUVRiseSet();

    if (targetUVRiseSet != null) {
      // target is observable :

      XYSeries xySeriesBL;

      double[] u;
      double[] v;

      for (UVBaseLineData uvBL : targetUVRiseSet) {
        xySeriesBL = new XYSeries(uvBL.getName(), false);
        xySeriesBL.setNotify(false);

        u = uvBL.getU();
        v = uvBL.getV();

        for (int i = 0, size = uvBL.getNPoints(); i < size; i++) {
          xySeriesBL.add(-u[i], -v[i]);
        } // points

        // add an invalid point to break the line between the 2 segments :
        xySeriesBL.add(Double.NaN, Double.NaN);

        for (int i = 0, size = uvBL.getNPoints(); i < size; i++) {
          xySeriesBL.add(u[i], v[i]);
        } // points

        xySeriesBL.setNotify(true);
        dataset.addSeries(xySeriesBL);
      } // BL
    }

    // set the main data set :
    this.localXYPlot.setDataset(dataset);
  }
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JButton jButtonPDF;
  private javax.swing.JComboBox jComboBoxInstrumentMode;
  private javax.swing.JComboBox jComboBoxTarget;
  private javax.swing.JFormattedTextField jFormattedTextField1;
  private javax.swing.JLabel jLabel1;
  private javax.swing.JLabel jLabel2;
  private javax.swing.JLabel jLabel3;
  private javax.swing.JPanel jPanelRight;
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
}
