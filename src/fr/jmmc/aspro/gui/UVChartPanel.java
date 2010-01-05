/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: UVChartPanel.java,v 1.4 2010-01-05 17:19:29 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.gui.chart.ChartUtils;
import fr.jmmc.aspro.model.ObservationListener;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.event.ChartProgressEvent;
import org.jfree.chart.event.ChartProgressListener;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.chart.title.TextTitle;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 * This panel represents the UV coverage plot
 * @author bourgesl
 */
public class UVChartPanel extends javax.swing.JPanel implements ChartProgressListener, ObservationListener {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.UVChartPanel";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /* members */
  /** jFreeChart instance */
  private JFreeChart localJFreeChart;
  /** xy plot instance */
  private XYPlot localXYPlot;
  /* swing */
  /** chart panel */
  private ChartPanel chartPanel;

  /**
   * Constructor
   */
  public UVChartPanel() {
    super(new BorderLayout());
    initComponents();

    // register this as an observation listener :
    ObservationManager.getInstance().register(this);
  }

  /**
   * Initialize the components (once)
   */
  private void initComponents() {

    this.localJFreeChart = createChart();
    this.localXYPlot = (XYPlot) localJFreeChart.getPlot();

    // add listener :
    this.localJFreeChart.addProgressListener(this);

    this.chartPanel = new ChartPanel(this.localJFreeChart,
            600, 400, /* prefered size */
            300, 200, /* minimum size before scaling */
            1900, 1200, /* maximum size before scaling */
            true, /* use buffer */
            false, true, false, false, false) {

      /** default serial UID for Serializable interface */
      private static final long serialVersionUID = 1;

      /**
       * Hack : return the min(width, height) to get a square shape
       */
      @Override
      public Dimension getSize() {
        final int width = getWidth();
        final int height = getHeight();

        final int min = Math.min(width, height);

        return new Dimension(min, min);
      }
    };

    // TODO : remove later
    // set the main data set :
    this.localXYPlot.setDataset(createDataset());

    this.add(chartPanel);
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
          logger.fine("onChange occured : " + observation.getName());
        }
        break;
      default:
    }
  }

  private static XYDataset createDataset() {
    final XYSeries uv = new XYSeries("UV");

    // load data from file :
    BufferedReader reader = null;
    try {
      final File data = new File(UVChartPanel.class.getResource("UV-aspro-AMBER.tsv").toURI());

      reader = new BufferedReader(new FileReader(data));

      int n = 0;
      String line;
      StringTokenizer tok;
      double u, v;

      while ((line = reader.readLine()) != null) {
        tok = new StringTokenizer(line, "\t");
        if (tok.countTokens() > 1) {
          u = Double.parseDouble(tok.nextToken());
          v = Double.parseDouble(tok.nextToken());

          uv.add(u, v);
          n++;
        }
      }

      System.out.println("uv points : " + n);

    } catch (FileNotFoundException fnfe) {
      Logger.getLogger(UVChartPanel.class.getName()).log(Level.SEVERE, null, fnfe);
    } catch (IOException ioe) {
      Logger.getLogger(UVChartPanel.class.getName()).log(Level.SEVERE, null, ioe);
    } catch (URISyntaxException use) {
      Logger.getLogger(UVChartPanel.class.getName()).log(Level.SEVERE, null, use);
    } finally {
      if (reader != null) {
        try {
          reader.close();
        } catch (IOException ex) {
          Logger.getLogger(UVChartPanel.class.getName()).log(Level.SEVERE, null, ex);
        }
      }
    }
    /*
    uv.add(-1.3833865242417043E7d, -1.3833865242417043E7d);
    uv.add(-1.367576429101594E7d, 2.703247125777618E7d);
     */
    final XYSeriesCollection dataset = new XYSeriesCollection(uv);

    return dataset;
  }

  /**
   * Create the basic XYBarChart
   * @return jFreeChart instance
   */
  private static JFreeChart createChart() {
    final JFreeChart localJFreeChart = ChartFactory.createScatterPlot(
            "UV tracks", "U", "V", null, PlotOrientation.VERTICAL, false, false, false);

    localJFreeChart.addSubtitle(new TextTitle("Test"));

    final XYPlot plot = (XYPlot) localJFreeChart.getPlot();

    final Object localObject = (NumberAxis) plot.getRangeAxis();
    ((NumberAxis) localObject).setStandardTickUnits(ChartUtils.createScientificTickUnits());

    plot.getDomainAxis().setStandardTickUnits(ChartUtils.createScientificTickUnits());

// background image :
//    plot.setBackgroundImage(null);

    final XYLineAndShapeRenderer localLineAndShapeRenderer = (XYLineAndShapeRenderer) plot.getRenderer();

    localLineAndShapeRenderer.setBaseShapesVisible(true);
    localLineAndShapeRenderer.setDrawOutlines(false);
    localLineAndShapeRenderer.setUseFillPaint(false);
//    localLineAndShapeRenderer.setBaseFillPaint(Color.white);
    localLineAndShapeRenderer.setSeriesStroke(0, new BasicStroke(1.0F));
//    localLineAndShapeRenderer.setSeriesOutlineStroke(0, new BasicStroke(2.0F));
//    localLineAndShapeRenderer.setSeriesShape(0, new Rectangle(-3, -3, 3, 3));

//    ChartUtilities.applyCurrentTheme(localJFreeChart);

    return localJFreeChart;
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
