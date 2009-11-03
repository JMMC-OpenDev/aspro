package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.gui.chart.ChartUtils;
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
 * Test the UV coverage chart
 * @author bourgesl
 */
public class UVChartPanel extends javax.swing.JPanel implements ChartProgressListener {

    private ChartPanel chartPanel;


  public UVChartPanel() {
    super(new BorderLayout());
    initComponents();
  }

    private void initComponents() {

      final JFreeChart chart = createChart(createDataset());

      // add listener :
      chart.addProgressListener(this);

      this.chartPanel = new ChartPanel(chart);
      this.chartPanel.setPreferredSize(new Dimension(600, 450));
      this.add(chartPanel);
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
      double u,v;

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



 private static JFreeChart createChart(XYDataset dataset)
  {
    JFreeChart localJFreeChart = ChartFactory.createScatterPlot(
            "UV tracks", "U", "V", dataset, PlotOrientation.VERTICAL, false, false, false);

    localJFreeChart.addSubtitle(new TextTitle("Test"));

    XYPlot plot = (XYPlot)localJFreeChart.getPlot();

    Object localObject = (NumberAxis)plot.getRangeAxis();
    ((NumberAxis)localObject).setStandardTickUnits(ChartUtils.createScientificTickUnits());

    plot.getDomainAxis().setStandardTickUnits(ChartUtils.createScientificTickUnits());

// background image :
//    plot.setBackgroundImage(null);


    final XYLineAndShapeRenderer localLineAndShapeRenderer = (XYLineAndShapeRenderer)plot.getRenderer();

    localLineAndShapeRenderer.setBaseShapesVisible(true);
    localLineAndShapeRenderer.setDrawOutlines(false);
    localLineAndShapeRenderer.setUseFillPaint(false);
//    localLineAndShapeRenderer.setBaseFillPaint(Color.white);
    localLineAndShapeRenderer.setSeriesStroke(0, new BasicStroke(1.0F));
//    localLineAndShapeRenderer.setSeriesOutlineStroke(0, new BasicStroke(2.0F));
//    localLineAndShapeRenderer.setSeriesShape(0, new Rectangle(-3, -3, 3, 3));

    return ((JFreeChart)localJFreeChart);
  }

 private long lastTime = 0l;

  public void chartProgress(ChartProgressEvent event) {
    switch (event.getType()) {
      case ChartProgressEvent.DRAWING_STARTED:
        lastTime = System.currentTimeMillis();
        break;
      case ChartProgressEvent.DRAWING_FINISHED:
        System.out.println("Drawing chart time : " + (System.currentTimeMillis() - lastTime) + " ms.");
        lastTime = 0l;
      default:
    }
  }

}
