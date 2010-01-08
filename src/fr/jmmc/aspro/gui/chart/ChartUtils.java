/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ChartUtils.java,v 1.2 2010-01-08 16:51:17 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.chart;

import java.awt.Font;
import java.text.DecimalFormat;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.NumberTickUnit;
import org.jfree.chart.axis.TickUnitSource;
import org.jfree.chart.axis.TickUnits;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.event.ChartChangeEventType;
import org.jfree.chart.event.PlotChangeEvent;
import org.jfree.chart.labels.StandardXYToolTipGenerator;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.chart.title.LegendTitle;
import org.jfree.chart.title.TextTitle;
import org.jfree.chart.title.Title;
import org.jfree.chart.urls.StandardXYURLGenerator;
import org.jfree.data.Range;
import org.jfree.data.general.DatasetChangeEvent;
import org.jfree.data.xy.XYDataset;

/**
 *
 * @author bourgesl
 */
public class ChartUtils {

  /** The default font for titles. */
  private static final Font DEFAULT_TITLE_FONT = new Font("SansSerif", Font.BOLD, 14);
  /** scientific tick units singleton */
  private static TickUnits SCIENTIFIC_TICK_UNITS = null;

  public static void clearTextSubTitle(final JFreeChart localJFreeChart) {
    Title legend = null;
    Title title;
    for (int i = 0; i < localJFreeChart.getSubtitleCount(); i++) {
      title = localJFreeChart.getSubtitle(i);
      if (title instanceof LegendTitle) {
        legend = title;
        break;
      }
    }
    localJFreeChart.clearSubtitles();
    if (legend != null) {
      localJFreeChart.addSubtitle(legend);
    }
  }

  /**
   * Add a sub title to the given chart
   * @param localJFreeChart chart
   * @param text sub title content
   */
  public static void addSubtitle(final JFreeChart localJFreeChart, final String text) {
    localJFreeChart.addSubtitle(new TextTitle(text, DEFAULT_TITLE_FONT));
  }

  /**
   * Returns a collection of tick units for integer values.
   *
   * @return A collection of tick units for integer values.
   *
   * @see #setStandardTickUnits(TickUnitSource)
   * @see #createStandardTickUnits()
   */
  public static TickUnitSource createScientificTickUnits() {
    if (SCIENTIFIC_TICK_UNITS == null) {
      final TickUnits units = new TickUnits();
      final DecimalFormat df0 = new DecimalFormat("0");
      final DecimalFormat df1 = new DecimalFormat("0.00E00");

      units.add(new NumberTickUnit(1, df0, 2));
      units.add(new NumberTickUnit(5, df0, 5));
      units.add(new NumberTickUnit(10, df0, 2));
      units.add(new NumberTickUnit(50, df0, 5));
      units.add(new NumberTickUnit(100, df0, 2));
      units.add(new NumberTickUnit(500, df0, 5));

      units.add(new NumberTickUnit(1000, df1, 2));
      units.add(new NumberTickUnit(5000, df1, 5));
      units.add(new NumberTickUnit(10000, df1, 2));
      units.add(new NumberTickUnit(50000, df1, 5));
      units.add(new NumberTickUnit(100000, df1, 2));
      units.add(new NumberTickUnit(500000, df1, 5));
      units.add(new NumberTickUnit(1000000, df1, 2));
      units.add(new NumberTickUnit(5000000, df1, 5));
      units.add(new NumberTickUnit(10000000, df1, 2));
      units.add(new NumberTickUnit(50000000, df1, 5));
      units.add(new NumberTickUnit(100000000, df1, 2));
      units.add(new NumberTickUnit(500000000, df1, 5));
      units.add(new NumberTickUnit(1000000000, df1, 2));
      units.add(new NumberTickUnit(5000000000.0, df1, 5));
      units.add(new NumberTickUnit(10000000000.0, df1, 2));

      SCIENTIFIC_TICK_UNITS = units;
    }
    return SCIENTIFIC_TICK_UNITS;
  }


  /**
   * Creates a line chart (based on an {@link XYDataset}) with default
   * settings.
   *
   * @param title  the chart title (<code>null</code> permitted).
   * @param xAxisLabel  a label for the X-axis (<code>null</code> permitted).
   * @param yAxisLabel  a label for the Y-axis (<code>null</code> permitted).
   * @param dataset  the dataset for the chart (<code>null</code> permitted).
   * @param orientation  the plot orientation (horizontal or vertical)
   *                     (<code>null</code> NOT permitted).
   * @param legend  a flag specifying whether or not a legend is required.
   * @param tooltips  configure chart to generate tool tips?
   * @param urls  configure chart to generate URLs?
   *
   * @return The chart.
   */
  public static JFreeChart createSquareXYLineChart(String title,
          String xAxisLabel,
          String yAxisLabel,
          XYDataset dataset,
          PlotOrientation orientation,
          boolean legend,
          boolean tooltips,
          boolean urls) {

    if (orientation == null) {
      throw new IllegalArgumentException("Null 'orientation' argument.");
    }
    final NumberAxis xAxis = new NumberAxis(xAxisLabel);
    xAxis.setAutoRangeIncludesZero(false);
    final NumberAxis yAxis = new NumberAxis(yAxisLabel);
    final XYItemRenderer renderer = new XYLineAndShapeRenderer(true, false);

    // HACK :
    final XYPlot plot = new XYPlot(dataset, xAxis, yAxis, renderer) {

      /** default serial UID for Serializable interface */
      private static final long serialVersionUID = 1;

      /**
       * Hack : return the min(width, height) to get a square plot
       */
      @Override
      public void datasetChanged(final DatasetChangeEvent event) {
        configureDomainAxes();
        configureRangeAxes();

        double xMin = 0d;
        double xMax = 0d;
        double yMin = 0d;
        double yMax = 0d;

        final ValueAxis domainAxis = getDomainAxis(0);
        if (domainAxis != null) {
            Range xRange = domainAxis.getRange();

            xMin = xRange.getLowerBound();
            xMax = xRange.getUpperBound();
        }

        final ValueAxis rangeAxis = getRangeAxis(0);
        if (rangeAxis != null) {
            Range yRange = rangeAxis.getRange();

            yMin = yRange.getLowerBound();
            yMax = yRange.getUpperBound();
        }

        xMin = Math.abs(xMin);
        xMax = Math.abs(xMax);
        yMin = Math.abs(yMin);
        yMax = Math.abs(yMax);

        double max = Math.max(xMin, xMax);
        max = Math.max(max, yMin);
        max = Math.max(max, yMax);

        if (rangeAxis != null) {
          // do not disable auto range :
          domainAxis.setRange(new Range(-max, max), false, true);
          rangeAxis.setRange(new Range(-max, max), false, true);
        }

        final PlotChangeEvent e = new PlotChangeEvent(this);
        e.setType(ChartChangeEventType.DATASET_UPDATED);
        notifyListeners(e);
      }
    };
    plot.setOrientation(orientation);
    if (tooltips) {
      renderer.setBaseToolTipGenerator(new StandardXYToolTipGenerator());
    }
    if (urls) {
      renderer.setURLGenerator(new StandardXYURLGenerator());
    }

    final JFreeChart chart = new JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT,
            plot, legend);

    return chart;
  }
}
