/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ChartUtils.java,v 1.7 2010-02-04 17:05:06 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.6  2010/02/03 09:48:52  bourgesl
 * target model uvmap added on the uv coverage with zooming supported
 *
 * Revision 1.5  2010/01/19 11:00:50  bourgesl
 * changed base line stroke
 *
 * Revision 1.4  2010/01/14 17:03:06  bourgesl
 * No more gradient paint + smaller bar width
 *
 * Revision 1.3  2010/01/12 16:53:34  bourgesl
 * customized JFreeChart classes to get a square XY Plot supporting zooming in/out with mouse and mouse wheel
 *
 * Revision 1.2  2010/01/08 16:51:17  bourgesl
 * initial uv coverage
 *
 */
package fr.jmmc.aspro.gui.chart;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.text.DecimalFormat;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.StandardChartTheme;
import org.jfree.chart.axis.NumberTickUnit;
import org.jfree.chart.axis.TickUnitSource;
import org.jfree.chart.axis.TickUnits;
import org.jfree.chart.labels.StandardXYToolTipGenerator;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.StandardXYBarPainter;
import org.jfree.chart.renderer.xy.XYBarRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.chart.title.LegendTitle;
import org.jfree.chart.title.TextTitle;
import org.jfree.chart.title.Title;
import org.jfree.chart.urls.StandardXYURLGenerator;
import org.jfree.data.xy.XYDataset;
import org.jfree.ui.RectangleEdge;
import org.jfree.ui.RectangleInsets;

/**
 * Several static methods related to the JFreeChart library
 * @author bourgesl
 */
public class ChartUtils {

  /** The default font for titles. */
  private static final Font DEFAULT_TITLE_FONT = new Font("SansSerif", Font.BOLD, 14);
  /** scientific tick units singleton */
  private static TickUnits SCIENTIFIC_TICK_UNITS = null;
  /** default tick label rectangle insets */
  public final static RectangleInsets TICK_LABEL_INSETS = new RectangleInsets(1.0, 1.0, 1.0, 1.0);
  /** default axis offset */
  public final static RectangleInsets ZERO_AXIS_OFFSET = new RectangleInsets(0.0, 0.0, 0.0, 0.0);

  /**
   * Forbidden constructor
   */
  private ChartUtils() {
    // no-op
  }

  static {

    // Change the default chart theme before creating any chart :
    if (ChartFactory.getChartTheme() instanceof StandardChartTheme) {
      final StandardChartTheme theme = (StandardChartTheme) ChartFactory.getChartTheme();

      // Disable Bar shadows :
      theme.setShadowVisible(false);

      // Disable Bar gradient :
      theme.setXYBarPainter(new StandardXYBarPainter());

      // Axis offset = gap between the axis line and the data area :
      theme.setAxisOffset(ZERO_AXIS_OFFSET);

      // plot outline :
      theme.setPlotOutlinePaint(Color.BLACK);

      // axis colors :
      theme.setAxisLabelPaint(Color.BLACK);
      theme.setTickLabelPaint(Color.BLACK);
    }

  }

  /**
   * Create an empty XYBarChart for the observability chart
   * @return jFreeChart instance
   */
  public static JFreeChart createXYBarChart() {
    // no title :
    final JFreeChart localJFreeChart = ChartFactory.createXYBarChart("", null, false, null, null, PlotOrientation.HORIZONTAL, false, false, false);

    final XYPlot localXYPlot = (XYPlot) localJFreeChart.getPlot();
    localXYPlot.setNoDataMessage("NO DATA");

    localXYPlot.getDomainAxis().setVisible(false);
    localXYPlot.getRangeAxis().setVisible(false);

    // Adjust outline :
    localXYPlot.setOutlineStroke(new BasicStroke(1.f));

    final XYBarRenderer localXYBarRenderer = (XYBarRenderer) localXYPlot.getRenderer();
    localXYBarRenderer.setUseYInterval(true);
    localXYBarRenderer.setDrawBarOutline(true);

    return localJFreeChart;
  }

  /**
   * Create the custom Square XY Line Chart for the UV coverage chart
   * @param xLabel label for the x axis (range)
   * @param yLabel label for the y axis (domain)
   * @return jFreeChart instance
   */
  public static JFreeChart createSquareXYLineChart(final String xLabel, final String yLabel) {
    final JFreeChart localJFreeChart = createSquareXYLineChart(null, xLabel, yLabel, null, PlotOrientation.VERTICAL, true, false, false);

    final SquareXYPlot localXYPlot = (SquareXYPlot) localJFreeChart.getPlot();
    localXYPlot.setNoDataMessage("NO DATA");

    // show crosshair :
    /*
    localXYPlot.setDomainCrosshairVisible(true);
    localXYPlot.setRangeCrosshairVisible(true);
     */

    // reset bounds to [-1;1]
    localXYPlot.defineBounds(1);

    // display axes at [0,0] :
    localXYPlot.setDomainZeroBaselineVisible(true);
    localXYPlot.setRangeZeroBaselineVisible(true);

    // use custom units :
    localXYPlot.getRangeAxis().setStandardTickUnits(ChartUtils.createScientificTickUnits());
    localXYPlot.getDomainAxis().setStandardTickUnits(ChartUtils.createScientificTickUnits());

    // tick color :
    localXYPlot.getRangeAxis().setTickMarkPaint(Color.BLACK);
    localXYPlot.getDomainAxis().setTickMarkPaint(Color.BLACK);

    final XYLineAndShapeRenderer localLineAndShapeRenderer = (XYLineAndShapeRenderer) localXYPlot.getRenderer();

    // force to use the base stroke :
    localLineAndShapeRenderer.setAutoPopulateSeriesStroke(false);
    localLineAndShapeRenderer.setBaseStroke(new BasicStroke(1.25F));

    // update theme at end :
    ChartUtilities.applyCurrentTheme(localJFreeChart);

    return localJFreeChart;
  }

  /**
   * Creates a line chart (based on an {@link XYDataset}) with default
   * settings BUT using a Square data area with consistent zooming in/out
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

    // Axes are bounded to avoid zooming out where there is no data :

    final BoundedNumberAxis xAxis = new BoundedNumberAxis(xAxisLabel);
    xAxis.setAutoRangeIncludesZero(false);
    final BoundedNumberAxis yAxis = new BoundedNumberAxis(yAxisLabel);
    yAxis.setAutoRangeIncludesZero(false);

    // only lines are rendered :
    final XYItemRenderer renderer = new XYLineAndShapeRenderer(true, false);

    // customized XYPlot to have a square data area :
    final XYPlot plot = new SquareXYPlot(dataset, xAxis, yAxis, renderer);

    plot.setOrientation(orientation);
    if (tooltips) {
      renderer.setBaseToolTipGenerator(new StandardXYToolTipGenerator());
    }
    if (urls) {
      renderer.setURLGenerator(new StandardXYURLGenerator());
    }

    final JFreeChart chart = new JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT,
            plot, legend);

    chart.getLegend().setPosition(RectangleEdge.RIGHT);

    return chart;
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
   * Clear the sub titles except the legend
   * @param localJFreeChart chart to process
   */
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
      final DecimalFormat df1 = new DecimalFormat("0.0E0");

      units.add(new NumberTickUnit(1, df0));
      units.add(new NumberTickUnit(5, df0));
      units.add(new NumberTickUnit(10, df0));
      units.add(new NumberTickUnit(50, df0));
      units.add(new NumberTickUnit(100, df0));
      units.add(new NumberTickUnit(500, df0));

      units.add(new NumberTickUnit(1000, df1));
      units.add(new NumberTickUnit(5000, df1));
      units.add(new NumberTickUnit(10000, df1));
      units.add(new NumberTickUnit(50000, df1));
      units.add(new NumberTickUnit(100000, df1));
      units.add(new NumberTickUnit(500000, df1));
      units.add(new NumberTickUnit(1000000, df1));
      units.add(new NumberTickUnit(5000000, df1));
      units.add(new NumberTickUnit(10000000, df1));
      units.add(new NumberTickUnit(50000000, df1));
      units.add(new NumberTickUnit(100000000, df1));
      units.add(new NumberTickUnit(500000000, df1));
      units.add(new NumberTickUnit(1000000000, df1));
      units.add(new NumberTickUnit(5000000000.0, df1));
      units.add(new NumberTickUnit(10000000000.0, df1));

      SCIENTIFIC_TICK_UNITS = units;
    }
    return SCIENTIFIC_TICK_UNITS;
  }
}
