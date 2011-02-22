/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ChartUtils.java,v 1.15 2011-02-22 18:10:16 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.14  2011/01/25 12:29:36  bourgesl
 * fixed javadoc errors
 *
 * Revision 1.13  2010/10/21 16:50:11  bourgesl
 * added autoFitFont and several factory methods related to annotations to set default font
 *
 * Revision 1.12  2010/10/18 14:27:43  bourgesl
 * reduce text annotation size
 *
 * Revision 1.11  2010/10/08 12:30:53  bourgesl
 * added tests with cross hairs
 *
 * Revision 1.10  2010/10/01 15:35:42  bourgesl
 * removed 'NO DATA' message on plots
 * new Time and HA tick units factory methods
 *
 * Revision 1.9  2010/05/11 12:01:54  bourgesl
 * added createLegend argument to createSquareXYLineChart(...)
 *
 * Revision 1.8  2010/04/02 10:04:18  bourgesl
 * added diamond shape
 *
 * Revision 1.7  2010/02/04 17:05:06  bourgesl
 * UV bounds are coming from UVCoverageService
 *
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

import fr.jmmc.aspro.util.TimeFormat;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.Map;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.StandardChartTheme;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.axis.DateTickUnit;
import org.jfree.chart.axis.DateTickUnitType;
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
import org.jfree.text.TextUtilities;
import org.jfree.ui.RectangleEdge;
import org.jfree.ui.RectangleInsets;

/**
 * Several static methods related to the JFreeChart library
 * @author bourgesl
 */
public final class ChartUtils {

  /** cache for annotation fonts to autofit size */
  private final static Map<Integer, Font> cachedFonts = new HashMap<Integer, Font>();
  /** The default font for titles. */
  public static final Font DEFAULT_TITLE_FONT = new Font("SansSerif", Font.BOLD, 14);
  /** The default font for annotation texts */
  public static final Font DEFAULT_TEXT_ANNOTATION_FONT = getAnnotationFont(9);
  /** The default small font for annotation texts */
  public static final Font SMALL_TEXT_ANNOTATION_FONT = getAnnotationFont(8);
  /** default tick label rectangle insets */
  public final static RectangleInsets TICK_LABEL_INSETS = new RectangleInsets(2.0, 2.0, 2.0, 2.0);
  /** default axis offset */
  public final static RectangleInsets ZERO_AXIS_OFFSET = new RectangleInsets(0.0, 0.0, 0.0, 0.0);
  /** custom chart theme */
  public final static StandardChartTheme CHART_THEME;
  /** The default panel width. */
  public static final int DEFAULT_WIDTH = 600;
  /** The default panel height. */
  public static final int DEFAULT_HEIGHT = 400;
  /** The default limit below which chart scaling kicks in. */
  public static final int DEFAULT_MINIMUM_DRAW_WIDTH = 300;
  /** The default limit below which chart scaling kicks in. */
  public static final int DEFAULT_MINIMUM_DRAW_HEIGHT = 200;
  /** The default limit above which chart scaling kicks in. */
  public static final int DEFAULT_MAXIMUM_DRAW_WIDTH = 2560;
  /** The default limit above which chart scaling kicks in. */
  public static final int DEFAULT_MAXIMUM_DRAW_HEIGHT = 2048;

  /**
   * Forbidden constructor
   */
  private ChartUtils() {
    // no-op
  }

  static {

    // Change the default chart theme before creating any chart :
    if (ChartFactory.getChartTheme() instanceof StandardChartTheme) {
      CHART_THEME = (StandardChartTheme) ChartFactory.getChartTheme();

      // Disable Bar shadows :
      CHART_THEME.setShadowVisible(false);

      // Disable Bar gradient :
      CHART_THEME.setXYBarPainter(new StandardXYBarPainter());

      // Axis offset = gap between the axis line and the data area :
      CHART_THEME.setAxisOffset(ZERO_AXIS_OFFSET);

      // plot outline :
      CHART_THEME.setPlotOutlinePaint(Color.BLACK);

      // axis colors :
      CHART_THEME.setAxisLabelPaint(Color.BLACK);
      CHART_THEME.setTickLabelPaint(Color.BLACK);

      // text annotations :
      CHART_THEME.setItemLabelPaint(Color.BLACK);
      CHART_THEME.setSmallFont(DEFAULT_TEXT_ANNOTATION_FONT);
    } else {
      throw new IllegalStateException("unsupported chart theme : " + ChartFactory.getChartTheme());
    }
  }

  /**
   * Return new chart panel using special draw widths to avoid scaling effects
   * @param chart chart to use
   * @return chart panel
   */
  public static ChartPanel createChartPanel(final JFreeChart chart) {
    return new ChartPanel(chart,
            DEFAULT_WIDTH, DEFAULT_HEIGHT, /* prefered size */
            DEFAULT_MINIMUM_DRAW_WIDTH, DEFAULT_MINIMUM_DRAW_HEIGHT, /* minimum size before scaling */
            DEFAULT_MAXIMUM_DRAW_WIDTH, DEFAULT_MAXIMUM_DRAW_HEIGHT, /* maximum size before scaling */
            true, /* use buffer */
            false, /* properties */
            true, /* copy */
            true, /* save */
            true, /* print */
            false, /* zoom */
            false /* tooltips */);
  }

  /**
   * Return new square chart panel using special draw widths to avoid scaling effects
   * @param chart chart to use
   * @return chart panel
   */
  public static SquareChartPanel createSquareChartPanel(final JFreeChart chart) {
    return new SquareChartPanel(chart,
            DEFAULT_HEIGHT, DEFAULT_HEIGHT, /* prefered size */
            DEFAULT_MINIMUM_DRAW_HEIGHT, DEFAULT_MINIMUM_DRAW_HEIGHT, /* minimum size before scaling */
            DEFAULT_MAXIMUM_DRAW_HEIGHT, DEFAULT_MAXIMUM_DRAW_HEIGHT, /* maximum size before scaling */
            true, /* use buffer */
            false, /* properties */
            true, /* copy */
            true, /* save */
            true, /* print */
            false, /* zoom */
            false /* tooltips */);
  }

  /**
   * Return the annotation font for the given size (cached)
   * @param size font size
   * @return annotation font
   */
  protected static Font getAnnotationFont(final int size) {
    final Integer key = Integer.valueOf(size);
    Font f = cachedFonts.get(key);
    if (f == null) {
      f = new Font("SansSerif", Font.PLAIN, size);
      cachedFonts.put(key, f);
    }
    return f;
  }

  /**
   * Return the biggest font whose size best fits the given text for the given width
   * @param text text to use
   * @param maxWidth maximum pixel width to fit
   * @param g2 graphics object
   * @param minFontSize minimum size for the font
   * @param maxFontSize maximum size for the font
   * @return font
   */
  protected static Font autoFitText(final String text, final double maxWidth,
                                    final Graphics2D g2, final int minFontSize, final int maxFontSize) {

    Font f;
    FontMetrics fm;

    int size = maxFontSize;
    double width = 0;

    do {
      f = ChartUtils.getAnnotationFont(size);

//      System.out.println("font      = " + f);
      fm = g2.getFontMetrics(f);

      // get pixel width of the given text with the current font :
      width = TextUtilities.getTextBounds(text, g2, fm).getWidth();

//      System.out.println("width     = " + width);

      size--;

    } while (width > maxWidth && size > minFontSize);

    return f;
  }

  /**
   * Create an empty XYBarChart for the observability chart
   * @return jFreeChart instance
   */
  public static JFreeChart createXYBarChart() {
    // no title :
    final JFreeChart localJFreeChart = ChartFactory.createXYBarChart("", null, false, null, null, PlotOrientation.HORIZONTAL, false, false, false);

    final XYPlot localXYPlot = (XYPlot) localJFreeChart.getPlot();

    localXYPlot.setDomainCrosshairVisible(false);
    // show crosshair on date axis :
    localXYPlot.setRangeCrosshairLockedOnData(false);
    localXYPlot.setRangeCrosshairVisible(false);

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
   * @param legend create a legend ?
   * @return jFreeChart instance
   */
  public static JFreeChart createSquareXYLineChart(final String xLabel, final String yLabel, final boolean legend) {
    final JFreeChart localJFreeChart = createSquareXYLineChart(null, xLabel, yLabel, null, PlotOrientation.VERTICAL, legend, false, false);

    final SquareXYPlot localXYPlot = (SquareXYPlot) localJFreeChart.getPlot();

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

    if (legend) {
      chart.getLegend().setPosition(RectangleEdge.RIGHT);
    }

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
   * @see org.jfree.chart.axis.ValueAxis#setStandardTickUnits(TickUnitSource)
   * @see org.jfree.chart.axis.NumberAxis#createStandardTickUnits()
   */
  public static TickUnitSource createScientificTickUnits() {
    final TickUnits units = new TickUnits();
    final DecimalFormat df0 = new DecimalFormat("0");
    final DecimalFormat df1 = new DecimalFormat("0.0E0");

    // note : number lower than 1 are not supported !

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

    return units;
  }

  /**
   * Returns a collection of tick units for time values (hours and minutes).
   *
   * @return A collection of tick units for time values.
   *
   * @see org.jfree.chart.axis.ValueAxis#setStandardTickUnits(TickUnitSource)
   * @see org.jfree.chart.axis.DateAxis#createStandardDateTickUnits()
   */
  public static TickUnitSource createTimeTickUnits() {

    final TickUnits units = new TickUnits();

    // H:MM format :
    final DateFormat tf = new TimeFormat(false, true);

    // hours
    units.add(new DateTickUnit(DateTickUnitType.HOUR, 1,
            DateTickUnitType.MINUTE, 5, tf));
    units.add(new DateTickUnit(DateTickUnitType.HOUR, 2,
            DateTickUnitType.MINUTE, 10, tf));
    units.add(new DateTickUnit(DateTickUnitType.HOUR, 3,
            DateTickUnitType.MINUTE, 30, tf));
    units.add(new DateTickUnit(DateTickUnitType.HOUR, 6,
            DateTickUnitType.HOUR, 1, tf));

    return units;
  }

  /**
   * Returns a collection of tick units for time values (hours and minutes).
   *
   * @return A collection of tick units for time values.
   *
   * @see org.jfree.chart.axis.ValueAxis#setStandardTickUnits(TickUnitSource)
   * @see org.jfree.chart.axis.DateAxis#createStandardDateTickUnits()
   */
  public static TickUnitSource createHourAngleTickUnits() {

    final TickUnits units = new TickUnits();

    // HA format :
    final DateFormat haf = new TimeFormat(true, false);

    // hours
    units.add(new DateTickUnit(DateTickUnitType.HOUR, 1,
            DateTickUnitType.MINUTE, 5, haf));
    units.add(new DateTickUnit(DateTickUnitType.HOUR, 2,
            DateTickUnitType.MINUTE, 10, haf));
    units.add(new DateTickUnit(DateTickUnitType.HOUR, 3,
            DateTickUnitType.MINUTE, 30, haf));
    units.add(new DateTickUnit(DateTickUnitType.HOUR, 6,
            DateTickUnitType.HOUR, 1, haf));

    return units;
  }

  /**
   * Creates a new text annotation to be displayed at the given coordinates.  The
   * coordinates are specified in data space.
   *
   * HACK : use default text annotation font
   *
   * @param text  the text (<code>null</code> not permitted).
   * @param x  the x-coordinate (in data space).
   * @param y  the y-coordinate (in data space).
   * @return new annotation
   */
  public static XYTextAnnotation createXYTextAnnotation(final String text, final double x, final double y) {
    final XYTextAnnotation a = new XYTextAnnotation(text, x, y);
    a.setFont(DEFAULT_TEXT_ANNOTATION_FONT);
    // default color is BLACK

    return a;
  }

  /**
   * Creates a new text annotation to be displayed at the given coordinates.  The
   * coordinates are specified in data space.
   *
   * HACK : use default text annotation font
   *
   * @param text  the text (<code>null</code> not permitted).
   * @param x  the x-coordinate (in data space).
   * @param y  the y-coordinate (in data space).
   * @return new annotation
   */
  public static FitXYTextAnnotation createFitXYTextAnnotation(final String text, final double x, final double y) {
    final FitXYTextAnnotation a = new FitXYTextAnnotation(text, x, y);
    a.setFont(DEFAULT_TEXT_ANNOTATION_FONT);
    // default color is BLACK

    return a;
  }

  /**
   * Creates a new text annotation to be displayed at the given coordinates.  The
   * coordinates are specified in data space.
   *
   * HACK : use small text annotation font
   *
   * @param text  the text (<code>null</code> not permitted).
   * @param x  the x-coordinate (in data space).
   * @param y  the y-coordinate (in data space).
   * @param angle  the angle of the arrow's line (in radians).
   * @return new annotation
   */
  public static XYTickAnnotation createXYTickAnnotation(final String text, final double x, final double y,
                                                        final double angle) {

    final XYTickAnnotation a = new XYTickAnnotation(text, x, y, angle);
    a.setFont(SMALL_TEXT_ANNOTATION_FONT);
    // default color is BLACK

    return a;
  }
}
