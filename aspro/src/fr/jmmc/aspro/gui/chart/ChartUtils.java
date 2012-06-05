/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import fr.jmmc.aspro.util.TimeFormat;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.Stroke;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.Map;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.LegendItem;
import org.jfree.chart.StandardChartTheme;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.axis.DateAxis;
import org.jfree.chart.axis.DateTickUnit;
import org.jfree.chart.axis.DateTickUnitType;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.NumberTickUnit;
import org.jfree.chart.axis.TickUnitSource;
import org.jfree.chart.axis.TickUnits;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.labels.StandardXYToolTipGenerator;
import org.jfree.chart.labels.XYToolTipGenerator;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.StandardXYBarPainter;
import org.jfree.chart.renderer.xy.XYBarRenderer;
import org.jfree.chart.renderer.xy.XYErrorRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.chart.title.LegendTitle;
import org.jfree.chart.title.TextTitle;
import org.jfree.chart.title.Title;
import org.jfree.chart.urls.StandardXYURLGenerator;
import org.jfree.data.xy.IntervalXYDataset;
import org.jfree.data.xy.XYDataset;
import org.jfree.text.TextUtilities;
import org.jfree.ui.RectangleEdge;
import org.jfree.ui.RectangleInsets;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Several static methods related to the JFreeChart library
 * @author bourgesl
 */
public final class ChartUtils {

  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(ChartUtils.class.getName());
  /** cache for annotation fonts to autofit size */
  private final static Map<Integer, Font> cachedFonts = new HashMap<Integer, Font>();
  /** The default font for titles. */
  public static final Font DEFAULT_TITLE_FONT = getFont(14, Font.BOLD);
  /** The default font for titles. */
  public static final Font DEFAULT_FONT = getFont(12);
  /** The default font for small texts */
  public static final Font DEFAULT_TEXT_SMALL_FONT = getFont(9);
  /** The default small font for annotation texts */
  public static final Font SMALL_TEXT_ANNOTATION_FONT = getFont(8);
  /** default draw stroke */
  public static final Stroke DEFAULT_STROKE = new BasicStroke(1.0f);
  /** thin draw stroke */
  public static final Stroke THIN_STROKE = new BasicStroke(0.5f);
  /** larger draw stroke */
  public static final Stroke LARGE_STROKE = new BasicStroke(1.25f);
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

      // use 'SansSerif' fonts:
      CHART_THEME.setExtraLargeFont(getFont(20, Font.BOLD)); /* new Font("Tahoma", Font.BOLD, 20) */
      CHART_THEME.setLargeFont(DEFAULT_TITLE_FONT); /* new Font("Tahoma", Font.BOLD, 14); */
      CHART_THEME.setRegularFont(DEFAULT_FONT); /* new Font("Tahoma", Font.PLAIN, 12); */
      CHART_THEME.setSmallFont(DEFAULT_TEXT_SMALL_FONT); /* new Font("Tahoma", Font.PLAIN, 10) */

    } else {
      throw new IllegalStateException("Unsupported chart theme : " + ChartFactory.getChartTheme());
    }
  }

  /**
   * Return new chart panel using special draw widths to avoid scaling effects
   * @param chart chart to use
   * @return chart panel
   */
  public static ChartPanel createChartPanel(final JFreeChart chart) {
    final ChartPanel panel = new ChartPanel(chart,
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

    panel.getChartRenderingInfo().setEntityCollection(null);
    return panel;
  }

  /**
   * Return new square chart panel using special draw widths to avoid scaling effects
   * @param chart chart to use
   * @return chart panel
   */
  public static SquareChartPanel createSquareChartPanel(final JFreeChart chart) {
    final SquareChartPanel panel = new SquareChartPanel(chart,
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

    panel.getChartRenderingInfo().setEntityCollection(null);
    return panel;
  }

  /**
   * Return the font (SansSerif / Plain) for the given size (cached)
   * @param size font size
   * @return annotation font
   */
  private static Font getFont(final int size) {
    return getFont(size, Font.PLAIN);
  }

  /**
   * Return the font (SansSerif / Plain) for the given size (cached)
   * @param size font size
   * @param style font style
   * @return annotation font
   */
  private static Font getFont(final int size, final int style) {
    final Integer key = Integer.valueOf(size);
    Font f = cachedFonts.get(key);
    if (f == null) {
      f = new Font("SansSerif", style, size);
      cachedFonts.put(key, f);
    }
    return f;
  }

  /**
   * Return the biggest font whose size best fits the given text for the given width
   * @param g2d graphics object
   * @param text text to use
   * @param maxWidth maximum pixel width to fit
   * @param minFontSize minimum size for the font
   * @param maxFontSize maximum size for the font
   * @param allowDontFit flag indicating to use the minimum font size if the text dont fit; null otherwise
   * @return font
   */
  static Font autoFitTextWidth(final Graphics2D g2d,
          final String text, final double maxWidth,
          final int minFontSize, final int maxFontSize,
          final boolean allowDontFit) {

    Font f;
    FontMetrics fm;

    int size = maxFontSize;
    double width;

    do {
      f = ChartUtils.getFont(size);

//      logger.info("font      : {}", f);
      fm = g2d.getFontMetrics(f);

      // get pixel width of the given text with the current font :
      width = TextUtilities.getTextBounds(text, g2d, fm).getWidth();

//      logger.info("width     : {}", width);

      size--;

    } while (width > maxWidth && size >= minFontSize);

    if (!allowDontFit && width > maxWidth) {
      f = null;
    }

    return f;
  }

  /**
   * Return the biggest font whose size best fits the given text for the given height
   * @param g2d graphics object
   * @param text text to use
   * @param maxHeight maximum pixel height to fit
   * @param minFontSize minimum size for the font
   * @param maxFontSize maximum size for the font
   * @param allowDontFit flag indicating to use the minimum font size if the text dont fit; null otherwise
   * @return font
   */
  static Font autoFitTextHeight(final Graphics2D g2d,
          final String text, final double maxHeight,
          final int minFontSize, final int maxFontSize,
          final boolean allowDontFit) {

    Font f;
    FontMetrics fm;

    int size = maxFontSize;
    double height;

    do {
      f = ChartUtils.getFont(size);

//      logger.info("font      : {}", f);
      fm = g2d.getFontMetrics(f);

      // get pixel height of the given text with the current font :
      height = TextUtilities.getTextBounds(text, g2d, fm).getHeight();

//      logger.info("height     : {}", height);

      size--;

    } while (height > maxHeight && size >= minFontSize);

    if (!allowDontFit && height > maxHeight) {
      f = null;
    }

    return f;
  }

  /**
   * Create an empty XYBarChart for the observability chart
   * @return jFreeChart instance
   */
  public static JFreeChart createXYBarChart() {
    // no title :
    final JFreeChart chart = createXYBarChart("", null, false, null, null, PlotOrientation.HORIZONTAL, true, false, false);

    final XYPlot xyPlot = (XYPlot) chart.getPlot();

    // enlarge right margin to have last displayed hour (00:00)
    xyPlot.setInsets(new RectangleInsets(2d, 10d, 2d, 20d));

    xyPlot.setDomainCrosshairVisible(false);
    // show crosshair on date axis :
    xyPlot.setRangeCrosshairLockedOnData(false);
    xyPlot.setRangeCrosshairVisible(false);

    xyPlot.getDomainAxis().setVisible(false);
    xyPlot.getRangeAxis().setVisible(false);

    // Adjust outline :
    xyPlot.setOutlineStroke(DEFAULT_STROKE);

    final XYBarRenderer xyBarRenderer = (XYBarRenderer) xyPlot.getRenderer();
    xyBarRenderer.setUseYInterval(true);
    xyBarRenderer.setDrawBarOutline(true);

    return chart;
  }

  /**
   * Creates and returns a default instance of an XY bar chart
   * BUT it uses an alternate XYPlot implementation to have gridlines visible even with range markers
   * <P>
   * The chart object returned by this method uses an {@link XYPlot} instance
   * as the plot, with a {@link DateAxis} for the domain axis, a
   * {@link NumberAxis} as the range axis, and a {@link XYBarRenderer} as the
   * renderer.
   *
   * @param title  the chart title (<code>null</code> permitted).
   * @param xAxisLabel  a label for the X-axis (<code>null</code> permitted).
   * @param dateAxis  make the domain axis display dates?
   * @param yAxisLabel  a label for the Y-axis (<code>null</code> permitted).
   * @param dataset  the dataset for the chart (<code>null</code> permitted).
   * @param orientation  the orientation (horizontal or vertical)
   *                     (<code>null</code> NOT permitted).
   * @param legend  a flag specifying whether or not a legend is required.
   * @param tooltips  configure chart to generate tool tips?
   * @param urls  configure chart to generate URLs?
   *
   * @return An XY bar chart.
   */
  public static JFreeChart createXYBarChart(final String title,
          final String xAxisLabel,
          final boolean dateAxis,
          final String yAxisLabel,
          final IntervalXYDataset dataset,
          final PlotOrientation orientation,
          final boolean legend,
          final boolean tooltips,
          final boolean urls) {

    if (orientation == null) {
      throw new IllegalArgumentException("Null 'orientation' argument.");
    }
    ValueAxis domainAxis = null;
    if (dateAxis) {
      domainAxis = new DateAxis(xAxisLabel);
    } else {
      final NumberAxis axis = new NumberAxis(xAxisLabel);
      axis.setAutoRangeIncludesZero(false);
      domainAxis = axis;
    }
    final ValueAxis valueAxis = new NumberAxis(yAxisLabel);

    final XYBarRenderer renderer = new XYBarRenderer();
    if (tooltips) {
      XYToolTipGenerator tt;
      if (dateAxis) {
        tt = StandardXYToolTipGenerator.getTimeSeriesInstance();
      } else {
        tt = new StandardXYToolTipGenerator();
      }
      renderer.setBaseToolTipGenerator(tt);
    }
    if (urls) {
      renderer.setURLGenerator(new StandardXYURLGenerator());
    }

    final XYPlot plot = new GridLineFixedXYPlot(dataset, domainAxis, valueAxis, renderer);
    plot.setOrientation(orientation);

    final JFreeChart chart = new JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT, plot, legend);

    return chart;
  }

  /**
   * Create the custom Square XY Line Chart for the UV coverage chart
   * @param xLabel label for the x axis (range)
   * @param yLabel label for the y axis (domain)
   * @param legend create a legend ?
   * @return jFreeChart instance
   */
  public static JFreeChart createSquareXYLineChart(final String xLabel, final String yLabel, final boolean legend) {
    final JFreeChart chart = createSquareXYLineChart(null, xLabel, yLabel, null, PlotOrientation.VERTICAL, legend, false, false);

    final SquareXYPlot xyPlot = (SquareXYPlot) chart.getPlot();

    // reset bounds to [-1;1]
    xyPlot.defineBounds(1);

    // display axes at [0,0] :
    xyPlot.setDomainZeroBaselineVisible(true);
    xyPlot.setRangeZeroBaselineVisible(true);

    // use custom units :
    xyPlot.getRangeAxis().setStandardTickUnits(ChartUtils.createScientificTickUnits());
    xyPlot.getDomainAxis().setStandardTickUnits(ChartUtils.createScientificTickUnits());

    // tick color :
    xyPlot.getRangeAxis().setTickMarkPaint(Color.BLACK);
    xyPlot.getDomainAxis().setTickMarkPaint(Color.BLACK);

    // Adjust outline :
    xyPlot.setOutlineStroke(DEFAULT_STROKE);

    final XYLineAndShapeRenderer lineAndShapeRenderer = (XYLineAndShapeRenderer) xyPlot.getRenderer();

    // force to use the base stroke :
    lineAndShapeRenderer.setAutoPopulateSeriesStroke(false);
    lineAndShapeRenderer.setBaseStroke(LARGE_STROKE);

    // update theme at end :
    ChartUtilities.applyCurrentTheme(chart);

    return chart;
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
  public static JFreeChart createSquareXYLineChart(final String title,
          final String xAxisLabel,
          final String yAxisLabel,
          final XYDataset dataset,
          final PlotOrientation orientation,
          final boolean legend,
          final boolean tooltips,
          final boolean urls) {

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

    final JFreeChart chart = new JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT, plot, legend);

    if (legend) {
      chart.getLegend().setPosition(RectangleEdge.RIGHT);
    }

    return chart;
  }

  /**
   * Creates a scatter plot (based on an {@link XYDataset}) with default
   * settings BUT bounded axes
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
  public static JFreeChart createScatterPlot(final String title,
          final String xAxisLabel,
          final String yAxisLabel,
          final XYDataset dataset,
          final PlotOrientation orientation,
          final boolean legend,
          final boolean tooltips,
          final boolean urls) {

    if (orientation == null) {
      throw new IllegalArgumentException("Null 'orientation' argument.");
    }

    // Axes are bounded to avoid zooming out where there is no data :

    final BoundedNumberAxis xAxis = new AutoBoundedNumberAxis(xAxisLabel);
    xAxis.setAutoRangeIncludesZero(false);
    final BoundedNumberAxis yAxis = new AutoBoundedNumberAxis(yAxisLabel);
    yAxis.setAutoRangeIncludesZero(false);

    // only lines are rendered :
//    final XYItemRenderer renderer = new XYLineAndShapeRenderer(false, true);
    final XYErrorRenderer renderer = new XYErrorRenderer();
    renderer.setDrawXError(false);

    // customized XYPlot to have a square data area :
    final XYPlot plot = new XYPlot(dataset, xAxis, yAxis, renderer);

    plot.setOrientation(orientation);
    if (tooltips) {
      renderer.setBaseToolTipGenerator(new StandardXYToolTipGenerator());
    }
    if (urls) {
      renderer.setURLGenerator(new StandardXYURLGenerator());
    }

    final JFreeChart chart = new JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT, plot, legend);

    if (legend) {
      chart.getLegend().setPosition(RectangleEdge.RIGHT);
    }

    return chart;
  }

  /**
   * Add a sub title to the given chart
   * @param chart chart to use
   * @param text sub title content
   */
  public static void addSubtitle(final JFreeChart chart, final String text) {
    chart.addSubtitle(new TextTitle(text, DEFAULT_TITLE_FONT));
  }

  /**
   * Clear the sub titles except the legend
   * @param chart chart to process
   */
  public static void clearTextSubTitle(final JFreeChart chart) {
    Title legend = null;
    Title title;
    for (int i = 0; i < chart.getSubtitleCount(); i++) {
      title = chart.getSubtitle(i);
      if (title instanceof LegendTitle) {
        legend = title;
        break;
      }
    }
    chart.clearSubtitles();
    if (legend != null) {
      chart.addSubtitle(legend);
    }
  }

  /**
   * Returns a default legend item for the specified series.
   * note : code inspired from XYBarRenderer.createLegendItem()
   *
   * @param xyBarRenderer XY bar renderer to get visual default attributes (font, colors ...)
   * @param label the legend label
   * @param paint the legend shape paint
   *
   * @return A legend item for the series.
   */
  public static LegendItem createLegendItem(final XYBarRenderer xyBarRenderer, final String label, final Paint paint) {

    // use first serie to get visual attributes :
    final int series = 0;

    final Shape shape = xyBarRenderer.getLegendBar();

    final Paint outlinePaint = xyBarRenderer.lookupSeriesOutlinePaint(series);
    final Stroke outlineStroke = xyBarRenderer.lookupSeriesOutlineStroke(series);

    final LegendItem item;
    if (xyBarRenderer.isDrawBarOutline()) {
      item = new LegendItem(label, null, null, null, shape, paint, outlineStroke, outlinePaint);
    } else {
      item = new LegendItem(label, null, null, null, shape, paint);
    }
    item.setLabelFont(xyBarRenderer.lookupLegendTextFont(series));
    final Paint labelPaint = xyBarRenderer.lookupLegendTextPaint(series);
    if (labelPaint != null) {
      item.setLabelPaint(labelPaint);
    }
    if (xyBarRenderer.getGradientPaintTransformer() != null) {
      item.setFillPaintTransformer(xyBarRenderer.getGradientPaintTransformer());
    }

    return item;
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
    final DecimalFormat df000 = new DecimalFormat("0.00");
    final DecimalFormat df00 = new DecimalFormat("0.0");
    final DecimalFormat df0 = new DecimalFormat("0");
    final DecimalFormat df3 = new DecimalFormat("0.0##E0");

    units.add(new NumberTickUnit(1e-10d, df3));
    units.add(new NumberTickUnit(5e-10d, df3));

    units.add(new NumberTickUnit(1e-9d, df3));
    units.add(new NumberTickUnit(5e-9d, df3));

    units.add(new NumberTickUnit(1e-8d, df3));
    units.add(new NumberTickUnit(5e-8d, df3));

    units.add(new NumberTickUnit(1e-7d, df3));
    units.add(new NumberTickUnit(5e-7d, df3));

    units.add(new NumberTickUnit(1e-6d, df3));
    units.add(new NumberTickUnit(5e-6d, df3));

    units.add(new NumberTickUnit(1e-5d, df3));
    units.add(new NumberTickUnit(5e-5d, df3));

    units.add(new NumberTickUnit(1e-4d, df3));
    units.add(new NumberTickUnit(5e-4d, df3));

    units.add(new NumberTickUnit(1e-3d, df3));
    units.add(new NumberTickUnit(5e-3d, df3));

    units.add(new NumberTickUnit(1e-2d, df000));
    units.add(new NumberTickUnit(5e-2d, df000));

    units.add(new NumberTickUnit(1e-1d, df00));
    units.add(new NumberTickUnit(5e-1d, df00));

    units.add(new NumberTickUnit(1d, df0));
    units.add(new NumberTickUnit(5d, df0));

    units.add(new NumberTickUnit(1e1d, df0));
    units.add(new NumberTickUnit(5e1d, df0));

    units.add(new NumberTickUnit(1e2d, df0));
    units.add(new NumberTickUnit(5e2d, df0));

    units.add(new NumberTickUnit(1e3d, df3));
    units.add(new NumberTickUnit(5e3d, df3));

    units.add(new NumberTickUnit(1e4d, df3));
    units.add(new NumberTickUnit(5e4d, df3));

    units.add(new NumberTickUnit(1e5d, df3));
    units.add(new NumberTickUnit(5e5d, df3));

    units.add(new NumberTickUnit(1e6d, df3));
    units.add(new NumberTickUnit(5e6d, df3));

    units.add(new NumberTickUnit(1e7d, df3));
    units.add(new NumberTickUnit(5e7d, df3));

    units.add(new NumberTickUnit(1e8d, df3));
    units.add(new NumberTickUnit(5e8d, df3));

    units.add(new NumberTickUnit(1e9d, df3));
    units.add(new NumberTickUnit(5e9d, df3));

    units.add(new NumberTickUnit(1e10d, df3));

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

    // minutes
    units.add(new DateTickUnit(DateTickUnitType.MINUTE, 15, DateTickUnitType.MINUTE, 5, tf));
    units.add(new DateTickUnit(DateTickUnitType.MINUTE, 30, DateTickUnitType.MINUTE, 5, tf));

    // hours
    units.add(new DateTickUnit(DateTickUnitType.HOUR, 1, DateTickUnitType.MINUTE, 5, tf));
    units.add(new DateTickUnit(DateTickUnitType.HOUR, 2, DateTickUnitType.MINUTE, 10, tf));
    units.add(new DateTickUnit(DateTickUnitType.HOUR, 3, DateTickUnitType.MINUTE, 30, tf));
    units.add(new DateTickUnit(DateTickUnitType.HOUR, 6, DateTickUnitType.HOUR, 1, tf));

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
    // TODO: fix buf for HA < 0.0 !!
    final DateFormat haf = new TimeFormat(true, false);
    /*    
    // minutes
    units.add(new DateTickUnit(DateTickUnitType.MINUTE, 15, DateTickUnitType.MINUTE, 5, haf));
    units.add(new DateTickUnit(DateTickUnitType.MINUTE, 30, DateTickUnitType.MINUTE, 5, haf));
     */
    // hours
    units.add(new DateTickUnit(DateTickUnitType.HOUR, 1, DateTickUnitType.MINUTE, 5, haf));
    units.add(new DateTickUnit(DateTickUnitType.HOUR, 2, DateTickUnitType.MINUTE, 10, haf));
    units.add(new DateTickUnit(DateTickUnitType.HOUR, 3, DateTickUnitType.MINUTE, 30, haf));
    units.add(new DateTickUnit(DateTickUnitType.HOUR, 6, DateTickUnitType.HOUR, 1, haf));

    return units;
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
   * @return new annotation
   */
  public static XYTextAnnotation createXYTextAnnotation(final String text, final double x, final double y) {
    final XYTextAnnotation a = new XYTextAnnotation(text, x, y);
    a.setFont(SMALL_TEXT_ANNOTATION_FONT);
    return a;
  }

  /**
   * Creates a new text annotation to be displayed at the given coordinates.  
   * The coordinates are specified in data space.
   *
   * @param text  the text (<code>null</code> not permitted).
   * @param x  the x-coordinate (in data space).
   * @param y  the y-coordinate (in data space).
   * @return new annotation
   */
  public static FitXYTextAnnotation createFitXYTextAnnotation(final String text, final double x, final double y) {
    final FitXYTextAnnotation a = new FitXYTextAnnotation(text, x, y);
    // font is determined automatically (auto-fit)
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
