/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import fr.jmmc.aspro.model.observability.StarObservabilityData;
import fr.jmmc.aspro.model.observability.TargetPositionDate;
import fr.jmmc.aspro.model.oi.Target;
import java.awt.Color;
import java.awt.Paint;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.annotations.XYAnnotation;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.labels.XYToolTipGenerator;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYBarRenderer;
import org.jfree.data.Range;
import org.jfree.data.gantt.TaskSeries;
import org.jfree.data.gantt.TaskSeriesCollection;
import org.jfree.data.gantt.XYTaskDataset;
import org.jfree.data.time.TimePeriod;
import org.jfree.data.xy.XYDataset;
import org.jfree.ui.Layer;

/**
 * This class is a custom XYPlot adapter to provide both a sliding dataset and annotations in sync.
 * @author bourgesl
 */
public final class SlidingXYPlotAdapter implements XYToolTipGenerator {

  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(SlidingXYPlotAdapter.class.getName());
  /* members */
  /** jFreeChart instance */
  private final JFreeChart chart;
  /** xy plot instance */
  private final XYPlot xyPlot;
  /** JMMC annotation */
  private XYTextAnnotation aJMMC = null;
  /** plot renderer to define annotations */
  private final XYBarRenderer renderer;
  /** max items in the chart view if the useSubset mode is enabled */
  private int maxViewItems;
  /** flag to enable/disable the subset mode */
  private boolean useSubset = false;
  /** current position of the subset */
  private int position = 0;
  /** number of data items */
  private int size;
  /** data collection */
  private TaskSeriesCollection collection = null;
  /** data symbols (targets) */
  private List<String> symbols = null;
  /** data colors (targets) */
  private List<Paint> colors = null;
  /** annotations */
  private Map<Integer, List<XYAnnotation>> annotations = null;
  /** last used start position for the subset */
  private int lastStart = -1;
  /** last used end position for the subset */
  private int lastEnd = -1;
  /* tooltip information */
  /** target list for tooltip generation */
  private List<Target> targetList = null;
  /** data labels (legend) */
  private List<String> labels = null;
  /** StarObservabilityData list for tooltip generation */
  private List<StarObservabilityData> soTargetList = null;
  /** 24h date formatter like in france */
  private final DateFormat timeFormatter = DateFormat.getTimeInstance(DateFormat.SHORT, Locale.FRANCE);
  /** double formatter for HA */
  private final NumberFormat df1 = new DecimalFormat("0.0");

  /**
   * Constructor
   * @param chart chart instance
   * @param plot xy plot
   * @param maxElements max items in the chart view
   * @param aJMMC JMMC annotation instance to be added to renderer's annotations
   */
  public SlidingXYPlotAdapter(final JFreeChart chart, final XYPlot plot, final int maxElements, final XYTextAnnotation aJMMC) {
    this.chart = chart;
    this.xyPlot = plot;
    this.renderer = (XYBarRenderer) plot.getRenderer();
    this.renderer.setBaseToolTipGenerator(this);
    this.maxViewItems = maxElements;
    this.aJMMC = aJMMC;
  }

  /**
   * Define the new data
   * @param collection data collection
   * @param symbols data symbols
   * @param colors data colors
   * @param annotations map of annotations keyed by position
   * @param targetList target list for tooltip generation
   * @param labels data labels (legend)
   * @param soTargetList StarObservabilityData list for tooltip generation
   * @param sdTargetList StarData list for tooltip generation
   */
  public void setData(final TaskSeriesCollection collection, final List<String> symbols, final List<Paint> colors,
          final Map<Integer, List<XYAnnotation>> annotations,
          final List<Target> targetList, final List<String> labels,
          final List<StarObservabilityData> soTargetList) {
    this.size = symbols.size();
    this.collection = collection;
    this.symbols = symbols;
    this.colors = colors;
    this.annotations = annotations;
    this.targetList = targetList;
    this.labels = labels;
    this.soTargetList = soTargetList;
  }

  /**
   * Return the max items in the chart view when the useSubset mode is enabled
   * @return max items in the chart view
   */
  public int getMaxViewItems() {
    return maxViewItems;
  }

  /**
   * Define the max items in the chart view when the useSubset mode is enabled
   * @param maxViewItems max items in the chart view
   */
  public void setMaxViewItems(final int maxViewItems) {
    this.maxViewItems = maxViewItems;
  }

  /**
   * Return the number of data items
   * @return number of data items
   */
  public int getSize() {
    return size;
  }

  /**
   * Return the current position of the subset
   * @return current position of the subset
   */
  public int getPosition() {
    return position;
  }

  /**
   * Define the new position of the subset and refresh the plot
   *
   * @param pos new position of the subset
   */
  public void setPosition(final int pos) {
    this.position = pos;
    if (this.useSubset) {
      updatePlot(this.maxViewItems, false);
    }
  }

  /**
   * Return the flag to enable/disable the subset mode
   * @return flag to enable/disable the subset mode
   */
  public boolean isUseSubset() {
    return this.useSubset;
  }

  /**
   * Define the flag to enable/disable the subset mode and force an update of the plot
   * @param useSubset new value
   */
  public void setUseSubset(final boolean useSubset) {
    if (logger.isDebugEnabled()) {
      logger.debug("useSubset: {}", useSubset);
    }

    this.useSubset = useSubset;
    if (useSubset) {
      updatePlot(this.maxViewItems, true);
    } else {
      updatePlot(this.size, true);
    }
  }

  /**
   * Update the plot
   * @param max number of items to display
   * @param forceUpdate flag to force an update of the plot
   */
  private void updatePlot(final int max, final boolean forceUpdate) {

    int start = this.position;
    if (start < 0) {
      start = 0;
    }

    int end = start + max;
    if (end > this.size) {
      end = this.size;
      start = end - max;
      if (start < 0) {
        start = 0;
      }
    }

    if (!forceUpdate && start == this.lastStart && end == this.lastEnd) {
      if (logger.isDebugEnabled()) {
        logger.debug("same positions : ignoring ({} to {})", start, end);
      }
      return;
    }

    this.lastStart = start;
    this.lastEnd = end;

    final int newSize = end - start;

    if (logger.isDebugEnabled()) {
      logger.debug("updatePlot: pos = {} :: ({} to {})", new Object[]{this.position, start, end});
    }

    final double barWidth;
    final double rangeMin, rangeMax;

    if (newSize > 0) {
      rangeMin = -(2d / 3d);
      rangeMax = newSize - (1d / 3d);
    } else {
      rangeMin = 0d;
      rangeMax = 1d;
    }

    // adjust bar width :
    if (newSize > 15) {
      barWidth = 0.8d;
    } else if (newSize > 6) {
      barWidth = 0.6d;
    } else if (newSize > 1) {
      barWidth = 0.5d;
    } else {
      barWidth = 0.25d;
    }

    // disable chart & plot notifications:
    final boolean savedNotify = this.chart.isNotify();
    this.chart.setNotify(false);
    this.xyPlot.setNotify(false);
    try {

      // reset colors :
      this.renderer.clearSeriesPaints(false);
      // side effect with chart theme :
      this.renderer.setAutoPopulateSeriesPaint(false);

      final TaskSeriesCollection subTaskSeriesCollection = new TaskSeriesCollection();

      final String[] subSymbols = new String[newSize];

      for (int i = start, n = 0; i < end; i++, n++) {
        subSymbols[n] = this.symbols.get(i);

        subTaskSeriesCollection.add(this.collection.getSeries(i));

        // color :
        this.renderer.setSeriesPaint(n, this.colors.get(i), false);
      }

      final XYTaskDataset dataset = new XYTaskDataset(subTaskSeriesCollection);

      dataset.setSeriesWidth(barWidth);

      // update dataset :
      this.xyPlot.setDataset(dataset);

      // change the Domain axis (vertical) :
      final BoundedSymbolAxis symbolAxis = new BoundedSymbolAxis(null, subSymbols);
      symbolAxis.setInverted(true);
      symbolAxis.setGridBandsVisible(false);
      symbolAxis.setBounds(new Range(rangeMin, rangeMax));
      symbolAxis.setRange(rangeMin, rangeMax);
      this.xyPlot.setDomainAxis(symbolAxis);

      // remove Annotations :
      this.renderer.removeAnnotations();

      // annotations :
      if (this.annotations != null) {

        final ObservabilityPlotContext renderContext = ObservabilityPlotContext.getInstance();

        // set text maximum width = bar width :
        renderContext.setMaxTextWidth(barWidth);

        // set diamond maximum size = 85% bar width :
        renderContext.setMaxDiamondWidth(barWidth * 0.85d);

        // set tip radius = 50% bar width (tick location over bar edges) :
        renderContext.setTipRadius(barWidth * 0.5d);

        // set max tip height = margin between bars (half tick + text) :
        renderContext.setMaxTipHeight(0.5d * (1d - barWidth));


        // Redefine the x-position of annotations (corresponding to visible targets) :

        List<XYAnnotation> list;
        Integer pos;
        for (int i = start, n = 0; i < end; i++, n++) {
          pos = Integer.valueOf(i);

          list = this.annotations.get(pos);

          if (list != null) {

            for (XYAnnotation annotation : list) {

              if (annotation instanceof XYTextAnnotation) {
                // Applies to XYTickAnnotation also :
                final XYTextAnnotation a = (XYTextAnnotation) annotation;
                a.setX(n);
                this.renderer.addAnnotation(a);

              } else if (annotation instanceof XYDiamondAnnotation) {
                final XYDiamondAnnotation a = (XYDiamondAnnotation) annotation;
                a.setX(n);
                this.renderer.addAnnotation(a);
              }
            }
          }
        }
      }

      // add JMMC annotation (moving position):
      this.renderer.addAnnotation(this.aJMMC, Layer.BACKGROUND);

      // tick color :
      this.xyPlot.getRangeAxis().setTickMarkPaint(Color.BLACK);
      this.xyPlot.getDomainAxis().setTickMarkPaint(Color.BLACK);

      // update theme at end :
      ChartUtilities.applyCurrentTheme(this.chart);

    } finally {
      // restore chart & plot notifications:
      this.xyPlot.setNotify(savedNotify);
      this.chart.setNotify(savedNotify);
    }
  }

  /**
   * Generates the tooltip text for the specified item.
   *
   * @param dataset  the dataset (<code>null</code> not permitted).
   * @param series  the series index (zero-based).
   * @param item  the item index (zero-based).
   *
   * @return The tooltip text (possibly <code>null</code>).
   */
  public String generateToolTip(final XYDataset dataset, final int series, final int item) {
    // note: series corresponds to the target, item to its observability ranges
    if (targetList != null && !targetList.isEmpty()) {
      final int index = this.lastStart + series;

      final Target target = targetList.get(index);

      if (target != null) {
        logger.debug("target= {}", target);

        final String label = labels.get(index);

        final StringBuilder sb = new StringBuilder(255);
        sb.append("<b>").append(label).append(" Observability</b>");

        final TaskSeries taskSeries = this.collection.getSeries(index);

        // use interval or star data:
        final TimePeriod period = taskSeries.get(item).getDuration();

        final StarObservabilityData starObs = this.soTargetList.get(index);
        if (starObs != null) {

          // note: often the start or end TargetPositionDate can not be found
          // when the interval has been splitted due to night overlaps (RA)

          Date date = period.getStart();
          TargetPositionDate pos = starObs.getTargetPosition(date);

          if (pos != null) {
            sb.append("<br><b>Start</b>: ").append(this.timeFormatter.format(date));
            sb.append(" - <b>HA</b>: ").append(this.df1.format(pos.getHa()));
            sb.append(" (az ").append(pos.getAzimuth());
            sb.append(", el ").append(pos.getElevation()).append(")");
          }

          date = period.getEnd();
          pos = starObs.getTargetPosition(date);

          if (pos != null) {
            sb.append("<br><b>End</b>: ").append(this.timeFormatter.format(date));
            sb.append(" - <b>HA</b>: ").append(this.df1.format(pos.getHa()));
            sb.append(" (az ").append(pos.getAzimuth());
            sb.append(", el ").append(pos.getElevation()).append(")");
          }

          date = starObs.getTransitDate();
          pos = starObs.getTargetPosition(date);

          if (pos != null) {
            sb.append("<br><b>Transit</b>: ").append(this.timeFormatter.format(date));
            sb.append(" - <b>HA</b>: ").append(this.df1.format(pos.getHa()));
            sb.append(" (az ").append(pos.getAzimuth());
            sb.append(", el ").append(pos.getElevation()).append(")");
          }
        }

        return target.toHtml(sb.toString(), false);
      }
    }
    return null;
  }
}
