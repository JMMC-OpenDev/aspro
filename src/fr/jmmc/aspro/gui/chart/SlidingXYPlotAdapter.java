/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SlidingXYPlotAdapter.java,v 1.7 2011-04-14 14:36:22 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.6  2011/04/13 14:38:50  bourgesl
 * refactored annotations to use the plot context (only perform text / block fitting once)
 * support autofit size (height / width) for fonts / marks / ticks
 *
 * Revision 1.5  2011/03/01 17:14:00  bourgesl
 * renamed variables
 *
 * Revision 1.4  2010/12/15 13:33:18  bourgesl
 * removed comment
 *
 * Revision 1.3  2010/10/21 16:49:12  bourgesl
 * better annotation position and font size
 * modified bar width for many targets
 *
 * Revision 1.2  2010/10/18 14:27:07  bourgesl
 * javadoc
 * disabled logs
 *
 * Revision 1.1  2010/10/15 17:03:20  bourgesl
 * major changes to add sliding behaviour (scrollbar) to view only a subset of targets if there are too many.
 * PDF options according to the number of targets
 *
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import fr.jmmc.aspro.AsproConstants;
import java.awt.Color;
import java.awt.Paint;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.annotations.XYAnnotation;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYBarRenderer;
import org.jfree.data.Range;
import org.jfree.data.gantt.TaskSeriesCollection;
import org.jfree.data.gantt.XYTaskDataset;
import org.jfree.ui.Layer;
import org.jfree.ui.TextAnchor;

/**
 * This class is a custom XYPlot adapter to provide both a sliding dataset and annotations in sync.
 * @author bourgesl
 */
public final class SlidingXYPlotAdapter {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.chart.SlidingXYDatasetAdapter";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
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

  /**
   * Constructor
   * @param chart chart instance
   * @param plot xy plot
   * @param maxElements max items in the chart view
   */
  public SlidingXYPlotAdapter(final JFreeChart chart, final XYPlot plot, final int maxElements) {
    this.chart = chart;
    this.xyPlot = plot;
    this.renderer = (XYBarRenderer) plot.getRenderer();
    this.maxViewItems = maxElements;
  }

  /**
   * Define the new data
   * @param collection data collection
   * @param symbols data symbols
   * @param colors data colors
   * @param annotations map of annotations keyed by position
   */
  public void setData(final TaskSeriesCollection collection, final List<String> symbols, final List<Paint> colors,
                      final Map<Integer, List<XYAnnotation>> annotations) {
    this.size = symbols.size();
    this.collection = collection;
    this.symbols = symbols;
    this.colors = colors;
    this.annotations = annotations;
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
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("useSubset = " + useSubset);
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
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("same positions : ignoring (" + start + " to " + end + ")");
      }
      return;
    }

    this.lastStart = start;
    this.lastEnd = end;

    final int newSize = end - start;

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("updatePlotDataset : pos = " + this.position + " :: (" + start + " to " + end + ")");
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
    if (newSize > 6) {
      barWidth = 0.75d;
    } else if (newSize > 1) {
      barWidth = 0.5d;
    } else {
      barWidth = 0.25d;
    }

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
      renderContext.setMaxTipHeight(1d - barWidth);


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

    // annotation JMMC (fixed position) :
    if (this.aJMMC == null) {
      this.aJMMC = ChartUtils.createXYTextAnnotation(AsproConstants.JMMC_ANNOTATION,
              symbolAxis.getRange().getUpperBound(),
              this.xyPlot.getRangeAxis().getRange().getUpperBound());
      this.aJMMC.setTextAnchor(TextAnchor.BOTTOM_RIGHT);
      this.aJMMC.setPaint(Color.DARK_GRAY);
    } else {
      this.aJMMC.setX(symbolAxis.getRange().getUpperBound());
      this.aJMMC.setY(this.xyPlot.getRangeAxis().getRange().getUpperBound());
    }
    this.renderer.addAnnotation(aJMMC, Layer.BACKGROUND);

    // tick color :
    this.xyPlot.getRangeAxis().setTickMarkPaint(Color.BLACK);
    this.xyPlot.getDomainAxis().setTickMarkPaint(Color.BLACK);

    // update theme at end :
    ChartUtilities.applyCurrentTheme(this.chart);
  }
}
