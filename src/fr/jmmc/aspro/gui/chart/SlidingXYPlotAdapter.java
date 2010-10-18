/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SlidingXYPlotAdapter.java,v 1.2 2010-10-18 14:27:07 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/10/15 17:03:20  bourgesl
 * major changes to add sliding behaviour (scrollbar) to view only a subset of targets if there are too many.
 * PDF options according to the number of targets
 *
 */
package fr.jmmc.aspro.gui.chart;

import fr.jmmc.aspro.AsproConstants;
import java.awt.Color;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.annotations.XYAnnotation;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.axis.SymbolAxis;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYBarRenderer;
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
  private final JFreeChart localJFreeChart;
  /** xy plot instance */
  private XYPlot localXYPlot;
  /** plot renderer to define annotations */
  private final XYBarRenderer renderer;
  /** max items in the chart view if the useSubset mode is enabled */
  private final int maxViewItems;
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
  private List<Color> colors = null;
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
    this.localJFreeChart = chart;
    this.localXYPlot = plot;
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
  public void setData(final TaskSeriesCollection collection, final List<String> symbols, final List<Color> colors,
                      final Map<Integer, List<XYAnnotation>> annotations) {
    this.size = symbols.size();
    this.collection = collection;
    this.symbols = symbols;
    this.colors = colors;
    this.annotations = annotations;
  }

  /**
   * Return the max items in the chart view if the useSubset mode is enabled
   * @return max items in the chart view
   */
  public int getMaxViewItems() {
    return maxViewItems;
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
   * Define the flag to enable/disable the subset mode and (force) refresh the plot
   * @param useSubset new value
   */
  public void setUseSubset(final boolean useSubset) {
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

    // Fix the plot size and bar width :

    // TODO : adjust range to have correct bar size if there is a lot of targets :
    // Add a scrollbar arround the plot only ?

    final double barWidth;
    final double rangeMin, rangeMax;

    if (newSize > 0) {
      rangeMin = -0.5d;
      rangeMax = newSize - 0.5d;
    } else {
      rangeMin = 0d;
      rangeMax = 1d;
    }

    // adjust bar width :
    if (newSize > 1) {
      barWidth = 0.5d;
    } else {
      barWidth = 0.25d;
    }

    // reset colors :
    this.renderer.clearSeriesPaints(false);
    // side effect with chart theme :
    this.renderer.setAutoPopulateSeriesPaint(false);

    final TaskSeriesCollection localTaskSeriesCollection = new TaskSeriesCollection();

    final String[] localSymbols = new String[newSize];

    for (int i = start, n = 0; i < end; i++, n++) {
      localSymbols[n] = this.symbols.get(i);

      localTaskSeriesCollection.add(this.collection.getSeries(i));

      // color :
      this.renderer.setSeriesPaint(n, this.colors.get(i), false);
    }

    final XYTaskDataset dataset = new XYTaskDataset(localTaskSeriesCollection);

    dataset.setSeriesWidth(barWidth);

    // update dataset :
    this.localXYPlot.setDataset(dataset);

    // change the Domain axis (vertical) :
    final SymbolAxis localSymbolAxis = new SymbolAxis(null, localSymbols);
    localSymbolAxis.setInverted(true);
    localSymbolAxis.setGridBandsVisible(false);
    localSymbolAxis.setAutoRange(false);
    localSymbolAxis.setRange(rangeMin, rangeMax);
    this.localXYPlot.setDomainAxis(localSymbolAxis);

    // remove Annotations :
    this.renderer.removeAnnotations();

    // annotations :
    if (this.annotations != null) {

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
    final XYTextAnnotation aJMMC = new XYTextAnnotation(AsproConstants.JMMC_ANNOTATION,
            localSymbolAxis.getRange().getUpperBound(),
            this.localXYPlot.getRangeAxis().getRange().getUpperBound());

    aJMMC.setTextAnchor(TextAnchor.BOTTOM_RIGHT);
    aJMMC.setPaint(Color.BLACK);
    this.renderer.addAnnotation(aJMMC, Layer.BACKGROUND);

    // tick color :
    this.localXYPlot.getRangeAxis().setTickMarkPaint(Color.BLACK);
    this.localXYPlot.getDomainAxis().setTickMarkPaint(Color.BLACK);

    // update theme at end :
    ChartUtilities.applyCurrentTheme(this.localJFreeChart);
  }
}
