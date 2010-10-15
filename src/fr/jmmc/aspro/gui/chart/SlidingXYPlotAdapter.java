/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SlidingXYPlotAdapter.java,v 1.1 2010-10-15 17:03:20 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.chart;

import fr.jmmc.aspro.AsproConstants;
import java.awt.Color;
import java.util.List;
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
 *
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
  private final XYBarRenderer renderer;

  private final int maxViewItems;

  private boolean useSubset = false;
  private int position = 0;

  private int size;
  private TaskSeriesCollection collection = null;
  private List<String> symbols = null;
  private List<Color> colors = null;
  private List<XYAnnotation> annotations = null;

  private int lastStart = -1;
  private int lastEnd = -1;

  public SlidingXYPlotAdapter(final JFreeChart chart, final XYPlot plot, final int maxElements) {
    this.localJFreeChart = chart;
    this.localXYPlot = plot;
    this.renderer = (XYBarRenderer) plot.getRenderer();
    this.maxViewItems = maxElements;
  }
  
  public void setData(final TaskSeriesCollection collection, final List<String> symbols, final List<Color> colors,
          final List<XYAnnotation> annotations) {
    this.size = symbols.size();
    this.collection = collection;
    this.symbols = symbols;
    this.colors = colors;
    this.annotations = annotations;
  }

  public int getMaxViewItems() {
    return maxViewItems;
  }

  public int getSize() {
    return size;
  }

  public int getPosition() {
    return position;
  }

  public void setPosition(final int pos) {
    this.position = pos;
    if (useSubset) {
      updatePlot(maxViewItems, false);
    }
  }

  public boolean isUseSubset() {
    return this.useSubset;
  }

  public void setUseSubset(final boolean useSubset) {
    this.useSubset = useSubset;
    if (useSubset) {
      updatePlot(maxViewItems, true);
    } else {
      updatePlot(size, true);
    }
  }

  private void updatePlot(final int max, final boolean forceUpdate) {

    int start = position;
    if (start < 0) {
      start = 0;
    }
    
    int end = start + max;
    if (end > size) {
      end = size;
      start = end - max;
      if (start < 0) {
        start = 0;
      }
    }

    if (!forceUpdate && start == lastStart && end == lastEnd) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("same positions : ignoring ("+ start + " to " + end + ")");
      }
      return;
    }

    this.lastStart = start;
    this.lastEnd = end;

    final int newSize = end - start;

    logger.severe("updatePlotDataset : pos = " + position + " :: ("+ start + " to " + end + ")");

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
    renderer.clearSeriesPaints(false);
    // side effect with chart theme :
    renderer.setAutoPopulateSeriesPaint(false);
    // remove Annotations :
    renderer.removeAnnotations();

    final TaskSeriesCollection localTaskSeriesCollection = new TaskSeriesCollection();

    final String[] localSymbols = new String[newSize];

    for (int i = start, n = 0; i < end; i++, n++) {
      localSymbols[n] = symbols.get(i);

      localTaskSeriesCollection.add(collection.getSeries(i));

      // color :
      renderer.setSeriesPaint(n, colors.get(i), false);
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

    // annotations :
    if (this.annotations != null) {

      try {
        double n;
        for (XYAnnotation annotation : this.annotations) {
          if (annotation instanceof XYTickAnnotation) {
            XYTickAnnotation a = (XYTickAnnotation)annotation;

            if (a.getX() >= start && a.getX() < end) {
              // valid, relocate :
              n = a.getX() - start;

              // clone annotation to change x position :
              a = (XYTickAnnotation)a.clone();
              a.setX(n);
              this.renderer.addAnnotation(a);
            }
           }
          else
          if (annotation instanceof XYTextAnnotation) {
            XYTextAnnotation a = (XYTextAnnotation)annotation;

            if (a.getX() >= start && a.getX() < end) {
              // valid, relocate :
              n = a.getX() - start;

              // clone annotation to change x position :
              a = (XYTextAnnotation)a.clone();
              a.setX(n);
              this.renderer.addAnnotation(a);
            }
          }
          else
          if (annotation instanceof XYDiamondAnnotation) {
            XYDiamondAnnotation a = (XYDiamondAnnotation)annotation;

            if (a.getX() >= start && a.getX() < end) {
              // valid, relocate :
              n = a.getX() - start;

              // clone annotation to change x position :
              a = (XYDiamondAnnotation)a.clone();
              a.setX(n);
              this.renderer.addAnnotation(a);
            }
          }
        }
      } catch (CloneNotSupportedException cnse) {
        logger.log(Level.SEVERE, "clone failure", cnse);
      }
//              XYTextAnnotation
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
    ChartUtilities.applyCurrentTheme(localJFreeChart);
  }


}
