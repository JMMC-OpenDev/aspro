/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SquareChartPanel.java,v 1.1 2010-01-12 16:53:20 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.chart;

import java.awt.Point;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.Plot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.PlotRenderingInfo;
import org.jfree.chart.plot.Zoomable;

/**
 *
 * @author bourgesl
 */
public class SquareChartPanel extends ChartPanel {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;

  public SquareChartPanel(JFreeChart chart, int width, int height,
          int minimumDrawWidth, int minimumDrawHeight, int maximumDrawWidth,
          int maximumDrawHeight, boolean useBuffer, boolean properties,
          boolean copy, boolean save, boolean print, boolean zoom,
          boolean tooltips) {
    super(chart, width, height, minimumDrawWidth, minimumDrawHeight, maximumDrawWidth, maximumDrawHeight,
            useBuffer, properties, copy, save, print, zoom, tooltips);
  }

  /**
   * Zooms in on a selected region.
   *
   * HACK : keep a constant aspect ratio 1:1
   *
   * @param selection  the selected region.
   */
  @Override
  public void zoom(final Rectangle2D selection) {

    // get the origin of the zoom selection in the Java2D space used for
    // drawing the chart (that is, before any scaling to fit the panel)
    final Point2D selectOrigin = translateScreenToJava2D(new Point(
            (int) Math.ceil(selection.getX()),
            (int) Math.ceil(selection.getY())));
    final PlotRenderingInfo plotInfo = getChartRenderingInfo().getPlotInfo();
    final Rectangle2D scaledDataArea = getScreenDataArea(
            (int) selection.getCenterX(), (int) selection.getCenterY());
    if ((selection.getHeight() > 0) && (selection.getWidth() > 0)) {

      // percents :
      double hLower = (selection.getMinX() - scaledDataArea.getMinX()) / scaledDataArea.getWidth();
      double hUpper = (selection.getMaxX() - scaledDataArea.getMinX()) / scaledDataArea.getWidth();
      double vLower = (scaledDataArea.getMaxY() - selection.getMaxY()) / scaledDataArea.getHeight();
      double vUpper = (scaledDataArea.getMaxY() - selection.getMinY()) / scaledDataArea.getHeight();

      // HACK to have a square zoom area :
      // average the horizontal and vertical lengths :
      final double len = ((hUpper - hLower) + (vUpper - vLower)) / 2d;

      // center of the square :
      final double cx = (hLower + hUpper) / 2d;
      final double cy = (vLower + vUpper) / 2d;

      hLower = cx - len;
      hUpper = cx + len;
      vLower = cy - len;
      vUpper = cy + len;

      final Plot p = getChart().getPlot();
      if (p instanceof Zoomable) {
        // here we tweak the notify flag on the plot so that only
        // one notification happens even though we update multiple
        // axes...
        final boolean savedNotify = p.isNotify();
        p.setNotify(false);
        final Zoomable z = (Zoomable) p;
        if (z.getOrientation() == PlotOrientation.HORIZONTAL) {
          z.zoomDomainAxes(vLower, vUpper, plotInfo, selectOrigin);
          z.zoomRangeAxes(hLower, hUpper, plotInfo, selectOrigin);
        } else {
          z.zoomDomainAxes(hLower, hUpper, plotInfo, selectOrigin);
          z.zoomRangeAxes(vLower, vUpper, plotInfo, selectOrigin);
        }
        p.setNotify(savedNotify);
      }
    }
  }

  /**
   * Restores the auto-range calculation on both axes.
   *
   * Used by the mouse listener to reset the zoom.
   *
   * HACK : call the SquareXYPlot.restoreAxesBounds() method to restore both axes bounds.
   */
  @Override
  public void restoreAutoBounds() {
    final Plot plot = getChart().getPlot();
    if (plot == null || !(plot instanceof SquareXYPlot)) {
      return;
    }

    ((SquareXYPlot) plot).restoreAxesBounds();
  }
}
