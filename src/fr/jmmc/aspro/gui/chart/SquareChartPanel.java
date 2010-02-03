/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SquareChartPanel.java,v 1.3 2010-02-03 09:48:53 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2010/01/13 16:12:08  bourgesl
 * comments
 *
 * Revision 1.1  2010/01/12 16:53:20  bourgesl
 * customized JFreeChart classes to get a square XY Plot supporting zooming in/out with mouse and mouse wheel
 *
 */
package fr.jmmc.aspro.gui.chart;

import java.awt.Point;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.io.Serializable;
import java.util.logging.Level;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartRenderingInfo;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.Plot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.PlotRenderingInfo;
import org.jfree.chart.plot.Zoomable;
import org.jfree.data.Range;

/**
 * This class extends the ChartPanel to customize the zoom and restoreAutoBounds methods 
 * in order to maintain a square data area and fire a zoom event
 *
 * Note : this class must support the inherited cloneable interface.
 *
 * @author bourgesl
 */
public class SquareChartPanel extends ChartPanel {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.chart.SquareChartPanel";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /* members */
  /** single zoom event listener */
  private ZoomEventListener zoomListener = null;

  /**
   * Constructs a SQUARE JFreeChart panel.
   *
   * @param chart  the chart.
   * @param width  the preferred width of the panel.
   * @param height  the preferred height of the panel.
   * @param minimumDrawWidth  the minimum drawing width.
   * @param minimumDrawHeight  the minimum drawing height.
   * @param maximumDrawWidth  the maximum drawing width.
   * @param maximumDrawHeight  the maximum drawing height.
   * @param useBuffer  a flag that indicates whether to use the off-screen
   *                   buffer to improve performance (at the expense of
   *                   memory).
   * @param properties  a flag indicating whether or not the chart property
   *                    editor should be available via the popup menu.
   * @param copy  a flag indicating whether or not a copy option should be
   *              available via the popup menu.
   * @param save  a flag indicating whether or not save options should be
   *              available via the popup menu.
   * @param print  a flag indicating whether or not the print option
   *               should be available via the popup menu.
   * @param zoom  a flag indicating whether or not zoom options should be
   *              added to the popup menu.
   * @param tooltips  a flag indicating whether or not tooltips should be
   *                  enabled for the chart.
   *
   * @since 1.0.13
   */
  public SquareChartPanel(JFreeChart chart, int width, int height,
          int minimumDrawWidth, int minimumDrawHeight, int maximumDrawWidth,
          int maximumDrawHeight, boolean useBuffer, boolean properties,
          boolean copy, boolean save, boolean print, boolean zoom,
          boolean tooltips) {
    super(chart, width, height, minimumDrawWidth, minimumDrawHeight, maximumDrawWidth, maximumDrawHeight,
            useBuffer, properties, copy, save, print, zoom, tooltips);

    // add our custom mouse wheel listener :
    this.addMouseWheelListener(new MouseWheelHandler(this));
  }

  public ZoomEventListener getZoomEventListener() {
    return zoomListener;
  }

  public void setZoomEventListener(ZoomEventListener listener) {
    this.zoomListener = listener;
  }

  /**
   * Zooms in on an anchor point (specified in screen coordinate space).
   *
   * @param x  the x value (in screen coordinates).
   * @param y  the y value (in screen coordinates).
   */
  @Override
  public void zoomInBoth(double x, double y) {
    Plot plot = getChart().getPlot();
    if (plot == null) {
      return;
    }
    // here we tweak the notify flag on the plot so that only
    // one notification happens even though we update multiple
    // axes...
    boolean savedNotify = plot.isNotify();
    plot.setNotify(false);
    zoomInDomain(x, y);
    zoomInRange(x, y);

    // HACK to get new axis ranges after zoom :
    fireZoomEvent((SquareXYPlot) plot);

    plot.setNotify(savedNotify);
  }

  /**
   * Zooms out on an anchor point (specified in screen coordinate space).
   *
   * @param x  the x value (in screen coordinates).
   * @param y  the y value (in screen coordinates).
   */
  @Override
  public void zoomOutBoth(double x, double y) {
    Plot plot = getChart().getPlot();
    if (plot == null) {
      return;
    }
    // here we tweak the notify flag on the plot so that only
    // one notification happens even though we update multiple
    // axes...
    boolean savedNotify = plot.isNotify();
    plot.setNotify(false);
    zoomOutDomain(x, y);
    zoomOutRange(x, y);

    // HACK to get new axis ranges after zoom :
    fireZoomEvent((SquareXYPlot) plot);

    plot.setNotify(savedNotify);
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

    if ((selection.getHeight() > 0) && (selection.getWidth() > 0)) {
      // get the origin of the zoom selection in the Java2D space used for
      // drawing the chart (that is, before any scaling to fit the panel)
      final Point2D selectOrigin = translateScreenToJava2D(new Point(
              (int) Math.ceil(selection.getX()),
              (int) Math.ceil(selection.getY())));
      final PlotRenderingInfo plotInfo = getChartRenderingInfo().getPlotInfo();
      final Rectangle2D scaledDataArea = getScreenDataArea(
              (int) selection.getCenterX(), (int) selection.getCenterY());

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

      final Plot plot = getChart().getPlot();
      if (plot instanceof Zoomable) {
        // here we tweak the notify flag on the plot so that only
        // one notification happens even though we update multiple
        // axes...
        final boolean savedNotify = plot.isNotify();
        plot.setNotify(false);
        final Zoomable z = (Zoomable) plot;
        if (z.getOrientation() == PlotOrientation.HORIZONTAL) {
          z.zoomDomainAxes(vLower, vUpper, plotInfo, selectOrigin);
          z.zoomRangeAxes(hLower, hUpper, plotInfo, selectOrigin);
        } else {
          z.zoomDomainAxes(hLower, hUpper, plotInfo, selectOrigin);
          z.zoomRangeAxes(vLower, vUpper, plotInfo, selectOrigin);
        }

        // HACK to get new axis ranges after zoom :
        fireZoomEvent((SquareXYPlot) plot);

        plot.setNotify(savedNotify);
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

    // here we tweak the notify flag on the plot so that only
    // one notification happens even though we update multiple
    // axes...
    boolean savedNotify = plot.isNotify();
    plot.setNotify(false);

    ((SquareXYPlot) plot).restoreAxesBounds();

    // HACK to get new axis ranges after zoom :
    fireZoomEvent((SquareXYPlot) plot);

    plot.setNotify(savedNotify);
  }

  protected void fireZoomEvent(final SquareXYPlot plot) {
    final ValueAxis domainAxis = plot.getDomainAxis(0);
    final ValueAxis rangeAxis = plot.getRangeAxis(0);

    double domainLowerBound = 0d;
    double domainUpperBound = 0d;
    double rangeLowerBound = 0d;
    double rangeUpperBound = 0d;

    if (domainAxis != null) {
      final Range xRange = domainAxis.getRange();

      domainLowerBound = xRange.getLowerBound();
      domainUpperBound = xRange.getUpperBound();
    }

    if (rangeAxis != null) {
      final Range yRange = rangeAxis.getRange();

      rangeLowerBound = yRange.getLowerBound();
      rangeUpperBound = yRange.getUpperBound();
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Axis ranges :\nX [" + domainLowerBound + ", " + domainUpperBound + "]" + "\nY [" + rangeLowerBound + ", " + rangeUpperBound + "]");
    }

    if (this.zoomListener != null) {
      this.zoomListener.chartChanged(new ZoomEvent(domainLowerBound, domainUpperBound, rangeLowerBound, rangeUpperBound));
    }
  }

  /**
   * HACK to intercept the mouse wheel events to fire the zoom event
   */
  private class MouseWheelHandler implements MouseWheelListener, Serializable {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** default zoom factor = 0.25 (twenty five percent) */
    private final static double DEFAULT_ZOOM_FACTOR = 0.25d;

    /* members */
    /** The chart panel. */
    private ChartPanel chartPanel;
    /** The zoom factor. */
    protected double zoomFactor;

    /**
     * Creates a new instance.
     *
     * @param chartPanel  the chart panel (<code>null</code> not permitted).
     */
    public MouseWheelHandler(ChartPanel chartPanel) {
      this.chartPanel = chartPanel;
      this.zoomFactor = DEFAULT_ZOOM_FACTOR;
    }

    /**
     * Returns the current zoom factor.  The default value is 0.25 (twenty five percent).
     *
     * @return The zoom factor.
     *
     * @see #setZoomFactor(double)
     */
    public double getZoomFactor() {
      return this.zoomFactor;
    }

    /**
     * Sets the zoom factor.
     *
     * @param zoomFactor  the zoom factor.
     *
     * @see #getZoomFactor()
     */
    public void setZoomFactor(double zoomFactor) {
      this.zoomFactor = zoomFactor;
    }

    /**
     * Handles a mouse wheel event from the underlying chart panel.
     *
     * @param e  the event.
     */
    public void mouseWheelMoved(MouseWheelEvent e) {
      JFreeChart chart = this.chartPanel.getChart();
      if (chart == null) {
        return;
      }
      Plot plot = chart.getPlot();
      if (plot instanceof Zoomable) {
        Zoomable zoomable = (Zoomable) plot;
        handleZoomable(zoomable, e);
      }
    }

    /**
     * Handle the case where a plot implements the {@link Zoomable} interface.
     *
     * @param zoomable  the zoomable plot.
     * @param e  the mouse wheel event.
     */
    private void handleZoomable(Zoomable zoomable, MouseWheelEvent e) {
      Plot plot = (Plot) zoomable;
      ChartRenderingInfo info = this.chartPanel.getChartRenderingInfo();
      PlotRenderingInfo pinfo = info.getPlotInfo();
      Point2D p = this.chartPanel.translateScreenToJava2D(e.getPoint());
      if (!pinfo.getDataArea().contains(p)) {
        return;
      }
      int clicks = e.getWheelRotation();
      int direction = 0;
      if (clicks < 0) {
        direction = -1;
      } else if (clicks > 0) {
        direction = 1;
      }

      boolean old = plot.isNotify();

      // do not notify while zooming each axis
      plot.setNotify(false);

      final double increment = 1.0 + this.zoomFactor;
      if (direction > 0) {
        zoomable.zoomDomainAxes(increment, pinfo, p, true);
        zoomable.zoomRangeAxes(increment, pinfo, p, true);
      } else if (direction < 0) {
        zoomable.zoomDomainAxes(1.0 / increment, pinfo, p, true);
        zoomable.zoomRangeAxes(1.0 / increment, pinfo, p, true);
      }

      // HACK to get new axis ranges after zoom :
      fireZoomEvent((SquareXYPlot) plot);

      // set the old notify status
      plot.setNotify(old);
    }
  }
}
