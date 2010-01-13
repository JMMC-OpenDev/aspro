/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SquareXYPlot.java,v 1.2 2010-01-13 16:12:08 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/01/12 16:53:21  bourgesl
 * customized JFreeChart classes to get a square XY Plot supporting zooming in/out with mouse and mouse wheel
 *
 */
package fr.jmmc.aspro.gui.chart;

import java.awt.Graphics2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import org.jfree.chart.axis.AxisSpace;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.event.ChartChangeEventType;
import org.jfree.chart.event.PlotChangeEvent;
import org.jfree.chart.plot.PlotRenderingInfo;
import org.jfree.chart.plot.PlotState;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.Range;
import org.jfree.data.general.DatasetChangeEvent;
import org.jfree.data.xy.XYDataset;
import org.jfree.ui.RectangleInsets;

/**
 * This custom xy plot ensures the drawing area to have a square shape and finds the axes bounds.
 *
 * Note : this class must support the inherited cloneable interface.
 *
 * @author bourgesl
 */
public class SquareXYPlot extends XYPlot {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;

  /**
   * Creates a new plot with the specified dataset, axes and renderer.  Any
   * of the arguments can be <code>null</code>, but in that case you should
   * take care to specify the value before using the plot (otherwise a
   * <code>NullPointerException</code> may be thrown).
   *
   * @param dataset  the dataset (<code>null</code> permitted).
   * @param domainAxis  the domain axis (<code>null</code> permitted).
   * @param rangeAxis  the range axis (<code>null</code> permitted).
   * @param renderer  the renderer (<code>null</code> permitted).
   */
  public SquareXYPlot(XYDataset dataset,
          ValueAxis domainAxis,
          ValueAxis rangeAxis,
          XYItemRenderer renderer) {
    super(dataset, domainAxis, rangeAxis, renderer);
  }

  /**
   * Draws the plot within the specified area on a graphics device.
   *
   * @param g2  the graphics device.
   * @param area  the plot area (in Java2D space).
   * @param anchor  an anchor point in Java2D space (<code>null</code>
   *                permitted).
   * @param parentState  the state from the parent plot, if there is one
   *                     (<code>null</code> permitted).
   * @param info  collects chart drawing information (<code>null</code>
   *              permitted).
   */
  @Override
  public void draw(Graphics2D g2, Rectangle2D area, Point2D anchor,
          PlotState parentState, PlotRenderingInfo info) {

    double hSpace = 0d;
    double vSpace = 0d;

    // get plot insets :
    final RectangleInsets insets = getInsets();

    hSpace += insets.getLeft() + insets.getRight();
    vSpace += insets.getTop() + insets.getBottom();

    // compute Axis Space :
    final AxisSpace space = calculateAxisSpace(g2, area);

    hSpace += space.getLeft() + space.getRight();
    vSpace += space.getTop() + space.getBottom();

    // compute the square data area size :
    final double size = Math.min(area.getWidth() - hSpace, area.getHeight() - vSpace);

    // adjusted dimensions to get a square data area :
    final double adjustedWidth = size + hSpace;
    final double adjustedHeight = size + vSpace;

    // margins to center the plot into the rectangle area :
    final double marginWidth = (area.getWidth() - adjustedWidth) / 2d;
    final double marginHeight = (area.getHeight() - adjustedHeight) / 2d;

    final Rectangle2D adjustedArea = new Rectangle2D.Double();
    adjustedArea.setRect(area.getX() + marginWidth, area.getY() + marginHeight, adjustedWidth, adjustedHeight);

    super.draw(g2, adjustedArea, anchor, parentState, info);
  }

  /**
   * Receives notification of a change to the plot's dataset.
   * The axis ranges are updated if necessary.
   *
   * HACK : find the bounds for both range and domain axes and adjust the axes bounds (reset zoom)
   *
   * @param event  information about the event (not used here).
   */
  @Override
  public void datasetChanged(final DatasetChangeEvent event) {
    findAxesBounds();
    restoreAxesBounds();
  }

  /**
   * Use the axis bounds to redefine the ranges for both axes (reset zoom)
   */
  public void restoreAxesBounds() {
    final BoundedNumberAxis domainAxis = (BoundedNumberAxis) getDomainAxis(0);
    final BoundedNumberAxis rangeAxis = (BoundedNumberAxis) getRangeAxis(0);

    if (rangeAxis != null) {
      // same bounds for both axes :
      final Range bounds = rangeAxis.getBounds();
      // do not disable auto range :
      domainAxis.setRange(bounds, false, false);

      rangeAxis.setRange(bounds, false, false);
    }

    final PlotChangeEvent e = new PlotChangeEvent(this);
    e.setType(ChartChangeEventType.DATASET_UPDATED);
    notifyListeners(e);
  }

  /**
   * Find the bounds for both range and domain axes and defines the largest bounds to both axes to get a square area
   */
  private void findAxesBounds() {
    final BoundedNumberAxis domainAxis = (BoundedNumberAxis) getDomainAxis(0);
    final BoundedNumberAxis rangeAxis = (BoundedNumberAxis) getRangeAxis(0);

    if (domainAxis != null) {
      // do not notify :
      domainAxis.setAutoRange(true, false);
      domainAxis.setBounds(null);
    }
    if (rangeAxis != null) {
      // do not notify :
      rangeAxis.setAutoRange(true, false);
      rangeAxis.setBounds(null);
    }

    configureDomainAxes();
    configureRangeAxes();

    double xMin = 0d;
    double xMax = 0d;
    double yMin = 0d;
    double yMax = 0d;

    if (domainAxis != null) {
      Range xRange = domainAxis.getRange();

      xMin = xRange.getLowerBound();
      xMax = xRange.getUpperBound();
    }

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
      final Range bounds = new Range(-max, max);

      domainAxis.setBounds(bounds);
      rangeAxis.setBounds(bounds);
    }
  }
}
