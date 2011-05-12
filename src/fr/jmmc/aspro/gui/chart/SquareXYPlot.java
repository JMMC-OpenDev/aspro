/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SquareXYPlot.java,v 1.8 2011-04-14 14:36:46 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.7  2010/06/17 10:02:50  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.6  2010/02/18 09:52:37  bourgesl
 * added rendering hints (anti-aliasing)
 *
 * Revision 1.5  2010/02/04 17:05:06  bourgesl
 * UV bounds are coming from UVCoverageService
 *
 * Revision 1.4  2010/02/03 16:07:49  bourgesl
 * refactoring to use the custom swing worker executor
 * when zomming uv map is computed asynchronously
 *
 * Revision 1.3  2010/02/03 09:48:53  bourgesl
 * target model uvmap added on the uv coverage with zooming supported
 *
 * Revision 1.2  2010/01/13 16:12:08  bourgesl
 * comments
 *
 * Revision 1.1  2010/01/12 16:53:21  bourgesl
 * customized JFreeChart classes to get a square XY Plot supporting zooming in/out with mouse and mouse wheel
 *
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import org.jfree.chart.axis.AxisSpace;
import org.jfree.chart.axis.ValueAxis;
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
public final class SquareXYPlot extends XYPlot {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.chart.SquareXYPlot";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

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
  public SquareXYPlot(final XYDataset dataset,
                      final ValueAxis domainAxis,
                      final ValueAxis rangeAxis,
                      final XYItemRenderer renderer) {
    super(dataset, domainAxis, rangeAxis, renderer);
  }

  /**
   * Draws the plot within the specified area on a graphics device.
   *
   * @param g2d  the graphics device.
   * @param area  the plot area (in Java2D space).
   * @param anchor  an anchor point in Java2D space (<code>null</code>
   *                permitted).
   * @param parentState  the state from the parent plot, if there is one
   *                     (<code>null</code> permitted).
   * @param info  collects chart drawing information (<code>null</code>
   *              permitted).
   */
  @Override
  public void draw(final Graphics2D g2d, final Rectangle2D area, final Point2D anchor,
                   final PlotState parentState, final PlotRenderingInfo info) {

    double hSpace = 0d;
    double vSpace = 0d;

    // get plot insets :
    final RectangleInsets insets = getInsets();

    hSpace += insets.getLeft() + insets.getRight();
    vSpace += insets.getTop() + insets.getBottom();

    // compute Axis Space :
    final AxisSpace space = calculateAxisSpace(g2d, area);

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

    // note : 
    // - rounding is required to have the background image fitted (int coordinates) in the plot area (double rectangle) :
    // - there can be some rounding issue that adjust lightly the square shape :
    adjustedArea.setRect(Math.round(area.getX() + marginWidth), Math.round(area.getY() + marginHeight), Math.round(adjustedWidth), Math.round(adjustedHeight));

    // Force rendering hints :
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    // Use bilinear for performance instead of bicubic (available but slower):
    g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
    g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);

    super.draw(g2d, adjustedArea, anchor, parentState, info);
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
    restoreAxesBounds();
  }

  /**
   * Define the bounds for both range and domain axes using the given maximum value.
   *
   * Note : this method must be called before calling plot.setDataset(...)
   *
   * @param max maximum value
   */
  public void defineBounds(final double max) {
    final BoundedNumberAxis domainAxis = (BoundedNumberAxis) getDomainAxis(0);
    final BoundedNumberAxis rangeAxis = (BoundedNumberAxis) getRangeAxis(0);

    if (rangeAxis != null) {
      // same range on both axes :
      final Range bounds = new Range(-max, max);

      domainAxis.setBounds(bounds);
      rangeAxis.setBounds(bounds);
    }
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

      if (bounds != null) {
        // do not disable auto range :
        domainAxis.setRange(bounds, false, false);
        rangeAxis.setRange(bounds, false, false);
      }
    }
  }
}
