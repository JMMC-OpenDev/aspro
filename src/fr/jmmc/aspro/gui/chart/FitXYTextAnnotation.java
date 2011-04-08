/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: FitXYTextAnnotation.java,v 1.2 2011-04-08 15:11:41 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/10/21 16:47:43  bourgesl
 * Custom XYTextAnnotation that adjust its font size to respect sizing constraints i.e. text width is fit to maxRadius
 *
 */
package fr.jmmc.aspro.gui.chart;

import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;
import java.io.Serializable;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.Plot;
import org.jfree.chart.plot.PlotRenderingInfo;
import org.jfree.chart.plot.XYPlot;
import org.jfree.ui.RectangleEdge;
import org.jfree.util.PublicCloneable;

/**
 * Custom XYTextAnnotation that adjust font size to respect sizing constraints
 * @author bourgesl
 */
public final class FitXYTextAnnotation extends XYTextAnnotation
        implements Cloneable, PublicCloneable, Serializable {

  /** For serialization. */
  private static final long serialVersionUID = 1L;
  /** maximum font size */
  private final static int MAX_SIZE_FONT = 11;
  /** minimum font size */
  private final static int MIN_SIZE_FONT = 5;
  /** margin in pixels */
  private final static int MARGIN = 1;

  /* members */
  /** HACK : maximum width defined in data units to scale the text */
  private double maxRadius;

  /**
   * Creates a new annotation to be displayed at the given coordinates.  The
   * coordinates are specified in data space (they will be converted to
   * Java2D space for display).
   *
   * @param text  the text (<code>null</code> not permitted).
   * @param x  the x-coordinate (in data space).
   * @param y  the y-coordinate (in data space).
   */
  public FitXYTextAnnotation(final String text, final double x, final double y) {
    super(text, x, y);
  }

  /**
   * Return the maximum radius defined in data units to scale the text
   * @return maximum radius defined in data units
   */
  public double getMaxRadius() {
    return maxRadius;
  }

  /**
   * Set the maximum radius defined in data units to scale the text
   * @param maxRadius maximum radius defined in data units
   */
  public void setMaxRadius(final double maxRadius) {
    this.maxRadius = maxRadius;
  }

  /**
   * Draws the annotation.
   *
   * @param g2  the graphics device.
   * @param plot  the plot.
   * @param dataArea  the data area.
   * @param domainAxis  the domain axis.
   * @param rangeAxis  the range axis.
   * @param rendererIndex  the renderer index.
   * @param info  an optional info object that will be populated with
   *              entity information.
   */
  @Override
  public void draw(final Graphics2D g2, final XYPlot plot, final Rectangle2D dataArea,
                   final ValueAxis domainAxis, final ValueAxis rangeAxis,
                   final int rendererIndex, final PlotRenderingInfo info) {

    final RectangleEdge domainEdge = Plot.resolveDomainAxisLocation(
            plot.getDomainAxisLocation(), plot.getOrientation());

    // HACK (max radius) is related to bar width i.e. domain axis :
    final double j2MAX = domainAxis.lengthToJava2D(getMaxRadius(), dataArea, domainEdge) - 2d * MARGIN;

    final Font bestFont = ChartUtils.autoFitText(getText(), j2MAX, g2, MIN_SIZE_FONT, MAX_SIZE_FONT);

    // change font :
    setFont(bestFont);

    super.draw(g2, plot, dataArea, domainAxis, rangeAxis, rendererIndex, info);
  }
}
