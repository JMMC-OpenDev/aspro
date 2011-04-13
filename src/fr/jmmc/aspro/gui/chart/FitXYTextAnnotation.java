/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: FitXYTextAnnotation.java,v 1.3 2011-04-13 14:38:50 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2011/04/08 15:11:41  bourgesl
 * margin set to 1px
 *
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
  /** margin in pixels */
  private final static int MARGIN = 1;

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
   * Draws the annotation.
   *
   * @param g2d  the graphics device.
   * @param plot  the plot.
   * @param dataArea  the data area.
   * @param domainAxis  the domain axis.
   * @param rangeAxis  the range axis.
   * @param rendererIndex  the renderer index.
   * @param info  an optional info object that will be populated with
   *              entity information.
   */
  @Override
  public void draw(final Graphics2D g2d, final XYPlot plot, final Rectangle2D dataArea,
                   final ValueAxis domainAxis, final ValueAxis rangeAxis,
                   final int rendererIndex, final PlotRenderingInfo info) {

    // Use Observability Plot Context to determine once for all the appropriate font size
    // that best fits the bar width; if too large, do not render the annotation

    final ObservabilityPlotContext renderContext = ObservabilityPlotContext.getInstance();

    final Font bestFont;

    if (renderContext.autoFitTimeWidthDone()) {
      bestFont = renderContext.autoFitTimeWidthFont();
    } else {
      // first time, perform fit:
      final RectangleEdge domainEdge = Plot.resolveDomainAxisLocation(plot.getDomainAxisLocation(), plot.getOrientation());

      // convert max text width in data units (equals to bar width) i.e. domain axis :
      final double j2Max = domainAxis.lengthToJava2D(renderContext.getMaxTextWidth(), dataArea, domainEdge) - 2d * MARGIN;

      bestFont = renderContext.autoFitTimeWidthFont(g2d, j2Max);
    }

    // Dont render if the text do not fit in block size:
    if (bestFont == null) {
      return;
    }
    setFont(bestFont);

    super.draw(g2d, plot, dataArea, domainAxis, rangeAxis, rendererIndex, info);
  }
}
