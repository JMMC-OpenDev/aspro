/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.geom.Rectangle2D;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.Plot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.renderer.PaintScale;
import org.jfree.chart.title.PaintScaleLegend;
import org.jfree.ui.RectangleEdge;

/**
 * This class hacks PaintScaleLegend to support correct log axis Paint
 * @author bourgesl
 */
public class PaintLogScaleLegend extends PaintScaleLegend {

  /** For serialization. */
  static final long serialVersionUID = -1L;

  /**
   * Creates a new instance.
   *
   * @param scale  the scale (<code>null</code> not permitted).
   * @param axis  the axis (<code>null</code> not permitted).
   */
  public PaintLogScaleLegend(final PaintScale scale, final ValueAxis axis) {
    super(scale, axis);
  }

  /**
   * Draws the legend within the specified area.
   *
   * @param g2  the graphics target (<code>null</code> not permitted).
   * @param area  the drawing area (<code>null</code> not permitted).
   * @param params  drawing parameters (ignored here).
   *
   * @return <code>null</code>.
   */
  @Override
  public Object draw(Graphics2D g2, Rectangle2D area, Object params) {

    Rectangle2D target = (Rectangle2D) area.clone();
    target = trimMargin(target);
    if (this.getBackgroundPaint() != null) {
      g2.setPaint(this.getBackgroundPaint());
      g2.fill(target);
    }
    getFrame().draw(g2, target);
    getFrame().getInsets().trim(target);
    target = trimPadding(target);

    final int subdivisions = getSubdivisionCount();
    final double stripWidth = getStripWidth();

    // LAURENT: convert to 10^increment instead of linear scale:
    final double base = Math.log10(getAxis().getLowerBound());
    final double increment = (Math.log10(getAxis().getUpperBound()) - base) / subdivisions;
    
    Rectangle2D r = new Rectangle2D.Double();

    if (RectangleEdge.isTopOrBottom(getPosition())) {
      RectangleEdge axisEdge = Plot.resolveRangeAxisLocation(
              this.getAxisLocation(), PlotOrientation.HORIZONTAL);
      if (axisEdge == RectangleEdge.TOP) {
        for (int i = 0; i < subdivisions; i++) {
          double v = Math.pow(10d, base + (i * increment));
          Paint p = this.getScale().getPaint(v);
          double vv0 = getAxis().valueToJava2D(v, target, RectangleEdge.TOP);
          double vv1 = getAxis().valueToJava2D(v + increment, target, RectangleEdge.TOP);
          double ww = Math.abs(vv1 - vv0) + 1.0;
          r.setRect(Math.min(vv0, vv1), target.getMaxY() - stripWidth, ww, stripWidth);
          g2.setPaint(p);
          g2.fill(r);
        }
        if (isStripOutlineVisible()) {
          g2.setPaint(this.getStripOutlinePaint());
          g2.setStroke(this.getStripOutlineStroke());
          g2.draw(new Rectangle2D.Double(target.getMinX(), target.getMaxY() - stripWidth, target.getWidth(), stripWidth));
        }
        getAxis().draw(g2, target.getMaxY() - stripWidth - this.getAxisOffset(), target, target, RectangleEdge.TOP, null);

      } else if (axisEdge == RectangleEdge.BOTTOM) {
        for (int i = 0; i < subdivisions; i++) {
          double v = Math.pow(10d, base + (i * increment));
          Paint p = this.getScale().getPaint(v);
          double vv0 = getAxis().valueToJava2D(v, target, RectangleEdge.BOTTOM);
          double vv1 = getAxis().valueToJava2D(v + increment, target, RectangleEdge.BOTTOM);
          double ww = Math.abs(vv1 - vv0) + 1.0;
          r.setRect(Math.min(vv0, vv1), target.getMinY(), ww, stripWidth);
          g2.setPaint(p);
          g2.fill(r);
        }
        if (isStripOutlineVisible()) {
          g2.setPaint(this.getStripOutlinePaint());
          g2.setStroke(this.getStripOutlineStroke());
          g2.draw(new Rectangle2D.Double(target.getMinX(), target.getMinY(), target.getWidth(), stripWidth));
        }
        getAxis().draw(g2, target.getMinY() + stripWidth + this.getAxisOffset(), target, target, RectangleEdge.BOTTOM, null);
      }
    } else {
      RectangleEdge axisEdge = Plot.resolveRangeAxisLocation(
              this.getAxisLocation(), PlotOrientation.VERTICAL);
      if (axisEdge == RectangleEdge.LEFT) {
        for (int i = 0; i < subdivisions; i++) {
          double v = Math.pow(10d, base + (i * increment));
          Paint p = this.getScale().getPaint(v);
          double vv0 = getAxis().valueToJava2D(v, target, RectangleEdge.LEFT);
          double vv1 = getAxis().valueToJava2D(v + increment, target, RectangleEdge.LEFT);
          double hh = Math.abs(vv1 - vv0) + 1.0;
          r.setRect(target.getMaxX() - stripWidth, Math.min(vv0, vv1), stripWidth, hh);
          g2.setPaint(p);
          g2.fill(r);
        }
        if (isStripOutlineVisible()) {
          g2.setPaint(this.getStripOutlinePaint());
          g2.setStroke(this.getStripOutlineStroke());
          g2.draw(new Rectangle2D.Double(target.getMaxX() - stripWidth, target.getMinY(), stripWidth, target.getHeight()));
        }
        getAxis().draw(g2, target.getMaxX() - stripWidth - this.getAxisOffset(), target, target, RectangleEdge.LEFT, null);
      } else if (axisEdge == RectangleEdge.RIGHT) {
        for (int i = 0; i < subdivisions; i++) {
          double v = Math.pow(10d, base + (i * increment));
          Paint p = this.getScale().getPaint(v);
          double vv0 = getAxis().valueToJava2D(v, target, RectangleEdge.LEFT);
          double vv1 = getAxis().valueToJava2D(v + increment, target, RectangleEdge.LEFT);
          double hh = Math.abs(vv1 - vv0) + 1.0;
          r.setRect(target.getMinX(), Math.min(vv0, vv1), stripWidth, hh);
          g2.setPaint(p);
          g2.fill(r);
        }
        if (isStripOutlineVisible()) {
          g2.setPaint(this.getStripOutlinePaint());
          g2.setStroke(this.getStripOutlineStroke());
          g2.draw(new Rectangle2D.Double(target.getMinX(), target.getMinY(), stripWidth, target.getHeight()));
        }
        getAxis().draw(g2, target.getMinX() + stripWidth + this.getAxisOffset(), target, target, RectangleEdge.RIGHT, null);
      }
    }
    return null;
  }
}
