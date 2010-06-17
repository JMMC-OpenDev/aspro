/* ===========================================================
 * JFreeChart : a free chart library for the Java(tm) platform
 * ===========================================================
 *
 * (C) Copyright 2000-2009, by Object Refinery Limited and Contributors.
 *
 * Project Info:  http://www.jfree.org/jfreechart/index.html
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 *
 * [Java is a trademark or registered trademark of Sun Microsystems, Inc.
 * in the United States and other countries.]
 *
 * ------------------------
 * XYPointerAnnotation.java
 * ------------------------
 * (C) Copyright 2003-2009, by Object Refinery Limited.
 *
 * Original Author:  David Gilbert (for Object Refinery Limited);
 * Contributor(s):   -;
 *
 * Changes:
 * --------
 * 21-May-2003 : Version 1 (DG);
 * 10-Jun-2003 : Changed BoundsAnchor to TextAnchor (DG);
 * 02-Jul-2003 : Added accessor methods and simplified constructor (DG);
 * 19-Aug-2003 : Implemented Cloneable (DG);
 * 13-Oct-2003 : Fixed bug where arrow paint is not set correctly (DG);
 * 21-Jan-2004 : Update for renamed method in ValueAxis (DG);
 * 29-Sep-2004 : Changes to draw() method signature (DG);
 * ------------- JFREECHART 1.0.x ---------------------------------------------
 * 20-Feb-2006 : Correction for equals() method (fixes bug 1435160) (DG);
 * 12-Jul-2006 : Fix drawing for PlotOrientation.HORIZONTAL, thanks to
 *               Skunk (DG);
 * 12-Feb-2009 : Added support for rotated label, plus background and
 *               outline (DG);
 *
 */
package fr.jmmc.aspro.gui.chart;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.io.Serializable;

import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.Plot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.PlotRenderingInfo;
import org.jfree.chart.plot.XYPlot;
import org.jfree.text.TextUtilities;
import org.jfree.ui.RectangleEdge;
import org.jfree.util.PublicCloneable;

/**
 * An arrow and label that can be placed on an {@link XYPlot}.  The arrow is
 * drawn at a user-definable angle so that it points towards the (x, y)
 * location for the annotation.
 * <p>
 * The arrow length (and its offset from the (x, y) location) is controlled by
 * the tip radius and the base radius attributes.  Imagine two circles around
 * the (x, y) coordinate: the inner circle defined by the tip radius, and the
 * outer circle defined by the base radius.  Now, draw the arrow starting at
 * some point on the outer circle (the point is determined by the angle), with
 * the arrow tip being drawn at a corresponding point on the inner circle.
 *
 */
public class XYTickAnnotation extends XYTextAnnotation
        implements Cloneable, PublicCloneable, Serializable {

    /** For serialization. */
    private static final long serialVersionUID = -4031161445009858551L;

    /** The default font for labels */
    private static final Font DEFAULT_LABEL_FONT = new Font("SansSerif", Font.PLAIN, 8);

    /** The default tip radius (in Java2D units). */
    public static final double DEFAULT_TIP_RADIUS = 15.0;

    /** The default base radius (in Java2D units). */
    public static final double DEFAULT_BASE_RADIUS = 20.0;

    /** The default label offset (in Java2D units). */
    public static final double DEFAULT_LABEL_OFFSET = 4.0;

    /** The angle of the arrow's line (in radians). */
    private double angle;

    /**
     * The radius from the (x, y) point to the tip of the arrow (in Java2D
     * units).
     */
    private double tipRadius;
    /**
     * The radius from the (x, y) point to the start of the arrow line (in
     * Java2D units).
     */
    private double baseRadius;

    /** The line stroke. */
    private transient Stroke lineStroke;

    /** The line paint. */
    private transient Paint linePaint;

    /** The radius from the base point to the anchor point for the label. */
    private double labelOffset;

    /**
     * Creates a new label and arrow annotation.
     *
     * @param label  the label (<code>null</code> permitted).
     * @param x  the x-coordinate (measured against the chart's domain axis).
     * @param y  the y-coordinate (measured against the chart's range axis).
     * @param angle  the angle of the arrow's line (in radians).
     */
    public XYTickAnnotation(String label, double x, double y, double angle) {

        super(label, x, y);
        this.angle = angle;
        this.tipRadius = DEFAULT_TIP_RADIUS;
        this.baseRadius = DEFAULT_BASE_RADIUS;
        this.labelOffset = DEFAULT_LABEL_OFFSET;
        this.lineStroke = new BasicStroke(1.0f);

        // hack :
        this.linePaint = Color.BLACK;
        setFont(DEFAULT_LABEL_FONT);
    }

    /**
     * Returns the angle of the arrow.
     *
     * @return The angle (in radians).
     *
     * @see #setAngle(double)
     */
    public double getAngle() {
        return this.angle;
    }

    /**
     * Sets the angle of the arrow.
     *
     * @param angle  the angle (in radians).
     *
     * @see #getAngle()
     */
    public void setAngle(double angle) {
        this.angle = angle;
    }

    /**
     * Returns the tip radius.
     *
     * @return The tip radius (in Java2D units).
     *
     * @see #setTipRadius(double)
     */
    public double getTipRadius() {
        return this.tipRadius;
    }

    /**
     * Sets the tip radius.
     *
     * @param radius  the radius (in Java2D units).
     *
     * @see #getTipRadius()
     */
    public void setTipRadius(double radius) {
        this.tipRadius = radius;
    }
    
    /**
     * Returns the base radius.
     *
     * @return The base radius (in Java2D units).
     *
     * @see #setBaseRadius(double)
     */
    public double getBaseRadius() {
        return this.baseRadius;
    }

    /**
     * Sets the base radius.
     *
     * @param radius  the radius (in Java2D units).
     *
     * @see #getBaseRadius()
     */
    public void setBaseRadius(double radius) {
        this.baseRadius = radius;
    }

    /**
     * Returns the label offset.
     *
     * @return The label offset (in Java2D units).
     *
     * @see #setLabelOffset(double)
     */
    public double getLabelOffset() {
        return this.labelOffset;
    }

    /**
     * Sets the label offset (from the arrow base, continuing in a straight
     * line, in Java2D units).
     *
     * @param offset  the offset (in Java2D units).
     *
     * @see #getLabelOffset()
     */
    public void setLabelOffset(double offset) {
        this.labelOffset = offset;
    }

    /**
     * Returns the stroke used to draw the arrow line.
     *
     * @return The arrow stroke (never <code>null</code>).
     *
     * @see #setArrowStroke(Stroke)
     */
    public Stroke getLineStroke() {
        return this.lineStroke;
    }

    /**
     * Sets the stroke used to draw the arrow line.
     *
     * @param stroke  the stroke (<code>null</code> not permitted).
     *
     * @see #getArrowStroke()
     */
    public void setLineStroke(Stroke stroke) {
        if (stroke == null) {
            throw new IllegalArgumentException("Null 'stroke' not permitted.");
        }
        this.lineStroke = stroke;
    }

    /**
     * Returns the paint used for the arrow.
     *
     * @return The arrow paint (never <code>null</code>).
     *
     * @see #setArrowPaint(Paint)
     */
    public Paint getLinePaint() {
        return this.linePaint;
    }

    /**
     * Sets the paint used for the arrow.
     *
     * @param paint  the arrow paint (<code>null</code> not permitted).
     *
     * @see #getArrowPaint()
     */
    public void setLinePaint(Paint paint) {
        if (paint == null) {
            throw new IllegalArgumentException("Null 'paint' argument.");
        }
        this.linePaint = paint;
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
     * @param info  the plot rendering info.
     */
    @Override
    public void draw(Graphics2D g2, XYPlot plot, Rectangle2D dataArea,
                     ValueAxis domainAxis, ValueAxis rangeAxis,
                     int rendererIndex,
                     PlotRenderingInfo info) {

        PlotOrientation orientation = plot.getOrientation();

        RectangleEdge domainEdge = Plot.resolveDomainAxisLocation(
                plot.getDomainAxisLocation(), orientation);
        RectangleEdge rangeEdge = Plot.resolveRangeAxisLocation(
                plot.getRangeAxisLocation(), orientation);

        double j2DX = domainAxis.valueToJava2D(getX(), dataArea, domainEdge);
        double j2DY = rangeAxis.valueToJava2D(getY(), dataArea, rangeEdge);

        if (orientation == PlotOrientation.HORIZONTAL) {
            double temp = j2DX;
            j2DX = j2DY;
            j2DY = temp;
        }
        double startX = j2DX + Math.cos(this.angle) * this.baseRadius;
        double startY = j2DY + Math.sin(this.angle) * this.baseRadius;

        double endX = j2DX + Math.cos(this.angle) * this.tipRadius;
        double endY = j2DY + Math.sin(this.angle) * this.tipRadius;

        g2.setStroke(this.lineStroke);
        g2.setPaint(this.linePaint);
        Line2D line = new Line2D.Double(startX, startY, endX, endY);
        g2.draw(line);

        // draw the label
        double labelX = j2DX + Math.cos(this.angle) * (this.baseRadius
                + this.labelOffset);
        double labelY = j2DY + Math.sin(this.angle) * (this.baseRadius
                + this.labelOffset);
        g2.setFont(getFont());

        Shape hotspot = TextUtilities.calculateRotatedStringBounds(
                getText(), g2, (float) labelX, (float) labelY, getTextAnchor(),
                getRotationAngle(), getRotationAnchor());
        if (getBackgroundPaint() != null) {
            g2.setPaint(getBackgroundPaint());
            g2.fill(hotspot);
        }
        g2.setPaint(getPaint());
        TextUtilities.drawRotatedString(getText(), g2, (float) labelX,
                (float) labelY, getTextAnchor(), getRotationAngle(),
                getRotationAnchor());
        if (isOutlineVisible()) {
            g2.setStroke(getOutlineStroke());
            g2.setPaint(getOutlinePaint());
            g2.draw(hotspot);
        }

        String toolTip = getToolTipText();
        String url = getURL();
        if (toolTip != null || url != null) {
            addEntity(info, hotspot, rendererIndex, toolTip, url);
        }

    }

}
