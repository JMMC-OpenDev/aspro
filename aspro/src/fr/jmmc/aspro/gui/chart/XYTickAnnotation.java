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

import fr.jmmc.oiexplorer.core.gui.chart.ChartUtils;
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
import org.jfree.ui.TextAnchor;
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
public final class XYTickAnnotation extends XYTextAnnotation
        implements Cloneable, PublicCloneable, Serializable {

    /** For serialization. */
    private static final long serialVersionUID = -4031161445009858551L;
    /** The default label margin (in Java2D units). */
    public static final double DEFAULT_LABEL_MARGIN = -1d;
    /** shared drawing line */
    private static final Line2D.Double drawLine = new Line2D.Double();

    /* members */
    /** cosinus of the angle of the arrow's line */
    private final double cosAngle;
    /** sinus of the angle of the arrow's line */
    private final double sinAngle;
    /** The line stroke. */
    private transient Stroke lineStroke;
    /** The line paint. */
    private transient Paint linePaint;

    /**
     * Creates a new label and arrow annotation.
     *
     * @param label  the label (<code>null</code> permitted).
     * @param x  the x-coordinate (measured against the chart's domain axis).
     * @param y  the y-coordinate (measured against the chart's range axis).
     * @param cosAngle  cosinus of the angle of the arrow's line (in radians).
     * @param sinAngle  sinus of the angle of the arrow's line (in radians).
     */
    public XYTickAnnotation(final String label, final double x, final double y,
            final double cosAngle, final double sinAngle) {

        super(label, x, y);

        this.cosAngle = cosAngle;
        this.sinAngle = sinAngle;

        this.lineStroke = ChartUtils.DEFAULT_STROKE;
        this.linePaint = Color.BLACK;

        setTextAnchor(TextAnchor.TOP_CENTER);
        setRotationAnchor(TextAnchor.TOP_CENTER);

        setOutlineVisible(false);
    }

    /**
     * Returns the stroke used to draw the arrow line.
     *
     * @return The arrow stroke (never <code>null</code>).
     *
     * @see #setLineStroke(Stroke)
     */
    public Stroke getLineStroke() {
        return this.lineStroke;
    }

    /**
     * Sets the stroke used to draw the arrow line.
     *
     * @param stroke  the stroke (<code>null</code> not permitted).
     *
     * @see #getLineStroke()
     */
    public void setLineStroke(final Stroke stroke) {
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
     * @see #setLinePaint(Paint)
     */
    public Paint getLinePaint() {
        return this.linePaint;
    }

    /**
     * Sets the paint used for the arrow.
     *
     * @param paint  the arrow paint (<code>null</code> not permitted).
     *
     * @see #getLinePaint()
     */
    public void setLinePaint(final Paint paint) {
        if (paint == null) {
            throw new IllegalArgumentException("Null 'paint' argument.");
        }
        this.linePaint = paint;
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
     * @param info  the plot rendering info.
     */
    @Override
    public void draw(final Graphics2D g2d, final XYPlot plot, final Rectangle2D dataArea,
            final ValueAxis domainAxis, final ValueAxis rangeAxis,
            final int rendererIndex, final PlotRenderingInfo info) {

        final PlotOrientation orientation = plot.getOrientation();

        final RectangleEdge domainEdge = Plot.resolveDomainAxisLocation(plot.getDomainAxisLocation(), orientation);
        final RectangleEdge rangeEdge = Plot.resolveRangeAxisLocation(plot.getRangeAxisLocation(), orientation);

        double j2DX = domainAxis.valueToJava2D(getX(), dataArea, domainEdge);
        double j2DY = rangeAxis.valueToJava2D(getY(), dataArea, rangeEdge);

        // Use Observability Plot Context to determine once for all the appropriate font size
        // that best fits the bar width; if too large, do not render the annotation

        final ObservabilityPlotContext renderContext = ObservabilityPlotContext.getInstance();

        // convert the tip radius in data units (equals to bar width / 2) i.e. domain axis :
        final double j2Radius = domainAxis.lengthToJava2D(renderContext.getTipRadius(), dataArea, domainEdge);

        Font bestFont;
        double halfTickLength;

        if (renderContext.autoFitTipDone()) {
            halfTickLength = renderContext.autoFitTickLength();
            bestFont = renderContext.autoFitTipFont();
        } else {
            // first time, perform fit:

            // convert the max tip height in data units (equals to margin) i.e. domain axis :
            final double j2MaxHeight = domainAxis.lengthToJava2D(renderContext.getMaxTipHeight(), dataArea, domainEdge);

            halfTickLength = renderContext.autoFitTickLength(Math.min(j2Radius, j2MaxHeight));

            final double j2MaxTextHeight = j2MaxHeight - (halfTickLength + 2d * DEFAULT_LABEL_MARGIN);

            bestFont = renderContext.autoFitTipFont(g2d, j2MaxTextHeight);
        }

        // Dont render if the tick is too small:
        if (halfTickLength == 0d) {
            return;
        }

        // Dont render if the text do not fit in block size:
        if (bestFont == null) {
            return;
        }
        setFont(bestFont);

        final double baseRadiusScaled = j2Radius - halfTickLength;
        final double tipRadiusScaled = j2Radius + halfTickLength;

        if (orientation == PlotOrientation.HORIZONTAL) {
            final double temp = j2DX;
            j2DX = j2DY;
            j2DY = temp;
        }

        final double startX = j2DX + cosAngle * baseRadiusScaled;
        final double startY = j2DY + sinAngle * baseRadiusScaled;

        final double endX = j2DX + cosAngle * tipRadiusScaled;
        final double endY = j2DY + sinAngle * tipRadiusScaled;

        drawLine.setLine(startX, startY, endX, endY);

        // clipping checks:
        if (drawLine.intersects(dataArea)) {
            g2d.setStroke(this.lineStroke);
            g2d.setPaint(this.linePaint);
            g2d.draw(drawLine);

            // draw the label
            final float labelX = (float) (j2DX + cosAngle * (tipRadiusScaled + DEFAULT_LABEL_MARGIN));
            final float labelY = (float) (j2DY + sinAngle * (tipRadiusScaled + DEFAULT_LABEL_MARGIN));
            g2d.setFont(getFont());

            final Paint background = getBackgroundPaint();
            final boolean drawOutline = isOutlineVisible();
            final String toolTip = getToolTipText();
            final String url = getURL();

            // only compute text area if needed:
            final Shape hotspot = (background != null || drawOutline || toolTip != null || url != null)
                    ? TextUtilities.calculateRotatedStringBounds(getText(), g2d, labelX, labelY,
                    getTextAnchor(), getRotationAngle(), getRotationAnchor())
                    : null;

            if (background != null) {
                g2d.setPaint(getBackgroundPaint());
                g2d.fill(hotspot);
            }

            g2d.setPaint(getPaint());
            TextUtilities.drawRotatedString(getText(), g2d, labelX, labelY, getTextAnchor(), getRotationAngle(), getRotationAnchor());

            if (drawOutline) {
                g2d.setStroke(getOutlineStroke());
                g2d.setPaint(getOutlinePaint());
                g2d.draw(hotspot);
            }

            if (toolTip != null || url != null) {
                addEntity(info, hotspot, rendererIndex, toolTip, url);
            }
        }
    }
}
