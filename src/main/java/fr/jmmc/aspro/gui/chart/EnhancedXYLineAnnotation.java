/* ===========================================================
 * JFreeChart : a free chart library for the Java(tm) platform
 * ===========================================================
 *
 * (C) Copyright 2000-2008, by Object Refinery Limited and Contributors.
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
 * --------------------
 * XYBoxAnnotation.java
 * --------------------
 * (C) Copyright 2005-2008, by Object Refinery Limited and Contributors.
 *
 * Original Author:  David Gilbert (for Object Refinery Limited);
 * Contributor(s):   -;
 *
 * Changes:
 * --------
 * 19-Jan-2005 : Version 1 (DG);
 * 06-Jun-2005 : Fixed equals() method to handle GradientPaint (DG);
 *
 */
package fr.jmmc.aspro.gui.chart;

import fr.jmmc.oiexplorer.core.gui.chart.ChartUtils;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Stroke;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import org.jfree.chart.annotations.AbstractXYAnnotation;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.Plot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.PlotRenderingInfo;
import org.jfree.chart.plot.XYPlot;
import org.jfree.io.SerialUtilities;
import org.jfree.ui.RectangleEdge;
import org.jfree.util.ObjectUtilities;
import org.jfree.util.PaintUtilities;
import org.jfree.util.PublicCloneable;

/**
 * A line annotation that can be placed on an {@link XYPlot}.  The line coordinates are specified in data space.
 *
 * @author bourgesl
 */
public final class EnhancedXYLineAnnotation extends AbstractXYAnnotation
        implements Cloneable, PublicCloneable, Serializable {

    /** For serialization. */
    private static final long serialVersionUID = 1L;
    /** shared clip rect */
    private static final Rectangle2D.Double clipRect = new Rectangle2D.Double();
    /** shared drawing line */
    private static final Line2D.Double drawLine = new Line2D.Double();

    /* members */
    /** The lower x-coordinate. */
    private double x0;
    /** The lower y-coordinate. */
    private double y0;
    /** The upper x-coordinate. */
    private double x1;
    /** The upper y-coordinate. */
    private double y1;
    /** The stroke used to draw the line. */
    private transient BasicStroke stroke;
    /** The paint used to draw the line. */
    private transient Paint paint;

    /**
     * Creates a new annotation (where, by default, the line is drawn
     * with a black outline).
     *
     * @param x0  the lower x-coordinate of the box (in data space).
     * @param y0  the lower y-coordinate of the box (in data space).
     * @param x1  the upper x-coordinate of the box (in data space).
     * @param y1  the upper y-coordinate of the box (in data space).
     */
    public EnhancedXYLineAnnotation(final double x0, final double y0, final double x1, final double y1) {
        this(x0, y0, x1, y1, ChartUtils.DEFAULT_STROKE, Color.BLACK);
    }

    /**
     * Creates a new annotation where the line is drawn using
     * the specified <code>stroke</code> and <code>paint</code>.
     *
     * @param x0  the lower x-coordinate of the box (in data space).
     * @param y0  the lower y-coordinate of the box (in data space).
     * @param x1  the upper x-coordinate of the box (in data space).
     * @param y1  the upper y-coordinate of the box (in data space).
     * @param stroke  the shape stroke (<code>null</code> permitted).
     * @param paint  the shape color (<code>null</code> permitted).
     */
    public EnhancedXYLineAnnotation(final double x0, final double y0, final double x1, final double y1,
            final BasicStroke stroke, final Paint paint) {
        this.x0 = x0;
        this.y0 = y0;
        this.x1 = x1;
        this.y1 = y1;
        this.stroke = stroke;
        this.paint = paint;
    }

    /**
     * Draws the annotation.  This method is usually called by the
     * {@link XYPlot} class, you shouldn't need to call it directly.
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
    public void draw(final Graphics2D g2, final XYPlot plot, final Rectangle2D dataArea,
            final ValueAxis domainAxis, final ValueAxis rangeAxis,
            final int rendererIndex, final PlotRenderingInfo info) {

        if (this.stroke != null && this.paint != null) {

            final PlotOrientation orientation = plot.getOrientation();

            final RectangleEdge domainEdge = Plot.resolveDomainAxisLocation(plot.getDomainAxisLocation(), orientation);
            final RectangleEdge rangeEdge = Plot.resolveRangeAxisLocation(plot.getRangeAxisLocation(), orientation);

            final double transX0 = domainAxis.valueToJava2D(this.x0, dataArea, domainEdge);
            final double transX1 = domainAxis.valueToJava2D(this.x1, dataArea, domainEdge);

            final double transY0 = rangeAxis.valueToJava2D(this.y0, dataArea, rangeEdge);
            final double transY1 = rangeAxis.valueToJava2D(this.y1, dataArea, rangeEdge);

            // Perform manual clipping because dashed stroke causes a major performance penalty
            // for large shapes (no clipping in Java2D rendering)

            // margin arround clipping bounds:
            final double margin = 1d + 0.5d * this.stroke.getLineWidth();

            if (orientation == PlotOrientation.HORIZONTAL) {
                // HORIZONTAL
                drawLine.setLine(transY0, transX0, transY1, transX1);

                // data area boundaries with stroke margin:
                clipRect.setFrame(dataArea.getMinY() - margin, dataArea.getMinX() - margin,
                        dataArea.getHeight() + 2.0 * margin, dataArea.getWidth() + 2.0 * margin);

            } else {
                // VERTICAL
                drawLine.setLine(transX0, transY0, transX1, transY1);

                // data area boundaries with stroke margin:
                clipRect.setFrame(dataArea.getMinX() - margin, dataArea.getMinY() - margin,
                        dataArea.getWidth() + 2.0 * margin, dataArea.getHeight() + 2.0 * margin);
            }

            // clipping checks:
            if (clipLine(drawLine, clipRect)) {
                g2.setPaint(this.paint);
                g2.setStroke(this.stroke);
                g2.draw(drawLine);
            }
        }
    }

    /**
     * Tests this annotation for equality with an arbitrary object.
     *
     * @param obj  the object (<code>null</code> permitted).
     *
     * @return A boolean.
     */
    @Override
    public boolean equals(final Object obj) {
        if (obj == this) {
            return true;
        }
        // now try to reject equality
        if (!super.equals(obj)) {
            return false;
        }
        if (!(obj instanceof EnhancedXYLineAnnotation)) {
            return false;
        }
        final EnhancedXYLineAnnotation that = (EnhancedXYLineAnnotation) obj;
        if (!(this.x0 == that.getX0())) {
            return false;
        }
        if (!(this.y0 == that.getY0())) {
            return false;
        }
        if (!(this.x1 == that.getX1())) {
            return false;
        }
        if (!(this.y1 == that.getY1())) {
            return false;
        }
        if (!ObjectUtilities.equal(this.stroke, that.getStroke())) {
            return false;
        }
        if (!PaintUtilities.equal(this.paint, that.getPaint())) {
            return false;
        }
        // seem to be the same
        return true;
    }

    /**
     * Returns a hash code.
     *
     * @return A hash code.
     */
    @Override
    public int hashCode() {
        int result;
        long temp;
        temp = Double.doubleToLongBits(this.x0);
        result = (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(this.x1);
        result = 29 * result + (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(this.y0);
        result = 29 * result + (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(this.y1);
        result = 29 * result + (int) (temp ^ (temp >>> 32));
        return result;
    }

    /**
     * Returns a clone.
     *
     * @return A clone.
     *
     * @throws CloneNotSupportedException not thrown by this class, but may be
     *                                    by subclasses.
     */
    @Override
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }

    /**
     * Provides serialization support.
     *
     * @param stream  the output stream (<code>null</code> not permitted).
     *
     * @throws IOException if there is an I/O error.
     */
    private void writeObject(final ObjectOutputStream stream) throws IOException {
        stream.defaultWriteObject();
        SerialUtilities.writeStroke(this.stroke, stream);
        SerialUtilities.writePaint(this.paint, stream);
    }

    /**
     * Provides serialization support.
     *
     * @param stream  the input stream (<code>null</code> not permitted).
     *
     * @throws IOException  if there is an I/O error.
     * @throws ClassNotFoundException  if there is a classpath problem.
     */
    private void readObject(final ObjectInputStream stream)
            throws IOException, ClassNotFoundException {

        stream.defaultReadObject();
        this.stroke = (BasicStroke) SerialUtilities.readStroke(stream);
        this.paint = SerialUtilities.readPaint(stream);
    }

    /**
     * Return the lower x-coordinate.
     * @return lower x-coordinate.
     */
    public double getX0() {
        return x0;
    }

    /**
     * Define the lower x-coordinate.
     * @param x0 lower x-coordinate.
     */
    public void setX0(final double x0) {
        this.x0 = x0;
    }

    /**
     * Return the The lower y-coordinate.
     * @return The lower y-coordinate.
     */
    public double getY0() {
        return y0;
    }

    /**
     * Define the The lower y-coordinate.
     * @param y0 The lower y-coordinate.
     */
    public void setY0(final double y0) {
        this.y0 = y0;
    }

    /**
     * Return the upper x-coordinate.
     * @return upper x-coordinate.
     */
    public double getX1() {
        return x1;
    }

    /**
     * Define the upper x-coordinate.
     * @param x1 upper x-coordinate.
     */
    public void setX1(final double x1) {
        this.x1 = x1;
    }

    /**
     * Return the The upper y-coordinate
     * @return The upper y-coordinate
     */
    public double getY1() {
        return y1;
    }

    /**
     * Define the The upper y-coordinate
     * @param y1 The upper y-coordinate
     */
    public void setY1(final double y1) {
        this.y1 = y1;
    }

    /**
     * Return the stroke used to draw the box outline
     * @return stroke used to draw the box outline
     */
    public Stroke getStroke() {
        return stroke;
    }

    /**
     * Return the paint used to draw the line
     * @return paint used to draw the line
     */
    public Paint getPaint() {
        return paint;
    }

    /**
     * Clips the specified line to the given rectangle.
     *
     * @param line  the line (<code>null</code> not permitted).
     * @param rect  the clipping rectangle (<code>null</code> not permitted).
     *
     * @return <code>true</code> if the clipped line is visible, and
     *     <code>false</code> otherwise.
     */
    public static boolean clipLine(final Line2D line, final Rectangle2D rect) {

        double x1 = line.getX1();
        double y1 = line.getY1();
        double x2 = line.getX2();
        double y2 = line.getY2();

        final double minX = rect.getMinX();
        final double maxX = rect.getMaxX();
        final double minY = rect.getMinY();
        final double maxY = rect.getMaxY();

        int f1 = rect.outcode(x1, y1);
        int f2 = rect.outcode(x2, y2);

        double dx, dy;

        while ((f1 | f2) != 0) {
            if ((f1 & f2) != 0) {
                return false;
            }
            dx = (x2 - x1);
            dy = (y2 - y1);

            // update (x1, y1), (x2, y2) and f1 and f2 using intersections
            // then recheck
            if (f1 != 0) {
                // first point is outside, so we update it against one of the
                // four sides then continue
                if ((f1 & Rectangle2D.OUT_LEFT) == Rectangle2D.OUT_LEFT && dx != 0.0) {
                    y1 += (minX - x1) * dy / dx;
                    x1 = minX;
                } else if ((f1 & Rectangle2D.OUT_RIGHT) == Rectangle2D.OUT_RIGHT && dx != 0.0) {
                    y1 += (maxX - x1) * dy / dx;
                    x1 = maxX;
                } else if ((f1 & Rectangle2D.OUT_BOTTOM) == Rectangle2D.OUT_BOTTOM && dy != 0.0) {
                    x1 += (maxY - y1) * dx / dy;
                    y1 = maxY;
                } else if ((f1 & Rectangle2D.OUT_TOP) == Rectangle2D.OUT_TOP && dy != 0.0) {
                    x1 += (minY - y1) * dx / dy;
                    y1 = minY;
                }
                f1 = rect.outcode(x1, y1);
            } else if (f2 != 0) {
                // second point is outside, so we update it against one of the
                // four sides then continue
                if ((f2 & Rectangle2D.OUT_LEFT) == Rectangle2D.OUT_LEFT && dx != 0.0) {
                    y2 += (minX - x2) * dy / dx;
                    x2 = minX;
                } else if ((f2 & Rectangle2D.OUT_RIGHT) == Rectangle2D.OUT_RIGHT && dx != 0.0) {
                    y2 += (maxX - x2) * dy / dx;
                    x2 = maxX;
                } else if ((f2 & Rectangle2D.OUT_BOTTOM) == Rectangle2D.OUT_BOTTOM && dy != 0.0) {
                    x2 += (maxY - y2) * dx / dy;
                    y2 = maxY;
                } else if ((f2 & Rectangle2D.OUT_TOP) == Rectangle2D.OUT_TOP && dy != 0.0) {
                    x2 += (minY - y2) * dx / dy;
                    y2 = minY;
                }
                f2 = rect.outcode(x2, y2);
            }
        }

        line.setLine(x1, y1, x2, y2);

        // the line is visible - if it wasn't, we'd have
        // returned false from within the while loop above
        return true;
    }
}
