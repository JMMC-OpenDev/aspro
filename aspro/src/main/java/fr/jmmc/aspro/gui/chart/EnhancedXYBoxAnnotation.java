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

import fr.jmmc.jmcs.util.StringUtils;
import fr.jmmc.oiexplorer.core.gui.chart.ChartUtils;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Stroke;
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
import org.jfree.chart.ui.GradientPaintTransformer;
import org.jfree.chart.ui.Layer;
import org.jfree.chart.ui.RectangleEdge;
import org.jfree.chart.ui.StandardGradientPaintTransformer;
import org.jfree.chart.util.ObjectUtils;
import org.jfree.chart.util.PaintUtils;
import org.jfree.chart.util.PublicCloneable;
import org.jfree.chart.util.SerialUtils;

/**
 * A box annotation that can be placed on an {@link XYPlot}.  The box coordinates are specified in data space.
 *
 * @author bourgesl
 */
public final class EnhancedXYBoxAnnotation extends AbstractXYAnnotation
        implements Cloneable, PublicCloneable, Serializable {

    /** For serialization. */
    private static final long serialVersionUID = 1L;
    /** shared drawing rectangle */
    private static final Rectangle2D.Double drawRect = new Rectangle2D.Double();

    private static final GradientPaintTransformer gradientTransformer = new StandardGradientPaintTransformer();

    /* members */
    /** The lower x-coordinate. */
    private double x0;
    /** The lower y-coordinate. */
    private double y0;
    /** The upper x-coordinate. */
    private double x1;
    /** The upper y-coordinate. */
    private double y1;
    /** The stroke used to draw the box outline. */
    private transient BasicStroke stroke;
    /** The paint used to draw the box outline. */
    private transient Paint outlinePaint;
    /** The paint used to fill the box. */
    private transient Paint fillPaint;
    /** highlighted flag */
    private transient boolean highlighted;
    /** The paint used to fill the box if highlighted. */
    private transient Paint highlightPaint;
    /** layer to draw this annotation = foreground or background (default) */
    private transient Layer layer;
    /** optional series index */
    private transient int seriesIndex = -1;

    /**
     * Creates a new annotation (where, by default, the box is drawn
     * with a black outline).
     *
     * @param x0  the lower x-coordinate of the box (in data space).
     * @param y0  the lower y-coordinate of the box (in data space).
     * @param x1  the upper x-coordinate of the box (in data space).
     * @param y1  the upper y-coordinate of the box (in data space).
     */
    public EnhancedXYBoxAnnotation(final double x0, final double y0, final double x1, final double y1) {
        this(x0, y0, x1, y1, ChartUtils.DEFAULT_STROKE, Color.BLACK);
    }

    /**
     * Creates a new annotation where the box is drawn as an outline using
     * the specified <code>stroke</code> and <code>outlinePaint</code>.
     *
     * @param x0  the lower x-coordinate of the box (in data space).
     * @param y0  the lower y-coordinate of the box (in data space).
     * @param x1  the upper x-coordinate of the box (in data space).
     * @param y1  the upper y-coordinate of the box (in data space).
     * @param stroke  the shape stroke (<code>null</code> permitted).
     * @param outlinePaint  the shape color (<code>null</code> permitted).
     */
    public EnhancedXYBoxAnnotation(final double x0, final double y0, final double x1, final double y1,
                                   final BasicStroke stroke, final Paint outlinePaint) {
        this(x0, y0, x1, y1, stroke, outlinePaint, null);
    }

    /**
     * Creates a new annotation.
     *
     * @param x0  the lower x-coordinate of the box (in data space).
     * @param y0  the lower y-coordinate of the box (in data space).
     * @param x1  the upper x-coordinate of the box (in data space).
     * @param y1  the upper y-coordinate of the box (in data space).
     * @param stroke  the shape stroke (<code>null</code> permitted).
     * @param outlinePaint  the shape color (<code>null</code> permitted).
     * @param fillPaint  the paint used to fill the shape (<code>null</code>
     *                   permitted).
     */
    public EnhancedXYBoxAnnotation(final double x0, final double y0, final double x1, final double y1,
                                   final BasicStroke stroke, final Paint outlinePaint, final Paint fillPaint) {
        this(x0, y0, x1, y1, stroke, outlinePaint, fillPaint, Layer.BACKGROUND, null);
    }

    /**
     * Creates a new annotation.
     *
     * @param x0  the lower x-coordinate of the box (in data space).
     * @param y0  the lower y-coordinate of the box (in data space).
     * @param x1  the upper x-coordinate of the box (in data space).
     * @param y1  the upper y-coordinate of the box (in data space).
     * @param stroke  the shape stroke (<code>null</code> permitted).
     * @param outlinePaint  the shape color (<code>null</code> permitted).
     * @param fillPaint  the paint used to fill the shape (<code>null</code>
     *                   permitted).
     * @param layer layer to draw this annotation = foreground or background (default)
     * @param tooltipText optional tooltip text
     */
    public EnhancedXYBoxAnnotation(final double x0, final double y0, final double x1, final double y1,
                                   final BasicStroke stroke, final Paint outlinePaint, final Paint fillPaint,
                                   final Layer layer, final String tooltipText) {
        this(x0, y0, x1, y1, stroke, outlinePaint, fillPaint, null, layer, tooltipText);
    }

    /**
     * Creates a new annotation.
     *
     * @param x0  the lower x-coordinate of the box (in data space).
     * @param y0  the lower y-coordinate of the box (in data space).
     * @param x1  the upper x-coordinate of the box (in data space).
     * @param y1  the upper y-coordinate of the box (in data space).
     * @param stroke  the shape stroke (<code>null</code> permitted).
     * @param outlinePaint  the shape color (<code>null</code> permitted).
     * @param fillPaint  the paint used to fill the shape (<code>null</code>
     *                   permitted).
     * @param highlightPaint  the paint used to fill the shape if highlighted (<code>null</code>
     *                   permitted).
     * @param layer layer to draw this annotation = foreground or background (default)
     * @param tooltipText optional tooltip text
     */
    public EnhancedXYBoxAnnotation(final double x0, final double y0, final double x1, final double y1,
                                   final BasicStroke stroke, final Paint outlinePaint,
                                   final Paint fillPaint, final Paint highlightPaint,
                                   final Layer layer, final String tooltipText) {
        this.x0 = x0;
        this.y0 = y0;
        this.x1 = x1;
        this.y1 = y1;
        this.stroke = stroke;
        this.outlinePaint = outlinePaint;
        this.fillPaint = fillPaint;
        this.highlightPaint = highlightPaint;
        this.layer = layer;
        if (!StringUtils.isEmpty(tooltipText)) {
            this.setToolTipText(tooltipText);
        }
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

        final PlotOrientation orientation = plot.getOrientation();

        final RectangleEdge domainEdge = Plot.resolveDomainAxisLocation(plot.getDomainAxisLocation(), orientation);
        final RectangleEdge rangeEdge = Plot.resolveRangeAxisLocation(plot.getRangeAxisLocation(), orientation);

        final double transX0 = domainAxis.valueToJava2D(this.x0, dataArea, domainEdge);
        final double transX1 = domainAxis.valueToJava2D(this.x1, dataArea, domainEdge);
        /*
         if (transX1 < transX0) {
         final double tmp = transX0;
         transX0 = transX1;
         transX1 = tmp;
         }
         */
        final double transY0 = rangeAxis.valueToJava2D(this.y0, dataArea, rangeEdge);
        final double transY1 = rangeAxis.valueToJava2D(this.y1, dataArea, rangeEdge);
        /*
         if (transY1 < transY0) {
         final double tmp = transY0;
         transY0 = transY1;
         transY1 = tmp;
         }
         */

        // Perform manual clipping because dashed stroke causes a major performance penalty
        // for large shapes (no clipping in Java2D rendering)
        // margin arround clipping bounds:
        final double margin = 1d + 0.5d * this.stroke.getLineWidth();

        // data area boundaries with stroke margin:
        final double bbx1 = dataArea.getMinX() - margin;
        final double bbx2 = dataArea.getMaxX() + margin;
        final double bby1 = dataArea.getMinY() - margin;
        final double bby2 = dataArea.getMaxY() + margin;

        double px1, py1, px2, py2;

        if (orientation == PlotOrientation.HORIZONTAL) {
            // HORIZONTAL
            px1 = crop(transY0, bbx1, bbx2);
            px2 = crop(transY1, bbx1, bbx2);
            py1 = crop(transX0, bby1, bby2);
            py2 = crop(transX1, bby1, bby2);
        } else {
            // VERTICAL
            px1 = crop(transX0, bbx1, bbx2);
            px2 = crop(transX1, bbx1, bbx2);
            py1 = crop(transY0, bby1, bby2);
            py2 = crop(transY1, bby1, bby2);
        }

        drawRect.setFrameFromDiagonal(px1, py1, px2, py2);

        // clipping checks:
        if (drawRect.intersects(dataArea)) {
            Paint paint = (this.highlighted) ? this.highlightPaint : this.fillPaint;
            if (paint != null) {
                if (paint instanceof GradientPaint) {
                    paint = gradientTransformer.transform((GradientPaint) paint, drawRect);
                }
                g2.setPaint(paint);
                g2.fill(drawRect);
            }

            if (this.stroke != null && this.outlinePaint != null) {
                g2.setPaint(this.outlinePaint);
                g2.setStroke(this.stroke);
                g2.draw(drawRect);
            }

            final String toolTip = getToolTipText();
            final String url = getURL();
            if (toolTip != null || url != null) {
                // clone shared bbox stored in drawRect:
                // note: store the optional series index into the chart entity:
                addEntity(info, drawRect.getBounds2D(), (seriesIndex != -1) ? seriesIndex : rendererIndex, toolTip, url);
            }
        }
    }

    private static double crop(final double val, final double min, final double max) {
        if (val < min) {
            return min;
        }
        if (val > max) {
            return max;
        }
        return val;
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
        if (!(obj instanceof EnhancedXYBoxAnnotation)) {
            return false;
        }
        final EnhancedXYBoxAnnotation that = (EnhancedXYBoxAnnotation) obj;
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
        if (!ObjectUtils.equal(this.stroke, that.getStroke())) {
            return false;
        }
        if (!PaintUtils.equal(this.outlinePaint, that.getOutlinePaint())) {
            return false;
        }
        if (!PaintUtils.equal(this.fillPaint, that.getFillPaint())) {
            return false;
        }
        return ObjectUtils.equal(this.layer, that.getLayer());
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
        SerialUtils.writeStroke(this.stroke, stream);
        SerialUtils.writePaint(this.outlinePaint, stream);
        SerialUtils.writePaint(this.fillPaint, stream);
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
        this.stroke = (BasicStroke) SerialUtils.readStroke(stream);
        this.outlinePaint = SerialUtils.readPaint(stream);
        this.fillPaint = SerialUtils.readPaint(stream);
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
     * Return the paint used to draw the box outline
     * @return paint used to draw the box outline
     */
    public Paint getOutlinePaint() {
        return outlinePaint;
    }

    /**
     * Return the paint used to fill the box.
     * @return 
     */
    public Paint getFillPaint() {
        return fillPaint;
    }

    /**
     * Return the layer to draw this annotation = foreground or background (default)
     * @return layer to draw this annotation = foreground or background (default)
     */
    public Layer getLayer() {
        return layer;
    }

    /**
     * Define the layer to draw this annotation = foreground or background (default)
     * @param layer layer to draw this annotation = foreground or background (default)
     */
    public void setLayer(Layer layer) {
        this.layer = layer;
    }

    /**
     * @return optional series index
     */
    public int getSeriesIndex() {
        return seriesIndex;
    }

    /**
     * Define the optional series index
     @param seriesIndex optional series index
     */
    public void setSeriesIndex(final int seriesIndex) {
        this.seriesIndex = seriesIndex;
    }

    /**
     * @return highlighted flag
     */
    public boolean isHighlighted() {
        return highlighted;
    }

    /**
     * Define the highlighted flag
     * @param highlighted highlighted flag
     */
    public void setHighlighted(final boolean highlighted) {
        this.highlighted = highlighted;
    }

    /**
     * @return paint used to fill the box if highlighted.
     */
    public Paint getHighlightPaint() {
        return highlightPaint;
    }

    /**
     * Define the paint used to fill the box if highlighted.
     * @param highlightPaint paint used to fill the box if highlighted.
     */
    public void setHighlightPaint(final Paint highlightPaint) {
        this.highlightPaint = highlightPaint;
    }

}
