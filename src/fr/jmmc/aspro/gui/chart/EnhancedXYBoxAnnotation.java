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
import java.awt.Color;
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
import org.jfree.io.SerialUtilities;
import org.jfree.ui.RectangleEdge;
import org.jfree.util.ObjectUtilities;
import org.jfree.util.PaintUtilities;
import org.jfree.util.PublicCloneable;

/**
 * A box annotation that can be placed on an {@link XYPlot}.  The box coordinates are specified in data space.
 *
 * @author bourgesl
 */
public final class EnhancedXYBoxAnnotation extends AbstractXYAnnotation
        implements Cloneable, PublicCloneable, Serializable {

  /** For serialization. */
  private static final long serialVersionUID = 1L;

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
  private transient Stroke stroke;
  /** The paint used to draw the box outline. */
  private transient Paint outlinePaint;
  /** The paint used to fill the box. */
  private transient Paint fillPaint;

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
          final Stroke stroke, final Paint outlinePaint) {
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
          final Stroke stroke, final Paint outlinePaint, final Paint fillPaint) {
    this.x0 = x0;
    this.y0 = y0;
    this.x1 = x1;
    this.y1 = y1;
    this.stroke = stroke;
    this.outlinePaint = outlinePaint;
    this.fillPaint = fillPaint;
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
    Rectangle2D box = null;
    if (orientation == PlotOrientation.HORIZONTAL) {
      box = new Rectangle2D.Double(transY0, transX0, transY1 - transY0, transX1 - transX0);
    } else if (orientation == PlotOrientation.VERTICAL) {
      box = new Rectangle2D.Double(transX0, transY0, transX1 - transX0, transY1 - transY0);
    }

    if (this.fillPaint != null) {
      g2.setPaint(this.fillPaint);
      g2.fill(box);
    }

    if (this.stroke != null && this.outlinePaint != null) {
      g2.setPaint(this.outlinePaint);
      g2.setStroke(this.stroke);
      g2.draw(box);
    }
    addEntity(info, box, rendererIndex, getToolTipText(), getURL());
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
    if (!ObjectUtilities.equal(this.stroke, that.getStroke())) {
      return false;
    }
    if (!PaintUtilities.equal(this.outlinePaint, that.getOutlinePaint())) {
      return false;
    }
    if (!PaintUtilities.equal(this.fillPaint, that.getFillPaint())) {
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
    SerialUtilities.writePaint(this.outlinePaint, stream);
    SerialUtilities.writePaint(this.fillPaint, stream);
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
    this.stroke = SerialUtilities.readStroke(stream);
    this.outlinePaint = SerialUtilities.readPaint(stream);
    this.fillPaint = SerialUtilities.readPaint(stream);
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
   * Define the stroke used to draw the box outline
   * @param stroke stroke used to draw the box outline
   */
  public void setStroke(final Stroke stroke) {
    this.stroke = stroke;
  }

  /**
   * Return the paint used to draw the box outline
   * @return paint used to draw the box outline
   */
  public Paint getOutlinePaint() {
    return outlinePaint;
  }

  /**
   * Define the paint used to draw the box outline
   * @param outlinePaint paint used to draw the box outline
   */
  public void setOutlinePaint(final Paint outlinePaint) {
    this.outlinePaint = outlinePaint;
  }

  /**
   * Return the paint used to fill the box.
   * @return 
   */
  public Paint getFillPaint() {
    return fillPaint;
  }

  /**
   * Define the paint used to fill the box.
   * @param fillPaint paint to use
   */
  public void setFillPaint(final Paint fillPaint) {
    this.fillPaint = fillPaint;
  }
}
