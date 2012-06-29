/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Stroke;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.event.RendererChangeEvent;
import org.jfree.chart.plot.CrosshairState;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.PlotRenderingInfo;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRendererState;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.Range;
import org.jfree.data.general.DatasetUtilities;
import org.jfree.data.xy.IntervalXYDataset;
import org.jfree.data.xy.XYDataset;
import org.jfree.io.SerialUtilities;
import org.jfree.ui.RectangleEdge;
import org.jfree.util.ObjectUtilities;
import org.jfree.util.PaintUtilities;

/**
 * This class extends XYErrorRenderer for performance
 * @author bourgesl
 */
public final class FastXYErrorRenderer extends XYLineAndShapeRenderer {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1L;

  /* member */
  /** A flag that controls whether or not the x-error bars are drawn. */
  private boolean drawXError;
  /** A flag that controls whether or not the y-error bars are drawn. */
  private boolean drawYError;
  /** The length of the cap at the end of the error bars. */
  private double capLength;
  /**
   * The paint used to draw the error bars (if <code>null</code> we use the
   * series paint).
   */
  private transient Paint errorPaint;
  /**
   * The stroke used to draw the error bars (if <code>null</code> we use the
   * series outline stroke).
   *
   * @since 1.0.13
   */
  private transient Stroke errorStroke;
  /** flag to draw cap */
  private transient boolean useCap;

  /**
   * Creates a new <code>XYErrorRenderer</code> instance.
   */
  public FastXYErrorRenderer() {
    super(false, true);
    this.drawXError = true;
    this.drawYError = true;
    this.errorPaint = null;
    this.errorStroke = null;
    setCapLength(4d);
  }

  /**
   * Returns the flag that controls whether or not the renderer draws error
   * bars for the x-values.
   *
   * @return A boolean.
   *
   * @see #setDrawXError(boolean)
   */
  public boolean getDrawXError() {
    return this.drawXError;
  }

  /**
   * Sets the flag that controls whether or not the renderer draws error
   * bars for the x-values and, if the flag changes, sends a
   * {@link RendererChangeEvent} to all registered listeners.
   *
   * @param draw  the flag value.
   *
   * @see #getDrawXError()
   */
  public void setDrawXError(final boolean draw) {
    if (this.drawXError != draw) {
      this.drawXError = draw;
      fireChangeEvent();
    }
  }

  /**
   * Returns the flag that controls whether or not the renderer draws error
   * bars for the y-values.
   *
   * @return A boolean.
   *
   * @see #setDrawYError(boolean)
   */
  public boolean getDrawYError() {
    return this.drawYError;
  }

  /**
   * Sets the flag that controls whether or not the renderer draws error
   * bars for the y-values and, if the flag changes, sends a
   * {@link RendererChangeEvent} to all registered listeners.
   *
   * @param draw  the flag value.
   *
   * @see #getDrawYError()
   */
  public void setDrawYError(final boolean draw) {
    if (this.drawYError != draw) {
      this.drawYError = draw;
      fireChangeEvent();
    }
  }

  /**
   * Returns the length (in Java2D units) of the cap at the end of the error
   * bars.
   *
   * @return The cap length.
   *
   * @see #setCapLength(double)
   */
  public double getCapLength() {
    return this.capLength;
  }

  /**
   * Sets the length of the cap at the end of the error bars, and sends a
   * {@link RendererChangeEvent} to all registered listeners.
   *
   * @param length  the length (in Java2D units).
   *
   * @see #getCapLength()
   */
  public void setCapLength(final double length) {
    this.capLength = length;
    this.useCap = (length > 0d);
    fireChangeEvent();
  }

  /**
   * Returns the paint used to draw the error bars.  If this is
   * <code>null</code> (the default), the item paint is used instead.
   *
   * @return The paint (possibly <code>null</code>).
   *
   * @see #setErrorPaint(Paint)
   */
  public Paint getErrorPaint() {
    return this.errorPaint;
  }

  /**
   * Sets the paint used to draw the error bars and sends a
   * {@link RendererChangeEvent} to all registered listeners.
   *
   * @param paint  the paint (<code>null</code> permitted).
   *
   * @see #getErrorPaint()
   */
  public void setErrorPaint(final Paint paint) {
    this.errorPaint = paint;
    fireChangeEvent();
  }

  /**
   * Returns the stroke used to draw the error bars.  If this is
   * <code>null</code> (the default), the item outline stroke is used
   * instead.
   *
   * @return The stroke (possibly <code>null</code>).
   *
   * @see #setErrorStroke(Stroke)
   *
   * @since 1.0.13
   */
  public Stroke getErrorStroke() {
    return this.errorStroke;
  }

  /**
   * Sets the stroke used to draw the error bars and sends a
   * {@link RendererChangeEvent} to all registered listeners.
   *
   * @param stroke   the stroke (<code>null</code> permitted).
   *
   * @see #getErrorStroke()
   *
   * @since 1.0.13
   */
  public void setErrorStroke(final Stroke stroke) {
    this.errorStroke = stroke;
    fireChangeEvent();
  }

  /**
   * Returns the range required by this renderer to display all the domain
   * values in the specified dataset.
   *
   * @param dataset  the dataset (<code>null</code> permitted).
   *
   * @return The range, or <code>null</code> if the dataset is
   *     <code>null</code>.
   */
  @Override
  public Range findDomainBounds(final XYDataset dataset) {
    if (dataset != null) {
      return DatasetUtilities.findDomainBounds(dataset, true);
    } else {
      return null;
    }
  }

  /**
   * Returns the range required by this renderer to display all the range
   * values in the specified dataset.
   *
   * @param dataset  the dataset (<code>null</code> permitted).
   *
   * @return The range, or <code>null</code> if the dataset is
   *     <code>null</code>.
   */
  @Override
  public Range findRangeBounds(final XYDataset dataset) {
    if (dataset != null) {
      return DatasetUtilities.findRangeBounds(dataset, true);
    } else {
      return null;
    }
  }

  /**
   * Draws the visual representation for one data item.
   *
   * @param g2  the graphics output target.
   * @param state  the renderer state.
   * @param dataArea  the data area.
   * @param info  the plot rendering info.
   * @param plot  the plot.
   * @param domainAxis  the domain axis.
   * @param rangeAxis  the range axis.
   * @param dataset  the dataset.
   * @param series  the series index.
   * @param item  the item index.
   * @param crosshairState  the crosshair state.
   * @param pass  the pass index.
   */
  @Override
  public void drawItem(final Graphics2D g2, final XYItemRendererState state,
          final Rectangle2D dataArea, final PlotRenderingInfo info, final XYPlot plot,
          final ValueAxis domainAxis, final ValueAxis rangeAxis, final XYDataset dataset,
          final int series, final int item, final CrosshairState crosshairState, final int pass) {

    if (isLinePass(pass) && dataset instanceof IntervalXYDataset && getItemVisible(series, item)) {
      final IntervalXYDataset ixyd = (IntervalXYDataset) dataset;

      final PlotOrientation orientation = plot.getOrientation();
      RectangleEdge xAxisLocation = plot.getDomainAxisEdge();
      RectangleEdge yAxisLocation = plot.getRangeAxisEdge();

      final Paint paint = this.getErrorPaint();
      final Stroke stroke = this.getErrorStroke();
      final double adj = (this.useCap) ? 0.5d * this.getCapLength() : 0d;

      if (this.getDrawXError()) {
        // draw the error bar for the x-interval
        final double x0 = ixyd.getStartXValue(series, item);
        final double x1 = ixyd.getEndXValue(series, item);
        final double y = ixyd.getYValue(series, item);

        if (!Double.isNaN(x0) && !Double.isNaN(x1) && !Double.isNaN(y)) {
          final double xx0 = domainAxis.valueToJava2D(x0, dataArea, xAxisLocation);
          final double xx1 = domainAxis.valueToJava2D(x1, dataArea, xAxisLocation);
          final double yy = rangeAxis.valueToJava2D(y, dataArea, yAxisLocation);

          final Line2D line;
          Line2D cap1 = null;
          Line2D cap2 = null;
          if (orientation == PlotOrientation.VERTICAL) {
            line = new Line2D.Double(xx0, yy, xx1, yy);

            if (this.useCap) {
              cap1 = new Line2D.Double(xx0, yy - adj, xx0, yy + adj);
              cap2 = new Line2D.Double(xx1, yy - adj, xx1, yy + adj);
            }
          } else {  // PlotOrientation.HORIZONTAL
            line = new Line2D.Double(yy, xx0, yy, xx1);

            if (this.useCap) {
              cap1 = new Line2D.Double(yy - adj, xx0, yy + adj, xx0);
              cap2 = new Line2D.Double(yy - adj, xx1, yy + adj, xx1);
            }
          }
          if (paint != null) {
            g2.setPaint(paint);
          } else {
            g2.setPaint(getItemPaint(series, item));
          }
          if (stroke != null) {
            g2.setStroke(stroke);
          } else {
            g2.setStroke(getItemStroke(series, item));
          }
          g2.draw(line);

          if (this.useCap) {
            g2.draw(cap1);
            g2.draw(cap2);
          }
        }
      }
      if (this.getDrawYError()) {
        // draw the error bar for the y-interval
        final double y0 = ixyd.getStartYValue(series, item);
        final double y1 = ixyd.getEndYValue(series, item);
        final double x = ixyd.getXValue(series, item);

        if (!Double.isNaN(y0) && !Double.isNaN(y1) && !Double.isNaN(x)) {
          final double yy0 = rangeAxis.valueToJava2D(y0, dataArea, yAxisLocation);
          final double yy1 = rangeAxis.valueToJava2D(y1, dataArea, yAxisLocation);
          final double xx = domainAxis.valueToJava2D(x, dataArea, xAxisLocation);

          final Line2D line;
          Line2D cap1 = null;
          Line2D cap2 = null;
          if (orientation == PlotOrientation.VERTICAL) {
            line = new Line2D.Double(xx, yy0, xx, yy1);

            if (this.useCap) {
              cap1 = new Line2D.Double(xx - adj, yy0, xx + adj, yy0);
              cap2 = new Line2D.Double(xx - adj, yy1, xx + adj, yy1);
            }
          } else {  // PlotOrientation.HORIZONTAL
            line = new Line2D.Double(yy0, xx, yy1, xx);

            if (this.useCap) {
              cap1 = new Line2D.Double(yy0, xx - adj, yy0, xx + adj);
              cap2 = new Line2D.Double(yy1, xx - adj, yy1, xx + adj);
            }
          }
          if (paint != null) {
            g2.setPaint(paint);
          } else {
            g2.setPaint(getItemPaint(series, item));
          }
          if (stroke != null) {
            g2.setStroke(stroke);
          } else {
            g2.setStroke(getItemStroke(series, item));
          }
          g2.draw(line);

          if (this.useCap) {
            g2.draw(cap1);
            g2.draw(cap2);
          }
        }
      }
    }
    super.drawItem(g2, state, dataArea, info, plot, domainAxis, rangeAxis, dataset, series, item, crosshairState, pass);
  }

  /**
   * Returns a boolean that indicates whether or not the specified item
   * should be drawn (this is typically used to hide an entire series).
   *
   * @param series  the series index.
   * @param item  the item index.
   *
   * @return A boolean.
   */
  @Override
  public boolean getItemVisible(final int series, final int item) {
    return true;
  }

  /**
   * Returns a boolean that indicates whether or not the specified series
   * should be drawn.
   *
   * @param series  the series index.
   *
   * @return A boolean.
   */
  @Override
  public boolean isSeriesVisible(final int series) {
    return true;
  }

  /**
   * Tests this instance for equality with an arbitrary object.
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
    if (!(obj instanceof FastXYErrorRenderer)) {
      return false;
    }
    FastXYErrorRenderer that = (FastXYErrorRenderer) obj;
    if (this.drawXError != that.drawXError) {
      return false;
    }
    if (this.drawYError != that.drawYError) {
      return false;
    }
    if (this.capLength != that.capLength) {
      return false;
    }
    if (!PaintUtilities.equal(this.errorPaint, that.errorPaint)) {
      return false;
    }
    if (!ObjectUtilities.equal(this.errorStroke, that.errorStroke)) {
      return false;
    }
    return super.equals(obj);
  }

  /**
   * Provides serialization support.
   *
   * @param stream  the input stream.
   *
   * @throws IOException  if there is an I/O error.
   * @throws ClassNotFoundException  if there is a classpath problem.
   */
  private void readObject(final ObjectInputStream stream) throws IOException, ClassNotFoundException {
    stream.defaultReadObject();
    this.errorPaint = SerialUtilities.readPaint(stream);
    this.errorStroke = SerialUtilities.readStroke(stream);
  }

  /**
   * Provides serialization support.
   *
   * @param stream  the output stream.
   *
   * @throws IOException  if there is an I/O error.
   */
  private void writeObject(final ObjectOutputStream stream) throws IOException {
    stream.defaultWriteObject();
    SerialUtilities.writePaint(this.errorPaint, stream);
    SerialUtilities.writeStroke(this.errorStroke, stream);
  }
}
