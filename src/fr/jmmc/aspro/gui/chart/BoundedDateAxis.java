/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import org.jfree.chart.axis.DateAxis;
import org.jfree.chart.event.AxisChangeEvent;
import org.jfree.data.Range;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This customized date axis uses bounds to limits its expansion (zoom out).
 *
 * Note : this class must support the inherited cloneable interface.
 *
 * @author bourgesl
 */
public final class BoundedDateAxis extends DateAxis {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(BoundedDateAxis.class.getName());

  /* members */
  /** axis bounds */
  private Range bounds = null;

  /**
   * Constructs a number axis, using default values where necessary.
   *
   * @param label  the axis label (<code>null</code> permitted).
   */
  public BoundedDateAxis(final String label) {
    super(label);
    setAutoRange(false, false);
    setTickLabelInsets(ChartUtils.TICK_LABEL_INSETS);
  }

  /**
   * Returns a clone of the object.
   *
   * @return A clone.
   *
   * @throws CloneNotSupportedException if some component of the axis does
   *         not support cloning.
   */
  @Override
  public Object clone() throws CloneNotSupportedException {
    return (BoundedDateAxis) super.clone();
  }

  /**
   * Return the axis bounds
   * @return axis bounds or null if undefined
   */
  public Range getBounds() {
    return this.bounds;
  }

  /**
   * Define the axis bounds
   * @param bounds axis bounds or null
   */
  public void setBounds(final Range bounds) {
    this.bounds = bounds;
  }

  /**
   * Sets the auto range attribute.  If the <code>notify</code> flag is set,
   * an {@link AxisChangeEvent} is sent to registered listeners.
   *
   * HACK : log a message and setRange(bounds) to reset the zoom
   *
   * @param auto  the flag.
   * @param notify  notify listeners?
   *
   * @see #isAutoRange()
   */
  @Override
  protected void setAutoRange(final boolean auto, final boolean notify) {
    if (auto) {
      // autoRange must stay disabled.

      // This is called by JFreeChart to reset the zoom:
      /*
       at fr.jmmc.aspro.gui.chart.BoundedDateAxis.setAutoRange(BoundedDateAxis.java:93)
       at org.jfree.chart.axis.ValueAxis.setAutoRange(ValueAxis.java:975)
       at org.jfree.chart.axis.ValueAxis.resizeRange(ValueAxis.java:1563)
       at org.jfree.chart.axis.ValueAxis.resizeRange(ValueAxis.java:1539)
       at org.jfree.chart.plot.XYPlot.zoomRangeAxes(XYPlot.java:5158)
       at org.jfree.chart.plot.XYPlot.zoomRangeAxes(XYPlot.java:5123)
       at org.jfree.chart.ChartPanel.restoreAutoRangeBounds(ChartPanel.java:2430)
       at org.jfree.chart.ChartPanel.restoreAutoBounds(ChartPanel.java:2391)
       at org.jfree.chart.ChartPanel.mouseReleased(ChartPanel.java:2044)
       */

      // Use the axis bounds to redefine the ranges (reset zoom)
      setRange(this.bounds, false, notify);

      return;
    }

    super.setAutoRange(auto, notify);
  }

  /**
   * HACK : check the defined bounds to avoid excessive zoom out
   *
   * Sets the range for the axis, if requested, sends an
   * {@link AxisChangeEvent} to all registered listeners.  As a side-effect,
   * the auto-range flag is set to <code>false</code> (optional).
   *
   * @param range  the range (<code>null</code> not permitted).
   * @param turnOffAutoRange  a flag that controls whether or not the auto
   *                          range is turned off.
   * @param notify  a flag that controls whether or not listeners are
   *                notified.
   */
  @Override
  public void setRange(final Range range, final boolean turnOffAutoRange,
          final boolean notify) {

    Range newRange = range;

    // check if range is within bounds :

    if (this.bounds != null) {
      final double lower = this.bounds.getLowerBound();
      final double upper = this.bounds.getUpperBound();

      // check bounds :
      double min = range.getLowerBound();
      double max = range.getUpperBound();

      if (min < lower || max > upper) {
        // range is outside bounds :

        if (min < lower && max > upper) {
          newRange = bounds;
        } else {
          // try to relocate the range inside bounds
          // to keep its length so aspect ratio constant :

          final double len = range.getLength();

          if (min < lower) {
            min = lower;
            // keep range length constant to keep aspect ratio :
            max = min + len;

          } else if (max > upper) {
            max = upper;
            // keep range length constant to keep aspect ratio :
            min = max - len;
          }

          if (min < lower || max > upper) {
            // again, range is outside bounds :
            newRange = bounds;
          } else {
            newRange = new Range(min, max);
          }
        }
      }
    }

    if (!getRange().equals(newRange)) {
      super.setRange(newRange, turnOffAutoRange, notify);
    }
  }
}
