/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: BoundedDateAxis.java,v 1.1 2011-04-13 14:33:45 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.chart;

import org.jfree.chart.axis.DateAxis;
import org.jfree.data.Range;

/**
 * This customized date axis used bounds to limits its expansion (zoom out).
 *
 * Note : this class must support the inherited cloneable interface.
 *
 * @author bourgesl
 */
public class BoundedDateAxis extends DateAxis {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          BoundedDateAxis.class.getName());
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
    BoundedDateAxis clone = (BoundedDateAxis) super.clone();
    return clone;
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
  public void setRange(Range range, final boolean turnOffAutoRange,
                       final boolean notify) {

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
          range = bounds;
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
            range = bounds;
          } else {
            range = new Range(min, max);
          }
        }
      }
    }

    super.setRange(range, turnOffAutoRange, notify);
  }
}
