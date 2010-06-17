/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: BoundedNumberAxis.java,v 1.5 2010-06-17 10:02:50 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.4  2010/05/11 12:02:24  bourgesl
 * disable autoRange at all
 *
 * Revision 1.3  2010/02/12 15:53:25  bourgesl
 * comments
 *
 * Revision 1.2  2010/01/13 16:12:08  bourgesl
 * comments
 *
 * Revision 1.1  2010/01/12 16:53:20  bourgesl
 * customized JFreeChart classes to get a square XY Plot supporting zooming in/out with mouse and mouse wheel
 *
 */
package fr.jmmc.aspro.gui.chart;

import org.jfree.chart.axis.NumberAxis;
import org.jfree.data.Range;
import org.jfree.ui.RectangleInsets;

/**
 * This customized number axis used bounds to limits its expansion (zoom out).
 *
 * Note : this class must support the inherited cloneable interface.
 *
 * @author bourgesl
 */
public final class BoundedNumberAxis extends NumberAxis {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.chart.BoundedNumberAxis";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** default tick rectangle insets */
  public final static RectangleInsets TICK_LABEL_INSETS = new RectangleInsets(1.0, 1.0, 1.0, 1.0);

  /* members */
  /** axis bounds */
  private Range bounds = null;

  /**
   * Constructs a number axis, using default values where necessary.
   *
   * Changes the default tick label insets
   *
   * @param label  the axis label (<code>null</code> permitted).
   */
  public BoundedNumberAxis(String label) {
    super(label);
    setAutoRange(false, false);
    setTickLabelInsets(TICK_LABEL_INSETS);
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
    BoundedNumberAxis clone = (BoundedNumberAxis) super.clone();
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
   * Method overriden to make it public
   *
   * Sets the auto range attribute.
   * @param auto  the flag.
   * @param notify  notify listeners?
   */
  @Override
  public void setAutoRange(boolean auto, boolean notify) {
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
