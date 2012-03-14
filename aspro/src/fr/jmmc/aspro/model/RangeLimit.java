/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

/**
 * Utility class used by intersection and merge algorithms
 * @author bourgesl
 */
public final class RangeLimit implements Comparable<RangeLimit> {

  /** position of the limit */
  double position;
  /** boolean value to indicate the start [true] or end of the initial range [false] */
  boolean flag;

  /**
   * Create a RangeLimit array filled with empty RangeLimit instances
   * @param length array length
   * @return RangeLimit array filled with empty RangeLimit instances
   */
  public static RangeLimit[] createArray(final int length) {
    final RangeLimit[] array = new RangeLimit[length];
    for (int i = 0; i < length; i++) {
      array[i] = new RangeLimit();
    }
    return array;
  }

  /**
   * Empty Constructor
   */
  private RangeLimit() {
  }

  /**
   * Constructor with given position and flag
   * @param position position of the limit
   * @param flag flag indicating a starting [true] or ending [false] range
   */
  protected RangeLimit(final double position, final boolean flag) {
    this.position = position;
    this.flag = flag;
  }

  /**
   * set the given position and flag from the given range limit
   * @param source range limit to copy
   */
  public void set(final RangeLimit source) {
    this.position = source.position;
    this.flag = source.flag;
  }

  /**
   * set the given position and flag
   * @param position position of the limit
   * @param flag flag indicating a starting [true] or ending [false] range
   */
  public void set(final double position, final boolean flag) {
    this.position = position;
    this.flag = flag;
  }

  /**
   * Compare this instance with another rangeLimit instance = compare positions
   * 
   * @param other another RangeLimit instance
   * @return Double.compare(this.position, o.position)
   */
  public int compareTo(final RangeLimit other) {
    return Double.compare(this.position, other.position);
  }
}
