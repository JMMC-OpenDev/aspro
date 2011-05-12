/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A simple range of double values
 * @author bourgesl
 */
public final class Range {

  /** minimum value */
  private double min;
  /** maximum value */
  private double max;

  /**
   * Constructor
   */
  public Range() {
    this.min = 0d;
    this.max = 0d;
  }

  /**
   * Constructor with given minimum and maximum value
   * @param min minimum value
   * @param max maximum value
   */
  public Range(final double min, final double max) {
    this.min = min;
    this.max = max;
  }

  /**
   * Return the minimum value
   * @return minimum value
   */
  public double getMin() {
    return min;
  }

  /**
   * Set the minimum value
   * @param min minimum value
   */
  public void setMin(final double min) {
    this.min = min;
  }

  /**
   * Return the maximum value
   * @return maximum value
   */
  public double getMax() {
    return max;
  }

  /**
   * Set the maximum value
   * @param max maximum value
   */
  public void setMax(final double max) {
    this.max = max;
  }

  /**
   * Return the length (max - min)
   * @return length
   */
  public double getLength() {
    return this.max - this.min;
  }

  /**
   * Return true if this range contains the given value (including bounds)
   * @param value value to check
   * @return true if this range contains the given value
   */
  public boolean contains(final double value) {
    return value >= this.min && value <= this.max;
  }

  /**
   * Return a string representation of this range
   * @return string representation of this range
   */
  @Override
  public String toString() {
    return "[" + this.min + ", " + this.max + "]";
  }

  /* --- Utility methods ---------------------------------------------------- */
  /**
   * Test if the given value is inside the given list of ranges
   * @param ranges list of ranges
   * @param value value to test
   * @return true if the given value is inside given ranges
   */
  public static boolean contains(final List<Range> ranges, final double value) {
    return find(ranges, value) != null;
  }

  /**
   * Find the range containing the given value
   * @param ranges list of ranges
   * @param value value to test
   * @return range containing the given value or null
   */
  public static Range find(final List<Range> ranges, final double value) {
    for (Range range : ranges) {
      if (range.contains(value)) {
        return range;
      }
    }
    return null;
  }

  /**
   * Find the minimum value of the given list of ranges
   * @param ranges list of ranges
   * @return minimum value of the given list of ranges
   */
  public static Double getMinimum(final List<Range> ranges) {
    if (ranges != null && !ranges.isEmpty()) {
      double min = Double.POSITIVE_INFINITY;
      for (Range range : ranges) {
        if (min > range.getMin()) {
          min = range.getMin();
        }
      }
      return min;
    }
    return null;
  }

  /**
   * Find the maximum value of the given list of ranges
   * @param ranges list of ranges
   * @return maximum value of the given list of ranges
   */
  public static Double getMaximum(final List<Range> ranges) {
    if (ranges != null && !ranges.isEmpty()) {
      double max = Double.NEGATIVE_INFINITY;
      for (Range range : ranges) {
        if (max < range.getMax()) {
          max = range.getMax();
        }
      }
      return max;
    }
    return null;
  }

  /**
   * Return the list of ranges cropped to stay inside range [min;max]
   * @param ranges list of ranges to use
   * @param min lower bound
   * @param max upper bound
   * @return list of ranges inside range [min;max]
   */
  public static List<Range> restrictRange(final List<Range> ranges, final double min, final double max) {
    final List<Range> intervals = new ArrayList<Range>(ranges.size());

    double start, end;
    for (Range range : ranges) {
      start = range.getMin();
      end = range.getMax();

      if (start >= min) {
        if (end <= max) {
          // interval in inside [min;max]
          intervals.add(range);
        } else {
          if (start > max) {
            // two points over max : skip
          } else {
            // end occurs after max :
            intervals.add(new Range(start, max));
          }
        }
      } else {
        // start occurs before min :
        if (end < min) {
          // two points before min : skip
        } else if (end > max) {
          // two points overlapping [min;max] : keep
          intervals.add(new Range(min, max));
        } else {
          intervals.add(new Range(min, end));
        }
      }
    }
    return intervals;
  }

  /**
   * Traverse the given list of date intervals to merge contiguous intervals.
   *
   * @param ranges SORTED ranges to fix
   */
  public static void union(final List<Range> ranges) {
    int size = ranges.size();
    if (size > 1) {
      Range range1, range2;
      for (int i = size - 2, j = size - 1; i >= 0; i--, j--) {
        range1 = ranges.get(i);
        range2 = ranges.get(j);

        if (range1.getMax() >= range2.getMin()) {
          // merge interval :
          range1.setMax(range2.getMax());
          // remove interval2 :
          ranges.remove(j);
        }
      }
    }
  }

  /**
   * Intersect overlapping ranges according to the nValid parameter that indicates how many times a point must be inside a range
   * to consider the point as valid
   * @param ranges list of ranges to merge
   * @param nValid number of ranges to consider a point is valid
   * @return new list of ranges
   */
  public static List<Range> intersectRanges(final List<Range> ranges, final int nValid) {
    return intersectRanges(ranges, nValid, null);
  }

  /**
   * Intersect overlapping ranges according to the nValid parameter that indicates how many times a point must be inside a range
   * to consider the point as valid
   * @param ranges list of ranges to merge
   * @param nValid number of ranges to consider a point is valid
   * @param results output list of ranges
   * @return new list of ranges or null
   */
  public static List<Range> intersectRanges(final List<Range> ranges, final int nValid, final List<Range> results) {
    // table of start/end time :
    final List<RangeLimit> limits = new ArrayList<RangeLimit>(ranges.size() * 2);

    for (Range range : ranges) {
      limits.add(new RangeLimit(range.getMin(), true));
      limits.add(new RangeLimit(range.getMax(), false));
    }

    // sort the array by increasing time :
    Collections.sort(limits);

    //  Explore range. When the running sum of flag is equal to the
    //  number nValid, we are in a valid range

    List<Range> mRanges = results;

    int s = 0;
    RangeLimit limit;
    for (int i = 0, size = limits.size(); i < size; i++) {
      limit = limits.get(i);

      // sum of flags :
      if (limit.isFlag()) {
        s++;
      } else {
        s--;
      }

      if (s == nValid) {
        if (mRanges == null) {
          // lazy instanciation :
          mRanges = new ArrayList<Range>();
        }
        mRanges.add(new Range(limit.getPosition(), limits.get(i + 1).getPosition()));
      }
    }

    return mRanges;
  }

  /**
   * Utility class used by intersection and merge algorithms
   */
  private static final class RangeLimit implements Comparable<RangeLimit> {

    /** position of the limit */
    private final double position;
    /** boolean value to indicate the start [true] or end of the initial range [false] */
    private final boolean flag;

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
     * Return the flag indicating a starting [true] or ending [false] range
     * @return flag indicating a starting [true] or ending [false] range
     */
    public boolean isFlag() {
      return flag;
    }

    /**
     * Return the position of the limit
     * @return position of the limit
     */
    public double getPosition() {
      return position;
    }

    public int compareTo(final RangeLimit limit) {
      return Double.compare(this.position, limit.position);
    }
  }
}
