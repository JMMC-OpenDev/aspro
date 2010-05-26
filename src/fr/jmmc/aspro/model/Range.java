/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: Range.java,v 1.9 2010-05-26 15:27:15 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.8  2010/04/02 14:39:54  bourgesl
 * added contains method
 *
 * Revision 1.7  2010/01/21 16:39:49  bourgesl
 * added static getMinimum(List) and getMaximum(List) methods
 *
 * Revision 1.6  2009/12/02 17:23:51  bourgesl
 * fixed several bugs on pop finder + refactoring
 *
 * Revision 1.5  2009/12/01 17:14:45  bourgesl
 * first try to add the pop configuration finder
 *
 * Revision 1.4  2009/11/24 17:27:12  bourgesl
 * first attempt to merge ranges
 *
 * Revision 1.3  2009/11/24 15:12:09  bourgesl
 * first step to handle delay line limits
 *
 * Revision 1.2  2009/11/23 16:49:17  bourgesl
 * added horizonService to check horizon profiles (VLTI)
 *
 * Revision 1.1  2009/11/20 16:55:47  bourgesl
 * Added Beam / Delay Line definition
 * ObservabilityService is stateless to simplify coding
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A simple range of double values
 * @author bourgesl
 */
public class Range {

  private double min = 0d;
  private double max = 0d;

  public Range() {
    /* no-op */
  }

  public Range(final double min, final double max) {
    this.min = min;
    this.max = max;
  }

  public double getMin() {
    return min;
  }

  public void setMin(double min) {
    this.min = min;
  }

  public double getMax() {
    return max;
  }

  public void setMax(double max) {
    this.max = max;
  }

  public double getLength() {
    return this.max - this.min;
  }

  public boolean contains(final double value) {
    return value >= this.min && value <= this.max;
  }

  @Override
  public String toString() {
    return "[" + this.min + ", " + this.max + "]";
  }

  /* utility methods */

  public static Double getMinimum(final List<Range> ranges) {
    if (ranges != null && !ranges.isEmpty()) {
      double min = Double.MAX_VALUE;
      for (Range range : ranges) {
        if (min > range.getMin()) {
          min = range.getMin();
        }
      }
      return min;
    }
    return null;
  }

  public static Double getMaximum(final List<Range> ranges) {
    if (ranges != null && !ranges.isEmpty()) {
      double max = Double.MIN_VALUE;
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
   * Merge a list of overlapping ranges according to the nValid parameter that indicates how many times a point must be inside a range
   * to consider the point as valid
   * @param ranges list of ranges to merge
   * @param nValid number of ranges to consider a point is valid
   * @return new list of ranges
   */
  public static List<Range> mergeRanges(final List<Range> ranges, final int nValid) {
    return mergeRanges(ranges, nValid, null);
  }

  /**
   * Merge a list of overlapping ranges according to the nValid parameter that indicates how many times a point must be inside a range
   * to consider the point as valid
   * @param ranges list of ranges to merge
   * @param nValid number of ranges to consider a point is valid
   * @param results output list of ranges
   * @return new list of ranges or null
   */
  public static List<Range> mergeRanges(final List<Range> ranges, final int nValid, final List<Range> results) {
    // table of start/end time :
    final List<RangeLimit> limits = new ArrayList<RangeLimit>(ranges.size() * 2);

    for (Range range : ranges) {
      limits.add(new RangeLimit(range.getMin(), 1));
      limits.add(new RangeLimit(range.getMax(), -1));
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
      s += limit.getFlag();

      if (s == nValid) {
        if (mRanges == null) {
          // lazy instanciation :
          mRanges = new ArrayList<Range>();
        }
        mRanges.add(new Range(limit.getPosition(), limits.get(i+1).getPosition()));
      }
    }

    return mRanges;
  }

  private static final class RangeLimit implements Comparable<RangeLimit> {
    /** limit position */
    private final double position;
    /** integer value to indicate the start [+1] or end of the initial range */
    private final int flag;

    protected RangeLimit(final double position, final int flag) {
      this.position = position;
      this.flag = flag;
    }

    public int getFlag() {
      return flag;
    }

    public double getPosition() {
      return position;
    }

    public int compareTo(final RangeLimit limit) {
      return Double.compare(this.position, limit.position);
    }
    
  }
}
