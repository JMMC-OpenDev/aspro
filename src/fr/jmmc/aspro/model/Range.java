/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: Range.java,v 1.5 2009-12-01 17:14:45 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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

  @Override
  public String toString() {
    return "[" + this.min + ", " + this.max + "]";
  }

  /**
   * Merge a list of overlapping ranges according to the nValid parameter that indicates how many times a point must be inside a range
   * to consider the point as valid
   * @param ranges list of ranges to merge
   * @param nValid number of ranges to consider a point is valid
   * @return new list of ranges
   */
  public static List<Range> mergeRanges(final List<Range> ranges, final int nValid) {
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

    final List<Range> mRanges = new ArrayList<Range>();

    int s = 0;
    RangeLimit limit;
    for (int i = 0, size = limits.size(); i < size; i++) {
      limit = limits.get(i);

      // sum of flags :
      s += limit.getFlag();

      if (s == nValid) {
        mRanges.add(new Range(limit.getPosition(), limits.get(i+1).getPosition()));
      }
    }

    return mRanges;
  }

  private static class RangeLimit implements Comparable<RangeLimit> {
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
