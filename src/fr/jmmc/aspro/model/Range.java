/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.jmcs.util.ObjectUtils;
import java.util.ArrayList;
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
    this(0d, 0d);
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
   * Update with given minimum and maximum value
   * @param min minimum value
   * @param max maximum value
   */
  public void set(final double min, final double max) {
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
   * Return the center value
   * @return center value
   */
  public double getCenter() {
    return 0.5d * (this.min + this.max);
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
   * Returns true if this range equals the given range
   * @param obj another object instance
   * @return true if this range equals the given range
   */
  @Override
  public boolean equals(final Object obj) {
    if (obj == null) {
      return false;
    }
    // identity check:
    if (this == obj) {
      return true;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final Range other = (Range) obj;
    if (this.min != other.getMin()) {
      return false;
    }
    if (this.max != other.getMax()) {
      return false;
    }
    return true;
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
   * Return true if both range list are equals
   * @param ranges list of ranges
   * @param otherRanges other list of ranges
   * @return true if both range list are equals
   */
  public static boolean equals(final List<Range> ranges, final List<Range> otherRanges) {
    if (ranges == otherRanges) {
      // identity or both nulls:
      return true;
    }
    if ((ranges == null && otherRanges != null) || (ranges != null && otherRanges == null)) {
      return false;
    }
    // not nulls:
    final int len = ranges.size();
    if (len != otherRanges.size()) {
      return false;
    }
    for (int i = 0; i < len; i++) {
      final Range range = ranges.get(i);
      final Range otherRange = otherRanges.get(i);
      if (!ObjectUtils.areEquals(range, otherRange)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Test if the given value is inside the given list of ranges
   * @param ranges list of ranges
   * @param value value to test
   * @return true if the given value is inside given ranges
   */
  public static boolean contains(final List<Range> ranges, final double value) {
    if (ranges == null) {
      return false;
    }
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
   * @return list of ranges inside range [min;max] or null
   */
  public static List<Range> restrictRange(final List<Range> ranges, final double min, final double max) {
    List<Range> intervals = null;

    Range rangeToAdd;
    double start, end;
    for (Range range : ranges) {
      start = range.getMin();
      end = range.getMax();

      rangeToAdd = null;

      if (start >= min) {
        if (end <= max) {
          // interval in inside [min;max]
          rangeToAdd = range;
        } else {
          if (start > max) {
            // two points over max : skip
          } else {
            // end occurs after max :
            rangeToAdd = new Range(start, max);
          }
        }
      } else {
        // start occurs before min :
        if (end < min) {
          // two points before min : skip
        } else if (end > max) {
          // two points overlapping [min;max] : keep
          rangeToAdd = new Range(min, max);
        } else {
          rangeToAdd = new Range(min, end);
        }
      }

      if (rangeToAdd != null) {
        if (intervals == null) {
          intervals = new ArrayList<Range>(ranges.size());
        }
        intervals.add(rangeToAdd);
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
   * @param rangeFactory Factory used to create Range instances
   * @return new list of ranges
   */
  public static List<Range> intersectRanges(final List<Range> ranges, final int nValid, final RangeFactory rangeFactory) {
    return intersectRanges(ranges, nValid, null, rangeFactory);
  }

  /**
   * Intersect overlapping ranges according to the nValid parameter that indicates how many times a point must be inside a range
   * to consider the point as valid
   * @param ranges list of ranges to merge
   * @param nValid number of ranges to consider a point is valid
   * @param results output list of ranges
   * @param rangeFactory Factory used to create Range instances
   * @return new list of ranges or null
   */
  public static List<Range> intersectRanges(final List<Range> ranges, final int nValid,
          final List<Range> results, final RangeFactory rangeFactory) {
    final int len = ranges.size() * 2;

    // table of start/end time :
    final RangeLimit[] limits = new RangeLimit[len];

    int n = 0;
    for (Range range : ranges) {
      limits[n++] = new RangeLimit(range.getMin(), 1);
      limits[n++] = new RangeLimit(range.getMax(), -1);
    }

    return intersectRanges(limits, n, nValid, results, rangeFactory);
  }

  /**
   * Intersect overlapping ranges according to the nValid parameter that indicates how many times a point must be inside a range
   * to consider the point as valid
   * @param limits array of range limits to intersect
   * @param nLimits number of range limits present in the given array
   * @param nValid number of ranges to consider a point is valid
   * @param results output list of ranges
   * @param rangeFactory Factory used to create Range instances
   * @return new list of ranges or null
   */
  public static List<Range> intersectRanges(final RangeLimit[] limits, final int nLimits, final int nValid,
          final List<Range> results, final RangeFactory rangeFactory) {

    // use customized binary sort to be in-place (no memory usage):
    // inspired from Arrays.sort but do not use tim sort as it makes array copies:
    binarySort(limits, 0, nLimits, countRunAndMakeAscending(limits, 0, nLimits));

    //  Explore range: when the running sum of flag is equal to the
    //  number nValid, we are in a valid range

    List<Range> mRanges = results;

    for (int i = 0, s = 0, len = nLimits - nValid; i < len; i++) {

      // sum of flags :
      s += limits[i].flag;

      if (s == nValid) {
        if (mRanges == null) {
          // lazy instanciation (statically 1 range only) :
          mRanges = new ArrayList<Range>(1);
        }
        mRanges.add(rangeFactory.valueOf(limits[i].position, limits[i + 1].position));
      }
    }

    return mRanges;
  }

  // --- Customized BinarySort from Arrays.sort() -----------------------------
  /**
   * Sorts the specified portion of the specified array using a binary
   * insertion sort.  This is the best method for sorting small numbers
   * of elements.  It requires O(n log n) compares, but O(n^2) data
   * movement (worst case).
   *
   * If the initial part of the specified range is already sorted,
   * this method can take advantage of it: the method assumes that the
   * elements from index {@code lo}, inclusive, to {@code start},
   * exclusive are already sorted.
   *
   * @param a the array in which a range is to be sorted
   * @param lo the index of the first element in the range to be sorted
   * @param hi the index after the last element in the range to be sorted
   * @param start the index of the first element in the range that is
   *        not already known to be sorted ({@code lo <= start <= hi})
   */
  @SuppressWarnings("unchecked")
  private static void binarySort(final RangeLimit[] a, final int lo, final int hi, int start) {
    if (start == lo) {
      start++;
    }
    int left, right, mid, n;
    RangeLimit pivot;

    for (; start < hi; start++) {
      pivot = a[start];

      // Set left (and right) to the index where a[start] (pivot) belongs
      left = lo;
      right = start;
      /*
       * Invariants:
       *   pivot >= all in [lo, left).
       *   pivot <  all in [right, start).
       */
      while (left < right) {
        mid = (left + right) >>> 1;
        if (pivot.compareTo(a[mid]) < 0) {
          right = mid;
        } else {
          left = mid + 1;
        }
      }

      /*
       * The invariants still hold: pivot >= all in [lo, left) and
       * pivot < all in [left, start), so pivot belongs at left.  Note
       * that if there are elements equal to pivot, left points to the       * first slot after them -- that's why this sort is stable.
       * Slide elements over to make room for pivot.
       */
      n = start - left;  // The number of elements to move
      // Switch is just an optimization for arraycopy in default case
      switch (n) {
        case 2:
          a[left + 2] = a[left + 1];
        case 1:
          a[left + 1] = a[left];
          break;
        default:
          System.arraycopy(a, left, a, left + 1, n);
      }
      a[left] = pivot;
    }
  }

  /**
   * Returns the length of the run beginning at the specified position in
   * the specified array and reverses the run if it is descending (ensuring
   * that the run will always be ascending when the method returns).
   *
   * A run is the longest ascending sequence with:
   *
   *    a[lo] <= a[lo + 1] <= a[lo + 2] <= ...
   *
   * or the longest descending sequence with:
   *
   *    a[lo] >  a[lo + 1] >  a[lo + 2] >  ...
   *
   * For its intended use in a stable mergesort, the strictness of the
   * definition of "descending" is needed so that the call can safely
   * reverse a descending sequence without violating stability.
   *
   * @param a the array in which a run is to be counted and possibly reversed
   * @param lo index of the first element in the run
   * @param hi index after the last element that may be contained in the run.
   * It is required that {@code lo < hi}.
   * @return  the length of the run beginning at the specified position in
   *          the specified array
   */
  @SuppressWarnings("unchecked")
  private static int countRunAndMakeAscending(final RangeLimit[] a, final int lo, final int hi) {
    int runHi = lo + 1;
    if (runHi == hi) {
      return 1;
    }

    // Find end of run, and reverse range if descending
    if (a[runHi++].compareTo(a[lo]) < 0) {
      // Descending
      while (runHi < hi && a[runHi].compareTo(a[runHi - 1]) < 0) {
        runHi++;
      }
      reverseRange(a, lo, runHi);
    } else {
      // Ascending
      while (runHi < hi && a[runHi].compareTo(a[runHi - 1]) >= 0) {
        runHi++;
      }
    }

    return runHi - lo;
  }

  /**
   * Reverse the specified range of the specified array.
   *
   * @param a the array in which a range is to be reversed
   * @param lo the index of the first element in the range to be reversed
   * @param hi the index after the last element in the range to be reversed
   */
  @SuppressWarnings("unchecked")
  private static void reverseRange(final RangeLimit[] a, int lo, int hi) {
    hi--;
    RangeLimit t;
    while (lo < hi) {
      t = a[lo];
      a[lo++] = a[hi];
      a[hi--] = t;
    }
  }
}
