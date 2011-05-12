/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: DateTimeInterval.java,v 1.3 2011-01-25 13:48:56 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2010/04/13 15:26:21  bourgesl
 * add Comparable support
 *
 * Revision 1.1  2010/01/08 16:48:30  bourgesl
 * package refactoring
 *
 * Revision 1.2  2009/12/18 11:52:02  bourgesl
 * added Pops Finder Compatible Mode for a list of targets
 *
 ******************************************************************************/
package fr.jmmc.aspro.model.observability;

import java.util.Collections;
import java.util.Date;
import java.util.List;

/**
 * This class contains a simple date/time interval
 * @author bourgesl
 */
public class DateTimeInterval implements Comparable<DateTimeInterval> {

  /** starting date/time */
  private final Date startDate;
  /** ending date/time */
  private Date endDate;

  /**
   * Public constructor
   * @param startDate starting date/time
   * @param endDate ending date/time
   */
  public DateTimeInterval(final Date startDate, final Date endDate) {
    this.startDate = startDate;
    this.endDate = endDate;
  }

  /**
   * Simple comparator that takes into account the starting date and finally the ending date if start dates are equals
   * @param other date interval
   * @return Double.compare(startDate1, startDate2)
   */
  public final int compareTo(final DateTimeInterval other) {
    int diff = this.startDate.compareTo(other.getStartDate());
    if (diff == 0) {
      diff = this.endDate.compareTo(other.getEndDate());
    }
    return diff;
  }

  /**
   * Return the starting date/time
   * @return starting date/time
   */
  public final Date getStartDate() {
    return startDate;
  }

  /**
   * Return the ending date/time
   * @return ending date/time
   */
  public final Date getEndDate() {
    return endDate;
  }

  /**
   * Update the ending date/time
   * @param endDate ending date/time
   */
  public final void setEndDate(final Date endDate) {
    this.endDate = endDate;
  }

  /**
   * Return a string representation "[startDate - endDate]"
   * @return "[startDate - endDate]"
   */
  @Override
  public final String toString() {
    return "[" + this.startDate + " - " + this.endDate + "]";
  }

  /**
   * Sort and traverse the given list of date intervals to merge contiguous intervals.
   * This fixes the problem due to HA limit [+/-12h] i.e. the converted JD / Date ranges
   * can have a discontinuity on the date axis.
   *
   * @param intervals date intervals to fix
   */
  public static void merge(final List<DateTimeInterval> intervals) {
    // first sort date intervals :
    Collections.sort(intervals);
    mergeSorted(intervals);
  }

  /**
   * Traverse the given list of date intervals to merge contiguous intervals.
   * This fixes the problem due to HA limit [+/-12h] i.e. the converted JD / Date ranges
   * can have a discontinuity on the date axis.
   *
   * @param intervals SORTED date intervals to fix
   */
  public static void mergeSorted(final List<DateTimeInterval> intervals) {
    int size = intervals.size();
    if (size > 1) {
      DateTimeInterval interval1, interval2;
      for (int i = size - 2, j = size - 1; i >= 0; i--, j--) {
        interval1 = intervals.get(i);
        interval2 = intervals.get(j);

        if (interval1.getEndDate().compareTo(interval2.getStartDate()) >= 0) {
          // merge interval :
          interval1.setEndDate(interval2.getEndDate());
          // remove interval2 :
          intervals.remove(j);
        }
      }
    }
  }
}
