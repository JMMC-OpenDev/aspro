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
 */
package fr.jmmc.aspro.model.observability;

import java.util.Date;

/**
 * This class contains a simple date/time interval
 * @author bourgesl
 */
public class DateTimeInterval implements Comparable<DateTimeInterval> {

  /** starting date/time */
  private final Date startDate;
  /** ending date/time */
  private final Date endDate;

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
   * Simple comparator that takes into account only the starting date
   * @param other date interval
   * @return Double.compare(startDate1, startDate2)
   */
  public final int compareTo(final DateTimeInterval other) {
    return this.startDate.compareTo(other.getStartDate());
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
   * Return a string representation "[startDate - endDate]"
   * @return "[startDate - endDate]"
   */
  @Override
  public final String toString() {
    return "[" + this.startDate + " - " + this.endDate + "]";
  }
}
