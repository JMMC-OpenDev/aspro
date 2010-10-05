/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: DateTimeInterval.java,v 1.2 2010-04-13 15:26:21 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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

  /** starting date */
  private final Date startDate;
  /** ending date */
  private final Date endDate;

  public DateTimeInterval(final Date startDate, final Date endDate) {
    this.startDate = startDate;
    this.endDate = endDate;
  }

  /**
   * Simple comparator that takes into account only the starting date
   * @param other date interval
   * @return Double.compare(startDate1, startDate2)
   */
  public int compareTo(final DateTimeInterval other) {
    return this.startDate.compareTo(other.getStartDate());
  }

  public final Date getStartDate() {
    return startDate;
  }

  public final Date getEndDate() {
    return endDate;
  }

  @Override
  public String toString() {
    return "[" + this.startDate + " - " + this.endDate + "]";
  }
}
