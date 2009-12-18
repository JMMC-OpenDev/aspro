/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: DateTimeInterval.java,v 1.2 2009-12-18 11:52:02 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.model;

import java.util.Date;

/**
 * This class contains a simple date/time interval
 * @author bourgesl
 */
public class DateTimeInterval {
  /** starting date */
  private Date startDate;
  /** ending date */
  private Date endDate;

  public DateTimeInterval() {
  }

  public void setEndDate(Date endDate) {
    this.endDate = endDate;
  }

  public void setStartDate(Date startDate) {
    this.startDate = startDate;
  }

  public Date getEndDate() {
    return endDate;
  }

  public Date getStartDate() {
    return startDate;
  }

  @Override
  public String toString() {
    return "[" + this.startDate + " - " + this.endDate + "]";
  }

}
