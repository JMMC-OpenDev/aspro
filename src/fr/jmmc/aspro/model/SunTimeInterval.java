/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.jmmc.aspro.model;

import java.util.Date;

/**
 * This class contains a simple date/time interval corresponding to a sun set/rise/twilight
 * @author bourgesl
 */
public class SunTimeInterval {

  public enum SunType {
    Day,
    Night,
    Twilight;
  }
  /** starting date */
  private final Date startDate;
  /** ending date */
  private final Date endDate;
  /** type of the interval : twilight, rise, set */
  private final SunType type;

  public SunTimeInterval(final Date start, final Date end, final SunType type) {
    this.startDate = start;
    this.endDate = end;
    this.type = type;
  }

  public Date getEndDate() {
    return endDate;
  }

  public Date getStartDate() {
    return startDate;
  }

  public SunType getType() {
    return type;
  }
}
