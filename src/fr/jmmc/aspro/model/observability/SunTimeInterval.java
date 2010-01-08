/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SunTimeInterval.java,v 1.1 2010-01-08 16:48:29 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.3  2009/12/02 17:23:51  bourgesl
 * fixed several bugs on pop finder + refactoring
 *
 */
package fr.jmmc.aspro.model.observability;

import java.util.Date;

/**
 * This class contains a simple date/time interval corresponding to a sun set/rise/twilight
 * @author bourgesl
 */
public class SunTimeInterval extends DateTimeInterval {

  public enum SunType {
    Day,
    Night,
    Twilight;
  }
  /** type of the interval : twilight, rise, set */
  private final SunType type;

  public SunTimeInterval(final Date start, final Date end, final SunType type) {
    super();
    setStartDate(start);
    setEndDate(end);
    this.type = type;
  }

  public SunType getType() {
    return type;
  }
}
