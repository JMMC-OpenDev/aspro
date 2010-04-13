/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SunTimeInterval.java,v 1.2 2010-04-13 15:26:21 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/01/08 16:48:29  bourgesl
 * package refactoring
 *
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

  public SunTimeInterval(final Date startDate, final Date endDate, final SunType type) {
    super(startDate, endDate);
    this.type = type;
  }

  public SunType getType() {
    return type;
  }
}
