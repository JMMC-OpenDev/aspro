/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SunTimeInterval.java,v 1.3 2011-01-25 13:48:55 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2010/04/13 15:26:21  bourgesl
 * add Comparable support
 *
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
 * This class contains a simple date/time interval corresponding to DAY / NIGHT / TWILIGHT.
 * @author bourgesl
 */
public final class SunTimeInterval extends DateTimeInterval {

  /**
   * Type of SunTimeInterval (DAY / NIGHT / TWILIGHT)
   */
  public enum SunType {

    /** DAY */
    Day,
    /** NIGHT */
    Night,
    /** TWILIGHT */
    Twilight;
  }
  /** type of the interval : DAY / NIGHT / TWILIGHT */
  private final SunType type;

  /**
   * Public constructor
   * @param startDate starting date/time
   * @param endDate ending date/time
   * @param type type of the interval
   */
  public SunTimeInterval(final Date startDate, final Date endDate, final SunType type) {
    super(startDate, endDate);
    this.type = type;
  }

  /**
   * Return the type of the interval
   * @return type of the interval
   */
  public SunType getType() {
    return type;
  }
}
