/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ElevationDate.java,v 1.2 2011-01-25 13:48:56 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/04/02 14:37:40  bourgesl
 * simple bean to store elevation information
 *
 */
package fr.jmmc.aspro.model.observability;

import java.util.Date;

/**
 * This class describes the date corresponding to the elevation of the target
 * @author bourgesl
 */
public final class ElevationDate {

  /** date/time */
  private final Date date;
  /** elevation in degrees */
  private final int elevation;

  /**
   * Public constructor
   * @param date date/time
   * @param elevation elevation in degrees
   */
  public ElevationDate(final Date date, final int elevation) {
    this.date = date;
    this.elevation = elevation;
  }

  /**
   * Return the date/time
   * @return date/time
   */
  public Date getDate() {
    return date;
  }

  /**
   * Return the elevation in degrees
   * @return elevation in degrees
   */
  public int getElevation() {
    return elevation;
  }

  /**
   * Return a string representation "[date = elevation]"
   * @return "[date = elevation]"
   */
  @Override
  public String toString() {
    return "[" + this.date + " = " + this.elevation + "°]";
  }
}
