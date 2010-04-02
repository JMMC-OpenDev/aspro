/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ElevationDate.java,v 1.1 2010-04-02 14:37:40 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.model.observability;

import java.util.Date;

/**
 * This class describes the date corresponding to the elevation of the target
 * @author bourgesl
 */
public class ElevationDate {

  /** event date */
  private final Date date;
  /** elevation */
  private final int elevation;

  public ElevationDate(final Date date, final int elevation) {
    this.date = date;
    this.elevation = elevation;
  }

  public Date getDate() {
    return date;
  }

  public int getElevation() {
    return elevation;
  }

  @Override
  public String toString() {
    return "[" + this.date + " = " + this.elevation + "°]";
  }
}
