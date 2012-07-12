/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.observability;

import java.util.Date;

/**
 * This class describes the date corresponding to the position (azimuth / elevation) of the target
 * @author bourgesl
 */
public final class TargetPositionDate {

  /** date/time */
  private final Date date;
  /** azimuth in degrees */
  private final int azimuth;
  /** elevation in degrees */
  private final int elevation;

  /**
   * Public constructor
   * @param date date/time
   * @param azimuth azimuth in degrees
   * @param elevation elevation in degrees
   */
  public TargetPositionDate(final Date date, final int azimuth, final int elevation) {
    this.date = date;
    this.azimuth = (azimuth >= 360) ? azimuth - 360: azimuth;
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
   * Return the azimuth in degrees
   * @return azimuth in degrees
   */
  public int getAzimuth() {
    return azimuth;
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
    return "[" + this.date + "] = (" + this.azimuth + "°, " + this.elevation + "°)";
  }
}
