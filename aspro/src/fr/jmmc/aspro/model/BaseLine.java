/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

/**
 * This class contains the information for a base line
 * @author bourgesl
 */
public final class BaseLine {

  /** name (station1 - station2) used for debugging purpose */
  private final String name;
  /** first beam (station 1) */
  private final Beam beam1;
  /** second beam (station 2) */
  private final Beam beam2;
  /** relative position of stations in local equatorial plane - X in the meridian plane */
  private final double x;
  /** relative position of stations in local equatorial plane - Y toward east */
  private final double y;
  /** relative position of stations Z - toward north */
  private final double z;

  /**
   * BaseLine constructor
   * @param beam1 beam 1
   * @param beam2 beam 2
   * @param x relative position of stations in local equatorial plane
   * @param y relative position of stations in local equatorial plane
   * @param z relative position of stations Z
   */
  public BaseLine(final Beam beam1, final Beam beam2, final double x, final double y, final double z) {
    this.name = beam1.getStation().getName() + "-" + beam2.getStation().getName();
    this.beam1 = beam1;
    this.beam2 = beam2;
    this.x = x;
    this.y = y;
    this.z = z;
  }

  /**
   * Return the first beam (station 1)
   * @return first beam (station 1)
   */
  public Beam getBeam1() {
    return beam1;
  }

  /**
   * Return the second beam (station 2)
   * @return second beam (station 2)
   */
  public Beam getBeam2() {
    return beam2;
  }

  /**
   * Return relative position of stations in local equatorial plane - X in the meridian plane
   * @return relative position of stations in local equatorial plane
   */
  public double getX() {
    return x;
  }

  /**
   * Return relative position of stations in local equatorial plane - Y toward east
   * @return relative position of stations in local equatorial plane
   */
  public double getY() {
    return y;
  }

  /**
   * Return relative position of stations Z - toward north
   * @return relative position of stations Z
   */
  public double getZ() {
    return z;
  }

  /**
   * Return the baseline name (station1 - station2)
   * @return baseline name (station1 - station2)
   */
  public String getName() {
    return name;
  }

  @Override
  public String toString() {
    return "BL : " + this.name + " [" + this.x + ", " + this.y + ", " + this.z + "]";
  }
}
