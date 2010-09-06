/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: BaseLine.java,v 1.2 2009-11-24 15:12:09 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2009/11/20 16:55:46  bourgesl
 * Added Beam / Delay Line definition
 * ObservabilityService is stateless to simplify coding
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.model;

/**
 * This class contains the information for a base line
 * @author bourgesl
 */
public class BaseLine {
  /** name used for debug purpose */
  private final String name;

  /** first beam (station) */
  private final Beam beam1;

  /** second beam (station) */
  private final Beam beam2;

  /** relative position of stations in local equatorial plane - X in the meridian plane */
  private final double x;
  /** relative position of stations in local equatorial plane - Y toward east */
  private final double y;
  /** relative position of stations Z - toward north */
  private final double z;

  public BaseLine(final Beam b1, final Beam b2, final double x, final double y, final double z) {
    this.name = b1.getStation().getName() + "-" + b2.getStation().getName();
    this.beam1 = b1;
    this.beam2 = b2;
    this.x = x;
    this.y = y;
    this.z = z;
  }

  public Beam getBeam1() {
    return beam1;
  }

  public Beam getBeam2() {
    return beam2;
  }

  public double getX() {
    return x;
  }

  public double getY() {
    return y;
  }

  public double getZ() {
    return z;
  }

  public String getName() {
    return name;
  }

  @Override
  public String toString() {
    return "BL : " + this.name + " [" + this.x + ", " + this.y + ", " + this.z + "]";
  }

}
