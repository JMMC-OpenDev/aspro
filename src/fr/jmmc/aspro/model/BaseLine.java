/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: BaseLine.java,v 1.1 2009-11-20 16:55:46 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 *
 ******************************************************************************/
package fr.jmmc.aspro.model;

/**
 * This class contains the information for a base line
 * @author bourgesl
 */
public class BaseLine {

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

  

}
