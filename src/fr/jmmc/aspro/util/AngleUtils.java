/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AngleUtils.java,v 1.2 2010-06-17 10:02:51 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/01/08 16:51:18  bourgesl
 * initial uv coverage
 *
 */
package fr.jmmc.aspro.util;

/**
 * This class has several utility methods to convert angles 
 * @author bourgesl
 */
public final class AngleUtils {

  /** degrees to hour angle = 15 */
  public final static double HA_TO_DEG = 15d;

  /**
   * Forbidden constructor
   */
  private AngleUtils() {
    // no-op
  }

  public static double rad2hours(final double angrad) {
    return Math.toDegrees(angrad) / HA_TO_DEG;
  }

  public static double deg2hours(final double angdeg) {
    return angdeg / HA_TO_DEG;
  }

  public static double hours2rad(final double ha) {
    return Math.toRadians(ha * HA_TO_DEG);
  }

  public static double hours2deg(final double ha) {
    return ha * HA_TO_DEG;
  }
}
