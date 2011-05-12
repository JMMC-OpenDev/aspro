/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AngleUtils.java,v 1.3 2010-07-07 09:23:09 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2010/06/17 10:02:51  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.1  2010/01/08 16:51:18  bourgesl
 * initial uv coverage
 *
 ******************************************************************************/
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

  /**
   * Convert an angle given in radians to decimal hours
   * @param angrad angle in radians
   * @return angle in decimal hours
   */
  public static double rad2hours(final double angrad) {
    return Math.toDegrees(angrad) / HA_TO_DEG;
  }

  /**
   * Convert an angle given in degrees to decimal hours
   * @param angdeg angle in radians
   * @return angle in decimal hours
   */
  public static double deg2hours(final double angdeg) {
    return angdeg / HA_TO_DEG;
  }

  /**
   * Convert an angle given in decimal hours to radians
   * @param ha angle in decimal hours
   * @return angle in radians
   */
  public static double hours2rad(final double ha) {
    return Math.toRadians(ha * HA_TO_DEG);
  }

  /**
   * Convert an angle given in decimal hours to degrees
   * @param ha angle in decimal hours
   * @return angle in degrees
   */
  public static double hours2deg(final double ha) {
    return ha * HA_TO_DEG;
  }
}
