/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.util;

/**
 * This class has several utility methods to convert angles 
 * @author bourgesl
 */
public final class AngleUtils {

  /** degrees to hour angle = 15 */
  public final static double HA_TO_DEG = 15d;
  /** radian to hour angle */
  public final static double RAD_TO_HOUR = 180d / Math.PI / HA_TO_DEG;

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
//    return Math.toDegrees(angrad) / HA_TO_DEG;
    return angrad * RAD_TO_HOUR;    
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
