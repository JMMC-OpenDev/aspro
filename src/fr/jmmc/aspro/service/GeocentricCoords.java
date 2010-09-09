/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: GeocentricCoords.java,v 1.7 2010-09-09 16:01:19 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.6  2010/06/17 10:02:50  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.5  2010/04/02 14:37:52  bourgesl
 * javadoc
 *
 * Revision 1.4  2009/11/05 12:59:39  bourgesl
 * first simple source observability (only min elevation condition)
 *
 * Revision 1.3  2009/10/22 15:47:22  bourgesl
 * beginning of observability computation with jSkyCalc
 *
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.oi.LonLatAlt;
import fr.jmmc.aspro.model.oi.Position3D;
import fr.jmmc.mcs.astro.ALX;
import java.util.logging.Level;

/**
 * This class has several methods useful to convert the Geocentric Earth Coordinate frame to Geographic coordinates
 * @author bourgesl
 */
public final class GeocentricCoords {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.service.GeocentricCoords";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /**
   * Forbidden constructor
   */
  private GeocentricCoords() {
    // no-op
  }

  /**
   * Return the longitude, latitude in radians and the altitude (m)
   * @param position XYZ Geocentric coordinates (m)
   * @return longitude, latitude in radians and the altitude (m)
   */
  public static LonLatAlt getLonLatAlt(final Position3D position) {

    final double[] sph = cartesianToSpherical(position);

    // altitude correction relative to the fixed earth radius only :
    final LonLatAlt coords = new LonLatAlt(sph[0], sph[1], altitude(sph[2]));

    return coords;
  }

  /**
   * Convert cartesian coordinates to spherical coordinates (radians,radians,meters)
   * @param position XYZ Geocentric coordinates (m)
   * @return longitude, latitude in radians and distance (m)
   */
  public static double[] cartesianToSpherical(final Position3D position) {
    final double x = position.getPosX();
    final double y = position.getPosY();
    final double z = position.getPosZ();

    final double rxy2 = x * x + y * y;
    final double rxy = Math.sqrt(rxy2);

    double a;
    double b;
    if (rxy2 != 0d) {
      a = Math.atan2(y, x);
      b = Math.atan2(z, rxy);
    } else {
      a = 0d;
      b = (z == 0d) ? 0d : Math.atan2(z, rxy);
    }
    final double r = Math.sqrt(rxy2 + z * z);

    return new double[]{a, b, r};
  }

  /**
   * Dump the given spherical coordinates in the logs
   * @param msg starting message
   * @param sph longitude, latitude in radians and the distance (m)
   */
  public static void dump(final String msg, final LonLatAlt sph) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine(msg + " = " + sph.toString());
    }
  }

  /**
   * Return a string representing the given coordinates
   * @param lon longitude in radians
   * @param lat latitude in radians
   * @param d distance (m)
   * @return string representing the given coordinates
   */
  public static String toString(final double lon, final double lat, final double d) {
    return ALX.toDms(Math.toDegrees(lon)) + ", " + ALX.toDms(Math.toDegrees(lat)) + ", " + d + " m";
  }

  /**
   * Return the altitude of the given distance
   * @param distance distance to the earth center (m)
   * @return altitude (m)
   */
  private static double altitude(final double distance) {
    return distance - AsproConstants.EARTH_RADIUS;
  }

  /**
   * Convert local coordinates i.e. Local east, north, up (ENU) coordinates) to
   * Earth Centred Earth Fixed (ECEF or ECF) coordinates
   */
  public static void convertENUtoECEF() {
    // http://en.wikipedia.org/wiki/Geodetic_system
    // ecef_origin(latitude lat, longitude lon, height z)
    // ECEF = M * ENU + ECEF_ORIGIN
    // [X] = [-sin(lon) -sin(lat) * cos(lon) cos(lat)*cos(lon) ] [x] + [X_orig]
    // [Y] = [ cos(lon) -sin(lat) * sin(lon) cos(lat)*sin(lon) ] [y] + [Y_orig]
    // [Z] = [    0            cos(lat)           sin(lat)     ] [z] + [Z_orig]
  }

  /**
   * Test coordinate conversions
   * @param args
   */
  public static void main(String[] args) {
    /*
    -24.570106, -70.406044
    -24° 34' 12.38", -70° 24' 21.76
     */
    System.out.println("-24.570106 [-24° 34' 12.38\"] : " + ALX.toDms(-24.570106d));
    System.out.println("-70.406044 [-70° 24' 21.76\"] : " + ALX.toDms(-70.406044d));

  }
}
