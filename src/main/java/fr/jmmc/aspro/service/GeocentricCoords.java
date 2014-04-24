/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.oi.LonLatAlt;
import fr.jmmc.aspro.model.oi.Position3D;
import fr.jmmc.jmal.ALX;
import fr.jmmc.jmcs.util.NumberUtils;
import net.jafama.FastMath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class has several methods useful to convert the Geocentric Earth Coordinate frame to Geographic coordinates
 * @author bourgesl
 */
public final class GeocentricCoords {

  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(GeocentricCoords.class.getName());

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
      a = FastMath.atan2(y, x);
      b = FastMath.atan2(z, rxy);
    } else {
      a = 0d;
      b = (z == 0d) ? 0d : FastMath.atan2(z, rxy);
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
    logger.debug("{} = {}", msg, sph);
  }

  /**
   * Return a string representing the given coordinates
   * @param lon longitude in radians
   * @param lat latitude in radians
   * @param d distance (m)
   * @return string representing the given coordinates
   */
  public static String toString(final double lon, final double lat, final double d) {
    return ALX.toDMS(new StringBuilder(16), FastMath.toDegrees(lon), 360d) 
            + ", " + ALX.toDMS(FastMath.toDegrees(lat)) 
            + ", " + NumberUtils.trimTo3Digits(d) + " m";
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
}
