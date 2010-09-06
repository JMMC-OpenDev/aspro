/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: GeocentricCoords.java,v 1.6 2010-06-17 10:02:50 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
import fr.jmmc.aspro.model.oi.Position3D;
import java.util.logging.Level;
import uk.ac.starlink.pal.Cartesian;
import uk.ac.starlink.pal.Pal;
import uk.ac.starlink.pal.Spherical;

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
  /** AstroLib Positional astronomical Library instance */
  private final static Pal pal = new Pal();

  /**
   * Forbidden constructor
   */
  private GeocentricCoords() {
    // no-op
  }

  /**
   * Return the longitude, latitude in radians and the altitude (m)
   * @param position XYZ Geocentric coordinates
   * @return longitude, latitude in radians and the altitude (m)
   */
  public static Spherical getLonLatAlt(final Position3D position) {
    final Spherical sph = pal.Dc62s(new Cartesian(position.getPosX(), position.getPosY(), position.getPosZ()));

    // altitude correction relative to the fixed earth radius only :
    return new Spherical(sph.getLong(), sph.getLat(), altitude(sph));
  }

  /**
   * Dump the given spherical coordinates in the logs
   * @param msg starting message
   * @param sph longitude, latitude in radians and the distance (m)
   */
  public static void dump(final String msg, final Spherical sph) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine(msg + " = " + toString(sph.getLong(), sph.getLat(), sph.getRadial()));
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
    return pal.Dr2af(lon) + ", "
            + pal.Dr2af(lat) + ", "
            + d + " m";
  }

  /**
   * Return the altitude of the given spherical location
   * @param sph location : longitude, latitude in radians and distance to the earth center (m)
   * @return altitude (m)
   */
  private static double altitude(final Spherical sph) {
    return sph.getRadial() - AsproConstants.EARTH_RADIUS;
  }
}
