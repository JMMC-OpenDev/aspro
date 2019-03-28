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
    /* Ellipsoid WGS84 */
    /** EARTH FLATTENING f = 1/(298.257,223,563) */
    public final static double EARTH_FLATTENING = 1.0 / 298.257223560;
    /** Earth square excentricity e^2 = 2 x f - f^2 */
    public final static double EARTH_SQUARE_EXCENTRICITY = 2.0 * EARTH_FLATTENING - EARTH_FLATTENING * EARTH_FLATTENING;
    /** Earth radius at equator (in meters) */
    public final static double EARTH_RADIUS_EQUATOR = 6378137.0;
    /**
     * Forbidden constructor
     */
    private GeocentricCoords() {
        // no-op
    }
    
    
    /**
     * Convert geodetic long/lat/alt to geocentric coordinates
     *
     * @param lon longitude in radians
     * @param lat latitude in radians
     * @param alt altitude in meters
     * @return positions (x,y,z)
     */
    public static Position3D getGeocentric(final double lon, final double lat, final double alt) {
        
        final double gcLat = toGeocentricLatitude(lat);
        final double radius = earthRadius(gcLat);

        // note: approximation that radius change due to altitude is considered minor (ellipsoid wgs84 ?)
        final double r = radius + alt;

        final Position3D position = new Position3D(); 
        position.setPosX(r * Math.cos(gcLat) * Math.cos(lon));
        position.setPosY(r * Math.cos(gcLat) * Math.sin(lon));
        position.setPosZ(r * Math.sin(gcLat));

        return position;
    }

    /**
     * Return the longitude, latitude (geodetic) in radians and the altitude (m)
     * @param position XYZ Geocentric coordinates (m)
     * @return longitude, latitude in radians and the altitude (m)
     */
    public static LonLatAlt getLonLatAlt(final Position3D position) {

        final double[] sph = cartesianToSpherical(position);

        // altitude is corrected relative to the earth radius at the given latitude:
        final LonLatAlt coords = new LonLatAlt(sph[0], toGeodeticLatitude(sph[1]), altitude(sph[1], sph[2]));

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
     * @param geocentricLatitude geocentric latitiude in radians
     * @param distance distance to the earth center (m)
     * @return altitude (m)
     */
    private static double altitude(final double geocentricLatitude, final double distance) {
        return distance - earthRadius(geocentricLatitude);
    }
    
    private static double earthRadius(final double geocentricLatitude) {
        // r ( θ ) = a ( 1 - f sin(θ)^2 )
        return EARTH_RADIUS_EQUATOR * (1.0 - EARTH_FLATTENING * Math.pow(Math.sin(geocentricLatitude), 2.0));
    }

    private static double toGeocentricLatitude(final double geodeticLatitude) {
        // geocentricLatitude = tan-1( (1-e^2) * tan geodeticLatitude)
        // e^2 = 2 x f - f^2
        // f = 1/(298.257,223,563)

        return Math.atan(Math.tan(geodeticLatitude) * (1.0 - EARTH_SQUARE_EXCENTRICITY));
    }

    private static double toGeodeticLatitude(final double geocentricLatitude) {
        return Math.atan(Math.tan(geocentricLatitude) / (1.0 - EARTH_SQUARE_EXCENTRICITY));
    }
    
    /**
     * Convert local equatorial coordinates to geocentric coordinates ie rotate by longitude and correct by latitude (geocentric / geodetic)
     */
    public static void convertEquatorialToGeocentric(final Position3D position, final Position3D dest, final LonLatAlt posCenter) {
        
        // rotate x/z coordinates by geodetic / geocentric latitude delta:
        final double gdLat = posCenter.getLatitude();
        final double gcLat = toGeocentricLatitude(gdLat);
        final double diffLat = gcLat - gdLat;  
        
        final double cosLat = Math.cos(diffLat);
        final double sinLat = Math.sin(diffLat);
        
        double x =  cosLat * position.getPosX() + sinLat * position.getPosZ();
        double y =           position.getPosY();
        double z = -sinLat * position.getPosX() + cosLat * position.getPosZ();

        // rotate x/y coordinates by longitude:
        final double phi = -posCenter.getLongitude();
        final double cosPhi = Math.cos(phi);
        final double sinPhi = Math.sin(phi);

        dest.setPosX( cosPhi * x + sinPhi * y);
        dest.setPosY(-sinPhi * x + cosPhi * y);
        dest.setPosZ(z);
    }
}
