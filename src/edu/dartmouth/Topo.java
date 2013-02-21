package edu.dartmouth;

import net.jafama.FastMath;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
 modify it for their own purposes, provided that credit is given to the author
 in a prominent place.  For the present program that means that the green
 title and author banner appearing on the main window must not be removed,
 and may not be altered without premission of the author. */
/** topocentric correction stuff */
public final class Topo {

    static final double FLATTEN = 0.003352813;  // flattening, 1/298.257
    static final double EQUAT_RAD = 6378137.;   // equatorial radius, meters
    static final double INV_EQUAT_RAD = 1d / EQUAT_RAD;

    static double[] Geocent(final double longitin, final double latitin, final double height) {
        // XYZ coordinates given geographic.  Declared static because it will often
        // be used with lst in place of Longitude.
        // input is decimal hours, decimal degrees, and meters.
        // See 1992 Astr Almanac, p. K11.

        //System.out.printf("lat long %f %f%n",latitin,longitin);
        final double geolat = latitin * Const.RADIAN_IN_DEG;
        final double geolong = longitin * Const.RADIAN_IN_HRS;
        //System.out.printf("radians %f %f%n",geolat,geolong);
        final double coslat = FastMath.cos(geolat);
        final double sinlat = FastMath.sin(geolat);
        final double coslong = FastMath.cos(geolong);
        final double sinlong = FastMath.sin(geolong);

        final double denom = coslat * coslat + (1d - FLATTEN) * sinlat * (1d - FLATTEN) * sinlat;

        final double C_geo = 1d / Math.sqrt(denom) + height * INV_EQUAT_RAD;
        final double S_geo = (1d - FLATTEN) * (1d - FLATTEN) * C_geo + height * INV_EQUAT_RAD;

        return new double[]{
                    C_geo * coslat * coslong,
                    C_geo * coslat * sinlong,
                    S_geo * sinlat
                };
    }

    static Celest topocorr(final Celest geopos, final InstantInTime when, final Site where, final double sidereal) {
        // The geopos to which this is being applied needs to have its
        // distance set.

        final double alphaRad = geopos.alpha.radians();
        final double deltaRad = geopos.delta.radians();

        final double cosdec = FastMath.cos(deltaRad);

        double x = FastMath.cos(alphaRad) * cosdec * geopos.distance;
        double y = FastMath.sin(alphaRad) * cosdec * geopos.distance;
        double z = FastMath.sin(deltaRad) * geopos.distance;

        final double[] retvals = Geocent(sidereal, where.lat.value, where.elevsea);

        x -= retvals[0] * Const.AU_IN_EARTHRAD;
        y -= retvals[1] * Const.AU_IN_EARTHRAD;
        z -= retvals[2] * Const.AU_IN_EARTHRAD;

        final double topodist = Math.sqrt(x * x + y * y + z * z);

        x /= topodist;
        y /= topodist;
        z /= topodist;

        return new Celest(FastMath.atan2(y, x) * Const.HRS_IN_RADIAN,
                FastMath.asin(z) * Const.DEG_IN_RADIAN,
                when.julianEpoch(), topodist);
    }
}
