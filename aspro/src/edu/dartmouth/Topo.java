package edu.dartmouth;

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

  static double[] Geocent(final double longitin, final double latitin, final double height) {
    // XYZ coordinates given geographic.  Declared static because it will often
    // be used with lst in place of Longitude.
    // input is decimal hours, decimal degrees, and meters.
    // See 1992 Astr Almanac, p. K11.

    //System.out.printf("lat long %f %f%n",latitin,longitin);
    final double geolat = latitin / Const.DEG_IN_RADIAN;
    final double geolong = longitin / Const.HRS_IN_RADIAN;
    //System.out.printf("radians %f %f%n",geolat,geolong);
    final double coslat = Math.cos(geolat);
    final double sinlat = Math.sin(geolat);
    final double coslong = Math.cos(geolong);
    final double sinlong = Math.sin(geolong);

    final double denom = coslat * coslat + (1d - FLATTEN) * sinlat * (1d - FLATTEN) * sinlat;

    final double C_geo = 1d / Math.sqrt(denom) + height / EQUAT_RAD;
    final double S_geo = (1d - FLATTEN) * (1d - FLATTEN) * C_geo + height / EQUAT_RAD;

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

    double x = Math.cos(alphaRad) * Math.cos(deltaRad) * geopos.distance;
    double y = Math.sin(alphaRad) * Math.cos(deltaRad) * geopos.distance;
    double z = Math.sin(deltaRad) * geopos.distance;

    final double[] retvals = Geocent(sidereal, where.lat.value, where.elevsea);

    x -= retvals[0] / Const.EARTHRAD_IN_AU;
    y -= retvals[1] / Const.EARTHRAD_IN_AU;
    z -= retvals[2] / Const.EARTHRAD_IN_AU;

    final double topodist = Math.sqrt(x * x + y * y + z * z);

    x /= topodist;
    y /= topodist;
    z /= topodist;

    return new Celest(Math.atan2(y, x) * Const.HRS_IN_RADIAN,
            Math.asin(z) * Const.DEG_IN_RADIAN,
            when.julianEpoch(), topodist);
  }
}
