package edu.dartmouth;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
modify it for their own purposes, provided that credit is given to the author
in a prominent place.  For the present program that means that the green
title and author banner appearing on the main window must not be removed,
and may not be altered without premission of the author. */
public final class Ecliptic {
  /* Holds some static methods for rotating from ecliptic to equatorial and back ... */

  static double[] eclrot(final double jd, final double x, final double y, final double z) {
    /** rotates x,y,z coordinates to equatorial x,y,z; all are
    in equinox of date. Returns [0] = x, [1] = y, [2] = z */
    final double T = (jd - Const.J2000) / 36525d;
    final double incl = (23.439291d + T * (-0.0130042d - 0.00000016d * T)) / Const.DEG_IN_RADIAN;
    /* 1992 Astron Almanac, p. B18, dropping the
    cubic term, which is 2 milli-arcsec! */
    // System.out.printf("T incl %f %f\n",T,incl);
    return new double[]{
              x,
              Math.cos(incl) * y - Math.sin(incl) * z,
              Math.sin(incl) * y + Math.cos(incl) * z
            };
  }

  static double[] Cel2Ecl(Observation o) {
    /** rotates celestial coords to equatorial at
    equinox of jd. Returns [0] = x, [1] = y, [2] = z */
    double incl;
    double T;
    double[] retval = {0d, 0d};   // ecliptic Longitude and Latitude
    double[] equat = {0d, 0d, 0d}; // equatorial unit vector
    double[] eclipt = {0d, 0d, 0d}; // ecliptic unit vector

    T = (o.w.when.jd - Const.J2000) / 36525;
    incl = (23.439291 + T * (-0.0130042 - 0.00000016 * T)) / Const.DEG_IN_RADIAN;
    /* 1992 Astron Almanac, p. B18, dropping the
    cubic term, which is 2 milli-arcsec! */
    // System.out.printf("T incl %f %f\n",T,incl);

    equat = o.current.cel_unitXYZ();

    eclipt[1] = Math.cos(incl) * equat[1] + Math.sin(incl) * equat[2];
    eclipt[2] = -1d * Math.sin(incl) * equat[1] + Math.cos(incl) * equat[2];
    eclipt[0] = equat[0];

    retval[0] = Math.atan2(eclipt[1], eclipt[0]) * Const.DEG_IN_RADIAN;
    while (retval[0] < 0d) {
      retval[0] += 360d;
    }
    while (retval[0] >= 360d) {
      retval[0] -= 360d;
    }
    retval[1] = Math.asin(eclipt[2]) * Const.DEG_IN_RADIAN;

    return (retval);
  }
}
