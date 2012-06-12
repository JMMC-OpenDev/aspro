package edu.dartmouth;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
modify it for their own purposes, provided that credit is given to the author
in a prominent place.  For the present program that means that the green
title and author banner appearing on the main window must not be removed,
and may not be altered without premission of the author. */
public final class SkyIllum {

  /** container for the ztwilight and krisciunas/schaefer routines */
  static double ztwilight(final double alt) {

    /* evaluates a polynomial expansion for the approximate brightening
    in magnitudes of the zenith in twilight compared to its
    value at full night, as function of altitude of the sun (in degrees).
    To get this expression I looked in Meinel, A.,
    & Meinel, M., "Sunsets, Twilight, & Evening Skies", Cambridge U.
    Press, 1983; there's a graph on p. 38 showing the decline of
    zenith twilight.  I read points off this graph and fit them with a
    polynomial; I don't even know what band there data are for! */
    /* Comparison with Ashburn, E. V. 1952, JGR, v.57, p.85 shows that this
    is a good fit to his B-band measurements.  */

    if (alt > 0d) {
      return 99d;  // guard
    }
    if (alt < -18d) {
      return 0d;
    }

    final double y = (-1d * alt - 9d) / 9d;  /* my polynomial's argument...*/
    return ((2.0635175d * y + 1.246602d) * y - 9.4084495d) * y + 6.132725d;
  }

  /** Evaluates predicted LUNAR part of sky brightness, in
  V magnitudes per square arcsecond, following K. Krisciunas
  and B. E. Schaeffer (1991) PASP 103, 1033.*/
  static double lunskybright(double alpha, final double rho, final double kzen,
                             final double altmoon, final double alt, double moondist) {
    /*
    alpha = separation of sun and moon as seen from earth,
    converted internally to its supplement,
    rho = separation of moon and object,
    kzen = zenith extinction coefficient,
    altmoon = altitude of moon above horizon,
    alt = altitude of object above horizon
    moondist = distance to moon, in earth radii

    all are in decimal degrees. */

    final double rho_rad = rho / Const.DEG_IN_RADIAN;
    alpha = (180d - alpha);
    final double Zmoon = (90d - altmoon) / Const.DEG_IN_RADIAN;
    final double Z = (90d - alt) / Const.DEG_IN_RADIAN;
    moondist = Const.EARTHRAD_IN_AU * moondist / (60.27d);
    /* distance arrives in AU, want it normalized to mean distance,
    60.27 earth radii. */

    double istar = -0.4d * (3.84d + 0.026d * Math.abs(alpha) + 4.0e-9d * Math.pow(alpha, 4d)); /*eqn 20*/
    istar = Math.pow(10d, istar) / (moondist * moondist);
    if (Math.abs(alpha) < 7d) /* crude accounting for opposition effect */ {
      istar *= (1.35d - 0.05d * Math.abs(istar));
    }
    /* 35 per cent brighter at full, effect tapering linearly to
    zero at 7 degrees away from full. mentioned peripherally in
    Krisciunas and Scheafer, p. 1035. */
    double fofrho = 229087d * (1.06d + Math.cos(rho_rad) * Math.cos(rho_rad));
    if (Math.abs(rho) > 10d) {
      fofrho += Math.pow(10d, (6.15d - rho / 40d));            /* eqn 21 */
    } else if (Math.abs(rho) > 0.25d) {
      fofrho += 6.2e7d / (rho * rho);   /* eqn 19 */
    } else {
      fofrho += 9.9e8d;  /*for 1/4 degree -- radius of moon! */
    }
    double Xzm = Math.sqrt(1.0d - 0.96d * Math.sin(Zmoon) * Math.sin(Zmoon));
    if (Xzm != 0d) {
      Xzm = 1d / Xzm;
    } else {
      Xzm = 10000d;
    }
    double Xo = Math.sqrt(1.0d - 0.96d * Math.sin(Z) * Math.sin(Z));
    if (Xo != 0d) {
      Xo = 1d / Xo;
    } else {
      Xo = 10000d;
    }
    final double Bmoon = fofrho * istar * Math.pow(10d, (-0.4d * kzen * Xzm)) * (1d - Math.pow(10d, (-0.4d * kzen * Xo)));   /* nanoLamberts */
    if (Bmoon > 0.001d) {
      return (22.50d - 1.08574d * Math.log(Bmoon / 34.08d)); /* V mag per sq arcs-eqn 1 */
    } else {
      return 99d;
    }
  }
}
