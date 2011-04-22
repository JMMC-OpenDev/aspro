package edu.dartmouth;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
modify it for their own purposes, provided that credit is given to the author
in a prominent place.  For the present program that means that the green
title and author banner appearing on the main window must not be removed,
and may not be altered without premission of the author. */
public final class Sun implements Cloneable {

  Celest geopos;
  Celest topopos;
  double[] xyz = {0d, 0d, 0d};     /* for use in barycentric correction */

  double[] xyzvel = {0d, 0d, 0d};  /* ditto. */


  Sun(final WhenWhere w) {
    update(w.when, w.where, w.sidereal);
  }

  Sun(final InstantInTime inst) {  // no site, so no topo
    final double[] retvals = computesun(inst.jd);
    xyz[0] = retvals[3];
    xyz[1] = retvals[4];
    xyz[2] = retvals[5];
    geopos = new Celest(retvals[0], retvals[1], inst.JulianEpoch());
    geopos.distance = retvals[2];
    topopos = new Celest(0d, 0d, inst.JulianEpoch());  // not set
    topopos.distance = 0d;
  }

  Sun(final double jdin) {        // no site, so no topo possible
    final double[] retvals = computesun(jdin);
    xyz[0] = retvals[3];
    xyz[1] = retvals[4];
    xyz[2] = retvals[5];
    final double eq = 2000d + (jdin - Const.J2000) / 365.25d;
    geopos = new Celest(retvals[0], retvals[1], eq);
    geopos.distance = retvals[2];
    topopos = new Celest(0d, 0d, eq); // not set, no geogr. info
    topopos.distance = 0d;
  }

  @Override
  public Sun clone() {
    try {
      final Sun copy = (Sun) super.clone();
      copy.geopos = geopos.clone();
      copy.topopos = topopos.clone();
      return copy;
    } catch (CloneNotSupportedException e) {
      throw new Error("This should never happen!");
    }
  }

  void update(final InstantInTime when, final Site where, final double sidereal) {
    // System.out.printf("updating sun jd %f ... ",w.when.jd);

    // need to avoid handing in a whenwhere or get circularity ...
    final double[] retvals = computesun(when.jd);
    xyz = new double[3];
    xyz[0] = retvals[3];
    xyz[1] = retvals[4];
    xyz[2] = retvals[5];

    //System.out.printf("Sun constructor - jd %f xyz = %f %f %f\n",
    //     w.when.jd,xyz[0],xyz[1],xyz[2]);

    // ignoring topocentric part of helio time correction.

    geopos = new Celest(retvals[0], retvals[1], when.JulianEpoch());
    geopos.distance = retvals[2];
    topopos = Topo.topocorr(geopos, when, where, sidereal);
    // System.out.printf("topo radec %s %s\n",topopos.alpha.RoundedRAString(2,":"),
    //        topopos.delta.RoundedDecString(1,":"));
  }


  /* Implements Jean Meeus' solar ephemeris, from Astronomical
  Formulae for Calculators, pp. 79 ff.  Position is wrt *mean* equinox of
  date. */
  static double[] computesun(final double jdIn) {

    // correct jd to ephemeris time once we have that done ...

    final double jd = jdIn + DeltaT.etcorr(jdIn) / 86400d;
    final double T = (jd - 2415020d) / 36525d;  // Julian centuries since 1900
    final double Tsq = T * T;
    final double Tcb = T * Tsq;

    final double M = 358.47583d + 35999.04975d * T - 0.000150d * Tsq - 0.0000033d * Tcb;
    final double e = 0.01675104d - 0.0000418d * T - 0.000000126d * Tsq;

    final double A = (153.23d + 22518.7541d * T) / Const.DEG_IN_RADIAN;  /* A, B due to Venus */
    final double B = (216.57d + 45037.5082d * T) / Const.DEG_IN_RADIAN;
    final double C = (312.69d + 32964.3577d * T) / Const.DEG_IN_RADIAN;  /* C due to Jupiter */
    /* D -- rough correction from earth-moon barycenter to center of earth. */
    final double D = (350.74d + 445267.1142d * T - 0.00144d * Tsq) / Const.DEG_IN_RADIAN;
    final double E = (231.19d + 20.20d * T) / Const.DEG_IN_RADIAN;
    /* "inequality of long period .. */
    final double H = (353.40d + 65928.7155d * T) / Const.DEG_IN_RADIAN;  /* Jupiter. */

    final double L = 279.69668d + 36000.76892d * T + 0.0003025d * Tsq
            + 0.00134d * Math.cos(A) + 0.00154d * Math.cos(B) + 0.00200d * Math.cos(C) + 0.00179d * Math.sin(D) + 0.00178d * Math.sin(E);

//    final double Lrad = L / Const.DEG_IN_RADIAN;
    final double Mrad = M / Const.DEG_IN_RADIAN;

    final double Cent = (1.919460d - 0.004789d * T - 0.000014d * Tsq) * Math.sin(Mrad)
            + (0.020094d - 0.000100d * T) * Math.sin(2d * Mrad)
            + 0.000293d * Math.sin(3d * Mrad);

    final double nu = M + Cent;
    final double nurad = nu / Const.DEG_IN_RADIAN;

    final double R = (1.0000002d * (1d - e * e)) / (1d + e * Math.cos(nurad))
            + 0.00000543d * Math.sin(A) + 0.00001575d * Math.sin(B) + 0.00001627d * Math.sin(C) + 0.00003076d * Math.cos(D) + 0.00000927d * Math.sin(H);

    final double sunlong = (L + Cent) / Const.DEG_IN_RADIAN;
    /*      printf("solar Longitude: %10.5f  Radius vector %10.7f\n",sunlong,R);
    printf("eccentricity %10.7f  eqn of center %10.5f\n",e,Cent);   */

    final double[] retvals = new double[6];
    // will be geora, geodec, geodist, x, y, z (geo)

    retvals[2] = R; // distance

    /* geocentric */
    final double[] equatorial = Ecliptic.eclrot(jd, Math.cos(sunlong), Math.sin(sunlong), 0d);

    retvals[0] = Math.atan2(equatorial[1], equatorial[0]) * Const.HRS_IN_RADIAN;
    while (retvals[0] < 0d) {
      retvals[0] += 24d;
    }
    retvals[1] = Math.asin(equatorial[2]) * Const.DEG_IN_RADIAN;

    retvals[3] = equatorial[0] * R;  // xyz
    retvals[4] = equatorial[1] * R;
    retvals[5] = equatorial[2] * R;
//       System.out.printf("computesun XYZ %f %f %f  %f\n",
//          retvals[3],retvals[4],retvals[5],jd);

    return retvals;
  }

  void sunvel(final double jd) {
    /* numerically differentiates sun xyz to get velocity. */
    final double dt = 0.05d; // days ... gives about 8 digits ...
    int i;

    final double[] pos1 = computesun(jd - dt / 2d);
    final double[] pos2 = computesun(jd + dt / 2d);
    for (i = 0; i < 3; i++) {
      xyzvel[i] = (pos2[i + 3] - pos1[i + 3]) / dt;  // AU/d, eq. of date.
    }
  }
}
