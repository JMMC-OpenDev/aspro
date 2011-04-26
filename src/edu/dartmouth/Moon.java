package edu.dartmouth;

import java.util.Locale;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
modify it for their own purposes, provided that credit is given to the author
in a prominent place.  For the present program that means that the green
title and author banner appearing on the main window must not be removed,
and may not be altered without premission of the author. */
public final class Moon implements Cloneable {

  Celest geopos;
  Celest topopos;

  Moon(final double jd) {
    final double[] retvals = computeMoon(jd);
    final double eq = InstantInTime.julianEpoch(jd);
    geopos = new Celest(retvals[0], retvals[1], eq, retvals[2]);
    topopos = new Celest(0d, 0d, eq);  // not set, no geogr info
  }

  Moon(final WhenWhere w) {
//      double [] retvals;
//      retvals = computeMoon(w.when.jd);
//      geopos = new Celest(retvals[0],retvals[1],w.when.julianEpoch(),retvals[2]);
//      topopos = Topo.topocorr(geopos, w);
    update(w.when, w.where, w.sidereal);
  }

  void update(final InstantInTime when, final Site where, final double sidereal) {
    final double[] retvals = computeMoon(when.jd);
    geopos = new Celest(retvals[0], retvals[1], when.julianEpoch(), retvals[2]);
    topopos = Topo.topocorr(geopos, when, where, sidereal);
  }

  @Override
  public Moon clone() {
    try {
      final Moon copy = (Moon) super.clone();
      copy.geopos = geopos.clone();
      copy.topopos = topopos.clone();
      return copy;
    } catch (CloneNotSupportedException e) {
      throw new Error("This should never happen!");
    }
  }

  /** Rather accurate lunar
  ephemeris, from Jean Meeus' *Astronomical Formulae For Calculators*,
  pub. Willman-Bell.  Includes all the terms given there. */
  static double[] computeMoon(final double jdIn) {

    final double jd = jdIn + DeltaT.etcorr(jdIn) / 86400d;
    /* approximate correction to ephemeris time */
    final double T = (jd - 2415020d) / 36525d;   /* this based around 1900 ... */
    final double Tsq = T * T;
    final double Tcb = Tsq * T;

    double Lpr = 270.434164d + 481267.8831d * T - 0.001133d * Tsq + 0.0000019d * Tcb;
    double M = 358.475833d + 35999.0498d * T - 0.000150d * Tsq - 0.0000033d * Tcb;
    double Mpr = 296.104608d + 477198.8491d * T + 0.009192d * Tsq + 0.0000144d * Tcb;
    double D = 350.737486d + 445267.1142d * T - 0.001436d * Tsq + 0.0000019d * Tcb;
    double F = 11.250889d + 483202.0251d * T - 0.003211d * Tsq - 0.0000003d * Tcb;
    double Om = 259.183275d - 1934.1420d * T + 0.002078d * Tsq + 0.0000022d * Tcb;

    Lpr %= 360d;
    M %= 360d;
    Mpr %= 360d;
    D %= 360d;
    F %= 360d;
    Om %= 360d;

    double sinx = Math.sin((51.2d + 20.2d * T) / Const.DEG_IN_RADIAN);
    Lpr += 0.000233d * sinx;
    M -= 0.001778d * sinx;
    Mpr += 0.000817d * sinx;
    D += 0.002011d * sinx;

    sinx = 0.003964d * Math.sin((346.560d + 132.870d * T - 0.0091731d * Tsq) / Const.DEG_IN_RADIAN);

    Lpr += sinx;
    Mpr += sinx;
    D += sinx;
    F += sinx;

    sinx = Math.sin(Om / Const.DEG_IN_RADIAN);
    Lpr += 0.001964d * sinx;
    Mpr += 0.002541d * sinx;
    D += 0.001964d * sinx;
    F -= 0.024691d * sinx;
    F -= 0.004328d * Math.sin((Om + 275.05d - 2.30d * T) / Const.DEG_IN_RADIAN);

    final double e = 1d - 0.002495d * T - 0.00000752d * Tsq;

    M /= Const.DEG_IN_RADIAN;   /* these will all be arguments ... */
    Mpr /= Const.DEG_IN_RADIAN;
    D /= Const.DEG_IN_RADIAN;
    F /= Const.DEG_IN_RADIAN;

    double lambda = Lpr + 6.288750d * Math.sin(Mpr) + 1.274018d * Math.sin(2d * D - Mpr)
            + 0.658309d * Math.sin(2d * D) + 0.213616d * Math.sin(2d * Mpr) - e * 0.185596d * Math.sin(M)
            - 0.114336d * Math.sin(2d * F) + 0.058793d * Math.sin(2d * D - 2d * Mpr)
            + e * 0.057212d * Math.sin(2d * D - M - Mpr) + 0.053320d * Math.sin(2d * D + Mpr)
            + e * 0.045874d * Math.sin(2d * D - M) + e * 0.041024d * Math.sin(Mpr - M)
            - 0.034718d * Math.sin(D) - e * 0.030465d * Math.sin(M + Mpr) + 0.015326d * Math.sin(2d * D - 2d * F)
            - 0.012528d * Math.sin(2d * F + Mpr) - 0.010980d * Math.sin(2d * F - Mpr)
            + 0.010674d * Math.sin(4d * D - Mpr) + 0.010034d * Math.sin(3d * Mpr)
            + 0.008548d * Math.sin(4d * D - 2d * Mpr) - e * 0.007910d * Math.sin(M - Mpr + 2d * D)
            - e * 0.006783d * Math.sin(2d * D + M) + 0.005162d * Math.sin(Mpr - D)
            + e * 0.005000d * Math.sin(M + D) + e * 0.004049d * Math.sin(Mpr - M + 2d * D)
            + 0.003996d * Math.sin(2d * Mpr + 2d * D) + 0.003862d * Math.sin(4d * D)
            + 0.003665d * Math.sin(2d * D - 3d * Mpr) + e * 0.002695d * Math.sin(2d * Mpr - M)
            + 0.002602d * Math.sin(Mpr - 2d * F - 2d * D) + e * 0.002396d * Math.sin(2d * D - M - 2d * Mpr)
            - 0.002349d * Math.sin(Mpr + D) + e * e * 0.002249d * Math.sin(2d * D - 2d * M)
            - e * 0.002125d * Math.sin(2d * Mpr + M) - e * e * 0.002079d * Math.sin(2d * M)
            + e * e * 0.002059d * Math.sin(2d * D - Mpr - 2d * M) - 0.001773d * Math.sin(Mpr + 2d * D - 2d * F)
            - 0.001595d * Math.sin(2d * F + 2d * D) + e * 0.001220d * Math.sin(4d * D - M - Mpr)
            - 0.001110d * Math.sin(2d * Mpr + 2d * F) + 0.000892d * Math.sin(Mpr - 3d * D)
            - e * 0.000811d * Math.sin(M + Mpr + 2d * D) + e * 0.000761d * Math.sin(4d * D - M - 2d * Mpr)
            + e * e * 0.000717d * Math.sin(Mpr - 2d * M) + e * e * 0.000704d * Math.sin(Mpr - 2d * M - 2d * D)
            + e * 0.000693d * Math.sin(M - 2d * Mpr + 2d * D) + e * 0.000598d * Math.sin(2d * D - M - 2d * F)
            + 0.000550d * Math.sin(Mpr + 4d * D) + 0.000538d * Math.sin(4d * Mpr)
            + e * 0.000521d * Math.sin(4d * D - M) + 0.000486d * Math.sin(2d * Mpr - D);

    /*              *eclongit = lambda;  */

    final double B = 5.128189d * Math.sin(F) + 0.280606d * Math.sin(Mpr + F) + 0.277693d * Math.sin(Mpr - F)
            + 0.173238d * Math.sin(2d * D - F) + 0.055413d * Math.sin(2d * D + F - Mpr)
            + 0.046272d * Math.sin(2d * D - F - Mpr) + 0.032573d * Math.sin(2d * D + F)
            + 0.017198d * Math.sin(2d * Mpr + F) + 0.009267d * Math.sin(2d * D + Mpr - F)
            + 0.008823d * Math.sin(2d * Mpr - F) + e * 0.008247d * Math.sin(2d * D - M - F)
            + 0.004323d * Math.sin(2d * D - F - 2d * Mpr) + 0.004200d * Math.sin(2d * D + F + Mpr)
            + e * 0.003372d * Math.sin(F - M - 2d * D) + 0.002472d * Math.sin(2d * D + F - M - Mpr)
            + e * 0.002222d * Math.sin(2d * D + F - M) + e * 0.002072d * Math.sin(2d * D - F - M - Mpr)
            + e * 0.001877d * Math.sin(F - M + Mpr) + 0.001828d * Math.sin(4d * D - F - Mpr)
            - e * 0.001803d * Math.sin(F + M) - 0.001750d * Math.sin(3d * F) + e * 0.001570d * Math.sin(Mpr - M - F)
            - 0.001487d * Math.sin(F + D) - e * 0.001481d * Math.sin(F + M + Mpr)
            + e * 0.001417d * Math.sin(F - M - Mpr) + e * 0.001350d * Math.sin(F - M)
            + 0.001330d * Math.sin(F - D) + 0.001106d * Math.sin(F + 3d * Mpr)
            + 0.001020d * Math.sin(4d * D - F) + 0.000833d * Math.sin(F + 4d * D - Mpr)
            + 0.000781d * Math.sin(Mpr - 3d * F) + 0.000670d * Math.sin(F + 4d * D - 2d * Mpr)
            + 0.000606d * Math.sin(2d * D - 3d * F) + 0.000597d * Math.sin(2d * D + 2d * Mpr - F)
            + e * 0.000492d * Math.sin(2d * D + Mpr - M - F) + 0.000450d * Math.sin(2d * Mpr - F - 2d * D)
            + 0.000439d * Math.sin(3d * Mpr - F) + 0.000423d * Math.sin(F + 2d * D + 2d * Mpr)
            + 0.000422d * Math.sin(2d * D - F - 3d * Mpr) - e * 0.000367d * Math.sin(M + F + 2d * D - Mpr)
            - e * 0.000353d * Math.sin(M + F + 2d * D) + 0.000331d * Math.sin(F + 4d * D)
            + e * 0.000317d * Math.sin(2d * D + F - M + Mpr) + e * e * 0.000306d * Math.sin(2d * D - 2d * M - F)
            - 0.000283d * Math.sin(Mpr + 3d * F);

    final double om1 = 0.0004664d * Math.cos(Om / Const.DEG_IN_RADIAN);
    final double om2 = 0.0000754d * Math.cos((Om + 275.05d - 2.30d * T) / Const.DEG_IN_RADIAN);

    double beta = B * (1d - om1 - om2);
    /*      *eclatit = beta; */

    final double pie = 0.950724d + 0.051818d * Math.cos(Mpr) + 0.009531d * Math.cos(2d * D - Mpr)
            + 0.007843d * Math.cos(2d * D) + 0.002824d * Math.cos(2d * Mpr) + 0.000857d * Math.cos(2d * D + Mpr)
            + e * 0.000533d * Math.cos(2d * D - M) + e * 0.000401d * Math.cos(2d * D - M - Mpr)
            + e * 0.000320d * Math.cos(Mpr - M) - 0.000271d * Math.cos(D) - e * 0.000264d * Math.cos(M + Mpr)
            - 0.000198d * Math.cos(2d * F - Mpr) + 0.000173d * Math.cos(3d * Mpr) + 0.000167d * Math.cos(4d * D - Mpr)
            - e * 0.000111d * Math.cos(M) + 0.000103d * Math.cos(4d * D - 2d * Mpr) - 0.000084d * Math.cos(2d * Mpr - 2d * D)
            - e * 0.000083d * Math.cos(2d * D + M) + 0.000079d * Math.cos(2d * D + 2d * Mpr) + 0.000072d * Math.cos(4d * D)
            + e * 0.000064d * Math.cos(2d * D - M + Mpr) - e * 0.000063d * Math.cos(2d * D + M - Mpr) + e * 0.000041d * Math.cos(M + D)
            + e * 0.000035d * Math.cos(2d * Mpr - M) - 0.000033d * Math.cos(3d * Mpr - 2d * D) - 0.000030d * Math.cos(Mpr + D)
            - 0.000029d * Math.cos(2d * F - 2d * D) - e * 0.000029d * Math.cos(2d * Mpr + M)
            + e * e * 0.000026d * Math.cos(2d * D - 2d * M) - 0.000023d * Math.cos(2d * F - 2d * D + Mpr) + e * 0.000019d * Math.cos(4d * D - M - Mpr);

    // System.out.printf("beta lambda %f %f",beta,lambda);
    beta /= Const.DEG_IN_RADIAN;
    lambda /= Const.DEG_IN_RADIAN;

    final double dist = 1d / Math.sin(pie / Const.DEG_IN_RADIAN);
//      System.out.printf("dist %f\n",dist);

    final double[] retvals = new double[6];
    retvals[2] = dist / Const.EARTHRAD_IN_AU;

    final double[] equatorial = Ecliptic.eclrot(jd, Math.cos(lambda) * Math.cos(beta), Math.sin(lambda) * Math.cos(beta), Math.sin(beta));
    retvals[3] = equatorial[0] * dist;
    retvals[4] = equatorial[1] * dist;
    retvals[5] = equatorial[2] * dist;

    retvals[0] = Math.atan2(equatorial[1], equatorial[0]) * Const.HRS_IN_RADIAN;
    retvals[1] = Math.asin(equatorial[2]) * Const.DEG_IN_RADIAN;

    return retvals;
  }

  public static double flmoon(int n, int nph) {
    /* Gives JD (+- 2 min) of phase nph during a given lunation n.
    Implements formulae in Meeus' Astronomical Formulae for Calculators,
    2nd Edn, publ. by Willman-Bell. */
    /* nph = 0 for new, 1 first qtr, 2 full, 3 last quarter. */

    double jd, cor;
    double M, Mpr, F;
    double T;
    double lun;

    lun = (double) n + (double) nph / 4.;
    T = lun / 1236.85;
    jd = 2415020.75933 + 29.53058868 * lun + 0.0001178 * T * T - 0.000000155 * T * T * T + 0.00033 * Math.sin((166.56 + 132.87 * T - 0.009173 * T * T) / Const.DEG_IN_RADIAN);
    M = 359.2242 + 29.10535608 * lun - 0.0000333 * T * T - 0.00000347 * T * T * T;
    M = M / Const.DEG_IN_RADIAN;
    Mpr = 306.0253 + 385.81691806 * lun + 0.0107306 * T * T + 0.00001236 * T * T * T;
    Mpr = Mpr / Const.DEG_IN_RADIAN;
    F = 21.2964 + 390.67050646 * lun - 0.0016528 * T * T - 0.00000239 * T * T * T;
    F = F / Const.DEG_IN_RADIAN;
    if ((nph == 0) || (nph == 2)) {/* new or full */
      cor = (0.1734 - 0.000393 * T) * Math.sin(M) + 0.0021 * Math.sin(2 * M) - 0.4068 * Math.sin(Mpr) + 0.0161 * Math.sin(2 * Mpr) - 0.0004 * Math.sin(3 * Mpr) + 0.0104 * Math.sin(2 * F) - 0.0051 * Math.sin(M + Mpr) - 0.0074 * Math.sin(M - Mpr) + 0.0004 * Math.sin(2 * F + M) - 0.0004 * Math.sin(2 * F - M) - 0.0006 * Math.sin(2 * F + Mpr) + 0.0010 * Math.sin(2 * F - Mpr) + 0.0005 * Math.sin(M + 2 * Mpr);
      jd += cor;
    } else {
      cor = (0.1721 - 0.0004 * T) * Math.sin(M) + 0.0021 * Math.sin(2 * M) - 0.6280 * Math.sin(Mpr) + 0.0089 * Math.sin(2 * Mpr) - 0.0004 * Math.sin(3 * Mpr) + 0.0079 * Math.sin(2 * F) - 0.0119 * Math.sin(M + Mpr) - 0.0047 * Math.sin(M - Mpr) + 0.0003 * Math.sin(2 * F + M) - 0.0004 * Math.sin(2 * F - M) - 0.0006 * Math.sin(2 * F + Mpr) + 0.0021 * Math.sin(2 * F - Mpr) + 0.0003 * Math.sin(M + 2 * Mpr) + 0.0004 * Math.sin(M - 2 * Mpr) - 0.0003 * Math.sin(2 * M + Mpr);
      if (nph == 1) {
        cor += 0.0028
                - 0.0004 * Math.cos(M) + 0.0003 * Math.cos(Mpr);
      }
      if (nph == 3) {
        cor += 0.0028
                + 0.0004 * Math.cos(M) - 0.0003 * Math.cos(Mpr);
      }
      jd += cor;

    }
    return jd;
  }

  public static int lunation(final double jd) {
    int nlast;
    double newjd;
    int kount = 0;

    nlast = (int) ((jd - 2415020.5d) / 29.5307d) - 1;

    nlast++;
    newjd = flmoon(nlast, 0);
    // increment lunations until you're sure the last and next new
    // moons bracket your input value.
    while ((newjd < jd) && (kount < 40)) {
      nlast++;
      kount++;
      newjd = flmoon(nlast, 0);
    }
    if (kount > 35) {
//      System.out.printf("didn't find lunation!\n");
    }
    return (nlast - 1);
  }

  public static String MoonPhaseDescr(final double jd) {

    int nlast, noctiles;
    double newjd, lastnewjd;
    double fqjd, fljd, lqjd;
    double x;

    nlast = lunation(jd);
    lastnewjd = flmoon(nlast, 0);
    x = jd - lastnewjd;
    noctiles = (int) (x / 3.69134);  // 1/8 month, truncated.
    if (noctiles == 0) {
      return String.format(Locale.ENGLISH, "%3.1f days since new moon.", x);
    } else if (noctiles <= 2) {  // nearest first quarter ...
      fqjd = flmoon(nlast, 1);
      x = jd - fqjd;
      if (x < 0d) {
        return String.format(Locale.ENGLISH, "%3.1f days before first quarter.", (-1d * x));
      } else {
        return String.format(Locale.ENGLISH, "%3.1f days after first quarter.", x);
      }
    } else if (noctiles <= 4) {  // nearest full ...
      fljd = flmoon(nlast, 2);
      x = jd - fljd;
      if (x < 0d) {
        return String.format(Locale.ENGLISH, "%3.1f days before full moon.", (-1d * x));
      } else {
        return String.format(Locale.ENGLISH, "%3.1f days after full moon.", x);
      }
    } else if (noctiles <= 6) {  // nearest last quarter ...
      lqjd = flmoon(nlast, 3);
      x = jd - lqjd;
      if (x < 0d) {
        return String.format(Locale.ENGLISH, "%3.1f days before last quarter.", (-1d * x));
      } else {
        return String.format(Locale.ENGLISH, "%3.1f days after last quarter.", x);
      }
    } else {
      newjd = flmoon(nlast + 1, 0);
      x = jd - newjd;
      return String.format(Locale.ENGLISH, "%3.1f days before new moon.", (-1d * x));
    }
  }
}
