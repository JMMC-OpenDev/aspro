package edu.dartmouth;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
modify it for their own purposes, provided that credit is given to the author
in a prominent place.  For the present program that means that the green
title and author banner appearing on the main window must not be removed,
and may not be altered without premission of the author. */
public final class Observation implements Cloneable {

  WhenWhere w;
  Celest c;
  Celest current;
  HA ha;
  final double[] altazpar = new double[3];
  double altitude, azimuth, parallactic;
  double airmass;
  double barytcor, baryvcor;   // time and velocity corrections to barycenter
  double baryjd;
  // I'd been hauling around a separate sun and moon, but these are now
  // expunged ... they're in the WhenWhere.  But moonlight, moonobj, and sunobj
  // depend on the object coordinates so they are here:
  double moonlight;
  double moonobj, sunobj;    // angular sepn of moon from obj and sun from obj

  Observation(final WhenWhere wIn, final Celest celIn) {
    w = wIn.clone();
    c = celIn.clone();
    ha = new HA(0d);
    computeSky();
  }

  void computeSky() {   // bare-bones updater
    computeSky(true);
  }

  /**
   * LAURENT : custom computeSky to avoid computing true_airmass and precess target at every calls (same night)
   * @param doOptionalComputation true indicates to perform target precession and compute true_airmass
   */
  void computeSky(final boolean doOptionalComputation) {   // bare-bones updater
    // assumes WhenWhere w has been updated.
    if (doOptionalComputation) {
      current = c.precessed(w.when.julianEpoch());
    }
    ha.setHA(w.sidereal - current.alpha.value);
    altit(current.delta.value, ha.value, w.where.lat.value, altazpar);
    altitude = altazpar[0];
    azimuth = altazpar[1];
    parallactic = altazpar[2];

    // LAURENT : disable airmass computation
    if (doOptionalComputation) {
      airmass = Spherical.true_airmass(altitude);
    }
  }

  // Split off the sun, moon, barycenter etc. to save time -- they're
  // not always needed in every instance.
  @Override
  public Observation clone() {  // override clone method to make it public
    try {
      final Observation copy = (Observation) super.clone();   // this needs to be try/catch to make it work
      copy.w = w.clone();
      copy.c = c.clone();
      copy.current = current.clone();
      copy.ha = ha.clone();
      return copy;
    } catch (CloneNotSupportedException e) {
      throw new Error("This should never happen!\n");
    }
  }

  void computeSunMoon() {
    current = c.precessed(w.when.julianEpoch());
    //System.out.printf("computeSunMoon %s (%f)\n",
    //       current.alpha.RoundedRAString(2," "),current.equinox);
    w.computeSunMoon();   // the non-object related parts are all done here

    sunobj = Const.DEG_IN_RADIAN * Spherical.subtend(w.sun.topopos, current);
    moonobj = Const.DEG_IN_RADIAN * Spherical.subtend(w.moon.topopos, current);

    moonlight = SkyIllum.lunskybright(w.sunmoon, moonobj, 0.172d, w.altmoon, altitude, w.moon.topopos.distance);
  }

  /** returns altitiude (degr), azimuth (degrees), parallactic angle (degr) */
  static void altit(final double decIn, final double hrangleIn, final double latIn, final double[] retval) {
    final double dec = decIn / Const.DEG_IN_RADIAN;
    final double hrangle = hrangleIn / Const.HRS_IN_RADIAN;
    final double lat = latIn / Const.DEG_IN_RADIAN;

    final double cosdec = Math.cos(dec);
    final double sindec = Math.sin(dec);
    final double cosha = Math.cos(hrangle);
    final double sinha = Math.sin(hrangle);
    final double coslat = Math.cos(lat);
    final double sinlat = Math.sin(lat);

    final double x = Const.DEG_IN_RADIAN * Math.asin(cosdec * cosha * coslat + sindec * sinlat);
    // x is the altitude.
    final double y = sindec * coslat - cosdec * cosha * sinlat; /* due N comp. */
    final double z = -1d * cosdec * sinha; /* due east comp. */
    double az = Math.atan2(z, y);

    final double parang;

    if (cosdec != 0d) { // protect from divide by zero
      // sine and cosine of parallactic angle
      final double sinp = -1d * Math.sin(az) * coslat / cosdec;
      /* spherical law of sines ... cosdec = sine of codec, coslat = sine of colatitude */
      final double cosp = -1d * Math.cos(az) * cosha - Math.sin(az) * sinha * sinlat;
      /* spherical law of cosines ... also transformed to local available variables. */
      parang = Math.atan2(sinp, cosp) * Const.DEG_IN_RADIAN;
      /* library function gets the quadrant. */
    } else { // you're on the pole ...
      if (lat >= 0d) {
        parang = 180d;
      } else {
        parang = 0d;
      }
    }

    az *= Const.DEG_IN_RADIAN;
    while (az < 0d) {
      az += 360d;
    }
    while (az >= 360d) {
      az -= 360d;
    }

    // set results:
    retval[0] = x;
    retval[1] = az;
    retval[2] = parang;
  }

  void computeBary(final Planets p) {
    w.baryxyzvel(p, w.sun);  /* find the position and velocity of the
    observing site wrt the solar system barycent */
    double[] unitvec = current.cel_unitXYZ();
//      System.out.printf("Current: %s\n",current.checkstring());
    barytcor = 0d;
    baryvcor = 0d;
//      System.out.printf("Bary xyz %f %f %f \n",w.barycenter[0],
//             w.barycenter[1],w.barycenter[2]);
    for (int i = 0; i < 3; i++) {
      barytcor += unitvec[i] * w.barycenter[i];
      baryvcor += unitvec[i] * w.barycenter[i + 3];
    }
//      System.out.printf("Bary %f sec %f km/s ...\n",
//          barytcor, baryvcor);
    baryjd = w.when.jd + barytcor / 86400d;
  }
}
