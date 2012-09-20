package edu.dartmouth;

import java.util.Locale;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
modify it for their own purposes, provided that credit is given to the author
in a prominent place.  For the present program that means that the green
title and author banner appearing on the main window must not be removed,
and may not be altered without premission of the author. */
public final class Celest implements Cloneable {

  // A celestial coordinate, which knows its equinox.
  RA alpha;
  DEC delta;
  double equinox;
  double distance;  // not always used but sometimes useful
  double galat, galong;  //  galactic coords, in degrees.

  // To do:  Add an elaborate input parsing mechanism and overload
  // constructors like crazy.
  public Celest(final RA a, final DEC d, final double e) {
    alpha = a;
    delta = d;
    equinox = e;
    distance = 0d;
  }

  public Celest(final double r, final double d, final double e) {  // ra, DEC, and equinox
    // decimal hours, degr.
    alpha = new RA(r);
    delta = new DEC(d);
    equinox = e;
    distance = 0d;
  }

  public Celest(final double r, final double d, final double e, final double dist) {
    // ra, DEC, and equinox decimal hours, degr; dist in
    // undefined units.
    alpha = new RA(r);
    delta = new DEC(d);
    equinox = e;
    distance = dist;
  }

  public Celest(final String[] s) {  // ra, DEC, and equinox as separate strings
    alpha = new RA(s[0]);
    delta = new DEC(s[1]);
    equinox = Double.parseDouble(s[2]);
    distance = 0d;
  }

  public Celest(final String ras, final String decs, final String eqs) {
    // ra, DEC, and equinox as separate strings not in an array
    alpha = new RA(ras);
    delta = new DEC(decs);
    equinox = Double.parseDouble(eqs);
    distance = 0d;
  }

  public Celest(final String s) {  // colon-separated ra, DEC, equinox (3 string)
    final String[] fields = s.split("\\s+");  // whitespace
    alpha = new RA(fields[0]);
    delta = new DEC(fields[1]);
    equinox = Double.parseDouble(fields[2]);
    distance = 0d;
  }

  @Override
  public boolean equals(final Object arg) {
    // override (I hope) the Object equals method to use in at least one test later.
    if ((arg != null) && (arg instanceof Celest)) {
      Celest c = (Celest) arg;
      if (c.alpha.value == alpha.value && c.delta.value == delta.value
              && c.equinox == equinox) {
        return true;
      } else {
        return false;
      }
    } else {
      return false;
    }
  }

  void UpdateFromStrings(final String rastr, final String decstr, final String eqstr) {
    // updates a previously instantiated celest from three strings.
    alpha.setRA(rastr);
    delta.setDec(decstr);
    equinox = Double.parseDouble(eqstr);
  }

  @Override
  public Celest clone() {
    try {
      final Celest copy = (Celest) super.clone();
      copy.alpha = alpha.clone();
      copy.delta = delta.clone();
      return copy;
    } catch (CloneNotSupportedException e) {
      throw new Error("This should never happen!");
    }
  }

  String checkstring() {
    String outstr = alpha.roundedRAString(2, ":") + "  ";
    outstr += delta.roundedDecString(1, ":") + "  ";
    outstr += String.format(Locale.ENGLISH, "%6.1f", equinox);
    return outstr;
  }

  String shortstring() {
    String outstr = alpha.roundedRAString(1, ":") + "  ";
    outstr += delta.roundedDecString(0, ":") + "  ";
    //outstr = outstr + String.format(Locale.ENGLISH, "%6.1f",equinox);
    return outstr;
  }

  /** Given x, y, and z, returns {ra, DEC, distance}.  */
  static double[] xyzCel(double x, double y, double z) {
    final double mod = Math.sqrt(x * x + y * y + z * z);
    if (mod > 0d) {
      x /= mod;
      y /= mod;
      z /= mod;
    } else {
//      System.out.println("Zero modulus in xyzCel.");
      return new double[]{0d, 0d, 0d};
    }

    // System.out.printf("xyzCel: %f %f %f ->%n",x,y,z);
    double raout, decout;

    final double xy = Math.sqrt(x * x + y * y);
    if (xy < 1.0e-11d) {  // on the pole
      raout = 0d;      // ra is degenerate
      decout = Const.PI_OVER_2;
      if (z < 0d) {
        decout *= -1d;
      }
    } else {
      raout = Math.atan2(y, x) * Const.HRS_IN_RADIAN;
      decout = Math.asin(z) * Const.DEG_IN_RADIAN;
    }
    // System.out.printf("%f %f%n",raout,decout);
    return new double[]{raout, decout, mod};
    // it will sometimes be useful to have the dist.
  }

  /** Given instance variables, returns UNIT VECTOR {x,y,z}. */
  double[] cel_unitXYZ() {

    final double cosdec = Math.cos(delta.radians());

    return new double[]{
              Math.cos(alpha.radians()) * cosdec,
              Math.sin(alpha.radians()) * cosdec,
              Math.sin(delta.radians())
            };
  }

  /** generates a unit vector XYZ in equinox NewEquinox from the current
  alpha, delta, and equinox.  I believe these are IUA 1976 precession
  constants, which are not fully up-to-date but which are close
  enough for most puropses. */
  double[] precess(final double NewEquinox) {

    //System.out.printf("equinoxes %f %f%n",equinox,NewEquinox);
    final double ti = (equinox - 2000d) / 100d;
    final double tf = (NewEquinox - 2000d - 100d * ti) / 100d;

    final double zeta = (2306.2181d + 1.39656d * ti + 0.000139d * ti * ti) * tf
            + (0.30188d - 0.000344d * ti) * tf * tf + 0.017998d * tf * tf * tf;
    final double z = zeta + (0.79280d + 0.000410d * ti) * tf * tf + 0.000205d * tf * tf * tf;
    final double theta = (2004.3109d - 0.8533d * ti - 0.000217d * ti * ti) * tf - (0.42665d + 0.000217d * ti) * tf * tf - 0.041833d * tf * tf * tf;

    final double cosz = Math.cos(z / Const.ARCSEC_IN_RADIAN);
    final double coszeta = Math.cos(zeta / Const.ARCSEC_IN_RADIAN);
    final double costheta = Math.cos(theta / Const.ARCSEC_IN_RADIAN);
    final double sinz = Math.sin(z / Const.ARCSEC_IN_RADIAN);
    final double sinzeta = Math.sin(zeta / Const.ARCSEC_IN_RADIAN);
    final double sintheta = Math.sin(theta / Const.ARCSEC_IN_RADIAN);

    final double[][] p = new double[3][3];
    p[0][0] = coszeta * cosz * costheta - sinzeta * sinz;
    p[0][1] = -1d * sinzeta * cosz * costheta - coszeta * sinz;
    p[0][2] = -1d * cosz * sintheta;

    p[1][0] = coszeta * sinz * costheta + sinzeta * cosz;
    p[1][1] = -1d * sinzeta * sinz * costheta + coszeta * cosz;
    p[1][2] = -1d * sinz * sintheta;

    p[2][0] = coszeta * sintheta;
    p[2][1] = -1d * sinzeta * sintheta;
    p[2][2] = costheta;

    final double alphaRad = this.alpha.radians();
    final double deltaRad = this.delta.radians();

    final double[] orig = new double[]{
      Math.cos(deltaRad) * Math.cos(alphaRad),
      Math.cos(deltaRad) * Math.sin(alphaRad),
      Math.sin(deltaRad)
    };

    final double[] fin = new double[3];

    for (int i = 0, j = 0; i < 3; i++) {  // matrix multiplication
      fin[i] = 0d;
      //System.out.printf("orig[%d] = %f%n",i,orig[i]);
      for (j = 0; j < 3; j++) {
        //System.out.printf("%d%d: %f  ",i,j,p[i][j]);
        fin[i] += p[i][j] * orig[j];
      }
      //System.out.printf("%nfin[%d] = %f%n%n",i,fin[i]);
    }
    return xyzCel(fin[0], fin[1], fin[2]);
  }

  public void selfprecess(double newEquinox) {
    /** precesses a Celest in place. */
    double[] radecOut;
    radecOut = precess(newEquinox);
    alpha.setRA(radecOut[0]);
    delta.setDec(radecOut[1]);
    equinox = newEquinox;

  }

  public Celest precessed(final double newEquinox) {
    /** returns a new Celest precessed from this one. */
    final double[] radecOut = precess(newEquinox);
    // System.out.printf("radecOut %f %f%n",radecOut[0],radecOut[1]);
    return new Celest(radecOut[0], radecOut[1], newEquinox);
  }

  void galactic() {
    /** computes instance variables galong and galat.  Algorithm is
    rigorous. */
    Celest cel1950 = precessed(1950);
    double[] xyz;
    double[] xyzgal = {0d, 0d, 0d};
    double[] temp;

    double p11 = -0.066988739415,
            p12 = -0.872755765853,
            p13 = -0.483538914631,
            p21 = 0.492728466047,
            p22 = -0.450346958025,
            p23 = 0.744584633299,
            p31 = -0.867600811168,
            p32 = -0.188374601707,
            p33 = 0.460199784759;

    xyz = cel1950.cel_unitXYZ();

    xyzgal[0] = xyz[0] * p11 + xyz[1] * p12 + xyz[2] * p13;
    xyzgal[1] = xyz[0] * p21 + xyz[1] * p22 + xyz[2] * p23;
    xyzgal[2] = xyz[0] * p31 + xyz[1] * p32 + xyz[2] * p33;
    temp = xyzCel(xyzgal[0], xyzgal[1], xyzgal[2]);
    galong = temp[0] * 15d;
    while (galong < 0d) {
      galong += 360d;
    }
    galat = temp[1];
  }
}
