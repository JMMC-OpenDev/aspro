package edu.dartmouth;

import java.util.HashMap;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
modify it for their own purposes, provided that credit is given to the author
in a prominent place.  For the present program that means that the green
title and author banner appearing on the main window must not be removed,
and may not be altered without premission of the author. */
public final class WhenWhere implements Cloneable {

  InstantInTime when;
  Site where;
  double sidereal;
  RA siderealobj;   // for output
  double[] barycenter = {0d, 0d, 0d, 0d, 0d, 0d};
  // Barycentric coords in epoch of date; 0-2 are XYZ, 3-5 are velocity.
  final double[] altazparSun = new double[3]; // LBO: tmp array
  Sun sun;
  HA hasun;
  double altsun, azsun;
  double twilight;
  final double[] altazparMoon = new double[3]; // LBO: tmp array
  Moon moon;    // mostly for use in rise-set calculations.
  HA hamoon;
  double altmoon, azmoon;
  double sunmoon;
  double moonillum;
  double cusppa;

  void dump() {
    System.out.printf("jd %f  Local %d %d %d  %d %d  UT %d %d %d  %d %d%n",
            when.jd, when.localDate.year, when.localDate.month, when.localDate.day,
            when.localDate.timeofday.hour, when.localDate.timeofday.minute,
            when.UTDate.year, when.UTDate.month, when.UTDate.day, when.UTDate.timeofday.hour,
            when.UTDate.timeofday.minute);
    System.out.printf("lst %f%n", sidereal);
  }

  WhenWhere(final InstantInTime t, final Site loc) {
    when = t;
    where = loc;
    sidereal = lstCalc(when.jd, where.longit.value);
    siderealobj = new RA(sidereal);
    makeLocalSun();
    makeLocalMoon();   // these always need instantiation to avoid trouble later.
  }

  WhenWhere(final double jdin, final Site loc) {
    this(jdin, loc, true);
  }

  /**
   * LAURENT : custom constructor to avoid making sun / moon anyway
   * @param jdIn
   * @param loc
   * @param makeLocalSunMoon
   */
  WhenWhere(final double jdIn, final Site loc, final boolean makeLocalSunMoon) {
    when = new InstantInTime(jdIn, loc.stdz, loc.use_dst, true);
    where = loc;
    sidereal = lstCalc(when.jd, where.longit.value);
    siderealobj = new RA(sidereal);
    if (makeLocalSunMoon) {
      makeLocalSun();
      makeLocalMoon();   // these always need instantiation to avoid trouble later.
    }
  }

  void changeWhen(final double jdin) {
    when.setInstant(jdin, where.stdz, where.use_dst, true);
    sidereal = lstCalc(when.jd, where.longit.value);
    siderealobj.setRA(sidereal);
  }

  void changeWhen(final String s, final boolean is_ut) {
    when.setInstant(s, where.stdz, where.use_dst, is_ut);
    sidereal = lstCalc(when.jd, where.longit.value);
    siderealobj.setRA(sidereal);
  }

  void setToNow() {
    when.setInstant(where.stdz, where.use_dst);
    sidereal = lstCalc(when.jd, where.longit.value);
    siderealobj.setRA(sidereal);
  }

  void changeSite(final HashMap<String, Site> hash, final String s) {
    // changing site involves synching sidereal and local time
    // so test to see if these are needed.
    final Site ss = hash.get(s);
    if (where.equals(ss) == false) {  // there's an equals method for this now ...
      // System.out.printf("Changing site ...%n");
      where = ss;
      // System.out.printf("Site changed, stdz = %f%n",where.stdz);
      sidereal = lstCalc(when.jd, where.longit.value);
      when.setInstant(when.jd, where.stdz, where.use_dst, true);
      siderealobj.setRA(sidereal);
    }
    // otherwise do nothing.
    // else System.out.printf("Not changing site ...%n");
  }

  void advanceWhen(final String s) {
    when.advanceTime(s, where.stdz, where.use_dst);
    sidereal = lstCalc(when.jd, where.longit.value);
    siderealobj.setRA(sidereal);
  }

  void advanceWhen(final String s, final boolean forward) {
    when.advanceTime(s, where.stdz, where.use_dst, forward);
    sidereal = lstCalc(when.jd, where.longit.value);
    siderealobj.setRA(sidereal);
  }

  Celest zenith2000() {
    final Celest c = new Celest(sidereal, where.lat.value, when.julianEpoch());
    c.selfprecess(2000d);
    return c;
  }

  @Override
  public WhenWhere clone() {  // override clone method to make it public
    try {
      final WhenWhere copy = (WhenWhere) super.clone();   // this needs to be try/catch to make it work
      copy.when = when.clone();
// LBO: avoid cloning site all the time:
//      copy.where = where.clone();
      copy.siderealobj = siderealobj.clone();
      copy.sun = sun.clone();
      copy.hasun = hasun.clone();
      copy.moon = moon.clone();
      copy.hamoon = hamoon.clone();
      return copy;
    } catch (CloneNotSupportedException e) {
      throw new Error("This should never happen!\n");
    }
  }

  static double lstCalc(final double jdin, final double longitin) {
    final long jdintt = (long) jdin;
    final double jdfrac = jdin - jdintt;

    final double jdmid;
    final double ut;
    if (jdfrac < 0.5d) {
      jdmid = jdintt - 0.5d;
      ut = jdfrac + 0.5d;
    } else {
      jdmid = jdintt + 0.5d;
      ut = jdfrac - 0.5d;
    }
    final double tt = (jdmid - Const.J2000) / 36525d;
    double sid_g = (24110.54841d + 8640184.812866d * tt + 0.093104d * tt * tt - 6.2e-6d * tt * tt * tt) / 86400d;
    long sid_int = (long) sid_g;
    sid_g -= (double) sid_int;
    sid_g += Const.SID_RATE * ut - longitin / 24d;
    sid_int = (long) sid_g;
    sid_g = (sid_g - (double) sid_int) * 24d;
    if (sid_g < 0d) {
      sid_g += 24d;
    }
    return sid_g;
  }

  // Pass in a previously-computed planets and sun for this ...
  // Planets and sun are assumed up-to-date.
  void baryxyzvel(Planets p, Sun s) {

    int i;
    double[] geopos;

    // need sunvel now so get it ...

    //      System.out.printf("into baryxyzvel, jd = %f%n",when.jd);
    s.sunvel(when.jd);  // compute sun velocity  ...

//       System.out.printf("Helio Vxyz sun: %f %f %f  %f%n",
    //         s.xyzvel[0] * Const.KMS_AUDAY,
    //        s.xyzvel[1] * Const.KMS_AUDAY,
    //       s.xyzvel[2] * Const.KMS_AUDAY, when.jd);

    p.ComputeBaryCor(); // compute offset of barycenter from heliocenter

//       System.out.printf("sun xyz  %f %f %f%n",s.xyz[0],s.xyz[1],s.xyz[2]);
//       System.out.printf(" baryc   %f %f %f%n",p.barycor[0],p.barycor[1],p.barycor[2]);
    for (i = 0; i < 3; i++) {
      barycenter[i] = (-1d * s.xyz[i] - p.barycor[i])
              * Const.LIGHTSEC_IN_AU;
      barycenter[i + 3] = (-1d * s.xyzvel[i] - p.barycor[i + 3])
              * Const.KMS_AUDAY;
//          System.out.printf("pre-topo: %d   %f   %f%n",i,
//              barycenter[i] / Const.LIGHTSEC_IN_AU, barycenter[i+3]);
    }

    // add in the topocentric velocity ... note use of sidereal for longit

    geopos = Topo.Geocent(sidereal, where.lat.value, where.elevsea);

//       System.out.printf("Geopos: %f %f %f%n",geopos[0],geopos[1],geopos[2]);
//       System.out.printf("topo corrn vx vy %f %f%n",
//          Const.OMEGA_EARTH * geopos[1] * Const.EARTHRAD_IN_KM,
//          Const.OMEGA_EARTH * geopos[0] * Const.EARTHRAD_IN_KM);

    // rotation vel is vector omega crossed into posn vector

    barycenter[3] -= Const.OMEGA_EARTH * geopos[1] * Const.EARTHRAD_IN_KM;
    barycenter[4] += Const.OMEGA_EARTH * geopos[0] * Const.EARTHRAD_IN_KM;

  }

  void makeLocalSun() {
    //System.out.printf("Making a new sun, jd = %f%n",when.jd);
    sun = new Sun(this);
    //System.out.printf("Made sun, sidereal = %f%n",sidereal);
    hasun = new HA(sidereal - sun.topopos.alpha.value);
    Observation.altit(sun.topopos.delta.value, hasun.value, where.lat.value, altazparSun);
    altsun = altazparSun[0];
    azsun = altazparSun[1];
    twilight = SkyIllum.ztwilight(altsun);
    //System.out.printf("Made a new sun: %s alt %f%n",sun.topopos.checkstring(),altazpar[0]);
  }

  void updateLocalSun() {
    sun.update(when, where, sidereal);
    hasun = new HA(sidereal - sun.topopos.alpha.value);
    Observation.altit(sun.topopos.delta.value, hasun.value, where.lat.value, altazparSun);
    altsun = altazparSun[0];
    azsun = altazparSun[1];
    twilight = SkyIllum.ztwilight(altsun);
//       System.out.printf("Updated sun: %s %f%n",sun.topopos.checkstring(),altazpar[0]);
  }

  void makeLocalMoon() {
    moon = new Moon(this);
    hamoon = new HA(sidereal - moon.topopos.alpha.value);
    Observation.altit(moon.topopos.delta.value, hamoon.value, where.lat.value, altazparMoon);
    altmoon = altazparMoon[0];
    azmoon = altazparMoon[1];

//      System.out.printf("Made a new moon: %s HA %s alt %f%n",moon.topopos.checkstring(),
//          hamoon.roundedHAString(0,":"),altazpar[0]);
  }

  void updateLocalMoon() {
    moon.update(when, where, sidereal);
    hamoon = new HA(sidereal - moon.topopos.alpha.value);
    Observation.altit(moon.topopos.delta.value, hamoon.value, where.lat.value, altazparMoon);
    altmoon = altazparMoon[0];
    azmoon = altazparMoon[1];

//      System.out.printf("Updated the moon: %s HA %s alt %f%n",moon.topopos.checkstring(),
//          hamoon.roundedHAString(0,":"),altazpar[0]);
  }

  void computeSunMoon() {
    updateLocalSun();
    updateLocalMoon();
    final double[] retvals = Spherical.cuspPA(sun.topopos, moon.topopos);
    sunmoon = retvals[1];
    moonillum = 0.5d * (1d - Math.cos(sunmoon));
    sunmoon *= Const.DEG_IN_RADIAN;
    cusppa = retvals[0];  // radians ...
  }
}
