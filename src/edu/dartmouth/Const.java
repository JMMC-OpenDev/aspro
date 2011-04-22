package edu.dartmouth;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
modify it for their own purposes, provided that credit is given to the author
in a prominent place.  For the present program that means that the green
title and author banner appearing on the main window must not be removed,
and may not be altered without premission of the author. */
public interface Const {
  /* some constants */

  public static final double J2000 = 2451545d; // the julian epoch 2000
  public static final double DEG_IN_RADIAN = 57.2957795130823d;
  public static final double HRS_IN_RADIAN = 3.81971863420549d;
  public static final double ARCSEC_IN_RADIAN = 206264.806247096d;
  public static final double PI_OVER_2 = 1.5707963267949d;
  public static final double PI = 3.14159265358979d;
  public static final double TWOPI = 6.28318530717959d;
  public static final double EARTHRAD_IN_AU = 23454.7910556298d; // earth radii in 1 AU
  public static final double EARTHRAD_IN_KM = 6378.1366d; // equatorial
  public static final double KMS_AUDAY = 1731.45683633d; // 1731 km/sec = 1 AU/d
  public static final double SPEED_OF_LIGHT = 299792.458d; // exact, km/s.
  public static final double SS_MASS = 1.00134198d; // solar system mass, M_sun
  public static final double ASTRO_UNIT = 1.4959787066e11d; // 1 AU in meters
  public static final double LIGHTSEC_IN_AU = 499.0047863852d; // 1 AU in SI meters
  public static final double OMEGA_EARTH = 7.292116e-5d; // inertial ang vel of earth
  public static final double SID_RATE = 1.0027379093d;  // sidereal/solar ratio
}
