package edu.dartmouth;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
modify it for their own purposes, provided that credit is given to the author
in a prominent place.  For the present program that means that the green
title and author banner appearing on the main window must not be removed,
and may not be altered without premission of the author. */
/** holds only a static method that returns a rough ephemeris time
correction */
public final class DeltaT {

  final static double jd1900 = 2415019.5d;
  final static double[] dates = {1900d, 1905d, 1910d, 1915d, 1920d, 1925d, 1930d,
                                 1935d, 1940d, 1945d, 1950d, 1955d, 1960d, 1965d, 1970d, 1975d, 1980d,
                                 1985d, 1990d, 1995d, 2000d, 2004d};
  // 2004 is the last one tabulated in the 2006 almanac
  final static double[] delts = {-2.72d, 3.86d, 10.46d, 17.20d, 21.16d, 23.62d,
                                 24.02d, 23.93d, 24.33d, 26.77d, 29.15d, 31.07d, 33.15d, 35.73d, 40.18d,
                                 45.48d, 50.54d, 54.34d, 56.86d, 60.78d, 63.83d, 64.57d};

  static double etcorr(final double jd) {

    /* Given a julian date in 1900-2100, returns the correction
    delta t which is:
    TDT - UT (after 1983 and before 1998)
    ET - UT (before 1983)
    an extrapolated guess  (after 2001).

    For dates in the past (<= 2001 and after 1900) the value is linearly
    interpolated on 5-year intervals; for dates after the present,
    an extrapolation is used, because the true value of delta t
    cannot be predicted precisely.  Note that TDT is essentially the
    modern version of ephemeris time with a slightly cleaner
    definition.

    Where the algorithm shifts there will be a small (< 0.1 sec)
    discontinuity.  Also, the 5-year linear interpolation scheme can
    lead to errors as large as 0.5 seconds in some cases, though
    usually rather smaller.   One seldom has actual UT to work with anyway,
    since the commonly-used UTC is tied to TAI within an integer number
    of seconds.  */

    double delt = 0d;

    final double year = 1900d + (jd - jd1900) / 365.25d;

    if (year < 2004d && year >= 1900d) {
      final int i = (int) ((year - 1900d) / 5d);
      delt = delts[i] + ((delts[i + 1] - delts[i]) / (dates[i + 1] - dates[i])) * (year - dates[i]);
    } else if (year >= 2004d && year < 2100d) {
      delt = 31.69d + (2.164e-3d) * (jd - 2436935.4d);  /* rough extrapolation */
    } /* the 31.69 is adjusted to give 64.09 sec at the start of 2001. */ else if (year < 1900d) {
      // printf("etcorr ... no ephemeris time data for < 1900.\n");
      delt = 0d;
    } else if (year >= 2100d) {
      // printf("etcorr .. very long extrapolation in delta T - inaccurate.\n");
      delt = 180d; /* who knows? */
    }

    return delt;
  }
}
