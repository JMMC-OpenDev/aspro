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

  final static double jd1900 = 2415019.5;
  final static double[] dates = {1900., 1905., 1910., 1915., 1920., 1925., 1930.,
    1935., 1940., 1945., 1950., 1955., 1960., 1965., 1970., 1975., 1980.,
    1985., 1990., 1995., 2000., 2005., 2010., 2014.};
  // Updated 2012 Jan using http://stjarnhimlen.se/comp/time.html#deltat72p
  // 2010 value is real, 2014 is extrapolated.
  final static double[] delts = {-2.72, 3.86, 10.46, 17.20, 21.16, 23.62,
    24.02, 23.93, 24.33, 26.77, 29.15, 31.07, 33.15, 35.73, 40.18,
    45.48, 50.54, 54.34, 56.86, 60.78, 63.83, 64.68, 66.07, 67.7};

  /** 
   Given a julian date in 1900-2100, returns the correction
   delta t which is:
   TDT - UT (after 1983 and before 1998)
   ET - UT (before 1983)
   an extrapolated guess  (after 2014). 

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
  static double etcorr(final double jd) {

    double delt = 0d;

    final double year = 1900d + (jd - jd1900) * Const.DAY_IN_YEAR;

    if (year < 2014d && year >= 1900d) {
      final int i = (int) ((year - 1900d) / 5d);
      delt = delts[i] + ((delts[i + 1] - delts[i]) / (dates[i + 1] - dates[i])) * (year - dates[i]);

    } else if (year >= 2014d && year < 2100d) {
      delt = 67.7d + (1.2e-3d) * (jd - 2456658.5d);  /* rough extrapolation */

      /* Wide disagreement, but guessing 0.45 sec per year after 2014 (that's
       the JD given).  Using USNO MICA v2.2.1 and IERS versions, which
       disagree widely; this is closer to USNO MICA.  */

    } else if (year < 1900d) {
      // printf("etcorr ... no ephemeris time data for < 1900.%n");
      delt = 0d;
    } else if (year >= 2100d) {
      // printf("etcorr .. very long extrapolation in delta T - inaccurate.%n");
      delt = 180d; /* who knows? */
    }

    // System.out.printf("year %f, delta-T %f\n",year,delt);
    //  wow, this is called MANY times per update.
    return delt;
  }
}
