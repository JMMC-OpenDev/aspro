package edu.dartmouth;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
modify it for their own purposes, provided that credit is given to the author
in a prominent place.  For the present program that means that the green
title and author banner appearing on the main window must not be removed,
and may not be altered without premission of the author. */
public final class NightlyAlmanac {

  /** For finding timing of various phenomena, esp sun and moon rise and set. */
  WhenWhere midnight;
  WhenWhere sunrise;
  WhenWhere sunset;
  WhenWhere moonrise;
  WhenWhere moonset;
  WhenWhere eveningTwilight18;
  WhenWhere morningTwilight18;
  WhenWhere eveningTwilight12;
  WhenWhere morningTwilight12;
  WhenWhere eveningTwilight06;
  WhenWhere morningTwilight06;
  WhenWhere nightcenter;

  static double jd_sun_alt(final double alt, final WhenWhere wIn) {
    /**  finds the jd at which the sun is at altitude alt, given initial guess handed
    in with a whenwhere.  */
    double jdguess, lastjd;
    double deriv, err;
    final double del = 0.002d;
    double alt2, alt3;
    int i = 0;

    final WhenWhere w = wIn.clone();

    /* Set up calculation, then walk in with Newton-Raphson scheme (guess and
    check using numerical derivatives). */

    jdguess = w.when.jd;
//      System.out.printf("Before makelocalsun, w.when.jd %f\n",w.when.jd);
    w.MakeLocalSun();
    alt2 = w.altsun;
//      System.out.printf("after alt2: w.when.jd %f alt2 %f\n",
    //                  w.when.jd,alt2);
    jdguess += del;
    w.ChangeWhen(jdguess);
    w.UpdateLocalSun();
    alt3 = w.altsun;
    err = alt3 - alt;
    deriv = (alt3 - alt2) / del;
//      System.out.printf("alt2 alt3 %f %f  err %f deriv %f\n",
    //             alt2,alt3,err,deriv);
    while ((Math.abs(err) > 0.02d) && (i < 10)) {
      lastjd = jdguess;
      alt2 = alt3;       // save last guess
      jdguess -= err / deriv;
      w.ChangeWhen(jdguess);
      w.UpdateLocalSun();
      alt3 = w.altsun;
//          System.out.printf("alt3 %f jdguess %f\n",alt3,jdguess);
      err = alt3 - alt;
      i++;
      deriv = (alt3 - alt2) / (jdguess - lastjd);
/*
      if (i == 9) {
        System.out.printf("jd_sun_alt not converging.\n");
      }
*/
    }
    if (i >= 9) {
      jdguess = -1000d;
    }
//      System.out.printf("Leaving sun w/ wIn %f w %f\n",wIn.when.jd,w.when.jd);
    return jdguess;
  }

  static double jd_moon_alt(final double alt, final WhenWhere wIn) {
    /**  finds the jd at which the moon is at altitude alt, given initial guess handed
    in with a whenwhere.  */
    double jdguess, lastjd;
    double deriv, err;
    final double del = 0.002d;
    double alt2, alt3;
    int i = 0;

    /* Set up calculation, then walk in with Newton-Raphson scheme (guess and
    check using numerical derivatives). */

    final WhenWhere w = wIn.clone();

    // System.out.printf("Into jd_moon_alt, target = %f\n",alt);
    jdguess = w.when.jd;
    w.MakeLocalMoon();
    alt2 = w.altmoon;
    // System.out.printf("after alt2: w.when.jd %f alt2 %f\n",
    //              w.when.jd,alt2);
    jdguess += del;
    w.ChangeWhen(jdguess);
    w.UpdateLocalMoon();
    alt3 = w.altmoon;
    err = alt3 - alt;
    deriv = (alt3 - alt2) / del;
    // System.out.printf("alt2 alt3 %f %f  err %f deriv %f\n",
    //        alt2,alt3,err,deriv);
    while ((Math.abs(err) > 0.02d) && (i < 10)) {
      lastjd = jdguess;
      alt2 = alt3;       // save last guess
      jdguess -= err / deriv;
      w.ChangeWhen(jdguess);
      w.UpdateLocalMoon();
      alt3 = w.altmoon;
      // System.out.printf("alt3 %f jdguess %f ",alt3,jdguess,deriv);
      err = alt3 - alt;
      i++;
      deriv = (alt3 - alt2) / (jdguess - lastjd);
      // System.out.printf(" err %f deriv %f\n",err,deriv);
/*
      if (i == 9) {
        System.out.printf("jd_moon_alt not converging.\n");
      }
*/
    }
    if (i >= 9) {
      jdguess = -1000d;
    }
    // System.out.printf("Exiting with jdguess = %f\n\n",jdguess);
    return jdguess;
  }

  /** Computes rise, set, and twilight for the night nearest in time to wIn.when */
  NightlyAlmanac(final WhenWhere wIn) {
    // sure to call MakeLocalSun() / MakeLocalMoon()
    final WhenWhere w = new WhenWhere(wIn.when.clone(), wIn.where);

    // instantiate these ...
    midnight = w.clone();

    // AAACK!  Have to clone EVERY WHEN, or they are all the SAME WHEN forever.

    // use clone to have quickly new instances where when/where are different and local sun / moon done

    sunrise = w.clone();
    sunset = w.clone();
    moonrise = w.clone();
    moonset = w.clone();
    eveningTwilight18 = w.clone();
    morningTwilight18 = w.clone();
    eveningTwilight12 = w.clone();
    morningTwilight12 = w.clone();
    eveningTwilight06 = w.clone();
    morningTwilight06 = w.clone();
    nightcenter = w.clone();

    Update(w);
  }

  void Update(final WhenWhere w) {
    /** Computes rise, set, and twilight for the night nearest in time to wIn.when */
    double dtrise, dtset;
    double jdtemp, jdnoon;
    final double twilight_alt_18 = -18d;  // the standard choice for solar altitude at twilight
    final double twilight_alt_12 = -12d;  // the standard choice for solar altitude at nautical twilight
    final double twilight_alt_06 = -6d;  // the standard choice for solar altitude at civil twilight

    // approx means you can't use a negative elevation for the obs to
    // compensate for higher terrain.

    // depression of the horizon in degrees
    final double horiz = Const.DEG_IN_RADIAN * Math.sqrt(2d * w.where.elevhoriz / (1000d * Const.EARTHRAD_IN_KM));
    final double rise_set_alt = -(0.83d + horiz);
    // upper limb of sun and moon rise and set when center of disk is about 50 arcmin
    // below horizon, mostly because of refraction.  Sun and moon are almost the same
    // angular size, and variation in refraction is much larger than ang. size variations.


    // Establish and set the nearest midnight -- previous if before local noon,
    // next midnight if after local noon.

    midnight.when = w.when.clone();
//      System.out.printf("Entering almanac with local  %s\n",midnight.when.localDate.RoundedCalString(0,0));
    if (midnight.when.localDate.timeofday.hour >= 12) {
      midnight.when.localDate.timeofday.hour = 23;
      midnight.when.localDate.timeofday.minute = 59;
      midnight.when.localDate.timeofday.second = 59.9999999999d;
    } else {
      midnight.when.localDate.timeofday.hour = 0;
      midnight.when.localDate.timeofday.minute = 0;
      midnight.when.localDate.timeofday.second = 0d;
    }
    jdtemp = midnight.when.localDate.Cal2JD();
    // System.out.printf("jdtemp (local) = %f\n",jdtemp);
    midnight.when.SetInstant(jdtemp, w.where.stdz, w.where.use_dst, false);
    // System.out.printf("translates to midnight.jd %f\n",midnight.when.jd);
    final double jdmid = midnight.when.jd;   // the real JD
    midnight.ChangeWhen(jdmid);        // to synch sidereal etc.
//      System.out.printf("Midnight set to %s\n", midnight.when.UTDate.RoundedCalString(0,1));
//      System.out.printf("lst at midnight = %f\n",midnight.sidereal);

    midnight.UpdateLocalSun();
    midnight.UpdateLocalMoon();

    // See if the sun rises or sets ...
    final double hasunrise = Spherical.ha_alt(midnight.sun.topopos.delta.value, midnight.where.lat.value, rise_set_alt);
//      System.out.printf("hourangle sunrise: %f\n",hasunrise);

    if (hasunrise < 11.8d && hasunrise > 0.2d) {
      // if sun grazes horizon, small changes in DEC may affect whether it actually
      // rises or sets.  Since DEC is computed at midnight, put a little pad on to
      // avoid non-convergent calculations.
      // sunrise = new WhenWhere(jdmid + hasunrise/24.,w.where);

      dtrise = midnight.sun.topopos.alpha.value - hasunrise - midnight.sidereal;

      while (dtrise >= 12d) {
        dtrise -= 24d;
      }
      while (dtrise < -12d) {
        dtrise += 24d;
      }

      dtset = midnight.sun.topopos.alpha.value + hasunrise - midnight.sidereal;

      while (dtset >= 12d) {
        dtset -= 24d;
      }
      while (dtset < -12d) {
        dtset += 24d;
      }

//         System.out.printf("going to jd_sun_alt with est sunrise = %f\n",jdmid + dtrise/24.);
      sunrise.ChangeWhen(jdmid + dtrise / 24d);
      sunrise.UpdateLocalSun();
//         System.out.printf("sunrise.when.jd %f, sunrise.altsun %f\n",sunrise.when.jd,
//             sunrise.altsun);
      jdtemp = jd_sun_alt(rise_set_alt, sunrise);
//         System.out.printf("out, sunrise = %f\n",jdtemp);
      sunrise.ChangeWhen(jdtemp);

//         System.out.printf("going to jd_sun_alt with est sunset = %f\n",jdmid + dtset/24.);
      sunset.ChangeWhen(jdmid + dtset / 24d);
      sunset.UpdateLocalSun();
      jdtemp = jd_sun_alt(rise_set_alt, sunset);
//         System.out.printf("out, sunset = %f\n",jdtemp);
      sunset.ChangeWhen(jdtemp);
//         System.out.printf("In NightlyAlmanac.Update, sunset set to:\n");
//         sunset.dump();
      nightcenter.ChangeWhen((sunset.when.jd + sunrise.when.jd) / 2d);
    } else if (hasunrise < 0.2d) {  // may not rise ... set sunrise to noontime to flag.
      if (midnight.when.localDate.timeofday.hour == 23) {
        jdnoon = jdmid - 0.5d;
      } else {
        jdnoon = jdmid + 0.5d;
      }
      sunrise.ChangeWhen(jdnoon);
      sunset.ChangeWhen(jdnoon);
      nightcenter.ChangeWhen(jdnoon);
    } else if (hasunrise >= 11.8d) { // may not set ... set sunset to midnight to flag.
      sunrise.ChangeWhen(jdmid);
      sunset.ChangeWhen(jdmid);
      nightcenter.ChangeWhen(jdmid);
    }

    // Now let's do the same thing for astronomical twilight ...
    final double hatwilight18 = Spherical.ha_alt(midnight.sun.topopos.delta.value, midnight.where.lat.value, twilight_alt_18);
    // System.out.printf("hourangle sunrise: %f\n",hasunrise);
    if (hatwilight18 < 11.8d && hatwilight18 > 0.2d) {

      dtrise = midnight.sun.topopos.alpha.value - hatwilight18 - midnight.sidereal;

      while (dtrise >= 12d) {
        dtrise -= 24d;
      }
      while (dtrise < -12d) {
        dtrise += 24d;
      }

      dtset = midnight.sun.topopos.alpha.value + hatwilight18 - midnight.sidereal;

      while (dtset >= 12d) {
        dtset -= 24d;
      }
      while (dtset < -12d) {
        dtset += 24d;
      }

      eveningTwilight18.ChangeWhen(jdmid + dtset / 24d);
      eveningTwilight18.UpdateLocalSun();
      jdtemp = jd_sun_alt(twilight_alt_18, eveningTwilight18);
      eveningTwilight18.ChangeWhen(jdtemp);

      morningTwilight18.ChangeWhen(jdmid + dtrise / 24d);
      morningTwilight18.UpdateLocalSun();
      jdtemp = jd_sun_alt(twilight_alt_18, morningTwilight18);
      morningTwilight18.ChangeWhen(jdtemp);

    } else if (hatwilight18 < 0.2d) {  // twilight may not begin ... set to noon to flag.
      if (midnight.when.localDate.timeofday.hour == 23) // this case will be rare.
      {
        jdnoon = jdmid - 0.5d;
      } else {
        jdnoon = jdmid + 0.5d;
      }
      morningTwilight18.ChangeWhen(jdnoon);
      eveningTwilight18.ChangeWhen(jdnoon);

    } else if (hatwilight18 >= 11.8d) { // twilight may not end (midsummer at high lat).
      morningTwilight18.ChangeWhen(jdmid);
      eveningTwilight18.ChangeWhen(jdmid);
      // flag -- set twilight to exactly midn.
    }

    // Now let's do the same thing for nautical twilight ...
    final double hatwilight12 = Spherical.ha_alt(midnight.sun.topopos.delta.value, midnight.where.lat.value, twilight_alt_12);
    // System.out.printf("hourangle sunrise: %f\n",hasunrise);
    if (hatwilight12 < 11.8d && hatwilight12 > 0.2d) {

      dtrise = midnight.sun.topopos.alpha.value - hatwilight12 - midnight.sidereal;

      while (dtrise >= 12d) {
        dtrise -= 24d;
      }
      while (dtrise < -12d) {
        dtrise += 24d;
      }

      dtset = midnight.sun.topopos.alpha.value + hatwilight12 - midnight.sidereal;

      while (dtset >= 12d) {
        dtset -= 24d;
      }
      while (dtset < -12d) {
        dtset += 24d;
      }

      eveningTwilight12.ChangeWhen(jdmid + dtset / 24d);
      eveningTwilight12.UpdateLocalSun();
      jdtemp = jd_sun_alt(twilight_alt_12, eveningTwilight12);
      eveningTwilight12.ChangeWhen(jdtemp);

      morningTwilight12.ChangeWhen(jdmid + dtrise / 24d);
      morningTwilight12.UpdateLocalSun();
      jdtemp = jd_sun_alt(twilight_alt_12, morningTwilight12);
      morningTwilight12.ChangeWhen(jdtemp);

    } else if (hatwilight12 < 0.2d) {  // twilight may not begin ... set to noon to flag.
      if (midnight.when.localDate.timeofday.hour == 23) // this case will be rare.
      {
        jdnoon = jdmid - 0.5d;
      } else {
        jdnoon = jdmid + 0.5d;
      }
      morningTwilight12.ChangeWhen(jdnoon);
      eveningTwilight12.ChangeWhen(jdnoon);

    } else if (hatwilight12 >= 11.8d) { // twilight may not end (midsummer at high lat).
      morningTwilight12.ChangeWhen(jdmid);
      eveningTwilight12.ChangeWhen(jdmid);
      // flag -- set twilight to exactly midn.
    }

    // Now let's do the same thing for twilight ...
    final double hatwilight06 = Spherical.ha_alt(midnight.sun.topopos.delta.value, midnight.where.lat.value, twilight_alt_06);
    // System.out.printf("hourangle sunrise: %f\n",hasunrise);
    if (hatwilight06 < 11.8d && hatwilight06 > 0.2d) {

      dtrise = midnight.sun.topopos.alpha.value - hatwilight06 - midnight.sidereal;

      while (dtrise >= 12d) {
        dtrise -= 24d;
      }
      while (dtrise < -12d) {
        dtrise += 24d;
      }

      dtset = midnight.sun.topopos.alpha.value + hatwilight06 - midnight.sidereal;

      while (dtset >= 12d) {
        dtset -= 24d;
      }
      while (dtset < -12d) {
        dtset += 24d;
      }

      eveningTwilight06.ChangeWhen(jdmid + dtset / 24d);
      eveningTwilight06.UpdateLocalSun();
      jdtemp = jd_sun_alt(twilight_alt_06, eveningTwilight06);
      eveningTwilight06.ChangeWhen(jdtemp);

      morningTwilight06.ChangeWhen(jdmid + dtrise / 24d);
      morningTwilight06.UpdateLocalSun();
      jdtemp = jd_sun_alt(twilight_alt_06, morningTwilight06);
      morningTwilight06.ChangeWhen(jdtemp);

    } else if (hatwilight06 < 0.2d) {  // twilight may not begin ... set to noon to flag.
      if (midnight.when.localDate.timeofday.hour == 23) // this case will be rare.
      {
        jdnoon = jdmid - 0.5d;
      } else {
        jdnoon = jdmid + 0.5d;
      }
      morningTwilight06.ChangeWhen(jdnoon);
      eveningTwilight06.ChangeWhen(jdnoon);

    } else if (hatwilight06 >= 11.8d) { // twilight may not end (midsummer at high lat).
      morningTwilight06.ChangeWhen(jdmid);
      eveningTwilight06.ChangeWhen(jdmid);
      // flag -- set twilight to exactly midn.
    }

    // now we tackle the moon ... which is a bit harder.

    final double hamoonrise = Spherical.ha_alt(midnight.moon.topopos.delta.value, midnight.where.lat.value, rise_set_alt);

    if (hamoonrise < 11d && hamoonrise > 1d) {
      // The moon moves faster than the sun, so set more conservative limits for
      // proceeding with the calculation.

      dtrise = midnight.moon.topopos.alpha.value - hamoonrise - midnight.sidereal;

      while (dtrise >= 12d) {
        dtrise -= 24d;
      }
      while (dtrise < -12d) {
        dtrise += 24d;
      }

      dtset = midnight.moon.topopos.alpha.value + hamoonrise - midnight.sidereal;

      while (dtset >= 12d) {
        dtset -= 24d;
      }
      while (dtset < -12d) {
        dtset += 24d;
      }
      /*
      System.out.printf("ra moon midn    %s\n",midnight.moon.topopos.alpha.RoundedRAString(0,":"));
      System.out.printf("lst at midnight %f\n",midnight.sidereal);
      System.out.printf("rise-set HA %f\n",hamoonrise);
      System.out.printf("dtrise %f dtset %f\n",dtrise,dtset);
       */

      moonrise.ChangeWhen(jdmid + dtrise / 24d);
      moonrise.UpdateLocalMoon();
      jdtemp = jd_moon_alt(rise_set_alt, moonrise);
      moonrise.ChangeWhen(jdtemp);

      moonset.ChangeWhen(jdmid + dtset / 24d);
      moonset.UpdateLocalMoon();
      jdtemp = jd_moon_alt(rise_set_alt, moonset);
      moonset.ChangeWhen(jdtemp);
    }
  }
}