package edu.dartmouth;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
 modify it for their own purposes, provided that credit is given to the author
 in a prominent place.  For the present program that means that the green
 title and author banner appearing on the main window must not be removed,
 and may not be altered without premission of the author. */
public final class NightlyAlmanac {

    final boolean useInfinity;
    /** For finding timing of various phenomena, esp sun and moon rise and set. */
    WhenWhere midnight;
    WhenWhere sunrise;
    WhenWhere sunset;
    WhenWhere moonrise;
    WhenWhere moonset;
    WhenWhere sunsetTwilight18;
    WhenWhere sunriseTwilight18;
    WhenWhere sunsetTwilight12;
    WhenWhere sunriseTwilight12;
    WhenWhere sunsetTwilight06;
    WhenWhere sunriseTwilight06;
    WhenWhere nightcenter;

    double jdMidnight;

    /** finds the jd at which the sun is at altitude alt, given initial guess handed
     in with a whenwhere.  */
    static double jd_sun_alt(final double alt, final WhenWhere wIn) {
        double jdguess, lastjd;
        double deriv, err;
        final double del = 1e-3;
        double alt2, alt3;
        int i = 0;

        final WhenWhere w = wIn.clone();

        /* Set up calculation, then walk in with Newton-Raphson scheme (guess and
         check using numerical derivatives). */
        jdguess = w.when.jd;
        // System.out.printf("Before makelocalsun, w.when.jd %f%n",w.when.jd);
        w.makeLocalSun();
        alt2 = w.altsun;
        // System.out.printf("after alt2: w.when.jd %f alt2 %f%n",
        //                  w.when.jd,alt2);
        jdguess += del;
        w.changeWhen(jdguess);
        w.updateLocalSun();
        alt3 = w.altsun;
        err = alt3 - alt;
        deriv = (alt3 - alt2) / del;
        // System.out.printf("alt2 alt3 %f %f  err %f deriv %f%n",
        //             alt2,alt3,err,deriv);
        while ((Math.abs(err) > 1e-4d) && (i <= 10)) {
            lastjd = jdguess;
            alt2 = alt3;       // save last guess
            jdguess -= err / deriv;
            w.changeWhen(jdguess);
            w.updateLocalSun();
            alt3 = w.altsun;
            // System.out.printf("alt3 %f jdguess %f%n",alt3,jdguess);
            err = alt3 - alt;
            i++;
            deriv = (alt3 - alt2) / (jdguess - lastjd);
        }
        if (i >= 10) {
            // System.out.println("jd_sun_alt not converging.");
            jdguess = Double.NaN;
        }
        // System.out.println("jd_sun_alt iterations: " + i);
        // System.out.printf("Leaving sun w/ wIn %f w %f%n",wIn.when.jd,w.when.jd);
        return jdguess;
    }

    /**  finds the jd at which the moon is at altitude alt, given initial guess handed
     in with a whenwhere.  */
    static double jd_moon_alt(final double alt, final WhenWhere wIn) {
        double jdguess, lastjd;
        double deriv, err;
        final double del = 1e-3;
        double alt2, alt3;
        int i = 0;

        /* Set up calculation, then walk in with Newton-Raphson scheme (guess and
         check using numerical derivatives). */
        final WhenWhere w = wIn.clone();

        // System.out.printf("Into jd_moon_alt, target = %f%n",alt);
        jdguess = w.when.jd;
        w.makeLocalMoon();
        alt2 = w.altmoon;
        // System.out.printf("after alt2: w.when.jd %f alt2 %f%n",
        //              w.when.jd,alt2);
        jdguess += del;
        w.changeWhen(jdguess);
        w.updateLocalMoon();
        alt3 = w.altmoon;
        err = alt3 - alt;
        deriv = (alt3 - alt2) / del;
        // System.out.printf("alt2 alt3 %f %f  err %f deriv %f%n",
        //        alt2,alt3,err,deriv);
        while ((Math.abs(err) > 1e-4d) && (i <= 10)) {
            lastjd = jdguess;
            alt2 = alt3;       // save last guess
            jdguess -= err / deriv;
            w.changeWhen(jdguess);
            w.updateLocalMoon();
            alt3 = w.altmoon;
            // System.out.printf("alt3 %f jdguess %f ",alt3,jdguess,deriv);
            err = alt3 - alt;
            i++;
            deriv = (alt3 - alt2) / (jdguess - lastjd);
            // System.out.printf(" err %f deriv %f%n",err,deriv);
        }
        if (i >= 10) {
            // System.out.println("jd_moon_alt not converging.");
            jdguess = Double.NaN;
        }
        // System.out.println("jd_moon_alt iterations: " + i);
        // System.out.printf("Exiting with jdguess = %f%n%n",jdguess);
        return jdguess;
    }

    /** 
     * Computes rise, set, and twilight for the night nearest in time to wIn.when 
     * @param wIn WhenWhere input
     */
    NightlyAlmanac(final WhenWhere wIn) {
        this(wIn, false);
    }

    /** 
     * Computes rise, set, and twilight for the night nearest in time to wIn.when 
     * @param wIn WhenWhere input
     */
    NightlyAlmanac(final WhenWhere wIn, final boolean useInfinity) {
        this.useInfinity = useInfinity;

        /** Computes rise, set, and twilight for the night nearest in time to wIn.when */
        final WhenWhere w = new WhenWhere(wIn.when.clone(), wIn.where);

        // AAACK!  Have to clone EVERY WHEN, or they are all the SAME WHEN forever.
        // LBO: use clone() to clone when information but not where (site):
        midnight = w.clone();
        sunrise = w.clone();
        sunset = w.clone();
        moonrise = w.clone();
        moonset = w.clone();
        sunsetTwilight18 = w.clone();
        sunriseTwilight18 = w.clone();
        sunsetTwilight12 = w.clone();
        sunriseTwilight12 = w.clone();
        sunsetTwilight06 = w.clone();
        sunriseTwilight06 = w.clone();
        nightcenter = w.clone();

        Update(w);
    }

    void Update(final WhenWhere wIn) {
        final WhenWhere w = wIn.clone();

        // be sure the site information is up to date
        // LBO: use single site copy:
        w.where = wIn.where.clone();
        midnight.where = w.where;
        sunrise.where = w.where;
        sunset.where = w.where;
        moonrise.where = w.where;
        moonset.where = w.where;
        sunsetTwilight18.where = w.where;
        sunriseTwilight18.where = w.where;
        sunsetTwilight12.where = w.where;
        sunriseTwilight12.where = w.where;
        sunsetTwilight06.where = w.where;
        sunriseTwilight06.where = w.where;
        nightcenter.where = w.where;

        /* Computes rise, set, and twilight for the night nearest in time to wIn.when */
        double jdtemp;
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
        // System.out.printf("Entering almanac with local  %s%n",midnight.when.localDate.roundedCalString(0,0));
        if (midnight.when.localDate.timeofday.hour >= 12) {
            midnight.when.localDate.timeofday.hour = 23;
            midnight.when.localDate.timeofday.minute = 59;
            midnight.when.localDate.timeofday.second = 59.9999999999d;
        } else {
            midnight.when.localDate.timeofday.hour = 0;
            midnight.when.localDate.timeofday.minute = 0;
            midnight.when.localDate.timeofday.second = 0d;
        }
        jdtemp = midnight.when.localDate.cal2JD();
        // System.out.printf("jdtemp (local) = %f%n",jdtemp);

        midnight.when.setInstant(jdtemp, w.where.stdz, w.where.use_dst, false);
        // System.out.printf("translates to midnight.jd %f%n",midnight.when.jd);

        final double jdmid = midnight.when.jd;   // the real JD
        midnight.changeWhen(jdmid);        // to synch sidereal etc.
        // System.out.printf("Midnight set to %s%n", midnight.when.UTDate.roundedCalString(0,1));
        // System.out.printf("lst at midnight = %f%n",midnight.sidereal);

        midnight.updateLocalSun();
        midnight.updateLocalMoon();

        this.jdMidnight = jdmid;
        final double lat = midnight.where.lat.value;

        // See if the sun rises or sets ...
        computeSunRiseSet(midnight, lat, rise_set_alt, sunrise, sunset, 0.2d, 11.8d);
        // define night center:
        nightcenter.changeWhen(0.5d * (sunset.when.jd + sunrise.when.jd)); // may include +/-Inf !

        // Now let's do the same thing for astronomical twilight ...
        computeSunRiseSet(midnight, lat, twilight_alt_18, sunriseTwilight18, sunsetTwilight18, 0.2d, 11.8d);

        // Now let's do the same thing for nautical twilight ...
        computeSunRiseSet(midnight, lat, twilight_alt_12, sunriseTwilight12, sunsetTwilight12, 0.2d, 11.8d);

        // Now let's do the same thing for civil twilight ...
        computeSunRiseSet(midnight, lat, twilight_alt_06, sunriseTwilight06, sunsetTwilight06, 0.2d, 11.8d);

        // now we tackle the moon ... which is a bit harder.
        // The moon moves faster than the sun, so set more conservative limits for
        // proceeding with the calculation.
        computeMoonRiseSet(midnight, lat, rise_set_alt, moonrise, moonset, 1d, 11d);
    }

    private void computeSunRiseSet(final WhenWhere midnight,
                                   final double lat, final double rise_set_alt,
                                   final WhenWhere rise, final WhenWhere set,
                                   final double limitLow, final double limitHigh) {
        if (useInfinity) {
            computeSunRiseSet(midnight, lat, rise_set_alt, rise, set, limitLow, limitHigh, Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY);
        } else {
            final double jdmid = midnight.when.jd;   // the real JD
            computeSunRiseSet(midnight, lat, rise_set_alt, rise, set, limitLow, limitHigh, jdmid, jdmid + 0.5D);
        }
    }

    private static void computeSunRiseSet(final WhenWhere midnight,
                                          final double lat, final double rise_set_alt,
                                          final WhenWhere rise, final WhenWhere set,
                                          final double limitLow, final double limitHigh,
                                          final double valLower, final double valHigher) {

        final double jdmid = midnight.when.jd;   // the real JD

        WhenWhere ref = midnight;
        double deMid = ref.sun.topopos.delta.value;
        // System.out.println("deMid: " + deMid);
        //System.out.println("altSun: "+ ref.altsun);

        // See if the sun rises or sets ...
        double ha_rise = Spherical.ha_alt_Inf(deMid, lat, rise_set_alt);
        // System.out.printf("hourangle rise: %f%n", ha_rise);

        // Sun: Use altitude uncertainty = 0.5 degrees
        if ((ha_rise < limitLow || ha_rise > limitHigh) && Math.abs(ref.altsun - rise_set_alt) < 0.5) {
            // bad case: try -12H
            // System.out.println("bad case["+rise_set_alt+"]: " + ha_rise);
            // System.out.printf("mid set to %s%n", ref.when.UTDate.roundedCalString(0, 1));

            boolean rising = false;
            final double ha_rise_Mid = ha_rise;

            ref = midnight.clone();

            ref.changeWhen(jdmid - 0.5D); // to synch sidereal etc.
            // System.out.printf("ref set to %s%n", ref.when.UTDate.roundedCalString(0, 1));
            // System.out.printf("lst at ref = %f%n", ref.sidereal);

            ref.updateLocalSun();

            deMid = ref.sun.topopos.delta.value;
            // System.out.println("deMid-12: " + deMid);

            // See if the sun rises or sets ...
            ha_rise = Spherical.ha_alt_Inf(deMid, lat, rise_set_alt);
            // System.out.printf("hourangle rise: %f%n", ha_rise);

            if (ha_rise < limitLow || ha_rise > limitHigh) {

                if (ha_rise_Mid != ha_rise) {
                    // transition: always lower to always upper:
                    // System.out.println("transition between midnight and midnight - 12H");

                    rising = (ha_rise_Mid - ha_rise) > 0;

                    // half between [-12, 0]:
                    ha_rise = -6D;

                } else {
                    // bad case: try +12H
                    // System.out.println("bad case: " + ha_rise);

                    ref.changeWhen(jdmid + 0.5D); // to synch sidereal etc.
                    // System.out.printf("ref set to %s%n", ref.when.UTDate.roundedCalString(0, 1));
                    // System.out.printf("lst at ref = %f%n", ref.sidereal);

                    ref.updateLocalSun();

                    deMid = ref.sun.topopos.delta.value;
                    // System.out.println("deMid+12: " + deMid);

                    // See if the sun rises or sets ...
                    ha_rise = Spherical.ha_alt_Inf(deMid, lat, rise_set_alt);
                    // System.out.printf("hourangle rise: %f%n", ha_rise);

                    if (ha_rise < limitLow || ha_rise > limitHigh) {

                        if (ha_rise_Mid != ha_rise) {
                            // transition: always lower to always upper:
                            // System.out.println("transition between midnight and midnight + 12H");

                            rising = (ha_rise - ha_rise_Mid) > 0;

                            // half between [0, 12]:
                            ha_rise = 6D;
                        }
                    }
                }
            }

            // Valid ?
            if (ha_rise == 6D || ha_rise == -6D) {
                final double lstRef = ref.sidereal;
                final double raRef = ref.sun.topopos.alpha.value;

                final double dt = fixHA((raRef - lstRef) + ha_rise);

                ref.changeWhen(jdmid + dt * Const.HOUR_IN_DAY);
                ref.updateLocalSun();
                // System.out.printf("rise.when.jd %f, rise.altsun %f%n",rise.when.jd, rise.altsun);

                double jdtemp = jd_sun_alt(rise_set_alt, ref);
                // System.out.printf("ref transition = %f%n", jdtemp);

                if (Double.isNaN(jdtemp)) {
                    // revert back:
                    ref = midnight;
                    ha_rise = ha_rise_Mid;
                } else {
                    if (rising) {
                        rise.changeWhen(jdtemp);
                        // set set to -Inf to flag = inclusive range.
                        set.changeWhen(valLower);
                    } else {
                        // set rise to -Inf to flag = inclusive range.
                        rise.changeWhen(valLower);
                        set.changeWhen(jdtemp);
                    }
                    return;
                }
            }
        }

        if (ha_rise < limitHigh && ha_rise > limitLow) {
            final double lstMid = ref.sidereal;
            final double raMid = ref.sun.topopos.alpha.value;

            final double dtrise = fixHA((raMid - lstMid) - ha_rise);
            final double dtset = fixHA((raMid - lstMid) + ha_rise);

            rise.changeWhen(jdmid + dtrise * Const.HOUR_IN_DAY);
            rise.updateLocalSun();
            // System.out.printf("rise.when.jd %f, rise.altsun %f%n",rise.when.jd, rise.altsun);

            // note: NaN should not happen:
            double jdtemp = jd_sun_alt(rise_set_alt, rise);
            // System.out.printf("out, rise = %f%n",jdtemp);
            rise.changeWhen(jdtemp);

            set.changeWhen(jdmid + dtset * Const.HOUR_IN_DAY);
            set.updateLocalSun();

            // note: NaN should not happen:
            jdtemp = jd_sun_alt(rise_set_alt, set);
            // System.out.printf("out, set = %f%n",jdtemp);
            set.changeWhen(jdtemp);

        } else if (ha_rise < limitLow) {  // may not rise (never upper) ... set rise to +Inf to flag = empty range
            rise.changeWhen(valHigher);
            set.changeWhen(valHigher);
        } else { // may not set (always upper) ... set set to -Inf to flag = full range.
            // ha_rise >= limitHigh
            rise.changeWhen(valLower);
            set.changeWhen(valLower);
        }
    }

    private static void computeMoonRiseSet(final WhenWhere midnight,
                                           final double lat, final double rise_set_alt,
                                           final WhenWhere rise, final WhenWhere set,
                                           final double limitLow, final double limitHigh) {

        final double jdmid = midnight.when.jd;   // the real JD

        WhenWhere ref = midnight;
        double deMid = ref.moon.topopos.delta.value;
        // System.out.println("deMid: " + deMid);
        // System.out.println("altMoon: "+ ref.altmoon);

        // See if the moon rises or sets ...
        double ha_rise = Spherical.ha_alt_Inf(deMid, lat, rise_set_alt);
        // System.out.printf("hourangle rise: %f%n",ha_rise);

        // Moon: Use altitude uncertainty = 5 degrees
        if ((ha_rise < limitLow || ha_rise > limitHigh) && Math.abs(ref.altmoon - rise_set_alt) < 5.0) {
            // bad case: try -12H
            // System.out.println("bad case["+rise_set_alt+"]: " + ha_rise);
            // System.out.printf("mid set to %s%n", ref.when.UTDate.roundedCalString(0, 1));

            boolean rising = false;
            final double ha_rise_Mid = ha_rise;

            ref = midnight.clone();

            ref.changeWhen(jdmid - 0.5D); // to synch sidereal etc.
            // System.out.printf("ref set to %s%n", ref.when.UTDate.roundedCalString(0, 1));
            // System.out.printf("lst at ref = %f%n", ref.sidereal);

            ref.updateLocalMoon();

            deMid = ref.moon.topopos.delta.value;
            // System.out.println("deMid-12: " + deMid);

            // See if the moon rises or sets ...
            ha_rise = Spherical.ha_alt_Inf(deMid, lat, rise_set_alt);
            // System.out.printf("hourangle rise: %f%n", ha_rise);

            if (ha_rise < limitLow || ha_rise > limitHigh) {

                if (ha_rise_Mid != ha_rise) {
                    // transition: always lower to always upper:
                    // System.out.println("transition between midnight and midnight - 12H");

                    rising = (ha_rise_Mid - ha_rise) > 0;

                    // half between [-12, 0]:
                    ha_rise = -6D;

                } else {
                    // bad case: try +12H
                    // System.out.println("bad case: " + ha_rise);

                    ref.changeWhen(jdmid + 0.5D); // to synch sidereal etc.
                    // System.out.printf("ref set to %s%n", ref.when.UTDate.roundedCalString(0, 1));
                    // System.out.printf("lst at ref = %f%n", ref.sidereal);

                    ref.updateLocalMoon();

                    deMid = ref.moon.topopos.delta.value;
                    // System.out.println("deMid+12: " + deMid);

                    // See if the moon rises or sets ...
                    ha_rise = Spherical.ha_alt_Inf(deMid, lat, rise_set_alt);
                    // System.out.printf("hourangle rise: %f%n", ha_rise);

                    if (ha_rise < limitLow || ha_rise > limitHigh) {

                        if (ha_rise_Mid != ha_rise) {
                            // transition: always lower to always upper:
                            // System.out.println("transition between midnight and midnight + 12H");

                            rising = (ha_rise - ha_rise_Mid) > 0;

                            // half between [0, 12]:
                            ha_rise = 6D;
                        }
                    }
                }
            }

            // Valid ?
            if (ha_rise == 6D || ha_rise == -6D) {
                final double lstRef = ref.sidereal;
                final double raRef = ref.moon.topopos.alpha.value;

                final double dt = fixHA((raRef - lstRef) + ha_rise);

                ref.changeWhen(jdmid + dt * Const.HOUR_IN_DAY);
                ref.updateLocalMoon();
                // System.out.printf("rise.when.jd %f, rise.altmoon %f%n",rise.when.jd, rise.altmoon);

                double jdtemp = jd_moon_alt(rise_set_alt, ref);
                // System.out.printf("ref transition = %f%n", jdtemp);

                if (Double.isNaN(jdtemp)) {
                    // revert back:
                    ref = midnight;
                    ha_rise = ha_rise_Mid;
                } else {
                    if (rising) {
                        rise.changeWhen(jdtemp);
                        // set set to -Inf to flag = inclusive range.
                        set.changeWhen(Double.NEGATIVE_INFINITY);
                    } else {
                        // set rise to -Inf to flag = inclusive range.
                        rise.changeWhen(Double.NEGATIVE_INFINITY);
                        set.changeWhen(jdtemp);
                    }
                    return;
                }
            }
        }

        if (ha_rise < limitHigh && ha_rise > limitLow) {
            final double lstMid = ref.sidereal;
            final double raMid = ref.moon.topopos.alpha.value;

            final double dtrise = fixHA((raMid - lstMid) - ha_rise);
            final double dtset = fixHA((raMid - lstMid) + ha_rise);

            rise.changeWhen(jdmid + dtrise * Const.HOUR_IN_DAY);
            rise.updateLocalMoon();
            // System.out.printf("rise.when.jd %f, rise.altmoon %f%n",rise.when.jd, rise.altmoon);

            // note: NaN should not happen:
            double jdtemp = jd_moon_alt(rise_set_alt, rise);
            /*
             System.out.printf("ra moon midn    %s%n",midnight.moon.topopos.alpha.RoundedRAString(0,":"));
             System.out.printf("lst at midnight %f%n",lstMid);
             System.out.printf("rise-set HA %f%n",hamoonrise);
             System.out.printf("dtrise %f dtset %f%n",dtrise,dtset);
             */
            rise.changeWhen(jdtemp);

            set.changeWhen(jdmid + dtset * Const.HOUR_IN_DAY);
            set.updateLocalMoon();

            // note: NaN should not happen:
            jdtemp = jd_moon_alt(rise_set_alt, set);
            // System.out.printf("out, set = %f%n",jdtemp);
            set.changeWhen(jdtemp);

        } else if (ha_rise < limitLow) {   // may not rise ... set moonrise to +Inf to flag.
            rise.changeWhen(Double.POSITIVE_INFINITY);
            set.changeWhen(Double.POSITIVE_INFINITY);
        } else { // may not set ... set moonset to -Inf to flag.
            // ha_rise >= limitHigh
            rise.changeWhen(Double.NEGATIVE_INFINITY);
            set.changeWhen(Double.NEGATIVE_INFINITY);
        }
    }

    private static double fixHA(double in) {
        double ha = in;
        while (ha >= 12d) {
            ha -= 24d;
        }
        while (ha < -12d) {
            ha += 24d;
        }
        return ha;
    }
}
