package edu.dartmouth;

import net.jafama.FastMath;

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
    final double[] altazpar = new double[3]; // LBO: tmp array
    double altitude, azimuth, parallactic;
    double airmass;
    double barytcor, baryvcor;   // time and velocity corrections to barycenter
    double baryjd;
    // I'd been hauling around a separate sun and moon, but these are now
    // expunged ... they're in the WhenWhere.  But moonlight, moonobj, and sunobj
    // depend on the object coordinates so they are here:
    double moonlight;
    double moonobj; // angular sepn of moon from obj and sun from obj
/*  double sunobj;  // angular sepn of sun from obj */

    Observation(final WhenWhere wIn, final Celest celIn) {
        this(wIn, celIn, true);
    }

    /**
     * LBO: special constructor without cloning given instances
     * @param wIn
     * @param celIn 
     */
    Observation(final WhenWhere wIn, final Celest celIn, final boolean doClone) {
        w = (doClone) ? wIn.clone() : wIn;
        c = (doClone) ? celIn.clone() : celIn;
        ha = new HA(0d);
        computeSky();
    }
    
    void computeSky() {   // bare-bones updater
        // assumes WhenWhere w has been updated.
        current = c.precessed(w.when.julianEpoch());

        ha.setHA(w.sidereal - current.alpha.value);

        altit(current.delta.value, ha.value, w.where.lat.value, altazpar);

        altitude = altazpar[0];
        azimuth = altazpar[1];
        parallactic = altazpar[2];

        airmass = Spherical.true_airmass(altitude);
    }

    /**
     * LAURENT : custom computeSky implementation (optimized)
     * @param cosLat cosinus of site latitude
     * @param sinLat sinus of site latitude
     * @param cosDec cosinus of target declination
     * @param sinDec sinus of target declination
     */
    void computeSkyFast(final double cosLat, final double sinLat,
            final double cosDec, final double sinDec) {
        // assumes WhenWhere w has been updated.
        // do not precess current coordinates as JD does not change much during night:
        //  current = c.precessed(w.when.julianEpoch());

        ha.setHA(w.sidereal - current.alpha.value);

        altAz(cosLat, sinLat, cosDec, sinDec, ha.value, altazpar);

        altitude = altazpar[0];
        azimuth = altazpar[1];

        // parallactic angle and airmass are undefined:
        parallactic = 0d;
        airmass = 0d;
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

    /**
     * LBO: custom computeMoonSeparation implementation (optimized)
     */
    void computeMoonSeparation() {
        // assumes WhenWhere w has been updated.
        // do not precess current coordinates as JD does not change much during night:
        // current = c.precessed(w.when.julianEpoch());

        //System.out.printf("computeSunMoon %s (%f)%n",
        //       current.alpha.RoundedRAString(2," "),current.equinox);
        // compute only the moon position:
        w.updateLocalMoon();

        moonobj = Const.DEG_IN_RADIAN * Spherical.subtend(w.moon.topopos, current);

        // moonlight is undefined
        moonlight = 0d;
    }

    void computeSunMoon() {
        current = c.precessed(w.when.julianEpoch());
        //System.out.printf("computeSunMoon %s (%f)%n",
        //       current.alpha.RoundedRAString(2," "),current.equinox);
        w.computeSunMoon();   // the non-object related parts are all done here

//    sunobj = Const.DEG_IN_RADIAN * Spherical.subtend(w.sun.topopos, current);
        moonobj = Const.DEG_IN_RADIAN * Spherical.subtend(w.moon.topopos, current);

        moonlight = SkyIllum.lunskybright(w.sunmoon, moonobj, 0.172d, w.altmoon, altitude, w.moon.topopos.distance);
    }

    /**
     * Return altitude (deg), azimuth (deg), parallactic angle (deg)
     * @param decIn declination (deg)
     * @param hrangleIn hour angle (decimal hours)
     * @param latIn latitude (deg)
     * @param retval double[3] array = altitude (deg), azimuth (deg) and parallactic angle (deg)
     */
    static void altit(final double decIn, final double hrangleIn, final double latIn, final double[] retval) {
        final double dec = decIn * Const.RADIAN_IN_DEG;
        final double hrangle = hrangleIn * Const.RADIAN_IN_HRS;
        final double lat = latIn * Const.RADIAN_IN_DEG;

        final double cosdec = FastMath.cos(dec);
        final double sindec = FastMath.sin(dec);
        final double cosha = FastMath.cos(hrangle);
        final double sinha = FastMath.sin(hrangle);
        final double coslat = FastMath.cos(lat);
        final double sinlat = FastMath.sin(lat);

        final double x = Const.DEG_IN_RADIAN * FastMath.asin(cosdec * cosha * coslat + sindec * sinlat);
        // x is the altitude.
        final double y = sindec * coslat - cosdec * cosha * sinlat; /* due N comp. */
        final double z = -1d * cosdec * sinha; /* due east comp. */
        double az = FastMath.atan2(z, y);

        final double parang;

        if (cosdec != 0d) { // protect from divide by zero
            final double sinAz = FastMath.sin(az);
            // sine and cosine of parallactic angle
            final double sinp = -1d * sinAz * coslat / cosdec;
            /* spherical law of sines ... cosdec = sine of codec, coslat = sine of colatitude */
            final double cosp = -1d * FastMath.cos(az) * cosha - sinAz * sinha * sinlat;
            /* spherical law of cosines ... also transformed to local available variables. */
            parang = FastMath.atan2(sinp, cosp) * Const.DEG_IN_RADIAN;
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

    /**
     * Return altitude (deg) and azimuth (deg) only
     * @param cosLat cosinus of observatory latitude
     * @param sinLat sinus of observatory latitude
     * @param cosDec cosinus of target declination
     * @param sinDec sinus of target declination
     * @param ha hour angle (decimal hours)
     * @param retval double[3] array = altitude (deg), azimuth (deg)
     */
    static void altAz(final double cosLat, final double sinLat,
            final double cosDec, final double sinDec,
            final double ha,
            final double[] retval) {

        final double haRad = ha * Const.RADIAN_IN_HRS;

        final double cosha = FastMath.cos(haRad);
        final double sinha = FastMath.sin(haRad);

        final double x = Const.DEG_IN_RADIAN * FastMath.asin(cosDec * cosha * cosLat + sinDec * sinLat); // x is the altitude.
        final double y = sinDec * cosLat - cosDec * cosha * sinLat; /* due N comp. */
        final double z = -1d * cosDec * sinha; /* due east comp. */
        double az = FastMath.atan2(z, y);

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
        retval[2] = 0d;
    }

    void computeBary(final Planets p) {
        w.baryxyzvel(p, w.sun);  /* find the position and velocity of the
         observing site wrt the solar system barycent */
        double[] unitvec = current.cel_unitXYZ();
//      System.out.printf("Current: %s%n",current.checkstring());
        barytcor = 0d;
        baryvcor = 0d;
//      System.out.printf("Bary xyz %f %f %f%n",w.barycenter[0],
//             w.barycenter[1],w.barycenter[2]);
        for (int i = 0; i < 3; i++) {
            barytcor += unitvec[i] * w.barycenter[i];
            baryvcor += unitvec[i] * w.barycenter[i + 3];
        }
//      System.out.printf("Bary %f sec %f km/s ...%n",
//          barytcor, baryvcor);
        baryjd = w.when.jd + barytcor / 86400d;
    }
}
