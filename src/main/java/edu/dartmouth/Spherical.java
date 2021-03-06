package edu.dartmouth;

import net.jafama.FastMath;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
 modify it for their own purposes, provided that credit is given to the author
 in a prominent place.  For the present program that means that the green
 title and author banner appearing on the main window must not be removed,
 and may not be altered without premission of the author. */
/** container for several static spherical trig methods. */
public final class Spherical {

    // LBO: higher threshold to avoid too many precession calls:
    private final static double THRESHOLD_PRECESS_IN_DAY = 0.5d / 365.25d; // 12H

    /** angle in radians between two positions. */
    static double subtend(final Celest a, final Celest b) {
        final Celest bprime;

        if (Math.abs(a.equinox - b.equinox) > THRESHOLD_PRECESS_IN_DAY) {
            // System.out.println("moon/target delta equinox = " + Math.abs(a.equinox - b.equinox)); // moon
            bprime = b.precessed(a.equinox);
        } else {
            bprime = b;
        }

        final double[] aCart = a.cel_unitXYZ();
        final double[] bCart = bprime.cel_unitXYZ();

        final double dotproduct = aCart[0] * bCart[0] + aCart[1] * bCart[1] + aCart[2] * bCart[2];

        double theta = FastMath.acos(dotproduct);

        // if the angle is tiny, use a flat sky approximation away from
        // the poles to get a more accurate answer.
        if (theta < 1.0e-5d) {
            if (Math.abs(a.delta.radians()) < (Const.PI_OVER_2 - 0.001d) && Math.abs(bprime.delta.radians()) < (Const.PI_OVER_2 - 0.001d)) {
                final double dr = (bprime.alpha.radians() - a.alpha.radians())
                        * FastMath.cos(0.5d * (a.delta.radians() + bprime.delta.radians()));
                final double dd = bprime.delta.radians() - a.delta.radians();
                theta = Math.sqrt(dr * dr + dd * dd);

            }
        }
        return theta;
    }

    /** Given the positions of the sun and the moon, returns the position angle
     of the line connecting the moon's two cusps.  Ported from python. */
    static double[] cuspPA(final Celest s, final Celest m) {
        final double codecsun = (90d - s.delta.value) * Const.RADIAN_IN_DEG;
        final double codecmoon = (90d - m.delta.value) * Const.RADIAN_IN_DEG;
        double dra = s.alpha.value - m.alpha.value;
        while (dra < -12d) {
            dra += 24d;
        }
        while (dra >= 12d) {
            dra -= 24d;
        }
        dra *= Const.RADIAN_IN_HRS;

        // Spherical law of cosines gives moon-sun separation
        final double moonsun = FastMath.acos(FastMath.cos(codecsun) * FastMath.cos(codecmoon)
                + FastMath.sin(codecsun) * FastMath.sin(codecmoon) * FastMath.cos(dra));
        // spherical law of sines + law of cosines needed to get
        // sine and cosine of pa separately; this gets quadrant etc.!
        final double pasin = FastMath.sin(dra) * FastMath.sin(codecsun) / FastMath.sin(moonsun);
        final double pacos = (FastMath.cos(codecsun) - FastMath.cos(codecmoon) * FastMath.cos(moonsun))
                / (FastMath.sin(codecmoon) * FastMath.sin(moonsun));

        final double pa = FastMath.atan2(pasin, pacos);

//   print "pa of arc from moon to sun is %5.2f deg." % (pa * _skysub.DEG_IN_RADIAN)
        final double cusppa = pa - Const.PI_OVER_2;   // line of cusps ...
//     System.out.printf("cusppa = %f%n",cusppa);
        return new double[]{cusppa, moonsun};
    }

    /** returns the minimum and maximum altitudes (degrees) of an object at
     declination DEC as viewed from lat. returns +Inf if asin(>1) */
    static double[] min_max_alt(final double lat, final double dec) {
        /* translated straight from skycalc. */
        final double[] retvals = {0d, 0d};

        final double latrad = lat * Const.RADIAN_IN_DEG;
        final double decrad = dec * Const.RADIAN_IN_DEG;
        double x = FastMath.cos(decrad) * FastMath.cos(latrad) + FastMath.sin(decrad) * FastMath.sin(latrad);
        if (Math.abs(x) <= 1d) {
            retvals[1] = FastMath.asin(x) * Const.DEG_IN_RADIAN;
        } else {
            // System.out.printf("min_max_alt ... asin(>1)%n");
            retvals[1] = Double.POSITIVE_INFINITY;
        }

        x = FastMath.sin(decrad) * FastMath.sin(latrad) - FastMath.cos(decrad) * FastMath.cos(latrad);
        if (Math.abs(x) <= 1d) {
            retvals[0] = FastMath.asin(x) * Const.DEG_IN_RADIAN;
        } else {
            // System.out.printf("min_max_alt ... asin(>1)%n");
            retvals[0] = Double.POSITIVE_INFINITY;
        }

        return retvals;
    }

    /** Finds the hour angle at which an object at declination DEC is at altitude
     alt, as viewed from Latitude lat;  returns +Inf if always higher,
     -Inf if always lower. */
    static double ha_alt(final double dec, final double lat, final double alt) {
        return ha_alt(dec, lat, alt, -1000d, 1000d);
    }

    static double ha_alt_Inf(final double dec, final double lat, final double alt) {
        return ha_alt(dec, lat, alt, Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY);
    }

    private static double ha_alt(final double dec, final double lat, final double alt,
                                 final double valLower, final double valHigher) {
        final double[] minmax = min_max_alt(lat, dec);
        if (alt < minmax[0]) {
            return valHigher;   // always higher than asked
        }
        if (alt > minmax[1]) {
            return valLower;  // always lower than asked
        }
        // System.out.printf("DEC %f lat %f alt %f ...%n",DEC,lat,alt);
        final double codec = Const.PI_OVER_2 - dec * Const.RADIAN_IN_DEG;
        final double colat = Const.PI_OVER_2 - lat * Const.RADIAN_IN_DEG;
        final double coalt = Const.PI_OVER_2 - alt * Const.RADIAN_IN_DEG;
        final double x = (FastMath.cos(coalt) - FastMath.cos(codec) * FastMath.cos(colat)) / (FastMath.sin(codec) * FastMath.sin(colat));
        if (Math.abs(x) <= 1d) {
            return (FastMath.acos(x) * Const.HRS_IN_RADIAN);
        } else {
            // System.out.printf("Bad inverse trig in ha_alt ... acos(%f)%n", x);
            return valHigher;
        }
    }

    /* returns the true airmass for a given altitude (degrees).  Ported from C. */
    static double true_airmass(final double alt) {
        /* The expression used is based on a tabulation of the mean KPNO
         atmosphere given by C. M. Snell & A. M. Heiser, 1968,
         PASP, 80, 336.  They tabulated the airmass at 5 degr
         intervals from z = 60 to 85 degrees; I fit the data with
         a fourth order poly for (secz - airmass) as a function of
         (secz - 1) using the IRAF curfit routine, then adjusted the
         zeroth order term to force (secz - airmass) to zero at
         z = 0.  The poly fit is very close to the tabulated points
         (largest difference is 3.2e-4) and appears smooth.
         This 85-degree point is at secz = 11.47, so for secz > 12
         I just return secz - 1.5 ... about the largest offset
         properly determined. */
        double secz, seczmin1;
        int i, ord = 3;
        double[] coef = {2.879465e-3, 3.033104e-3, 1.351167e-3, -4.716679e-5};
        double result = 0d;

        if (alt <= 0d) {
            return (-1d);   /* out of range. */
        }
        secz = 1d / FastMath.sin(alt * Const.RADIAN_IN_DEG);
        seczmin1 = secz - 1d;
        if (secz > 12d) {
            return (secz - 1.5);   // approx at extreme airmass
        }
        for (i = ord; i >= 0; i--) {
            result = (result + coef[i]) * seczmin1;
        }
        result = secz - result;
        return result;
    }

    /** computes instance variables galong and galat.  Algorithm is
     rigorous. */
    static Celest gal2Cel(double galacticlongit, double galacticlatit) {
        double[] xyz = {0d, 0d, 0d};
        double[] xyzgal = {0d, 0d, 0d};

        double p11 = -0.066988739415,
                p12 = -0.872755765853,
                p13 = -0.483538914631,
                p21 = 0.492728466047,
                p22 = -0.450346958025,
                p23 = 0.744584633299,
                p31 = -0.867600811168,
                p32 = -0.188374601707,
                p33 = 0.460199784759;

        double galongitrad = galacticlongit * Const.RADIAN_IN_DEG;
        double galatitrad = galacticlatit * Const.RADIAN_IN_DEG;

        xyzgal[0] = FastMath.cos(galongitrad) * FastMath.cos(galatitrad);
        xyzgal[1] = FastMath.sin(galongitrad) * FastMath.cos(galatitrad);
        xyzgal[2] = FastMath.sin(galatitrad);
        // System.out.printf("Galactic xyz %f %f %f%n",xyzgal[0],xyzgal[1],xyzgal[2]);

        // for rotation matrices, inverse is the transpose, so ...
        xyz[0] = xyzgal[0] * p11 + xyzgal[1] * p21 + xyzgal[2] * p31;
        xyz[1] = xyzgal[0] * p12 + xyzgal[1] * p22 + xyzgal[2] * p32;
        xyz[2] = xyzgal[0] * p13 + xyzgal[1] * p23 + xyzgal[2] * p33;
        // System.out.printf("Equatorial xyz %f %f %f%n",xyz[0],xyz[1],xyz[2]);

        double[] retvals = Celest.xyzCel(xyz[0], xyz[1], xyz[2]);
        Celest cel = new Celest(retvals[0], retvals[1], 1950.);  // galactic are defined for 1950

        return cel;   // and precess elsehwere to whatever.
    }
    
    private Spherical() {
        // no-op
    }
}
