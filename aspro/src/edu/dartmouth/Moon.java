package edu.dartmouth;

import java.util.Locale;
import net.jafama.FastMath;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
 modify it for their own purposes, provided that credit is given to the author
 in a prominent place.  For the present program that means that the green
 title and author banner appearing on the main window must not be removed,
 and may not be altered without premission of the author. */
public final class Moon implements Cloneable {

    Celest geopos;
    Celest topopos;
    double[] retvals = new double[6]; // LBO: temporary results

    Moon(final double jd) {
        final double eq = InstantInTime.julianEpoch(jd);
        topopos = new Celest(0d, 0d, eq, 0d);  // not set
        computeMoon(jd, retvals, topopos.tmpVals());
        geopos = new Celest(retvals[0], retvals[1], eq, retvals[2]);
    }

    Moon(final WhenWhere w) {
        geopos = new Celest(0d, 0d, 0d, 0d);  // not set
        topopos = new Celest(0d, 0d, 0d, 0d);  // not set
        update(w.when, w.where, w.sidereal);
    }

    void update(final InstantInTime when, final Site where, final double sidereal) {
        computeMoon(when.jd, retvals, topopos.tmpVals());
        geopos.update(retvals[0], retvals[1], when.julianEpoch(), retvals[2]);
        Topo.topocorr(geopos, when, where, sidereal, topopos);
    }

    @Override
    public Moon clone() {
        try {
            final Moon copy = (Moon) super.clone();
            copy.geopos = geopos.clone();
            copy.topopos = topopos.clone();
            copy.retvals = new double[6];
            return copy;
        } catch (CloneNotSupportedException e) {
            throw new Error("This should never happen!");
        }
    }

    /** Rather accurate lunar
     ephemeris, from Jean Meeus' *Astronomical Formulae For Calculators*,
     pub. Willman-Bell.  Includes all the terms given there. */
    static void computeMoon(final double jdIn, final double[] retvals, final double[] equatorial) {

        final double jd = jdIn + DeltaT.etcorr(jdIn) / 86400d;
        /* approximate correction to ephemeris time */
        final double T = (jd - 2415020d) * Const.INV_CENTURY;   /* this based around 1900 ... */
        final double Tsq = T * T;
        final double Tcb = Tsq * T;

        double Lpr = 270.434164d + 481267.8831d * T - 0.001133d * Tsq + 0.0000019d * Tcb;
        double M = 358.475833d + 35999.0498d * T - 0.000150d * Tsq - 0.0000033d * Tcb;
        double Mpr = 296.104608d + 477198.8491d * T + 0.009192d * Tsq + 0.0000144d * Tcb;
        double D = 350.737486d + 445267.1142d * T - 0.001436d * Tsq + 0.0000019d * Tcb;
        double F = 11.250889d + 483202.0251d * T - 0.003211d * Tsq - 0.0000003d * Tcb;
        double Om = 259.183275d - 1934.1420d * T + 0.002078d * Tsq + 0.0000022d * Tcb;

        Lpr %= 360d;
        M %= 360d;
        Mpr %= 360d;
        D %= 360d;
        F %= 360d;
        Om %= 360d;

        double sinx = FastMath.sin((51.2d + 20.2d * T) * Const.RADIAN_IN_DEG);
        Lpr += 0.000233d * sinx;
        M -= 0.001778d * sinx;
        Mpr += 0.000817d * sinx;
        D += 0.002011d * sinx;

        sinx = 0.003964d * FastMath.sin((346.560d + 132.870d * T - 0.0091731d * Tsq) * Const.RADIAN_IN_DEG);

        Lpr += sinx;
        Mpr += sinx;
        D += sinx;
        F += sinx;

        sinx = FastMath.sin(Om * Const.RADIAN_IN_DEG);
        Lpr += 0.001964d * sinx;
        Mpr += 0.002541d * sinx;
        D += 0.001964d * sinx;
        F -= 0.024691d * sinx;
        F -= 0.004328d * FastMath.sin((Om + 275.05d - 2.30d * T) * Const.RADIAN_IN_DEG);

        final double e = 1d - 0.002495d * T - 0.00000752d * Tsq;

        M *= Const.RADIAN_IN_DEG;   /* these will all be arguments ... */
        Mpr *= Const.RADIAN_IN_DEG;
        D *= Const.RADIAN_IN_DEG;
        F *= Const.RADIAN_IN_DEG;

        double lambda = Lpr + 6.288750d * FastMath.sin(Mpr) + 1.274018d * FastMath.sin(2d * D - Mpr)
                + 0.658309d * FastMath.sin(2d * D) + 0.213616d * FastMath.sin(2d * Mpr) - e * 0.185596d * FastMath.sin(M)
                - 0.114336d * FastMath.sin(2d * F) + 0.058793d * FastMath.sin(2d * D - 2d * Mpr)
                + e * 0.057212d * FastMath.sin(2d * D - M - Mpr) + 0.053320d * FastMath.sin(2d * D + Mpr)
                + e * 0.045874d * FastMath.sin(2d * D - M) + e * 0.041024d * FastMath.sin(Mpr - M)
                - 0.034718d * FastMath.sin(D) - e * 0.030465d * FastMath.sin(M + Mpr) + 0.015326d * FastMath.sin(2d * D - 2d * F)
                - 0.012528d * FastMath.sin(2d * F + Mpr) - 0.010980d * FastMath.sin(2d * F - Mpr)
                + 0.010674d * FastMath.sin(4d * D - Mpr) + 0.010034d * FastMath.sin(3d * Mpr)
                + 0.008548d * FastMath.sin(4d * D - 2d * Mpr) - e * 0.007910d * FastMath.sin(M - Mpr + 2d * D)
                - e * 0.006783d * FastMath.sin(2d * D + M) + 0.005162d * FastMath.sin(Mpr - D)
                + e * 0.005000d * FastMath.sin(M + D) + e * 0.004049d * FastMath.sin(Mpr - M + 2d * D)
                + 0.003996d * FastMath.sin(2d * Mpr + 2d * D) + 0.003862d * FastMath.sin(4d * D)
                + 0.003665d * FastMath.sin(2d * D - 3d * Mpr) + e * 0.002695d * FastMath.sin(2d * Mpr - M)
                + 0.002602d * FastMath.sin(Mpr - 2d * F - 2d * D) + e * 0.002396d * FastMath.sin(2d * D - M - 2d * Mpr)
                - 0.002349d * FastMath.sin(Mpr + D) + e * e * 0.002249d * FastMath.sin(2d * D - 2d * M)
                - e * 0.002125d * FastMath.sin(2d * Mpr + M) - e * e * 0.002079d * FastMath.sin(2d * M)
                + e * e * 0.002059d * FastMath.sin(2d * D - Mpr - 2d * M) - 0.001773d * FastMath.sin(Mpr + 2d * D - 2d * F)
                - 0.001595d * FastMath.sin(2d * F + 2d * D) + e * 0.001220d * FastMath.sin(4d * D - M - Mpr)
                - 0.001110d * FastMath.sin(2d * Mpr + 2d * F) + 0.000892d * FastMath.sin(Mpr - 3d * D)
                - e * 0.000811d * FastMath.sin(M + Mpr + 2d * D) + e * 0.000761d * FastMath.sin(4d * D - M - 2d * Mpr)
                + e * e * 0.000717d * FastMath.sin(Mpr - 2d * M) + e * e * 0.000704d * FastMath.sin(Mpr - 2d * M - 2d * D)
                + e * 0.000693d * FastMath.sin(M - 2d * Mpr + 2d * D) + e * 0.000598d * FastMath.sin(2d * D - M - 2d * F)
                + 0.000550d * FastMath.sin(Mpr + 4d * D) + 0.000538d * FastMath.sin(4d * Mpr)
                + e * 0.000521d * FastMath.sin(4d * D - M) + 0.000486d * FastMath.sin(2d * Mpr - D);

        /*              *eclongit = lambda;  */

        final double B = 5.128189d * FastMath.sin(F) + 0.280606d * FastMath.sin(Mpr + F) + 0.277693d * FastMath.sin(Mpr - F)
                + 0.173238d * FastMath.sin(2d * D - F) + 0.055413d * FastMath.sin(2d * D + F - Mpr)
                + 0.046272d * FastMath.sin(2d * D - F - Mpr) + 0.032573d * FastMath.sin(2d * D + F)
                + 0.017198d * FastMath.sin(2d * Mpr + F) + 0.009267d * FastMath.sin(2d * D + Mpr - F)
                + 0.008823d * FastMath.sin(2d * Mpr - F) + e * 0.008247d * FastMath.sin(2d * D - M - F)
                + 0.004323d * FastMath.sin(2d * D - F - 2d * Mpr) + 0.004200d * FastMath.sin(2d * D + F + Mpr)
                + e * 0.003372d * FastMath.sin(F - M - 2d * D) + 0.002472d * FastMath.sin(2d * D + F - M - Mpr)
                + e * 0.002222d * FastMath.sin(2d * D + F - M) + e * 0.002072d * FastMath.sin(2d * D - F - M - Mpr)
                + e * 0.001877d * FastMath.sin(F - M + Mpr) + 0.001828d * FastMath.sin(4d * D - F - Mpr)
                - e * 0.001803d * FastMath.sin(F + M) - 0.001750d * FastMath.sin(3d * F) + e * 0.001570d * FastMath.sin(Mpr - M - F)
                - 0.001487d * FastMath.sin(F + D) - e * 0.001481d * FastMath.sin(F + M + Mpr)
                + e * 0.001417d * FastMath.sin(F - M - Mpr) + e * 0.001350d * FastMath.sin(F - M)
                + 0.001330d * FastMath.sin(F - D) + 0.001106d * FastMath.sin(F + 3d * Mpr)
                + 0.001020d * FastMath.sin(4d * D - F) + 0.000833d * FastMath.sin(F + 4d * D - Mpr)
                + 0.000781d * FastMath.sin(Mpr - 3d * F) + 0.000670d * FastMath.sin(F + 4d * D - 2d * Mpr)
                + 0.000606d * FastMath.sin(2d * D - 3d * F) + 0.000597d * FastMath.sin(2d * D + 2d * Mpr - F)
                + e * 0.000492d * FastMath.sin(2d * D + Mpr - M - F) + 0.000450d * FastMath.sin(2d * Mpr - F - 2d * D)
                + 0.000439d * FastMath.sin(3d * Mpr - F) + 0.000423d * FastMath.sin(F + 2d * D + 2d * Mpr)
                + 0.000422d * FastMath.sin(2d * D - F - 3d * Mpr) - e * 0.000367d * FastMath.sin(M + F + 2d * D - Mpr)
                - e * 0.000353d * FastMath.sin(M + F + 2d * D) + 0.000331d * FastMath.sin(F + 4d * D)
                + e * 0.000317d * FastMath.sin(2d * D + F - M + Mpr) + e * e * 0.000306d * FastMath.sin(2d * D - 2d * M - F)
                - 0.000283d * FastMath.sin(Mpr + 3d * F);

        final double om1 = 0.0004664d * FastMath.cos(Om * Const.RADIAN_IN_DEG);
        final double om2 = 0.0000754d * FastMath.cos((Om + 275.05d - 2.30d * T) * Const.RADIAN_IN_DEG);

        double beta = B * (1d - om1 - om2);
        /*      *eclatit = beta; */

        final double pie = 0.950724d + 0.051818d * FastMath.cos(Mpr) + 0.009531d * FastMath.cos(2d * D - Mpr)
                + 0.007843d * FastMath.cos(2d * D) + 0.002824d * FastMath.cos(2d * Mpr) + 0.000857d * FastMath.cos(2d * D + Mpr)
                + e * 0.000533d * FastMath.cos(2d * D - M) + e * 0.000401d * FastMath.cos(2d * D - M - Mpr)
                + e * 0.000320d * FastMath.cos(Mpr - M) - 0.000271d * FastMath.cos(D) - e * 0.000264d * FastMath.cos(M + Mpr)
                - 0.000198d * FastMath.cos(2d * F - Mpr) + 0.000173d * FastMath.cos(3d * Mpr) + 0.000167d * FastMath.cos(4d * D - Mpr)
                - e * 0.000111d * FastMath.cos(M) + 0.000103d * FastMath.cos(4d * D - 2d * Mpr) - 0.000084d * FastMath.cos(2d * Mpr - 2d * D)
                - e * 0.000083d * FastMath.cos(2d * D + M) + 0.000079d * FastMath.cos(2d * D + 2d * Mpr) + 0.000072d * FastMath.cos(4d * D)
                + e * 0.000064d * FastMath.cos(2d * D - M + Mpr) - e * 0.000063d * FastMath.cos(2d * D + M - Mpr) + e * 0.000041d * FastMath.cos(M + D)
                + e * 0.000035d * FastMath.cos(2d * Mpr - M) - 0.000033d * FastMath.cos(3d * Mpr - 2d * D) - 0.000030d * FastMath.cos(Mpr + D)
                - 0.000029d * FastMath.cos(2d * F - 2d * D) - e * 0.000029d * FastMath.cos(2d * Mpr + M)
                + e * e * 0.000026d * FastMath.cos(2d * D - 2d * M) - 0.000023d * FastMath.cos(2d * F - 2d * D + Mpr) + e * 0.000019d * FastMath.cos(4d * D - M - Mpr);

        // System.out.printf("beta lambda %f %f",beta,lambda);
        beta *= Const.RADIAN_IN_DEG;
        lambda *= Const.RADIAN_IN_DEG;

        final double dist = 1d / FastMath.sin(pie * Const.RADIAN_IN_DEG);
//      System.out.printf("dist %f%n",dist);

        // Define results:
        Ecliptic.eclrot(jd, FastMath.cos(lambda) * FastMath.cos(beta), FastMath.sin(lambda) * FastMath.cos(beta), FastMath.sin(beta), equatorial);

        retvals[2] = dist * Const.AU_IN_EARTHRAD;

        retvals[3] = equatorial[0] * dist;
        retvals[4] = equatorial[1] * dist;
        retvals[5] = equatorial[2] * dist;

        retvals[0] = FastMath.atan2(equatorial[1], equatorial[0]) * Const.HRS_IN_RADIAN;
        retvals[1] = FastMath.asin(equatorial[2]) * Const.DEG_IN_RADIAN;
    }

    public static double flmoon(int n, int nph) {
        /* Gives JD (+- 2 min) of phase nph during a given lunation n.
         Implements formulae in Meeus' Astronomical Formulae for Calculators,
         2nd Edn, publ. by Willman-Bell. */
        /* nph = 0 for new, 1 first qtr, 2 full, 3 last quarter. */

        double jd, cor;
        double M, Mpr, F;
        double T;
        double lun;

        lun = (double) n + (double) nph / 4.;
        T = lun / 1236.85;
        jd = 2415020.75933 + 29.53058868 * lun + 0.0001178 * T * T - 0.000000155 * T * T * T + 0.00033 * FastMath.sin((166.56 + 132.87 * T - 0.009173 * T * T) * Const.RADIAN_IN_DEG);
        M = 359.2242 + 29.10535608 * lun - 0.0000333 * T * T - 0.00000347 * T * T * T;
        M = M * Const.RADIAN_IN_DEG;
        Mpr = 306.0253 + 385.81691806 * lun + 0.0107306 * T * T + 0.00001236 * T * T * T;
        Mpr = Mpr * Const.RADIAN_IN_DEG;
        F = 21.2964 + 390.67050646 * lun - 0.0016528 * T * T - 0.00000239 * T * T * T;
        F = F * Const.RADIAN_IN_DEG;
        if ((nph == 0) || (nph == 2)) {/* new or full */
            cor = (0.1734 - 0.000393 * T) * FastMath.sin(M) + 0.0021 * FastMath.sin(2 * M) - 0.4068 * FastMath.sin(Mpr) + 0.0161 * FastMath.sin(2 * Mpr) - 0.0004 * FastMath.sin(3 * Mpr) + 0.0104 * FastMath.sin(2 * F) - 0.0051 * FastMath.sin(M + Mpr) - 0.0074 * FastMath.sin(M - Mpr) + 0.0004 * FastMath.sin(2 * F + M) - 0.0004 * FastMath.sin(2 * F - M) - 0.0006 * FastMath.sin(2 * F + Mpr) + 0.0010 * FastMath.sin(2 * F - Mpr) + 0.0005 * FastMath.sin(M + 2 * Mpr);
            jd += cor;
        } else {
            cor = (0.1721 - 0.0004 * T) * FastMath.sin(M) + 0.0021 * FastMath.sin(2 * M) - 0.6280 * FastMath.sin(Mpr) + 0.0089 * FastMath.sin(2 * Mpr) - 0.0004 * FastMath.sin(3 * Mpr) + 0.0079 * FastMath.sin(2 * F) - 0.0119 * FastMath.sin(M + Mpr) - 0.0047 * FastMath.sin(M - Mpr) + 0.0003 * FastMath.sin(2 * F + M) - 0.0004 * FastMath.sin(2 * F - M) - 0.0006 * FastMath.sin(2 * F + Mpr) + 0.0021 * FastMath.sin(2 * F - Mpr) + 0.0003 * FastMath.sin(M + 2 * Mpr) + 0.0004 * FastMath.sin(M - 2 * Mpr) - 0.0003 * FastMath.sin(2 * M + Mpr);
            if (nph == 1) {
                cor += 0.0028
                        - 0.0004 * FastMath.cos(M) + 0.0003 * FastMath.cos(Mpr);
            }
            if (nph == 3) {
                cor += 0.0028
                        + 0.0004 * FastMath.cos(M) - 0.0003 * FastMath.cos(Mpr);
            }
            jd += cor;

        }
        return jd;
    }

    public static int lunation(final double jd) {
        int nlast;
        double newjd;
        int kount = 0;

        nlast = (int) ((jd - 2415020.5d) / 29.5307d) - 1;

        nlast++;
        newjd = flmoon(nlast, 0);
        // increment lunations until you're sure the last and next new
        // moons bracket your input value.
        while ((newjd < jd) && (kount < 40)) {
            nlast++;
            kount++;
            newjd = flmoon(nlast, 0);
        }
        if (kount > 35) {
//      System.out.printf("didn't find lunation!%n");
        }
        return (nlast - 1);
    }

    public static String getPhaseDescription(final double jd) {

        int nlast, noctiles;
        double newjd, lastnewjd;
        double fqjd, fljd, lqjd;
        double x;

        nlast = lunation(jd);
        lastnewjd = flmoon(nlast, 0);
        x = jd - lastnewjd;
        noctiles = (int) (x / 3.69134);  // 1/8 month, truncated.
        if (noctiles == 0) {
            return String.format(Locale.ENGLISH, "%3.1f days since new moon.", x);
        } else if (noctiles <= 2) {  // nearest first quarter ...
            fqjd = flmoon(nlast, 1);
            x = jd - fqjd;
            if (x < 0d) {
                return String.format(Locale.ENGLISH, "%3.1f days before first quarter.", (-1d * x));
            } else {
                return String.format(Locale.ENGLISH, "%3.1f days after first quarter.", x);
            }
        } else if (noctiles <= 4) {  // nearest full ...
            fljd = flmoon(nlast, 2);
            x = jd - fljd;
            if (x < 0d) {
                return String.format(Locale.ENGLISH, "%3.1f days before full moon.", (-1d * x));
            } else {
                return String.format(Locale.ENGLISH, "%3.1f days after full moon.", x);
            }
        } else if (noctiles <= 6) {  // nearest last quarter ...
            lqjd = flmoon(nlast, 3);
            x = jd - lqjd;
            if (x < 0d) {
                return String.format(Locale.ENGLISH, "%3.1f days before last quarter.", (-1d * x));
            } else {
                return String.format(Locale.ENGLISH, "%3.1f days after last quarter.", x);
            }
        } else {
            newjd = flmoon(nlast + 1, 0);
            x = jd - newjd;
            return String.format(Locale.ENGLISH, "%3.1f days before new moon.", (-1d * x));
        }
    }
}
