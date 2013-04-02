package edu.dartmouth;

import java.util.Locale;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
 modify it for their own purposes, provided that credit is given to the author
 in a prominent place.  For the present program that means that the green
 title and author banner appearing on the main window must not be removed,
 and may not be altered without premission of the author. */
/** The instance variable jd is the true jd (always), and there are
 two GenericCalDat instances, UTDate and localDate.  Instance
 variables stdz and useDST are the std zone offset (hours west) and an
 integer encoding which daylight savings convention is used.
 FindDSTBounds figures out whether DST is actually in effect.
 **/
public final class InstantInTime implements Cloneable {

    static final double TheEpoch = 2440587.5d;  // Jan 1, 1970 0h UT

    /* Members */
    double jd;   /* the real JD, always. */

    GenericCalDat UTDate;
    GenericCalDat localDate;
    double localjd;   /* used transiently ... */

    double stdz;   // hours west
    int useDST;
    boolean dstInEffect = false;
    double[] DSTBounds = {0d, 0d};

    public InstantInTime(final double stdzin, final int use_dst) {
        // default constructor sets to system time ...
        stdz = stdzin;
        useDST = use_dst;
        setInstant(stdzin, use_dst);
    }

    public InstantInTime(final String s, final double stdzin,
            final int use_dst, final boolean is_ut) {
        /* allowed formats are "yyyy mo dd hh mm ss" and "mo" can
         be a three-letter abbreviation. */
        stdz = stdzin;
        useDST = use_dst;
        setInstant(s, stdzin, use_dst, is_ut);
    }

    public InstantInTime(final double jdin, final double stdzin,
            final int use_dst, final boolean is_ut) {
        /* allowed formats are "yyyy mo dd hh mm ss" and "mo" can
         be a three-letter abbreviation. */
        stdz = stdzin;
        useDST = use_dst;
        setInstant(jdin, stdzin, use_dst, is_ut);
    }

    void setInstant(final double stdzin, final int use_dst) {
        // when no time specified, sets to system time.
        long milliseconds;
        double jdnow;

        stdz = stdzin;
        useDST = use_dst;
        milliseconds = System.currentTimeMillis();
        jdnow = TheEpoch + milliseconds / 86400000d;
        setInstant(jdnow, stdzin, use_dst, true);
    }

    @Override
    public InstantInTime clone() {
        try {
            final InstantInTime copy = (InstantInTime) super.clone();
            copy.UTDate = UTDate.clone();
            copy.localDate = localDate.clone();
            return copy;
        } catch (CloneNotSupportedException e) {
            throw new Error("This should never happen!");
        }
    }

    void setInstant(final String s, final double stdzin, final int use_dst, final boolean is_ut) {

        useDST = use_dst;

        if (is_ut) {
            UTDate = new GenericCalDat(s);
            jd = UTDate.cal2JD();
            // System.out.printf("Setting to UT, s = %s,jd = %f%n",s,jd);
            // The DST calculation is not sensitive to year if dstInEffect is
            // computed using the same year as used in findDSTBounds.
            if (use_dst == 0) {
                dstInEffect = false;
            } else {  // determine if DST is in effect ...
                DSTBounds = findDSTBounds(UTDate.year, stdzin, use_dst);
                if (use_dst > 0) {  // northern hemisphere logic
                    if (jd > DSTBounds[0] && jd < DSTBounds[1]) {
                        dstInEffect = true;
                    } else {
                        dstInEffect = false;
                    }
                }
                if (use_dst < 0) {  // southern hemisphere logic
                    if (jd < DSTBounds[0] || jd > DSTBounds[1]) {
                        dstInEffect = true;
                    } else {
                        dstInEffect = false;
                    }
                }
                // System.out.printf("use_dst %d jd %f Bounds[0,1] = %f %f%n",
                //   use_dst,jd,DSTBounds[0],DSTBounds[1]);
                // if (dstInEffect) System.out.printf("DST is in effect.%n");
                // else System.out.printf("DST is NOT in effect.%n");
            }

            if (dstInEffect) {
                localjd = jd - (stdzin - 1d) * Const.HOUR_IN_DAY;
                //  System.out.printf("setting localjd using DST%n");
            } else {
                localjd = jd - (stdzin * Const.HOUR_IN_DAY);
                //  System.out.printf("Setting localjd using std time%n");
            }
            localDate = new GenericCalDat(localjd);

        } else {  // input string is local date and time.
            localDate = new GenericCalDat(s);  // by definition ..
            localjd = localDate.cal2JD();
            // System.out.printf("using localjd = %f%n",localjd);
            if (use_dst == 0) {
                dstInEffect = false;
            } else { // dst is used if applicable ... use local-time limits of
                // applicability {DSTBounds [2] and [3] instead of [0] and [1])
                DSTBounds = findDSTBounds(localDate.year, stdzin, use_dst);
                if (use_dst > 0) {  // northern hemisphere logic
                    if (localjd > DSTBounds[2] && localjd < DSTBounds[3]) {
                        dstInEffect = true;
                    } else {
                        dstInEffect = false;
                    }
                }
                if (use_dst < 0) {  // southern hemisphere logic
                    if (localjd < DSTBounds[2] || localjd > DSTBounds[3]) {
                        dstInEffect = true;
                    } else {
                        dstInEffect = false;
                    }
                }
            }

            if (dstInEffect) {
                jd = localjd + (stdzin - 1d) * Const.HOUR_IN_DAY;
            } else {
                jd = localjd + stdzin * Const.HOUR_IN_DAY;
            }
            // System.out.printf("Setting jd to %f%n",jd);
            UTDate = new GenericCalDat(jd);
        }
    }

    void advanceTime(final String s, final double stdzin, final int use_dst_in) {
        String[] fields;
        String lastfield;
        double inputdelta, delta;
        int nf;

        stdz = stdzin;
        useDST = use_dst_in;

        fields = s.split("\\s+");  // whitespace

        nf = fields.length;
        inputdelta = Double.parseDouble(fields[0]);
        if (nf > 1) {
            lastfield = fields[nf - 1].toLowerCase();
        } else {
            lastfield = "h";   // defaults to hours
        }
        // use first character to tell us what the unit is
//      System.out.printf("last field %s  lastfield.substring(0,1) :%s:%n",
        //          lastfield,lastfield.substring(0,1));
        if (lastfield.startsWith("h")) {
            delta = inputdelta * Const.HOUR_IN_DAY;
        } else if (lastfield.startsWith("m")) {
            delta = inputdelta / 1440d;
        } else if (lastfield.startsWith("s")) {
            delta = inputdelta / 86400d;
        } else if (lastfield.startsWith("d")) {
            delta = inputdelta;
        } else if (lastfield.startsWith("t")) {
            delta = inputdelta / Const.SID_RATE;
        } // 1 sidereal day
        else if (lastfield.startsWith("l")) {
            delta = 29.5307d * inputdelta;
        } // 1 lunation
        else if (lastfield.startsWith("y")) {
            delta = 365d * inputdelta;
        } else {
            delta = inputdelta * Const.HOUR_IN_DAY;
        }

        setInstant(jd + delta, stdz, useDST, true);
        // jd is always UT, so use_dst is true

    }

    void advanceTime(final String s, final double stdzin, final int use_dst_in, final boolean forward) {
        String[] fields;
        String lastfield;
        double inputdelta, delta;
        int nf;

        stdz = stdzin;
        useDST = use_dst_in;

        fields = s.split("\\s+");  // whitespace

        nf = fields.length;
        inputdelta = Double.parseDouble(fields[0]);
        if (nf > 1) {
            lastfield = fields[nf - 1].toLowerCase();
        } else {
            lastfield = "h";   // defaults to hours
        }
        // use first character to tell us what the unit is
//      System.out.printf("last field %s  lastfield.substring(0,1) :%s:%n",
        //          lastfield,lastfield.substring(0,1));
        if (lastfield.startsWith("h")) {
            delta = inputdelta * Const.HOUR_IN_DAY;
        } else if (lastfield.startsWith("m")) {
            delta = inputdelta / 1440d;
        } else if (lastfield.startsWith("s")) {
            delta = inputdelta / 86400d;
        } else if (lastfield.startsWith("d")) {
            delta = inputdelta;
        } else if (lastfield.startsWith("t")) {
            delta = inputdelta / Const.SID_RATE;
        } // 1 sidereal day
        else if (lastfield.startsWith("l")) {
            delta = 29.5307d * inputdelta;
        } // 1 lunation
        else if (lastfield.startsWith("y")) {
            delta = 365d * inputdelta;
        } else {
            delta = inputdelta * Const.HOUR_IN_DAY;
        }
        /*      System.out.println("advanceTime, delta = " + String.format(Locale.ENGLISH, "%f",delta) +
         "forward = " + forward); */

        if (forward) {
            setInstant(jd + delta, stdz, useDST, true);
        } // jd is always UT, so use_dst is true
        else {
            setInstant(jd - delta, stdz, useDST, true);
            // System.out.printf("advanceTime: JD, delta %f %f  stdz %f useDST %d%n",
            //          jd,delta,stdz,useDST);
        }
    }

    void setInstant(final double jdin, final double stdzin, final int use_dst, final boolean is_ut) {
        // Silly to repeat all that code just to change datatype ...
        // but it'll work correctly at least.

        useDST = use_dst;

        if (is_ut) {
            jd = jdin;  // by definition, it's the JD

            // LBO: reduce memory footprint
            UTDate = (UTDate == null) ? new GenericCalDat(jd) : UTDate.update(jd);

            // The DST calculation is not sensitive to year if dstInEffect is
            // computed using the same year as used in findDSTBounds.
            if (use_dst == 0) {
                dstInEffect = false;
            } else {  // determine if DST is in effect ...
                DSTBounds = findDSTBounds(UTDate.year, stdzin, use_dst);
                if (use_dst > 0) {  // northern hemisphere logic
                    if (jd > DSTBounds[0] && jd < DSTBounds[1]) {
                        dstInEffect = true;
                    } else {
                        dstInEffect = false;
                    }
                }
                if (use_dst < 0) {  // southern hemisphere logic
                    if (jd < DSTBounds[0] || jd > DSTBounds[1]) {
                        dstInEffect = true;
                    } else {
                        dstInEffect = false;
                    }
                }
                // System.out.printf("use_dst %d jd %f Bounds[0,1] = %f %f%n",
                //   use_dst,jd,DSTBounds[0],DSTBounds[1]);
                // if (dstInEffect) System.out.printf("DST is in effect.%n");
                // else System.out.printf("DST is NOT in effect.%n");
            }

            if (dstInEffect) {
                localjd = jd - (stdzin - 1d) * Const.HOUR_IN_DAY;
            } else {
                localjd = jd - (stdzin * Const.HOUR_IN_DAY);
            }

            // LBO: reduce memory footprint
            localDate = (localDate == null) ? new GenericCalDat(localjd) : localDate.update(localjd);

        } else {
            // input string is local date and time.

            // LBO: reduce memory footprint
            localDate = (localDate == null) ? new GenericCalDat(jdin) : localDate.update(jdin); // by definition ...

            localjd = localDate.cal2JD();
            if (use_dst == 0) {
                dstInEffect = false;
            } else { // dst is used if applicable ... use local-time limits of
                // applicability (DSTBounds [2] and [3] instead of [0] and [1])
                DSTBounds = findDSTBounds(localDate.year, stdzin, use_dst);
                if (use_dst > 0) {  // northern hemisphere logic
                    if (localjd > DSTBounds[2] && localjd < DSTBounds[3]) {
                        dstInEffect = true;
                    } else {
                        dstInEffect = false;
                    }
                }
                if (use_dst < 0) {  // southern hemisphere logic
                    if (localjd < DSTBounds[2] || localjd > DSTBounds[3]) {
                        dstInEffect = true;
                    } else {
                        dstInEffect = false;
                    }
                }
            }

            if (dstInEffect) {
                jd = localjd + (stdzin - 1d) * Const.HOUR_IN_DAY;
            } else {
                jd = localjd + stdzin * Const.HOUR_IN_DAY;
            }

            // LBO: reduce memory footprint
            UTDate = (UTDate == null) ? new GenericCalDat(jd) : UTDate.update(jd);
        }
    }

    double[] findDSTBounds(final int year, final double stdz, final int use_dst) {
        /** returns jdb and jde, first and last dates for dst in year.
         [0] and [1] are true JD, [2] and [3] are reckoned wrt local time.
         This proved to be useful later.
         The parameter use_dst allows for a number
         of conventions, namely:
         0 = don't use it at all (standard time all the time)
         1 = use USA convention (1st Sun in April to
         last Sun in Oct 1986 - 2006; last Sun in April before;
         2nd sunday in March to first in Nov from 2007 on.)
         2 = use Spanish convention (for Canary Islands)
         -1 = use Chilean convention (CTIO).
         -2 = Australian convention (for AAT).
         Negative numbers denote sites in the southern hemisphere,
         where jdb and jde are beginning and end of STANDARD time for
         the year.
         It's assumed that the time changes at 2AM local time; so
         when clock is set ahead, time jumps suddenly from 2 to 3,
         and when time is set back, the hour from 1 to 2 AM local
         time is repeated.  This could be changed in code if need be.
         Straight translation of skycalc c routine.
         **/
        int nSundays = 0;
        int nSaturdays = 0;  // for Chile, keep descriptive name
        int trialday = 1;
        int trialdow = 0;
        String trialstring;
        double[] JDBoundary = {0d, 0d, 0d, 0d};
        double jdtmp;

        GenericCalDat trial = new GenericCalDat("2000 1 1 1 1 1");
        if (use_dst == 0) {  // if DST is not used this shouldn't matter.
            return JDBoundary;  // this is a common case, get outta here fast.
        } else if (use_dst == 1) {
            // USA conventions from mid-60s (?) onward.
            // No attempt to account for energy-crisis measures (Nixon admin.)
            if (year < 1986) {  // last sunday in April
                trialday = 30;
                while (trialdow != 6) {
                    trialstring = String.format(Locale.ENGLISH, "%d 4 %d 2 0 0", year, trialday);
                    trial.calFromString(trialstring);
                    trialdow = trial.dayOfWeek();
                    trialday--;
                }
            } else if (year <= 2006) {  // first Sunday in April
                trialday = 1;
                trialdow = 0;
                while (trialdow != 6) {
                    trialstring = String.format(Locale.ENGLISH, "%d 4 %d 2 0 0", year, trialday);
                    trial.calFromString(trialstring);
                    trialdow = trial.dayOfWeek();
                    trialday++;
                }
            } else {  // 2007 and after, it's 2nd Sunday in March ....
                nSundays = 0;
                trialday = 1;
                trialdow = 0;
                while (nSundays < 2) {
                    trialstring = String.format(Locale.ENGLISH, "%d 3 %d 2 0 0", year, trialday);
                    trial.calFromString(trialstring);
                    trialdow = trial.dayOfWeek();
                    if (trialdow == 6) {
                        nSundays++;
                    }
                    trialday++;
                }
            }
            jdtmp = trial.cal2JD();
            JDBoundary[0] = jdtmp + stdz * Const.HOUR_IN_DAY;  // true JD of change.
            JDBoundary[2] = jdtmp;  // local-time version


            trialdow = 0;  // for next round

            if (year < 2007) {  // last Sunday in October
                trialday = 31;
                while (trialdow != 6) {
                    trialstring = String.format(Locale.ENGLISH, "%d 10 %d 2 0 0", year, trialday);
                    trial.calFromString(trialstring);
                    trialdow = trial.dayOfWeek();
                    trialday--;
                }
            } else {   // first Sunday in November, didn't change in 1986.
                trialday = 1;
                trialdow = 0;
                while (trialdow != 6) {
                    trialstring = String.format(Locale.ENGLISH, "%d 11 %d 2 0 0", year, trialday);
                    trial.calFromString(trialstring);
                    trialdow = trial.dayOfWeek();
                    trialday++;
                }
            }
            jdtmp = trial.cal2JD();
            JDBoundary[1] = jdtmp + (stdz - 1d) * Const.HOUR_IN_DAY; // true JD
            JDBoundary[3] = jdtmp;  // local-time version
        } else if (use_dst == 2) { // EU convention
            trialday = 31; // start last Sunday in March at 1 AM
            while (trialdow != 6) {
                trialstring = String.format(Locale.ENGLISH, "%d 3 %d 1 0 0", year, trialday);
                trial.calFromString(trialstring);
                trialdow = trial.dayOfWeek();
                trialday--;
            }
            jdtmp = trial.cal2JD();
            JDBoundary[0] = jdtmp + stdz * Const.HOUR_IN_DAY;
            JDBoundary[2] = jdtmp;

            trialday = 30; // end last Sunday in October at 1 AM
            trialdow = 0;
            while (trialdow != 6) {
                trialstring = String.format(Locale.ENGLISH, "%d 10 %d 1 0 0", year, trialday);
                trial.calFromString(trialstring);
                trialdow = trial.dayOfWeek();
                trialday--;
            }
            jdtmp = trial.cal2JD();
            JDBoundary[1] = jdtmp + (stdz - 1d) * Const.HOUR_IN_DAY;
            JDBoundary[3] = jdtmp;
        } else if (use_dst == -1) { // Chile - negative -> southern
            // In the south, [0] and [2] -> March-ish -> END of DST
            //               [1] and [3] -> Octoberish -> BEGIN DST

            trialday = 1;  // starts at 24h on 2nd Sat. of Oct.
            nSaturdays = 0;
            while (nSaturdays != 2) {
                trialstring = String.format(Locale.ENGLISH, "%d 10 %d 23 0 0", year, trialday);
                // use 11 pm for day calculation to avoid any ambiguity
                trial.calFromString(trialstring);
                trialdow = trial.dayOfWeek();
                if (trialdow == 5) {
                    nSaturdays++;
                }
                trialday++;
            }
            jdtmp = trial.cal2JD();
            JDBoundary[1] = jdtmp + (stdz + 1d) * Const.HOUR_IN_DAY;
            JDBoundary[3] = jdtmp;
            // add the hour back here (DST start)

            nSaturdays = 0;
            trialday = 1;
            while (nSaturdays != 2) { // end on the 2nd Sat in March
                trialstring = String.format(Locale.ENGLISH, "%d 3 %d 23 0 0", year, trialday);
                // use 11 pm for day calculation to avoid any ambiguity
                trial.calFromString(trialstring);
                trialdow = trial.dayOfWeek();
                if (trialdow == 5) {
                    nSaturdays++;
                }
                trialday++;
            }
            jdtmp = trial.cal2JD();
            JDBoundary[0] = jdtmp + stdz * Const.HOUR_IN_DAY;
            JDBoundary[2] = jdtmp;
            // no need to add the hour back, DST is ending now.
        } else if (use_dst == -2) {  // Australia (NSW)
            trialday = 31;
            trialdow = 0;
            while (trialdow != 6) { // DST begins at 2 AM, Last sun in Oct
                trialstring = String.format(Locale.ENGLISH, "%d 10 %d 2 0 0", year, trialday);
                trial.calFromString(trialstring);
                trialdow = trial.dayOfWeek();
                trialday--;
            }
            jdtmp = trial.cal2JD();
            JDBoundary[1] = jdtmp + stdz * Const.HOUR_IN_DAY;
            JDBoundary[3] = jdtmp;

            trialday = 31;
            trialdow = 0;
            while (trialdow != 6) { // DST ends at 3 AM, Last sun in March
                trialstring = String.format(Locale.ENGLISH, "%d 3 %d 3 0 0", year, trialday);
                trial.calFromString(trialstring);
                trialdow = trial.dayOfWeek();
                trialday--;
            }
            jdtmp = trial.cal2JD();
            JDBoundary[0] = jdtmp + (stdz + 1d) * Const.HOUR_IN_DAY;
            JDBoundary[2] = jdtmp;
        }
        return JDBoundary;
    }

    double julianEpoch() {
        return julianEpoch(jd);
    }

    public static double julianEpoch(final double jdIn) {
        return 2000d + (jdIn - Const.J2000) * Const.DAY_IN_YEAR;
    }
}
