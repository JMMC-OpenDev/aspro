/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package edu.dartmouth;

import edu.dartmouth.AstroAlmanacTime.AlmanacType;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

/**
 * This class gathers both almanac times for sun and moon objects
 * @author bourgesl
 */
public final class AstroAlmanac {

    /** Sun unique sorted JD time stamps */
    private final Set<AstroAlmanacTime> sunTimes = new TreeSet<AstroAlmanacTime>();
    /** Moon unique sorted JD time stamps */
    private final Set<AstroAlmanacTime> moonTimes = new TreeSet<AstroAlmanacTime>();

    /**
     * Protected constructor
     */
    protected AstroAlmanac() {
        super();
    }

    /**
     * Return the Sun unique sorted JD time stamps
     * @return Sun unique sorted JD time stamps
     */
    public Set<AstroAlmanacTime> getSunTimes() {
        return sunTimes;
    }

    /**
     * Return the Moon unique sorted JD time stamps
     * @return Moon unique sorted JD time stamps
     */
    public Set<AstroAlmanacTime> getMoonTimes() {
        return moonTimes;
    }

    /**
     * Return a string representation
     * @return string representation
     */
    @Override
    public String toString() {
        return "sun times:\n" + getSunTimes()
                + "\nmoon times:\n" + getMoonTimes();
    }

    /**
     * Translate the given source time collection into the given dest time collection
     * @param jdOffset julian day offset to add
     * @param source source time collection
     * @param dest dest time collection
     */
    public static void translate(final double jdOffset, final Set<AstroAlmanacTime> source, final Set<AstroAlmanacTime> dest) {
        AstroAlmanacTime time, newTime;
        for (Iterator<AstroAlmanacTime> it = source.iterator(); it.hasNext();) {
            time = it.next();

            newTime = new AstroAlmanacTime(time.getJd() + jdOffset, time.getType());
            dest.add(newTime);
        }
    }

    /**
     * Trim the given source time collection (ie remove redundant times = zero-length) into the given dest time collection
     * @param source source time collection
     * @param dest dest time collection
     */
    public static void trim(final Set<AstroAlmanacTime> source, final Set<AstroAlmanacTime> dest) {
        // Suppose the source set is sorted:
        double jd = Double.NaN;
        final ArrayList<AstroAlmanacTime> sameJD = new ArrayList<AstroAlmanacTime>(8);

        for (final Iterator<AstroAlmanacTime> it = source.iterator(); it.hasNext();) {
            final AstroAlmanacTime newTime = it.next();

            final double newJd = newTime.getJd();

            // check jd is the same within epsilon ?
            if (!Double.isNaN(jd) && Math.abs(newJd - jd) > AstroAlmanacTime.PREC_JD) {
                // trim & push previous JD to dest:
                trimAndPush(sameJD, dest);
                // reset
                sameJD.clear();
            }
            // anyway add the new time to the same jd list:
            sameJD.add(newTime);
            jd = newJd;
        }
        if (!sameJD.isEmpty()) {
            // trim & push to dest:
            trimAndPush(sameJD, dest);
        }
    }

    private static void trimAndPush(final ArrayList<AstroAlmanacTime> sameJD, final Set<AstroAlmanacTime> dest) {
        final int len = sameJD.size();

        if (len > 1) {
            final AstroAlmanacTime[] rem = sameJD.toArray(new AstroAlmanacTime[len]);

            // jd reference (with 1 ms):
            final double refJd = rem[0].getJd();

            for (int i = 0; i < len; i++) {
                final AstroAlmanacTime time = rem[i];

                // this time is present in sameJD, is its contrary also present ?
                final AlmanacType otherType = AlmanacType.mapping.get(time.getType());

                final AstroAlmanacTime otherTime = findByType(sameJD, otherType);
                if (otherTime != null) {
                    // remove both:
                    sameJD.remove(time);
                    sameJD.remove(otherTime);
                } else {
                    // fix JD:
                    time.setJd(refJd);
                }
            }
        }

        if (!sameJD.isEmpty()) {
            // Push remaining times into dest:
            dest.addAll(sameJD);
        }
    }

    private static AstroAlmanacTime findByType(final ArrayList<AstroAlmanacTime> sameJD, final AlmanacType type) {
        for (int i = 0, len = sameJD.size(); i < len; i++) {
            final AstroAlmanacTime time = sameJD.get(i);
            if (time.getType() == type) {
                return time;
            }
        }
        return null;
    }

}
