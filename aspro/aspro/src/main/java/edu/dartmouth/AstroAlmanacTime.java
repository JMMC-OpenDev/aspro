/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package edu.dartmouth;

import java.util.EnumMap;

/**
 * This class describe the julian date of a Sun/Moon event (twilight, set or rise)
 * @author bourgesl
 */
public final class AstroAlmanacTime implements Comparable<AstroAlmanacTime> {
    /* 1ms precision in JD (day) */

    public final static double PREC_JD = 1.0 / (86400000.0);

    /** type of almanac event */
    public enum AlmanacType {

        /** Sun Astronomical Twilight (before rise) */
        SunTwl18Rise,
        /** Sun Nautical Twilight (before rise) */
        SunTwl12Rise,
        /** Sun Civil Twilight (before rise) */
        SunTwl06Rise,
        /** Sun Rise */
        SunRise,
        /** Sun Set */
        SunSet,
        /** Sun Civil Twilight (after set) */
        SunTwl06Set,
        /** Sun Nautical Twilight (after set) */
        SunTwl12Set,
        /** Sun Astronomical Twilight (after set) */
        SunTwl18Set,
        /** Moon Rise */
        MoonRise,
        /** Moon Set */
        MoonSet;

        public final static EnumMap<AlmanacType, AlmanacType> mapping = createMapping();

        private static EnumMap<AlmanacType, AlmanacType> createMapping() {
            final EnumMap<AlmanacType, AlmanacType> mapping = new EnumMap<AlmanacType, AlmanacType>(AlmanacType.class);

            mapping.put(SunTwl18Rise, SunTwl18Set);
            mapping.put(SunTwl18Set, SunTwl18Rise);

            mapping.put(SunTwl12Rise, SunTwl12Set);
            mapping.put(SunTwl12Set, SunTwl12Rise);

            mapping.put(SunTwl06Rise, SunTwl06Set);
            mapping.put(SunTwl06Set, SunTwl06Rise);

            mapping.put(SunRise, SunSet);
            mapping.put(SunSet, SunRise);

            mapping.put(MoonRise, MoonSet);
            mapping.put(MoonSet, MoonRise);

            return mapping;
        }
    }
    /** julian date */
    private double jd;
    /** event type */
    private final AlmanacType type;

    /**
     * Protected constructor
     * @param jd julian date of the almanac event
     * @param type type of the almanac event
     */
    protected AstroAlmanacTime(final double jd, final AlmanacType type) {
        super();
        this.jd = jd;
        this.type = type;
    }

    /**
     * Return the julian date of the almanac event
     * @return julian date of the almanac event
     */
    public double getJd() {
        return jd;
    }
    
    /**
     * Update the julian date of the almanac event
     * @param jd julian date of the almanac event
     */
    void setJd(final double jd) {
        this.jd = jd;
    }

    /**
     * Return the type of the almanac event
     * @return type of the almanac event
     */
    public AlmanacType getType() {
        return type;
    }

    /**
     * Override equals() required by Set implementation
     * @param obj object to test
     * @return true if both type and jd are equals
     */
    @Override
    public boolean equals(final Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final AstroAlmanacTime other = (AstroAlmanacTime) obj;
        if (this.jd != other.getJd()) {
            return false;
        }
        return this.type == other.getType();
    }

    /**
     * Override hashCode() required by Set implementation
     * @return composed hash code based on jd and type fields
     */
    @Override
    public int hashCode() {
        final long bits = Double.doubleToLongBits(this.jd);
        int hash = 7;
        hash = 37 * hash + (int) (bits ^ (bits >>> 32));
        hash = 37 * hash + this.type.hashCode();
        return hash;
    }

    @Override
    public int compareTo(final AstroAlmanacTime t) {
        // take care of precision issue (below 1ms):
        int pos = (Math.abs(this.jd - t.getJd()) > PREC_JD) ? Double.compare(this.jd, t.getJd()) : 0;
        if (pos == 0) {
            pos = this.type.compareTo(t.getType());
        }
        return pos;
    }

    /**
     * Return a string representation
     * @return 'type @ jd'
     */
    @Override
    public String toString() {
        return getType() + " @ " + getJd();
    }
}
