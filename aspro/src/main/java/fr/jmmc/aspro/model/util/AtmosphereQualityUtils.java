/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

import fr.jmmc.aspro.model.oi.AtmosphereQuality;
import java.util.EnumMap;
import java.util.Vector;

/**
 * This class provides static methods to use easily AtmosphereQuality enum
 * @author bourgesl
 */
public final class AtmosphereQualityUtils {

    /** computed AtmosphereQuality */
    private static Vector<String> atmosphereQualityList = null;

    /** computed AtmosphereQuality tooltip */
    private static EnumMap<AtmosphereQuality, String> atmosphereQualityTooltip = null;

    /**
     * Forbidden constructor
     */
    private AtmosphereQualityUtils() {
        // no-op
    }

    /**
     * Return the vector of all atmosphere quality items as string
     * @return vector of all atmosphere quality items as string
     */
    public static Vector<String> getAtmosphereQualityList() {
        if (atmosphereQualityList == null) {
            final AtmosphereQuality[] vals = AtmosphereQuality.values();
            atmosphereQualityList = new Vector<String>(vals.length);
            for (AtmosphereQuality a : vals) {
                atmosphereQualityList.add(a.value());
            }
        }
        return atmosphereQualityList;
    }

    /**
     * Return the vector of all atmosphere quality items as string
     * @return vector of all atmosphere quality items as string
     */
    public static EnumMap<AtmosphereQuality, String> getAtmosphereQualityTooltips() {
        if (atmosphereQualityTooltip == null) {
            atmosphereQualityTooltip = new EnumMap<AtmosphereQuality, String>(AtmosphereQuality.class);
            for (AtmosphereQuality atmQuality : AtmosphereQuality.values()) {
                atmosphereQualityTooltip.put(atmQuality, computeTooltip(atmQuality));
            }
        }
        return atmosphereQualityTooltip;
    }

    /**
     * Return the atmosphere quality for the given string
     * @param value atmosphere quality as string
     * @return atmosphere quality item
     */
    public static AtmosphereQuality getAtmosphereQuality(final String value) {
        return AtmosphereQuality.fromValue(value);
    }

    /**
     * Return the corresponding seeing in arc seconds for the given atmosphere quality
     * @param atmQuality atmosphere quality
     * @return seeing in arc seconds
     */
    public static double getSeeing(final AtmosphereQuality atmQuality) {
        switch (atmQuality) {
            case EXCELLENT:
                /* "10%  (Seeing < 0.6  arcsec, t0 > 5.2 ms)" */
                return 0.6;
            case GOOD:
                /* "20%  (Seeing < 0.7  arcsec, t0 > 4.4 ms)" */
                return 0.7;
            default:
            case AVERAGE:
                /* "50%  (Seeing < 1.0  arcsec, t0 > 3.2 ms)" */
                return 1.0;
            case WORSE:
                /* "70%  (Seeing < 1.15 arcsec, t0 > 2.2 ms)" */
                return 1.15;
            case BAD:
                return 1.4;
            case AWFUL:
                return 1.8;
        }
    }

    /**
     * Return the corresponding coherence time (t0) in milli-seconds for the given atmosphere quality
     * @param atmQuality atmosphere quality
     * @return coherence time (t0) in milli-seconds
     */
    public static double getCoherenceTime(final AtmosphereQuality atmQuality) {
        switch (atmQuality) {
            case EXCELLENT:
                /* "10%  (Seeing < 0.6  arcsec, t0 > 5.2 ms)" */
                return 5.2;
            case GOOD:
                /* "20%  (Seeing < 0.7  arcsec, t0 > 4.4 ms)" */
                return 4.4;
            default:
            case AVERAGE:
                /* "50%  (Seeing < 1.0  arcsec, t0 > 3.2 ms)" */
                return 3.2;
            case WORSE:
                /* "70%  (Seeing < 1.15 arcsec, t0 > 2.2 ms)" */
                return 2.2;
            case BAD:
                return 1.6;
            case AWFUL:
                return 0.5;
        }
    }

    /**
     * Return the tooltip corresponding to the given atmosphere quality
     * @param value atmosphere quality as string
     * @return tooltip 
     */
    public static String getTooltip(final String value) {
        return getAtmosphereQualityTooltips().get(getAtmosphereQuality(value));
    }

    private static String computeTooltip(final AtmosphereQuality atmQuality) {
        return atmQuality.value() + " sky conditions: seeing = "
                + getSeeing(atmQuality) + " arcsec, t0 = "
                + getCoherenceTime(atmQuality) + " ms";
    }

}
