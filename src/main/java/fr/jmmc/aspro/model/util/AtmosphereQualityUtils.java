/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

import fr.jmmc.aspro.model.oi.AtmosphereQuality;
import static fr.jmmc.aspro.model.oi.AtmosphereQuality.AVERAGE;
import static fr.jmmc.aspro.model.oi.AtmosphereQuality.AWFUL;
import static fr.jmmc.aspro.model.oi.AtmosphereQuality.BAD;
import static fr.jmmc.aspro.model.oi.AtmosphereQuality.EXCELLENT;
import static fr.jmmc.aspro.model.oi.AtmosphereQuality.GOOD;
import static fr.jmmc.aspro.model.oi.AtmosphereQuality.WORSE;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.List;
import java.util.Vector;

/**
 * This class provides static methods to use easily AtmosphereQuality enum
 * @author bourgesl
 */
public final class AtmosphereQualityUtils {

    /** AtmosphereQuality set ordered by seeing ascending */
    public static final List<AtmosphereQuality> ORDERED = Arrays.asList(new AtmosphereQuality[]{EXCELLENT, GOOD, AVERAGE, WORSE, BAD, AWFUL});

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

    /*
    * Using ESO turbulence categories:
    - GRAVITY: https://www.eso.org/sci/observing/phase2/ObsConditions.GRAVITY.html
        More specifically, the categories are:
                T < 10%, corresponding to seeing ≤ 0.60“ and τ0 > 5.2ms
                T < 20%, corresponding to seeing ≤ 0.70“ and τ0 > 4.4ms
                T < 30%, corresponding to seeing ≤ 0.80“ and τ0 > 4.1ms
                T < 50%, corresponding to seeing ≤ 1.00“ and τ0 > 3.2ms
                T < 70%, corresponding to seeing ≤ 1.15“ and τ0 > 2.2ms
                T < 85%, corresponding to seeing ≤ 1.40“ and τ0 > 1.6ms
        For conditions worse than T = 85%, no GRAVITY operations are possible
    
    - MATISSE: https://www.eso.org/sci/observing/phase2/ObsConditions.MATISSE.html
        More specifically, the categories are:
            T < 10%, corresponding to seeing ≤ 0.60“ and τ0 > 5.2ms
            T < 20%, corresponding to seeing ≤ 0.70“ and τ0 > 4.4ms
            T < 30%, corresponding to seeing ≤ 0.80“ and τ0 > 4.1ms
            T < 50%, corresponding to seeing ≤ 1.00“ and τ0 > 3.2ms
            T < 70%, corresponding to seeing ≤ 1.15“ and τ0 > 2.2ms
            T < 85%, corresponding to seeing ≤ 1.40“ and τ0 > 1.6ms
        For conditions worse than T = 85%, no MATISSE operations are possible
    
    - PIONIER: https://www.eso.org/sci/observing/phase2/ObsConditions.PIONIER.html
        More specifically, the categories are:
            T < 10%, corresponding to seeing ≤ 0.60“ and τ0 > 5.2ms
            T < 20%, corresponding to seeing ≤ 0.70“ and τ0 > 4.4ms
            T < 30%, corresponding to seeing ≤ 0.80“ and τ0 > 4.1ms
            T < 50%, corresponding to seeing ≤ 1.00“ and τ0 > 3.2ms
            T < 70%, corresponding to seeing ≤ 1.15“ and τ0 > 2.2ms
            T < 85%, corresponding to seeing ≤ 1.40“ and τ0 > 1.6ms
    For conditions worse than T = 70%, no PIONIER operations are possible
    
    So for ASPRO2: last category is T < 85% (GRAVITY-MATISSE - PIONIER)
     */
    /**
     * Return the corresponding seeing in arc seconds for the given atmosphere quality
     * @param atmQuality atmosphere quality
     * @return seeing in arc seconds
     */
    public static double getSeeing(final AtmosphereQuality atmQuality) {
        switch (atmQuality) {
            case EXCELLENT:
                /* T < 10%, corresponding to seeing ≤ 0.60“ and τ0 > 5.2ms */
                return 0.60;
            case GOOD:
                /* T < 20%, corresponding to seeing ≤ 0.70“ and τ0 > 4.4ms */
                return 0.70;
            /* missing: T < 30%, corresponding to seeing ≤ 0.80“ and τ0 > 4.1ms */
            default:
            case AVERAGE:
                /* T < 50%, corresponding to seeing ≤ 1.00“ and τ0 > 3.2ms */
                return 1.00;
            case WORSE:
                /* T < 70%, corresponding to seeing ≤ 1.15“ and τ0 > 2.2ms */
                return 1.15;
            case BAD:
                /* T < 85%, corresponding to seeing ≤ 1.40“ and τ0 > 1.6ms */
                return 1.40;
            case AWFUL:
                return 1.80;
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
                /* T < 10%, corresponding to seeing ≤ 0.60“ and τ0 > 5.2ms */
                return 5.2;
            case GOOD:
                /* T < 20%, corresponding to seeing ≤ 0.70“ and τ0 > 4.4ms */
                return 4.4;
            /* missing: T < 30%, corresponding to seeing ≤ 0.80“ and τ0 > 4.1ms */
            default:
            case AVERAGE:
                /* T < 50%, corresponding to seeing ≤ 1.00“ and τ0 > 3.2ms */
                return 3.2;
            case WORSE:
                /* T < 70%, corresponding to seeing ≤ 1.15“ and τ0 > 2.2ms */
                return 2.2;
            case BAD:
                /* T < 85%, corresponding to seeing ≤ 1.40“ and τ0 > 1.6ms */
                return 1.6;
            case AWFUL:
                return 1.0;
        }
    }

    /**
     * Return the corresponding turburlence layer's height (h0) in meters for the given atmosphere quality
     * @param atmQuality atmosphere quality
     * @return turburlence layer's height (h0) in meters
     */
    public static double getTurbulenceHeight(final AtmosphereQuality atmQuality) {
        /*
        # from http://archive.eso.org/wdb/wdb/asm/mass_paranal/form:
        # Median $8 * $10 = median (MASS Turb Altitude [m] * MASS-DIMM Cn2 fraction at ground)

        seeing_values = np.array([0.60, 0.70, 0.80, 1.00, 1.15, 1.40])
        tau0_values = np.array([5.2, 4.4, 4.1, 3.2, 2.2, 1.6])
        ho_values = np.array([5850.0, 5250.0, 4650.0, 3700.0, 3200.0, 2700.0])
         */
        switch (atmQuality) {
            case EXCELLENT:
                /* T < 10%, corresponding to seeing ≤ 0.60“ and τ0 > 5.2ms */
                return 5850.0;
            case GOOD:
                /* T < 20%, corresponding to seeing ≤ 0.70“ and τ0 > 4.4ms */
                return 5250.0;
            /* missing: T < 30%, corresponding to seeing ≤ 0.80“ and τ0 > 4.1ms */
            default:
            case AVERAGE:
                /* T < 50%, corresponding to seeing ≤ 1.00“ and τ0 > 3.2ms */
                return 3700.0;
            case WORSE:
                /* T < 70%, corresponding to seeing ≤ 1.15“ and τ0 > 2.2ms */
                return 3200.0;
            case BAD:
                /* T < 85%, corresponding to seeing ≤ 1.40“ and τ0 > 1.6ms */
                return 2700.0;
            case AWFUL:
                return 2000.0;
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
        return atmQuality.value() + " sky conditions ["
                + "seeing = " + getSeeing(atmQuality) + " as, "
                + "t0 = " + getCoherenceTime(atmQuality) + " ms, "
                + "h0 = " + getTurbulenceHeight(atmQuality) + " m]";
    }

}
