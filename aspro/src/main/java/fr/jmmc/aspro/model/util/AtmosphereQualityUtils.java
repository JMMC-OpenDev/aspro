/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

import fr.jmmc.aspro.model.oi.AtmosphereQuality;
import java.util.Vector;

/**
 * This class provides static methods to use easily AtmosphereQuality enum
 * @author bourgesl
 */
public final class AtmosphereQualityUtils {

    /** computed AtmosphereQuality  */
    private static Vector<String> atmosphereQualityList = null;

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
                return 0.4d;
            case GOOD:
                return 0.7d;
            default:
            case AVERAGE:
                return 1.0d;
            case BAD:
                return 1.4d;
            case AWFUL:
                return 1.8d;
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
                return 10.0;
            case GOOD:
                return 6.0;
            default:
            case AVERAGE:
                return 4.0;
            case BAD:
                return 1.0;
            case AWFUL:
                return 0.5;
        }
    }

}
