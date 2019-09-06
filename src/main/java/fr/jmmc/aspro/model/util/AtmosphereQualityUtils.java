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
                return 0.4;
            case GOOD:
                return 0.7;
            default:
            case AVERAGE:
                return 1.0;
            case WORSE:
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
                return 8.0; /* "10%  (Seeing < 0.6  arcsec, t0 > 5.2 ms)", */
            case GOOD:
                return 5.0; /* "20%  (Seeing < 0.7  arcsec, t0 > 4.4 ms)" */
            default:
            case AVERAGE:
                return 3.2; /* "50%  (Seeing < 1.0  arcsec, t0 > 3.2 ms)" */
            case WORSE:
                return 2.2; /* "70%  (Seeing < 1.15 arcsec, t0 > 2.2 ms)" */
            case BAD:
                return 1.6; /* "85%  (Seeing < 1.4  arcsec, t0 > 1.6 ms)" */
            case AWFUL:
                return 0.5;
        }
    }

}
