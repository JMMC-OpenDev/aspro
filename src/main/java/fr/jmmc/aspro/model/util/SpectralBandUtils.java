/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.jmal.Band;

/**
 * This class provides static methods to use easily SpectralBand enum
 * @author bourgesl
 */
public final class SpectralBandUtils {

    /**
     * Forbidden constructor
     */
    private SpectralBandUtils() {
        // no-op
    }

    /**
     * Find the SpectralBand corresponding to the given Band
     * @param band band to use
     * @return SpectralBand or null
     */
    public static SpectralBand findBand(final Band band) {
        switch (band) {
            case U:
                // band U not supported
                throw new IllegalArgumentException("Band " + band + " not supported !");
            case B:
                return SpectralBand.B;
            case V:
                return SpectralBand.V;
            // note: SpectralBand.G is not available in Band !
            case R:
                return SpectralBand.R;
            case I:
                return SpectralBand.I;
            case J:
                return SpectralBand.J;
            case H:
                return SpectralBand.H;
            case K:
                return SpectralBand.K;
            case L:
                return SpectralBand.L;
            case M:
                return SpectralBand.M;
            case N:
                return SpectralBand.N;
            case Q:
                // band Q not supported
                throw new IllegalArgumentException("Band " + band + " not supported !");
            default:
                return null;
        }
    }

    /**
     * Find the Band corresponding to the given SpectralBand
     * @param band band to use
     * @return Band or null
     */
    public static Band findBand(final SpectralBand band) {
        switch (band) {
            case B:
                return Band.B;
            case V:
                return Band.V;
            case G:
                // note: SpectralBand.G is not available in Band !
                return Band.V;
            case R:
                return Band.R;
            case I:
                return Band.I;
            case J:
                return Band.J;
            case H:
                return Band.H;
            case K:
                return Band.K;
            case L:
                return Band.L;
            case M:
                return Band.M;
            case N:
                return Band.N;
            default:
                return null;
        }
    }

}
