/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.service.UserModelService.MathMode;

/**
 * Basic bean gathering OIFits producer options
 * @author bourgesl
 */
public final class OIFitsProducerOptions {

    /** ignore SNR check: enabled for debugging / ETC tests */
    private final static boolean IGNORE_SNR_THRESHOLD = false;

    public final boolean userModelCubeInterpolation;
    public final boolean userModelCubeExtrapolation;
    /** OIFits supersampling */
    public final int supersampling;
    /** OIFits MathMode */
    public final MathMode mathMode;
    /** SNR threshold on VIS to flag values with low SNR */
    public final double snrThreshold;

    public OIFitsProducerOptions(boolean userModelCubeInterpolation,
                                 boolean userModelCubeExtrapolation,
                                 int supersampling, MathMode mathMode,
                                 double snrThreshold) {

        this.userModelCubeInterpolation = userModelCubeInterpolation;
        this.userModelCubeExtrapolation = userModelCubeExtrapolation;
        this.supersampling = supersampling;
        this.mathMode = mathMode;
        // use 1/2 to make SNR(VIS) < TH and not SNR(VIS2) < TH ( SNR(VIS2) = SNR(VIS) / 2 )
        this.snrThreshold = (IGNORE_SNR_THRESHOLD) ? 0.0 : snrThreshold / 2.0;
    }

    /**
     * @return string representation
     */
    @Override
    public String toString() {
        return "OIFitsProducerOptions{"
                + "userModelCubeInterpolation=" + userModelCubeInterpolation
                + ", userModelCubeExtrapolation=" + userModelCubeExtrapolation
                + ", supersampling=" + supersampling
                + ", mathMode=" + mathMode
                + ", snrThreshold=" + snrThreshold + '}';
    }

    /**
     *
     * @param other another OIFits producer options
     * @return true if the OIFITS producer options are the same; false otherwise
     */
    public boolean equals(final OIFitsProducerOptions other) {
        if (userModelCubeInterpolation != other.userModelCubeInterpolation) {
            return false;
        }
        if (userModelCubeExtrapolation != other.userModelCubeExtrapolation) {
            return false;
        }
        if (supersampling != other.supersampling) {
            return false;
        }
        if (mathMode != other.mathMode) {
            return false;
        }
        return snrThreshold == other.snrThreshold;
    }
}
