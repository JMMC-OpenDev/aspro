/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.util;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.gpao.AsproLGS;
import fr.jmmc.jmal.Band;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author bourgesl
 */
public class StrehlUtils {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(StrehlUtils.class.getName());

    public static double[] strehl(final String aoName, final Band aoBand, final double adaptiveOpticsMag, final double[] waveLengths,
                                  final double telDiam, final double seeing, final int nbSubPupils, final int nbActuators,
                                  final double td, final double t0, final double qe, final double ron, final double h0,
                                  final double elevation, double strehlMax, final double instrumentDIT, final double magOffset) {

        if (false) {
            System.out.println("setup[" + aoName + "(" + aoBand + ")] seeing: " + seeing
                    + " telDiam: " + telDiam
                    + " nbSubPupils: " + nbSubPupils
                    + " nbActuators: " + nbActuators
                    + " td: " + td
                    + " t0: " + t0
                    + " qe: " + qe
                    + " ron: " + ron
                    + " h0: " + h0
                    + " strehlMax: " + strehlMax
                    + " instrumentDIT: " + instrumentDIT
                    + " - mag = " + adaptiveOpticsMag
            );
        }

        final double[] strehlPerChannel;
        if ((aoName != null) && (aoName.startsWith(AsproConstants.GPAO_START))) {
            if (strehlMax <= 0.0) {
                final double waveMid = waveLengths[waveLengths.length / 2];
                final Band insBand = Band.findBand(1e6 * waveMid);

                strehlMax = insBand.getStrehlMax();

                if (false) {
                    System.out.println("strehlMax: " + strehlMax + " for " + insBand + " at " + waveMid);
                }
            }
            if (strehlMax > 1.0) {
                strehlMax = 1.0;
            }

            if (false) {
                System.out.println("strehlMax: " + strehlMax);
            }

            final String flagMode = aoName.substring(AsproConstants.GPAO_START.length());

            strehlPerChannel = AsproLGS.strehl(flagMode, (adaptiveOpticsMag + magOffset), 90.0 - elevation, seeing, t0, h0, strehlMax, waveLengths);
        } else {
            strehlPerChannel = Band.strehl(aoBand, (adaptiveOpticsMag + magOffset), waveLengths, telDiam,
                    seeing, nbSubPupils, nbActuators, td, t0, qe, ron, elevation, strehlMax);
        }

        if (false) {
            System.out.println("t0: " + t0 + " instrumentDIT: " + instrumentDIT + " ratio: " + (instrumentDIT / t0));
        }
        // check instrument DIT:
        if (instrumentDIT >= 3.0 * t0) {
            // use long-exposure strehl:
            Band.strehlLongExposure(waveLengths, telDiam, seeing, elevation, strehlPerChannel);
        }
        return strehlPerChannel;
    }

}
