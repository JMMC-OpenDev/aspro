/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.jmal.ALX;
import fr.jmmc.jmal.CoordUtils;
import fr.jmmc.jmal.star.Star;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.StringUtils;
import java.util.List;

/**
 *
 * @author bourgesl
 */
public final class TargetUtils {

    /** distance in degrees to consider same targets = 5 arcsecs */
    public final static double SAME_TARGET_DISTANCE = 5d * fr.jmmc.jmal.ALX.ARCSEC_IN_DEGREES;

    /**
     * Forbidden constructor
     */
    private TargetUtils() {
        // no-op
    }

    /**
     * Fix RA: parse given value as HMS and re-format to HMS (normalization)
     * @param ra right ascension as HMS
     * @return right ascension as HMS
     */
    public static String fixRA(final String ra) {
        return ALX.toHMS(ALX.parseHMS(ra));
    }

    /**
     * Fix DEC: parse given value as DMS and re-format to DMS (normalization)
     * @param dec declination as DMS
     * @return declination as DMS
     */
    public static String fixDEC(final String dec) {
        return ALX.toDMS(ALX.parseDEC(dec));
    }

    /**
     * Check the distance between the given source target and the given list of targets (5 arcesecs)
     * @param srcTarget source target
     * @param targets list of targets
     * @param throwException true indicates to throw an IllegalArgumentException if the target is found (giving distance and coordinates)
     * @return Target if found or null
     * @throws IllegalArgumentException if the target is too close to another target present in the given list of targets
     */
    public static Target matchTargetCoordinates(final Target srcTarget, final List<Target> targets,
                                                final boolean throwException) throws IllegalArgumentException {
        final double srcRaDeg = srcTarget.getRADeg();
        final double srcDecDeg = srcTarget.getDECDeg();

        double distance;

        for (Target target : targets) {
            distance = CoordUtils.computeDistanceInDegrees(srcRaDeg, srcDecDeg, target.getRADeg(), target.getDECDeg());

            if (distance <= SAME_TARGET_DISTANCE) {
                // first one (not the closest one):
                if (throwException) {
                    throw new IllegalArgumentException("Target[" + srcTarget.getName() + "](" + srcTarget.getRA() + ", " + srcTarget.getDEC()
                            + ") too close to Target[" + target.getName() + "](" + target.getRA() + ", " + target.getDEC()
                            + "): " + NumberUtils.trimTo3Digits(distance * ALX.DEG_IN_ARCSEC) + " arcsec !");
                }
                return target;
            }
        }
        return null;
    }

    /**
     * COnvert the given Star instance to a Target instance
     * @param star star instance
     * @return new Target instance
     */
    public static Target convert(final Star star) {
        if (star == null || StringUtils.isEmpty(star.getName())) {
            return null;
        }
        /* 
         Star data:
         Strings = {DEC=+43 49 23.910, RA=05 01 58.1341, OTYPELIST=**,Al*,SB*,*,Em*,V*,IR,UV, SPECTRALTYPES=A8Iab:}
         Doubles = {PROPERMOTION_RA=0.18, PARALLAX=1.6, DEC_d=43.8233083, FLUX_J=1.88, PROPERMOTION_DEC=-2.31, FLUX_K=1.533, PARALLAX_err=1.16, FLUX_V=3.039, FLUX_H=1.702, RA_d=75.4922254}
         */
        final Target newTarget = new Target();
        // format the target name:
        newTarget.updateNameAndIdentifier(star.getName());

        // coordinates (HMS / DMS) (mandatory):
        newTarget.setCoords(
                star.getPropertyAsString(Star.Property.RA).replace(' ', ':'),
                star.getPropertyAsString(Star.Property.DEC).replace(' ', ':'), 
                AsproConstants.EPOCH_J2000);

        // Proper motion (mas/yr) (optional) :
        newTarget.setPMRA(star.getPropertyAsDouble(Star.Property.PROPERMOTION_RA));
        newTarget.setPMDEC(star.getPropertyAsDouble(Star.Property.PROPERMOTION_DEC));

        // Parallax (mas) (optional) :
        newTarget.setPARALLAX(star.getPropertyAsDouble(Star.Property.PARALLAX));
        newTarget.setPARAERR(star.getPropertyAsDouble(Star.Property.PARALLAX_err));

        // Magnitudes (optional) :
        newTarget.setFLUXV(star.getPropertyAsDouble(Star.Property.FLUX_V));
        newTarget.setFLUXI(star.getPropertyAsDouble(Star.Property.FLUX_I));
        newTarget.setFLUXJ(star.getPropertyAsDouble(Star.Property.FLUX_J));
        newTarget.setFLUXH(star.getPropertyAsDouble(Star.Property.FLUX_H));
        newTarget.setFLUXK(star.getPropertyAsDouble(Star.Property.FLUX_K));
        newTarget.setFLUXN(star.getPropertyAsDouble(Star.Property.FLUX_N));

        // Spectral types :
        newTarget.setSPECTYP(star.getPropertyAsString(Star.Property.SPECTRALTYPES));

        // Object types :
        newTarget.setOBJTYP(star.getPropertyAsString(Star.Property.OTYPELIST));

        // Radial velocity (km/s) (optional) :
        newTarget.setSYSVEL(star.getPropertyAsDouble(Star.Property.RV));
        newTarget.setVELTYP(star.getPropertyAsString(Star.Property.RV_DEF));

        // Identifiers :
        newTarget.setIDS(star.getPropertyAsString(Star.Property.IDS));

        // fix NaN / null values:
        newTarget.checkValues();

        return newTarget;
    }
}
