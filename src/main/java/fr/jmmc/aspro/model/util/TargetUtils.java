/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.jmal.ALX;
import fr.jmmc.jmal.CoordUtils;
import fr.jmmc.jmcs.util.NumberUtils;
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

}
