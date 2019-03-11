/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

import fr.jmmc.aspro.model.oi.Target;

/**
 * Basic wrapper to return results from TargetUtils#matchTargetCoordinates()
 * @author bourgesl
 */
public final class TargetMatch {

    private final Target match;
    private final double distance;

    public TargetMatch(final Target match) {
        this(match, 0.0); // means equal
    }
    
    public TargetMatch(final Target match, final double distance) {
        this.match = match;
        this.distance = distance;
    }

    public Target getMatch() {
        return match;
    }

    public double getDistance() {
        return distance;
    }

    @Override
    public String toString() {
        return "TargetMatch{" + "match=" + match + ", distance=" + distance + '}';
    }

}
