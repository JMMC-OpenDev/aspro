/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;

/**
 *
 * @author bourgesl
 */
public final class TargetObservation {

    final ObservationSetting observation;
    final Target target;

    public TargetObservation(final ObservationSetting observation, final Target target) {
        this.observation = observation;
        this.target = target;
    }

    public ObservationSetting getObservation() {
        return observation;
    }

    public Target getTarget() {
        return target;
    }

}
