/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.event;

import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;

/**
 * This event extends the ObservationEvent to contain the selected target
 */
public final class TargetSelectionEvent extends ObservationEvent {

  /** selected target */
  private final Target target;

  /**
   * Public constructor
   * @param observation observation related to this event
   * @param target selected target
   */
  public TargetSelectionEvent(final ObservationSetting observation, final Target target) {
    super(ObservationEventType.TARGET_SELECTION_CHANGED, observation);
    this.target = target;
  }

  /**
   * Return the selected target
   * @return selected target
   */
  public Target getTarget() {
    return this.target;
  }
}
