/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.event;

import fr.jmmc.aspro.model.oi.Target;

/**
 * This event extends the ObservationEvent to contain the selected target
 */
public final class TargetSelectionEvent extends ObservationEvent {

  /** selected target */
  private final Target target;

  /**
   * Public constructor
   * @param target selected target
   */
  public TargetSelectionEvent(final Target target) {
    super(ObservationEventType.TARGET_SELECTION_CHANGED);
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
