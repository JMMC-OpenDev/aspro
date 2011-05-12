/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.event;

import fr.jmmc.aspro.model.WarningContainer;

/**
 * This event extends the ObservationEvent to contain only the warning container
 */
public final class WarningContainerEvent extends ObservationEvent {

  /** warning container */
  private final WarningContainer warningContainer;

  /**
   * Public constructor
   * @param warningContainer warning container
   */
  public WarningContainerEvent(final WarningContainer warningContainer) {
    super(ObservationEventType.WARNINGS_READY);
    this.warningContainer = warningContainer;
  }

  /**
   * Return the warning container
   * @return warning container
   */
  public WarningContainer getWarningContainer() {
    return warningContainer;
  }
}
