/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: WarningContainerEvent.java,v 1.1 2011-01-31 15:26:31 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
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
    super(ObservationEventType.WARNINGS_READY, null);
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
