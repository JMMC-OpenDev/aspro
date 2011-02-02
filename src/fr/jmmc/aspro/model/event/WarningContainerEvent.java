/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: WarningContainerEvent.java,v 1.2 2011-02-02 17:40:36 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2011/01/31 15:26:31  bourgesl
 * new event to contain warnings
 *
 */
package fr.jmmc.aspro.model.event;

import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.oi.ObservationSetting;

/**
 * This event extends the ObservationEvent to contain only the warning container
 */
public final class WarningContainerEvent extends ObservationEvent {

  /** warning container */
  private final WarningContainer warningContainer;

  /**
   * Public constructor
   * @param observation observation related to this event
   * @param warningContainer warning container
   */
  public WarningContainerEvent(final ObservationSetting observation, final WarningContainer warningContainer) {
    super(ObservationEventType.WARNINGS_READY, observation);
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
