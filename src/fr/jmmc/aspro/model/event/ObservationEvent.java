/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservationEvent.java,v 1.2 2011-01-31 15:25:09 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 *
 ******************************************************************************/
package fr.jmmc.aspro.model.event;

import fr.jmmc.aspro.model.oi.ObservationSetting;

/**
 * Base class for Observation events consumed by ObservationListeners.
 */
public class ObservationEvent {

  /** event type */
  private final ObservationEventType type;
  /** observation related to this event */
  private final ObservationSetting observation;

  /**
   * Public constructor
   * @param type event type
   * @param observation observation related to this event
   */
  public ObservationEvent(final ObservationEventType type, final ObservationSetting observation) {
    this.type = type;
    this.observation = observation;
  }

  /**
   * Return the event type
   * @return event type
   */
  public final ObservationEventType getType() {
    return type;
  }

  /**
   * Return the observation related to this event
   * @return observation related to this event
   */
  public final ObservationSetting getObservation() {
    return observation;
  }

  /**
   * Return a string representation "ObservationEvent{type=...}"
   * @return "ObservationEvent{type=...}"
   */
  @Override
  public String toString() {
    return "ObservationEvent{type=" + getType() + "}";
  }
}
