/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.event;

import fr.jmmc.aspro.model.ObservationVersion;
import fr.jmmc.aspro.model.oi.ObservationCollection;
import fr.jmmc.aspro.model.oi.ObservationSetting;

/**
 * Base class for Observation events consumed by ObservationListeners
 */
public class ObservationEvent {

  /** event type */
  private final ObservationEventType type;
  /** observation related to this event (can be null) */
  private final ObservationSetting observation;
  /** observation collection used by computations (can be null) */
  private final ObservationCollection obsCollection;

  /**
   * Protected constructor
   * @param type event type
   */
  protected ObservationEvent(final ObservationEventType type) {
    this.type = type;
    this.observation = null;
    this.obsCollection = null;
  }

  /**
   * Public constructor dealing with an observation
   * @param type event type
   * @param observation observation related to this event
   */
  public ObservationEvent(final ObservationEventType type, final ObservationSetting observation) {
    this.type = type;
    this.observation = observation;
    this.obsCollection = null;
  }

  /**
   * Public constructor
   * @param type event type
   * @param obsCollection observation collection related to this event
   */
  public ObservationEvent(final ObservationEventType type, final ObservationCollection obsCollection) {
    this.type = type;
    this.observation = null;
    this.obsCollection = obsCollection;
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
   * @return observation related to this event or null if undefined
   */
  public final ObservationSetting getObservation() {
    return observation;
  }

  /**
   * Return the observation collection used by computations
   *
   * @return observation collection used by computations
   */
  public final ObservationCollection getObservationCollection() {
    return this.obsCollection;
  }

  /**
   * Return the observation version
   * @return observation version
   */
  public final ObservationVersion getVersion() {
    if (this.obsCollection != null) {
      return this.obsCollection.getVersion();
    }
    if (this.observation != null) {
      return this.observation.getVersion();
    }
    return null;
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
