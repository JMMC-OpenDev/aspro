package fr.jmmc.aspro.model.event;

import fr.jmmc.aspro.model.oi.ObservationSetting;

/**
 *  Base class for ObservationListener events.
 */
public class ObservationEvent {

  private final ObservationSetting observation;
  private final ObservationEventType type;

  public ObservationEvent(final ObservationEventType type, final ObservationSetting observation) {
    this.type = type;
    this.observation = observation;
  }

  public final ObservationEventType getType() {
    return type;
  }

  public final ObservationSetting getObservation() {
    return observation;
  }

  @Override
  public String toString() {
    return "ObservationEvent{type=" + type + '}';
  }
}
