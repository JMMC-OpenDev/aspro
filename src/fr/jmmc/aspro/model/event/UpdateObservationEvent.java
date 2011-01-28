package fr.jmmc.aspro.model.event;

import fr.jmmc.aspro.model.oi.ObservationSetting;

/**
 *
 * @author mella
 */
public final class UpdateObservationEvent extends ObservationEvent {

  public enum ChangeType {
    NONE,
    UV,
    MAIN
  };
  private ChangeType changed = ChangeType.NONE;

  public UpdateObservationEvent(final ObservationEventType type, final ObservationSetting observation) {
    super(type, observation);
  }

  public ChangeType getChanged() {
    return changed;
  }

  public void setChanged(ChangeType value) {
    if (value == ChangeType.NONE || this.changed == ChangeType.MAIN) {
      return;
    }

    this.changed = value;
  }
}
