/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: UpdateObservationEvent.java,v 1.2 2011-01-31 15:25:42 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 *
 ******************************************************************************/
package fr.jmmc.aspro.model.event;

import fr.jmmc.aspro.model.oi.ObservationSetting;

/**
 * This event extends the ObservationEvent to have the changed state
 * @see ChangeType
 */
public final class UpdateObservationEvent extends ObservationEvent {

  /**
   * This enumeration describes change states
   */
  public enum ChangeType {

    /** NOTHING CHANGED */
    NONE,
    /** ONLY UV INFORMATION CHANGED */
    UV,
    /** MAIN INFORMATION CHANGED */
    MAIN
  };
  /** changed flag */
  private ChangeType changed = ChangeType.NONE;

  /**
   * Public constructor
   * @param observation observation related to this event
   */
  public UpdateObservationEvent(final ObservationSetting observation) {
    super(ObservationEventType.DO_UPDATE, observation);
  }

  /**
   * Return the changed flag
   * Used by ObservationManager.fireObservationUpdate()
   *
   * @return changed flag
   */
  public ChangeType getChanged() {
    return changed;
  }

  /**
   * Define the changed flag respecting the following priority :
   * NONE < UV < MAIN
   *
   * @param value value to use
   */
  public void setChanged(final ChangeType value) {
    if (value == ChangeType.NONE || this.changed == ChangeType.MAIN) {
      return;
    }

    this.changed = value;
  }

  /**
   * Return a string representation "ObservationEvent{type=...}"
   * @return "ObservationEvent{type=...}"
   */
  @Override
  public String toString() {
    return "ObservationEvent{type=" + getType() + ", changed=" + getChanged() + "}";
  }
}
