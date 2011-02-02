/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityEvent.java,v 1.1 2011-02-02 17:41:03 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 *
 ******************************************************************************/
package fr.jmmc.aspro.model.event;

import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.oi.ObservationSetting;

/**
 * This event extends the ObservationEvent to have the observability data
 */
public final class ObservabilityEvent extends ObservationEvent {

  /** observability data */
  private final ObservabilityData obsData;

  /**
   * Public constructor
   * @param observation observation related to this event
   * @param obsData observability data
   */
  public ObservabilityEvent(final ObservationSetting observation, final ObservabilityData obsData) {
    super(ObservationEventType.OBSERVABILITY_DONE, observation);
    this.obsData = obsData;
  }

  /**
   * Return the observability data
   * @return observability data
   */
  public ObservabilityData getObservabilityData() {
    return obsData;
  }
}
