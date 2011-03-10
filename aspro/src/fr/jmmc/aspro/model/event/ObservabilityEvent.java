/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityEvent.java,v 1.2 2011-02-24 17:14:12 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2011/02/02 17:41:03  bourgesl
 * new event to transfer Observability Data
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.model.event;

import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.oi.ObservationCollection;
import java.util.List;

/**
 * This event extends the ObservationEvent to have the observability data
 */
public final class ObservabilityEvent extends ObservationEvent {

  /** observability data */
  private final List<ObservabilityData> obsDataList;

  /**
   * Public constructor
   * @param obsCollection observation collection used by computations
   * @param obsDataList observability data
   */
  public ObservabilityEvent(final ObservationCollection obsCollection, final List<ObservabilityData> obsDataList) {
    super(ObservationEventType.OBSERVABILITY_DONE, obsCollection);
    this.obsDataList = obsDataList;
  }

  /**
   * Return the observability data
   * @return observability data
   */
  public List<ObservabilityData> getObservabilityData() {
    return obsDataList;
  }
}
