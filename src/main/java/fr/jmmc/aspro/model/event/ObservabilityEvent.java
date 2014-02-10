/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
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
