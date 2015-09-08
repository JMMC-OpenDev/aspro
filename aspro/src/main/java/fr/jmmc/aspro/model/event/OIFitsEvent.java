/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.event;

import fr.jmmc.oitools.model.OIFitsFile;
import java.util.List;

/**
 * This event extends the ObservationEvent to contain only the OIFits structure
 */
public final class OIFitsEvent extends ObservationEvent {

  /** OIFits structures */
  private final List<OIFitsFile> oiFitsList;

  /**
   * Public constructor
   * @param oiFitsList OIFits structures
   */
  public OIFitsEvent(final List<OIFitsFile> oiFitsList) {
    super(ObservationEventType.OIFITS_DONE);
    this.oiFitsList = oiFitsList;
  }

  /**
   * Return the computed OIFits structures (read only)
   * @return OIFits structures or null
   */
  public List<OIFitsFile> getOIFitsList() {
    return this.oiFitsList;
  }
}
