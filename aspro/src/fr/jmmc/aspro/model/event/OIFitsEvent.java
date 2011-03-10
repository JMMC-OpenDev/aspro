/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: OIFitsEvent.java,v 1.1 2011-03-01 17:08:05 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 *
 */
package fr.jmmc.aspro.model.event;

import fr.jmmc.oitools.model.OIFitsFile;

/**
 * This event extends the ObservationEvent to contain only the OIFits structure
 */
public final class OIFitsEvent extends ObservationEvent {

  /** OIFits structure */
  private final OIFitsFile oiFitsFile;

  /**
   * Public constructor
   * @param oiFitsFile OIFits structure
   */
  public OIFitsEvent(final OIFitsFile oiFitsFile) {
    super(ObservationEventType.OIFITS_DONE);
    this.oiFitsFile = oiFitsFile;
  }

  /**
   * Return the computed OIFits structure (read only)
   * @return OIFits structure or null
   */
  public final OIFitsFile getOIFitsFile() {
    return this.oiFitsFile;
  }
}
