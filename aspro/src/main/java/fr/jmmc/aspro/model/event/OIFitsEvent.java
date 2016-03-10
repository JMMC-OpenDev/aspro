/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.event;

import fr.jmmc.aspro.model.OIFitsData;

/**
 * This event extends the ObservationEvent to contain only the OIFits structure
 */
public final class OIFitsEvent extends ObservationEvent {

    /** OIFits structures */
    private final OIFitsData oiFitsData;

    /**
     * Public constructor
     * @param oiFitsData OIFits structures
     */
    public OIFitsEvent(final OIFitsData oiFitsData) {
        super(ObservationEventType.OIFITS_DONE);
        this.oiFitsData = oiFitsData;
    }

    /**
     * Return the computed OIFits structures (read only)
     * @return OIFits structures or null
     */
    public OIFitsData getOIFitsData() {
        return this.oiFitsData;
    }
}
