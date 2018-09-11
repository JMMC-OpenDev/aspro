/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.oi.ObservationCollection;
import java.util.ArrayList;
import java.util.List;

/**
 * This class extends the observation collection to have observability data
 * @author bourgesl
 */
public class ObservationCollectionObsData extends ObservationCollection {

    /** observability data */
    private final List<ObservabilityData> obsDataList;
    /** configuration names using the format 'CONF' */
    private final List<String> confNames;
    /** configuration names using the format 'CONF + PoPs' */
    private final List<String> confLabels;

    /**
     * Public constructor : copy information from the given observation collection (by reference)
     * @param obsCollection observation collection to copy
     * @param obsDataList observability data
     */
    public ObservationCollectionObsData(final ObservationCollection obsCollection,
                                        final List<ObservabilityData> obsDataList) {
        super(obsCollection);
        this.obsDataList = obsDataList;

        this.confNames = new ArrayList<String>(size());
        this.confLabels = new ArrayList<String>(size());
        prepareConfigurationNames();
    }

    /**
     * Prepare the configuration names using the format 'CONF + PoPs'
     */
    private final void prepareConfigurationNames() {
        final StringBuilder sb = new StringBuilder();

        // Iterate over Observability data (multi conf) :
        for (ObservabilityData obsData : getObsDataList()) {
            sb.append(obsData.getStationNames());
            this.confNames.add(sb.toString());

            if (!obsData.isUserPops() && obsData.getBestPops() != null) {
                obsData.getBestPops().toString(sb);
            }

            this.confLabels.add(sb.toString());
            sb.setLength(0);
        }
    }

    /**
     * Return the observability data
     * @return observability data
     */
    public final List<ObservabilityData> getObsDataList() {
        return this.obsDataList;
    }

    /**
     * Return the first observability data
     * @return first observability data
     */
    public final ObservabilityData getFirstObsData() {
        return this.getObsDataList().get(0);
    }

    /**
     * Return the flag to find baseline limits
     * @return flag to find baseline limits
     */
    public final boolean isDoBaseLineLimits() {
        return this.getFirstObsData().isDoBaseLineLimits();
    }

    /**
     * Return the flag to produce detailed output with all BL / horizon / rise intervals per target
     * @return flag to produce detailed output with all BL / horizon / rise intervals per target
     */
    public final boolean isDoDetailedOutput() {
        return this.getFirstObsData().isDoDetailedOutput();
    }

    /**
     * Return the configuration names using the format 'CONF'
     * @return the configuration names using the format 'CONF'
     */
    public final List<String> getConfigurationName() {
        return this.confNames;
    }

    /**
     * Return the configuration labels using the format 'CONF + PoPs'
     * @return the configuration labels using the format 'CONF + PoPs'
     */
    public final List<String> getConfigurationLabel() {
        return this.confLabels;
    }
}
