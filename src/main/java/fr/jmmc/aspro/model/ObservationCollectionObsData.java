/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.oi.ObservationCollection;
import fr.jmmc.aspro.model.oi.Station;
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
        final StringBuilder sb = new StringBuilder(32);

        // Iterate over Observability data (multi conf) :
        for (ObservabilityData obsData : getObsDataList()) {
            final List<Station> stations = obsData.getStationList();
            if (stations != null) {
                sb.setLength(0);

                // no Pops:
                for (Station station : stations) {
                    sb.append(station.getName());
                    sb.append('-');
                }
                sb.deleteCharAt(sb.length() - 1);
                final String confName = sb.toString();
                this.confNames.add(confName);

                // has Pops:
                if (obsData.getStationNameWithPops(sb)) {
                    this.confLabels.add(sb.toString());
                } else {
                    this.confLabels.add(confName);
                }
            }
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
    public final List<String> getConfigurationNames() {
        return this.confNames;
    }

    /**
     * Return the configuration labels using the format 'CONF + PoPs'
     * @return the configuration labels using the format 'CONF + PoPs'
     */
    public final List<String> getConfigurationLabels() {
        return this.confLabels;
    }

    /**
     * Return a string containing up to 3 configuration(s) separated with the given delimiter string
     * or 'MULTI CONFIGURATION' where space characters are replaced by '-'
     * @param separator delimiter string
     * @return string containing up to 3 configuration(s) or 'MULTI CONFIGURATION'
     */
    @Override
    public String getDisplayConfigurations(final String separator) {
        final List<String> labels = getConfigurationLabels();
        final int size = labels.size();
        final String result;
        if (size <= 0) {
            result = "";
        } else if (size <= 1) {
            result = labels.get(0); // with PoPs
        } else if (size <= 3) {
            final StringBuilder sb = new StringBuilder(32);

            // Without PoPs:
            for (String label : getConfigurationNames()) {
                sb.append(label).append(separator);
            }
            sb.delete(sb.length() - separator.length(), sb.length());

            result = sb.toString();
        } else {
            result = fr.jmmc.aspro.AsproConstants.MULTI_CONF;
        }
        return result;
    }

}
