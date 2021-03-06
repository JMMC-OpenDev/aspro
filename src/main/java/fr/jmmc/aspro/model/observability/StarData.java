/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.observability;

import fr.jmmc.oitools.model.range.Range;
import java.util.List;

/**
 * This class contains several star related data (precessed ra and dec, ha rise/set, and HA observability ranges)
 * @author bourgesl
 */
public final class StarData {

    /** target name */
    private final String name;
    /** precessed target right ascension in decimal hours */
    private double precRA;
    /** precessed target declination in degrees */
    private double precDEC;
    /** hour angle over the minimum elevation */
    private double haElev;
    /** observability HA intervals */
    private List<Range> obsRangesHA;

    /**
     * Public Constructor
     * @param name target name
     */
    public StarData(final String name) {
        this.name = name;
    }

    /**
     * Return the target name
     * @return target name
     */
    public String getName() {
        return name;
    }

    /**
     * Return the precessed target right ascension in decimal hours
     * @return precessed target right ascension in decimal hours
     */
    public double getPrecRA() {
        return precRA;
    }

    /**
     * Define the precessed target right ascension in decimal hours
     * @param precRA precessed target right ascension in decimal hours
     */
    public void setPrecRA(final double precRA) {
        this.precRA = precRA;
    }

    /**
     * Return the precessed target declination in degrees
     * @return precessed target declination in degrees
     */
    public double getPrecDEC() {
        return precDEC;
    }

    /**
     * Define the precessed target declination in degrees
     * @param precDEC precessed target declination in degrees
     */
    public void setPrecDEC(double precDEC) {
        this.precDEC = precDEC;
    }

    /**
     * Return the hour angle over the minimum elevation
     * @return hour angle over the minimum elevation
     */
    public double getHaElev() {
        return haElev;
    }

    /**
     * Define the hour angle over the minimum elevation
     * @param haElev hour angle over the minimum elevation
     */
    public void setHaElev(double haElev) {
        this.haElev = haElev;
    }

    /**
     * Return the observability HA intervals
     * Used by:
     * ExportOBVega.java
     * ExportOBVLTI.java
     * UVCoverageService.java
     *
     * @return observability HA intervals
     */
    public List<Range> getObsRangesHA() {
        return obsRangesHA;
    }

    /**
     * Define the observability HA intervals
     * @param obsRangesHA observability HA intervals
     */
    public void setObsRangesHA(List<Range> obsRangesHA) {
        this.obsRangesHA = obsRangesHA;
    }

    @Override
    public String toString() {
        return name + " [ " + getPrecRA() + ", " + getPrecDEC() + "] - " + getHaElev() + " - " + getObsRangesHA();
    }
}
