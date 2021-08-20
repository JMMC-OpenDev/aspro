/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.observability;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * This class contains several results for the observability of a target
 *
 * @author bourgesl
 */
public final class StarObservabilityData {

    /* type of data */
    /** star observability */
    public final static int TYPE_STAR = 0; // red
    /** calibrator observability */
    public final static int TYPE_CALIBRATOR = 1; // blue
    /** rise/set intervals */
    public final static int TYPE_RISE_SET = 2; // green
    /** horizon intervals */
    public final static int TYPE_HORIZON = 4; // violet
    /** moon distance OK intervals */
    public final static int TYPE_MOON_DIST = 3; // yellow
    /** wind intervals */
    public final static int TYPE_WIND = 5; // cyan
    /** baseline intervals */
    public final static int TYPE_BASE_LINE = 6; // automatic color

    /* label of main types */
    public final static String LABEL_STAR = "Science";
    public final static String LABEL_CALIBRATOR = "Calibrator";
    public final static String LABEL_CALIBRATOR_ORPHAN = "Orphan calibrator";

    /* members */
    /** name of the target */
    private final String targetName;
    /** additional information on data (rise/set, horizon, base line ...) */
    private final String info;
    /** type of data */
    private final int type;
    /** visible date intervals */
    private final List<DateTimeInterval> visible = new ArrayList<DateTimeInterval>(3);
    /** visible date intervals without soft limits (HA, moon, wind, elev) */
    private List<DateTimeInterval> visibleNoSoftLimits = null;
    /** description of visible date intervals without soft limits (HA, moon, wind, elev) */
    private String visibleNoSoftLimitsDesc = null;
    /** list of visible date intervals for every Vcm limits */
    private List<List<DateTimeInterval>> visibleVcmLimits = null;
    /** list of every Vcm limits (description) */
    private List<String> vcmLimits = null;
    /** transit date */
    private Date transitDate;
    /** sampled target positions (ha, az, el) keyed by date */
    private final Map<Date, TargetPositionDate> targetPositions = new LinkedHashMap<Date, TargetPositionDate>(8);

    /**
     * Constructor
     * @param targetName target name
     * @param type type of observability
     */
    public StarObservabilityData(final String targetName, final int type) {
        this(targetName, null, type);
    }

    /**
     * Constructor
     * @param targetName target name
     * @param info additional information on data (moon, rise/set, horizon, base line ...)
     * @param type type of observability
     */
    public StarObservabilityData(final String targetName, final String info, final int type) {
        this.targetName = targetName;
        this.info = info;
        this.type = type;
    }

    /**
     * Return the name of the target
     * @return name of the target
     */
    public String getTargetName() {
        return targetName;
    }

    /**
     * Return the additional information on data (rise/set, horizon, base line ...)
     * @return additional information on data (rise/set, horizon, base line ...)
     */
    public String getInfo() {
        return info;
    }

    /**
     * Return the legend label (Science or Calibrator target else return additional information on data)
     * @param idx type to use
     * @return string representing this type
     */
    public String getLegendLabel(final int idx) {
        switch (idx) {
            case TYPE_STAR:
                return LABEL_STAR;
            case TYPE_CALIBRATOR:
                return LABEL_CALIBRATOR;
            default:
        }
        return this.getInfo();
    }

    /**
     * Return the type of data
     * @return type of data
     */
    public int getType() {
        return type;
    }

    /**
     * Return the visible date intervals
     * @return visible date intervals
     */
    public List<DateTimeInterval> getVisible() {
        return visible;
    }

    /**
     * Return the visible date intervals without soft limits (HA, moon, wind, elev)
     * @return visible date intervals without soft limits (HA, moon, wind, elev)
     */
    public List<DateTimeInterval> getVisibleNoSoftLimits() {
        return visibleNoSoftLimits;
    }

    /**
     * Define the visible date intervals without soft limits (HA, moon, wind, elev)
     * @param ranges visible date intervals without soft limits (HA, moon, wind, elev)
     */
    public void setVisibleNoSoftLimits(final List<DateTimeInterval> ranges) {
        visibleNoSoftLimits = ranges;
    }

    /**
    * Return the description of visible date intervals without soft limits (HA, moon, wind, elev)
    * @return description of visible date intervals without soft limits (HA, moon, wind, elev)
    */
    public String getVisibleNoSoftLimitsDesc() {
        return visibleNoSoftLimitsDesc;
    }

    /**
    * Define the description of visible date intervals without soft limits (HA, moon, wind, elev)
    * @param visibleNoSoftLimitsDesc description of visible date intervals without soft limits (HA, moon, wind, elev)
    */
    public void setVisibleNoSoftLimitsDesc(final String visibleNoSoftLimitsDesc) {
        this.visibleNoSoftLimitsDesc = visibleNoSoftLimitsDesc;
    }

    /**
     * Return the list of every Vcm limits (description)
     * @return list of every Vcm limits (description)
     */
    public List<String> getVcmLimits() {
        return vcmLimits;
    }

    /**
     * Return the list of visible date intervals for every Vcm limits
     * @return list of visible date intervals for every Vcm limits
     */
    public List<List<DateTimeInterval>> getVisibleVcmLimits() {
        return visibleVcmLimits;
    }

    /**
     * Define the list of visible date intervals for every Vcm limits
     * @param vcmLimits list of every Vcm limits (description)
     * @param rangesList list of visible date intervals for every Vcm limits
     */
    public void setVisibleVcmLimits(final List<String> vcmLimits, final List<List<DateTimeInterval>> rangesList) {
        this.vcmLimits = vcmLimits;
        this.visibleVcmLimits = rangesList;
    }

    /**
     * Return the transit date
     * @return transit date
     */
    public Date getTransitDate() {
        return transitDate;
    }

    /**
     * Define the transit date
     * @param transitDate transit date
     */
    public void setTransitDate(final Date transitDate) {
        this.transitDate = transitDate;
    }

    /**
     * Return the sampled target positions (ha, az, el) keyed by date
     * @return sampled target positions (ha, az, el) keyed by date
     */
    public Map<Date, TargetPositionDate> getTargetPositions() {
        return targetPositions;
    }

    /**
     * Return the target position (ha, az, el) at the given date
     * @param date date to look up
     * @return target position (ha, az, el) or null if not found
     */
    public TargetPositionDate getTargetPosition(final Date date) {
        return targetPositions.get(date);
    }

    @Override
    public String toString() {
        return getTargetName() + " " + ((this.info != null) ? this.info : "") + " : " + getVisible();
    }
}
