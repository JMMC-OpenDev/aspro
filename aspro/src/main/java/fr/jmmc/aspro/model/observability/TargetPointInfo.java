/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.observability;

import java.util.Date;

/**
 * This class describes a target point (HA, JD, date) corresponding to the position (azimuth / elevation) of the target
 * @author bourgesl
 */
public final class TargetPointInfo {

    /** jd in night range */
    private final double jd;
    /** hour angle [-12; +12] */
    private final double ha;
    /** date/time */
    private final Date date;
    /** azimuth in degrees */
    private final double azimuth;
    /** elevation in degrees */
    private final double elevation;
    /** airmass */
    private final double airmass;

    /**
     * Public constructor
     * @param jd julian date in night range
     * @param ha hour angle
     * @param date date/time (LST or UTC)
     * @param azimuth azimuth in degrees
     * @param elevation elevation in degrees
     * @param airmass airmass
     */
    public TargetPointInfo(final double jd, final double ha, final Date date,
                           final double azimuth, final double elevation,
                           final double airmass) {
        this.jd = jd;
        this.ha = ha;
        this.date = date;
        this.azimuth = (azimuth >= 360) ? azimuth - 360 : azimuth;
        this.elevation = elevation;
        this.airmass = airmass;
    }

    /**
     * Return the jd in night range
     * @return jd in night range
     */
    public double getJd() {
        return jd;
    }

    /**
     * Return the hour angle [-12; +12]
     * @return hour angle
     */
    public double getHa() {
        return ha;
    }

    /**
     * Return the date/time
     * @return date/time
     */
    public Date getDate() {
        return date;
    }

    /**
     * Return the azimuth in degrees
     * @return azimuth in degrees
     */
    public double getAzimuth() {
        return azimuth;
    }

    /**
     * Return the elevation in degrees
     * @return elevation in degrees
     */
    public double getElevation() {
        return elevation;
    }

    /**
     * Return the airmass
     * @return airmass
     */
    public double getAirmass() {
        return airmass;
    }

    /**
     * Return a string representation "[date = elevation]"
     * @return "[date = elevation]"
     */
    @Override
    public String toString() {
        return "[" + this.jd + "][" + this.ha + "][" + this.date + "] = (" + this.azimuth + "°, " + this.elevation + "°) airmass = " + airmass;
    }
}
