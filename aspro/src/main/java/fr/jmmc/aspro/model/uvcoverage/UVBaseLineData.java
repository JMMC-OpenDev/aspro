/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.uvcoverage;

/**
 * This class contains uv coordinates related to a baseline used by the UVCoverageData class
 * @author bourgesl
 */
public class UVBaseLineData {

    /** baseline name */
    private final String name;
    /** number of uv points */
    private final int nPoints;
    /** u coordinates */
    private final double[] u;
    /** v coordinates */
    private final double[] v;

    /**
     * Constructor
     * @param name baseline name
     * @param nPoints number of valid U/V values
     * @param u U coords (m)
     * @param v V coords (m)
     */
    public UVBaseLineData(final String name, final int nPoints,
                          final double[] u, final double[] v) {
        this.name = name;
        this.nPoints = nPoints;
        this.u = u;
        this.v = v;
    }

    public final String getName() {
        return name;
    }

    public final int getNPoints() {
        return this.nPoints;
    }

    public final double[] getU() {
        return u;
    }

    public final double[] getV() {
        return v;
    }
}
