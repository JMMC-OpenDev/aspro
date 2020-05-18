/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.uvcoverage;

import fr.jmmc.aspro.model.BaseLine;
import java.util.Arrays;

/**
 * This class extends the UVBaseLineData class to define the second uv coordinates.
 * That defines an uv range corresponding to the minimal and maximal wavelength of the focal instrument (mode).
 * @author bourgesl
 */
public final class UVRangeBaseLineData extends UVBaseLineData {

    /** base line (used by OIFits) */
    private final BaseLine bl;
    /** u coordinates for the minimal wavelength */
    private final double[] uWMin;
    /** v coordinates for the minimal wavelength */
    private final double[] vWMin;
    /** u coordinates for the maximal wavelength */
    private final double[] uWMax;
    /** v coordinates for the maximal wavelength */
    private final double[] vWMax;

    /**
     * Constructor
     * @param bl base line
     * @param nPoints number of valid U/V values
     * @param u U coords (m)
     * @param v V coords (m)
     * @param uWMin u coordinates for the minimal wavelength (m)
     * @param vWMin v coordinates for the minimal wavelength (m)
     * @param uWMax u coordinates for the maximal wavelength (m)
     * @param vWMax v coordinates for the maximal wavelength (m)
     */
    public UVRangeBaseLineData(final BaseLine bl,
                               final double u, final double v,
                               final double uWMin, final double vWMin,
                               final double uWMax, final double vWMax) {
        this(bl, 1,
                new double[]{u}, new double[]{v},
                new double[]{uWMin}, new double[]{vWMin},
                new double[]{uWMax}, new double[]{vWMax});
    }

    /**
     * Constructor
     * @param bl base line
     * @param nPoints number of valid U/V values
     * @param u U coords (m)
     * @param v V coords (m)
     * @param uWMin u coordinates for the minimal wavelength (m)
     * @param vWMin v coordinates for the minimal wavelength (m)
     * @param uWMax u coordinates for the maximal wavelength (m)
     * @param vWMax v coordinates for the maximal wavelength (m)
     */
    public UVRangeBaseLineData(final BaseLine bl, final int nPoints,
                               final double[] u, final double[] v,
                               final double[] uWMin, final double[] vWMin,
                               final double[] uWMax, final double[] vWMax) {
        super(bl.getName(), nPoints, u, v);
        this.bl = bl;
        this.uWMin = uWMin;
        this.vWMin = vWMin;

        this.uWMax = uWMax;
        this.vWMax = vWMax;
    }

    /**
     * Constructor
     * @param name baseline name
     * @param nPoints number of valid U/V values
     * @param u U coords (m)
     * @param v V coords (m)
     * @param uWMin u coordinates for the minimal wavelength (m)
     * @param vWMin v coordinates for the minimal wavelength (m)
     * @param uWMax u coordinates for the maximal wavelength (m)
     * @param vWMax v coordinates for the maximal wavelength (m)
     */
    public UVRangeBaseLineData(final String name, final int nPoints,
                               final double[] u, final double[] v,
                               final double[] uWMin, final double[] vWMin,
                               final double[] uWMax, final double[] vWMax) {
        super(name, nPoints, u, v);
        this.bl = null;
        this.uWMin = uWMin;
        this.vWMin = vWMin;

        this.uWMax = uWMax;
        this.vWMax = vWMax;
    }

    public BaseLine getBaseLine() {
        return this.bl;
    }

    public double[] getUWMin() {
        return uWMin;
    }

    public double[] getVWMin() {
        return this.vWMin;
    }

    public double[] getUWMax() {
        return this.uWMax;
    }

    public double[] getVWMax() {
        return this.vWMax;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("UVRangeBaseLineData{name=").append(getName());
        sb.append(", nPoints=").append(getNPoints());

        sb.append(", u=").append(Arrays.toString(getU()));
        sb.append(", v=").append(Arrays.toString(getV()));

        sb.append(", bl=").append(bl);
        sb.append(", uWMin=").append(Arrays.toString(uWMin));
        sb.append(", vWMin=").append(Arrays.toString(vWMin));
        sb.append(", uWMax=").append(Arrays.toString(uWMax));
        sb.append(", vWMax=").append(Arrays.toString(vWMax));
        sb.append('}');
        return sb.toString();
    }


}
