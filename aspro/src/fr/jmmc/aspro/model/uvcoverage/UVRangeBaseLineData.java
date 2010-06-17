/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: UVRangeBaseLineData.java,v 1.1 2010-01-08 16:50:53 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.model.uvcoverage;

/**
 * This class extends the UVBaseLineData class to define the second uv coordinates.
 * That defines an uv range corresponding to the minimal and maximal wavelength of the focal instrument (mode).
 * @author bourgesl
 */
public class UVRangeBaseLineData extends UVBaseLineData {

  /** u coordinates for the maximal wavelength */
  private double[] u2 = null;
  /** v coordinates for the maximal wavelength */
  private double[] v2 = null;

  /**
   * Constructor
   * @param name baseline name
   */
  public UVRangeBaseLineData(final String name) {
    super(name);
  }

  public double[] getU2() {
    return u2;
  }

  public void setU2(double[] u2) {
    this.u2 = u2;
  }

  public double[] getV2() {
    return v2;
  }

  public void setV2(double[] v2) {
    this.v2 = v2;
  }

}
