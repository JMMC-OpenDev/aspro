/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.uvcoverage;

import fr.jmmc.aspro.model.BaseLine;

/**
 * This class extends the UVBaseLineData class to define the second uv coordinates.
 * That defines an uv range corresponding to the minimal and maximal wavelength of the focal instrument (mode).
 * @author bourgesl
 */
public final class UVRangeBaseLineData extends UVBaseLineData {

  /** base line (used by OIFits) */
  private final BaseLine bl;
  /** u coordinates for the minimal wavelength */
  private double[] uWMin = null;
  /** v coordinates for the minimal wavelength */
  private double[] vWMin = null;
  /** u coordinates for the maximal wavelength */
  private double[] uWMax = null;
  /** v coordinates for the maximal wavelength */
  private double[] vWMax = null;

  /**
   * Constructor
   * @param bl baseline
   */
  public UVRangeBaseLineData(final BaseLine bl) {
    super(bl.getName());
    this.bl = bl;
  }

  public BaseLine getBaseLine() {
    return this.bl;
  }

  public double[] getUWMin() {
    return uWMin;
  }

  public void setUWMin(double[] uWMin) {
    this.uWMin = uWMin;
  }

  public double[] getVWMin() {
    return this.vWMin;
  }

  public void setVWMin(final double[] vWMin) {
    this.vWMin = vWMin;
  }

  public double[] getUWMax() {
    return this.uWMax;
  }

  public void setUWMax(final double[] uWMax) {
    this.uWMax = uWMax;
  }

  public double[] getVWMax() {
    return this.vWMax;
  }

  public void setVWMax(final double[] vWMax) {
    this.vWMax = vWMax;
  }
}
