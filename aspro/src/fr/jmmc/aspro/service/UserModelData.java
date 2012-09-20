/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

/**
 * This class gathers prepared user model data as 1D arrays (normalized flux and X/Y spatial coordinates) 
 * to compute direct Fourier transform
 * @author bourgesl
 */
public final class UserModelData {

  /* members */
  /** number of data points */
  private int nData = 0;
  /** flattened data points (1D) */
  private float[] data1D = null;
  /** spatial coordinates along the column axis (rad) corresponding to data points */
  private float[] colCoord1D = null;
  /** spatial coordinates along the row axis (rad) corresponding to data points */
  private float[] rowCoord1D = null;

  /**
   * Public constructor
   */
  public UserModelData() {
    super();
  }

  /**
   * Define model data
   *
   * @param nData number of data points
   * @param data1D flattened data points (1D)
   * @param colCoord1D spatial coordinates along the column axis (rad) corresponding to data points
   * @param rowCoord1D spatial coordinates along the row axis (rad) corresponding to data points
   * @return this instance
   */
  public UserModelData set(final int nData, final float[] data1D, final float[] colCoord1D, final float[] rowCoord1D) {
    this.nData = nData;
    this.data1D = data1D;
    this.colCoord1D = colCoord1D;
    this.rowCoord1D = rowCoord1D;
    return this;
  }

  /**
   * Return the number of data points
   * @return number of data points
   */
  int getNData() {
    return nData;
  }

  /**
   * Return the flattened data points (1D)
   * @return flattened data points (1D)
   */
  float[] getData1D() {
    return data1D;
  }

  /**
   * Return the spatial coordinates along the column axis (rad) corresponding to data points
   * @return spatial coordinates along the column axis (rad) corresponding to data points
   */
  float[] getColCoord1D() {
    return colCoord1D;
  }

  /**
   * Return the spatial coordinates along the row axis (rad) corresponding to data points
   * @return spatial coordinates along the row axis (rad) corresponding to data points
   */
  float[] getRowCoord1D() {
    return rowCoord1D;
  }
}
