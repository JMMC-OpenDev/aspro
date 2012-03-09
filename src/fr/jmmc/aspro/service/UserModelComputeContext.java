/** *****************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ***************************************************************************** */
package fr.jmmc.aspro.service;

import fr.jmmc.jmal.model.ModelComputeContext;

/**
 * This class holds several variables used during user model computations: image data and frequencies ...
 *
 * @author bourgesl
 */
public final class UserModelComputeContext extends ModelComputeContext {

  /* members */
  /** number of data points */
  private final int n1D;
  /** flattened data points (1D) */
  private final float[] data1D;
  /** spatial coordinates along the column axis (rad) corresponding to data points */
  private final float[] colCoord1D;
  /** spatial coordinates along the row axis (rad) corresponding to data points */
  private final float[] rowCoord1D;

  /**
   * Protected constructor
   *
   * @param freqCount uv frequency count used to preallocate arrays
   * @param n1D number of data points
   * @param data1D flattened data points (1D)
   * @param colCoord1D spatial coordinates along the column axis (rad) corresponding to data points
   * @param rowCoord1D spatial coordinates along the row axis (rad) corresponding to data points
   */
  UserModelComputeContext(final int freqCount, final int n1D, final float[] data1D,
          final float[] colCoord1D, final float[] rowCoord1D) {

    super(freqCount);
    this.n1D = n1D;
    this.data1D = data1D;
    this.colCoord1D = colCoord1D;
    this.rowCoord1D = rowCoord1D;
  }

  /**
   * Return the number of data points
   * @return number of data points
   */
  int getN1D() {
    return n1D;
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
