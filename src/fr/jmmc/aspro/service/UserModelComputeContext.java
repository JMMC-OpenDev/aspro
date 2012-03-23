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
  /** user model data */
  private final UserModelData modelData;

  /**
   * Protected constructor
   *
   * @param freqCount uv frequency count used to preallocate arrays
   * @param modelData user model data
   */
  UserModelComputeContext(final int freqCount, final UserModelData modelData) {

    super(freqCount);
    this.modelData = modelData;
  }

  /**
   * Return the user model data
   * @return user model data
   */
  UserModelData getUserModelData() {
    return modelData;
  }
}
