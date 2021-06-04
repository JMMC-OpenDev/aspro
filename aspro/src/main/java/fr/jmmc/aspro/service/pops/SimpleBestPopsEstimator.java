/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service.pops;

import fr.jmmc.oitools.model.range.Range;

/**
 * This simple Best Pops estimator behaves like Aspro2 best PoPs algorithm (until dec 2012): 
 * only use interval length and minimum length to maximize both
 * 
 * @author bourgesl
 */
public final class SimpleBestPopsEstimator implements BestPopsEstimator {

  /**
   * Protected constructor
   */
  SimpleBestPopsEstimator() {
    super();
  }

  /**
   * Returns the estimation for the given observability range (HA) = range length
   * 
   * @param range HA range [-12; 12]
   * @return estimation = range length
   */
  public double compute(final Range range) {
    return range.getLength();
  }

  /**
   * Returns the estimation for a target list = average x minimum
   * 
   * @param average average value of all observability range estimations
   * @param minimum minimum value of all observability range estimations
   * @return estimation = average x minimum
   */
  public double compute(final double average, final double minimum) {
    return average * minimum;
  }
}
