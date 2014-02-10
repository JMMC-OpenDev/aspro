/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service.pops;

import fr.jmmc.aspro.model.Range;

/**
 * This interface defines two methods to be implemented by a Best Pops estimator:
 * - double <- estimate(Range) for a target observability interval
 * - double <- estimate(double avg_estimation, double min_estimation) for a target list
 * 
 * @author bourgesl
 */
public interface BestPopsEstimator {

  /**
   * Returns the estimation for the given observability range (HA).
   * 
   * @param range HA range [-12; 12]
   * @return estimation
   */
  public double compute(final Range range);

  /**
   * Returns the estimation for a target list.
   * 
   * @param average average value of all observability range estimations
   * @param minimum minimum value of all observability range estimations
   * @return estimation
   */
  public double compute(final double average, final double minimum);
}
