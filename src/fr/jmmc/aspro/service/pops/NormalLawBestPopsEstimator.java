/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service.pops;

import cern.jet.stat.Probability;
import fr.jmmc.aspro.model.Range;

/**
 * This advanced Best Pops estimator uses the normal distribution (gaussian) 
 * to give more importance to observability ranges convolved by the normal distribution (customizable mean, variance)
 *
 * @author bourgesl
 */
public final class NormalLawBestPopsEstimator implements BestPopsEstimator {

  /* members */
  /** normal distribution mean */
  private final double mean;
  /** normal distribution variance */
  private final double variance;
  /** normalized weight given to average value */
  private final double avgWeight;
  /** normalized weight given to minimum value */
  private final double minWeight;

  /**
   * Protected constructor with given normal distribution parameters and average weight
   * 
   * @param mean normal distribution mean
   * @param sigma normal distribution sigma
   * @param avgWeight average weight [0.0 .. 1.0]
   */
  NormalLawBestPopsEstimator(final double mean, final double sigma, final double avgWeight) {
    this.mean = mean;
    this.variance = sigma * sigma;

    if (avgWeight >= 1d) {
      this.avgWeight = 1d;
      this.minWeight = 0d;
    } else {
      this.avgWeight = avgWeight;
      this.minWeight = (1d - avgWeight);
    }
  }

  /**
   * Returns the estimation for the given observability range (HA) = 
   * area under the Normal (Gaussian) probability density function between range min to range max.
   * 
   * @param range HA range [-12; 12]
   * @return normal probability density integral between min / max
   */
  public double compute(final Range range) {
    return Probability.normal(mean, variance, range.getMax()) - Probability.normal(mean, variance, range.getMin());
  }

  /**
   * Returns the estimation for a target list = average x minimum
   * 
   * @param average average value of all observability range estimations
   * @param minimum minimum value of all observability range estimations
   * @return estimation = average x minimum
   */
  public double compute(final double average, final double minimum) {
    return this.avgWeight * average + this.minWeight * minimum;
  }
}