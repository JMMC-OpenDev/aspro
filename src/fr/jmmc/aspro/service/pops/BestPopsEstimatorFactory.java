/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service.pops;

/**
 * This factory provides several BestPopsEstimator implementations
 * @author bourgesl
 */
public final class BestPopsEstimatorFactory {

  /** Simple estimator singleton (not configurable) */
  private final static BestPopsEstimator simpleBestPopsEstimator = new SimpleBestPopsEstimator();

  /**
   * Best Pops algorithm enumeration
   */
  public enum Algorithm {

    /** Simple estimator = only use interval length and minimum length to maximize both */
    Simple,
    /** Normal distribution (gaussian) estimator with mean = 0.0 and configurable variance  */
    Transit,
    /** Normal distribution (gaussian) estimator with given mean and configurable variance  */
    HALimits
  }

  /**
   * Forbidden constructor
   */
  private BestPopsEstimatorFactory() {
    super();
  }

  /**
   * Return the Simple estimator singleton
   * @return Simple estimator singleton
   */
  public static BestPopsEstimator getSimpleEstimator() {
    return simpleBestPopsEstimator;
  }

  /**
   * Return the Normal distribution (gaussian) estimator with mean = 0.0
   * @param criteriaSigma criteria on sigma @see #getSigma(fr.jmmc.aspro.service.pops.Criteria)
   * @param criteriaAverageWeight criteria on average weight @see #getAverageWeight(fr.jmmc.aspro.service.pops.Criteria) 
   * @return new NormalLawBestPopsEstimator instance
   */
  public static BestPopsEstimator getTransitEstimator(final Criteria criteriaSigma, final Criteria criteriaAverageWeight) {
    return new NormalLawBestPopsEstimator(0d, getSigma(criteriaSigma), getAverageWeight(criteriaAverageWeight));
  }

  /**
   * Return the Normal distribution (gaussian) estimator with mean = haCenter
   * @param haCenter ha value to use as normal distribution mean value 
   * @param criteriaSigma criteria on sigma @see #getSigma(fr.jmmc.aspro.service.pops.Criteria)
   * @param criteriaAverageWeight criteria on average weight @see #getAverageWeight(fr.jmmc.aspro.service.pops.Criteria) 
   * @return new NormalLawBestPopsEstimator instance
   */
  public static BestPopsEstimator getHALimitsEstimator(final double haCenter, final Criteria criteriaSigma, final Criteria criteriaAverageWeight) {
    return new NormalLawBestPopsEstimator(haCenter, getSigma(criteriaSigma), getAverageWeight(criteriaAverageWeight));
  }

  /**
   * Return the sigma value for the given criteria:
   * - SMALLEST: 0.5h
   * - SMALL:      1h
   * - MEDIUM:     2h
   * - LARGE:      3h
   * - LARGEST:    4h
   * 
   * @param criteria criteria to use
   * @return sigma value
   */
  public static double getSigma(final Criteria criteria) {
    switch (criteria) {
      default:
      case MEDIUM:
        return 2d;
      case SMALLEST:
        return 0.5d;
      case SMALL:
        return 1d;
      case LARGE:
        return 3d;
      case LARGEST:
        return 4d;
    }
  }

  /**
   * Return the average weight for the given criteria:
   * - SMALLEST:   0.2
   * - SMALL:      0.4
   * - MEDIUM:     0.6
   * - LARGE:      0.8
   * - LARGEST:    1.0
   * 
   * @param criteria criteria to use
   * @return sigma value
   */
  public static double getAverageWeight(final Criteria criteria) {
    switch (criteria) {
      default:
      case MEDIUM:
        return 0.6d;
      case SMALLEST:
        return 0.2d;
      case SMALL:
        return 0.4d;
      case LARGE:
        return 0.8d;
      case LARGEST:
        return 1d;
    }
  }
}
