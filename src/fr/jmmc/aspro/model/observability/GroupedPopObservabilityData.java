/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.observability;

import fr.jmmc.aspro.service.pops.BestPopsEstimator;
import fr.jmmc.jmcs.util.NumberUtils;
import java.util.List;

/**
 * This class contains the list of Pop observability data for several targets grouped by Pop Combination
 * to estimate the best PoPs combination for the complete list of targets
 * @author bourgesl
 */
public final class GroupedPopObservabilityData implements Comparable<GroupedPopObservabilityData> {

  /** pop combination */
  private final PopCombination popCombination;
  /** Pop data grouped by Pop combination per target */
  private List<PopObservabilityData> popDataList;
  /** observability estimation */
  private double estimation;
  /** total length of HA ranges */
  private double totalLength;
  /** minimum length of an HA range */
  private double minLength;

  /**
   * Public constructor
   * @param popCombination pop combination
   * @param popDataList Pop data grouped by Pop combination per target
   */
  public GroupedPopObservabilityData(final PopCombination popCombination, final List<PopObservabilityData> popDataList) {
    this.popCombination = popCombination;
    this.popDataList = popDataList;
  }

  /**
   * Estimator : computes the global observability estimation.
   * @param estimator best PoPs estimator
   */
  public void estimateData(final BestPopsEstimator estimator) {

    double total = 0d;
    double acc = 0d;
    double min = Double.POSITIVE_INFINITY;
    double minEstimation = 0d;
    double len;

    for (PopObservabilityData popData : this.popDataList) {
      acc += popData.getEstimation();

      len = popData.getMaxLength();
      total += len;

      // min :
      if (len < min) {
        min = len;
        minEstimation = popData.getEstimation();
      }
    }

    this.estimation = estimator.compute(acc / this.popDataList.size(), minEstimation);
    this.totalLength = total;
    this.minLength = min;
  }

  /**
   * Simple comparator that takes into account only the estimation
   * @param other pop merge data
   * @return Double.compare(estimation1, estimation2)
   */
  public int compareTo(final GroupedPopObservabilityData other) {
    return Double.compare(this.estimation, other.getEstimation());
  }

  /**
   * Return the pop combination
   * @return pop combination
   */
  public PopCombination getPopCombination() {
    return this.popCombination;
  }

  /**
   * Return the identifier of the pop combination
   * @return identifier of the pop combination
   */
  public String getIdentifier() {
    return this.popCombination.getIdentifier();
  }

  /**
   * Return the total length of HA ranges
   * @return total length of HA ranges
   */
  public double getTotalLength() {
    return totalLength;
  }

  /**
   * Return the minimum length of an HA range
   * @return minimum length of an HA range
   */
  public double getMinLength() {
    return minLength;
  }

  /**
   * Return the observability estimation
   * @return observability estimation
   */
  public double getEstimation() {
    return this.estimation;
  }

  /**
   * Return the Pop data grouped by Pop combination per target
   * @return Pop data grouped by Pop combination per target
   */
  public List<PopObservabilityData> getPopDataList() {
    return this.popDataList;
  }

  // TODO: use ToStringable
  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder(64).append(getIdentifier()).append(" [");
    for (PopObservabilityData popData : this.popDataList) {
      sb.append('\t').append(popData.getTargetName()).append(" : ").append(NumberUtils.trimTo3Digits(popData.getMaxLength()));
    }
    sb.append(']');
    if (this.estimation != -1d) {
      sb.append('\t').append(NumberUtils.trimTo3Digits(this.estimation));
    }
    return sb.toString();
  }
}
