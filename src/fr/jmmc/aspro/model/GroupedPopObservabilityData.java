/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: GroupedPopObservabilityData.java,v 1.1 2009-12-18 11:52:02 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.model;

import java.util.List;

/**
 * This class contains the list of Pop observability data for several targets grouped by Pop Combination
 * to estimate the best PoPs combination for the complete list of targets
 * @author bourgesl
 */
public class GroupedPopObservabilityData implements Comparable<GroupedPopObservabilityData> {

  /** pop combination */
  private final PopCombination popCombination;
  /** Pop data for the same PoPs combination but several targets */
  private List<PopObservabilityData> popDataList;
  /** observability estimation */
  private double estimation = -1d;

  public GroupedPopObservabilityData(final PopCombination popCombination, final List<PopObservabilityData> popDataList) {
    this.popCombination = popCombination;
    this.popDataList = popDataList;
  }

  /**
   * Estimator 
   */
  public void estimateData() {
    // try a simple estimator (not using any union or intersection check) :

    // estimation = somme(maxLength) * minimum(maxLength) :
    double len;

    double acc = 0d;
    double min = Double.MAX_VALUE;
    for (PopObservabilityData popData : this.popDataList) {
      len = popData.getMaxLength();

      acc += len;

      // min :
      if (len < min) {
        min = len;
      }
    }

    this.estimation = acc * min;
  }

  /**
   * Simple comparator that takes into account only the estimation
   * @param other pop merge data
   * @return Double.compare(estimation1, estimation2)
   */
  public int compareTo(final GroupedPopObservabilityData other) {
    return Double.compare(this.estimation, other.getEstimation());
  }

  public PopCombination getPopCombination() {
    return this.popCombination;
  }

  public String getIdentifier() {
    return this.popCombination.getIdentifier();
  }

  public double getEstimation() {
    return this.estimation;
  }

  public List<PopObservabilityData> getPopDataList() {
    return this.popDataList;
  }

  @Override
  public String toString() {
    final StringBuffer sb = new StringBuffer(64).append(getIdentifier()).append(" [");
    for (PopObservabilityData popData : this.popDataList) {
      sb.append("\t").append(popData.getTargetName()).append(" : ").append(popData.getMaxLength());
    }
    sb.append("]");
    if (this.estimation != -1d) {
      sb.append("\t").append(this.estimation);
    }
    return sb.toString();
  }
}
