/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: GroupedPopObservabilityData.java,v 1.4 2011-01-25 13:48:56 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.3  2010/12/17 15:14:20  bourgesl
 * append char instead of string
 *
 * Revision 1.2  2010/10/22 11:12:02  bourgesl
 * javadoc
 * fixed minimum and maximum methods (Double extrema)
 *
 * Revision 1.1  2010/01/08 16:48:29  bourgesl
 * package refactoring
 *
 * Revision 1.1  2009/12/18 11:52:02  bourgesl
 * added Pops Finder Compatible Mode for a list of targets
 *
 */
package fr.jmmc.aspro.model.observability;

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
  private double estimation = -1d;

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
   * Estimator : computes the observability estimation = somme(maxLength) * minimum(maxLength).
   */
  public void estimateData() {
    // try a simple estimator (not using any union or intersection check) :

    // estimation = somme(maxLength) * minimum(maxLength) :
    double len;

    double acc = 0d;
    double min = Double.POSITIVE_INFINITY;
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

  @Override
  public String toString() {
    final StringBuffer sb = new StringBuffer(64).append(getIdentifier()).append(" [");
    for (PopObservabilityData popData : this.popDataList) {
      sb.append('\t').append(popData.getTargetName()).append(" : ").append(popData.getMaxLength());
    }
    sb.append(']');
    if (this.estimation != -1d) {
      sb.append('\t').append(this.estimation);
    }
    return sb.toString();
  }
}
