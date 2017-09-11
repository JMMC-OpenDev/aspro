/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.observability;

import fr.jmmc.aspro.model.BestPoPsObservabilityContext;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.service.pops.BestPopsEstimator;
import fr.jmmc.jmcs.util.NumberUtils;
import java.util.ArrayList;
import java.util.List;

/**
 * This class contains intermediate results for the observability of a target using a particular Pop combination
 * @author bourgesl
 */
public final class PopObservabilityData implements Comparable<PopObservabilityData> {

  /** target name */
  private final String targetName;
  /** pop combination */
  private final PopCombination popCombination;
  /** list of HA ranges per BL */
  private final List<List<Range>> rangesBL;
  /** observability estimation */
  private final double estimation;
  /** maximum length of an HA range after merging HA ranges per BL with Rise/Set range */
  private final double maxLength;

  /**
   * Estimator : find the longest observability interval and computes its estimation.
   * 
   * Compute the maximum length of HA observability intervals
   * @param targetName target name
   * @param popCombination pop combination
   * @param rangesBL list of HA ranges per BL
   * @param nValid number of ranges to consider a point is valid
   * @param bpObsCtxLocal observability context (temporary data) having rise/set intervals + night limits in HA
   * @param estimator best PoPs estimator
   * @param doSkipDL skip pop combination if any DL is unobservable (useful for performance and detailed output)
   * @param clearRangesBL true to free rangesBL; false otherwise
   * @return PopObservabilityData instance or null if intersection is empty (unobservable)
   */
  public static PopObservabilityData estimate(final String targetName, final PopCombination popCombination, final List<List<Range>> rangesBL,
          final int nValid, final BestPoPsObservabilityContext bpObsCtxLocal, final BestPopsEstimator estimator,
          final boolean doSkipDL, final boolean clearRangesBL) {

    // Estimate data first:

    // flatten ranges :
    List<Range> ranges;
    for (int i = 0, size = rangesBL.size(); i < size; i++) {
      ranges = rangesBL.get(i);
      if (!ranges.isEmpty()) {
        bpObsCtxLocal.addInFlatRangeLimits(ranges);
      }
    }

    if (clearRangesBL) {
      // recycle memory (to avoid GC):
      bpObsCtxLocal.recycleAll(rangesBL);
    }

    // clear and get merged ranges (temporary use) = empty list with 3 buckets
    // Merge HA ranges (BL) with HA Rise/set ranges (and optionally night limits):
    final List<Range> mergeRanges = bpObsCtxLocal.intersectAndGetMergeRanges(nValid);

    // find the maximum length of HA observable intervals:
    Range maxRange = null;
    Range range;
    for (int i = 0, size = mergeRanges.size(); i < size; i++) {
      range = mergeRanges.get(i);
      if (maxRange == null || range.getLength() > maxRange.getLength()) {
        maxRange = range;
      }
    }

    if (maxRange == null) {
      // intersection is empty:
      if (doSkipDL) {
        return null;
      }
      return new PopObservabilityData(targetName, popCombination,
              (clearRangesBL) ? null : new ArrayList<List<Range>>(rangesBL), 0d, 0d);
    }

    // copy range list as given rangeBL instance is reused:
    final PopObservabilityData popData = new PopObservabilityData(targetName, popCombination,
            (clearRangesBL) ? null : new ArrayList<List<Range>>(rangesBL),
            estimator.compute(maxRange), maxRange.getLength());

    // recycle memory (to avoid GC):
    bpObsCtxLocal.recycleRanges(mergeRanges);

    return popData;
  }

  /**
   * Public constructor
   * @param targetName target name
   * @param popCombination pop combination
   * @param rangesBL list of HA ranges per BL
   * @param estimation observability estimation
   * @param maxLength maximum length of an HA range after merging HA ranges per BL with Rise/Set range
   */
  private PopObservabilityData(final String targetName, final PopCombination popCombination, final List<List<Range>> rangesBL,
          final double estimation, final double maxLength) {
    this.targetName = targetName;
    this.popCombination = popCombination;
    this.rangesBL = rangesBL;
    this.estimation = estimation;
    this.maxLength = maxLength;
  }

  /**
   * Simple comparator that takes into account first the maximum length and then the distance to transit (if maximum length are equals)
   * @param other pop observability data
   * @return Double.compare(maxLength1, maxLength2)
   */
  @Override
  public int compareTo(final PopObservabilityData other) {
    return Double.compare(this.getEstimation(), other.getEstimation());
  }

  /**
   * Return the target name
   * @return target name
   */
  public String getTargetName() {
    return targetName;
  }

  /**
   * Return the maximum length of an HA range after merging HA ranges per BL with Rise/Set range
   * @return maximum length of an HA range after merging HA ranges per BL with Rise/Set range
   */
  public double getMaxLength() {
    return maxLength;
  }

  /**
   * Return the observability estimation (maximum length x transit weight)
   * @return observability estimation
   */
  public double getEstimation() {
    return estimation;
  }

  /**
   * Return the pop combination
   * @return pop combination
   */
  public PopCombination getPopCombination() {
    return popCombination;
  }

  /**
   * Return the list of HA ranges per BL
   * @return list of HA ranges per BL
   */
  public List<List<Range>> getRangesBL() {
    return rangesBL;
  }

  @Override
  public String toString() {
    return this.targetName + " : " + this.popCombination + '[' + NumberUtils.trimTo3Digits(this.estimation) + "] = " + NumberUtils.trimTo3Digits(this.maxLength);
  }
}
