/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.observability;

import fr.jmmc.aspro.model.ObservabilityContext;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.service.pops.BestPopsEstimator;
import fr.jmmc.jmcs.util.NumberUtils;
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
  private List<List<Range>> rangesBL;
  /** observability estimation */
  private double estimation;
  /** maximum length of an HA range after merging HA ranges per BL with Rise/Set range */
  private double maxLength;

  /**
   * Public constructor
   * @param targetName target name
   * @param popCombination pop combination
   * @param rangesBL list of HA ranges per BL
   */
  public PopObservabilityData(final String targetName, final PopCombination popCombination, final List<List<Range>> rangesBL) {
    this.targetName = targetName;
    this.popCombination = popCombination;
    this.rangesBL = rangesBL;
  }

  /**
   * Estimator : find the longest observability interval and computes its estimation.
   * 
   * Compute the maximum length of HA observability intervals
   * @param nValid number of ranges to consider a point is valid
   * @param obsCtx observability context (temporary data)
   * @param estimator best PoPs estimator
   * @param clearRangesBL true to free rangesBL; false otherwise
   */
  public void estimateData(final int nValid, final ObservabilityContext obsCtx, final BestPopsEstimator estimator, final boolean clearRangesBL) {
    final List<List<Range>> rangesPerBL = this.rangesBL;

    // flatten ranges :
    List<Range> ranges;
    for (int i = 0, size = rangesPerBL.size(); i < size; i++) {
      ranges = rangesPerBL.get(i);
      if (!ranges.isEmpty()) {
        obsCtx.addInFlatRangeLimits(ranges);
      }
    }

    if (clearRangesBL) {
      // recycle memory (to avoid GC):
      obsCtx.recycleRanges(rangesPerBL);

      this.rangesBL = null;
    }

    // merged ranges (temporary use) = empty list with 2 buckets
    final List<Range> mergeRanges = obsCtx.getMergeRanges();

    // Merge HA ranges (BL) with HA Rise/set ranges (and optionally night limits) :
    Range.intersectRanges(obsCtx.getFlatRangeLimits(), obsCtx.getSizeFlatRangeLimits(), nValid, mergeRanges);

    Range maxRange = null;

    // find the maximum length of HA observable intervals:
    if (mergeRanges != null) {
      Range range;
      for (int i = 0, size = mergeRanges.size(); i < size; i++) {
        range = mergeRanges.get(i);
        if (maxRange == null || range.getLength() > maxRange.getLength()) {
          maxRange = range;
        }
      }
    }

    if (maxRange == null) {
      this.estimation = 0d;
      this.maxLength = 0d;
    } else {
      this.estimation = estimator.compute(maxRange);
      this.maxLength = maxRange.getLength();
    }
  }

  /**
   * Simple comparator that takes into account first the maximum length and then the distance to transit (if maximum length are equals)
   * @param other pop observability data
   * @return Double.compare(maxLength1, maxLength2)
   */
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
