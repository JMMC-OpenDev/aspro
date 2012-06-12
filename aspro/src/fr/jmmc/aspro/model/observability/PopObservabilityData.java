/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.observability;

import fr.jmmc.aspro.model.ObservabilityContext;
import fr.jmmc.aspro.model.Range;
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
   * Compute the total observability length
   * @param nValid number of ranges to consider a point is valid
   * @param obsCtx observability context (temporary data)
   */
  public void computeMaxLength(final int nValid, final ObservabilityContext obsCtx) {
    
    // flatten ranges :
    for (List<Range> ranges : this.rangesBL) {
      if (ranges != null) {
        obsCtx.addInFlatRangeLimits(ranges);
      }
    }

    // merged ranges (temporary use) = empty list with 2 buckets
    final List<Range> mergeRanges = obsCtx.getMergeRanges();
    
    // Merge HA ranges with HA Rise/set ranges :
    Range.intersectRanges(obsCtx.getFlatRangeLimits(), obsCtx.getSizeFlatRangeLimits(), nValid, mergeRanges);

    double maxLen = 0d;

    if (mergeRanges != null) {
      for (Range range : mergeRanges) {
        final double len = range.getLength();
        if (len > maxLen) {
          maxLen = len;
        }
      }
    }

    this.maxLength = maxLen;
  }

  /**
   * Simple comparator that takes into account only the maximum length 
   * @param other pop observability data
   * @return Double.compare(maxLength1, maxLength2)
   */
  public int compareTo(final PopObservabilityData other) {
    return Double.compare(this.maxLength, other.getMaxLength());
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
    return this.targetName + " : " + this.popCombination + " = " + this.maxLength;
  }
}
