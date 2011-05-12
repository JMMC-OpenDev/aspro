/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.observability;

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
  /** list of HA ranges merged with the target Rise / Set */
  private List<Range> mergeRanges;
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
   * @param flatRanges ranges to merge
   */
  public void computeMaxLength(final int nValid, final List<Range> flatRanges) {
    // flatten ranges :
    for (List<Range> ranges : this.rangesBL) {
      if (ranges != null) {
        for (Range range : ranges) {
          flatRanges.add(range);
        }
      }
    }

    // Merge HA ranges with HA Rise/set ranges :
    this.mergeRanges = Range.intersectRanges(flatRanges, nValid);

    double maxLen = 0d;

    if (mergeRanges != null) {
      double len;
      for (Range range : mergeRanges) {
        len = range.getLength();
        if (len > maxLen) {
          maxLen = len;
        }
      }
    }

    this.maxLength = maxLen;

    flatRanges.clear();
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

  /**
   * Return the list of HA ranges merged with the target Rise / Set
   * @return list of HA ranges merged with the target Rise / Set
   */
  public List<Range> getMergeRanges() {
    return mergeRanges;
  }

  @Override
  public String toString() {
    return this.targetName + " : " + this.popCombination + " = " + this.maxLength;
  }
}
