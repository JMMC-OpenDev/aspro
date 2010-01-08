/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: PopObservabilityData.java,v 1.1 2010-01-08 16:48:30 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2009/12/18 11:52:02  bourgesl
 * added Pops Finder Compatible Mode for a list of targets
 *
 */
package fr.jmmc.aspro.model.observability;

import fr.jmmc.aspro.model.*;
import java.util.List;

/**
 * This class contains intermediate results for the observability of a target using a particular Pop combination
 * @author bourgesl
 */
public class PopObservabilityData implements Comparable<PopObservabilityData> {

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

  public PopObservabilityData(final String targetName, final PopCombination popCombination, final List<List<Range>> rangesBL) {
    this.targetName = targetName;
    this.popCombination = popCombination;
    this.rangesBL = rangesBL;
  }

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
    this.mergeRanges = Range.mergeRanges(flatRanges, nValid);

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

  public String getTargetName() {
    return targetName;
  }

  public double getMaxLength() {
    return maxLength;
  }

  public PopCombination getPopCombination() {
    return popCombination;
  }

  public List<List<Range>> getRangesBL() {
    return rangesBL;
  }

  public List<Range> getMergeRanges() {
    return mergeRanges;
  }

  @Override
  public String toString() {
    return this.targetName + " : " + this.popCombination + " = " + this.maxLength;
  }
}
