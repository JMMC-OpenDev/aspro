package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.Pop;
import java.util.List;

/**
 * This class contains intermediate results for the observability of a target using a particular Pop combination
 * @author bourgesl
 */
public class PopObservabilityData implements Comparable<PopObservabilityData> {

  /** pop combination */
  private final List<Pop> popCombination;
  /** list of HA ranges per BL */
  private final List<List<Range>> rangesBL;
  /** maximum length of an HA range after merging HA ranges per BL with Rise/Set range */
  private double maxLength;

  public PopObservabilityData(final List<Pop> popCombination, final List<List<Range>> rangesBL) {
    this.popCombination = popCombination;
    this.rangesBL = rangesBL;
  }

  public void computeMaxLength(final int nValid, final List<Range> flatRanges, final List<Range> mergeRanges) {
    // flatten ranges :
    for (List<Range> ranges : this.rangesBL) {
      if (ranges != null) {
        for (Range range : ranges) {
          flatRanges.add(range);
        }
      }
    }

    // Merge HA ranges with HA Rise/set ranges :
    Range.mergeRanges(flatRanges, nValid, mergeRanges);

    double len;
    double maxLen = 0d;
    for (Range range : mergeRanges) {
      len = range.getLength();
      if (len > maxLen) {
        maxLen = len;
      }
    }

    this.maxLength = maxLen;

    mergeRanges.clear();
    flatRanges.clear();
  }

  public int compareTo(final PopObservabilityData other) {
    return Double.compare(this.maxLength, other.getMaxLength());
  }

  public double getMaxLength() {
    return maxLength;
  }

  public List<Pop> getPopCombination() {
    return popCombination;
  }

  public List<List<Range>> getRangesBL() {
    return rangesBL;
  }

  @Override
  public String toString() {
    return this.popCombination + " = " + this.maxLength;
  }
}
