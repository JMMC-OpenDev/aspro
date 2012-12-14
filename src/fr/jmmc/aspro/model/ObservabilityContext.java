/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.observability.PopCombination;
import fr.jmmc.aspro.service.pops.BestPopsEstimator;
import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class gathers several variables used by the observability computation to enhance performance
 * and avoid memory allocations (best pops)
 * @author bourgesl
 */
public final class ObservabilityContext {

  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(ObservabilityContext.class.getName());
  /* members */
  /** temporary range limits array (for performance) to merge HA ranges with Rise/set range */
  private RangeLimit[] flatRangeLimits = null;
  /** number of valid elements in the flatRanges array */
  private int nFlatRangeLimits = 0;
  /** length of the flatRangeLimits array */
  private int lenFlatRangeLimits = 0;
  /** temporary lists to store merged HA ranges */
  private final List<Range> mergeRanges = new ArrayList<Range>(2);
  /* arrays instead of list for traversal performance */
  /** pop combinations array */
  PopCombination[] popCombs = null;
  /** base line array */
  BaseLine[] baseLines = null;
  /** W ranges array */
  Range[] wRanges = null;
  /** best PoPs estimator related to the current target (HA ranges) */
  private BestPopsEstimator popEstimator = null;

  /**
   * Public constructor
   * @param nBaseLines number of baselines
   */
  public ObservabilityContext(final int nBaseLines) {
    if (logger.isDebugEnabled()) {
      logger.debug("ObservabilityContext : nBaseLines: {}", nBaseLines);
    }
    // minimal capacity = 2 rangeLimits per range * ( 3 ranges * nBaseLines + 2 rise/set range + 2 nightLimits range)
    resizeFlatRangeLimits(2 * (3 * nBaseLines + 2 + 2));
  }

  /**
   * Public Copy constructor
   * @param obsCtx observability context to copy
   */
  public ObservabilityContext(final ObservabilityContext obsCtx) {
    this(obsCtx.baseLines.length);

    // Use arrays instead of List for performance:
    setPopCombs(obsCtx.popCombs);
    setBaseLines(obsCtx.baseLines);
    setWRanges(obsCtx.wRanges);
  }

  /**
   * Resize the flatRangeLimits array
   * @param capacity new capacity
   */
  private void resizeFlatRangeLimits(final int capacity) {
    if (this.flatRangeLimits == null) {
      if (logger.isDebugEnabled()) {
        logger.debug("resizeFlatRangeLimits : create with capacity: {}", capacity);
      }

      this.flatRangeLimits = RangeLimit.createArray(capacity);
    } else {
      if (logger.isDebugEnabled()) {
        logger.debug("resizeFlatRangeLimits : resize with capacity = {} / {}", capacity, this.lenFlatRangeLimits);
      }

      final RangeLimit[] newArray = RangeLimit.createArray(capacity);

      // copy old values:
      for (int i = 0; i < this.nFlatRangeLimits; i++) {
        newArray[i].set(this.flatRangeLimits[i]);
      }
      this.flatRangeLimits = newArray;
    }

    this.lenFlatRangeLimits = this.flatRangeLimits.length;
  }

  /**
   * Add the given list of ranges to the flat range limits array as range limits
   * @param ranges list of ranges
   */
  public void addInFlatRangeLimits(final List<Range> ranges) {
    final int size = ranges.size();

    int n = this.nFlatRangeLimits;

    final int newLength = n + 2 * size;
    if (newLength > this.lenFlatRangeLimits) {
      this.resizeFlatRangeLimits(newLength);
    }

    final RangeLimit[] limits = this.flatRangeLimits;

    Range range;
    for (int i = 0; i < size; i++) {
      range = ranges.get(i);
      limits[n++].set(range.getMin(), true);
      limits[n++].set(range.getMax(), false);
    }

    this.nFlatRangeLimits = n;
  }

  /**
   * Return the number of valid elements in the flatRangeLimits array
   * @return number of valid elements in the flatRangeLimits array
   */
  public int getSizeFlatRangeLimits() {
    return this.nFlatRangeLimits;
  }

  /**
   * Reset the number of valid elements in the flatRangeLimits array
   */
  public void resetFlagRanges() {
    this.nFlatRangeLimits = 0;
  }

  /**
   * Return the flat range limits array to merge HA ranges with Rise/set range
   * @return flat range limits array
   */
  public RangeLimit[] getFlatRangeLimits() {
    return flatRangeLimits;
  }

  /**
   * Return the temporary lists to store merged HA ranges (empty)
   * @return temporary lists to store merged HA ranges
   */
  public List<Range> getMergeRanges() {
    this.mergeRanges.clear();
    return this.mergeRanges;
  }

  /**
   * Return the pop combinations array
   * @return pop combinations array
   */
  public PopCombination[] getPopCombs() {
    return popCombs;
  }

  /**
   * Set the pop combinations array
   * @param popCombs pop combinations array
   */
  public void setPopCombs(final PopCombination[] popCombs) {
    this.popCombs = popCombs;
  }

  /**
   * Return the base line array
   * @return base line array
   */
  public BaseLine[] getBaseLines() {
    return baseLines;
  }

  /**
   * Set the base line array
   * @param baseLines base line array
   */
  public void setBaseLines(final BaseLine[] baseLines) {
    this.baseLines = baseLines;
  }

  /**
   * Return the W ranges array
   * @return W ranges array
   */
  public Range[] getWRanges() {
    return wRanges;
  }

  /**
   * Set the W ranges array
   * @param wRanges W ranges array
   */
  public void setWRanges(final Range[] wRanges) {
    this.wRanges = wRanges;
  }

  /**
   * Return the best PoPs estimator related to the current target
   * @return best PoPs estimator related to the current target
   */
  public BestPopsEstimator getPopEstimator() {
    return popEstimator;
  }

  /**
   * Define the best PoPs estimator related to the current target
   * @param popEstimator best PoPs estimator related to the current target
   */
  public void setPopEstimator(final BestPopsEstimator popEstimator) {
    this.popEstimator = popEstimator;
  }
}
