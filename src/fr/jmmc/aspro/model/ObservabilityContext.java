/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.observability.PopCombination;
import fr.jmmc.aspro.service.DelayLineService;
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
public final class ObservabilityContext implements RangeFactory {

  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(ObservabilityContext.class.getName());
  /** skip range checks (out of bounds) */
  private final static boolean SKIP_RANGE_CHECK = true;
  /** max number of List<Range> in pool */
  public final static int MAX_RANGE_LIST = 15 + 1; // 6T = 15 BL !
  /** max number of Range in pool */
  public final static int MAX_RANGE = 3 * MAX_RANGE_LIST + 1; // 6T = 15 BL and 3 ranges per BL max + 1 (merged ranges)
  /* members */
  /** temporary range limits array (for performance) to merge HA ranges with Rise/set range */
  private RangeLimit[] flatRangeLimits = null;
  /** number of valid elements in the flatRanges array */
  private int nFlatRangeLimits = 0;
  /** length of the flatRangeLimits array */
  private int lenFlatRangeLimits = 0;
  /** temporary lists to store merged HA ranges */
  private final List<Range> mergeRanges = new ArrayList<Range>(3 + 8); // cache line padding
  /** position in the Range pool (-1 if empty) */
  private int nRange = -1;
  /** position in the List<Range> pool (-1 if empty) */
  private int nRangeList = -1;
  /** Range pool */
  private final Range[] rangePool = new Range[MAX_RANGE + 8]; // cache line padding
  /** List<Range> pool */
  private final List[] rangeListPool = new List<?>[MAX_RANGE_LIST + 8]; // cache line padding
  /** created range instances */
  private int createdRanges = 0;
  /** created List<Range> instances */
  private int createdRangeLists = 0;
  /* arrays instead of list for traversal performance (read only) */
  /** pop combinations array */
  private PopCombination[] popCombs = null;
  /** base line array */
  private BaseLine[] baseLines = null;
  /** W ranges array */
  private Range[] wRanges = null;
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
    this.resizeFlatRangeLimits(2 * (3 * nBaseLines + 2 + 2));
  }

  /**
   * Public Copy constructor
   * @param obsCtx observability context to copy
   */
  public ObservabilityContext(final ObservabilityContext obsCtx) {
    this(obsCtx.getBaseLines().length);

    // Use arrays instead of List for performance:
    setPopCombs(obsCtx.getPopCombs());
    setBaseLines(obsCtx.getBaseLines());
    setWRanges(obsCtx.getWRanges());
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
   * Reset the flat range limits array and add the given list
   * @param ranges list of ranges
   */
  public void resetAndAddInFlatRangeLimits(final List<Range> ranges) {
    this.nFlatRangeLimits = 0;
    this.addInFlatRangeLimits(ranges);
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
    final List<Range> ranges = this.mergeRanges;
    ranges.clear();
    return ranges;
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
  // RangeFactory:

  /**
   * Return a Range instance with given minimum and maximum value
   * @param min minimum value
   * @param max maximum value
   * @return Range instance
   */
  public Range valueOf(final double min, final double max) {
    if (nRange >= 0) {
      final Range range = rangePool[nRange--];
      range.set(min, max);
      return range;
    }
    ++createdRanges;
    return new Range(min, max);
  }

  /**
   * Return a List<Range> instance
   * @return List<Range> instance
   */
  @SuppressWarnings("unchecked")
  public List<Range> getList() {
    if (nRangeList >= 0) {
      return rangeListPool[nRangeList--];
    }
    ++createdRangeLists;
    return new ArrayList<Range>(3); // max 3 intervals per BL
  }

  /**
   * Recycle both Range and List<Range> instances 
   * @param ranges List<Range> to recycle as List
   */
  public void recycleAll(final List<List<Range>> ranges) {
    final Range[] pr = rangePool;
    final List[] pl = rangeListPool;
    int nr = nRange;
    int nl = nRangeList;

    List<Range> rangeList;
    for (int i = 0, size = ranges.size(), len, j; i < size; i++) {
      rangeList = ranges.get(i);
      if (rangeList != DelayLineService.EMPTY_RANGE_LIST && rangeList != DelayLineService.FULL_RANGE_LIST) {
        len = (SKIP_RANGE_CHECK) ? rangeList.size() : Math.min(MAX_RANGE - nr, rangeList.size());

        // recycle ranges:
        for (j = 0; j < len; j++) {
          pr[++nr] = rangeList.get(j);
        }

        // recycle list:
        if (SKIP_RANGE_CHECK || nl < MAX_RANGE_LIST) {
          rangeList.clear(); // set to null !
          pl[++nl] = rangeList;
        }
      }
    }
    nRange = nr;
    nRangeList = nl;
  }

  /**
   * Recycle both Range instances only
   * @param ranges List<Range> to recycle
   */
  public void recycleRanges(final List<Range> ranges) {
    final Range[] pr = rangePool;
    int nr = nRange;

    for (int i = 0, len = (SKIP_RANGE_CHECK) ? ranges.size() : Math.min(MAX_RANGE - nr, ranges.size()); i < len; i++) {
      pr[++nr] = ranges.get(i);
    }
    nRange = nr;
  }

  /**
   * Reset the factory state
   */
  public void reset() {
    createdRanges = 0;
    createdRangeLists = 0;
  }

  /**
   * Dump the factory statistics
   */
  public void dumpStats() {
    logger.info("RangeFactory: {} created ranges - {} created lists.", createdRanges, createdRangeLists);
  }
}
