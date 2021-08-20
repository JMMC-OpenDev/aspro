/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.AzEl;
import fr.jmmc.oitools.model.range.RangeFactory;
import fr.jmmc.oitools.model.range.Range;
import fr.jmmc.oitools.model.range.RangeLimit;
import fr.jmmc.aspro.service.DelayLineService;
import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class gathers several variables used by the observability computation to enhance performance
 * and avoid memory allocations
 * @author bourgesl
 */
@SuppressWarnings("unchecked")
public class ObservabilityContext implements RangeFactory {

    /** Class logger */
    protected static final Logger logger = LoggerFactory.getLogger(ObservabilityContext.class.getName());
    /** max number of List[Range] in pool */
    public final static int MAX_RANGE_LIST = (15 + 1) * 2; // 6T = 15 BL !
    /** max number of Range in pool */
    public final static int MAX_RANGE = 2 * MAX_RANGE_LIST; // 6T = 15 BL and 3 ranges per BL max + 1 (merged ranges)
    /* members */
    /** position in the Range pool (-1 if empty) */
    private int nRange = -1;
    /** position in the List[Range] pool (-1 if empty) */
    private int nRangeList = -1;
    /** Range pool */
    private final Range[] rangePool = new Range[MAX_RANGE + 1];
    /** List[Range] pool */
    private final List[] rangeListPool = new List[MAX_RANGE_LIST + 1];
    /** created range instances */
    private int createdRanges = 0;
    /** created List[Range] instances */
    private int createdRangeLists = 0;
    /* intersection */
    /** temporary range limits array (for performance) to merge HA ranges with Rise/set range */
    protected RangeLimit[] flatRangeLimits = null;
    /** number of valid elements in the flatRanges array */
    protected int nFlatRangeLimits = 0;
    /** length of the flatRangeLimits array */
    private int lenFlatRangeLimits = 0;
    /** ha = double[2] array to avoid array allocations for DelayLineService */
    private final double[] ha = new double[2];
    /** ha values = double[6] array to avoid array allocations for DelayLineService */
    private final double[] haValues = new double[6];
    /** w = double[2] array to avoid array allocations for DelayLineService */
    private final double[] w = new double[2];
    /** temporary azEl instance */
    private final AzEl azEl = new AzEl();

    /**
     * Public constructor
     * @param nBaseLines number of baselines
     */
    public ObservabilityContext(final int nBaseLines) {
        if (logger.isDebugEnabled()) {
            logger.debug("ObservabilityContext : nBaseLines: {}", nBaseLines);
        }
        // minimal capacity = 2 rangeLimits per range * ( 3 ranges * nBaseLines + 2 rise/set range + 2 nightLimits range) + 4 (cache line padding)
        this.resizeFlatRangeLimits(2 * (3 * nBaseLines + 2 + 2 + 4));
    }

    /**
     * @return double[2] array to avoid array allocations for DelayLineService
     */
    public final double[] getW() {
        return w;
    }

    /**
     * @return double[2] array to avoid array allocations for DelayLineService
     */
    public final double[] getHa() {
        return ha;
    }

    /**
     * @return double[6] array to avoid array allocations for DelayLineService
     */
    public final double[] getHaValues() {
        return haValues;
    }

    /**
    * @return temporary azEl instance
    */
    public AzEl getAzEl() {
        return azEl;
    }

    /**
     * Resize the flatRangeLimits array
     * @param capacity new capacity
     */
    protected final void resizeFlatRangeLimits(final int capacity) {
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
    public final void resetAndAddInFlatRangeLimits(final List<Range> ranges) {
        this.nFlatRangeLimits = 0;
        this.addInFlatRangeLimits(ranges);
    }

    /**
     * Add the given list of ranges to the flat range limits array as range limits
     * @param ranges list of ranges
     */
    public final void addInFlatRangeLimits(final List<Range> ranges) {
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
            limits[n++].set(range.getMin(), 1);
            limits[n++].set(range.getMax(), -1);
        }

        this.nFlatRangeLimits = n;
    }

    /**
     * Intersect overlapping ranges according to the nValid parameter that indicates how many times a point must be inside a range
     * to consider the point as valid
     * @param ranges list of ranges
     * @param nValid number of ranges to consider a point is valid
     * @return List[Range] from the pool or null
     */
    public final List<Range> intersectRanges(final List<Range> ranges, final int nValid) {
        if (ranges.isEmpty()) {
            return null;
        }
        resetAndAddInFlatRangeLimits(ranges);

        return Range.intersectRanges(flatRangeLimits, nFlatRangeLimits, nValid, null, this);
    }

    // RangeFactory:
    /**
     * Return a Range instance with given minimum and maximum value
     * @param min minimum value
     * @param max maximum value
     * @return Range instance
     */
    @Override
    public final Range valueOf(final double min, final double max) {
        if (nRange >= 0) {
            final Range range = rangePool[nRange--];
            range.set(min, max);
            return range;
        }
        ++createdRanges;
        return new Range(min, max);
    }

    /**
     * Return a List[Range] instance
     * @return List[Range] instance
     */
    @Override
    public final ArrayList<Range> getList() {
        if (nRangeList >= 0) {
            return (ArrayList<Range>) rangeListPool[nRangeList--];
        }
        ++createdRangeLists;
        return new ArrayList<Range>(3); // max 3 intervals per BL
    }

    /**
     * Recycle both Range and List[Range] instances 
     * @param ranges List[Range] to recycle as List
     */
    public final void recycleAll(final List<List<Range>> ranges) {
        if ((ranges != null) && !ranges.isEmpty()) {
            final Range[] pr = rangePool;
            final List<Range>[] pl = rangeListPool;
            int nr = nRange;
            int nl = nRangeList;

            List<Range> rangeList;
            for (int i = 0, size = ranges.size(), len, j; i < size; i++) {
                rangeList = ranges.get(i);
                if (rangeList != null
                        && rangeList != DelayLineService.EMPTY_RANGE_LIST
                        && rangeList != DelayLineService.FULL_RANGE_LIST) {
                    len = Math.min(MAX_RANGE - nr, rangeList.size());

                    // recycle ranges:
                    for (j = 0; j < len; j++) {
                        pr[++nr] = rangeList.get(j);
                    }

                    // recycle list:
                    if (nl < MAX_RANGE_LIST) {
                        rangeList.clear(); // set to null !
                        pl[++nl] = rangeList;
                    }
                }
            }
            nRange = nr;
            nRangeList = nl;
        }
    }

    /**
     * Recycle both Range and List[Range] instances 
     * @param ranges List[Range] to recycle as List
     */
    public final void recycleRangesAndList(final List<Range> ranges) {
        recycleRanges(ranges);
        recycleList(ranges);
    }

    /**
     * Recycle Range instances only
     * @param ranges List[Range] to recycle
     */
    public final void recycleRanges(final List<Range> ranges) {
        if ((ranges != null) && !ranges.isEmpty()) {
            final Range[] pr = rangePool;
            int nr = nRange;

            for (int i = 0, len = Math.min(MAX_RANGE - nr, ranges.size()); i < len; i++) {
                pr[++nr] = ranges.get(i);
            }
            nRange = nr;
        }
    }

    /**
     * Recycle the given Range instance only
     * @param range Range to recycle
     */
    public final void recycleRange(final Range range) {
        if ((range != null) && (nRange < MAX_RANGE)) {
            rangePool[++nRange] = range;
        }
    }

    /**
     * Recycle Range instances only
     * @param rangeList List[Range] to recycle
     */
    public final void recycleList(final List<Range> rangeList) {
        if (rangeList != null) {
            if (rangeList != DelayLineService.EMPTY_RANGE_LIST
                    && rangeList != DelayLineService.FULL_RANGE_LIST) {
                // recycle list:
                if (nRangeList < MAX_RANGE_LIST) {
                    rangeList.clear(); // set to null !
                    rangeListPool[++nRangeList] = rangeList;
                }
            }
        }
    }

    /**
     * Reset the factory state
     */
    @Override
    public final void reset() {
        createdRanges = 0;
        createdRangeLists = 0;
    }

    /**
     * Dump the factory statistics
     */
    @Override
    public final void dumpStats() {
        logger.info("{}: {} created ranges ({} pool size) - {} created lists ({} pool size).",
                getClass().getSimpleName(), createdRanges, nRange, createdRangeLists, nRangeList);
    }
}
