/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.observability.PopCombination;
import fr.jmmc.aspro.model.observability.PopObservabilityData;
import fr.jmmc.aspro.service.pops.BestPopsEstimator;
import java.util.ArrayList;
import java.util.List;

/**
 * This class gathers several variables used by the observability computation to enhance performance
 * and avoid memory allocations (best pops)
 * @author bourgesl
 */
@SuppressWarnings("unchecked")
public final class BestPoPsObservabilityContext extends ObservabilityContext {

    /* members */
    /** temporary lists to store merged HA ranges */
    protected final ArrayList<Range> mergeRanges = new ArrayList<Range>(16);
    /** temporary PopObservabilityData list */
    private List<PopObservabilityData> popDataList = null;
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
    public BestPoPsObservabilityContext(final int nBaseLines) {
        super(nBaseLines);
    }

    /**
     * Public Copy constructor
     * @param obsCtx observability context to copy
     */
    public BestPoPsObservabilityContext(final BestPoPsObservabilityContext obsCtx) {
        this(obsCtx.getBaseLines().length);

        // Use arrays instead of List for performance:
        setPopCombs(obsCtx.getPopCombs());
        setBaseLines(obsCtx.getBaseLines());
        setWRanges(obsCtx.getWRanges());
    }

    /**
     * Intersect overlapping ranges according to the nValid parameter that indicates how many times a point must be inside a range
     * to consider the point as valid
     * @param nValid number of ranges to consider a point is valid
     * @return temporary lists to store merged HA ranges
     */
    public List<Range> intersectAndGetMergeRanges(final int nValid) {
        final List<Range> ranges = this.mergeRanges;
        ranges.clear();
        return Range.intersectRanges(flatRangeLimits, nFlatRangeLimits, nValid, ranges, this);
    }

    /**
     * Return the temporary PopObservabilityData list
     * @return temporary PopObservabilityData list
     */
    public List<PopObservabilityData> getPopDataList() {
        if (popDataList == null) {
            // lazily create a new list according to popCombs size:
            final int capacity = (popCombs.length > 1) ? ((popCombs.length > 1000) ? 500 : 100) : 1;
            popDataList = new ArrayList<PopObservabilityData>(capacity);
        }
        return popDataList;
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
