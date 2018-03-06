/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

/** Sliding XYPlot adapter state */
public final class SlidingXYPlotState {

    /** members */
    /** max items in the chart view if the useSubset mode is enabled */
    int maxViewItems;
    /** flag to enable/disable the subset mode */
    boolean useSubset = false;
    /** current position of the subset */
    int position = 0;
    /** selected position (highlight) */
    int selectedPosition = -1;

    /**
     * Private constructor
     */
    SlidingXYPlotState() {
        super();
    }

    /**
     * Private copy constructor
     * @param state state to copy
     */
    SlidingXYPlotState(final SlidingXYPlotState state) {
        super();
        copy(state);
    }

    /**
     * Copy the given state
     * @param state state to copy
     */
    void copy(final SlidingXYPlotState state) {
        this.maxViewItems = state.maxViewItems;
        this.useSubset = state.useSubset;
        this.position = state.position;
        this.selectedPosition = state.selectedPosition;
    }
}
