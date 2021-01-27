/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.observability.StarObservabilityData;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;

/**
 * Abstract filter on observability results
 */
public abstract class ObservabilityFilter {

    /** filter name */
    private final String name;
    /** filter label (JList) */
    private final String label;
    /** filter tooltip (JList tooltip) */
    private final String tooltip;

    /**
     * Filter constructor
     * @param name filter name
     * @param label filter label
     * @param tooltip filter tooltip
     */
    protected ObservabilityFilter(final String name, final String label, final String tooltip) {
        this.name = name;
        this.label = label;
        this.tooltip = tooltip;
    }

    /**
     * Return the filter name
     * @return filter name
     */
    public final String getName() {
        return name;
    }

    /**
     * Return the filter label
     * @return filter label
     */
    public final String getLabel() {
        return label;
    }

    /**
     * Return the filter tooltip
     * @return filter tooltip
     */
    public String getTooltip() {
        return this.tooltip;
    }

    /**
     * Return the filter label (JList)
     * @return filter label
     */
    @Override
    public final String toString() {
        return label;
    }

    /**
     * Prepare this filter before processing 
     * @param context any object or null
     */
    public void prepare(final Object context) {
        // no-op
    }

    /**
     * Apply the filter on the given target, its calibrator flag and star observability data
     * @param targetUserInfos TargetUserInformations instance
     * @param target target instance
     * @param calibrator calibrator flag
     * @param so star observability data
     * @return true if that item should be filtered; false otherwise
     */
    public abstract boolean apply(final TargetUserInformations targetUserInfos,
                                  final Target target,
                                  final boolean calibrator,
                                  final StarObservabilityData so);
}
