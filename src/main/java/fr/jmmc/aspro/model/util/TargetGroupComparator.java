/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

import fr.jmmc.aspro.model.oi.TargetGroup;
import java.util.Comparator;

/**
 * TargetGroup Comparator implementation based on category and name fields
 * @author bourgesl
 */
public final class TargetGroupComparator implements Comparator<TargetGroup> {

    /** singleton instance */
    private final static TargetGroupComparator instance = new TargetGroupComparator();

    /**
     * Return the comparator singleton
     * @return comparator singleton
     */
    public static Comparator<TargetGroup> getInstance() {
        return instance;
    }

    /**
     * Private constructor
     */
    private TargetGroupComparator() {
        super();
    }

    /**
     * Compares its two arguments for order.  Returns a negative integer,
     * zero, or a positive integer as the first argument is less than, equal
     * to, or greater than the second.<p>
     *
     * @param g1 the first target group to be compared.
     * @param g2 the second target group to be compared.
     * @return a negative integer, zero, or a positive integer as the
     * 	       first argument is less than, equal to, or greater than the
     *	       second.
     */
    @Override
    public int compare(final TargetGroup g1, final TargetGroup g2) {
        int cmp = g1.getCategory().compareTo(g2.getCategory());
        if (cmp == 0) {
            cmp = g1.getName().compareToIgnoreCase(g2.getName());
        }
        return cmp;
    }
}
