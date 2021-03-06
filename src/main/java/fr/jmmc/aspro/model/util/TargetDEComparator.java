/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

import fr.jmmc.aspro.model.oi.Target;
import java.util.Comparator;

/**
 * Target Comparator implementation based on DE field
 * @author bourgesl
 */
public final class TargetDEComparator implements Comparator<Target> {

    /** singleton instance */
    private final static TargetDEComparator instance = new TargetDEComparator();

    /**
     * Return the comparator singleton
     * @return comparator singleton
     */
    public static Comparator<Target> getInstance() {
        return instance;
    }

    /**
     * Private constructor
     */
    private TargetDEComparator() {
        super();
    }

    /**
     * Compares its two arguments for order.  Returns a negative integer,
     * zero, or a positive integer as the first argument is less than, equal
     * to, or greater than the second.<p>
     *
     * @param t1 the first target to be compared.
     * @param t2 the second target to be compared.
     * @return a negative integer, zero, or a positive integer as the
     * 	       first argument is less than, equal to, or greater than the
     *	       second.
     */
    @Override
    public int compare(final Target t1, final Target t2) {
        return Double.compare(t1.getDECDeg(), t2.getDECDeg());
    }
}
