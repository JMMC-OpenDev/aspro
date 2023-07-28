/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

import fr.jmmc.aspro.service.UserModelData;
import java.util.Comparator;

/**
 * UserModelData Comparator implementation based on wavelength field
 * @author bourgesl
 */
public final class UserModelDataComparator implements Comparator<UserModelData> {

    /** singleton instance */
    private final static UserModelDataComparator instance = new UserModelDataComparator();

    /**
     * Return the comparator singleton
     * @return comparator singleton
     */
    public static Comparator<UserModelData> getInstance() {
        return instance;
    }

    /**
     * Private constructor
     */
    private UserModelDataComparator() {
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
    public int compare(final UserModelData t1, final UserModelData t2) {
        return Double.compare(t1.getWaveLength(), t2.getWaveLength());
    }
}
