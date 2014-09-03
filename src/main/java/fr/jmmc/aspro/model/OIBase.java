/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.jmcs.util.CollectionUtils;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlTransient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This is a base class for all generated classes in model.oi (Optical Interferometry Data Model)
 * @author bourgesl
 */
@XmlTransient
public class OIBase implements Cloneable {

    /** Class logger for aspro model classes */
    protected static final Logger logger = LoggerFactory.getLogger("fr.jmmc.aspro.model");

    /**
     * Public Constructor
     */
    public OIBase() {
        super();
    }

    /**
     * Return by default the simple class name
     * @return simple class name
     */
    @Override
    public String toString() {
        return getClass().getSimpleName();
    }

    /**
     * Return a "shallow copy" of this instance
     * @return "shallow copy" of this instance
     */
    @Override
    public Object clone() {
        try {
            return super.clone();
        } catch (CloneNotSupportedException cnse) {
        }
        return null;
    }

    /**
     * Custom equals implementation for all OIBase instances.
     * To be overriden by child classes
     *
     * @param other other OIBase instance
     * @return true if both object are strictly equal (complete object graph)
     */
    protected boolean areEquals(final OIBase other) {
        return (other != null) && (other.getClass() == getClass());
    }

    /**
     * Return a deep "copy" of the list of objects (recursive call to clone() on each object instance)
     * @param <K> OIBase child class
     * @param list list of objects to clone
     * @return deep "copy" of the list
     */
    @SuppressWarnings("unchecked")
    public static final <K extends OIBase> List<K> deepCopyList(final List<K> list) {
        if (list != null) {
            final List<K> newList = new ArrayList<K>(list.size());
            for (K o : list) {
                newList.add((K) o.clone());
            }
            return newList;
        }
        return null;
    }

    /**
     * Return a simple "copy" of the list of objects without cloning each object instance
     * @param <K> OIBase child class
     * @param list list of objects to clone
     * @return deep "copy" of the list
     */
    @SuppressWarnings("unchecked")
    public static final <K extends OIBase> List<K> copyList(final List<K> list) {
        if (list != null) {
            final List<K> newList = new ArrayList<K>(list.size());
            for (K o : list) {
                newList.add(o);
            }
            return newList;
        }
        return null;
    }

    /**
     * Utility method for <code>equals()</code> methods.
     *
     * @param o1 one object
     * @param o2 another object
     *
     * @return <code>true</code> if they're both <code>null</code> or both equal
     */
    public static final boolean areEquals(final Object o1, final Object o2) {
        return (o1 == o2) || ((o1 != null) && (o2 != null) && o1.equals(o2));
    }

    /**
     * Custom OIBase <code>areEquals()</code> method.
     *
     * @param o1 one object
     * @param o2 another object
     *
     * @return <code>true</code> if they're both <code>null</code> or both equal
     */
    public static final boolean areEquals(final OIBase o1, final OIBase o2) {
        return (o1 == o2) || (o1 != null) && (o2 != null) && o1.areEquals(o2);
    }

    /**
     * Custom OIBase <code>areEquals()</code> method.
     *
     * @param o1 one object
     * @param o2 another object
     *
     * @return <code>true</code> if they're both <code>null</code> or both equal
     */
    public static final boolean areEqualsStrict(final OIBase o1, final OIBase o2) {
        return (o1 == o2) || (o1 != null) && (o2 != null) && o1.equals(o2);
    }

    /**
     * Custom List of OIBase <code>areEquals()</code> method calling areEquals() on each item.
     *
     * @param l1 one object
     * @param l2 another object
     *
     * @return <code>true</code> if they're both <code>null</code> or both equal
     */
    public static final boolean areEquals(final List<? extends OIBase> l1, final List<? extends OIBase> l2) {
        if (l1 == l2) {
            return true;
        }
        // check null and empty list:
        final boolean e1 = CollectionUtils.isEmpty(l1);
        final boolean e2 = CollectionUtils.isEmpty(l2);
        if (e1 && e2) {
            return true;
        }
        if (e1 || e2) {
            return false;
        }
        // both lists are not empty:
        final int len = l1.size();
        if (len != l2.size()) {
            return false;
        }
        OIBase o1, o2;
        for (int i = 0; i < len; i++) {
            o1 = l1.get(i);
            o2 = l2.get(i);

            if ((o1 == null) ? (o2 != null) : !o1.areEquals(o2)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Custom List of OIBase <code>areEquals()</code> method calling equals() on each item.
     *
     * @param l1 one object
     * @param l2 another object
     *
     * @return <code>true</code> if they're both <code>null</code> or both equal
     */
    public static final boolean areEqualsStrict(final List<? extends OIBase> l1, final List<? extends OIBase> l2) {
        if (l1 == l2) {
            return true;
        }
        // check null and empty list:
        final boolean e1 = CollectionUtils.isEmpty(l1);
        final boolean e2 = CollectionUtils.isEmpty(l2);
        if (e1 && e2) {
            return true;
        }
        if (e1 || e2) {
            return false;
        }
        // both lists are not empty:
        final int len = l1.size();
        if (len != l2.size()) {
            return false;
        }
        OIBase o1, o2;
        for (int i = 0; i < len; i++) {
            o1 = l1.get(i);
            o2 = l2.get(i);

            if ((o1 == null) ? (o2 != null) : !o1.equals(o2)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Utility method for <code>equals()</code> methods.
     *
     * @param o1 one object
     * @param o2 another object
     *
     * @return <code>true</code> if they're both <code>null</code> or both equal
     */
    public static final boolean areEquals(final String o1, final String o2) {
        final String s1 = (o1 == null) ? "" : o1;
        final String s2 = (o2 == null) ? "" : o2;
        return s1.equals(s2);
    }

    /**
     * Utility method for <code>equals()</code> methods.
     *
     * @param o1 one boolean
     * @param o2 another boolean
     *
     * @return <code>true</code> if they're both equal
     */
    public static final boolean areEquals(final boolean o1, final boolean o2) {
        return (o1 == o2);
    }

    /**
     * Utility method for <code>equals()</code> methods.
     *
     * @param o1 one float
     * @param o2 another float
     *
     * @return <code>true</code> if they're both equal
     */
    public static final boolean areEquals(final float o1, final float o2) {
        return (Float.floatToIntBits(o1) == Float.floatToIntBits(o2));
    }

    /**
     * Utility method for <code>equals()</code> methods.
     *
     * @param o1 one double
     * @param o2 another double
     *
     * @return <code>true</code> if they're both equal
     */
    public static final boolean areEquals(final double o1, final double o2) {
        return (Double.doubleToLongBits(o1) == Double.doubleToLongBits(o2));
    }

    /**
     * @param value value to check
     * @return true if the given value is NaN
     */
    public static final boolean isNaN(final Float value) {
        return (value != null && Float.isNaN(value));
    }

    /**
     * @param value value to check
     * @return true if the given value is NaN
     */
    public static final boolean isNaN(final Double value) {
        return (value != null && Double.isNaN(value));
    }

    /**
     * Test if value is empty (no chars)
     * 
     * @param value string value
     * @return true if value is empty (null or no chars)
     */
    public static boolean isEmpty(final String value) {
        return value != null && value.isEmpty();
    }
}
