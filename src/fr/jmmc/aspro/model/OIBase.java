/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: OIBase.java,v 1.6 2010-11-30 15:53:27 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.5  2010/11/29 17:14:35  bourgesl
 * proper clone implementatio (deep-copy)
 *
 * Revision 1.4  2010/02/12 15:53:25  bourgesl
 * comments
 *
 * Revision 1.3  2010/01/05 17:16:59  bourgesl
 * add Cloneable support
 *
 * Revision 1.2  2009/10/22 15:47:22  bourgesl
 * beginning of observability computation with jSkyCalc
 *
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.model;

import java.util.ArrayList;
import java.util.List;

/**
 * This is a base class for all generated classes in model.oi (Optical Interferometry Data Model)
 * @author bourgesl
 */
public class OIBase implements Cloneable {
  /** Class logger */
  protected static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          "fr.jmmc.aspro.model.OIBase");

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
    if ((o1 != o2) && ((o1 == null) || !o1.equals(o2))) {
      return false;
    }

    return true;
  }
}
