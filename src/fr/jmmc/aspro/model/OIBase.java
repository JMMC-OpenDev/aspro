/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: OIBase.java,v 1.3 2010-01-05 17:16:59 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2009/10/22 15:47:22  bourgesl
 * beginning of observability computation with jSkyCalc
 *
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.model;

/**
 * This is a base class for all generated classes in model.oi (Optical Interferometry Data Model)
 * @author bourgesl
 */
public class OIBase implements Cloneable {

  /**
   * Return by default the simple class name
   * @return simple class name
   */
  @Override
  public String toString() {
    return getClass().getSimpleName();
  }

//--simple--preserve

  /* In any child class, this comments indicate a custom section to be kept
  if the java code is generated again by XJC */

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
//--simple--preserve
}
