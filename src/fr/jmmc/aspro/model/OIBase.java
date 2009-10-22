/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: OIBase.java,v 1.2 2009-10-22 15:47:22 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.model;

/**
 * This is a base class for all generated classes in model.oi (Optical Interferometry Data Model)
 * @author bourgesl
 */
public class OIBase {

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
  
//--simple--preserve

}
