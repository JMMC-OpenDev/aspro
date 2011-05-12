/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: Disposable.java,v 1.1 2010-09-08 16:00:30 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 ******************************************************************************/
package fr.jmmc.aspro.gui;

/**
 * This interface defines the dispose() method to free ressources or references to itself to
 * avoid memory leak and Observer/Observable problems
 * @author bourgesl
 */
public interface Disposable {

  /**
   * Free any ressource or reference to this instance
   */
  public void dispose();

}
