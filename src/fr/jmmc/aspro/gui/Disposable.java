/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
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
