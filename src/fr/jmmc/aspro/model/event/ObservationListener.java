/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.event;

/**
 * This interface define the methods to be implemented by observation listener
 * @author bourgesl
 */
public interface ObservationListener {

  /**
   * Handle the given observation event
   * @param event observation event
   */
  public void onProcess(ObservationEvent event);
}
