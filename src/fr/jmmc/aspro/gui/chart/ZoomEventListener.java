/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

/**
 * This interface defines the zoom event listener
 * @author bourgesl
 */
public interface ZoomEventListener {

  /**
   * Invoked when the chart is zoomed in or out.
   * @see ZoomEvent
   */
  public void chartChanged(ZoomEvent ze);
}
