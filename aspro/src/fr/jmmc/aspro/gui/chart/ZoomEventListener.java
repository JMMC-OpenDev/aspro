/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ZoomEventListener.java,v 1.1 2010-02-03 09:48:52 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
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
