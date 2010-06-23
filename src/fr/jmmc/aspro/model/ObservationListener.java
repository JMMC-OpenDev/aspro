/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservationListener.java,v 1.4 2010-06-23 12:53:48 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.3  2010/01/08 16:51:17  bourgesl
 * initial uv coverage
 *
 * Revision 1.2  2009/12/04 15:38:27  bourgesl
 * Added Save action in the menu bar
 *
 * Revision 1.1  2009/11/03 16:57:55  bourgesl
 * added observability plot with LST/UTC support containing only day/night/twilight zones
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.ObservationSetting;

/**
 * This interface define the methods to be implemented by observation listener
 * @author bourgesl
 */
public interface ObservationListener {

  /**
   * This enumeration defines the several kind of observation events
   */
  enum ObservationEventType {

    /** one or more attribute(s) changed */
    CHANGED,
    /** the observation was loaded */
    LOADED,
    /** the observability was computed */
    OBSERVABILITY_DONE,
    /** the OIFits was computed */
    OIFITS_DONE
  }

  /**
   * Handle the given event on the given observation
   * @param type event type
   * @param observation observation
   */
  public void onProcess(ObservationEventType type, ObservationSetting observation);
}
