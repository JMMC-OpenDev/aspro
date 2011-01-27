/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservationListener.java,v 1.1 2011-01-27 17:03:38 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.7  2011/01/21 16:17:24  bourgesl
 * extracted ObservationEventType enum
 *
 * Revision 1.6  2010/12/14 09:25:50  bourgesl
 * added target change event
 *
 * Revision 1.5  2010/10/01 15:36:29  bourgesl
 * new event WARNING_READY
 * added setWarningContainer and fireWarningReady methods
 *
 * Revision 1.4  2010/06/23 12:53:48  bourgesl
 * added setOIFitsFile method and fire OIFits done event
 *
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
package fr.jmmc.aspro.model.event;

import fr.jmmc.aspro.model.oi.ObservationSetting;

/**
 * This interface define the methods to be implemented by observation listener
 * @author bourgesl
 */
public interface ObservationListener {

    /**
     * Handle the given event on the given observation
     * @param type event type
     * @param observation observation
     */
    public void onProcess(ObservationEventType type,
                          ObservationSetting observation);
}
