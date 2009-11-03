/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservationListener.java,v 1.1 2009-11-03 16:57:55 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 *
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.ObservationSetting;

/**
 * This interface define the methods to be implemented by observation listener
 * @author bourgesl
 */
public interface ObservationListener {

  public void onChange(ObservationSetting observation);

}
