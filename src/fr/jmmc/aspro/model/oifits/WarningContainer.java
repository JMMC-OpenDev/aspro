/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: WarningContainer.java,v 1.1 2010-09-02 15:46:32 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.model.oifits;

/**
 * This interface defines a single method addWarningMessage(String)
 * @author bourgesl
 */
public interface WarningContainer {

  /**
   * Add the given message to the warning messages
   * @param msg message to add
   */
  public void addWarningMessage(final String msg);
}
