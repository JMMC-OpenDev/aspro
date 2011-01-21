/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: TaskRegistry.java,v 1.1 2011-01-21 16:30:08 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.task;

/**
 * This interface defines the methods to be implemented by a task registry
 * @author bourgesl
 */
public interface TaskRegistry {

  /**
   * Return the number of tasks
   * @return number of tasks
   */
  public int getTaskCount();
}
