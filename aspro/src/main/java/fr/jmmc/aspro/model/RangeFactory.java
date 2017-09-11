/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import java.util.ArrayList;
import java.util.List;

/**
 * This interface defines the range factory pattern
 * @author bourgesl
 */
public interface RangeFactory {

  /**
   * Return a Range instance with given minimum and maximum value
   * @param min minimum value
   * @param max maximum value
   * @return Range instance
   */
  public Range valueOf(final double min, final double max);

  /**
   * Return a List<Range> instance
   * @return List<Range> instance
   */
  public ArrayList<Range> getList();

  /**
   * Reset the factory state
   */
  public void reset();

  /**
   * Dump the factory statistics
   */
  public void dumpStats();
}
