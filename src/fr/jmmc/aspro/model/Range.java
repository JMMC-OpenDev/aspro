/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: Range.java,v 1.1 2009-11-20 16:55:47 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 *
 ******************************************************************************/
package fr.jmmc.aspro.model;

/**
 * A simple range of double values
 * @author bourgesl
 */
public class Range {

  private final double min;

  private final double max;

  public Range(final double min, final double max) {
    this.min = min;
    this.max = max;
  }

  public double getMax() {
    return max;
  }

  public double getMin() {
    return min;
  }


}
