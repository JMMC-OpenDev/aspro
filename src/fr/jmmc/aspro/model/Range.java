/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: Range.java,v 1.2 2009-11-23 16:49:17 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2009/11/20 16:55:47  bourgesl
 * Added Beam / Delay Line definition
 * ObservabilityService is stateless to simplify coding
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.model;

/**
 * A simple range of double values
 * @author bourgesl
 */
public class Range {

  private double min = 0d;
  private double max = 0d;

  public Range() {
    /* no-op */
  }

  public Range(final double min, final double max) {
    this.min = min;
    this.max = max;
  }

  public double getMin() {
    return min;
  }

  public void setMin(double min) {
    this.min = min;
  }

  public double getMax() {
    return max;
  }

  public void setMax(double max) {
    this.max = max;
  }
}
