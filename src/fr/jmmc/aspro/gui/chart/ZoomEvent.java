/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ZoomEvent.java,v 1.1 2010-02-03 09:48:53 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.chart;

/**
 *
 * @author bourgesl
 */
public class ZoomEvent {
  /** domain lower bound */
  private final double domainLowerBound;

  /** domain Upper bound */
  private final double domainUpperBound;

  /** range lower bound */
  private final double rangeLowerBound;

  /** range Upper bound */
  private final double rangeUpperBound;

  public ZoomEvent(
          final double domainLowerBound,
          final double domainUpperBound,
          final double rangeLowerBound,
          final double rangeUpperBound) {
    this.domainLowerBound = domainLowerBound;
    this.domainUpperBound = domainUpperBound;
    this.rangeLowerBound = rangeLowerBound;
    this.rangeUpperBound = rangeUpperBound;
  }

  public double getDomainLowerBound() {
    return domainLowerBound;
  }

  public double getDomainUpperBound() {
    return domainUpperBound;
  }

  public double getRangeLowerBound() {
    return rangeLowerBound;
  }

  public double getRangeUpperBound() {
    return rangeUpperBound;
  }

}
