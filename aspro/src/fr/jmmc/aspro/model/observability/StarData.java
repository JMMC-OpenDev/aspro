/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: StarData.java,v 1.2 2010-01-22 13:16:19 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/01/08 16:48:30  bourgesl
 * package refactoring
 *
 */
package fr.jmmc.aspro.model.observability;

import fr.jmmc.aspro.model.Range;
import java.util.List;

/**
 * This class contains several star related data (precessed ra and dec, ha rise/set, and HA observability ranges)
 * @author bourgesl
 */
public class StarData {

  /** target name */
  private final String name;
  /** precessed target right ascension in decimal hours */
  private double precRA;
  /** precessed target declination in degrees */
  private double precDEC;
  /** hour angle over the minimum elevation (used in the observability service) */
  private double haElev;
  /** observability HA intervals */
  private List<Range> obsRangesHA;

  /**
   * Constructor
   * @param name target name
   */
  public StarData(final String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  public double getPrecDEC() {
    return precDEC;
  }

  public void setPrecDEC(double precDEC) {
    this.precDEC = precDEC;
  }

  public double getPrecRA() {
    return precRA;
  }

  public void setPrecRA(double precRA) {
    this.precRA = precRA;
  }

  public double getHaElev() {
    return haElev;
  }

  public void setHaElev(double haElev) {
    this.haElev = haElev;
  }

  public List<Range> getObsRangesHA() {
    return obsRangesHA;
  }

  public void setObsRangesHA(List<Range> obsRangesHA) {
    this.obsRangesHA = obsRangesHA;
  }

  @Override
  public String toString() {
    return name + " [ " + getPrecRA() + ", " + getPrecDEC() + "] - " + getHaElev() + " - " + getObsRangesHA();
  }
}
