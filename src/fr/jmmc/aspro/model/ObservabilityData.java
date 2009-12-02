/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityData.java,v 1.3 2009-12-02 17:23:51 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.model;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * This class contains the results of the Observability service
 * @author bourgesl
 */
public class ObservabilityData {

  /** starting date */
  private Date dateMin = null;
  /** ending date */
  private Date dateMax = null;
  /** list of sun time intervals */
  private List<SunTimeInterval> sunIntervals = null;
  /** list of star visibility intervals */
  private List<StarObservability> starVisibilities = new ArrayList<StarObservability>();

  /**
   * Constuctor
   */
  public ObservabilityData() {
    super();
  }

  /* Getter - Setter */

  public Date getDateMin() {
    return dateMin;
  }

  public void setDateMin(Date dateMin) {
    this.dateMin = dateMin;
  }

  public Date getDateMax() {
    return dateMax;
  }

  public void setDateMax(Date dateMax) {
    this.dateMax = dateMax;
  }

  public List<SunTimeInterval> getSunIntervals() {
    return sunIntervals;
  }

  public void setSunIntervals(List<SunTimeInterval> sunIntervals) {
    this.sunIntervals = sunIntervals;
  }

  public List<StarObservability> getStarVisibilities() {
    return starVisibilities;
  }


}
