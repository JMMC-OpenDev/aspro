/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.jmmc.aspro.model;

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
  /** list of sun time interval */
  private List<SunTimeInterval> sunIntervals = null;

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

  

}
