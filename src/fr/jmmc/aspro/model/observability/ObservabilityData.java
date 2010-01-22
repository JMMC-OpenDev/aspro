/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityData.java,v 1.3 2010-01-22 13:16:44 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2010/01/12 16:54:19  bourgesl
 * added PoPs in title + several changes on charts
 *
 * Revision 1.1  2010/01/08 16:48:30  bourgesl
 * package refactoring
 *
 * Revision 1.3  2009/12/02 17:23:51  bourgesl
 * fixed several bugs on pop finder + refactoring
 *
 */
package fr.jmmc.aspro.model.observability;

import fr.jmmc.aspro.model.BaseLine;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class contains the results of the Observability service
 * @author bourgesl
 */
public class ObservabilityData {

  /* observability plot data */
  /** starting date */
  private Date dateMin = null;
  /** ending date */
  private Date dateMax = null;
  /** list of sun time intervals */
  private List<SunTimeInterval> sunIntervals = null;
  /** list of star visibility intervals */
  private List<StarObservabilityData> starVisibilities = new ArrayList<StarObservabilityData>();
  /** optional */
  /** best PoPs combination */
  private PopCombination bestPops;

  /* other useful data for UV coverage */
  /** base line list */
  private List<BaseLine> baseLines = null;
  /** map of StarData keyed by target name */
  private Map<String, StarData> mapStarDatas = new HashMap<String, StarData>();

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

  public List<StarObservabilityData> getStarVisibilities() {
    return starVisibilities;
  }

  public PopCombination getBestPops() {
    return bestPops;
  }

  public void setBestPops(PopCombination bestPops) {
    this.bestPops = bestPops;
  }

  public List<BaseLine> getBaseLines() {
    return baseLines;
  }

  public void setBaseLines(List<BaseLine> baseLines) {
    this.baseLines = baseLines;
  }

  public void addStarData(final StarData starData) {
    this.mapStarDatas.put(starData.getName(), starData);
  }

  public StarData getStarData(final String name) {
    return this.mapStarDatas.get(name);
  }
}
