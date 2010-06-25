/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityData.java,v 1.5 2010-06-25 14:13:36 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.4  2010/06/23 12:54:17  bourgesl
 * added Beam list to use it in OIFits generation
 *
 * Revision 1.3  2010/01/22 13:16:44  bourgesl
 * added star observability type to change bar colors easily
 *
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

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Beam;
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
  /** astroSkyCalc instance useful to convert HA in LST or UTC */
  private AstroSkyCalc dateCalc = null;
  /** beam list */
  private List<Beam> beams = null;
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

  public void setDateMin(final Date dateMin) {
    this.dateMin = dateMin;
  }

  public Date getDateMax() {
    return dateMax;
  }

  public void setDateMax(final Date dateMax) {
    this.dateMax = dateMax;
  }

  public List<SunTimeInterval> getSunIntervals() {
    return sunIntervals;
  }

  public void setSunIntervals(final List<SunTimeInterval> sunIntervals) {
    this.sunIntervals = sunIntervals;
  }

  public List<StarObservabilityData> getStarVisibilities() {
    return starVisibilities;
  }

  public PopCombination getBestPops() {
    return bestPops;
  }

  public void setBestPops(final PopCombination bestPops) {
    this.bestPops = bestPops;
  }

  /* other useful data for UV coverage */
  public AstroSkyCalc getDateCalc() {
    return dateCalc;
  }

  public void setDateCalc(final AstroSkyCalc dateCalc) {
    this.dateCalc = dateCalc;
  }

  public List<Beam> getBeams() {
    return beams;
  }

  public void setBeams(final List<Beam> beams) {
    this.beams = beams;
  }

  public List<BaseLine> getBaseLines() {
    return baseLines;
  }

  public void setBaseLines(final List<BaseLine> baseLines) {
    this.baseLines = baseLines;
  }

  public void addStarData(final StarData starData) {
    this.mapStarDatas.put(starData.getName(), starData);
  }

  public StarData getStarData(final String name) {
    return this.mapStarDatas.get(name);
  }
}
