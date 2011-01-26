/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservabilityData.java,v 1.9 2011-01-26 17:20:53 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.8  2011/01/25 13:48:56  bourgesl
 * javadoc
 *
 * Revision 1.7  2010/12/17 15:14:11  bourgesl
 * list of StarObservabilityData replaced by map of StarObservabilityData list keyed by target name
 *
 * Revision 1.6  2010/09/15 13:56:10  bourgesl
 * added moon illumination (percent)
 *
 * Revision 1.5  2010/06/25 14:13:36  bourgesl
 * added dateCalc (AstroSkyCalc) to be reused by OIFits generation
 *
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
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * This class contains the results of the Observability service
 * @author bourgesl
 */
public final class ObservabilityData {

  /* input parameters */
  /** flag indicating if the timestamps are expressed in LST or in UTC */
  private final boolean useLST;
  /** flag to find baseline limits */
  private final boolean doBaseLineLimits;
  /** flag to produce detailed output with all BL / horizon / rise intervals per target */
  private final boolean doDetailedOutput;

  /* observability plot data */
  /** starting date */
  private Date dateMin = null;
  /** ending date */
  private Date dateMax = null;
  /** list of sun time intervals */
  private List<SunTimeInterval> sunIntervals = null;
  /** moon illumination fraction (percent) */
  private double moonIllumPercent = 0d;
  /** map of StarObservabilityData list keyed by target name */
  private final Map<String, List<StarObservabilityData>> mapStarVisibilities = new LinkedHashMap<String, List<StarObservabilityData>>();
  /** optional best PoPs combination */
  private PopCombination bestPops;

  /* other useful data for UV coverage */
  /** AstroSkyCalc instance useful to convert HA in LST or UTC */
  private AstroSkyCalc dateCalc = null;
  /** beam list */
  private List<Beam> beams = null;
  /** base line list */
  private List<BaseLine> baseLines = null;
  /** map of StarData keyed by target name */
  private final Map<String, StarData> mapStarDatas = new HashMap<String, StarData>();

  /**
   * Public Constuctor
   * @param useLST indicates if the timestamps are expressed in LST or in UTC
   * @param doDetailedOutput flag to produce detailed output with all BL / horizon / rise intervals per target
   * @param doBaseLineLimits flag to find base line limits
   */
  public ObservabilityData(final boolean useLST, final boolean doDetailedOutput, final boolean doBaseLineLimits) {
    this.useLST = useLST;
    this.doDetailedOutput = doDetailedOutput;
    this.doBaseLineLimits = doBaseLineLimits;
  }

  /* inputs */
  /**
   * Return the flag indicating if the timestamps are expressed in LST or in UTC
   * @return flag indicating if the timestamps are expressed in LST or in UTC
   */
  public boolean isUseLST() {
    return useLST;
  }

  /**
   * Return the flag to find baseline limits
   * @return flag to find baseline limits
   */
  public boolean isDoBaseLineLimits() {
    return doBaseLineLimits;
  }

  /**
   * Return the flag to produce detailed output with all BL / horizon / rise intervals per target
   * @return flag to produce detailed output with all BL / horizon / rise intervals per target
   */
  public boolean isDoDetailedOutput() {
    return doDetailedOutput;
  }

  /* outputs */
  /**
   * Return the starting date
   * @return starting date
   */
  public Date getDateMin() {
    return dateMin;
  }

  /**
   * Define the starting date
   * @param dateMin starting date
   */
  public void setDateMin(final Date dateMin) {
    this.dateMin = dateMin;
  }

  /**
   * Return the ending date
   * @return ending date
   */
  public Date getDateMax() {
    return dateMax;
  }

  /**
   * Define the ending date
   * @param dateMax ending date
   */
  public void setDateMax(final Date dateMax) {
    this.dateMax = dateMax;
  }

  /**
   * Return the list of sun time intervals
   * @return list of sun time intervals
   */
  public List<SunTimeInterval> getSunIntervals() {
    return sunIntervals;
  }

  /**
   * Define the list of sun time intervals
   * @param sunIntervals list of sun time intervals
   */
  public void setSunIntervals(final List<SunTimeInterval> sunIntervals) {
    this.sunIntervals = sunIntervals;
  }

  /**
   * Return the moon illumination fraction (percent)
   * @return moon illumination fraction (percent)
   */
  public double getMoonIllumPercent() {
    return moonIllumPercent;
  }

  /**
   * Define the moon illumination fraction (percent)
   * @param moonIllumPercent moon illumination fraction (percent)
   */
  public void setMoonIllumPercent(final double moonIllumPercent) {
    this.moonIllumPercent = moonIllumPercent;
  }

  /**
   * Add the StarObservabilityData for the given target name
   * @param name target name
   * @param starVis StarObservabilityData to add
   */
  public void addStarVisibilities(final String name, final StarObservabilityData starVis) {
    this.mapStarVisibilities.put(name, Arrays.asList(starVis));
  }

  /**
   * Add the StarObservabilityData list for the given target name
   * @param name target name
   * @param starVis StarObservabilityData list to add
   */
  public void addStarVisibilities(final String name, final List<StarObservabilityData> starVis) {
    this.mapStarVisibilities.put(name, starVis);
  }

  /**
   * Return the map of StarObservabilityData list keyed by target name
   * @return map of StarObservabilityData list keyed by target name
   */
  public Map<String, List<StarObservabilityData>> getMapStarVisibilities() {
    return this.mapStarVisibilities;
  }

  /**
   * Return the optional best PoPs combination
   * @return optional best PoPs combination
   */
  public PopCombination getBestPops() {
    return bestPops;
  }

  /**
   * Define the optional best PoPs combination
   * @param bestPops optional best PoPs combination
   */
  public void setBestPops(final PopCombination bestPops) {
    this.bestPops = bestPops;
  }

  /* other useful data for UV coverage */
  /**
   * Return the AstroSkyCalc instance useful to convert HA in LST or UTC
   * @return AstroSkyCalc instance useful to convert HA in LST or UTC
   */
  public AstroSkyCalc getDateCalc() {
    return dateCalc;
  }

  /**
   * Define the AstroSkyCalc instance
   * @param dateCalc AstroSkyCalc instance
   */
  public void setDateCalc(final AstroSkyCalc dateCalc) {
    this.dateCalc = dateCalc;
  }

  /**
   * Return the beam list
   * @return beam list
   */
  public List<Beam> getBeams() {
    return beams;
  }

  /**
   * Define the beam list
   * @param beams beam list
   */
  public void setBeams(final List<Beam> beams) {
    this.beams = beams;
  }

  /**
   * Return the base line list
   * @return base line list
   */
  public List<BaseLine> getBaseLines() {
    return baseLines;
  }

  /**
   * Define the base line list
   * @param baseLines base line list
   */
  public void setBaseLines(final List<BaseLine> baseLines) {
    this.baseLines = baseLines;
  }

  /**
   * Add the given StarData
   * @param starData StarData to add
   */
  public void addStarData(final StarData starData) {
    this.mapStarDatas.put(starData.getName(), starData);
  }

  /**
   * Return the StarData for the given target name
   * @param name target name
   * @return StarData or null if not found
   */
  public StarData getStarData(final String name) {
    return this.mapStarDatas.get(name);
  }
}
