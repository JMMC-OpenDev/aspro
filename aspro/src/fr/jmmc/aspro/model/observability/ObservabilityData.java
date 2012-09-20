/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.observability;

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.ObservationVersion;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.observability.SunTimeInterval.SunType;
import fr.jmmc.aspro.model.oi.Target;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class contains the results of the Observability service
 * @author bourgesl
 */
public final class ObservabilityData {

  /** observation version */
  private final ObservationVersion version;

  /* input parameters */
  /** flag indicating if the timestamps are expressed in LST or in UTC */
  private final boolean useLST;
  /** flag to find baseline limits */
  private final boolean doBaseLineLimits;
  /** flag to produce detailed output with all BL / horizon / rise intervals per target */
  private final boolean doDetailedOutput;
  /** flag to center the plot arround midnight */
  private final boolean doCenterMidnight;
  /** twilight considered as night limit */
  private final SunType twilightNightLimit;

  /* observability plot data */
  /** starting date */
  private Date dateMin = null;
  /** ending date */
  private Date dateMax = null;
  /** starting date as julian date */
  private double jdMin = 0d;
  /** ending date as julian date */
  private double jdMax = 0d;
  /** list of sun time intervals */
  private List<SunTimeInterval> sunIntervals = null;
  /** moon illumination fraction (percent) */
  private double moonIllumPercent = 0d;
  /** target list (useful for baseline limits) */
  private List<Target> targets;
  /** map of StarObservabilityData list keyed by target name */
  private final Map<String, List<StarObservabilityData>> mapStarVisibilities = new HashMap<String, List<StarObservabilityData>>();
  /** flag indicating to use user PoPs */
  private boolean userPops;
  /** optional best PoPs combination */
  private PopCombination bestPops;
  /** warning container */
  private final WarningContainer warningContainer = new WarningContainer();

  /* other useful data for UV coverage */
  /** AstroSkyCalc instance useful to convert HA in LST or UTC */
  private AstroSkyCalc dateCalc = null;
  /** configuration */
  private String stationNames;
  /** beam list */
  private List<Beam> beams = null;
  /** base line list */
  private List<BaseLine> baseLines = null;
  /** map of StarData keyed by target name */
  private final Map<String, StarData> mapStarDatas = new HashMap<String, StarData>();

  /**
   * Public Constructor
   * @param version observation version
   * @param useLST indicates if the timestamps are expressed in LST or in UTC
   * @param doDetailedOutput flag to produce detailed output with all BL / horizon / rise intervals per target
   * @param doBaseLineLimits flag to find base line limits
   * @param doCenterMidnight flag to center the plot arround midnight
   * @param twilightNightLimit twilight considered as night limit
   */
  public ObservabilityData(final ObservationVersion version, final boolean useLST, final boolean doDetailedOutput, final boolean doBaseLineLimits,
          final boolean doCenterMidnight, final SunType twilightNightLimit) {
    this.version = version;
    this.useLST = useLST;
    this.doDetailedOutput = doDetailedOutput;
    this.doBaseLineLimits = doBaseLineLimits;
    this.doCenterMidnight = doCenterMidnight;
    this.twilightNightLimit = twilightNightLimit;
  }

  /* version */
  /**
   * Return the observation version
   * @return observation version
   */
  public ObservationVersion getVersion() {
    return version;
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

  /**
   * Return the flag to center the plot arround midnight
   * @return flag to center the plot arround midnight
   */
  public boolean isDoCenterMidnight() {
    return doCenterMidnight;
  }

  /**
   * Return the twilight considered as night limit
   * @return twilight considered as night limit
   */
  public SunType getTwilightNightLimit() {
    return twilightNightLimit;
  }

  /* outputs */
  /**
   * Return the starting date as julian date
   * @return starting date as julian date
   */
  public double getJdMin() {
    return jdMin;
  }

  /**
   * Define the starting date as julian date
   * @param jdMin starting date as julian date
   */
  public void setJdMin(final double jdMin) {
    this.jdMin = jdMin;
  }

  /**
   * Return the ending date as julian date
   * @return ending date as julian date
   */
  public double getJdMax() {
    return jdMax;
  }

  /**
   * Define the ending date as julian date
   * @param jdMax ending date as julian date
   */
  public void setJdMax(final double jdMax) {
    this.jdMax = jdMax;
  }

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
   * Return the target list (useful for baseline limits)
   * @return target list (useful for baseline limits)
   */
  public List<Target> getTargets() {
    return targets;
  }

  /**
   * Define the target list (useful for baseline limits)
   * @param targets target list (useful for baseline limits)
   */
  public void setTargets(final List<Target> targets) {
    this.targets = targets;
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
   * Return the flag indicating to use user PoPs
   * @return flag indicating to use user PoPs
   */
  public boolean isUserPops() {
    return userPops;
  }

  /**
   * Define the flag indicating to use user PoPs
   * @param userPops flag indicating to use user PoPs
   */
  public void setUserPops(final boolean userPops) {
    this.userPops = userPops;
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
   * Return the configuration
   * @return configuration
   */
  public String getStationNames() {
    return stationNames;
  }

  /**
   * Define the configuration
   * @param stationNames configuration
   */
  public void setStationNames(final String stationNames) {
    this.stationNames = stationNames;
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

  /**
   * Return the warning container
   * @return warning container
   */
  public WarningContainer getWarningContainer() {
    return warningContainer;
  }
}
