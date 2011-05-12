/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.uvcoverage;

import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.ObservationVersion;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.oitools.model.OIFitsFile;
import java.util.List;

/**
 * This class contains the results of the UV coverage service
 * @author bourgesl
 */
public final class UVCoverageData {

  /** observation version */
  private final ObservationVersion version;
  /** name of the target */
  private String targetName;
  /** maximum U or V coordinate (scale) */
  private double uvMax;
  /** wave length */
  private double lambda;
  /** configuration */
  private String stationNames;
  /** base line list */
  private List<BaseLine> baseLines = null;
  /** list of uv points corresponding to the target rise/set */
  private List<UVBaseLineData> targetUVRiseSet;
  /** observable decimal hour angles (used by OIFits) */
  private double[] ha = null;
  /** list of uv point couples corresponding to the target observability */
  private List<UVRangeBaseLineData> targetUVObservability;
  /** warning container */
  private final WarningContainer warningContainer = new WarningContainer();
  /** oifits structure */
  private OIFitsFile oiFitsFile;

  /**
   * Public Constructor
   * @param version observation version
   */
  public UVCoverageData(final ObservationVersion version) {
    this.version = version;
  }

  /* version */
  /**
   * Return the observation version
   * @return observation version
   */
  public ObservationVersion getVersion() {
    return version;
  }

  public String getTargetName() {
    return targetName;
  }

  public void setTargetName(final String name) {
    this.targetName = name;
  }

  public double getLambda() {
    return lambda;
  }

  public void setLambda(final double lambda) {
    this.lambda = lambda;
  }

  public double getUvMax() {
    return uvMax;
  }

  public void setUvMax(final double uvMax) {
    this.uvMax = uvMax;
  }

  public String getStationNames() {
    return stationNames;
  }

  public void setStationNames(final String stationNames) {
    this.stationNames = stationNames;
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

  public List<UVBaseLineData> getTargetUVRiseSet() {
    return targetUVRiseSet;
  }

  public void setTargetUVRiseSet(final List<UVBaseLineData> targetUVRiseSet) {
    this.targetUVRiseSet = targetUVRiseSet;
  }

  public double[] getHA() {
    return ha;
  }

  public void setHA(final double[] ha) {
    this.ha = ha;
  }

  public List<UVRangeBaseLineData> getTargetUVObservability() {
    return targetUVObservability;
  }

  public void setTargetUVObservability(final List<UVRangeBaseLineData> targetUVObservability) {
    this.targetUVObservability = targetUVObservability;
  }

  public WarningContainer getWarningContainer() {
    return warningContainer;
  }

  public OIFitsFile getOiFitsFile() {
    return oiFitsFile;
  }

  public void setOiFitsFile(final OIFitsFile oiFitsFile) {
    this.oiFitsFile = oiFitsFile;
  }
}
