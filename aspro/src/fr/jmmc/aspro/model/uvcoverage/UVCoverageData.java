/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.uvcoverage;

import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.ObservationVersion;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.service.NoiseService;
import fr.jmmc.aspro.service.OIFitsCreatorService;
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
  /** maximum U and V coordinate (scale) */
  private double uvMax;
  /** central wave length (meter) */
  private double lambda;
  /** configuration i.e. station names */
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
  /** optional NoiseService ready to use to compute noise on model images */
  private NoiseService noiseService = null;
  /** OIFitsCreatorService ready to compute later OIFits: TODO : move elsewhere / another task worker */
  private OIFitsCreatorService oiFitsCreator = null;

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

  /**
   * Return the name of the target
   * @return name of the target
   */
  public String getTargetName() {
    return targetName;
  }

  /**
   * Define the name of the target
   * @param name name of the target
   */
  public void setTargetName(final String name) {
    this.targetName = name;
  }

  /**
   * Return the maximum U and V coordinate (scale)
   * @return maximum U and V coordinate (scale)
   */
  public double getUvMax() {
    return uvMax;
  }

  /**
   * Define the maximum U and V coordinate (scale)
   * @param uvMax maximum U and V coordinate (scale)
   */
  public void setUvMax(final double uvMax) {
    this.uvMax = uvMax;
  }

  /**
   * Return the central wave length
   * @return central wave length
   */
  public double getLambda() {
    return lambda;
  }

  /**
   * Define the central wave length (meter)
   * @param lambda central wave length (meter)
   */
  public void setLambda(final double lambda) {
    this.lambda = lambda;
  }

  /**
   * Return the configuration i.e. station names
   * @return configuration i.e. station names
   */
  public String getStationNames() {
    return stationNames;
  }

  /**
   * Define the configuration i.e. station names
   * @param stationNames configuration i.e. station names
   */
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

  /**
   * Return the list of uv points corresponding to the target rise/set
   * @return list of uv points corresponding to the target rise/set
   */
  public List<UVBaseLineData> getTargetUVRiseSet() {
    return targetUVRiseSet;
  }

  /**
   * Define the list of uv points corresponding to the target rise/set
   * @param targetUVRiseSet list of uv points corresponding to the target rise/set
   */
  public void setTargetUVRiseSet(final List<UVBaseLineData> targetUVRiseSet) {
    this.targetUVRiseSet = targetUVRiseSet;
  }

  /**
   * Return the observable decimal hour angles (used by OIFits)
   * @return observable decimal hour angles (used by OIFits)
   */
  public double[] getHA() {
    return ha;
  }

  /**
   * Define the observable decimal hour angles (used by OIFits)
   * @param ha observable decimal hour angles (used by OIFits)
   */
  public void setHA(final double[] ha) {
    this.ha = ha;
  }

  /**
   * Return the list of uv point couples corresponding to the target observability
   * @return list of uv point couples corresponding to the target observability
   */
  public List<UVRangeBaseLineData> getTargetUVObservability() {
    return targetUVObservability;
  }

  /**
   * Define the list of uv point couples corresponding to the target observability
   * @param targetUVObservability list of uv point couples corresponding to the target observability
   */
  public void setTargetUVObservability(final List<UVRangeBaseLineData> targetUVObservability) {
    this.targetUVObservability = targetUVObservability;
  }

  /**
   * Return the warning container
   * @return warning container
   */
  public WarningContainer getWarningContainer() {
    return warningContainer;
  }

  /**
   * Return the optional NoiseService ready to use to compute noise on model images
   * @return optional NoiseService ready to use to compute noise on model images
   */
  public NoiseService getNoiseService() {
    return noiseService;
  }

  /**
   * Define the optional NoiseService ready to use to compute noise on model images
   * @param noiseService optional NoiseService ready to use to compute noise on model images
   */
  public void setNoiseService(final NoiseService noiseService) {
    this.noiseService = noiseService;
  }

  /**
   * Return the OIFitsCreatorService ready to compute later OIFits: TODO : move elsewhere / another task worker
   * @return OIFitsCreatorService ready to compute later OIFits: TODO : move elsewhere / another task worker
   */
  public OIFitsCreatorService getOiFitsCreator() {
    return oiFitsCreator;
  }

  /**
   * Define the OIFitsCreatorService ready to compute later OIFits: TODO : move elsewhere / another task worker
   * @param oiFitsCreator OIFitsCreatorService ready to compute later OIFits: TODO : move elsewhere / another task worker
   */
  public void setOiFitsCreator(final OIFitsCreatorService oiFitsCreator) {
    this.oiFitsCreator = oiFitsCreator;
  }
}
