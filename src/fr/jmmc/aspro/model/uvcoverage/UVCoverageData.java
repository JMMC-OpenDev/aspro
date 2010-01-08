/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: UVCoverageData.java,v 1.1 2010-01-08 16:50:53 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.model.uvcoverage;

import java.util.List;

/**
 * This class contains the results of the UV coverage service
 * @author bourgesl
 */
public class UVCoverageData {

  /** name of the target */
  private String name;
  /** list of uv points corresponding to the target rise/set */
  private List<UVBaseLineData> targetUVRiseSet;
  /** list of uv points corresponding to the target rise/set */
  private List<UVBaseLineData> targetUVObservability;

  /**
   * Constructor
   */
  public UVCoverageData() {
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public List<UVBaseLineData> getTargetUVRiseSet() {
    return targetUVRiseSet;
  }

  public void setTargetUVRiseSet(List<UVBaseLineData> targetUVRiseSet) {
    this.targetUVRiseSet = targetUVRiseSet;
  }

  public List<UVBaseLineData> getTargetUVObservability() {
    return targetUVObservability;
  }

  public void setTargetUVObservability(List<UVBaseLineData> targetUVObservability) {
    this.targetUVObservability = targetUVObservability;
  }
}
