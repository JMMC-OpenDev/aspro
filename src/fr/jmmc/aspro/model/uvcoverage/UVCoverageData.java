/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: UVCoverageData.java,v 1.3 2010-02-03 09:48:53 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2010/01/15 16:14:16  bourgesl
 * added computation of UV points compatible with observability ranges, bandpass and sampling periodicity
 *
 * Revision 1.1  2010/01/08 16:50:53  bourgesl
 * initial uv coverage
 *
 */
package fr.jmmc.aspro.model.uvcoverage;

import java.awt.Image;
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
  /** list of uv point couples corresponding to the target observability */
  private List<UVRangeBaseLineData> targetUVObservability;
  /** uv map */
  private Image uvMap;

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

  public List<UVRangeBaseLineData> getTargetUVObservability() {
    return targetUVObservability;
  }

  public void setTargetUVObservability(List<UVRangeBaseLineData> targetUVObservability) {
    this.targetUVObservability = targetUVObservability;
  }

  public Image getUvMap() {
    return uvMap;
  }

  public void setUvMap(Image uvMap) {
    this.uvMap = uvMap;
  }
}
