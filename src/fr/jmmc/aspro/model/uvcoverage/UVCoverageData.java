/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: UVCoverageData.java,v 1.5 2010-02-04 17:05:05 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.4  2010/02/04 14:54:11  bourgesl
 * UVMapData refactoring (uvRect, min/max values) to keep the color mapping consistent when zooming
 * Compute an sub Image when a zoom occurs while the correct model is computed in the background
 *
 * Revision 1.3  2010/02/03 09:48:53  bourgesl
 * target model uvmap added on the uv coverage with zooming supported
 *
 * Revision 1.2  2010/01/15 16:14:16  bourgesl
 * added computation of UV points compatible with observability ranges, bandpass and sampling periodicity
 *
 * Revision 1.1  2010/01/08 16:50:53  bourgesl
 * initial uv coverage
 *
 */
package fr.jmmc.aspro.model.uvcoverage;

import fr.jmmc.mcs.model.UVMapData;
import java.util.List;

/**
 * This class contains the results of the UV coverage service
 * @author bourgesl
 */
public class UVCoverageData {

  /** name of the target */
  private String name;
  /** maximum U or V coordinate (scale) */
  private double uvMax;
  /** wave length */
  private double lambda;
  /** list of uv points corresponding to the target rise/set */
  private List<UVBaseLineData> targetUVRiseSet;
  /** list of uv point couples corresponding to the target observability */
  private List<UVRangeBaseLineData> targetUVObservability;
  /** uv map data */
  private UVMapData uvMapData;

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

  public double getLambda() {
    return lambda;
  }

  public void setLambda(double lambda) {
    this.lambda = lambda;
  }

  public double getUvMax() {
    return uvMax;
  }

  public void setUvMax(double uvMax) {
    this.uvMax = uvMax;
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

  public UVMapData getUvMapData() {
    return uvMapData;
  }

  public void setUvMapData(UVMapData uvMapData) {
    this.uvMapData = uvMapData;
  }
}
