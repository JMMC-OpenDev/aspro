/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: UVCoverageData.java,v 1.10 2011-02-03 17:27:38 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.9  2010/10/01 15:38:29  bourgesl
 * added warning container
 *
 * Revision 1.8  2010/06/29 14:24:45  bourgesl
 * javadoc comment
 *
 * Revision 1.7  2010/06/28 12:27:56  bourgesl
 * hour angles (HA) moved in UVCoverageData because it is not related to a particular baseline
 *
 * Revision 1.6  2010/06/23 12:56:13  bourgesl
 * added OIFits structure generation with OI_ARRAY and OI_TARGET tables
 *
 * Revision 1.5  2010/02/04 17:05:05  bourgesl
 * UV bounds are coming from UVCoverageService
 *
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

import fr.jmmc.aspro.model.ObservationVersion;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.mcs.model.UVMapData;
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
  /** list of uv points corresponding to the target rise/set */
  private List<UVBaseLineData> targetUVRiseSet;
  /** observable decimal hour angles (used by OIFits) */
  private double[] ha = null;
  /** list of uv point couples corresponding to the target observability */
  private List<UVRangeBaseLineData> targetUVObservability;
  /** uv map data : TODO remove */
  private UVMapData uvMapData;
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

  public UVMapData getUvMapData() {
    return uvMapData;
  }

  public void setUvMapData(final UVMapData uvMapData) {
    this.uvMapData = uvMapData;
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
