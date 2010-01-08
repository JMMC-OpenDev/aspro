/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: UVBaseLineData.java,v 1.1 2010-01-08 16:50:53 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.model.uvcoverage;

/**
 * This class contains uv coordinates related to a baseline used by the UVCoverageData class
 * @author bourgesl
 */
public class UVBaseLineData {
  /** baseline name */
  private String name;
  /** number of uv points */
  private int nPoints = 0;
  /** u coordinates */
  private double[] u = null;
  /** v coordinates */
  private double[] v = null;

  /**
   * Constructor
   * @param name baseline name
   */
  public UVBaseLineData(final String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public int getNPoints() {
    return nPoints;
  }

  public void setNPoints(int nPoints) {
    this.nPoints = nPoints;
  }

  public double[] getU() {
    return u;
  }

  public void setU(double[] u) {
    this.u = u;
  }

  public double[] getV() {
    return v;
  }

  public void setV(double[] v) {
    this.v = v;
  }

}
