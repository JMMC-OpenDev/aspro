/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: StarObservabilityData.java,v 1.6 2010-12-17 15:13:37 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.5  2010/10/08 12:29:44  bourgesl
 * add calibrator type (blue)
 *
 * Revision 1.4  2010/07/22 12:31:18  bourgesl
 * new moon type
 *
 * Revision 1.3  2010/04/02 14:40:39  bourgesl
 * added elevation data and transit date
 *
 * Revision 1.2  2010/01/22 13:16:18  bourgesl
 * fixed imports
 *
 * Revision 1.1  2010/01/08 16:48:29  bourgesl
 * package refactoring
 *
 * Revision 1.3  2009/12/18 11:52:02  bourgesl
 * added Pops Finder Compatible Mode for a list of targets
 *
 */
package fr.jmmc.aspro.model.observability;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * This class contains several results for the observability of a target
 *
 * @author bourgesl
 */
public final class StarObservabilityData {
  /* type of data */

  /** star observability */
  public final static int TYPE_STAR = 0;
  /** calibrator observability */
  public final static int TYPE_CALIBRATOR = 1;
  /** rise/set intervals */
  public final static int TYPE_RISE_SET = 2;
  /** horizon intervals */
  public final static int TYPE_HORIZON = 3;
  /** baseline intervals */
  public final static int TYPE_BASE_LINE = 4;
  /** moon intervals */
  public final static int TYPE_MOON = 27;

  /* members */
  /** name of the target */
  private final String targetName;
  /** additional information on data (moon, rise/set, horizon, base line ...) */
  private final String info;
  /** type of data */
  private final int type;
  /** visible date intervals */
  private final List<DateTimeInterval> visible = new ArrayList<DateTimeInterval>();
  /** transit date */
  private Date transitDate;
  /** visible date intervals */
  private final List<ElevationDate> elevations = new ArrayList<ElevationDate>();

  /**
   * Constructor
   * @param targetName target name
   * @param type type of observability
   */
  public StarObservabilityData(final String targetName, final int type) {
    this(targetName, null, type);
  }

  /**
   * Constructor
   * @param targetName target name
   * @param info additional information on data (moon, rise/set, horizon, base line ...)
   * @param type type of observability
   */
  public StarObservabilityData(final String targetName, final String info, final int type) {
    this.targetName = targetName;
    this.info = info;
    this.type = type;
  }

  public String getTargetName() {
    return targetName;
  }

  public String getInfo() {
    return info;
  }

  public int getType() {
    return type;
  }

  public List<DateTimeInterval> getVisible() {
    return visible;
  }

  public Date getTransitDate() {
    return transitDate;
  }

  public void setTransitDate(Date transitDate) {
    this.transitDate = transitDate;
  }

  public List<ElevationDate> getElevations() {
    return elevations;
  }

  @Override
  public String toString() {
    return getTargetName() + " " + ((this.info != null) ? this.info : "") + " : " + getVisible();
  }
}
