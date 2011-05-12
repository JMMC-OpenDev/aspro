/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: StarObservabilityData.java,v 1.8 2011-03-01 17:16:39 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.7  2011/01/25 13:48:55  bourgesl
 * javadoc
 *
 * Revision 1.6  2010/12/17 15:13:37  bourgesl
 * name attribute split into targetName and info
 *
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
 ******************************************************************************/
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

  /* members */
  /** name of the target */
  private final String targetName;
  /** additional information on data (rise/set, horizon, base line ...) */
  private final String info;
  /** type of data */
  private final int type;
  /** visible date intervals */
  private final List<DateTimeInterval> visible = new ArrayList<DateTimeInterval>(3);
  /** transit date */
  private Date transitDate;
  /** elevation sampled data */
  private final List<ElevationDate> elevations = new ArrayList<ElevationDate>(8);

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

  /**
   * Return the name of the target
   * @return name of the target
   */
  public String getTargetName() {
    return targetName;
  }

  /**
   * Return the additional information on data (rise/set, horizon, base line ...)
   * @return additional information on data (rise/set, horizon, base line ...)
   */
  public String getInfo() {
    return info;
  }

  /**
   * Return the legend label (Science or Calibrator target else return additional information on data)
   * @param idx type to use
   * @return string representing this type
   */
  public String getLegendLabel(final int idx) {
    switch (idx) {
      case TYPE_STAR:
        return "Science";
      case TYPE_CALIBRATOR:
        return "Calibrator";
      default:
    }
    return this.getInfo();
  }

  /**
   * Return the type of data
   * @return type of data
   */
  public int getType() {
    return type;
  }

  /**
   * Return the visible date intervals
   * @return visible date intervals
   */
  public List<DateTimeInterval> getVisible() {
    return visible;
  }

  /**
   * Return the transit date
   * @return transit date
   */
  public Date getTransitDate() {
    return transitDate;
  }

  /**
   * Define the transit date
   * @param transitDate transit date
   */
  public void setTransitDate(final Date transitDate) {
    this.transitDate = transitDate;
  }

  /**
   * Return the elevation sampled data
   * @return elevation sampled data
   */
  public List<ElevationDate> getElevations() {
    return elevations;
  }

  @Override
  public String toString() {
    return getTargetName() + " " + ((this.info != null) ? this.info : "") + " : " + getVisible();
  }
}
