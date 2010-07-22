/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: StarObservabilityData.java,v 1.4 2010-07-22 12:31:18 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
  /** rise/set intervals */
  public final static int TYPE_RISE_SET = 1;
  /** horizon intervals */
  public final static int TYPE_HORIZON = 2;
  /** baseline intervals */
  public final static int TYPE_BASE_LINE = 3;
  /** moon intervals */
  public final static int TYPE_MOON = 27;

  /* members */

  /** name of the target (+ base line) */
  private final String name;

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
   * @param name target name
   * @param type type of observability
   */
  public StarObservabilityData(final String name, final int type) {
    this.name = name;
    this.type = type;
  }

  public String getName() {
    return name;
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
    return name + " : " + getVisible();
  }
}
