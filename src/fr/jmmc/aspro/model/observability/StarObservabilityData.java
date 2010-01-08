/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: StarObservabilityData.java,v 1.1 2010-01-08 16:48:29 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.3  2009/12/18 11:52:02  bourgesl
 * added Pops Finder Compatible Mode for a list of targets
 *
 */
package fr.jmmc.aspro.model.observability;

import fr.jmmc.aspro.model.observability.DateTimeInterval;
import java.util.ArrayList;
import java.util.List;

/**
 * This class contains several results for the observability of a target
 *
 * @author bourgesl
 */
public class StarObservabilityData {
  /** name of the target (+ pop + base line) */
  private String name;

  /* visible date intervals */
  private final List<DateTimeInterval> visible = new ArrayList<DateTimeInterval>();

  /**
   * Constructor
   * @param name target name
   */
  public StarObservabilityData(final String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public List<DateTimeInterval> getVisible() {
    return visible;
  }

  @Override
  public String toString() {
    return name + " : " + getVisible();
  }
}
