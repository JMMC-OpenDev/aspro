/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: StarObservability.java,v 1.3 2009-12-18 11:52:02 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.model;

import java.util.ArrayList;
import java.util.List;

/**
 * This class contains several results for the observability of a target
 *
 * @author bourgesl
 */
public class StarObservability {
  /** name of the target (+ pop + base line) */
  private String name;

  /* visible date intervals */
  private final List<DateTimeInterval> visible = new ArrayList<DateTimeInterval>();


  public StarObservability(final String name) {
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
