/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: PopCombination.java,v 1.1 2009-12-18 11:52:02 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.Pop;
import java.util.List;

/**
 * This class defines a Pop combination associated to a complete baseline (i.e. for a given station list)
 * used by the observability service
 * @author bourgesl
 */
public class PopCombination {

  /** identifier for this combination */
  private final String identifier;
  /** list of Pops with the same order than the station list */
  private final List<Pop> popList;
  /** list of pop delays (offset) corresponding to each base line (2 telescopes) */
  private List<Double> popOffsets;

  /**
   * Constructor with the given list of Pops
   * @param popList list of Pops
   */
  public PopCombination(final List<Pop> popList) {
    this.popList = popList;

    final StringBuffer sb = new StringBuffer();
    for (Pop pop : this.popList) {
      sb.append(pop.getIndex());
    }
    this.identifier = sb.toString();
  }

  public String getIdentifier() {
    return identifier;
  }

  public List<Pop> getPopList() {
    return popList;
  }

  public List<Double> getPopOffsets() {
    return popOffsets;
  }

  public void setPopOffsets(List<Double> popOffsets) {
    this.popOffsets = popOffsets;
  }

  @Override
  public String toString() {
    return this.identifier;
  }
}
