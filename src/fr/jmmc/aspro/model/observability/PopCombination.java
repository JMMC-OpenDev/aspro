/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.observability;

import fr.jmmc.aspro.model.oi.Pop;
import java.util.List;

/**
 * This class defines a Pop combination associated to a complete baseline (i.e. for a given station list)
 * used by the observability service
 * @author bourgesl
 */
public final class PopCombination {

  /** identifier of this combination */
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

  /**
   * Return the identifier of this combination (1123, 1355)
   * @return identifier of this combination
   */
  public String getIdentifier() {
    return identifier;
  }

  /**
   * Return the list of Pops
   * @return list of Pops
   */
  public List<Pop> getPopList() {
    return popList;
  }

  /**
   * Return the list of pop delays
   * @return list of pop delays
   */
  public List<Double> getPopOffsets() {
    return popOffsets;
  }

  /**
   * Define the list of pop delays
   * @param popOffsets list of pop delays
   */
  public void setPopOffsets(List<Double> popOffsets) {
    this.popOffsets = popOffsets;
  }

  /**
   * Return the identifier of this combination (1123, 1355)
   * @return identifier of this combination (1123, 1355)
   */
  @Override
  public String toString() {
    return this.identifier;
  }

  /**
   * Append this pop combination to the given buffer using the format '+ PopN PopN ...'
   * @param sb buffer to append to
   */
  public void toString(final StringBuilder sb) {
    sb.append(" + ");
    for (Pop pop : getPopList()) {
      sb.append(pop.getName()).append(' ');
    }
  }
}
