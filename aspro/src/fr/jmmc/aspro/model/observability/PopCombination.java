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
  private double[] popOffsets;

  /**
   * Factory method : create one PopCombination for the given list of PoPs
   * @param popList list of PoPs
   * @param sb string builder used to build the identifier (empty when returning from this method)
   * @return one PopCombination for the given list of PoPs
   */
  public static PopCombination newInstance(final List<Pop> popList, final StringBuilder sb) {
    final String id = Pop.toString(sb, popList);
    // recycle the given string builder
    sb.setLength(0);
    return new PopCombination(id, popList);
  }

  /**
   * Constructor with the given list of Pops
   * @param identifier identifier of this combination
   * @param popList list of Pops
   */
  private PopCombination(final String identifier, final List<Pop> popList) {
    this.popList = popList;
    this.identifier = identifier;
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
   * Return the list of pop delays per baseline
   * @return list of pop delays per baseline
   */
  public double[] getPopOffsets() {
    return popOffsets;
  }

  /**
   * Define the list of pop delays per baseline
   * @param popOffsets list of pop delays per baseline
   */
  public void setPopOffsets(final double[] popOffsets) {
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
