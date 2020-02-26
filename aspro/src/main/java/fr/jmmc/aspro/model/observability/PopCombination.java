/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.observability;

import fr.jmmc.aspro.model.oi.Pop;

/**
 * This class defines a Pop combination associated to a complete baseline (i.e. for a given station list)
 * used by the observability service
 * @author bourgesl
 */
public final class PopCombination {

  /** shared pop combination */
  private final SharedPopCombination popComb;
  /** array of pop delays (offset) corresponding to each base line (2 telescopes) */
  private double[] popOffsets;

  /**
   * Constructor with the given shared Pop combination
   * @param popComb shared pop combination
   */
  public PopCombination(final SharedPopCombination popComb) {
    this.popComb = popComb;
  }

  /**
   * Return the identifier of this combination (1123, 1355)
   * @return identifier of this combination
   */
  public String getIdentifier() {
    return this.popComb.getIdentifier();
  }

  /**
   * Return the array of Pops
   * @return array of Pops
   */
  public Pop[] getPops() {
    return this.popComb.getPops();
  }

  /**
   * Return the array of pop delays per baseline
   * @return array of pop delays per baseline
   */
  public double[] getPopOffsets() {
    return popOffsets;
  }

  /**
   * Define the array of pop delays per baseline
   * @param popOffsets array of pop delays per baseline
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
    return getIdentifier();
  }

  /**
   * Append this pop combination to the given buffer using the format '+ PopN PopN ...'
   * @param sb buffer to append to
   */
  public void toString(final StringBuilder sb) {
    sb.append(" + ");
    for (Pop pop : getPops()) {
      sb.append(pop.getName()).append(' ');
    }
  }
}
