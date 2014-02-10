/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.observability;

import fr.jmmc.aspro.model.oi.Pop;

/**
 * This class defines a Pop combination (shared) used by the observability service
 * @author bourgesl
 */
public final class SharedPopCombination {

  /** identifier of this combination */
  private final String identifier;
  /** array of Pops with the same order than the station list */
  private final Pop[] pops;

  /**
   * Factory method : create one PopCombination for the given PoPs
   * @param pops array of PoPs
   * @param sb string builder used to build the identifier (empty when returning from this method)
   * @return one PopCombination for the given PoPs
   */
  public static SharedPopCombination newInstance(final Pop[] pops, final StringBuilder sb) {
    final String id = Pop.toString(sb, pops);
    // recycle the given string builder
    sb.setLength(0);
    return new SharedPopCombination(id, pops);
  }

  /**
   * Constructor with the given array of Pops
   * @param identifier identifier of this combination
   * @param pops array of Pops
   */
  private SharedPopCombination(final String identifier, final Pop[] pops) {
    this.pops = pops;
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
   * Return the array of Pops
   * @return array of Pops
   */
  public Pop[] getPops() {
    return pops;
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
    for (Pop pop : this.pops) {
      sb.append(pop.getName()).append(' ');
    }
  }
}
