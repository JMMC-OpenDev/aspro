/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: PermutationGenerator.java,v 1.1 2009-12-02 17:23:51 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.util;

/**
 * Systematically generate all permutations given an array.
 * @author bourgesl
 */
public class PermutationGenerator<T> {

  private final T[] array;
  private final int[] state;
  private int position = 0;

  public PermutationGenerator(final T[] data) {
    this.array = data;
    this.state = new int[data.length];
    this.position = 0;
  }

  public boolean next() {
    if (position == 0) {
      position++;
      return true;
    }
    while (position < state.length) {
      if (state[position] < position) {
        final int index = ((position % 2) == 0) ? 0 : state[position];
        swap(position, index);
        state[position]++;
        position = 1;
        return true;
      } else {
        state[position] = 0;
        position++;
      }
    }
    return false;
  }

  private void swap(final int i, final int j) {
    final T tmp = array[i];
    array[i] = array[j];
    array[j] = tmp;
  }
}
