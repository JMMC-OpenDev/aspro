/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: CombUtils.java,v 1.1 2010-06-30 14:53:29 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.util;

import java.util.ArrayList;
import java.util.List;

/**
 * This class gathers several combinatory algorithms
 * @author bourgesl
 */
public final class CombUtils {

  /**
   * Forbidden constructor
   */
  private CombUtils() {
    // no-op
  }

  /**
   * Return factorial(n) = n!
   * @param n integer
   * @return factorial(n)
   */
  public final static int fact(final int n) {
    int res = 1;
    for (int i = 1; i <= n; i++) {
      res *= i;
    }
    return res;
  }

  /**
   * Return the number of arrangements without repetition
   * @param n number of elements
   * @param k number of items to choose
   * @return number of arrangements
   */
  public final static int arr(final int n, final int k) {
    int res = 1;

    // A-n-k = n! / (n - k)!
    for (int i = n, min = n - k + 1; i >= min; i--) {
      res *= i;
    }
    return res;
  }

  /**
   * Return the number of combinations (no repetition, no ordering)
   * @param n number of elements
   * @param k number of items to choose
   * @return number of generateCombinations
   */
  public final static int comb(final int n, final int k) {
    //C-n-k = A-n-k/k!
    return arr(n, k) / fact(k);
  }

  /**
   * Generate all combinations (no repetition, no ordering)
   * @param n number of elements
   * @param k number of items to choose
   * @return list of all combinations (integer arrays)
   */
  public final static List<int[]> generateCombinations(final int n, final int k) {
    final int count = comb(n, k);

    final List<int[]> results = new ArrayList<int[]>(count);

    recursiveCombinations(n, k, results, new int[k], 0, 0);

    return results;
  }

  /**
   * Recursive algorithm to generate all combinations
   * @param n number of elements
   * @param k number of items to choose
   * @param results result array
   * @param current current array
   * @param position position in the array
   * @param nextInt next integer value
   */
  private static void recursiveCombinations(final int n, final int k, final List<int[]> results, final int[] current, final int position, final int nextInt) {
    for (int i = nextInt; i < n; i++) {
      current[position] = i;

      if (position + 1 == k) {
        // copy current result :
        final int[] res = new int[k];
        System.arraycopy(current, 0, res, 0, k);
        results.add(res);
      } else {
        recursiveCombinations(n, k, results, current, position + 1, i + 1);
      }
    }
  }

  /**
   * Return the number of n-tuples (repetition allowed)
   * @param n number of elements
   * @param k number of items to choose
   * @return number of n-tuples
   */
  public final static int tuples(final int n, final int k) {
    int res = 1;
    for (int i = 0; i < k; i++) {
      res *= n;
    }
    return res;
  }

  /**
   * Generate all n-tuples (repetition allowed)
   * @param n number of elements
   * @param k number of items to choose
   * @return list of all n-tuples (integer arrays)
   */
  public final static List<int[]> generateTuples(final int n, final int k) {
    final int count = tuples(n, k);

    final List<int[]> results = new ArrayList<int[]>(count);

    recursiveTuples(n, k, results, new int[k], 0, 0);

    return results;
  }

  /**
   * Recursive algorithm to generate all n-tuples
   * @param n number of elements
   * @param k number of items to choose
   * @param results result array
   * @param current current array
   * @param position position in the array
   * @param nextInt next integer value
   */
  private static void recursiveTuples(final int n, final int k, final List<int[]> results, final int[] current, final int position, final int nextInt) {
    for (int i = nextInt; i < n; i++) {
      current[position] = i;

      if (position + 1 == k) {
        // copy current result :
        final int[] res = new int[k];
        System.arraycopy(current, 0, res, 0, k);
        results.add(res);
      } else {
        recursiveTuples(n, k, results, current, position + 1, 0);
      }
    }
  }
}
