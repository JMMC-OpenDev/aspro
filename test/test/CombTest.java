/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package test;

import fr.jmmc.aspro.util.CombUtils;
import java.util.List;

/**
 *
 * @author bourgesl
 */
public class CombTest {

  /**
   * Forbidden constructor
   */
  private CombTest() {
    super();
  }

  /**
   * Test
   * @param args unused
   */
  public static void main(String[] args) {
    test(2);
    test(3);
    test(4);
//    test(6);

    List<int[]> res;

    System.out.println("permutations 2 :");
    res = CombUtils.generatePermutations(2);
    showResults(res);

    System.out.println("permutations 3 :");
    res = CombUtils.generatePermutations(3);
    showResults(res);

    System.out.println("permutations 4 :");
    res = CombUtils.generatePermutations(4);
    showResults(res);

    System.out.println("----------------------------------------");
  }

  private static final void test(final int nTel) {
    List<int[]> res;

    System.out.println("Telescopes : " + nTel);

    final int nTuples = CombUtils.tuples(5, nTel);
    System.out.println("nPops (" + nTel + "-tuples[5]) = " + nTuples);

    res = CombUtils.generateTuples(5, nTel);
    if (res.size() == nTuples) {
      System.out.println("tuples : ok");
    }
    showResults(res);

    System.out.println("nBaseLines (C" + nTel + "-2) = " + CombUtils.comb(nTel, 2));

    res = CombUtils.generateCombinations(nTel, 2);
    showResults(res);

    System.out.println("nTriplets  (C" + nTel + "-3) = " + CombUtils.comb(nTel, 3));

    if (nTel >= 3) {
      res = CombUtils.generateCombinations(nTel, 3);
      showResults(res);
    }

    System.out.println("----------------------------------------");
  }

  private static void showResults(final List<int[]> results) {
    for (int[] res : results) {
      for (int i = 0, size = res.length; i < size; i++) {
        System.out.print(res[i]);
      }
      System.out.println();
    }
  }
}
