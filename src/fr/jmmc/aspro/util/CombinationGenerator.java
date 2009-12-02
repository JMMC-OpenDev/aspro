/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: CombinationGenerator.java,v 1.1 2009-12-02 17:23:51 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.util;

/**
 * Systematically generate unique combinations.
 * @author bourgesl
 */
public class CombinationGenerator {

  private int[] a;
  private int n;
  private int r;
  private long numLeft;
  private long total;
//------------
// Constructor
//------------

  public CombinationGenerator(final int n, final int r) {
    super();
    if (r > n) {
      throw new IllegalArgumentException();
    }
    if (n < 1) {
      throw new IllegalArgumentException();
    }
    this.n = n;
    this.r = r;
    this.a = new int[r];

    final long nFact = getFactorial(n);
    final long rFact = getFactorial(r);
    final long nminusrFact = getFactorial(n - r);

    this.total = nFact / (rFact * nminusrFact);
    reset();
  }
//------
// Reset
//------

  public void reset() {
    for (int i = 0, size = a.length; i < size; i++) {
      a[i] = i;
    }
    this.numLeft = this.total;
  }
//------------------------------------------------
// Return number of combinations not yet generated
//------------------------------------------------

  public long getNumLeft() {
    return numLeft;
  }
//-----------------------------
// Are there more combinations?
//-----------------------------

  public boolean hasMore() {
    return numLeft > 0;
  }
//------------------------------------
// Return total number of combinations
//------------------------------------

  public long getTotal() {
    return total;
  }
//------------------
// Compute factorial
//------------------

  private static long getFactorial(final int n) {
    long fact = 1l;
    for (int i = n; i > 1; i--) {
      fact *= i;
    }
    return fact;
  }
//--------------------------------------------------------
// Generate next combination (algorithm from Rosen p. 286)
//--------------------------------------------------------

  public int[] getNext() {
    if (this.numLeft == this.total) {
      this.numLeft--;
      return a;
    }
    int i = r - 1;
    while (a[i] == n - r + i) {
      i--;
    }
    a[i] += 1;
    for (int j = i + 1; j < r; j++) {
      a[j] = a[i] + j - i;
    }
    this.numLeft--;
    return a;
  }
}
