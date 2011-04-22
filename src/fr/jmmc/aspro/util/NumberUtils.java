/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: NumberUtils.java,v 1.1 2011-04-22 15:36:08 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.util;

/**
 * This class handles double number comparisons with absolute error
 * http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm
 *
 * @author bourgesl
 */
public final class NumberUtils {

  /**
   * Smallest positive number used in double comparisons (rounding).
   */
  public final static double EPSILON = 1e-6d;

  /**
   * Returns true if two doubles are considered equal.  
   * Test if the absolute difference between two doubles has a difference less than EPSILON.
   *
   * @param a double to compare.
   * @param b double to compare.
   * @return true true if two doubles are considered equal.
   */
  public static boolean equals(final double a, final double b) {
    return equals(a, b, EPSILON);
  }

  /**
   * Returns true if two doubles are considered equal. 
   * 
   * Test if the absolute difference between the two doubles has a difference less then a given
   * double (epsilon).
   *
   * @param a double to compare.
   * @param b double to compare
   * @param epsilon double which is compared to the absolute difference.
   * @return true if a is considered equal to b.
   */
  public static boolean equals(final double a, final double b, final double epsilon) {
    return (a == b) ? true : (Math.abs(a - b) < epsilon);
  }

  /**
   * Returns true if the first double is considered greater than the second
   * double.  
   * 
   * Test if the difference of first minus second is greater than EPSILON.
   *
   * @param a first double
   * @param b second double
   * @return true if the first double is considered greater than the second
   *              double
   */
  public static boolean greaterThan(final double a, final double b) {
    return greaterThan(a, b, EPSILON);
  }

  /**
   * Returns true if the first double is considered greater than the second
   * double.
   *
   * Test if the difference of first minus second is greater then
   * a given double (epsilon).
   *
   * @param a first double
   * @param b second double
   * @param epsilon double which is compared to the absolute difference.
   * @return true if the first double is considered greater than the second
   *              double
   */
  public static boolean greaterThan(final double a, final double b, final double epsilon) {
    return a + epsilon - b > 0d;
  }

  /**
   * Returns true if the first double is considered less than the second
   * double.
   *
   * Test if the difference of second minus first is greater than EPSILON.
   *
   * @param a first double
   * @param b second double
   * @return true if the first double is considered less than the second
   *              double
   */
  public static boolean lessThan(final double a, final double b) {
    return greaterThan(b, a, EPSILON);
  }

  /**
   * Returns true if the first double is considered less than the second
   * double.  Test if the difference of second minus first is greater then
   * a given double (epsilon).  Determining the given epsilon is highly
   * dependant on the precision of the doubles that are being compared.
   *
   * @param a first double
   * @param b second double
   * @param epsilon double which is compared to the absolute difference.
   * @return true if the first double is considered less than the second
   *              double
   */
  public static boolean lessThan(final double a, final double b, final double epsilon) {
    return a - epsilon - b < 0d;
  }

  /**
   * Private constructor
   */
  private NumberUtils() {
    super();
  }
}
