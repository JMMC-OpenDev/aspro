/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ComplexUtils.java,v 1.1 2010-07-09 14:55:56 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.util;

import org.apache.commons.math.complex.Complex;

/**
 * This class contains static methods dedicated to computations on complex types
 * @author bourgesl
 */
public final class ComplexUtils {

  /**
   * Forbidden contructor
   */
  private ComplexUtils() {
  }

  /**
   * Compute the bispectrum C12*C23*C31
   * @param C12 complex data for couple 12
   * @param C23 complex data for couple 23
   * @param C31 complex data for couple 31
   * @return bispectrum C12*C23*C31
   */
  public static Complex bispectrum(final Complex C12, final Complex C23, final Complex C31) {

    final double x1 = C12.getReal();
    final double y1 = C12.getImaginary();

    final double x2 = C23.getReal();
    final double y2 = C23.getImaginary();

    final double x3 = C31.getReal();
    final double y3 = C31.getImaginary();

    final double re = x1 * x2 * x3 - x1 * y2 * y3 - y1 * x2 * y3 - y1 * y2 * x3;
    final double im = x1 * x2 * y3 + x1 * y2 * x3 + y1 * x2 * x3 - y1 * y2 * y3;

    return new Complex(re, im);
  }
}
