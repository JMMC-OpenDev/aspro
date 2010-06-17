/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: CalcUVW.java,v 1.3 2010-06-17 10:02:50 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2010/01/08 16:51:17  bourgesl
 * initial uv coverage
 *
 * Revision 1.1  2010/01/05 17:17:50  bourgesl
 * added U,V,W computation
 */
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.model.BaseLine;

/**
 * This class contains static computation methods to get the UVW values for a given baseline and a target declination
 * @author bourgesl
 */
public final class CalcUVW {

  /**
   * Forbidden constructor
   */
  private CalcUVW() {
    // no-op
  }

  /**
   * Compute U(H) = sin(H)*X+cos(H)*Y
   */
  public static double computeU(final BaseLine baseLine, final double ha) {
    return Math.sin(ha) * baseLine.getX() + Math.cos(ha) * baseLine.getY();
  }

  /**
   * Compute V(H) = sin(D)(-cos(H)*X+sin(H)*Y)+cos(D)*Z
   */
  public static double computeV(final double dec, final BaseLine baseLine, final double ha) {
    return computeV(Math.cos(dec), Math.sin(dec), baseLine, ha);
  }

  /**
   * Compute V(H) = sin(D)(-cos(H)*X+sin(H)*Y)+cos(D)*Z
   */
  public static double computeV(final double cosDec, final double sinDec, final BaseLine baseLine, final double ha) {
    return sinDec * (-Math.cos(ha) * baseLine.getX() + Math.sin(ha) * baseLine.getY()) + cosDec * baseLine.getZ();
  }

  /**
   * Compute W(H) = cos(D)(cos(H)*X-sin(H)*Y)+sin(D)*Z
   */
  public static double computeW(final double dec, final BaseLine baseLine, final double ha) {
    return computeW(Math.cos(dec), Math.sin(dec), baseLine, ha);
  }

  /**
   * Compute W(H) = cos(D)(cos(H)*X-sin(H)*Y)+sin(D)*Z
   */
  public static double computeW(final double cosDec, final double sinDec, final BaseLine baseLine, final double ha) {
    return cosDec * (Math.cos(ha) * baseLine.getX() - Math.sin(ha) * baseLine.getY()) + sinDec * baseLine.getZ();
  }
}
