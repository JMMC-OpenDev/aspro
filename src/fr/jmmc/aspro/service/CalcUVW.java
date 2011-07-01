/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
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
   * @param baseLine baseline vector to use
   * @param ha hour angle in radians
   * @return U coordinate in interferometric plan
   */
  public static double computeU(final BaseLine baseLine, final double ha) {
    return Math.sin(ha) * baseLine.getX() + Math.cos(ha) * baseLine.getY();
  }

  /**
   * Compute V(H) = sin(D)(-cos(H)*X+sin(H)*Y)+cos(D)*Z
   * @param cosDec cosinus of target declination
   * @param sinDec sinus of target declination
   * @param baseLine baseline vector to use
   * @param ha hour angle in radians
   * @return V coordinate in interferometric plan
   */
  public static double computeV(final double cosDec, final double sinDec, final BaseLine baseLine, final double ha) {
    return sinDec * (-Math.cos(ha) * baseLine.getX() + Math.sin(ha) * baseLine.getY()) + cosDec * baseLine.getZ();
  }

  /**
   * Compute W(H) = cos(D)(cos(H)*X-sin(H)*Y)+sin(D)*Z
   * @param cosDec cosinus of target declination
   * @param sinDec sinus of target declination
   * @param baseLine baseline vector to use
   * @param ha hour angle in radians
   * @return W coordinate in interferometric plan
   */
  public static double computeW(final double cosDec, final double sinDec, final BaseLine baseLine, final double ha) {
    return cosDec * (Math.cos(ha) * baseLine.getX() - Math.sin(ha) * baseLine.getY()) + sinDec * baseLine.getZ();
  }
}
