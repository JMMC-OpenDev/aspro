/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.jmal.Band;

/**
 * This class provides static methods to use easily SpectralBand enum
 * @author bourgesl
 */
public final class SpectralBandUtils {

  /**
   * Forbidden constructor
   */
  private SpectralBandUtils() {
    // no-op
  }

  /**
   * Find the SpectralBand corresponding to the given Band
   * @param band band to use
   * @return SpectralBand or null
   */
  public static SpectralBand findBand(final Band band) {
    switch (band) {
      case U:
        // band U not supported return V
      case B:
        // band B not supported return V
      case V:
        return SpectralBand.V;
      case R:
        // band R not supported return V
        return SpectralBand.V;
      case I:
        return SpectralBand.I;
      case J:
        return SpectralBand.J;
      case H:
        return SpectralBand.H;
      case K:
        return SpectralBand.K;
      case L:
        // band L not supported return N
      case M:
        // band M not supported return N
      case N:
        return SpectralBand.N;
      case Q:
        // band Q not supported return N
      default:
        return null;
    }
  }
}
