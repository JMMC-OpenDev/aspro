/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AtmosphereQualityUtils.java,v 1.1 2010-07-22 14:31:55 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

import fr.jmmc.aspro.model.oi.AtmosphereQuality;
import java.util.Vector;

/**
 * This class provides static methods to use easily AtmosphereQuality enum
 * @author bourgesl
 */
public final class AtmosphereQualityUtils {

  /** computed AtmosphereQuality  */
  private static Vector<String> atmosphereQualityList = null;

  /**
   * Forbidden constructor
   */
  private AtmosphereQualityUtils() {
    // no-op
  }

  /**
   * Return the vector of all atmosphere quality items as string
   * @return vector of all atmosphere quality items as string
   */
  public static Vector<String> getAtmosphereQualityList() {
    if (atmosphereQualityList == null) {
      final AtmosphereQuality[] vals = AtmosphereQuality.values();
      atmosphereQualityList = new Vector<String>(vals.length);
      for (AtmosphereQuality a : vals) {
        atmosphereQualityList.add(a.value());
      }
    }
    return atmosphereQualityList;
  }

  /**
   * Return the atmosphere quality for the given string
   * @param value atmosphere quality as string
   * @return atmosphere quality item
   */
  public static AtmosphereQuality getAtmosphereQuality(final String value) {
    return AtmosphereQuality.fromValue(value);
  }

  /**
   * Return the corresponding seeing in arc seconds for the given atmosphere quality
   * @param atmQuality atmosphere quality
   * @return seeing in arc seconds
   */
  public static double getSeeing(final AtmosphereQuality atmQuality) {
    switch (atmQuality) {
      default:
      case AVERAGE:
        return 1.0d;
      case GOOD:
        return 0.6d;
      case EXCELLENT:
        return 0.4d;
      case BAD:
        return 1.4d;
      case AWFUL:
        return 1.8d;
    }
  }

  /**
   * Tests
   * @param args unused
   */
  public static void main(String[] args) {

    System.out.println("atm = " + AtmosphereQuality.AVERAGE);

    System.out.println("atm list = " + getAtmosphereQualityList());

    System.out.println("atm = " + getAtmosphereQuality(AtmosphereQuality.AVERAGE.value()));

    System.out.println("seeing = " + getSeeing(AtmosphereQuality.AVERAGE));

  }
}
