/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package test;

import fr.jmmc.jmal.ALX;

/**
 *
 * @author bourgesl
 */
public class GeocentricCoordsTest {

  /**
   * Forbidden constructor
   */
  private GeocentricCoordsTest() {
    super();
  }

  /**
   * Test coordinate conversions
   * @param args unused
   */
  public static void main(String[] args) {
    /*
    -24.570106, -70.406044
    -24° 34' 12.38", -70° 24' 21.76
     */
    System.out.println("-24.570106 [-24° 34' 12.38\"] : " + ALX.toDMS(-24.570106d));
    System.out.println("-70.406044 [-70° 24' 21.76\"] : " + ALX.toDMS(-70.406044d));
  }
}
