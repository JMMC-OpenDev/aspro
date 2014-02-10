/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package test;

import fr.jmmc.aspro.model.util.XmlIdUtils;

/**
 *
 * @author bourgesl
 */
public class XmlIdUtilsTest {

  /**
   * Forbidden constructor
   */
  private XmlIdUtilsTest() {
    super();
  }

  /**
   * Test
   * @param args unused
   */
  public static void main(String[] args) {

    final String[] tests = new String[]{
      "PLX_707.00",
      "Cl* Melotte 20 MMU 605",
      "2MASS J03241936+4951401",
      "PLX 707",
      "* 33 Per",
      "* alf Per",
      "AG+49 396",
      "BD+49 917",
      "CCDM J03243+4951A",
      "CSI+49 917 1",
      "CSV 100269",
      "FK5 120",
      "GC 4041",
      "GCRV 1871",
      "GEN# +5.20200605",
      "GSC 03320-02808",
      "HD 20902",
      "HIC 15863",
      "HIP 15863",
      "HR 1017",
      "IDS 03171+4930 A",
      "IRAS 03207+4941",
      "IRC +50095",
      "JP11 4562",
      "N30 690",
      "NAME MIRFAK",
      "NSV 1125",
      "PMC 90-93 85",
      "PPM 46127",
      "RAFGL 487",
      "ROT 464",
      "SAO 38787",
      "SKY# 5117",
      "SV* ZI 183",
      "TD1 2127",
      "TYC 3320-2808-1",
      "UBV 3256",
      "UBV M 40922",
      "V* alf Per",
      "[HFE83] 221",
      "uvby98 520200605",
      "Cl Melotte 20 605"
    };
    for (String val : tests) {
      test(val);
    }

  }

  /**
   * test the convert method with the given string
   * @param input input string
   */
  public static void test(final String input) {
    System.out.println("'" + input + "' = '" + XmlIdUtils.convert(input) + "'");
  }
}
