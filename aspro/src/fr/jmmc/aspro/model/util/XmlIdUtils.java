/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: XmlIdUtils.java,v 1.1 2010-11-25 17:54:40 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.model.util;

/**
 * This class translated any string to a valid xml identifier (xml:id)
 * @author bourgesl
 */
public final class XmlIdUtils {

  /**
   * Forbidden constructor
   */
  private XmlIdUtils() {
    // no-op
  }

  /**
   * Convert the given string to a valid xsd:id (NCName class)
   *
   * NCName ::=  (Letter | '_') (NCNameChar)*
   * NCNameChar ::=  Letter | Digit | '.' | '-' | '_' | CombiningChar | Extender
   *
   * @param value input string
   * @return valid xsd:id value
   */
  public static String convert(final String value) {

    final int len = value.length();
    final StringBuilder sb = new StringBuilder(len);

    char ch;
    for (int i = 0; i < len; i++) {
      ch = value.charAt(i);

      if (i == 0) {
        // check : (Letter | '_')
        if (Character.isLetter(ch)) {
          sb.append(ch);
        } else {
          sb.append('_');
          if (Character.isDigit(ch)) {
            sb.append(ch);
          } else {
            sb.append('_');
          }
        }
      } else {
        // check : NCNameChar ::=  Letter | Digit | '.' | '-' | '_' | CombiningChar | Extender
        sb.append(convertToNCNameChar(ch));
      }
    }
    return sb.toString();
  }

  /**
   * Convert any character to a valid NCNameChar (xml)
   * @param ch character to convert
   * @return valid character
   */
  private final static char convertToNCNameChar(final char ch) {
    if (Character.isLetter(ch)) {
      return ch;
    }
    if (Character.isDigit(ch)) {
      return ch;
    }
    if (ch == '.' || ch == '-' || ch == '_') {
      return ch;
    }
    // arbitrary replace char by '_' :
    return '_';
  }

  /**
   * Simple test cases
   * @param args unused
   */
  public static void main(final String[] args) {

    String[] tests = new String[]{
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
    System.out.println("'" + input + "' = '" + convert(input) + "'");
  }
}
