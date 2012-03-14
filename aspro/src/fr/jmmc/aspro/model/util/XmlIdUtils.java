/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
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
  private static char convertToNCNameChar(final char ch) {
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
}
