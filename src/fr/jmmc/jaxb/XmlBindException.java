/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.jaxb;

/**
 * This class is a RuntimeException wrapping a JAXB Exception or a validation failure 
 * 
 * @author Laurent Bourges
 */
public final class XmlBindException extends RuntimeException {
  //~ Constants --------------------------------------------------------------------------------------------------------

  /** serial UID for Serializable interface */
  private static final long serialVersionUID = 1L;

  //~ Members ----------------------------------------------------------------------------------------------------------

  /** 
   * Constructs a new XmlBindException with the specified cause
   *
   * @param e JAXB Exception
   */
  public XmlBindException(final Exception e) {
    super(e);
  }

  /** 
   * Constructs a new XmlBindException with the specified message and cause
   *
   * @param message the detail message
   * @param e JAXB Exception
   */
  public XmlBindException(final String message, final Exception e) {
    super(message, e);
  }
}
