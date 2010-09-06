package fr.jmmc.jaxb;

import javax.xml.bind.JAXBException;

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
   * @param je JAXBException 
   */
  public XmlBindException(final JAXBException je) {
    super(je);
  }

  /** 
   * Constructs a new XmlBindException with the specified message and cause
   *
   * @param message the detail message
   * @param je JAXBException 
   */
  public XmlBindException(final String message, final JAXBException je) {
    super(message, je);
  }
}
