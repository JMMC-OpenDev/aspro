/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.jaxb;

import java.util.concurrent.ConcurrentHashMap;

import java.util.logging.Level;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

/**
 * JAXBFactory is an utility class to configure JAXB context & properties
 *
 * @author Laurent Bourges
 */
public final class JAXBFactory {
  //~ Constants --------------------------------------------------------------------------------------------------------

  /** Class Name */
  private static final String className_ = "fr.jmmc.jaxb.JAXBFactory";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /**
   * JAXB 2 Context Factory (System property) "javax.xml.bind.JAXBContext" instead of "javax.xml.bind.context.factory"
   * bug reported : http://java.net/jira/browse/JAXB-816
   */
  public static final String JAXB_CONTEXT_FACTORY = "javax.xml.bind.JAXBContext";
  /** JAXB implementation 2.1.12 provided in JMCS libraries */
  public static final String JAXB_CONTEXT_FACTORY_IMPLEMENTATION = "com.sun.xml.bind.v2.ContextFactory";
  /** all factories */
  private static ConcurrentHashMap<String, JAXBFactory> managedInstances = new ConcurrentHashMap<String, JAXBFactory>(4);
  //~ Members ----------------------------------------------------------------------------------------------------------
  /** jaxb context path : used to find a factory */
  private final String jaxbPath;
  /** JAXB Context for the given jaxb context path */
  private JAXBContext jc = null;

  //~ Constructors -----------------------------------------------------------------------------------------------------
  /**
   * Creates a new JPAFactory object
   *
   * @param pJaxbPath jaxb context path
   */
  private JAXBFactory(final String pJaxbPath) {
    this.jaxbPath = pJaxbPath;
  }

  //~ Methods ----------------------------------------------------------------------------------------------------------
  /**
   * Factory singleton per jaxb-context-path pattern
   *
   * @param jaxbPath jaxb context path
   *
   * @return JAXBFactory initialized
   *
   * @throws XmlBindException if a JAXBException was caught
   */
  public static JAXBFactory getInstance(final String jaxbPath) throws XmlBindException {
    JAXBFactory jf = managedInstances.get(jaxbPath);

    if (jf == null) {
      jf = new JAXBFactory(jaxbPath);

      jf.initialize();

      if (jf != null) {
        managedInstances.putIfAbsent(jaxbPath, jf);
        // to be sure to return the singleton :
        jf = managedInstances.get(jaxbPath);
      }
    }

    return jf;
  }

  /**
   * Initializes the JAXB Context
   *
   * @throws XmlBindException if a JAXBException was caught
   */
  protected void initialize() throws XmlBindException {
    try {
      this.jc = getContext(jaxbPath);
    } catch (XmlBindException xbe) {
      logger.log(Level.SEVERE, "JAXBFactory.initialize : JAXB failure : ", xbe);
      throw xbe;
    }
  }

  /**
   * JAXBContext factory for a given path
   *
   * @param path given path
   * @return JAXBContext instance
   * @throws XmlBindException if a JAXBException was caught
   */
  private JAXBContext getContext(final String path) throws XmlBindException {
    JAXBContext c = null;

    // Define the system property to define which JAXB implementation to use :
    System.setProperty(JAXB_CONTEXT_FACTORY, JAXB_CONTEXT_FACTORY_IMPLEMENTATION);

    if (logger.isLoggable(Level.INFO)) {
      logger.info("JAXB implementation = " + System.getProperty(JAXB_CONTEXT_FACTORY));
    }

    try {
      // create a JAXBContext capable of handling classes generated into
      // ivoa schema package
      c = JAXBContext.newInstance(path);

    } catch (JAXBException je) {
      throw new XmlBindException("JAXBFactory.getContext : Unable to create JAXBContext : " + path, je);
    }

    return c;
  }

  /**
   * Returns JAXB Context for the given jaxb context path
   *
   * @return JAXB Context for the given jaxb context path
   */
  private JAXBContext getJAXBContext() {
    return jc;
  }

  /**
   * Creates a JAXB Unmarshaller
   *
   * @return JAXB Unmarshaller
   * @throws XmlBindException if a JAXBException was caught while creating an unmarshaller
   */
  public Unmarshaller createUnMarshaller() throws XmlBindException {
    Unmarshaller u = null;

    try {
      // create an Unmarshaller
      u = getJAXBContext().createUnmarshaller();

    } catch (JAXBException je) {
      throw new XmlBindException("JAXBFactory.createUnMarshaller : JAXB Failure", je);
    }

    return u;
  }

  /**
   * Creates a JAXB Marshaller
   *
   * @return JAXB Marshaller
   * @throws XmlBindException if a JAXBException was caught while creating an marshaller
   */
  public Marshaller createMarshaller() throws XmlBindException {
    Marshaller m = null;

    try {
      // create an Unmarshaller
      m = getJAXBContext().createMarshaller();

      m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
    } catch (JAXBException je) {
      throw new XmlBindException("JAXBFactory.createMarshaller : JAXB Failure", je);
    }

    return m;
  }
}
//~ End of file --------------------------------------------------------------------------------------------------------

