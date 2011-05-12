/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.jaxb.AsproCustomPrefixMapper;
import fr.jmmc.jaxb.JAXBFactory;
import fr.jmmc.jaxb.XmlBindException;
import fr.jmmc.mcs.util.FileUtils;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.net.URL;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Vector;
import java.util.logging.Level;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.PropertyException;
import javax.xml.bind.UnmarshalException;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeConstants;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

/**
 * This class manages simple IO operations (read/write) of Aspro OI Model documents
 * @author bourgesl
 */
public class BaseOIManager {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.model.BaseOIManager";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** JAXB property to define a custom namspace prefix mapper */
  public static final String JAXB_NAMESPACE_PREFIX_MAPPER = "com.sun.xml.bind.namespacePrefixMapper";
  /** package name for JAXB generated code */
  private final static String OI_JAXB_PATH = "fr.jmmc.aspro.model.oi";
  /** empty vector */
  protected final static Vector<String> EMPTY_VECTOR = new Vector<String>(0);

  /* members */
  /** internal JAXB Factory */
  private final JAXBFactory jf;
  /** datatype factory used to create XMLGregorianCalendar instances */
  private final DatatypeFactory df;

  /**
   * Protected constructor
   *
   * @throws XmlBindException if a JAXBException was caught
   */
  protected BaseOIManager() throws XmlBindException {

    this.jf = JAXBFactory.getInstance(OI_JAXB_PATH);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("BaseOIManager : " + this.jf);
    }
    try {
      this.df = DatatypeFactory.newInstance();
    } catch (DatatypeConfigurationException dce) {
      throw new XmlBindException("Unable to resolve DatatypeFactory : ", dce);
    }
  }

  /**
   * Protected load method used by ConfigurationManager.initialize to load the aspro configuration files
   * @param uri relative URI of the document to load (class loader)
   * @return unmarshalled object
   *
   * @throws IllegalStateException if the file is not found or an I/O exception occured
   * @throws IllegalArgumentException if the load operation failed
   * @throws XmlBindException if a JAXBException was caught while creating an unmarshaller
   */
  protected final Object loadObject(final String uri)
          throws IllegalStateException, IllegalArgumentException, XmlBindException {

    if (logger.isLoggable(Level.INFO)) {
      logger.info("loading file : " + uri);
    }
    Object result = null;

    try {
      // use the class loader resource resolver
      final URL url = FileUtils.getResource("fr/jmmc/aspro/model/" + uri);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("BaseOIManager.loadObject : url : " + url);
      }

      // Note : use input stream to avoid JNLP offline bug with URL (Unknown host exception)
      result = this.jf.createUnMarshaller().unmarshal(new BufferedInputStream(url.openStream()));

    } catch (IOException ioe) {
      throw new IllegalStateException("Load failure on " + uri, ioe);
    } catch (JAXBException je) {
      throw new IllegalArgumentException("Load failure on " + uri, je);
    }

    return result;
  }

  /**
   * Protected load method
   * @param inputFile File to load
   * @return unmarshalled object
   *
   * @throws IOException if an I/O exception occured
   * @throws IllegalStateException if an unexpected exception occured
   * @throws XmlBindException if a JAXBException was caught while creating an unmarshaller
   */
  protected final Object loadObject(final File inputFile)
          throws IOException, IllegalStateException, XmlBindException {

    Object result = null;
    try {

      final long start = System.nanoTime();

      result = this.jf.createUnMarshaller().unmarshal(inputFile);

      if (logger.isLoggable(Level.INFO)) {
        logger.info("unmarshall : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");
      }

    } catch (JAXBException je) {
      handleException("Load failure on " + inputFile, je);
    }
    return result;
  }

  /**
   * Protected load method
   * @param reader any reader
   * @return unmarshalled object
   *
   * @throws IOException if an I/O exception occured
   * @throws IllegalStateException if an unexpected exception occured
   * @throws XmlBindException if a JAXBException was caught while creating an unmarshaller
   */
  protected final Object loadObject(final Reader reader)
          throws IOException, IllegalStateException, XmlBindException {

    Object result = null;
    try {

      final long start = System.nanoTime();

      result = this.jf.createUnMarshaller().unmarshal(reader);

      if (logger.isLoggable(Level.INFO)) {
        logger.info("unmarshall : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");
      }

    } catch (JAXBException je) {
      handleException("Load failure on " + reader, je);
    }
    return result;
  }

  /**
   * Protected save method
   * @param outputFile File to save
   * @param object to marshall
   *
   * @throws IOException if an I/O exception occured
   * @throws IllegalStateException if an unexpected exception occured
   * @throws XmlBindException if a JAXBException was caught while creating an marshaller
   */
  protected final void saveObject(final File outputFile, final Object object)
          throws IOException, IllegalStateException {
    try {

      final long start = System.nanoTime();

      this.createMarshaller().marshal(object, outputFile);

      if (logger.isLoggable(Level.INFO)) {
        logger.info("marshall : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");
      }

    } catch (JAXBException je) {
      handleException("Save failure on " + outputFile, je);
    }
  }

  /**
   * Public save method
   * @param writer writer to use
   * @param object to marshall
   *
   * @throws IllegalStateException if an unexpected exception occured
   */
  public final void saveObject(final Writer writer, final Object object)
          throws IllegalStateException {
    try {
      this.createMarshaller().marshal(object, writer);
    } catch (JAXBException je) {
      throw new IllegalStateException("Serialization failure", je);
    }
  }

  /**
   * Creates a JAXB Marshaller customized for Aspro 2 (name spaces)
   *
   * @return JAXB Marshaller
   * @throws XmlBindException if a JAXBException was caught while creating an marshaller
   */
  private final Marshaller createMarshaller() throws XmlBindException {
    final Marshaller m = this.jf.createMarshaller();

    /*
    to specify the URI->prefix mapping, you'll need to provide an
    implementation of NamespacePrefixMapper, which determines the
    prefixes used for marshalling.
    
    you specify this as a property of Marshaller to
    tell the marshaller to consult your mapper
    to assign a prefix for a namespace.
     */
    try {
      m.setProperty(JAXB_NAMESPACE_PREFIX_MAPPER, AsproCustomPrefixMapper.getInstance());
    } catch (PropertyException pe) {
      // if the JAXB provider doesn't recognize the prefix mapper,
      // it will throw this exception. Since being unable to specify
      // a human friendly prefix is not really a fatal problem,
      // you can just continue marshalling without failing

      if (logger.isLoggable(Level.WARNING)) {
        logger.warning("marshaller class = " + m.getClass().getName());
        logger.log(Level.WARNING, "JAXB Marshaller.setProperty failure", pe);
      }
    }
    return m;
  }

  /**
   * Handle JAXB Exception to extract IO Exception or unexpected exceptions
   * @param message message
   * @param je jaxb exception
   * 
   * @throws IllegalStateException if an unexpected exception occured
   * @throws IOException if an I/O exception occured
   */
  protected final static void handleException(final String message, final JAXBException je) throws IllegalStateException, IOException {
    final Throwable cause = je.getCause();
    if (cause != null) {
      if (cause instanceof IOException) {
        throw (IOException) cause;
      }
    }
    if (je instanceof UnmarshalException) {
      throw new IllegalArgumentException("The loaded file does not correspond to a valid file", cause);
    }
    throw new IllegalStateException(message, je);
  }

  /**
   * Convert the given date to a XMLGregorianCalendar instance using only date information
   * @param date date argument
   * @return XMLGregorianCalendar instance
   */
  protected final XMLGregorianCalendar getCalendar(final Date date) {
    final GregorianCalendar calendar = new GregorianCalendar();
    calendar.setTime(date);

    // the month field is given in the range [0;11]

    // Create an XMLGregorianCalendar with the given date :
    return this.df.newXMLGregorianCalendarDate(
            calendar.get(Calendar.YEAR),
            calendar.get(Calendar.MONTH) + 1,
            calendar.get(Calendar.DAY_OF_MONTH),
            DatatypeConstants.FIELD_UNDEFINED);
  }
}
