/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: BaseOIManager.java,v 1.23 2011-03-04 16:59:59 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.22  2011/03/03 15:51:30  bourgesl
 * added calibrator informations (searchCal main parameters and all values)
 *
 * Revision 1.21  2011/02/18 15:31:39  bourgesl
 * minor refactoring on target model serialization
 *
 * Revision 1.20  2011/02/14 15:33:10  bourgesl
 * use JMCS FileUtils
 *
 * Revision 1.19  2010/10/07 15:02:26  bourgesl
 * added load(reader)
 *
 * Revision 1.18  2010/10/04 16:25:39  bourgesl
 * proper JAXB / IO exception handling
 *
 * Revision 1.17  2010/09/26 12:47:40  bourgesl
 * better exception handling
 *
 * Revision 1.16  2010/09/24 15:52:35  bourgesl
 * removed catch RuntimeExceptionS to get it at higher level (better exception handling)
 *
 * Revision 1.15  2010/07/07 15:11:42  bourgesl
 * empty vector added
 *
 * Revision 1.14  2010/07/07 09:27:20  bourgesl
 * use FileUtils.getResource and buffered url.getOpenStream
 *
 * Revision 1.13  2010/06/17 10:02:51  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.12  2010/06/11 09:43:24  bourgesl
 * definitely use getResourceAsStream to fix bugs with jnlp offline mode
 *
 * Revision 1.11  2010/06/11 09:35:05  bourgesl
 * trying to use getResourceAsStream for jnlp offline mode
 *
 * Revision 1.10  2010/06/11 09:19:25  bourgesl
 * added log message to help debugging JNLP Offline problems
 *
 * Revision 1.9  2010/06/11 09:10:12  bourgesl
 * added log message to help debugging JNLP Offline problems
 *
 * Revision 1.8  2010/05/21 15:10:20  bourgesl
 * use classLoader.getResource() instead of this.getResource()
 *
 * Revision 1.7  2010/04/02 10:08:17  bourgesl
 * corrected logger name
 *
 * Revision 1.6  2009/12/04 16:26:58  bourgesl
 * Added Load action in the menu bar (partially handled)
 *
 * Revision 1.5  2009/12/04 15:38:27  bourgesl
 * Added Save action in the menu bar
 *
 * Revision 1.4  2009/10/27 16:47:17  bourgesl
 * fixed bug on month conversion
 *
 * Revision 1.3  2009/10/20 13:08:51  bourgesl
 * ObservationManager has methods to store observation properties
 *
 * Revision 1.2  2009/10/19 15:35:18  mella
 * add save method
 *
 * Revision 1.1  2009/10/13 16:04:14  bourgesl
 * Basic ConfigurationManager to load interferometer configuration file
 *
 * Revision 1.1  2009/09/21 15:38:51  bourgesl
 * initial jmcs gui + jaxb loader
 *
 *
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
      m.setProperty("com.sun.xml.bind.namespacePrefixMapper", AsproCustomPrefixMapper.getInstance());
    } catch (PropertyException pe) {
      // if the JAXB provider doesn't recognize the prefix mapper,
      // it will throw this exception. Since being unable to specify
      // a human friendly prefix is not really a fatal problem,
      // you can just continue marshalling without failing

      logger.log(Level.WARNING, "jaxb property failure", pe);
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
