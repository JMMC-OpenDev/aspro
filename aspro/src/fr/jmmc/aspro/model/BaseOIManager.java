/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: BaseOIManager.java,v 1.15 2010-07-07 15:11:42 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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

import fr.jmmc.aspro.util.FileUtils;
import fr.jmmc.jaxb.JAXBFactory;
import java.io.BufferedInputStream;
import java.io.File;
import java.net.URL;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Vector;
import java.util.logging.Level;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
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
  private JAXBFactory jf;
  /** datatype factory used to create XMLGregorianCalendar instances */
  private DatatypeFactory df;

  /**
   * Protected constructor
   */
  protected BaseOIManager() {
    this.jf = JAXBFactory.getInstance(OI_JAXB_PATH);
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("BaseOIManager : " + this.jf);
    }
    try {
      this.df = DatatypeFactory.newInstance();
    } catch (DatatypeConfigurationException dce) {
      logger.log(Level.SEVERE, "Unable to resolve DatatypeFactory : ", dce);
    }
  }

  /**
   * Protected load method
   * @param uri relative URI of the document to load
   * @return unmarshalled object
   * @throws RuntimeException if the load operation failed
   */
  protected final Object loadObject(final String uri) {
    if (logger.isLoggable(Level.INFO)) {
      logger.info("loading file : " + uri);
    }
    Object result = null;

    final Unmarshaller u = this.jf.createUnMarshaller();

    try {
      // use the class loader resource resolver
      final URL url = FileUtils.getResource("fr/jmmc/aspro/model/" + uri);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("BaseOIManager.loadObject : url : " + url);
      }

      // Note : use input stream to avoid JNLP offline bug with URL (Unknown host exception)
      result = u.unmarshal(new BufferedInputStream(url.openStream()));
    } catch (Exception e) {
      throw new RuntimeException("Load failure on " + uri, e);
    }

    return result;
  }

  /**
   * Protected load method
   * @param inputFile File to load
   * @return unmarshalled object
   * @throws RuntimeException if the load operation failed
   */
  protected final Object loadObject(final File inputFile) {
    final Unmarshaller u = this.jf.createUnMarshaller();

    Object result = null;
    try {
      result = u.unmarshal(inputFile);
    } catch (JAXBException je) {
      throw new RuntimeException("Load failure on " + inputFile, je);
    }
    return result;
  }

  /**
   * Protected save method
   * @param outputFile File to save
   * @param object to marshall
   * @throws RuntimeException if the save operation failed
   */
  protected final void saveObject(final File outputFile, final Object object) throws RuntimeException {
    final Marshaller marshaller = this.jf.createMarshaller();
    try {
      marshaller.marshal(object, outputFile);
    } catch (JAXBException je) {
      throw new RuntimeException("Save failure on " + outputFile, je);
    }
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
