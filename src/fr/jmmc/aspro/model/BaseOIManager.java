/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: BaseOIManager.java,v 1.11 2010-06-11 09:35:05 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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

import fr.jmmc.jaxb.JAXBFactory;
import fr.jmmc.mcs.util.Urls;
import java.io.File;
import java.net.URL;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
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
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** package name for JAXB generated code */
  private final static String OI_JAXB_PATH = "fr.jmmc.aspro.model.oi";
  // members :
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
  protected Object loadObject(final String uri) {
    final Unmarshaller u = this.jf.createUnMarshaller();

    Object result = null;

    // use the class loader :
    final URL url = this.getClass().getClassLoader().getResource("fr/jmmc/aspro/model/" + uri);

    if (logger.isLoggable(Level.INFO)) {
      logger.info("loading configuration from : " + url);
    }

    try {
      result = u.unmarshal(url);
    } catch (JAXBException je) {
      if (logger.isLoggable(Level.INFO)) {
        logger.log(Level.INFO, "unmarshalling failure : ", je);
      }
    }
    if (result == null) {
      if (logger.isLoggable(Level.INFO)) {
        logger.info("using stream : " + url);
      }
      try {
        result = u.unmarshal(this.getClass().getClassLoader().getResourceAsStream("fr/jmmc/aspro/model/" + uri));
      } catch (JAXBException je) {
        throw new RuntimeException("Load failure on " + uri, je);
      }
    }
    return result;
  }

  /**
   * Protected load method
   * @param inputFile File to load
   * @return unmarshalled object
   * @throws RuntimeException if the load operation failed
   */
  protected Object loadObject(final File inputFile) {
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
  protected void saveObject(final File outputFile, final Object object) throws RuntimeException {
    final Marshaller marshaller = this.jf.createMarshaller();
    try {
      marshaller.marshal(object, outputFile);
    } catch (JAXBException je) {
      throw new RuntimeException("Save failure on " + outputFile, je);
    }
  }

  protected XMLGregorianCalendar getCalendar(final Date date) {
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
