/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: BaseOIManager.java,v 1.4 2009-10-27 16:47:17 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
import java.io.File;
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
  private static final String className_ = "fr.jmmc.aspro.config.BaseOIManager";
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
   */
  protected Object load(final String uri) {
    final Unmarshaller u = this.jf.createUnMarshaller();

    Object result = null;
    try {
      result = u.unmarshal(this.getClass().getResource(uri));
    } catch (JAXBException je) {
      logger.log(Level.SEVERE, "load failure : ", je);
    }
    return result;
  }

  /**
   * Protected save method
   * @param uri relative URI of the document to save
   * @return unmarshalled object
   */
  protected void save(final File outputFile, Object object) {
    final Marshaller marshaller = this.jf.createMarshaller();
    try {
      marshaller.marshal(object, outputFile);
    } catch (JAXBException je) {
      logger.log(Level.SEVERE, "save failure : ", je);
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
