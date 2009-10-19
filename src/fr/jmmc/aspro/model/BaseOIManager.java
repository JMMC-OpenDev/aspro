/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: BaseOIManager.java,v 1.2 2009-10-19 15:35:18 mella Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

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

    /**
     * Protected constructor
     */
    protected BaseOIManager() {
        this.jf = JAXBFactory.getInstance(OI_JAXB_PATH);
        if (logger.isLoggable(Level.FINE)) {
            logger.fine("BaseOIManager : " + this.jf);
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
}
