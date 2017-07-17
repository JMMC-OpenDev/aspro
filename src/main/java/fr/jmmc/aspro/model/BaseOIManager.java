/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import com.sun.xml.bind.IDResolver;
import fr.jmmc.jaxb.AsproConfigurationIDResolver;
import fr.jmmc.jmcs.util.jaxb.XmlBindException;
import fr.jmmc.jmcs.util.ResourceUtils;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.net.URL;
import javax.xml.bind.JAXBException;
import javax.xml.bind.PropertyException;
import javax.xml.bind.Unmarshaller;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class manages simple IO operations (read/write) of Aspro OI Model documents
 * @author bourgesl
 */
public abstract class BaseOIManager extends BaseXmlManager {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(BaseOIManager.class.getName());
    /** classloader path to configuration files */
    public static final String CONF_CLASSLOADER_PATH = "fr/jmmc/aspro/model/";
    /** package name for JAXB generated code */
    private final static String OI_JAXB_PATH = "fr.jmmc.aspro.model.oi";

    /**
     * Protected constructor
     *
     * @throws XmlBindException if a JAXBException was caught
     */
    protected BaseOIManager() throws XmlBindException {
        super(OI_JAXB_PATH);
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

        Object result = null;
        try {
            // use the class loader resource resolver
            final URL url = ResourceUtils.getResource(CONF_CLASSLOADER_PATH + uri);

            logger.debug("BaseOIManager.loadObject: {}", url);

            // Note : use input stream to avoid JNLP offline bug with URL (Unknown host exception)
            result = createUnMarshaller().unmarshal(new BufferedInputStream(url.openStream()));

        } catch (IOException ioe) {
            throw new IllegalStateException("Load failure on " + uri, ioe);
        } catch (JAXBException je) {
            throw new IllegalArgumentException("Load failure on " + uri, je);
        }

        return result;
    }

    /**
     * Creates a JAXB Unmarshaller customized for Aspro 2 Observation Settings (custom ID resolver)
     *
     * @return JAXB Unmarshaller
     * @throws XmlBindException if a JAXBException was caught while creating an unmarshaller
     */
    @Override
    protected final Unmarshaller createCustomUnMarshaller() throws XmlBindException {
        final Unmarshaller u = this.createUnMarshaller();

        // This custom ID Resolver resolves xsd:IDREF(s) pointing to configuration elements (InterferometerDescription):
        try {
            u.setProperty(IDResolver.class.getName(), new AsproConfigurationIDResolver(ConfigurationManager.getInstance().getInitialConfiguration()));
        } catch (PropertyException pe) {
            throw new XmlBindException("JAXB Unmarshaller.setProperty() failure:", pe);
        }
        return u;
    }
}
