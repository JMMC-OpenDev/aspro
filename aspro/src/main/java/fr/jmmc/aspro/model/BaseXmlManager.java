/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.jaxb.AsproCustomPrefixMapper;
import fr.jmmc.jmcs.util.jaxb.JAXBFactory;
import fr.jmmc.jmcs.util.jaxb.XmlBindException;
import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.PropertyException;
import javax.xml.bind.UnmarshalException;
import javax.xml.bind.Unmarshaller;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeConstants;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class manages simple IO operations (read/write) of XML documents
 * @author bourgesl
 */
public abstract class BaseXmlManager {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(BaseXmlManager.class.getName());
    /** JAXB property to define a custom namspace prefix mapper */
    public static final String JAXB_NAMESPACE_PREFIX_MAPPER = "com.sun.xml.bind.namespacePrefixMapper";

    /* members */
    /** internal JAXB Factory */
    private final JAXBFactory jf;
    /** datatype factory used to create XMLGregorianCalendar instances */
    private final DatatypeFactory df;

    /**
     * Protected constructor
     *
     * @param jaxbPath JAXB context path
     * @throws XmlBindException if a JAXBException was caught
     */
    protected BaseXmlManager(final String jaxbPath) throws XmlBindException {
        this.jf = JAXBFactory.getInstance(jaxbPath);

        logger.debug("BaseXmlManager: {}", this.jf);
        try {
            this.df = DatatypeFactory.newInstance();
        } catch (DatatypeConfigurationException dce) {
            throw new XmlBindException("Unable to resolve DatatypeFactory : ", dce);
        }
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
    public final Object loadObject(final File inputFile)
            throws IOException, IllegalStateException, XmlBindException {

        Object result = null;
        try {

            final long start = System.nanoTime();

            result = createCustomUnMarshaller().unmarshal(inputFile);

            logger.info("unmarshall : duration = {} ms.", 1e-6d * (System.nanoTime() - start));

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
    public final Object loadObject(final Reader reader)
            throws IOException, IllegalStateException, XmlBindException {

        Object result = null;
        try {

            final long start = System.nanoTime();

            result = createCustomUnMarshaller().unmarshal(reader);

            logger.info("unmarshall : duration = {} ms.", 1e-6d * (System.nanoTime() - start));

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
    public final void saveObject(final File outputFile, final Object object)
            throws IOException, IllegalStateException {
        try {

            final long start = System.nanoTime();

            this.createMarshaller().marshal(object, outputFile);

            logger.info("marshall : duration = {} ms.", 1e-6d * (System.nanoTime() - start));

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
     * Creates a default JAXB Unmarshaller
     *
     * @return JAXB Unmarshaller
     * @throws XmlBindException if a JAXBException was caught while creating an unmarshaller
     */
    protected final Unmarshaller createUnMarshaller() throws XmlBindException {
        return this.jf.createUnMarshaller();
    }

    /**
     * Creates a custom JAXB Unmarshaller (can be overriden)
     *
     * @return JAXB Unmarshaller
     * @throws XmlBindException if a JAXBException was caught while creating an unmarshaller
     */
    protected Unmarshaller createCustomUnMarshaller() throws XmlBindException {
        return createUnMarshaller();
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

            if (logger.isWarnEnabled()) {
                logger.warn("JAXB Marshaller.setProperty() failure using marshaller class: {}", m.getClass().getName(), pe);
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
    protected static void handleException(final String message, final JAXBException je) throws IllegalStateException, IOException {
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
