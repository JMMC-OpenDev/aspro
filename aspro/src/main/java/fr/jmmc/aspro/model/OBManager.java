/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.jmcs.util.jaxb.XmlBindException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class manages simple IO operations (read/write) of Aspro OB Model documents
 * @author bourgesl
 */
public final class OBManager extends BaseXmlManager {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(OBManager.class.getName());
    /** package name for JAXB generated code */
    private final static String OB_JAXB_PATH = "fr.jmmc.aspro.model.ob";
    /** singleton pattern */
    private static final OBManager INSTANCE = new OBManager();

    /**
     * Return the singleton
     * @return singleton
     */
    public static OBManager getInstance() {
        return INSTANCE;
    }

    /**
     * Private constructor
     *
     * @throws XmlBindException if a JAXBException was caught
     */
    private OBManager() throws XmlBindException {
        super(OB_JAXB_PATH);
    }

}
