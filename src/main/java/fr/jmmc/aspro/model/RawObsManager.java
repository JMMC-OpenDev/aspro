/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.rawobs.Observations;
import fr.jmmc.jmcs.util.jaxb.XmlBindException;
import java.io.IOException;
import java.io.Reader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class manages simple IO operations (read/write) of Aspro Raw Observation Model documents
 * @author bourgesl
 */
public final class RawObsManager extends BaseXmlManager {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(RawObsManager.class.getName());
    /** package name for JAXB generated code */
    private final static String OB_JAXB_PATH = "fr.jmmc.aspro.model.rawobs";
    /** singleton pattern */
    private static final RawObsManager INSTANCE = new RawObsManager();

    /**
     * Return the singleton
     * @return singleton
     */
    public static RawObsManager getInstance() {
        return INSTANCE;
    }

    /**
     * Private constructor
     *
     * @throws XmlBindException if a JAXBException was caught
     */
    private RawObsManager() throws XmlBindException {
        super(OB_JAXB_PATH);
    }

    /**
     * Load a raw observation list from the given reader
     * Used by QueryRawObservationsAction
     *
     * @param reader any reader
     * @return loaded raw observation list
     *
     * @throws IOException if an I/O exception occured
     * @throws IllegalArgumentException if the file is not an Observation
     */
    public Observations loadRawObservations(final Reader reader) throws IOException, IllegalArgumentException {
        if (reader != null) {
            logger.debug("Load raw observation list from stream: ", reader);

            final Object loaded = loadObject(reader);

            if (!(loaded instanceof Observations)) {
                throw new IllegalArgumentException("The loaded document does not correspond to a valid Aspro2 file");
            }

            final Observations rawObservations = (Observations) loaded;

            // post load processing (ignore information message):
//            ObservationFileProcessor.onLoad(observations);
            return rawObservations;
        }
        return null;
    }
}
