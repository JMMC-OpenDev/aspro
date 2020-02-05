/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.rawobs.Observations;
import fr.jmmc.aspro.model.rawobs.RawObservation;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.jaxb.XmlBindException;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class manages simple IO operations (read/write) of Aspro Raw Observation Model documents
 * @author bourgesl
 */
public final class RawObsManager extends BaseXmlManager {

    private static final boolean DUMP = false;

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(RawObsManager.class.getName());
    /** package name for JAXB generated code */
    private final static String OB_JAXB_PATH = "fr.jmmc.aspro.model.rawobs";
    /** singleton pattern */
    private static final RawObsManager INSTANCE = new RawObsManager();
    /** time difference to consider exposures are within the same block */
    public static final double TIME_RANGE = 30.0 * 60.0; // 30 mins

    /* members */
    /** shared context */
    private final Map sharedContext = new HashMap(32);

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

            return rawObservations;
        }
        return null;
    }

    public List<Observations> analyze(final String name, final List<RawObservation> observations) {
        final int len = observations.size();
        logger.debug("analyze: Target '{}' {} observations", name, len);

        List<Observations> obsGroups = null;

        if (!observations.isEmpty()) {
            final ConfigurationManager cm = ConfigurationManager.getInstance();

            // 1. Analyze all individual observations:
            for (int i = 0; i < len; i++) {
                observations.get(i).prepare(logger, sharedContext, cm);
            }

            // 2. Group observations as Observation Blocks (30mins):
            int first = -1;
            for (int i = 0; i < len; i++) {
                final RawObservation rawObs = observations.get(i);
                // only valid:
                if (rawObs.isValid(RawObservation.INVALID_TIMES)) {
                    first = i;
                    break;
                }
            }

            logger.debug("first = {}", first);

            if (first != -1) {
                obsGroups = new ArrayList<Observations>();

                // Create initial group:
                int gid = 1;
                Observations group = new Observations();
                obsGroups.add(group);

                // last valid observation:
                RawObservation lastObs = observations.get(first);
                addObsInGroup(group, gid, lastObs);

                for (int i = first + 1; i < len; i++) {
                    final RawObservation rawObs = observations.get(i);

                    // only valid:
                    if (rawObs.isValid(RawObservation.INVALID_TIMES)) {
                        // check if time distance is within 30mins
                        final double elapsed = (rawObs.getMjdStart() - lastObs.getMjdEnd()) * 86400.0; // in seconds

                        logger.debug("elapsed [{}] = {}", i, elapsed);

                        boolean add = false;
                        if (elapsed < TIME_RANGE) {
                            // check if rawObs is compatible with lastObs:
                            if (rawObs.isCompatible(lastObs)) {
                                logger.debug("compatible [{}]", i);
                                add = true;
                            } else {
                                logger.debug("NOT compatible:\n{}\n{}", lastObs, rawObs);
                            }
                        } else {
                            logger.debug("NOT time:\n{}\n{}", lastObs, rawObs);
                        }
                        if (!add) {
                            gid++;
                            group = new Observations();
                            obsGroups.add(group);
                        }
                        addObsInGroup(group, gid, rawObs);
                        lastObs = rawObs;
                    }
                }

                if (DUMP) {
                    int i = 0;
                    for (Observations g : obsGroups) {
                        System.out.println("Group [" + i + "] ---");

                        for (RawObservation o : g.getObservations()) {
                            System.out.println("  " + o);
                        }
                        i++;
                    }
                }
            }
        }
        return obsGroups;
    }

    private void addObsInGroup(final Observations group, final int gid, final RawObservation obs) {
        obs.setGroupId(NumberUtils.valueOf(gid));
        group.getObservations().add(obs);
    }
}
