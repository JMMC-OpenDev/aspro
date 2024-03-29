/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import edu.dartmouth.Const;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.rawobs.Observations;
import fr.jmmc.aspro.model.rawobs.RawObservation;
import fr.jmmc.jmal.ALX;
import fr.jmmc.jmal.CoordUtils;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.jaxb.XmlBindException;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.jafama.FastMath;
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

    public List<Observations> analyze(final Target targetRef, final List<RawObservation> observations) {
        final String targetId = targetRef.getIdentifier();
        int len = observations.size();
        logger.debug("analyze: {} has {} observations", targetId, len);

        List<Observations> obsGroups = null;

        if (!observations.isEmpty()) {
            final ConfigurationManager cm = ConfigurationManager.getInstance();

            // 1. Analyze all individual observations:
            for (int i = 0; i < len; i++) {
                final RawObservation rawObs = observations.get(i);
                rawObs.prepare(logger, sharedContext, cm);

                // compute to allow distance filter:
                computeTargetDistance(targetRef, rawObs);
            }

            if (DUMP) {
                // show distinct values:
                for (String column : RawObservation.DEDUP_COLS) {
                    Map<String, String> map = RawObservation.getUniqueMap(sharedContext, column);
                    logger.info("[{}]: {}", column, map);
                }
            }

            // 2. Group observations as Observation Blocks (30mins):
            // 2.1: get only valid observations:
            final List<RawObservation> validObservations = new ArrayList<RawObservation>();

            for (int i = 0; i < len; i++) {
                final RawObservation rawObs = observations.get(i);
                // only valid:
                if (rawObs.isValid(RawObservation.INVALID_TIMES)) {
                    validObservations.add(rawObs);
                }
            }

            logger.debug("valid observations = {}", validObservations);

            if (!validObservations.isEmpty()) {
                len = validObservations.size();
                obsGroups = new ArrayList<Observations>();

                final Map<Integer, Observations> obsGroupById = new HashMap<Integer, Observations>();

                // Create initial group:
                int gid = 1;
                Observations group = addGroup(obsGroupById, obsGroups, gid, targetId);

                // first valid observation:
                addObsInGroup(group, validObservations.get(0));

                for (int i = 1; i < len; i++) {
                    // only valid:
                    final RawObservation rawObs = validObservations.get(i);

                    Observations prevGroup = null;

                    for (int j = i - 1; j >= 0; j--) {
                        final RawObservation prevObs = validObservations.get(j);

                        // check if time distance is within 30mins
                        final double elapsed = (rawObs.getMjdStart() - prevObs.getMjdEnd()) * 86400.0; // in seconds
                        logger.debug("elapsed [{}] = {}", i, elapsed);

                        if (elapsed < TIME_RANGE) {
                            // check if rawObs is compatible with prevObs:
                            if (rawObs.isCompatible(prevObs)) {
                                logger.debug("compatible [{}]", i);
                                prevGroup = obsGroupById.get(prevObs.getGroupId());
                                break;
                            } else {
                                logger.debug("NOT compatible:\n{}\n{}", prevObs, rawObs);
                            }
                        } else {
                            logger.debug("NOT time:\n{}\n{}", prevObs, rawObs);
                            break;
                        }
                    }

                    if (prevGroup == null) {
                        group = addGroup(obsGroupById, obsGroups, ++gid, targetId);
                    } else {
                        group = prevGroup;
                    }
                    addObsInGroup(group, rawObs);
                }

                if (DUMP) {
                    for (Observations g : obsGroups) {
                        logger.info("Group [{}] ---", g.getGroupId());

                        for (RawObservation o : g.getObservations()) {
                            if (o != null) {
                                logger.info(o.toString());
                            }
                        }
                    }
                }
            }
        }
        return obsGroups;
    }

    /**
     * Check the distance between the given source target and the given raw observation's target
     * @param targetRef source target
     * @param rawObs raw observation
     */
    private static void computeTargetDistance(final Target targetRef, final RawObservation rawObs) throws IllegalArgumentException {
        double ra = targetRef.getRADeg();
        double dec = targetRef.getDECDeg();

        // TODO: pm correction is not able to correct position difference (pointing model or binary center of mass ?)
        // maybe it depends on instruments ? are coordinates corrected in eso archive (oifits headers)
        if (false) {
            final Double pmRa = targetRef.getPMRA();
            final Double pmDec = targetRef.getPMDEC();

            // see AstroSkyCalcObservation.defineTarget()
            if ((pmRa != null) && (pmDec != null)) {
                final double jd_start = rawObs.getMjdStart() + edu.dartmouth.AstroSkyCalc.MJD_REF;
                final double years = (jd_start - Const.J2000) * Const.DAY_IN_YEAR;

                logger.info("Target PM[RA/DEC]: {}, {} mas/yr - years = {}", pmRa, pmDec, years);

                // pmRA is given in RA*cos(DE) cf ASCC (Proper Motion in RA*cos(DE)):
                // RAJ2000_ep2000 "RAJ2000+(2000-1991.25)*pmRA/cos(DEJ2000*PI/180)/1000/3600"
                final double deltaRa = years * (pmRa / FastMath.cos(FastMath.toRadians(dec))) * 1e-3d * ALX.ARCSEC_IN_DEGREES;

                // DEJ2000_ep2000 "DEJ2000+(2000-1991.25)*pmDE/1000/3600"        
                final double deltaDec = years * pmDec * 1e-3d * ALX.ARCSEC_IN_DEGREES;

                logger.info("Target delta[RA/DEC]: {} {} arcsec", deltaRa * ALX.DEG_IN_ARCSEC, deltaDec * ALX.DEG_IN_ARCSEC);

                ra += deltaRa;
                dec += deltaDec;
            }
        }

        final double distance = CoordUtils.computeDistanceInDegrees(ra, dec, rawObs.getTargetRa(), rawObs.getTargetDec());

        if (logger.isDebugEnabled()) {
            logger.debug("computeTargetDistance: sep = {} arcsec for [{}]: [{} {} ({} {})]",
                    NumberUtils.trimTo3Digits(distance * ALX.DEG_IN_ARCSEC),
                    targetRef.getId(), rawObs.getObsId(), rawObs.getTargetName(), rawObs.getRa(), rawObs.getDec());
        }

        // set distance anyway:
        rawObs.setDist(distance * ALX.DEG_IN_ARCSEC);
    }

    private static Observations addGroup(final Map<Integer, Observations> obsGroupById,
                                         final List<Observations> obsGroups,
                                         final int gid, final String targetId) {
        final Observations group = new Observations();
        group.setGroupId(NumberUtils.valueOf(gid));
        group.setTargetId(targetId);

        obsGroups.add(group);
        obsGroupById.put(group.getGroupId(), group);
        return group;
    }

    private static void addObsInGroup(final Observations group, final RawObservation obs) {
        obs.setGroupId(group.getGroupId());
        group.getObservations().add(obs);
    }
}
