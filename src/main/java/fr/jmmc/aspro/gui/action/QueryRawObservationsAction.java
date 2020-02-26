/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.gui.task.AsproTaskRegistry;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.RawObsManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetRawObservation;
import fr.jmmc.aspro.model.rawobs.Observations;
import fr.jmmc.aspro.model.rawobs.RawObservation;
import fr.jmmc.jmcs.gui.action.RegisteredAction;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.gui.task.TaskSwingWorker;
import fr.jmmc.jmcs.network.http.Http;
import fr.jmmc.jmcs.network.http.PostQueryProcessor;
import fr.jmmc.jmcs.service.XslTransform;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.io.StringReader;
import java.net.URI;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.httpclient.methods.PostMethod;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Query raw observations action
 * @author bourgesl
 */
public abstract class QueryRawObservationsAction extends RegisteredAction {

    // Use -DRemoteExecutionMode.local=true (dev) to use local obsportal instance (docker)
    private static final boolean USE_LOCAL = Boolean.getBoolean("QueryRawObservations.local");

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    public final static String className = QueryRawObservationsAction.class.getName();
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "queryRawObservations";
    /** Class logger */
    protected static final Logger logger = LoggerFactory.getLogger(QueryRawObservationsAction.class.getName());

    public static final String ESO_GET_PROG_URL = "http://archive.eso.org/wdb/wdb/eso/sched_rep_arc/query?progid=";

    public static final String OBS_SERVER = (USE_LOCAL)
            ? "http://localhost:6543/"
            : "http://obs.jmmc.fr/";

    public static final String OBS_SERVER_GET_OBS_URL = OBS_SERVER + "detail/exposure/";

    public static final String OBS_SERVER_SEARCH_URL = OBS_SERVER + "search.votable";

    /** XSLT file path */
    private final static String XSLT_FILE = "fr/jmmc/aspro/interop/Obsvot2RawObservations.xsl";

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    QueryRawObservationsAction(final String className, final String actionName) {
        super(className, actionName);
    }

    /**
     * Handle the action event
     * @param evt action event
     */
    @Override
    public abstract void actionPerformed(final ActionEvent evt);

    /**
     * This worker aims to query the obs portal in background.
     */
    static final class QueryObsPortalWorker extends TaskSwingWorker<Map<String, Observations>> {

        /* members */
        /** selected target */
        private final List<Target> targets;

        /**
         * Hidden constructor
         *
         * @param observation observation
         * @param target target
         */
        QueryObsPortalWorker(final List<Target> targets) {
            super(AsproTaskRegistry.TASK_QUERY_OBS);
            this.targets = targets;
        }

        /**
         * Send the obsportal query using HTTP in background
         * This code is executed by a Worker thread (Not Swing EDT)
         * @return String (Raw observations) document or null (if failure)
         */
        @Override
        public Map<String, Observations> computeInBackground() {
            final URI uri = Http.validateURL(OBS_SERVER_SEARCH_URL);

            final RawObsManager rom = RawObsManager.getInstance();

            Map<String, Observations> rawObsMap = null;

            final long start = System.nanoTime();

            for (final Target target : targets) {
                final String targetId = target.getIdentifier();

                final long startHttp = System.nanoTime();

                // Query JMMC ObsPortal:
                String result = null;

                try {
                    // use the multi threaded HTTP client
                    result = Http.post(uri, false, new PostQueryProcessor() {
                        /**
                         * Process the given post method to define its HTTP input fields
                         *
                         * @param method post method to complete
                         * @throws IOException if any IO error occurs
                         */
                        @Override
                        public void process(final PostMethod method) throws IOException {
                            final String ra = Double.toString(target.getRADeg());
                            final String de = Double.toString(target.getDECDeg());
                            logger.info("Query[{}]: ra={} dec={}", OBS_SERVER_SEARCH_URL, ra, de);

                            method.addParameter("ra", ra);
                            method.addParameter("dec", de);

                            // add radius ?
                            // add instrument (name) ?
                        }
                    });
                } catch (UnknownHostException uhe) {
                    _logger.error("Query failed: {}", OBS_SERVER_SEARCH_URL, uhe);
                    StatusBar.show("Query failed: " + OBS_SERVER_SEARCH_URL);
                    break;
                } catch (IOException ioe) {
                    _logger.error("Query failed: {}", OBS_SERVER_SEARCH_URL, ioe);
                    StatusBar.show("Query failed: " + OBS_SERVER_SEARCH_URL);
                    break;
                } finally {
                    logger.info("Query[{}] {}: {} ms.", (result != null) ? "OK" : "FAILED", OBS_SERVER_SEARCH_URL,
                            1e-6d * (System.nanoTime() - startHttp));
                }

                if (result != null) {
                    logger.debug("Query result:\n{}", result);

                    String rawObsDocument = null;
                    try {
                        rawObsDocument = processVOTable(result);

                    } catch (IllegalArgumentException iae) {
                        _logger.error("ProcessVOTable failed:\n{}", result, iae);
                    } catch (IOException ioe) {
                        _logger.error("ProcessVOTable failed: {}", OBS_SERVER_SEARCH_URL, ioe);
                    }

                    if (rawObsDocument != null) {
                        // Load RawObservations
                        Observations rawObservations = null;
                        try {
                            logger.debug("Transformed result:\n{}", rawObsDocument);

                            // throws IllegalArgumentException if the document can not be loaded (invalid or unmarshalling exception):
                            rawObservations = rom.loadRawObservations(new StringReader(rawObsDocument));

                        } catch (IllegalArgumentException iae) {
                            // Report both Observation and VOTable in a new IllegalArgumentException to get them in the feedback report:
                            throw new IllegalArgumentException("Invalid generated Aspro2 Raw Observations:\n" + rawObsDocument, iae);
                        } catch (IOException ioe) {
                            throw new IllegalArgumentException("Could not load the document:\n" + rawObsDocument, ioe);
                        }
                        if (rawObservations != null) {
                            logger.debug("Observations for target [{}]:\n{}", targetId, rawObservations);

                            if (rawObsMap == null) {
                                rawObsMap = new HashMap<String, Observations>();
                            }
                            rawObsMap.put(targetId, rawObservations);
                        }
                    }
                }
            }
            logger.info("Query[{} targets]: {} ms.", targets.size(), 1e-6d * (System.nanoTime() - start));

            return rawObsMap;
        }

        /**
         * Update the target observations with the given response.
         * This code is executed by the Swing Event Dispatcher thread (EDT)
         * @param rawObsMap map<target id, rawObsDocument>
         */
        @Override
        public void refreshUI(final Map<String, Observations> rawObsMap) {
            logger.debug("refreshUI: Transformed results:\n{}", rawObsMap);

            if (rawObsMap == null) {
                StatusBar.show("Query raw Observations returned no results (check your network).");
            } else {
                final long start = System.nanoTime();

                final ObservationManager om = ObservationManager.getInstance();

                // Use main observation to check instrument :
                final ObservationSetting observation = om.getMainObservation();

                final StringBuilder report = new StringBuilder(1024);
                report.append("Raw Observations updated for targets:\n");

                int modCount = 0;

                for (Map.Entry<String, Observations> e : rawObsMap.entrySet()) {
                    final String targetId = e.getKey();
                    final Observations rawObservations = e.getValue();

                    final Target target = observation.getTargetById(targetId);

                    if (target != null) {
                        // Possibly: target was removed meanwhile
                        final List<RawObservation> newObservations = rawObservations.getObservations();

                        final TargetRawObservation rawObs = observation.getOrCreateTargetRawObservation(target);
                        // TODO: update timestamp in rawObs

                        // Replace or Merge subset (use last_mod_date to do incremental updates ?)
                        // note: atomicity OK (no need clone ?)
                        rawObs.setObservations(newObservations);
                        modCount++;

                        report.append(target.getName()).append(" : ").append(newObservations.size()).append(" records.\n");
                    }
                }
                logger.info("refreshUI[{} targets updated]: {} ms.", modCount, 1e-6d * (System.nanoTime() - start));

                if (modCount != 0) {
                    om.fireTargetObservationsChangedEvents();

                    // display report message:
                    MessagePane.showMessage(report.toString());
                }
            }
        }

        /**
         * Process the given votable
         *
         * @param votable votable to process
         * @return Aspro 2 Raw Observation list (as string)
         * 
         * @throws IOException if an I/O exception occured
         * @throws IllegalArgumentException if the file is not an Observation
         */
        public static String processVOTable(final String votable) throws IOException {
            // use an XSLT to transform the votable document to an Aspro 2 Raw Observation list:
            final long start = System.nanoTime();

            final String document = XslTransform.transform(votable, XSLT_FILE);

            logger.info("VOTable transformation (XSLT): {} ms.", 1e-6d * (System.nanoTime() - start));

            return document;
        }

    }
}
