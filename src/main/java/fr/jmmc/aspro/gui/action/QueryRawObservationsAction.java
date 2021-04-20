/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.Aspro2;
import fr.jmmc.aspro.gui.task.AsproTaskRegistry;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.RawObsManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetRawObservation;
import fr.jmmc.aspro.model.rawobs.Observations;
import fr.jmmc.aspro.model.rawobs.RawObservation;
import fr.jmmc.jmcs.data.app.ApplicationDescription;
import fr.jmmc.jmcs.gui.action.RegisteredAction;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.gui.task.TaskSwingWorker;
import fr.jmmc.jmcs.gui.task.TaskSwingWorkerExecutor;
import fr.jmmc.jmcs.network.http.Http;
import fr.jmmc.jmcs.network.http.PostQueryProcessor;
import fr.jmmc.jmcs.service.XslTransform;
import fr.jmmc.jmcs.util.FileUtils;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.net.URI;
import java.net.UnknownHostException;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import org.apache.commons.httpclient.methods.PostMethod;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Query raw observations action
 * @author bourgesl
 */
public abstract class QueryRawObservationsAction extends RegisteredAction {

    /** flag -QueryRawObservations.local=true (dev) to use obsportal instance on localhost:6543 */
    private static final boolean USE_LOCAL = Boolean.getBoolean("QueryRawObservations.local");
    /** flag -QueryRawObservations.trace=true (dev) to log all http queries */
    private static final boolean DO_TRACE = Boolean.getBoolean("QueryRawObservations.trace");

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    public final static String className = QueryRawObservationsAction.class.getName();
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "queryRawObservations";
    /** Class logger */
    protected static final Logger logger = LoggerFactory.getLogger(QueryRawObservationsAction.class.getName());

    public static final String ESO_GET_PROG_URL = "http://archive.eso.org/wdb/wdb/eso/sched_rep_arc/query?progid=";

    public static final String OBS_SERVER;

    public static final String OBS_SERVER_GET_OBS_URL;

    public static final String OBS_SERVER_SEARCH_URL;

    /** XSLT file path */
    private final static String XSLT_FILE = "fr/jmmc/aspro/interop/Obsvot2RawObservations.xsl";

    static {
        OBS_SERVER = (USE_LOCAL) ? "http://localhost:6543/"
                : ((ApplicationDescription.isBetaVersion()) ? "http://obs-preprod.jmmc.fr/" : "http://obs.jmmc.fr/");

        OBS_SERVER_GET_OBS_URL = OBS_SERVER + "detail/exposure/";
        OBS_SERVER_SEARCH_URL = OBS_SERVER + "search.votable";

        logger.info("ObsPortal server url: {}", OBS_SERVER_SEARCH_URL);
    }

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    QueryRawObservationsAction(final String className, final String actionName) {
        super(className, actionName);
    }

    public static void cancelAnyTask() {
        TaskSwingWorkerExecutor.cancelTask(AsproTaskRegistry.TASK_QUERY_OBS);
    }

    /**
     * Handle the action event
     * @param evt action event
     */
    @Override
    public abstract void actionPerformed(final ActionEvent evt);

    /**
     * Process the given target list (asynchronously)
     * @param targets list of targets
     */
    public void process(final List<Target> targets) {
        logger.debug("actionPerformed");

        // Create progress panel:
        final JProgressBar progressBar = new JProgressBar();
        final JPanel progressPanel = Aspro2.createProgressPanel("querying observations ...",
                progressBar, new ActionListener() {

            @Override
            public void actionPerformed(final ActionEvent e) {
                cancelAnyTask();
            }
        });

        StatusBar.addCustomPanel(progressPanel);

        // launch a new worker
        new QueryObsPortalWorker(targets, new QueryObsListener() {

            @Override
            public void propertyChange(final PropertyChangeEvent pce) {
                if ("progress".equals(pce.getPropertyName())) {
                    progressBar.setValue((Integer) pce.getNewValue());
                }
            }

            @Override
            public void done(final boolean cancelled) {
                StatusBar.removeCustomPanel(progressPanel);
            }
        }).executeTask(true);

    }

    public interface QueryObsListener extends PropertyChangeListener {

        public void done(final boolean cancelled);
    }

    /**
     * This worker aims to query the obs portal in background.
     */
    static final class QueryObsPortalWorker extends TaskSwingWorker<Map<String, Observations>> {

        /* members */
        /** selected target */
        private final List<Target> targets;
        /** optional progress listener */
        private final QueryObsListener listener;

        /**
         * Hidden constructor
         *
         * @param targets target list
         */
        QueryObsPortalWorker(final List<Target> targets) {
            this(targets, null);
        }

        /**
         * Hidden constructor
         *
         * @param targets target list
         * @param listener optional listener
         */
        QueryObsPortalWorker(final List<Target> targets, final QueryObsListener listener) {
            super(AsproTaskRegistry.TASK_QUERY_OBS);
            this.targets = targets;
            this.listener = listener;
            if (listener != null) {
                this.addPropertyChangeListener(listener);
            }
        }

        /**
         * Send the obsportal query using HTTP in background
         * This code is executed by a Worker thread (Not Swing EDT)
         * @return String (Raw observations) document or null (if failure)
         */
        @Override
        public Map<String, Observations> computeInBackground() {
            final URI uri = Http.validateURL(OBS_SERVER_SEARCH_URL);

            final int total = targets.size();

            if (total > 1) {
                StatusBar.show("Querying observations from " + OBS_SERVER_SEARCH_URL + " ...");
            }

            final RawObsManager rom = RawObsManager.getInstance();

            Map<String, Observations> rawObsMap = null;

            final long start = System.nanoTime();

            int n = 0;

            for (final Target target : targets) {
                final int nb = ++n;
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
                            logger.info("Query({}/{})[{}]: ra={} dec={}", nb, total, OBS_SERVER_SEARCH_URL, ra, de);

                            method.addParameter("ra", ra);
                            method.addParameter("dec", de);

                            // add radius ?
                            method.addParameter("radius", "30.0");
                            // add instrument (name) ?
                            
                            // filter fields:
                            method.addParameter("all_fields", "false");
                            
                            if (DO_TRACE) {
                                logger.info("Query({}/{}):\nOBSPORTAL_SEARCH: {}?ra={}&dec={}&", nb, total, OBS_SERVER_SEARCH_URL, ra, de);
                            }
                        }
                    });
                } catch (UnknownHostException uhe) {
                    _logger.error("Query({}/{}) failed: {}", nb, total, OBS_SERVER_SEARCH_URL, uhe);
                    StatusBar.show("Query failed: " + OBS_SERVER_SEARCH_URL);
                    break;
                } catch (IOException ioe) {
                    _logger.error("Query({}/{}) failed: {}", nb, total, OBS_SERVER_SEARCH_URL, ioe);
                    StatusBar.show("Query failed: " + OBS_SERVER_SEARCH_URL);
                    break;
                } finally {
                    logger.info("Query({}/{})[{}] {}: {} ms.", nb, total, (result != null) ? "OK" : "FAILED", OBS_SERVER_SEARCH_URL,
                            1e-6d * (System.nanoTime() - startHttp));
                }

                // fast interrupt :
                if (Thread.currentThread().isInterrupted()) {
                    StatusBar.show("Query observations: cancelled.");
                    return null;
                }

                if (result != null) {
                    logger.debug("Query({}/{}) result:\n{}", nb, total, result);

                    String rawObsDocument = null;
                    try {
                        rawObsDocument = processVOTable(result);

                    } catch (IllegalArgumentException iae) {
                        _logger.error("ProcessVOTable failed:\n{}", result, iae);
                    } catch (IOException ioe) {
                        _logger.error("ProcessVOTable failed: {}", ioe);
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
                                rawObsMap = new LinkedHashMap<String, Observations>();
                            }
                            rawObsMap.put(targetId, rawObservations);

                            if (total > 1) {
                                StatusBar.show("Query[" + nb + " / " + total + "]: "
                                        + rawObservations.getObservations().size() + " observations"
                                        + " for target '" + target.getName() + "'");
                            }
                        }
                    }
                }
                // publish progress:
                setProgress(Math.round((100f * nb) / total));
            }
            logger.info("Query[{} targets]: {} ms.", total, 1e-6d * (System.nanoTime() - start));

            return rawObsMap;
        }

        /**
         * Update the target observations with the given response.
         * This code is executed by the Swing Event Dispatcher thread (EDT)
         * @param rawObsMap map<target id, rawObsDocument>
         */
        @Override
        public void refreshUI(final Map<String, Observations> rawObsMap) {
            if (listener != null) {
                listener.done(false);
            }

            logger.debug("refreshUI: Transformed results:\n{}", rawObsMap);

            if (rawObsMap == null) {
                StatusBar.show("Get observations returned no results (check your network).");
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

        @Override
        public void refreshNoData(final boolean cancelled) {
            if (listener != null) {
                listener.done(cancelled);
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

            // May be long (async // parallel task ?)
            final String document = XslTransform.transform(votable, XSLT_FILE);

            logger.info("VOTable transformation (XSLT): {} ms.", 1e-6d * (System.nanoTime() - start));

            return document;
        }
    }

    public static void main(String[] args) throws IOException {
        final String result = FileUtils.readFile(new File("/home/bourgesl/dev/aspro/src/test/resources/obsportal/big.vot"));

        final int N = 100;

        for (int i = 0; i < N; i++) {
            final String doc = QueryObsPortalWorker.processVOTable(result);

            if (false) {
                logger.info("doc: {}", doc);
            }
        }
    }
}
