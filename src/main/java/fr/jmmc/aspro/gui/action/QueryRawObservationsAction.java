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
import java.util.List;
import org.apache.commons.httpclient.methods.PostMethod;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Query raw observations action
 * @author bourgesl
 */
public final class QueryRawObservationsAction extends RegisteredAction {

    // Use -DRemoteExecutionMode.local=true (dev) to use local obsportal instance (docker)
    private static final boolean USE_LOCAL = Boolean.getBoolean("QueryRawObservations.local");

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    public final static String className = QueryRawObservationsAction.class.getName();
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "queryRawObservations";
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(className);

    public static final String SERVER_URL = (USE_LOCAL)
            ? "http://localhost:6543/observation/search.votable"
            : "http://obs.jmmc.fr/observation/search.votable";

    /** XSLT file path */
    private final static String XSLT_FILE = "fr/jmmc/aspro/interop/Obsvot2RawObservations.xsl";

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public QueryRawObservationsAction() {
        super(className, actionName);
    }

    /**
     * Handle the action event
     * @param evt action event
     */
    @Override
    public void actionPerformed(final ActionEvent evt) {
        logger.debug("actionPerformed");

        // Use main observation to check instrument :
        final ObservationSetting observation = ObservationManager.getInstance().getMainObservation();

        // retrieve the selected target from its name:
        final Target target = observation.getTarget(observation.getSelectedTargetName());

        if (target != null) {
            // launch a new worker
            new QueryObsPortalWorker(target).executeTask();
        }
    }

    /**
     * This worker aims to query the obs portal in background.
     */
    private static final class QueryObsPortalWorker extends TaskSwingWorker<String> {

        /* members */
        /** selected target */
        private final Target target;

        /**
         * Hidden constructor
         *
         * @param observation observation
         * @param target target
         */
        private QueryObsPortalWorker(final Target target) {
            super(AsproTaskRegistry.TASK_QUERY_OBS);
            this.target = target;
        }

        /**
         * Send the obsportal query using HTTP in background
         * This code is executed by a Worker thread (Not Swing EDT)
         * @return String (Raw observations) document or null (if failure)
         */
        @Override
        public String computeInBackground() {

            final long start = System.nanoTime();

            // Query JMMC ObsPortal:
            String result = null;

            try {
                final URI uri = Http.validateURL(SERVER_URL);

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
                        method.addParameter("ra", Double.toString(target.getRADeg()));
                        method.addParameter("dec", Double.toString(target.getDECDeg()));

                        // add radius ?
                        // add instrument (name) ?
                    }
                });

            } catch (IOException ioe) {
                _logger.error("Query failed: {}", SERVER_URL, ioe);
            } finally {
                logger.info("Query[{}] {}: {} ms.", (result != null) ? "OK" : "FAILED", SERVER_URL,
                        1e-6d * (System.nanoTime() - start));
            }

            if (result != null) {
                logger.debug("Query result:\n{}", result);
                try {
                    final String rawObsDocument = processVOTable(result);

                    logger.debug("Transformed result:\n{}", rawObsDocument);

                    result = rawObsDocument;
                } catch (IllegalArgumentException iae) {
                    _logger.error("ProcessVOTable failed:\n{}", result, iae);
                } catch (IOException ioe) {
                    _logger.error("ProcessVOTable failed: {}", SERVER_URL, ioe);
                }
            }

            return result;
        }

        /**
         * Update the target observations with the given response.
         * This code is executed by the Swing Event Dispatcher thread (EDT)
         * @param rawObsDocument votable
         */
        @Override
        public void refreshUI(final String rawObsDocument) {
            logger.debug("refreshUI: Transformed result:\n{}", rawObsDocument);

            Observations rawObservations = null;
            try {
                // throws IllegalArgumentException if the document can not be loaded (invalid or unmarshalling exception):
                rawObservations = RawObsManager.getInstance().loadRawObservations(new StringReader(rawObsDocument));

            } catch (IllegalArgumentException iae) {
                // Report both Observation and VOTable in a new IllegalArgumentException to get them in the feedback report:
                throw new IllegalArgumentException("Invalid generated Aspro2 Raw Observations:\n\n" + rawObsDocument, iae);
            } catch (IOException ioe) {
                MessagePane.showErrorMessage("Could not load the file : ", ioe);
            }

            if (rawObservations != null) {
                final ObservationManager om = ObservationManager.getInstance();

                // Use main observation to check instrument :
                final ObservationSetting observation = om.getMainObservation();

                TargetRawObservation rawObs = observation.getTargetRawObservation(target);
                if (rawObs == null) {
                    rawObs = new TargetRawObservation();
                    rawObs.setTargetRef(target);
                    observation.getTargetObservations().add(rawObs);
                }
                final List<RawObservation> obsList = rawObs.getObservations();
                obsList.clear();
                obsList.addAll(rawObservations.getObservations());

                om.fireTargetChangedEvents();

                // display report message:
                MessagePane.showMessage("Raw Observations updated for target '" + target.getName() + "': " + obsList.size() + " records.");
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
