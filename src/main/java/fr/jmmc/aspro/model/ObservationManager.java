/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.model.event.OIFitsEvent;
import fr.jmmc.aspro.model.event.ObservabilityEvent;
import fr.jmmc.aspro.model.event.ObservationEvent;
import fr.jmmc.aspro.model.event.ObservationEventType;
import fr.jmmc.aspro.model.event.ObservationListener;
import fr.jmmc.aspro.model.event.TargetSelectionEvent;
import fr.jmmc.aspro.model.event.UpdateObservationEvent;
import fr.jmmc.aspro.model.event.WarningContainerEvent;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.oi.AtmosphereQuality;
import fr.jmmc.aspro.model.oi.BaseValue;
import fr.jmmc.aspro.model.oi.FocalInstrumentConfiguration;
import fr.jmmc.aspro.model.oi.FocalInstrumentConfigurationChoice;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.InterferometerConfigurationChoice;
import fr.jmmc.aspro.model.oi.ObservationCollection;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.ObservationVariant;
import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.model.oi.UserModel;
import fr.jmmc.aspro.model.oi.WhenSetting;
import fr.jmmc.aspro.model.util.SpectralBandUtils;
import fr.jmmc.aspro.model.util.TargetUtils;
import fr.jmmc.aspro.service.UserModelService;
import fr.jmmc.jmal.Band;
import fr.jmmc.jmal.model.ModelDefinition;
import fr.jmmc.jmal.model.targetmodel.Model;
import fr.jmmc.jmal.model.targetmodel.Parameter;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.service.RecentFilesManager;
import fr.jmmc.jmcs.util.CollectionUtils;
import fr.jmmc.jmcs.util.FileUtils;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.nom.tam.fits.FitsException;
import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.io.StringWriter;
import java.util.Date;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import java.util.Vector;
import java.util.concurrent.CopyOnWriteArrayList;
import javax.xml.datatype.XMLGregorianCalendar;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class manages observation files i.e. user defined observation settings
 *
 * @author bourgesl
 */
public final class ObservationManager extends BaseOIManager implements Observer {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(ObservationManager.class.getName());
    /** flag to log a stack trace in method fireEvent() to debug events */
    private final static boolean DEBUG_FIRE_EVENT = false;
    /** configuration manager */
    private static final ConfigurationManager cm = ConfigurationManager.getInstance();
    /** singleton pattern */
    private static final ObservationManager instance = new ObservationManager();
    /* members */
    /** preference singleton */
    private final Preferences myPreferences = Preferences.getInstance();
    /** observation listeners */
    private final CopyOnWriteArrayList<ObservationListener> listeners = new CopyOnWriteArrayList<ObservationListener>();
    /** initial observation settings (loaded or saved state) */
    private ObservationSetting initialObservation = null;
    /** main observation settings */
    private ObservationSetting mainObservation = null;
    /** associated file to the observation settings */
    private File observationFile = null;
    /** derived observation collection used by computations */
    private ObservationCollection obsCollection = null;
    /** computed OIFits data */
    private OIFitsData oiFitsData = null;
    /** (cached) flag to use fast user model (preference) */
    private boolean useFastUserModel;

    /**
     * Return the ObservationManager singleton
     * @return ObservationManager singleton
     */
    public static ObservationManager getInstance() {
        return instance;
    }

    /**
     * Private constructor
     */
    private ObservationManager() {
        super();

        // copy fast user model preference:
        this.useFastUserModel = this.myPreferences.isFastUserModel();

        this.myPreferences.addObserver(this);
    }

    /**
     * Listen to preferences changes
     * @param o Preferences
     * @param arg unused
     */
    @Override
    public void update(final Observable o, final Object arg) {
        logger.debug("Preferences updated on : {}", this);

        final boolean newFastUserModel = this.myPreferences.isFastUserModel();
        if (this.useFastUserModel != newFastUserModel) {
            this.useFastUserModel = newFastUserModel;

            logger.debug("ObservationManager.update: checkAndLoadFileReferences ...");

            // reload user models (to prepare and validate again):
            final StringBuilder sb = new StringBuilder(128);
            checkAndLoadFileReferences(null, getMainObservation(), sb);

            if (sb.length() > 0) {
                MessagePane.showMessage(sb.toString());
            }
            logger.debug("ObservationManager.update: done.");

            // fire change events :
            this.fireTargetChangedEvents();
        }
    }

    /**
     * Return the main observation
     * @return main observation
     */
    public ObservationSetting getMainObservation() {
        return this.mainObservation;
    }

    /**
     * Private : define the main observation
     * @param observation new main observation to set
     */
    private void setMainObservation(final ObservationSetting observation) {
        this.initialObservation = null;
        this.mainObservation = observation;
    }

    /**
     * Private : define the initial observation as the current main observation (deep clone)
     */
    private void defineInitialObservation() {
        this.initialObservation = this.mainObservation.deepClone();
        // check and update target references :
        this.initialObservation.checkReferences();
    }

    /**
     * @return true if the main observation was modified since its initial state 
     */
    public boolean isMainObservationChanged() {
        // check and update target references (removes null and empty collections):
        this.mainObservation.checkReferences();

        // perform the complete graph comparison:
        return !OIBase.areEquals(this.mainObservation, this.initialObservation);
    }

    /**
     * Update the observation collection used by computations from the main observation :
     * - clone version
     * - use variants to generate the collection of observations with the correct instrument configuration (stations)
     */
    private void synchronizeObservations() {
        final ObservationSetting observation = getMainObservation();
        if (logger.isDebugEnabled()) {
            logger.debug("synchronizeObservations: {}", toString(observation));
        }

        // create a new observation collection :
        final ObservationCollection newObsCollection = new ObservationCollection();
        newObsCollection.setName("internal");
        newObsCollection.setVersion(new ObservationVersion(observation.getVersion()));

        ObservationSetting newObservation;
        FocalInstrumentConfigurationChoice instrumentChoice;

        // Process variants to generate correct observations (clones) :
        for (ObservationVariant obsVariant : observation.getVariants()) {

            // clone observation to ensure consistency (Swing can modify observation while the worker thread is running) :
            newObservation = (ObservationSetting) observation.clone();

            // apply changes coming from each variant :
            instrumentChoice = newObservation.getInstrumentConfiguration();

            // update the instrument configuration :
            instrumentChoice.setStations(obsVariant.getStations());
            instrumentChoice.setStationList(obsVariant.getStationList());

            // add observation to the new observation collection :
            newObsCollection.getObservations().add(newObservation);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("synchronizeObservations : obsCollection: {}", toString(newObsCollection));
        }

        setObservationCollection(newObsCollection);
    }

    /**
     * Return the observation collection used by computations
     *
     * @return observation collection used by computations
     */
    public ObservationCollection getObservationCollection() {
        return this.obsCollection;
    }

    /**
     * Private : define the observation collection used by computations
     * @param obsCollection new observation collection used by computations
     */
    private void setObservationCollection(final ObservationCollection obsCollection) {
        this.obsCollection = obsCollection;
    }

    /**
     * Return the current observation file
     * @return the current observation file or null if undefined
     */
    public File getObservationFile() {
        return this.observationFile;
    }

    /**
     * Private : define the current observation file
     * @param file new observation file to use
     */
    private void setObservationFile(final File file) {
        this.observationFile = file;

        // Now that a file has been loaded or saved:
        RecentFilesManager.addFile(file);
    }

    // --- MAIN FUNCTIONS --------------------------------------------------------
    /**
     * Reset the current observation
     *
     * @throws IllegalStateException if an invalid reference was found (interferometer / instrument / instrument configuration)
     */
    public void reset() throws IllegalStateException {
        logger.info("Reset observation");

        final ObservationSetting observation = new ObservationSetting();

        resetAndChangeObservation(observation);
    }

    /**
     * Load an observation from the given file
     * @param file file to load
     * @return optional information message or null
     *
     * @throws IOException if an I/O exception occured
     * @throws IllegalStateException if an invalid reference was found (interferometer / instrument / instrument configuration) or an unexpected exception occured
     * @throws IllegalArgumentException if the file is not an Observation
     */
    public String load(final File file) throws IOException, IllegalStateException, IllegalArgumentException {
        final StringBuilder sb = new StringBuilder(128);

        final ObservationSetting observation = loadObservation(file, sb);

        if (observation != null) {
            setObservationFile(file);

            // ready to use :
            changeObservation(observation);
        }
        return (sb.length() > 0) ? sb.toString() : null;
    }

    /**
     * Load only the observation from the given file
     * @param file file to load
     * @param sb buffer to append optional information messages (user model)
     * @return loaded observation or null
     *
     * @throws IOException if an I/O exception occured
     * @throws IllegalStateException if an invalid reference was found (interferometer / instrument / instrument configuration) or an unexpected exception occured
     * @throws IllegalArgumentException if the file is not an Observation
     */
    public ObservationSetting loadObservation(final File file, final StringBuilder sb) throws IOException, IllegalStateException, IllegalArgumentException {
        ObservationSetting observation = null;
        if (file != null) {
            logger.info("Load observation from file: {}", file);

            final Object loaded = loadObject(file);

            if (!(loaded instanceof ObservationSetting)) {
                throw new IllegalArgumentException("The loaded file does not correspond to a valid Aspro2 file: " + file);
            }

            observation = (ObservationSetting) loaded;

            // post load processing :
            ObservationFileProcessor.onLoad(observation);

            // update defaults and resolve references:
            defineDefaults(observation);

            // load user models (to prepare and validate):
            checkAndLoadFileReferences(file, observation, sb);

        }
        return observation;
    }

    /**
     * Load an observation from the given reader
     * Used by SearchCalSampMessageHandler
     *
     * @param reader any reader
     * @return loaded observation
     *
     * @throws IOException if an I/O exception occured
     * @throws IllegalArgumentException if the file is not an Observation
     */
    public ObservationSetting load(final Reader reader) throws IOException, IllegalArgumentException {
        if (reader != null) {
            logger.debug("Load observation from stream: ", reader);

            final Object loaded = loadObject(reader);

            if (!(loaded instanceof ObservationSetting)) {
                throw new IllegalArgumentException("The loaded document does not correspond to a valid Aspro2 file");
            }

            final ObservationSetting observation = (ObservationSetting) loaded;

            // post load processing (ignore information message):
            ObservationFileProcessor.onLoad(observation);

            return observation;
        }
        return null;
    }

    /**
     * Change the current observation with the given one
     * and fire load and change events
     * @param observation observation to use
     *
     * @throws IllegalStateException if an invalid reference was found (interferometer / instrument / instrument configuration)
     */
    public void resetAndChangeObservation(final ObservationSetting observation) throws IllegalStateException {
        defineDefaults(observation);

        setObservationFile(null);

        changeObservation(observation);
    }

    /**
     * Change the current observation with the given one
     * and fire load and change events
     * @param observation observation to use
     */
    private void changeObservation(final ObservationSetting observation) {
        // change the current observation :
        setMainObservation(observation);

        // fire an observation load event :
        this.fireObservationLoaded();

        // fire change events :
        this.fireTargetChangedEvents();

        // finally: set the initial state of the main observation (after GUI updates => potentially modified) 
        this.defineInitialObservation();
    }

    /**
     * Save the current observation in the given file
     * @param file file to save
     *
     * @throws IOException if an I/O exception occured
     * @throws IllegalStateException if an unexpected exception occured
     */
    public void saveObservation(final File file) throws IOException, IllegalStateException {
        if (file != null) {
            logger.info("Save observation to file: {}", file);

            final ObservationSetting observation = getMainObservation();

            // pre save processing :
            ObservationFileProcessor.onSave(observation);

            saveObject(file, observation);
        }
    }

    /**
     * Save the current observation in the given file
     * @param file file to save
     *
     * @throws IOException if an I/O exception occured
     * @throws IllegalStateException if an unexpected exception occured
     */
    public void save(final File file) throws IOException, IllegalStateException {
        if (file != null) {
            saveObservation(file);

            // finally: set the initial state of the main observation (as saved) 
            this.defineInitialObservation();

            setObservationFile(file);
        }
    }

    /**
     * Save the current observation into a String (xml)
     * @return observation setting serialized into a String
     * @throws IllegalStateException if an unexpected exception occured
     */
    public String saveToString() throws IllegalStateException {
        // Create a 16K buffer for the complete observation setting :
        final StringWriter sw = new StringWriter(16384);

        final ObservationSetting observation = getMainObservation();

        // pre save processing :
        ObservationFileProcessor.onSave(observation);

        // serialize observation to xml :
        saveObject(sw, observation);

        return sw.toString();
    }

    // --- EVENTS ----------------------------------------------------------------
    /**
     * Register the given observation listener
     * @param listener observation listener
     */
    public void register(final ObservationListener listener) {
        this.listeners.addIfAbsent(listener);
    }

    /**
     * Unregister the given observation listener
     * @param listener observation listener
     */
    public void unregister(final ObservationListener listener) {
        this.listeners.remove(listener);
    }

    /**
     * This fires an observation load event to all registered listeners.
     * Fired by changeObservation() when an observation is loaded or reset
     *
     * Listeners : SettingPanel / BasicObservationForm / ObservabilityPanel / UVCoveragePanel
     */
    private void fireObservationLoaded() {
        if (logger.isDebugEnabled()) {
            logger.debug("fireObservationLoaded: {}", toString(getMainObservation()));
        }

        fireEvent(new ObservationEvent(ObservationEventType.LOADED, getMainObservation()));
    }

    /**
     * This fires an observation target change event to all registered listeners.
     * Fired by fireTargetChangedEvents() when an observation is loaded or reset or the target list was modified
     *
     * Listeners : SettingPanel / BasicObservationForm
     */
    private void fireObservationTargetsChanged() {
        final ObservationSetting observation = getMainObservation();
        final ObservationVersion version = observation.getVersion();

        // Increment target and main versions :
        version.incTargetVersion();
        version.incMainVersion();

        if (logger.isDebugEnabled()) {
            logger.debug("fireObservationTargetsChanged: {}", toString(observation));
            logger.debug("observation version: {}", observation.getVersion());
        }

        fireEvent(new ObservationEvent(ObservationEventType.TARGET_CHANGED, observation));
    }

    /**
     * This fires a target selection changed event to all registered listeners.
     * Fired by [BasicObservationForm].fireTargetSelectionChangeEvent()
     * when the selected target changed
     *
     * Listeners : UVCoveragePanel
     *
     * @param target selected target (may be null)
     */
    public void fireTargetSelectionChanged(final Target target) {
        logger.debug("fireTargetSelectionChange: {}", target);

        final String targetName = (target != null) ? target.getName() : null;

        // store the selected target using its name (not instance):
        getMainObservation().setSelectedTargetName(targetName);

        fireEvent(new TargetSelectionEvent(target));
    }

    /**
     * This fires a target selection changed event to the given listener.
     *
     * Listeners : UVCoveragePanel
     *
     * @param listener which listener to call
     */
    public void fireTargetSelectionChanged(final ObservationListener listener) {
        final ObservationSetting observation = getMainObservation();

        // retrieve the selected target from its name:
        final Target target = observation.getTarget(observation.getSelectedTargetName());

        fireEvent(new TargetSelectionEvent(target), listener);
    }

    /**
     * This fires an observation update event to all registered listeners.
     * Fired by [BasicObservationForm|UVCoveragePanel].fireObservationUpdateEvent()
     * when a Swing component changed
     *
     * Listeners : BasicObservationForm / UVCoveragePanel
     */
    public void fireObservationUpdate() {
        fireObservationUpdate(false);
    }

    /**
     * This fires an observation update event to all registered listeners
     * and then fires an observation refresh event (MAIN or UV) if anything changed.
     * Fired by [BasicObservationForm|UVCoveragePanel].fireObservationUpdateEvent()
     * when a Swing component changed
     *
     * Listeners : BasicObservationForm / UVCoveragePanel
     *
     * @param forceRefresh flag to force an observation refresh event (MAIN)
     */
    private void fireObservationUpdate(final boolean forceRefresh) {
        final ObservationSetting observation = getMainObservation();
        if (logger.isDebugEnabled()) {
            logger.debug("fireObservationUpdate: {}", toString(observation));
        }

        final UpdateObservationEvent event = new UpdateObservationEvent(observation);

        fireEvent(event);

        if (forceRefresh) {
            if (logger.isDebugEnabled()) {
                logger.debug("fireObservationUpdate : FORCE REFRESH - changed: {}", event.getChanged());
            }
            event.setChanged(UpdateObservationEvent.ChangeType.MAIN);
        } else if (logger.isDebugEnabled()) {
            logger.debug("fireObservationUpdate : changed: {}", event.getChanged());
        }

        // Handle event result :
        if (event.getChanged() != UpdateObservationEvent.ChangeType.NONE) {

            final ObservationVersion version = observation.getVersion();

            // Update versions first :
            switch (event.getChanged()) {
                case MAIN:
                    // Increment observation main and UV versions :
                    version.incMainVersion();
                    version.incUVVersion();
                    break;
                case UV:
                    version.incUVVersion();
                    break;
                default:
                    break;
            }

            logger.debug("observation version = {}", version);

            // then synchronize the main observation with the observation collection used by computations :
            synchronizeObservations();

            // finally fire refresh event :
            switch (event.getChanged()) {
                case MAIN:
                    fireObservationRefresh();
                    break;
                case UV:
                    fireObservationRefreshUV();
                    break;
                default:
                    break;
            }
        }
    }

    /**
     * This fires an observation refresh event (MAIN) to all registered listeners.
     * Fired by fireObservationUpdate() <= [BasicObservationForm|UVCoveragePanel].fireObservationUpdateEvent()
     * when the observation changed (or force refresh)
     *
     * Listeners : BasicObservationForm / InterferometerMapPanel / ObservabilityPanel / OIFitsPanel
     */
    private void fireObservationRefresh() {
        // use the observation collection used by computations :
        if (logger.isDebugEnabled()) {
            logger.debug("fireObservationRefresh: {}", toString(getObservationCollection()));
        }

        fireEvent(new ObservationEvent(ObservationEventType.REFRESH, getObservationCollection()));
    }

    /**
     * This fires an observation refresh event (UV) to all registered listeners.
     * Fired by fireObservationUpdate() <= [BasicObservationForm|UVCoveragePanel].fireObservationUpdateEvent()
     * when the observation changed only UV related information
     *
     * Listeners : BasicObservationForm / UVCoveragePanel
     */
    private void fireObservationRefreshUV() {
        // use the observation collection used by computations :
        if (logger.isDebugEnabled()) {
            logger.debug("fireObservationRefreshUV: {}", toString(getObservationCollection()));
        }

        fireEvent(new ObservationEvent(ObservationEventType.REFRESH_UV, getObservationCollection()));
    }

    /**
     * This fires an observability done event to all registered listeners.
     * Fired by setObservabilityData() <= ObservabilityPanel.ObservabilitySwingWorker.refreshUI() (EDT) when the observability is computed
     *
     * Listeners : UVCoveragePanel
     * @param obsCollection observation collection used to compute observability data (consistency)
     * @param obsDataList computed observability data
     */
    public void fireObservabilityDone(final ObservationCollection obsCollection, final List<ObservabilityData> obsDataList) {
        // use the given observation collection used by computations :
        if (logger.isDebugEnabled()) {
            logger.debug("fireObservabilityDone: {}", toString(obsCollection));
        }

        fireEvent(new ObservabilityEvent(obsCollection, obsDataList));
    }

    /**
     * This fires a warnings ready event to all registered listeners.
     * Fired by UVCoveragePanel.UVCoverageSwingWorker.refreshUI() (EDT) when warnings are defined
     *
     * Listeners : BasicObservationForm
     *
     * @param warningContainer warning container
     */
    public void fireWarningsReady(final WarningContainer warningContainer) {
        logger.debug("fireWarningsReady: {}", warningContainer);

        fireEvent(new WarningContainerEvent(warningContainer));
    }

    /**
     * This fires an OIFits done event to all registered listeners.
     * Fired by setOIFitsFiles() <= UVCoveragePanel.UVCoverageSwingWorker.refreshUI() (EDT) when the OIFits is computed
     *
     * Listeners : SettingPanel / OIFitsPanel
     *
     * @param oiFitsData OIFits structures
     */
    public void fireOIFitsDone(final OIFitsData oiFitsData) {
        // use observation for computations :
        logger.debug("fireOIFitsDone: {}", oiFitsData);

        fireEvent(new OIFitsEvent(oiFitsData));
    }

    /**
     * Send an event to the registered listeners.
     * Note : new registered listeners may not be called during the processing of this event (no guaranty)
     * @param event event
     */
    private void fireEvent(final ObservationEvent event) {
        // ensure events are fired by Swing EDT :
        if (!SwingUtils.isEDT()) {
            logger.warn("invalid thread : use EDT", new Throwable());
        }
        if (DEBUG_FIRE_EVENT) {
            if (event.getVersion() != null) {
                logger.warn("FIRE {} on version = {}", event, event.getVersion(), new Throwable());
            } else {
                logger.warn("FIRE {}", event, new Throwable());
            }
        }

        logger.debug("fireEvent: {}", event);

        final long start = System.nanoTime();

        // use explicitely size() and get(int) methods to use the up-to-date state of the listener array: 
        for (int i = 0; i < this.listeners.size(); i++) {
            this.listeners.get(i).onProcess(event);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("fireEvent: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        }
    }

    /**
     * Send an event to the given listener.
     * @param event event
     * @param listener listener to call
     */
    private void fireEvent(final ObservationEvent event, final ObservationListener listener) {
        // ensure events are fired by Swing EDT :
        if (!SwingUtils.isEDT()) {
            logger.warn("invalid thread : use EDT", new Throwable());
        }
        if (DEBUG_FIRE_EVENT) {
            if (event.getVersion() != null) {
                logger.warn("FIRE {} on version = {}", event, event.getVersion(), new Throwable());
            } else {
                logger.warn("FIRE {}", event, new Throwable());
            }
        }

        logger.debug("fireEvent: {}", event);

        final long start = System.nanoTime();

        listener.onProcess(event);

        if (logger.isDebugEnabled()) {
            logger.debug("fireEvent: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        }
    }

    // --- Description -----------------------------------------------------------
    /**
     * Set the observation name
     * Used by ObservationForm.updateObservation()
     *
     * @param name name of the observation
     * @return true if the value changed
     */
    public boolean setObservationName(final String name) {
        final ObservationSetting observation = getMainObservation();

        final boolean changed = !name.equals(observation.getName());
        if (changed) {
            if (logger.isTraceEnabled()) {
                logger.trace("setObservationName: {}", name);
            }
            observation.setName(name);
        }
        return changed;
    }

    /**
     * Set the observation description
     * Used by ObservationForm.updateObservation()
     *
     * @param desc the observation description
     * @return true if the value changed
     */
    public boolean setObservationDescription(final String desc) {
        final ObservationSetting observation = getMainObservation();

        final boolean changed = !desc.equals(observation.getDescription());
        if (changed) {
            if (logger.isTraceEnabled()) {
                logger.trace("setObservationDescription: {}", desc);
            }
            observation.setDescription(desc);
        }
        return changed;
    }

    // --- WHEN ------------------------------------------------------------------
    /**
     * Set the observation date (no time)
     * Used by BasicObservationForm.updateObservation()
     *
     * @param date date to use
     * @return true if the value changed
     */
    public boolean setWhen(final Date date) {
        final WhenSetting when = getMainObservation().getWhen();

        final XMLGregorianCalendar newValue = getCalendar(date);

        final boolean changed = !newValue.equals(when.getDate());
        if (changed) {
            if (logger.isTraceEnabled()) {
                logger.trace("setWhen: {}", newValue);
            }
            when.setDate(newValue);
        }
        return changed;
    }

    /**
     * Set the night restriction flag
     * Used by BasicObservationForm.updateObservation()
     *
     * @param useNightLimits flag to enable/disable the night restriction (observability)
     * @return true if the value changed
     */
    public boolean setNightRestriction(final boolean useNightLimits) {
        final WhenSetting when = getMainObservation().getWhen();

        final boolean changed = when.isNightRestriction() != useNightLimits;
        if (changed) {
            if (logger.isTraceEnabled()) {
                logger.trace("setNightRestriction: {}", useNightLimits);
            }
            when.setNightRestriction(useNightLimits);
        }
        return changed;
    }

    /**
     * Set the atmosphere quality
     * Used by UVCoveragePanel.updateObservation()
     *
     * @param atmQuality atmosphere quality to use
     * @return true if the value changed
     */
    public boolean setAtmosphereQuality(final AtmosphereQuality atmQuality) {
        final WhenSetting when = getMainObservation().getWhen();

        final boolean changed = !atmQuality.equals(when.getAtmosphereQuality());
        if (changed) {
            if (logger.isTraceEnabled()) {
                logger.trace("setAtmosphereQuality: {}", atmQuality);
            }
            when.setAtmosphereQuality(atmQuality);
        }
        return changed;
    }

    /**
     * Set the wind azimuth (deg)
     * Used by BasicObservationForm.updateObservation()
     *
     * @param windAzimuth wind azimuth value
     * @return true if the value changed
     */
    public boolean setWindAzimuth(final Double windAzimuth) {
        final WhenSetting when = getMainObservation().getWhen();

        final boolean changed = isChanged(when.getWindAzimuth(), windAzimuth);
        if (changed) {
            if (logger.isTraceEnabled()) {
                logger.trace("setWindAzimuth: {}", windAzimuth);
            }
            when.setWindAzimuth(windAzimuth);
        }
        return changed;
    }

    // --- INTERFEROMETER --------------------------------------------------------
    /**
     * Set the minimum elevation (deg)
     * Used by BasicObservationForm.updateObservation()

     * @param minElev minimum elevation value
     * @return true if the value changed
     */
    public boolean setMinElevation(final double minElev) {
        final InterferometerConfigurationChoice interferometerChoice = getMainObservation().getInterferometerConfiguration();

        final boolean changed = interferometerChoice.getMinElevation() != minElev;
        if (changed) {
            if (logger.isTraceEnabled()) {
                logger.trace("setInterferometerMinElevation: {}", minElev);
            }
            interferometerChoice.setMinElevation(minElev);
        }
        return changed;
    }

    /**
     * Set the interferometer configuration (interferometer + period)
     * and refresh the internal InterferometerConfiguration reference
     * Used by BasicObservationForm.updateObservation()
     *
     * @param name name of the interferometer configuration
     * @return true if the value changed
     */
    public boolean setInterferometerConfigurationName(final String name) {
        final InterferometerConfigurationChoice interferometerChoice = getMainObservation().getInterferometerConfiguration();

        final boolean changed = !name.equals(interferometerChoice.getName());
        if (changed) {
            if (logger.isTraceEnabled()) {
                logger.trace("setInterferometerConfigurationName: {}", name);
            }
            interferometerChoice.setName(name);
            interferometerChoice.setInterferometerConfiguration(cm.getInterferometerConfiguration(name));
        }
        return changed;
    }

    // --- INSTRUMENT ------------------------------------------------------------
    /**
     * Set the instrument configuration (instrument for a given interferometer + period)
     * and refresh the internal InstrumentConfiguration reference
     * Used by BasicObservationForm.updateObservation()
     *
     * @param instrumentAlias name or alias of the instrument
     * @return true if the value changed
     */
    public boolean setInstrumentConfigurationName(final String instrumentAlias) {
        final FocalInstrumentConfigurationChoice instrumentChoice = getMainObservation().getInstrumentConfiguration();

        // use the real instrument name (not alias):
        final String insName = instrumentChoice.getInstrumentConfiguration().getFocalInstrument().getName();

        // Update the instrument anyway:
        if (logger.isTraceEnabled()) {
            logger.trace("setInstrumentConfigurationName: {}", instrumentAlias);
        }
        instrumentChoice.setName(instrumentAlias);
        instrumentChoice.setInstrumentConfiguration(cm.getInterferometerInstrumentConfiguration(
                getMainObservation().getInterferometerConfiguration().getName(), instrumentAlias));

        // Check if the real instrument changed (alias / names may be the same):
        final boolean changed = !insName.equals(instrumentChoice.getInstrumentConfiguration().getFocalInstrument().getName());

        if (logger.isTraceEnabled()) {
            logger.trace("setInstrumentConfigurationName: changed={}", changed);
        }
        return changed;
    }

    /**
     * Set the instrument station configurations (baseline for a given instrument)
     * and update the observation variants
     * Used by BasicObservationForm.updateObservation()
     *
     * @param stationConfs string representations of the station list (AA BB CC ...)
     * @return true if the value changed
     */
    public boolean setInstrumentConfigurationStations(final Object[] stationConfs) {
        if (stationConfs == null || stationConfs.length == 0) {
            throw new IllegalStateException("Undefined configurations !");
        }

        final int len = stationConfs.length;
        final ObservationSetting observation = getMainObservation();

        // first update observation variants :
        final List<ObservationVariant> obsVariants = observation.getVariants();

        boolean changed = this.defineVariants(obsVariants, len);

        String stations;
        ObservationVariant obsVariant;

        for (int i = 0; i < len; i++) {
            stations = (String) stationConfs[i];
            obsVariant = obsVariants.get(i);

            if (!stations.equals(obsVariant.getStations())) {
                changed |= true;
                if (logger.isTraceEnabled()) {
                    logger.trace("setInstrumentConfigurationStations[{}]: {}", i, stations);
                }
                obsVariant.setStations(stations);
                obsVariant.setStationList(cm.getInstrumentConfigurationStations(
                        observation.getInterferometerConfiguration().getName(),
                        observation.getInstrumentConfiguration().getName(), stations));
            }
        }

        // then update the instrument configuration for model compatibility :
        if (changed) {
            obsVariant = obsVariants.get(0);

            final FocalInstrumentConfigurationChoice instrumentChoice = observation.getInstrumentConfiguration();

            instrumentChoice.setStations(obsVariant.getStations());
            instrumentChoice.setStationList(obsVariant.getStationList());
        }
        return changed;
    }

    /**
     * Set the user PoPs associated to the station configuration (baseline for a given instrument)
     * and refresh the internal PopList reference
     * Used by BasicObservationForm.updateObservation()
     *
     * @param pops string representation of the pop list like '12', '111' or '541'
     * @return true if the value changed
     */
    public boolean setInstrumentConfigurationPoPs(final String pops) {
        final ObservationSetting observation = getMainObservation();
        final FocalInstrumentConfigurationChoice instrumentChoice = observation.getInstrumentConfiguration();

        // pops can be null :
        final boolean changed = isChanged(pops, instrumentChoice.getPops());
        if (changed) {
            if (logger.isTraceEnabled()) {
                logger.trace("setInstrumentConfigurationPoPs: {}", pops);
            }
            instrumentChoice.setPops(pops);

            instrumentChoice.setPopList(cm.parseInstrumentPoPs(
                    observation.getInterferometerConfiguration().getName(),
                    observation.getInstrumentConfiguration().getName(),
                    observation.getInstrumentConfiguration().getStations(), pops, null));
        }
        return changed;
    }

    /**
     * Set the instrument mode for the current interferometer and instrument
     * and refresh the internal FocalInstrumentMode reference
     * Used by UVCoveragePanel.updateObservation()
     *
     * @param mode string representation of the instrument mode
     * @return true if the value changed
     */
    public boolean setInstrumentMode(final String mode) {
        final ObservationSetting observation = getMainObservation();
        final FocalInstrumentConfigurationChoice instrumentChoice = observation.getInstrumentConfiguration();

        // mode can be null :
        final boolean changed = isChanged(mode, instrumentChoice.getInstrumentMode());
        if (changed) {
            if (logger.isTraceEnabled()) {
                logger.trace("setInstrumentMode: {}", mode);
            }
            instrumentChoice.setInstrumentMode(mode);
            instrumentChoice.setFocalInstrumentMode(cm.getInstrumentMode(
                    observation.getInterferometerConfiguration().getName(),
                    observation.getInstrumentConfiguration().getName(), mode));
        }
        return changed;
    }

    /**
     * Set the sampling period for the current instrument
     * Used by UVCoveragePanel.updateObservation()
     *
     * @param samplingPeriod sampling period (m) or null if undefined
     * @return true if the value changed
     */
    public boolean setInstrumentSamplingPeriod(final Double samplingPeriod) {
        final FocalInstrumentConfigurationChoice instrumentChoice = getMainObservation().getInstrumentConfiguration();

        // period can be null :
        final boolean changed = isChanged(samplingPeriod, instrumentChoice.getSamplingPeriod());
        if (changed) {
            if (logger.isTraceEnabled()) {
                logger.trace("setInstrumentSamplingPeriod: {}", samplingPeriod);
            }
            instrumentChoice.setSamplingPeriod(samplingPeriod);
        }
        return changed;
    }

    /**
     * Set the acquisition time for the current instrument
     * Used by UVCoveragePanel.updateObservation()
     *
     * @param obsDuration acquisition time (s) or null if undefined
     * @return true if the value changed
     */
    public boolean setInstrumentAcquisitionTime(final Double obsDuration) {
        final FocalInstrumentConfigurationChoice instrumentChoice = getMainObservation().getInstrumentConfiguration();

        // obsDuration can be null :
        final boolean changed = isChanged(obsDuration, instrumentChoice.getAcquisitionTime());
        if (changed) {
            if (logger.isTraceEnabled()) {
                logger.trace("setInstrumentAcquisitionTime: {}", obsDuration);
            }
            instrumentChoice.setAcquisitionTime(obsDuration);
        }
        return changed;
    }

    // --- TARGETS ---------------------------------------------------------------
    /**
     * Return the target of the given name
     * @param name target name
     * @return target or null if the target is not found
     */
    public Target getTarget(final String name) {
        return getMainObservation().getTarget(name);
    }

    /**
     * Return the list of all targets
     * @return list of all targets
     */
    public List<Target> getTargets() {
        return getMainObservation().getTargets();
    }

    /**
     * Return the target user informations (create a new one if needed)
     * @return target user informations
     */
    public TargetUserInformations getTargetUserInfos() {
        return getMainObservation().getOrCreateTargetUserInfos();
    }

    /**
     * Return true if the given target is a calibrator
     * i.e. the calibrator list contains the given target
     * @param target target to use
     * @return true if the given target is a calibrator
     */
    public boolean isCalibrator(final Target target) {
        return getTargetUserInfos().isCalibrator(target);
    }

    /**
     * Return a displayable list of targets containing
     * - science targets followed by their calibrators
     * - calibrator orphans
     * @return displayable list of targets
     */
    public List<Target> getDisplayTargets() {
        return getMainObservation().getDisplayTargets();
    }

    /**
     * Remove the given science target from the target list and
     * fires a target change and an observation change event
     * @param target science target to remove
     */
    public void removeTarget(final Target target) {
        if (Target.removeTarget(target, getTargets(), getTargetUserInfos())) {
            logger.trace("removeTarget: {}", target);
            // fire change events :
            this.fireTargetChangedEvents();
        }
    }

    /**
     * Remove the given calibrator target from the target list
     * and fires a target change and an observation change event
     * @param calibrator calibrator target to remove
     */
    public void removeCalibrator(final Target calibrator) {
        if (calibrator != null) {
            final List<Target> targets = getTargets();
            final TargetUserInformations targetUserInfos = getTargetUserInfos();

            Target.removeCalibratorReferences(calibrator, targets, targetUserInfos);

            if (Target.removeTarget(calibrator, targets, targetUserInfos)) {
                logger.trace("removeCalibrator: {}", calibrator);
                // fire change events :
                this.fireTargetChangedEvents();
            }
        }
    }

    /**
     * Remove the given targets (science and calibrators) from the target list
     * and fires a target change and an observation change event
     * @param removeTargets targets to remove
     */
    public void removeTargetAndCalibrators(final List<Target> removeTargets) {
        if (!CollectionUtils.isEmpty(removeTargets)) {

            final List<Target> targets = getTargets();
            final TargetUserInformations targetUserInfos = getTargetUserInfos();

            boolean changed = false;

            // Remove first science targets:
            for (Target target : removeTargets) {
                if (!isCalibrator(target)) {
                    if (Target.removeTarget(target, targets, targetUserInfos)) {
                        logger.trace("removeTarget: {}", target);
                        changed = true;
                    }
                }
            }

            // Remove calibrator targets:
            for (Target calibrator : removeTargets) {
                if (isCalibrator(calibrator)) {
                    Target.removeCalibratorReferences(calibrator, targets, targetUserInfos);

                    if (Target.removeTarget(calibrator, targets, targetUserInfos)) {
                        logger.trace("removeCalibrator: {}", calibrator);
                        changed = true;
                    }
                }
            }

            if (changed) {
                // fire change events :
                this.fireTargetChangedEvents();
            }
        }
    }

    /**
     * Update diameter parameter of uniform disk models for the given list of calibrator targets
     * using their CalibratorInformations (SearchCal) :
     *
     * find correct diameter among UD_ for the observation instrument band
     * or using alternate diameters (in order of priority) : UD, LD, UDDK, DIA12
     *
     * @param calibrators list of calibrator targets
     * @return true if any diameter value changed
     */
    public boolean defineCalibratorDiameter(final List<Target> calibrators) {
        boolean changed = false;
        final ObservationSetting observation = getMainObservation();

        // Find instrument band from main observation :
        FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();
        if (insMode == null) {
            // use the first instrument mode of the instrument:
            final Vector<String> instrumentModes = ConfigurationManager.getInstance().getInstrumentModes(
                    observation.getInterferometerConfiguration().getName(),
                    observation.getInstrumentConfiguration().getName());

            if (instrumentModes.isEmpty()) {
                throw new IllegalStateException("The instrumentMode is empty !");
            }
            insMode = ConfigurationManager.getInstance().getInstrumentMode(
                    observation.getInterferometerConfiguration().getName(),
                    observation.getInstrumentConfiguration().getName(),
                    instrumentModes.get(0));

            logger.info("The instrumentMode is empty; using first instrument Mode {}", insMode.getName());
        }

        final double lambda = insMode.getWaveLength();
        if (logger.isDebugEnabled()) {
            logger.debug("lambda: {}", lambda);
        }

        final Band band = Band.findBand(lambda);
        final SpectralBand insBand = SpectralBandUtils.findBand(band);

        if (logger.isDebugEnabled()) {
            logger.debug("band: {}", band);
            logger.debug("insBand: {}", insBand);
        }

        for (Target cal : calibrators) {

            // set disk diameter according to instrument band :
            if (!cal.getModels().isEmpty()) {
                final Model diskModel = cal.getModels().get(0);

                if (ModelDefinition.MODEL_DISK.equals(diskModel.getType())) {
                    final Parameter diameterParameter = diskModel.getParameter(ModelDefinition.PARAM_DIAMETER);

                    if (diameterParameter != null) {
                        // if UD_<band> diameter is missing, use other values ...
                        final Double udBand = cal.getCalibratorInfos().getUDDiameter(insBand);

                        if (udBand != null) {
                            if (logger.isInfoEnabled()) {
                                logger.info("Define uniform disk diameter for calibrator ["
                                        + cal.getName() + "] using diameter UD_" + insBand + " = " + udBand);
                            }
                            diameterParameter.setValue(udBand.doubleValue());
                            changed = true;

                        } else {
                            // use alternate diameter (in order of priority) :
                            final BaseValue diam = cal.getCalibratorInfos().getAlternateDiameter();

                            if (diam != null) {
                                if (logger.isInfoEnabled()) {
                                    logger.info("Define uniform disk diameter for calibrator ["
                                            + cal.getName() + "] using diameter " + diam.getName() + " = " + diam.getValue());
                                }

                                diameterParameter.setValue(diam.getNumber().doubleValue());
                                changed = true;

                            } else {
                                if (logger.isInfoEnabled()) {
                                    logger.info("No diameter available for calibrator [{}], set to 0.0", cal.getName());
                                }

                                diameterParameter.setValue(0d);
                                changed = true;
                            }
                        }
                    }
                }
            }
        }
        if (logger.isDebugEnabled()) {
            logger.debug("diameter(s) changed: {}", changed);
        }
        return changed;
    }

    /**
     * Update the list of targets using the given list of targets and target user informations
     * (different instances) and fires a target change and an observation change event
     * @param newTargets new target list
     * @param newTargetUserInfos new target user informations
     */
    public void updateTargets(final List<Target> newTargets, final TargetUserInformations newTargetUserInfos) {

        final List<Target> targets = getTargets();
        targets.clear();
        targets.addAll(newTargets);

        getMainObservation().setTargetUserInfos(newTargetUserInfos);

        // check and update target references :
        getMainObservation().checkReferences();

        // fire change events :
        this.fireTargetChangedEvents();
    }

    /**
     * This fires a target change and an observation change event to all registered listeners.
     */
    public void fireTargetChangedEvents() {
        // force clear display target cache :
        getMainObservation().clearCacheTargets();

        // fire an observation targets change event :
        this.fireObservationTargetsChanged();

        // fire an observation update event and force refresh :
        this.fireObservationUpdate(true);
    }

    // --- TARGET CONFIGURATION --------------------------------------------------
    /**
     * Return the target configuration for the given target (FT mode, HA min/max)
     * @param name target name
     * @return target configuration or null if the target is not found
     */
    public TargetConfiguration getTargetConfiguration(final String name) {
        return getMainObservation().getTargetConfiguration(name);
    }

    /**
     * Set the HA lower value for the given target name
     * Used by UVCoveragePanel.updateObservation()
     *
     * @param name name of the target
     * @param haMin HA lower value or null to clear it
     * @return true if the value changed
     */
    public boolean setTargetHAMin(final String name, final Double haMin) {
        final TargetConfiguration targetConf = getTargetConfiguration(name);
        if (targetConf == null) {
            return false;
        }
        // haMin can be null :
        final boolean changed = isChanged(haMin, targetConf.getHAMin());
        if (changed) {
            if (logger.isTraceEnabled()) {
                logger.trace("setTargetHAMin: {}", haMin);
            }
            targetConf.setHAMin(haMin);
        }
        return changed;
    }

    /**
     * Set the HA upper value for the given target name
     * Used by UVCoveragePanel.updateObservation()
     *
     * @param name name of the target
     * @param haMax HA upper value or null to clear it
     * @return true if the value changed
     */
    public boolean setTargetHAMax(final String name, final Double haMax) {
        final TargetConfiguration targetConf = getTargetConfiguration(name);
        if (targetConf == null) {
            return false;
        }
        // haMax can be null :
        final boolean changed = isChanged(haMax, targetConf.getHAMax());
        if (changed) {
            if (logger.isTraceEnabled()) {
                logger.trace("setTargetHAMax: {}", haMax);
            }
            targetConf.setHAMax(haMax);
        }
        return changed;
    }

    /**
     * Set the Fringe tracker mode for the given target name
     * Used by UVCoveragePanel.updateObservation()
     *
     * @param name name of the target
     * @param ftMode Fringe tracker mode or 'None'
     * @return true if the value changed
     */
    public boolean setTargetFTMode(final String name, final String ftMode) {
        final TargetConfiguration targetConf = getTargetConfiguration(name);
        if (targetConf == null) {
            return false;
        }
        // special case : None
        final String mode = (AsproConstants.NONE.equals(ftMode)) ? null : ftMode;

        // ftMode can be null :
        final boolean changed = isChanged(mode, targetConf.getFringeTrackerMode());
        if (changed) {
            if (logger.isTraceEnabled()) {
                logger.trace("setTargetFTMode: {}", mode);
            }
            targetConf.setFringeTrackerMode(mode);
        }
        return changed;
    }

    // --- COMPUTATION RESULTS ---------------------------------------------------
    /**
     * Defines the OIFits structures (SHARED) for later reuse (OIFits Explorer)
     * Used by UVCoveragePanel.UVCoverageSwingWorker.refreshUI()
     *
     * @param oiFitsData OIFits structures (or null)
     */
    public void setOIFitsData(final OIFitsData oiFitsData) {
        logger.debug("setOIFitsData: {}", oiFitsData);

        // TODO: use a new class OIFitsData (version / File / Target) ...
        // Use OIFitsCollectionManager ?
        this.oiFitsData = (oiFitsData == null || oiFitsData.getOIFitsList().isEmpty()) ? null : oiFitsData;

        // Fire OIFitsDone event to inform panels to be updated anyway:
        this.fireOIFitsDone(this.oiFitsData);
    }

    /**
     * Return the computed OIFits structures (read only)
     * @return OIFits structures or null
     */
    public OIFitsData getOIFitsData() {
        return this.oiFitsData;
    }

    /**
     * Return the computed OIFits structures (read only) but check warnings and potentially display a confirmation dialog
     * @return OIFits structures or null
     */
    public List<OIFitsFile> checkAndGetOIFitsList() {
        return (this.oiFitsData != null) ? this.oiFitsData.checkAndGetOIFitsList() : null;
    }

    // --- OBSERVATION VARIANTS -------------------------------------------------
    /**
     * Update the observation variant list to have the correct size and allocate observation variants if needed
     * @param obsVariants observation variant list (not null)
     * @param size number of variants
     * @return true if the list changed
     */
    private boolean defineVariants(final List<ObservationVariant> obsVariants, final int size) {
        if (size == obsVariants.size()) {
            return false;
        }
        // add new variants :
        for (int i = 0, len = size - obsVariants.size(); i < len; i++) {
            obsVariants.add(new ObservationVariant());
        }
        // remove unnecessary variants :
        for (int i = obsVariants.size() - 1; i >= size; i--) {
            obsVariants.remove(i);
        }
        return true;
    }

    // --- USER MODELS ----------------------------------------------------------
    /**
     * Check external file references (target user models) and load these files
     * @param obsFile observation file (may be null)
     * @param observation observation to process
     * @param sb message buffer
     */
    private static void checkAndLoadFileReferences(final File obsFile, final ObservationSetting observation, final StringBuilder sb) {
        // Check target user model files (exist and can read):
        for (Target target : observation.getTargets()) {
            checkTargetUserModel(obsFile, target, sb);
        }

        // Load only valid target user model files:
        UserModel userModel;

        for (Target target : observation.getTargets()) {
            userModel = target.getUserModel();

            if (userModel != null && userModel.isFileValid()) {
                // see TargetModelForm.prepareAndValidateUserModel()
                boolean valid = false;
                try {
                    // throws exceptions if the given fits file or image is incorrect:
                    UserModelService.prepareUserModel(userModel);

                    // validate image against the given observation:
                    ObservationManager.validateUserModel(observation, userModel);

                    // model is valid:
                    valid = true;

                } catch (IllegalArgumentException iae) {
                    logger.warn("Incorrect fits image in file [{}]", userModel.getFile(), iae);
                    sb.append("Loading user model file [").append(userModel.getFile()).append("] failed:\n").append(iae.getMessage());
                } catch (FitsException fe) {
                    logger.error("FITS failure on file [{}]", userModel.getFile(), fe);
                    sb.append("Loading user model file [").append(userModel.getFile()).append("] failed:\n").append(fe.getMessage());
                } catch (IOException ioe) {
                    logger.error("IO failure on file [{}]", userModel.getFile(), ioe);
                    sb.append("Loading user model file [").append(userModel.getFile()).append("] failed:\n").append(ioe.getMessage());
                } finally {
                    if (!valid) {
                        sb.append("\n\nThe target [").append(target.getName()).append("] has an invalid user model; this model is disabled.\n\n");
                    }
                    // anyway, update the valid flag:
                    userModel.setFileValid(valid);
                }
            }
        }
    }

    /**
     * Check the given target's user model
     * @param obsFile observation file (may be null)
     * @param target target to check
     * @param sb message buffer
     */
    private static void checkTargetUserModel(final File obsFile, final Target target, final StringBuilder sb) {
        final UserModel userModel = target.getUserModel();

        if (userModel != null) {
            String filePath = userModel.getFile();

            if (filePath != null && filePath.startsWith("./")) {
                filePath = obsFile.getParent() + filePath.substring(1);

                logger.info("Fixed relative path [{}] to [{}]", userModel.getFile(), filePath);
                userModel.setFile(filePath);
            }

            logger.debug("checking file path [{}]", filePath);

            final File file = FileUtils.getFile(filePath);

            boolean valid = false;

            if (file == null) {
                sb.append("User model file [").append(filePath).append("] does not exist.");
            } else if (!file.canRead()) {
                sb.append("User model file [").append(filePath).append("] can not be read.");
            } else {
                valid = true;
            }

            if (!valid) {
                sb.append("\nThe target [").append(target.getName()).append("] has an invalid user model; this model is disabled.\n\n");
            }
            // anyway, update the valid flag:
            userModel.setFileValid(valid);
        }
    }

    /**
     * Validate the given user model using the main observation to get the maximum UV frequency (rad-1) possible
     * @param model user model to validate
     */
    public void validateUserModel(final UserModel model) {
        validateUserModel(getMainObservation(), model);
    }

    /**
     * Validate the given user model using the given observation to get the maximum UV frequency (rad-1) possible
     * @param observation observation to get interferometer and instrument configurations
     * @param model user model to validate
     */
    public static void validateUserModel(final ObservationSetting observation, final UserModel model) {
        UserModelService.validateModel(model, getUVMaxFreq(observation));
    }

    /**
     * Get the maximum UV frequency (rad-1) possible in the GUI
     * @param observation observation to get interferometer and instrument configurations
     * @return maximum UV frequency (rad-1)
     */
    private static double getUVMaxFreq(final ObservationSetting observation) {
        final FocalInstrumentConfiguration insConf = observation.getInstrumentConfiguration().getInstrumentConfiguration();

        final double maxBaseLines = ConfigurationManager.getInstrumentConfigurationMaxBaseline(insConf,
                observation.getInstrumentConfiguration().getStations());

        if (logger.isDebugEnabled()) {
            logger.debug("instrument configuration: {}; baseline max = {}", observation.getInstrumentConfiguration().getStations(), maxBaseLines);
        }

        final FocalInstrumentMode instrumentMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();
        final double lambdaMin = AsproConstants.MICRO_METER * instrumentMode.getWaveLengthMin();

        // Adjust the user uv Max = max base line / minimum wave length
        // note : use the minimum wave length of the instrument to
        // - make all uv segment visible
        // - avoid too much model computations (when the instrument mode changes)
        final double uvMaxFreq = maxBaseLines / lambdaMin;

        return uvMaxFreq;
    }

    // --- INTERNAL METHODS ------------------------------------------------------
    /**
     * Define default values (empty child objects)
     * @param observation observation to modify
     * @throws IllegalStateException if an invalid reference was found (interferometer / instrument / instrument configuration)
     */
    private void defineDefaults(final ObservationSetting observation) throws IllegalStateException {

        boolean changeConfiguration = false;
        try {
            // First: update Configuration
            cm.changeConfiguration(observation.getExtendedConfiguration());

            if (observation.getName() == null || observation.getName().length() == 0) {
                observation.setName("default");
            }
            if (observation.getWhen() == null) {
                final WhenSetting when = new WhenSetting();
                when.setDate(getCalendar(new Date()));
                when.setNightRestriction(AsproConstants.DEFAULT_USE_NIGHT_LIMITS);

                observation.setWhen(when);
            }
            if (observation.getInterferometerConfiguration() == null) {
                final String defInterferometer = cm.getInterferometerNames().get(0);
                final String defInterferometerConfiguration = cm.getInterferometerConfigurationNames(defInterferometer).get(0);

                if (logger.isDebugEnabled()) {
                    logger.debug("default Interferometer: {}", defInterferometer);
                    logger.debug("default InterferometerConfiguration: {}", defInterferometerConfiguration);
                }

                final InterferometerConfigurationChoice interferometerChoice = new InterferometerConfigurationChoice();
                interferometerChoice.setName(defInterferometerConfiguration);
                interferometerChoice.setMinElevation(Preferences.getInstance().getPreferenceAsDouble(Preferences.MIN_ELEVATION));

                observation.setInterferometerConfiguration(interferometerChoice);
            }
            if (observation.getInstrumentConfiguration() == null) {
                final String defInstrument = cm.getInterferometerInstrumentNames(observation.getInterferometerConfiguration().getName()).get(0);
                final String defInstrumentConfiguration = cm.getInstrumentConfigurationNames(observation.getInterferometerConfiguration().getName(), defInstrument).get(0);

                if (logger.isDebugEnabled()) {
                    logger.debug("default Instrument: {}", defInstrument);
                    logger.debug("default InstrumentConfiguration: {}", defInstrumentConfiguration);
                }

                final FocalInstrumentConfigurationChoice instrumentChoice = new FocalInstrumentConfigurationChoice();
                instrumentChoice.setName(defInstrument);
                instrumentChoice.setStations(defInstrumentConfiguration);

                observation.setInstrumentConfiguration(instrumentChoice);
            }
            if (observation.getVariants().isEmpty()) {
                // create a new variant having the same configuration (stations only) :
                final ObservationVariant obsVariant = new ObservationVariant();

                // Note : stations can not be null :
                obsVariant.setStations(observation.getInstrumentConfiguration().getStations());

                // create a new collection :
                observation.getVariants().add(obsVariant);
            }

            // update references :
            // can throw IllegalStateException if an invalid reference was found :
            updateObservation(observation);

            // always define the default target groups:
            TargetUtils.createDefaultTargetGroups(observation.getOrCreateTargetUserInfos());

            // commit configuration changes:
            changeConfiguration = true;

        } finally {
            // Finally: commit or rollback Configuration change:
            cm.validateChangedConfiguration(changeConfiguration);
        }
    }

    /**
     * Update observation with resolved references  (interferometer / instrument / instrument configuration ...)
     * @param observation observation to use
     * @throws IllegalStateException if an invalid reference was found (interferometer / instrument / instrument configuration)
     */
    private void updateObservation(final ObservationSetting observation) throws IllegalStateException {
        // ugly code to update all resolved references used on post load :
        final InterferometerConfigurationChoice interferometerChoice = observation.getInterferometerConfiguration();

        String interferometerConfiguration = interferometerChoice.getName();
        interferometerChoice.setInterferometerConfiguration(cm.getInterferometerConfiguration(interferometerConfiguration));

        final FocalInstrumentConfigurationChoice instrumentChoice = observation.getInstrumentConfiguration();
        final String instrumentAlias = instrumentChoice.getName();

        if (interferometerChoice.getInterferometerConfiguration() == null) {
            logger.info("the interferometer configuration [{}] is not supported.", interferometerConfiguration);

            // use the first interferometer configuration that has the instrument:
            interferometerChoice.setInterferometerConfiguration(cm.getInterferometerConfigurationWithInstrument(instrumentAlias));

            if (interferometerChoice.getInterferometerConfiguration() != null) {
                interferometerConfiguration = interferometerChoice.getInterferometerConfiguration().getName();
                interferometerChoice.setName(interferometerConfiguration);

                logger.info("A correct instrument configuration is [{}]. Save your file to keep this modification", interferometerConfiguration);
            } else {
                throw new IllegalStateException("The interferometer configuration [" + interferometerConfiguration + "] is invalid"
                        + " and none has the instrument [" + instrumentAlias + "] !");
            }
        }

        instrumentChoice.setInstrumentConfiguration(cm.getInterferometerInstrumentConfiguration(interferometerConfiguration, instrumentAlias));

        final FocalInstrumentConfiguration insConf = instrumentChoice.getInstrumentConfiguration();
        if (insConf == null) {
            throw new IllegalStateException("The instrument [" + instrumentAlias + "] is invalid !");
        }

        // first resolve / fix observation variants :
        for (ObservationVariant obsVariant : observation.getVariants()) {
            obsVariant.setStationList(ConfigurationManager.getInstrumentConfigurationStations(insConf, obsVariant.getStations()));

            // fix invalid stations :
            if (obsVariant.getStationList() == null) {
                logger.info("the instrument configuration [{}] is incorrect, trying to match a possible configuration ...", obsVariant.getStations());

                final String stationIds = ConfigurationManager.findInstrumentConfigurationStations(insConf, obsVariant.getStations());

                if (stationIds == null) {
                    throw new IllegalStateException("The instrument configuration [" + obsVariant.getStations() + "] is invalid !");
                }

                logger.info("the correct instrument configuration is [{}]. Save your file to keep this modification", stationIds);

                obsVariant.setStations(stationIds);
                obsVariant.setStationList(ConfigurationManager.getInstrumentConfigurationStations(insConf, stationIds));
            }
        }

        // then update the instrument configuration for model compatibility :
        final ObservationVariant obsVariant = observation.getVariants().get(0);
        instrumentChoice.setStations(obsVariant.getStations());
        instrumentChoice.setStationList(obsVariant.getStationList());

        // pops can be undefined :
        instrumentChoice.setPopList(cm.parseInstrumentPoPs(interferometerConfiguration, instrumentAlias, obsVariant.getStations(),
                instrumentChoice.getPops(), null));

        // instrument mode can be undefined :
        instrumentChoice.setFocalInstrumentMode(cm.getInstrumentMode(interferometerConfiguration, instrumentAlias, instrumentChoice.getInstrumentMode()));

        if (logger.isTraceEnabled()) {
            logger.trace("updateObservation: {}", toString(observation));
        }
    }

    /**
     * Utility method to get a string representation of the given observation settings
     * @param observation observation to use
     * @return string representation of the given observation settings
     */
    public static String toString(final ObservationSetting observation) {
        final StringBuilder sb = new StringBuilder(255);
        sb.append("name : ").append(observation.getName());
        sb.append(", date : ").append(observation.getWhen().getDate());
        sb.append(", interferometer : ").append(observation.getInterferometerConfiguration().getName());
        // instrument :
        sb.append(", instrument : ").append(observation.getInstrumentConfiguration().getName());
        sb.append(", stations : ").append(observation.getInstrumentConfiguration().getStations());
        if (observation.getInstrumentConfiguration().getPops() != null) {
            sb.append(", pops : ").append(observation.getInstrumentConfiguration().getPops());
        }
        if (observation.getInstrumentConfiguration().getInstrumentMode() != null) {
            sb.append(", mode : ").append(observation.getInstrumentConfiguration().getInstrumentMode());
        }
        if (observation.hasTargets()) {
            sb.append(", targets : \n").append(observation.getTargets());
        }

        return sb.toString();
    }

    /**
     * Utility method to get a string representation of the given observation collection
     * @param obsCollection observation collection to use
     * @return string representation of the given observation collection
     */
    public static String toString(final ObservationCollection obsCollection) {
        final StringBuilder sb = new StringBuilder(255);
        sb.append("name : ").append(obsCollection.getName());
        int i = 0;
        for (ObservationSetting observation : obsCollection.getObservations()) {
            sb.append("\nobservation(").append(i++).append(") : ").append(toString(observation));
        }
        return sb.toString();
    }

    /**
     * Check if the objects are different supporting null values
     * @param value1 string 1
     * @param value2 string 2
     * @return true only if objects are different
     */
    private boolean isChanged(final Object value1, final Object value2) {
        return (value1 == null && value2 != null) || (value1 != null && value2 == null) || (value1 != null && value2 != null && !value1.equals(value2));
    }
}
