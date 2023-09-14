/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.conf.AsproConf;
import fr.jmmc.aspro.model.oi.AdaptiveOptics;
import fr.jmmc.aspro.model.oi.AdaptiveOpticsSetup;
import fr.jmmc.aspro.model.oi.AzEl;
import fr.jmmc.aspro.model.oi.Channel;
import fr.jmmc.aspro.model.oi.Configurations;
import fr.jmmc.aspro.model.oi.DelayLine;
import fr.jmmc.aspro.model.oi.FocalInstrument;
import fr.jmmc.aspro.model.oi.FocalInstrumentConfiguration;
import fr.jmmc.aspro.model.oi.FocalInstrumentConfigurationItem;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.FocalInstrumentSetup;
import fr.jmmc.aspro.model.oi.FringeTracker;
import fr.jmmc.aspro.model.oi.HorizonProfile;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.InterferometerFile;
import fr.jmmc.aspro.model.oi.InterferometerSetting;
import fr.jmmc.aspro.model.oi.LonLatAlt;
import fr.jmmc.aspro.model.oi.Pop;
import fr.jmmc.aspro.model.oi.Position3D;
import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.StationLinks;
import fr.jmmc.aspro.model.oi.SwitchYard;
import fr.jmmc.aspro.model.oi.Telescope;
import fr.jmmc.aspro.model.util.SpectralBandUtils;
import fr.jmmc.aspro.service.AtmosphereSpectrumService;
import fr.jmmc.aspro.service.GeocentricCoords;
import fr.jmmc.jmal.Band;
import fr.jmmc.jmal.util.MathUtils;
import fr.jmmc.jmcs.Bootstrapper;
import fr.jmmc.jmcs.data.app.ApplicationDescription;
import fr.jmmc.jmcs.gui.FeedbackReport;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.util.FileUtils;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.ObjectUtils;
import fr.jmmc.jmcs.util.ResourceUtils;
import fr.jmmc.jmcs.util.StringUtils;
import fr.jmmc.oitools.util.CombUtils;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class manages configuration files for the Interferometer configurations
 * @author bourgesl
 */
public final class ConfigurationManager extends BaseOIManager {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(ConfigurationManager.class.getName());
    /** enable support for user configurations */
    public static final boolean ENABLE_USER_CONFIG = (ApplicationDescription.isBetaVersion()
            || "true".equalsIgnoreCase(System.getProperty("aspro.expertMode", "false")));

    /** debug configuration at startup */
    private static final boolean DEBUG_CONF = false;
    /** dump horizons at startup */
    private static final boolean DUMP_HORIZON = false;
    /** Internal Configurations file name */
    private static final String CONF_FILE_INTERNAL = "AsproOIConfigurations.xml";
    /** User Configurations file path */
    private static final String CONF_FILE_USER = FileUtils.getPlatformPreferencesPath() + "AsproOIConfigurations-user.xml";
    /** singleton pattern */
    private static volatile ConfigurationManager instance = null;
    /** empty vector */
    private final static Vector<String> EMPTY_VECTOR = new Vector<String>(0);
    /** upper limit to generate combinations to match configurations */
    private final static int MAX_NUM_STATIONS = 6;

    /* members */
    /** aspro conf description (version and release notes) */
    private ApplicationDescription asproConfDescription = null;
    /** Internal Configuration read from configuration files (jar) */
    private final Configuration internalConfiguration = new Configuration();
    /** User Configurations read from configuration files (file) */
    private final UserConfigurations userConfigurations = new UserConfigurations();
    /** Initial Configuration = Internal + User Configuration (merged) */
    private final Configuration initialConfiguration = new Configuration();
    /** Current Configuration (may be different from initial configuration): observation can override some elements */
    private Configuration currentConfiguration = this.initialConfiguration;
    /** Previous Configuration (may be different from initial configuration): observation can override some elements */
    private Configuration previousConfiguration = null;

    /**
     * Return the ConfigurationManager singleton
     * @return ConfigurationManager singleton
     *
     * @throws IllegalStateException if the configuration files are not found or IO failure
     * @throws IllegalArgumentException if the load configuration failed
     */
    public static synchronized ConfigurationManager getInstance()
            throws IllegalStateException, IllegalArgumentException {

        if (instance == null) {
            final ConfigurationManager cm = new ConfigurationManager();

            // can throw RuntimeException :
            cm.initialize();

            instance = cm;
        }
        return instance;
    }

    /**
     * Private constructor
     */
    private ConfigurationManager() {
        super();
    }

    /**
     * Return the aspro conf description (version and release notes)
     * @return aspro conf description (version and release notes)
     */
    public ApplicationDescription getConfDescription() {
        return this.asproConfDescription;
    }

    /**
     * Initialize the configuration :
     * - load AsproOIConfigurations.xml to get configuration file paths
     * - load those files (InterferometerSetting)
     * - update interferometer description and configuration maps
     *
     * @throws IllegalStateException if the configuration files are not found or IO failure
     * @throws IllegalArgumentException if the load configuration failed
     */
    private void initialize()
            throws IllegalStateException, IllegalArgumentException {

        this.asproConfDescription = ApplicationDescription.loadDescription("fr/jmmc/aspro/conf/resource/ApplicationData.xml");

        logger.info("loading Aspro2 configuration '{}' ...", asproConfDescription.getProgramVersion());

        // skip loading configuration for CLI processing:
        if (Bootstrapper.isHeadless()) {
            logger.info("initialize: skipped (headless)");
            return;
        }

        initializeInternalConfiguration();
        if (ENABLE_USER_CONFIG) {
            initializeUserConfiguration();
            // save if any change:
            saveUserConfigurations();
        }
        mergeConfigurations();
    }

    /*
    ----- Internal configuration API -----
     */
    /**
     * Initialize the given configuration :
     * - load AsproOIConfigurations.xml to get configuration file paths in the classpath
     * - load those files (InterferometerSetting)
     * - update interferometer description and configuration
     *
     * @throws IllegalStateException if the configuration files are not found or IO failure
     * @throws IllegalArgumentException if the load configuration failed
     */
    private void initializeInternalConfiguration()
            throws IllegalStateException, IllegalArgumentException {

        final Configuration configuration = getInternalConfiguration();

        boolean isConfValid = true;

        final long start = System.nanoTime();

        final Configurations conf = (Configurations) loadConfigObject(CONF_FILE_INTERNAL);

        final String minVersion = conf.getMinVersion();

        // check configuration's mininimum version vs application's version:
        logger.info("initializeInternalConfiguration: minimum required version = {}", minVersion);

        final String appVersion = ApplicationDescription.getInstance().getProgramVersion();

        final double min = ApplicationDescription.parseVersion(minVersion);
        final double current = ApplicationDescription.parseVersion(appVersion);

        if (min > current) {
            FeedbackReport.openDialog(true,
                    new IllegalStateException("The Aspro2 configuration requires a more recent Aspro2 application: "
                            + minVersion + " > " + appVersion + ".\n\n Please use a public Aspro2 release available: "
                            + ApplicationDescription.getInstance().getLinkValue()));
        }

        // Load configuration files:
        for (InterferometerFile fileRef : conf.getInterferometerFiles()) {
            final String fileName = fileRef.getFile();
            final long checksumConf = fileRef.getChecksum();

            logger.info("initializeInternalConfiguration: loading configuration file = {}", fileName);

            final InterferometerSetting is = (InterferometerSetting) loadConfigObject(fileName);
            is.getDescription().setFileRef(fileRef);

            // compute checksum on file stream:
            final long checksumFile = checksum(fileName);

            // test checksum:
            final boolean isChecksumValid = (checksumFile == checksumConf);

            if (!isChecksumValid) {
                logger.info("initializeInternalConfiguration: checksum[{}] is invalid !", fileName);
            }
            // update information:
            fileRef.setChecksumFile(checksumFile);
            is.getDescription().setChecksumValid(isChecksumValid);

            addInterferometerSetting(configuration, is);

            isConfValid &= isChecksumValid;
        }

        logger.info("initializeInternalConfiguration: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

        // Warning:
        if (!isConfValid) {
            final StringBuilder msg = new StringBuilder(128);
            msg.append("Aspro2 configuration files have been modified for interferometers:\n");

            for (InterferometerDescription id : configuration.getInterferometerDescriptions()) {
                if (!id.isChecksumValid()) {
                    msg.append(id.getName()).append('\n');
                }
            }

            msg.append("\nUSE THESE INTERFEROMETER CONFIGURATIONS AT YOUR OWN RISKS.");

            MessagePane.showWarning(msg.toString(), "Configuration modified");
        }

        logger.debug("initializeInternalConfiguration: internalConfiguration: {}", configuration);
    }

    private Configuration getInternalConfiguration() {
        return this.internalConfiguration;
    }

    /*
    ----- User configuration API -----
     */
    /**
     * Initialize the given configuration :
     * - load user files (InterferometerSetting)
     *
     * @throws IllegalStateException if the configuration files are not found or IO failure
     * @throws IllegalArgumentException if the load configuration failed
     */
    private void initializeUserConfiguration()
            throws IllegalStateException, IllegalArgumentException {

        final File confFile = FileUtils.getFile(CONF_FILE_USER);

        if (confFile != null) {
            final long start = System.nanoTime();

            Configurations conf = null;
            try {
                conf = (Configurations) loadConfigObject(confFile);
            } catch (IOException ioe) {
                logger.error("Load failure on {}", confFile, ioe);
            } catch (RuntimeException re) {
                logger.error("Load failure on {}", confFile, re);
            }
            if (conf == null) {
                logger.info("initializeUserConfiguration: Unable to load user configurations: {} !", CONF_FILE_USER);
                return;
            }

            // keep reference to save later:
            getUserConfigurations().setConfigurations(conf);

            final StringBuilder sb = new StringBuilder(128);

            // Load configuration files:
            for (InterferometerFile fileRef : conf.getInterferometerFiles()) {
                loadUserConfiguration(fileRef, sb);
            }

            logger.info("initializeUserConfiguration: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

            if (sb.length() > 0) {
                MessagePane.showMessage(sb.toString(), "User Configuration");
            }

            if (!conf.getInterferometerFiles().isEmpty()) {
                final StringBuilder msg = new StringBuilder(128);
                msg.append("Aspro2 user configuration files have been loaded for interferometers:\n");

                for (InterferometerFile fileRef : conf.getInterferometerFiles()) {
                    final boolean enabled = fileRef.isReallyEnabled();
                    msg.append("[").append(enabled ? "X" : "-").append("] ");
                    if (fileRef.isValid()) {
                        msg.append("[").append(fileRef.getInterferometerName()).append("] ");
                    }
                    msg.append(new File(fileRef.getFile()).getName()).append('\n');
                }

                msg.append("\nUSE THESE INTERFEROMETER CONFIGURATIONS AT YOUR OWN RISKS.");

                MessagePane.showMessage(msg.toString(), "Configuration modified");
            }
        }

        logger.debug("initializeUserConfiguration: userConfigurations: {}", getUserConfigurations());
    }

    private UserConfigurations getUserConfigurations() {
        return this.userConfigurations;
    }

    public List<InterferometerFile> getUserConfigurationFiles() {
        final List<InterferometerFile> fileRefs = (ENABLE_USER_CONFIG) ? getUserConfigurations().getInterferometerFiles() : null;
        if ((fileRefs == null) || fileRefs.isEmpty()) {
            return Collections.emptyList();
        }
        return new ArrayList<>(fileRefs);
    }

    public boolean addUserConfiguration(final String filePath) {
        if (ENABLE_USER_CONFIG) {
            logger.debug("addUserConfiguration: {}", filePath);

            InterferometerFile fileRef = getUserConfigurations().getInterferometerFile(filePath);
            if (fileRef != null) {
                return reloadUserConfiguration(filePath);
            } else {
                fileRef = new InterferometerFile();
                fileRef.setFile(filePath);
                fileRef.setEnabled(true);

                // insert on top priority:
                getUserConfigurations().getOrCreateConfigurations().getInterferometerFiles().add(fileRef);

                final StringBuilder sb = new StringBuilder(128);

                final boolean ok = loadUserConfiguration(fileRef, sb);

                logger.debug("addUserConfiguration: userConfigurations: {}", getUserConfigurations());

                // save if any change:
                saveUserConfigurations();
                mergeConfigurations();

                if (sb.length() > 0) {
                    MessagePane.showMessage(sb.toString(), "User Configuration");
                }
                return ok;
            }
        }
        return false;
    }

    public boolean reloadUserConfiguration(final String filePath) {
        if (ENABLE_USER_CONFIG) {
            logger.debug("reloadUserConfiguration: {}", filePath);

            final InterferometerFile fileRef = getUserConfigurations().getInterferometerFile(filePath);
            if (fileRef != null) {
                fileRef.setEnabled(true);

                final StringBuilder sb = new StringBuilder(128);

                final boolean ok = loadUserConfiguration(fileRef, sb);

                logger.debug("reloadUserConfiguration: userConfigurations: {}", getUserConfigurations());

                // save if any change:
                saveUserConfigurations();
                mergeConfigurations();

                if (sb.length() > 0) {
                    MessagePane.showMessage(sb.toString(), "User Configuration");
                }
                return ok;
            }
        }
        return false;
    }

    public boolean removeUserConfiguration(final String filePath) {
        if (ENABLE_USER_CONFIG) {
            logger.debug("removeUserConfiguration: {}", filePath);

            final InterferometerFile fileRef = getUserConfigurations().getInterferometerFile(filePath);
            if (fileRef != null) {
                final List<InterferometerFile> files = getUserConfigurations().getInterferometerFiles();
                if (files != null) {
                    getUserConfigurations().setChanged(true);

                    boolean ok = files.remove(fileRef);

                    ok |= getUserConfigurations().removeLoadedConfiguration(filePath);

                    logger.debug("removeUserConfiguration: userConfigurations: {}", getUserConfigurations());

                    // save if any change:
                    saveUserConfigurations();
                    mergeConfigurations();
                    return ok;
                }
            }
        }
        return false;
    }

    public boolean setUserConfigurationEnabled(final String filePath, boolean enabled) {
        if (ENABLE_USER_CONFIG) {
            logger.debug("setUserConfigurationEnabled: {} enabled: {}", filePath, enabled);

            final InterferometerFile fileRef = getUserConfigurations().getInterferometerFile(filePath);
            if (fileRef != null) {
                if ((fileRef.isReallyEnabled() != enabled)
                        && (!enabled || (enabled && fileRef.isValid()))) {

                    getUserConfigurations().setChanged(true);
                    fileRef.setEnabled(enabled);

                    // disable other config:
                    disableOtherUserConfigurations(fileRef);

                    logger.debug("setUserConfigurationEnabled: userConfigurations: {}", getUserConfigurations());

                    // save if any change:
                    saveUserConfigurations();
                    mergeConfigurations();
                    return true;
                }
            }
        }
        return false;
    }

    private boolean loadUserConfiguration(final InterferometerFile fileRef, final StringBuilder sb) {
        final String filePath = fileRef.getFile();
        // indicate user config:
        fileRef.setUserConfig(true);
        fileRef.setInterferometerName(null);

        boolean loaded = false;

        final File isFile = FileUtils.getFile(filePath);
        if (isFile == null) {
            sb.append("Missing user config file [").append(filePath).append("].\n");
        } else {
            logger.info("loadUserConfiguration: loading configuration file = {}", isFile.getAbsolutePath());

            try {
                final Object loadedObject = loadConfigObject(isFile);

                // ensure correct type:
                if (loadedObject instanceof InterferometerSetting) {
                    final InterferometerSetting is = (InterferometerSetting) loadedObject;
                    is.getDescription().setFileRef(fileRef);

                    // compute checksum on file stream:
                    // final long checksumFile = checksum(fileName);
                    // update information:
                    // fileRef.setChecksumFile(checksumFile);
                    is.getDescription().setChecksumValid(false);

                    // get previously loaded configuration:
                    Configuration configuration = getUserConfigurations().getLoadedConfiguration(filePath);

                    if (configuration == null) {
                        configuration = new Configuration();
                    } else {
                        configuration.clear();
                    }

                    addInterferometerSetting(configuration, is);

                    // overwrite:
                    getUserConfigurations().addLoadedConfiguration(filePath, configuration);
                    getUserConfigurations().setChanged(true);

                    // disable other config:
                    disableOtherUserConfigurations(fileRef);

                    loaded = true;
                } else {
                    logger.error("Invalid user config file [{}] type: {}", isFile,
                            ((loadedObject != null) ? loadedObject.getClass() : null));
                    sb.append("Invalid user config file [").append(filePath).append("].\n");
                }
            } catch (IOException ioe) {
                logger.error("Load failure on {}", isFile, ioe);
                sb.append("Loading user config file [").append(filePath).append("] failed:\n").append(ioe.getMessage()).append("\n\n");
            } catch (RuntimeException re) {
                logger.error("Load failure on {}", isFile, re);
                sb.append("Loading user config file [").append(filePath).append("] failed:\n").append(re.getMessage()).append("\n\n");
            }
        }

        if (!loaded) {
            logger.info("loadUserConfiguration: missing or invalid configuration file = {}", filePath);
            // set disabled:
            fileRef.setEnabled(false);
            // Remove previously loaded configuration anyway:
            getUserConfigurations().removeLoadedConfiguration(filePath);
            getUserConfigurations().setChanged(true);
            return false;
        }
        return true;
    }

    private void disableOtherUserConfigurations(final InterferometerFile fileRef) {
        if (fileRef.isReallyEnabled() && fileRef.isValid()) {
            final String interferometerName = fileRef.getInterferometerName();

            final List<InterferometerFile> files = getUserConfigurations().getInterferometerFiles();
            if (files != null) {
                for (InterferometerFile otherFileRef : files) {
                    // only enabled and same interferometer name:
                    if ((fileRef != otherFileRef)
                            && otherFileRef.isReallyEnabled()
                            && ObjectUtils.areEquals(interferometerName, otherFileRef.getInterferometerName())) {

                        logger.debug("disableOtherUserConfigurations: disable {}", otherFileRef);

                        otherFileRef.setEnabled(false);
                    }
                }
            }
        }
    }

    private void saveUserConfigurations() {
        if (ENABLE_USER_CONFIG) {
            if (getUserConfigurations().isChanged()) {
                final Configurations conf = getUserConfigurations().getConfigurations();

                final File confFile = new File(CONF_FILE_USER);
                try {
                    logger.info("Save user configurations to file: {}", confFile);

                    saveObject(confFile, conf);

                    // reset conf changed:
                    conf.setChanged(false);

                } catch (IOException ioe) {
                    logger.error("Save failure on {}", confFile, ioe);
                } catch (RuntimeException re) {
                    logger.error("Save failure on {}", confFile, re);
                }
            }
        }
    }

    private void mergeConfigurations() {
        final Configuration configuration = getInitialConfiguration();

        // reset anyway:
        configuration.clear();

        if (ENABLE_USER_CONFIG) {
            // copy User configurations (prepared):
            final List<InterferometerFile> files = getUserConfigurations().getInterferometerFiles();
            if (files != null) {
                for (InterferometerFile fileRef : files) {
                    // only enabled:
                    if (fileRef.isReallyEnabled()) {
                        final Configuration userConfiguration = getUserConfigurations().getLoadedConfiguration(fileRef.getFile());
                        // only valid:
                        if (userConfiguration != null) {
                            logger.debug("mergeConfigurations: adding user config: {}", fileRef);

                            for (InterferometerDescription id : userConfiguration.getInterferometerDescriptions()) {
                                configuration.addInterferometerDescription(id);
                            }
                            for (InterferometerConfiguration ic : userConfiguration.getInterferometerConfigurations()) {
                                configuration.addInterferometerConfiguration(ic);
                            }
                        }
                    }
                }
            }
            logger.debug("mergeConfigurations: user: {}", configuration);
        }

        // copy missing Internal configurations (prepared):
        for (InterferometerDescription id : getInternalConfiguration().getInterferometerDescriptions()) {
            // only add configurations for missing interferometers (initial vs user config):
            if (!configuration.hasInterferometerDescription(id.getName())) {
                configuration.addInterferometerDescription(id);
            }
        }
        for (InterferometerConfiguration ic : getInternalConfiguration().getInterferometerConfigurations()) {
            final InterferometerDescription interferometer = ic.getInterferometer();
            // only add configurations for the same interferometer (initial vs user config):
            if (configuration.getInterferometerDescription(interferometer.getName()) == interferometer) {
                configuration.addInterferometerConfiguration(ic);
            }
        }

        logger.debug("mergeConfigurations: merged: {}", configuration);
    }

    /**
     * Get Initial Configuration used to resolve ID/IDREF in observation files
     * @return Initial Configuration
     */
    public Configuration getInitialConfiguration() {
        return this.initialConfiguration;
    }

    private Configuration getCurrentConfiguration() {
        return this.currentConfiguration;
    }

    /*
    ----- Configuration preparation API -----
     */
    /**
     * Computes checksum of the given file name loaded in the configuration path
     * @param fileName file name to load
     * @return checksum
     * @throws IllegalStateException if the file is not found or an I/O exception occurred
     */
    static long checksum(final String fileName) {
        final URL uri = ResourceUtils.getResource(CONF_CLASSLOADER_PATH + fileName);

        InputStream in = null;
        try {
            in = new BufferedInputStream(uri.openStream());

            return AsproConf.checksum(in);

        } catch (IOException ioe) {
            FileUtils.closeStream(in);
        }

        throw new IllegalStateException("Load failure on " + uri);
    }

    /**
     * Add a new interferometer setting in the given configuration
     * and compute transient information (long/lat and max uv coverage)
     * @param configuration configuration holder
     * @param is interferometer setting
     */
    private static void addInterferometerSetting(final Configuration configuration,
                                                 final InterferometerSetting is) {

        // process the InterferometerDescription:
        final InterferometerDescription id = is.getDescription();

        addInterferometerDescription(configuration, id);

        // process the InterferometerConfiguration list:
        for (InterferometerConfiguration ic : is.getConfigurations()) {
            addInterferometerConfiguration(configuration, ic);
        }
        // update instrument station numbers:
        computeInstrumentStationNumbers(is);
    }

    /**
     * Add a new interferometer description in the given configuration
     * and compute transient information (long/lat and max uv coverage) ...
     * @param configuration configuration holder
     * @param id interferometer description
     */
    private static void addInterferometerDescription(final Configuration configuration, final InterferometerDescription id) {
        final String name = id.getName();

        // update file ref anyway:
        if (id.getFileRef() != null) {
            id.getFileRef().setInterferometerName(name);
        }

        // check if the interferometer is unique (name) :
        if (configuration.hasInterferometerDescription(name)) {
            throw new IllegalStateException("The interferometer '" + name + "' is already present in loaded configurations !");
        }

        // TODO: handle properly spectral channels (rebinning):
        // initialize computed fields and check instrument modes (spectral channels):
        boolean dump = false;
        try {
            for (FocalInstrument instrument : id.getFocalInstruments()) {
                instrument.init(logger);
            }

            computeInterferometerLocation(id);
            computeInstrumentWaveLengthRange(id);
            adjustStationHorizons(name, id.getStations());
            associateAdaptiveOpticsSetup(id);

            configuration.addInterferometerDescription(id);

        } catch (IllegalStateException ise) {
            dump = true;
            throw ise;
        } finally {
            if (dump || DEBUG_CONF) {
                for (FocalInstrument instrument : id.getFocalInstruments()) {
                    instrument.dump(logger);
                }
            }
        }
    }

    /**
     * Add a new interferometer configuration in the given configuration
     * and compute transient information (max uv coverage) ...
     * @param configuration configuration holder
     * @param ic interferometer configuration belonging to the related interferometer description
     */
    private static void addInterferometerConfiguration(final Configuration configuration, final InterferometerConfiguration ic) {
        final String configurationName = computeAndSetConfigurationName(ic);

        // check if the interferometer is unique (name) :
        if (configuration.hasInterferometerConfiguration(configurationName)) {
            throw new IllegalStateException("The interferometer configuration '" + configurationName + "' is already present in loaded configurations !");
        }

        if (ic.getInterferometer() == null) {
            throw new IllegalStateException("The interferometer configuration '" + ic.getName()
                    + "' is not associated with an interferometer description (invalid identifier) !");
        }

        // Check switchyard references:
        final List<SwitchYard> switchyards = ic.getInterferometer().getSwitchyards();
        if (switchyards.size() > 1) {
            // Check that every interferometer configuration has a switchyard reference:
            if (ic.getSwitchyard() == null) {
                throw new IllegalStateException("The interferometer configuration '" + ic.getName()
                        + "' is not associated with a switchyard (invalid identifier) [" + switchyards + "] !");
            }
        }

        // initialize computed fields:
        computeBaselineUVWBounds(ic);

        configuration.addInterferometerConfiguration(ic);
    }

    /**
     * Compute the spherical coordinates for the interferometer
     * @param id interferometer description
     */
    private static void computeInterferometerLocation(final InterferometerDescription id) {

        // Interferometer center :
        final Position3D center = id.getPosition();
        final LonLatAlt posSph = GeocentricCoords.getLonLatAlt(center);

        if (logger.isDebugEnabled()) {
            logger.debug("Interferometer[{}] position: {}", id.getName(), posSph);
        }

        id.setPosSph(posSph);

        if (logger.isDebugEnabled()) {
            GeocentricCoords.dump(id.getName(), posSph);

            for (Station s : id.getStations()) {
                Position3D pos = s.getRelativePosition();
                logger.debug("Station[{}] norm(relativePosition) = {}", s.getName(), MathUtils.carthesianNorm(pos.getPosX(), pos.getPosY(), pos.getPosZ()));
            }
        }
    }

    /**
     * Compute the lower and upper wave length of every instrument
     * @param id interferometer description
     */
    private static void computeInstrumentWaveLengthRange(final InterferometerDescription id) {

        final AtmosphereSpectrumService atmService = AtmosphereSpectrumService.getInstance();

        final double[] lambda = new double[1];
        final double[] delta_lambda = new double[1];

        for (FocalInstrument instrument : id.getFocalInstruments()) {
            instrument.defineWaveLengthRange();

            final double lambdaMin = instrument.getWaveLengthMin();
            final double lambdaMax = instrument.getWaveLengthMax();

            lambda[0] = 0.5 * (lambdaMin + lambdaMax) * AsproConstants.MICRO_METER;
            delta_lambda[0] = (lambdaMax - lambdaMin) * AsproConstants.MICRO_METER;

            final double[] trans = atmService.getTransmission(lambda, delta_lambda);

            final double atmTrans = trans[0];

            if (logger.isDebugEnabled()) {
                logger.debug("Instrument [{}] - wavelengths [{} - {}]: mean atmosphere tranmission = {}",
                        instrument.getName(), lambdaMin, lambdaMax, atmTrans);
            }

            // Fix instrument transmission:
            for (FocalInstrumentSetup setup : instrument.getSetups()) {

                if (setup.isIncludeAtmosphereCorrection()) {
                    // hack transmission for old instruments to avoid taking into acount twice the mean atmosphere transmission
                    // (already included in instrument's transmission)

                    final double insTrans = setup.getTransmission();
                    final double correctedTrans = insTrans / atmTrans;

                    if (logger.isDebugEnabled()) {
                        logger.debug("Instrument [{}]: insTrans = {} correctedTrans = {}",
                                instrument.getName(), insTrans, correctedTrans);
                    }

                    setup.setTransmission(correctedTrans);
                }
            }
        }
    }

    /**
     * Compute the min and max number of stations using all instrument configurations of the given interferometer configurations
     * @param is interferometer setting
     */
    private static void computeInstrumentStationNumbers(final InterferometerSetting is) {
        final Map<FocalInstrument, int[]> insMinMax = new IdentityHashMap<FocalInstrument, int[]>(16);

        // for each interferometer configuration:
        for (InterferometerConfiguration intConf : is.getConfigurations()) {
            // for each instrument configuration:
            for (FocalInstrumentConfiguration insConf : intConf.getInstruments()) {
                final FocalInstrument instrument = insConf.getFocalInstrument();

                int[] minMax = insMinMax.get(instrument);
                // initialize if missing:
                if (minMax == null) {
                    minMax = new int[]{Integer.MAX_VALUE, 0};
                    insMinMax.put(instrument, minMax);
                }

                int min = minMax[0];
                int max = minMax[1];

                // for each instrument configuration:
                for (FocalInstrumentConfigurationItem c : insConf.getConfigurations()) {
                    int nChannels = c.getStations().size();

                    min = Math.min(min, nChannels);
                    max = Math.max(max, nChannels);
                }

                // update:
                minMax[0] = min;
                minMax[1] = max;
            }
        }

        for (Map.Entry<FocalInstrument, int[]> e : insMinMax.entrySet()) {
            final FocalInstrument instrument = e.getKey();
            final int[] minMax = e.getValue();

            instrument.setNumberChannelsMin(minMax[0]);
            instrument.setNumberChannelsMax(minMax[1]);

            if (logger.isDebugEnabled()) {
                logger.debug("computeInstrumentStationNumbers[{}] = [{} - {}] channels", instrument.getName(),
                        instrument.getNumberChannelsMin(), instrument.getNumberChannelsMax());
            }
        }

        if (DEBUG_CONF) {
            logger.info("computeInstrumentStationNumbers[{}] : Instruments:", is.getDescription().getName());
            for (FocalInstrument instrument : is.getDescription().getFocalInstruments()) {
                instrument.dump(logger);
            }
        }
    }

    /**
     * Compute the min and max baseline length (m) using all instrument baselines of the given interferometer configuration
     * @param intConf interferometer configuration
     */
    private static void computeBaselineUVWBounds(final InterferometerConfiguration intConf) {
        double minUV = Double.POSITIVE_INFINITY;
        double maxUV = 0d;

        final double[] minMax = new double[2];

        // for each instrument:
        for (FocalInstrumentConfiguration insConf : intConf.getInstruments()) {

            // for each instrument configuration:
            for (FocalInstrumentConfigurationItem c : insConf.getConfigurations()) {
                computeBaselineUVWBounds(c.getStations(), minMax);
                c.setMaxBaseLine(minMax[1]);

                minUV = Math.min(minUV, minMax[0]);
                maxUV = Math.max(maxUV, minMax[1]);
            }
        }

        if (logger.isDebugEnabled()) {
            logger.debug("computeBaselineUVWBounds = [{} - {}] m for configuration {}", minUV, maxUV, intConf.getName());
        }

        intConf.setMinBaseLine(minUV);
        intConf.setMaxBaseLine(maxUV);
    }

    /**
     * Compute the min and max baseline length (UVW) using all possible baselines
     * for given stations
     * @param stations list of stations to determine baselines
     * @return min - max
     */
    public static double[] computeBaselineUVWBounds(final List<Station> stations) {
        final double[] minMax = new double[2];

        computeBaselineUVWBounds(stations, minMax);

        return minMax;
    }

    /**
     * Compute the min and max baseline vector length (UVW) using all possible baselines
     * for given stations
     * @param stations list of stations to determine baselines
     * @param minMax double[min; max]
     */
    public static void computeBaselineUVWBounds(final List<Station> stations, final double[] minMax) {
        double minUV = Double.POSITIVE_INFINITY;
        double maxUV = 0d;

        final int size = stations.size();

        double x, y, z, distXYZ;
        Station s1, s2;
        for (int i = 0; i < size; i++) {
            s1 = stations.get(i);
            for (int j = i + 1; j < size; j++) {
                s2 = stations.get(j);

                x = s2.getRelativePosition().getPosX() - s1.getRelativePosition().getPosX();
                y = s2.getRelativePosition().getPosY() - s1.getRelativePosition().getPosY();
                z = s2.getRelativePosition().getPosZ() - s1.getRelativePosition().getPosZ();

                distXYZ = MathUtils.carthesianNorm(x, y, z);

                minUV = Math.min(minUV, distXYZ);
                maxUV = Math.max(maxUV, distXYZ);
            }
        }

        if (logger.isDebugEnabled()) {
            logger.debug("computeBaselineUVWBounds = [{} - {}] m for stations {}", minUV, maxUV, stations);
        }

        minMax[0] = minUV;
        minMax[1] = maxUV;
    }

    /**
     * Adjust the station horizons to respect the maximum elevation limit (85 deg for CHARA)
     * @param name interferometer name (debug)
     * @param stations station to update
     */
    private static void adjustStationHorizons(final String name, final List<Station> stations) {
        final StringBuilder sb = (DUMP_HORIZON) ? new StringBuilder(65336) : null;

        double maxElev;
        for (Station station : stations) {
            logger.debug("station: {}", station);

            Telescope tel = station.getTelescope();

            if (tel == null) {
                throw new IllegalStateException("Missing telescope reference found in the station '" + station.getName() + "' !");
            }

            // maximum elevation in degrees per telescope :
            maxElev = tel.getMaxElevation();

            if (station.getHorizon() != null && !station.getHorizon().getPoints().isEmpty()) {
                // horizon is defined : check elevation
                for (AzEl point : station.getHorizon().getPoints()) {
                    if (point.getElevation() > maxElev) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("station: {}: fix point: {}", station, point);
                        }
                        point.setElevation(maxElev);
                    }
                }

                if (DUMP_HORIZON && sb != null) {
                    sb.setLength(0);
                    sb.append("# ").append(name).append(" Station: ").append(station.getName()).append('\n');
                    sb.append("# AZ,EL\n");

                    for (AzEl point : station.getHorizon().getPoints()) {

                        sb.append(NumberUtils.trimTo3Digits(point.getAzimuth())).append(',')
                                .append(NumberUtils.trimTo3Digits(point.getElevation())).append('\n');
                    }

                    final String filePath = "/tmp/" + name + '_' + station.getName() + ".horizon";
                    logger.info("dump {} horizon to {}", station.getName(), filePath);

                    try {
                        FileUtils.writeFile(new File(filePath), sb.toString());
                    } catch (IOException ioe) {
                        logger.error("Unable to save file: {}", filePath, ioe);
                    }
                }

            } else {
                // missing horizon :
                final HorizonProfile horizon = new HorizonProfile();
                final List<AzEl> points = horizon.getPoints();

                points.add(new AzEl(360d, 0d));
                points.add(new AzEl(0d, 0d));
                points.add(new AzEl(0d, maxElev));
                points.add(new AzEl(360d, maxElev));

                logger.debug("station: {} use default horizon", station);

                // define fake horizon :
                station.setHorizon(horizon);
            }
        }
    }

    /**
     * Associate the AdaptiveOpticsSetup to their AdaptiveOptics parent
     * @param id interferometer description
     */
    private static void associateAdaptiveOpticsSetup(final InterferometerDescription id) {
        for (Telescope tel : id.getTelescopes()) {
            for (AdaptiveOptics ao : tel.getAdaptiveOptics()) {
                for (AdaptiveOpticsSetup aos : ao.getSetups()) {
                    aos.setAdaptiveOptics(ao);
                }
            }
        }
    }

    /**
     * Compute the name of the interferometer configuration according to the associated interferometer and the optional version.
     * Store this name in the interferometer configuration in the name field (xml id)
     * @param ic configuration
     * @return name of the interferometer configuration
     */
    private static String computeAndSetConfigurationName(final InterferometerConfiguration ic) {
        // compute configuration name if missing :
        String name = ic.getName();
        if (name == null) {
            name = "";

            // interferometer name is an id :
            if (ic.getInterferometer() != null) {
                name = ic.getInterferometer().getName();
            } else {
                name += "UNDEFINED";
            }

            if (ic.getVersion() != null) {
                name += " " + ic.getVersion();
            }
            ic.setName(name);
        }
        return name;
    }

    /*
    ----- Dynamic Configuration overriding API -----
     */
    /**
     * Change the current configuration by merging the initial interferometer configuration with the given extended interferometer configuration
     * @param extendedConfiguration extended interferometer configuration
     */
    public void changeConfiguration(final InterferometerConfiguration extendedConfiguration) {
        // backup configuration:
        this.previousConfiguration = getCurrentConfiguration();
        this.currentConfiguration = null; // for safety

        // use initial configuration as source:
        final Configuration configuration = getInitialConfiguration();

        final Configuration newConfiguration;

        if (extendedConfiguration != null) {
            // compute extended interferometer configuration name:
            final String configurationName = computeAndSetConfigurationName(extendedConfiguration);

            // Configuration merge (clone + update parts)
            newConfiguration = new Configuration();

            // copy InterferometerDescriptions (prepared):
            for (InterferometerDescription id : configuration.getInterferometerDescriptions()) {
                newConfiguration.addInterferometerDescription(id);
            }

            // process the InterferometerConfiguration list:
            boolean merged = false;

            for (InterferometerConfiguration ic : configuration.getInterferometerConfigurations()) {
                if (!merged && configurationName.equalsIgnoreCase(ic.getName())) {
                    merged = true;

                    logger.info("changeConfiguration: merge InterferometerConfiguration [{}]", configurationName);

                    // note: do not modify extendedConfiguration as it belongs to ObservationSetting used when marshalling to XML.
                    // note: do not modify ic as it belongs to initial configuration.
                    final InterferometerConfiguration mergedConfiguration = mergeConfiguration(ic, extendedConfiguration);

                    // add merged configuration:
                    addInterferometerConfiguration(newConfiguration, mergedConfiguration);
                } else {
                    newConfiguration.addInterferometerConfiguration(ic);
                }
            }

            if (!merged) {
                logger.info("changeConfiguration: add InterferometerConfiguration [{}]", configurationName);

                // add configuration (clone not needed):
                addInterferometerConfiguration(newConfiguration, extendedConfiguration);
            }
        } else {
            newConfiguration = configuration;
            logger.info("changeConfiguration: use initial configuration");
        }
        this.currentConfiguration = newConfiguration;
    }

    /**
     * Validate the configuration change
     * @param commit true to keep current configuration; false to use previous configuration
     */
    public void validateChangedConfiguration(final boolean commit) {
        logger.debug("validateChangedConfiguration: commit = {}", commit);

        if (!commit) {
            // restore previous configuration:
            this.currentConfiguration = this.previousConfiguration;
        }
        // reset previous configuration:
        this.previousConfiguration = null;
    }

    /**
     * Return a new merged interferometer configuration
     * @param initial initial interferometer configuration
     * @param extended extended interferometer configuration
     * @return new merged interferometer configuration
     */
    private static InterferometerConfiguration mergeConfiguration(final InterferometerConfiguration initial,
                                                                  final InterferometerConfiguration extended) {

        // duplicate initial configuration but copy list of instrument configuration:
        final InterferometerConfiguration merged = (InterferometerConfiguration) initial.clone();

        for (FocalInstrumentConfiguration insConf : extended.getInstruments()) {
            FocalInstrumentConfiguration insConfOriginal = null;
            int pos = -1;
            int i = 0;

            for (FocalInstrumentConfiguration insConfInitial : merged.getInstruments()) {
                // instrument reference equality (see AsproConfigurationIDResolver):
                if (insConfInitial.getFocalInstrument() == insConf.getFocalInstrument()) {
                    insConfOriginal = insConfInitial;
                    pos = i;
                    break;
                }
                i++;
            }

            if (insConfOriginal == null) {
                logger.info("mergeConfiguration: add FocalInstrumentConfiguration [{}]", insConf.getFocalInstrument().getName());

                // Suppose that FocalInstrumentConfiguration are valids (stations, channels, pops):
                merged.getInstruments().add(insConf);

            } else {
                logger.info("mergeConfiguration: merge FocalInstrumentConfiguration [{}]", insConf.getFocalInstrument().getName());

                final FocalInstrumentConfiguration insConfMerged = (FocalInstrumentConfiguration) insConfOriginal.clone();

                // replace by new instance:
                merged.getInstruments().remove(pos);
                merged.getInstruments().add(pos, insConfMerged);

                // for each configuration item:
                for (FocalInstrumentConfigurationItem insConfItem : insConf.getConfigurations()) {
                    FocalInstrumentConfigurationItem insConfItemOriginal = null;
                    pos = -1;
                    i = 0;

                    for (FocalInstrumentConfigurationItem insConfItemInitial : insConfMerged.getConfigurations()) {
                        if (insConfItemInitial.getName().equals(insConfItem.getName())) {
                            insConfItemOriginal = insConfItemInitial;
                            pos = i;
                            break;
                        }
                        i++;
                    }

                    if (insConfItemOriginal == null) {

                        // If the channels are undefined, try merging channels/PoPs from equivalent configuration (same stations, different order):
                        if (insConfItem.getChannels().isEmpty()) {
                            logger.debug("mergeConfiguration: try merging channels  {}", insConfItem);

                            final String stationIds = findInstrumentConfigurationStations(insConfOriginal, insConfItem.getName());

                            if (stationIds != null) {
                                insConfItemOriginal = getInstrumentConfiguration(insConfOriginal, stationIds);

                                logger.debug("mergeConfiguration: found equivalent configuration {}", insConfItemOriginal);

                                if (!insConfItemOriginal.getChannels().isEmpty()) {
                                    // handle permutations to get channels / pops:
                                    logger.info("mergeConfiguration: merge channels / pops for configuration {} with {}", insConfItem, insConfItemOriginal);

                                    mergeInstrumentConfiguration(insConfItem, insConfItemOriginal);
                                }
                            }
                        }

                        logger.info("mergeConfiguration: add {}", insConfItem);

                        insConfMerged.getConfigurations().add(insConfItem);

                    } else {
                        // if channels are defined, use the given configuration; otherwise keep original configuration:
                        if (!insConfItem.getChannels().isEmpty()) {
                            logger.info("mergeConfiguration: use given {}", insConfItem);

                            insConfMerged.getConfigurations().remove(pos);
                            insConfMerged.getConfigurations().add(pos, insConfItem);
                        } else {
                            logger.info("mergeConfiguration: ignore given {}; use {}", insConfItem, insConfItemOriginal);
                        }
                    }
                }
            }
        }

        return merged;
    }

    /**
     * Merge two instrument configuration item data (pops, beams) for matching stations
     * @param insConfItem instrument configuration item to update
     * @param insConfItemOriginal other instrument configuration item to get data
     */
    private static void mergeInstrumentConfiguration(final FocalInstrumentConfigurationItem insConfItem, final FocalInstrumentConfigurationItem insConfItemOriginal) {
        final boolean copyChannels = insConfItem.getChannels().isEmpty() && !insConfItemOriginal.getChannels().isEmpty();
        final boolean copyPops = insConfItem.getPops().isEmpty() && !insConfItemOriginal.getPops().isEmpty();

        // for each station, merge beams / pop data:
        for (int i = 0, size = insConfItem.getStations().size(); i < size; i++) {
            final Station station = insConfItem.getStations().get(i);

            // find corresponding station in the other instrument configuration item (station order may be different):
            int pos = -1;
            for (int j = 0; j < size; j++) {
                if (station.getName().equalsIgnoreCase(insConfItemOriginal.getStations().get(j).getName())) {
                    pos = j;
                    break;
                }
            }

            if (pos != -1) {
                if (copyChannels) {
                    // copy channel information:
                    insConfItem.getChannels().add(insConfItemOriginal.getChannels().get(pos));
                }
                if (copyPops) {
                    // copy PoPs information:
                    insConfItem.getPops().add(insConfItemOriginal.getPops().get(pos));
                }
            }
        }
    }

    /*
    ----- InterferometerDescription API -----
     */
    /**
     * Return the interferometer description for the given name
     * @param name interferometer name
     * @return interferometer description or null if not found
     */
    public InterferometerDescription getInterferometerDescription(final String name) {
        return getCurrentConfiguration().getInterferometerDescription(name);
    }

    /**
     * Return the list of all interferometer names
     * @return list of all interferometer names
     */
    public Vector<String> getInterferometerNames() {
        return getCurrentConfiguration().getInterferometerNames();
    }

    /**
     * Return the list of interferometer configurations associated to the given interferometer
     * @param interferometerName name of the interferometer
     * @return list of interferometer configurations
     */
    public Vector<String> getInterferometerConfigurationNames(final String interferometerName) {
        final Vector<String> names = getCurrentConfiguration().getInterferometerConfigurationNames(interferometerName);
        return (names != null) ? names : EMPTY_VECTOR;
    }

    /**
     * Return the switchyard links for the given station
     * @param sw interferometer switchyard
     * @param station station
     * @return switchyard links or null
     */
    public StationLinks getStationLinks(final SwitchYard sw, final Station station) {
        if (sw != null) {
            for (StationLinks sl : sw.getStationLinks()) {
                if (sl.getStation().equals(station)) {
                    return sl;
                }
            }
        }
        return null;
    }

    /**
     * Return the optional list of PoPs given the interferometer
     * @param interferometerName name of the interferometer
     * @return optional list of PoPs or null
     */
    public List<Pop> getPoPs(final String interferometerName) {
        final InterferometerDescription id = getInterferometerDescription(interferometerName);
        if (id != null) {
            return id.getPops();
        }
        return null;
    }

    /**
     * Return the optional list of stations having PoPs given the interferometer
     * @param interferometerName name of the interferometer
     * @return optional list of Station or null
     */
    public List<Station> getStationsWithPoPs(final String interferometerName) {
        final InterferometerDescription id = getInterferometerDescription(interferometerName);
        if (id != null) {
            final List<Station> stations = new ArrayList<>(6);

            for (Station station : id.getStations()) {
                if (!station.getPopLinks().isEmpty()) {
                    stations.add(station);
                }
            }
            return stations;
        }
        return null;
    }

    /**
     * Return if the given interferometer has wind pointing restriction
     * @param interferometerName name of the interferometer
     * @return the wind pointing restriction in degrees or null
     */
    public Double getWindPointingRestriction(final String interferometerName) {
        final InterferometerDescription id = getInterferometerDescription(interferometerName);
        if (id != null) {
            return id.getWindPointingRestriction();
        }
        return null;
    }

    /*
    ----- InterferometerConfiguration API -----
     */
    /**
     * Return the interferometer configuration collection
     * @return interferometer configuration collection
     */
    private Collection<InterferometerConfiguration> getInterferometerConfigurations() {
        return getCurrentConfiguration().getInterferometerConfigurations();
    }

    /**
     * Return the interferometer configuration for the given name
     * @param configurationName name of the interferometer configuration
     * @return interferometer configuration or null if not found
     */
    public InterferometerConfiguration getInterferometerConfiguration(final String configurationName) {
        return getCurrentConfiguration().getInterferometerConfiguration(configurationName);
    }

    /**
     * Return the first instrument configuration having the given instrument name
     * @param instrumentAlias name or alias of the instrument
     * @return interferometer configuration or null if not found
     */
    public InterferometerConfiguration getInterferometerConfigurationWithInstrument(final String instrumentAlias) {
        for (InterferometerConfiguration c : getInterferometerConfigurations()) {
            for (FocalInstrumentConfiguration ic : c.getInstruments()) {
                // match alias or name:
                if (ic.getFocalInstrument().getAliasOrName().equals(instrumentAlias)) {
                    return c;
                }
            }
        }
        return null;
    }

    /**
     * Return the list of all instrument names available for the given configuration
     * @param configurationName name of the interferometer configuration
     * @return list of all instrument names
     */
    public Vector<String> getInterferometerInstrumentNames(final String configurationName) {
        final InterferometerConfiguration c = getInterferometerConfiguration(configurationName);
        if (c != null) {
            final Vector<String> v = new Vector<String>(c.getInstruments().size());
            for (FocalInstrumentConfiguration ic : c.getInstruments()) {
                // use alias or name:
                v.add(ic.getFocalInstrument().getAliasOrName());
            }
            return v;
        }
        return EMPTY_VECTOR;
    }

    /**
     * Return the instrument configuration for the given interferometer configuration and instrument name
     * @param configurationName name of the interferometer configuration
     * @param instrumentAlias name or alias of the instrument
     * @return focal instrument configuration
     */
    public FocalInstrumentConfiguration getInterferometerInstrumentConfiguration(final String configurationName, final String instrumentAlias) {
        final InterferometerConfiguration c = getInterferometerConfiguration(configurationName);
        if (c != null) {
            for (FocalInstrumentConfiguration ic : c.getInstruments()) {
                // match alias or name:
                if (ic.getFocalInstrument().getAliasOrName().equals(instrumentAlias)) {
                    return ic;
                }
            }
        }
        return null;
    }

    /**
     * Return the instrument configuration for the given interferometer configuration and instrument name using alternative names
     * @param configurationName name of the interferometer configuration
     * @param instrumentName name of the instrument
     * @return focal instrument configuration or null if no match
     */
    public FocalInstrumentConfiguration getInterferometerInstrumentConfigurationByAltNames(final String configurationName, final String instrumentName) {
        final InterferometerConfiguration c = getInterferometerConfiguration(configurationName);
        if (c != null) {
            for (FocalInstrumentConfiguration ic : c.getInstruments()) {
                // match name on altNames:
                if (ic.getFocalInstrument().getAltNames().contains(instrumentName)) {
                    return ic;
                }
            }
        }
        return null;
    }

    /**
     * Return the instrument for the given interferometer configuration and instrument name
     * @param configurationName name of the interferometer configuration
     * @param instrumentAlias name or alias of the instrument
     * @return focal instrument
     */
    public FocalInstrument getInterferometerInstrument(final String configurationName, final String instrumentAlias) {
        final FocalInstrumentConfiguration ic = getInterferometerInstrumentConfiguration(configurationName, instrumentAlias);
        if (ic != null) {
            return ic.getFocalInstrument();
        }
        return null;
    }

    /**
     * Return the first instrument for the given instrument name prefix and mode (in the first instrument configuration)
     * @param instrumentPrefix prefix for the name or alias of the instrument
     * @param instrumentMode name or alias of the instrument
     * @return focal instrument
     */
    public FocalInstrument getInterferometerInstrumentByMode(final String instrumentPrefix, final String instrumentMode) {
        for (InterferometerConfiguration c : getInterferometerConfigurations()) {
            for (FocalInstrumentConfiguration ic : c.getInstruments()) {
                final FocalInstrument ins = ic.getFocalInstrument();
                // match alias or name:
                if (ins.getAliasOrName().startsWith(instrumentPrefix)
                        && getInstrumentMode(ins, instrumentMode) != null) {
                    return ins;
                }
            }
        }
        return null;
    }

    /**
     * Return the list of all instrument configuration names for the given interferometer configuration and instrument name
     * @param configurationName name of the interferometer configuration
     * @param instrumentAlias name or alias of the instrument
     * @return list of all instrument configuration names
     */
    public Vector<String> getInstrumentConfigurationNames(final String configurationName, final String instrumentAlias) {
        final FocalInstrumentConfiguration ic = getInterferometerInstrumentConfiguration(configurationName, instrumentAlias);
        if (ic != null) {
            final Vector<String> v = new Vector<String>(ic.getConfigurations().size());
            for (FocalInstrumentConfigurationItem c : ic.getConfigurations()) {
                v.add(c.getName());
            }
            return v;
        }
        return EMPTY_VECTOR;
    }

    /**
     * Return the list of all instrument configuration alternative names for the given interferometer configuration and instrument name
     * @param configurationName name of the interferometer configuration
     * @param instrumentAlias name or alias of the instrument
     * @return list of all instrument configuration alternative names
     */
    public Vector<String> getInstrumentConfigurationDisplayAltNames(final String configurationName, final String instrumentAlias) {
        final FocalInstrumentConfiguration ic = getInterferometerInstrumentConfiguration(configurationName, instrumentAlias);
        if (ic != null) {
            final Vector<String> v = new Vector<String>(ic.getConfigurations().size());
            for (FocalInstrumentConfigurationItem c : ic.getConfigurations()) {
                // may be null
                v.add(c.getDisplayAltName());
            }
            return v;
        }
        return EMPTY_VECTOR;
    }

    /**
     * Return the instrument configuration alternative baselines for the given interferometer configuration and instrument name
     * @param configurationName name of the interferometer configuration
     * @param instrumentAlias name or alias of the instrument
     * @param confName configuration name (stations)
     * @return instrument configuration alternative name or null
     */
    public String getInstrumentConfigurationAltBaselines(final String configurationName, final String instrumentAlias, final String confName) {
        final FocalInstrumentConfiguration ic = getInterferometerInstrumentConfiguration(configurationName, instrumentAlias);
        if (ic != null) {
            for (FocalInstrumentConfigurationItem c : ic.getConfigurations()) {
                // may be null
                if (confName.equals(c.getName())) {
                    return c.getAltBaselines();
                }
            }
        }
        return null;
    }

    /**
     * Return the instrument configuration item for the given interferometer configuration, instrument name and instrument configuration
     * @param insConf instrument configuration
     * @param instrumentConfigurationName name of the instrument configuration
     * @return instrument configuration item
     */
    public static FocalInstrumentConfigurationItem getInstrumentConfiguration(final FocalInstrumentConfiguration insConf, final String instrumentConfigurationName) {
        if (insConf != null) {
            for (FocalInstrumentConfigurationItem c : insConf.getConfigurations()) {
                if (c.getName().equals(instrumentConfigurationName)) {
                    return c;
                }
            }
        }
        return null;
    }

    /**
     * Return the instrument configuration item for the given interferometer configuration, instrument name and instrument configuration
     * @param configurationName name of the interferometer configuration
     * @param instrumentAlias name or alias of the instrument
     * @param instrumentConfigurationName name of the instrument configuration
     * @return list of instrument channels or null if undefined
     */
    public FocalInstrumentConfigurationItem getInstrumentConfiguration(final String configurationName, final String instrumentAlias, final String instrumentConfigurationName) {
        final FocalInstrumentConfiguration ic = getInterferometerInstrumentConfiguration(configurationName, instrumentAlias);
        if (ic != null) {
            final FocalInstrumentConfigurationItem c = getInstrumentConfiguration(ic, instrumentConfigurationName);
            if (c != null) {
                return c;
            }
        }
        return null;
    }

    /**
     * Return the list of stations for the given interferometer configuration, instrument name and instrument configuration
     * @param insConf instrument configuration
     * @param instrumentConfigurationName name of the instrument configuration
     * @return list of stations
     */
    public static List<Station> getInstrumentConfigurationStations(final FocalInstrumentConfiguration insConf, final String instrumentConfigurationName) {
        final FocalInstrumentConfigurationItem c = getInstrumentConfiguration(insConf, instrumentConfigurationName);
        if (c != null) {
            return c.getStations();
        }
        return null;
    }

    /**
     * Return the max baseline for the given interferometer configuration, instrument configuration
     * @param insConf instrument configuration
     * @param instrumentConfigurationName name of the instrument configuration
     * @return list of stations
     */
    public static double getInstrumentConfigurationMaxBaseline(final FocalInstrumentConfiguration insConf, final String instrumentConfigurationName) {
        final FocalInstrumentConfigurationItem c = getInstrumentConfiguration(insConf, instrumentConfigurationName);
        if (c != null) {
            return c.getMaxBaseLine();
        }
        return Double.NaN;
    }

    /**
     * Return the list of stations for the given interferometer configuration, instrument name and instrument configuration
     * @param configurationName name of the interferometer configuration
     * @param instrumentAlias name or alias of the instrument
     * @param instrumentConfigurationName name of the instrument configuration
     * @return list of stations
     */
    public List<Station> getInstrumentConfigurationStations(final String configurationName, final String instrumentAlias, final String instrumentConfigurationName) {
        final FocalInstrumentConfiguration ic = getInterferometerInstrumentConfiguration(configurationName, instrumentAlias);
        if (ic != null) {
            return getInstrumentConfigurationStations(ic, instrumentConfigurationName);
        }
        return null;
    }

    /**
     * Return the (optional) list of instrument channels for the given interferometer configuration, instrument name and instrument configuration
     * @param configurationName name of the interferometer configuration
     * @param instrumentAlias name or alias of the instrument
     * @param instrumentConfigurationName name of the instrument configuration
     * @return list of instrument channels or null if undefined
     */
    public List<Channel> getInstrumentConfigurationChannels(final String configurationName, final String instrumentAlias, final String instrumentConfigurationName) {
        final FocalInstrumentConfigurationItem c = getInstrumentConfiguration(configurationName, instrumentAlias, instrumentConfigurationName);
        if (c != null) {
            return c.getChannels();
        }
        return null;
    }

    /**
     * Return the (optional) list of delay lines for the given interferometer configuration, instrument name and instrument configuration
     * @param configurationName name of the interferometer configuration
     * @param instrumentAlias name or alias of the instrument
     * @param instrumentConfigurationName name of the instrument configuration
     * @return list of delay lines or null if undefined
     */
    public List<DelayLine> getInstrumentConfigurationDelayLines(final String configurationName, final String instrumentAlias, final String instrumentConfigurationName) {
        final FocalInstrumentConfigurationItem c = getInstrumentConfiguration(configurationName, instrumentAlias, instrumentConfigurationName);
        if (c != null) {
            return c.getDelayLines();
        }
        return null;
    }

    /**
     * Return the (optional) list of Pops for the given interferometer configuration, instrument name and instrument configuration
     * @param configurationName name of the interferometer configuration
     * @param instrumentAlias name or alias of the instrument
     * @param instrumentConfigurationName name of the instrument configuration
     * @return list of PoPs or null if undefined
     */
    public List<Pop> getInstrumentConfigurationPoPs(final String configurationName, final String instrumentAlias, final String instrumentConfigurationName) {
        final FocalInstrumentConfigurationItem c = getInstrumentConfiguration(configurationName, instrumentAlias, instrumentConfigurationName);
        if (c != null) {
            return c.getPops();
        }
        return null;
    }

    /**
     * Return the list of all adaptive optics setups available for the given interferometer configuration and instrument name
     * @param configurationName name of the interferometer configuration
     * @param instrumentAlias name or alias of the instrument
     * @param instrumentConfigurationName name of the instrument configuration
     * @return list of all adaptive optics setups
     */
    public Vector<String> getAdaptiveOpticsSetups(final String configurationName, final String instrumentAlias, final String instrumentConfigurationName) {
        final FocalInstrumentConfiguration ic = getInterferometerInstrumentConfiguration(configurationName, instrumentAlias);
        if (ic != null) {
            final FocalInstrumentConfigurationItem c = getInstrumentConfiguration(ic, instrumentConfigurationName);
            if (c != null) {
                final FocalInstrument instrument = ic.getFocalInstrument();
                final double lambda = 0.5 * (instrument.getWaveLengthMin() + instrument.getWaveLengthMax());
                final SpectralBand insBand = SpectralBandUtils.findBand(Band.findBand(lambda));

                final Set<Telescope> telescopes = new HashSet<Telescope>();
                for (Station station : c.getStations()) {
                    telescopes.add(station.getTelescope());
                }

                final Vector<String> v = new Vector<String>(telescopes.size() * 2);
                for (Telescope tel : telescopes) {
                    for (AdaptiveOptics ao : tel.getAdaptiveOptics()) {

                        if (ao.getInstrumentBand() != null) {
                            // check wavelength range
                            if (ao.getInstrumentBand() != insBand) {
                                logger.debug("skip {} band {} <> band {}", ao.getName(), ao.getInstrumentBand(), insBand);
                                continue;
                            }
                        }

                        for (AdaptiveOpticsSetup aos : ao.getSetups()) {
                            v.add(aos.getName());
                        }
                    }
                }
                return v;
            }
        }
        return EMPTY_VECTOR;
    }

    /**
     * Return the list of all fringe tracker modes available for the given interferometer configuration and instrument name
     * @param configurationName name of the interferometer configuration
     * @param instrumentName name of the instrument
     * @return list of all fringe tracker modes
     */
    public Vector<String> getFringeTrackerModes(final String configurationName, final String instrumentName) {
        final FocalInstrument ins = getInterferometerInstrument(configurationName, instrumentName);
        if (ins != null) {
            final boolean ftOptional = (ins.isFringeTrackerRequired() == null || !ins.isFringeTrackerRequired().booleanValue());
            final FringeTracker ft = ins.getFringeTracker();
            if (ft != null) {
                final Vector<String> v = new Vector<String>(ft.getModes().size() + ((ftOptional) ? 1 : 0));
                if (ftOptional) {
                    v.add(AsproConstants.NONE);
                }
                v.addAll(ft.getModes());
                return v;
            }
        }
        return EMPTY_VECTOR;
    }

    /**
     * Return the default sampling time for the given interferometer configuration and instrument name
     * @param configurationName name of the interferometer configuration
     * @param instrumentName name of the instrument
     * @return default sampling time
     */
    public int getInstrumentSamplingTime(final String configurationName, final String instrumentName) {
        final FocalInstrument ins = getInterferometerInstrument(configurationName, instrumentName);
        if (ins != null) {
            if (ins.getSetups() != null) {
                // TODO: use appropriate setup corresponding to the selected instrument mode (useless for now)
                return ins.getSetups().get(0).getDefaultSamplingTime();
            }
        }
        return -1;
    }

    /**
     * Return the default total integration time for the given interferometer configuration and instrument name
     * @param configurationName name of the interferometer configuration
     * @param instrumentName name of the instrument
     * @return default total integration time
     */
    public int getInstrumentTotalIntegrationTime(final String configurationName, final String instrumentName) {
        final FocalInstrument ins = getInterferometerInstrument(configurationName, instrumentName);
        if (ins != null) {
            if (ins.getSetups() != null) {
                // TODO: use appropriate setup corresponding to the selected instrument mode (useless for now)
                return ins.getSetups().get(0).getDefaultTotalIntegrationTime();
            }
        }
        return -1;
    }

    /**
     * Return the list of all instrument modes (spectral configuration) for the given interferometer configuration and instrument name
     * @param configurationName name of the interferometer configuration
     * @param instrumentName name of the instrument
     * @return list of all instrument modes
     */
    public Vector<String> getInstrumentModes(final String configurationName, final String instrumentName) {
        final FocalInstrument ins = getInterferometerInstrument(configurationName, instrumentName);
        if (ins != null) {
            final Vector<String> v = new Vector<String>(ins.getModes().size());
            for (FocalInstrumentMode m : ins.getModes()) {
                // handle modes dedicated to a specific version:
                if (m.getVersion() == null || configurationName.endsWith(m.getVersion())) {
                    v.add(m.getName());
                }
            }
            return v;
        }
        return EMPTY_VECTOR;
    }

    /**
     * Return the instrument mode for the given interferometer configuration, instrument name and mode
     * @param configurationName name of the interferometer configuration
     * @param instrumentName name of the instrument
     * @param instrumentMode instrument mode
     * @return instrument mode or null
     */
    public FocalInstrumentMode getInstrumentMode(final String configurationName, final String instrumentName, final String instrumentMode) {
        if (instrumentMode != null && instrumentMode.length() > 0) {
            final FocalInstrument ins = getInterferometerInstrument(configurationName, instrumentName);
            return getInstrumentMode(ins, instrumentMode);
        }
        return null;
    }

    /**
     * Return the instrument mode for the given instrument and mode
     * @param instrument instrument
     * @param instrumentMode instrument mode
     * @return instrument mode or null
     */
    public FocalInstrumentMode getInstrumentMode(final FocalInstrument instrument, final String instrumentMode) {
        if (instrument != null) {
            for (FocalInstrumentMode m : instrument.getModes()) {
                if (m.getName().equals(instrumentMode)) {
                    return m;
                }
            }
        }
        return null;
    }

    /**
     * Parse and return the list of PoPs for the given interferometer configuration, instrument name, Stations and Pops string.
     * The Pops string must only contain PoP indexes like '12', '111' or '541'.
     * The length of the Pops string must respect the number of stations of the instrument.
     * @param configurationName name of the interferometer configuration
     * @param stations Stations string
     * @param inputPoPs Pops string
     * @return list of PoPs or null if invalid
     */
    public List<Pop> parseInstrumentPoPs(final String configurationName,
                                         final String stations,
                                         final String inputPoPs) {
        return parseInstrumentPoPs(configurationName, stations, inputPoPs, null, null);
    }

    /**
     * Parse and return the list of PoPs for the given interferometer configuration, instrument name, Stations and Pops string.
     * The Pops string must only contain PoP indexes like '12', '111' or '541'.
     * The length of the Pops string must respect the number of stations of the instrument.
     * @param configurationName name of the interferometer configuration
     * @param stations Stations string
     * @param inputPoPs Pops string
     * @param prevStations previous stations corresponding to the Pops string to convert to the new Stations string
     * @param mappingFixed optional fixed mapping (station = PoP)
     * @return list of PoPs or null if invalid
     */
    public List<Pop> parseInstrumentPoPs(final String configurationName,
                                         final String stations,
                                         final String inputPoPs,
                                         final String prevStations,
                                         final Map<String, Pop> mappingFixed) {

        if (logger.isDebugEnabled()) {
            logger.debug("parseInstrumentPoPs: [{}] for {} + {} @ {} (prev @ {})", inputPoPs, configurationName,
                    stations, prevStations);
        }

        if (StringUtils.isEmpty(stations) || StringUtils.isEmpty(inputPoPs)) {
            return null;
        }

        final InterferometerConfiguration ic = getInterferometerConfiguration(configurationName);
        if (ic != null) {
            // number of stations :
            final String[] splitStaNames = stations.split(" ");
            final int nStations = splitStaNames.length;

            String configPoPs = inputPoPs;

            // Force reorder or use wildcard (8)
            // try parsing mapping [pop <=> telescope]

            /*
            [551511] for CHARA + MIRC_5T @ S1 S2 W1 W2 E1 (prev @ S1 S2 W1 W2 E1 E2)
             */
            final String[] splitPrevStaNames = (prevStations != null) ? prevStations.split(" ") : null;

            if ((splitPrevStaNames != null) && (splitPrevStaNames.length == inputPoPs.length())) {
                final Map<String, String> mapping = new HashMap<String, String>(8);

                final char[] pops = inputPoPs.toCharArray();

                for (int i = 0; i < splitPrevStaNames.length; i++) {
                    final String staName = splitPrevStaNames[i];
                    final char pop = pops[i];
                    mapping.put(staName, Character.toString(pop));
                }

                final StringBuilder sbPops = new StringBuilder(nStations);

                for (int i = 0; i < splitStaNames.length; i++) {
                    final String staName = splitStaNames[i];
                    // priority: fixed first, then previous mapping:
                    String pop = null;
                    if (mappingFixed != null) {
                        pop = Pop.getIndexAsString(mappingFixed.get(staName));
                    }
                    if (pop == null) {
                        pop = mapping.get(staName);
                    }
                    sbPops.append((pop != null) ? pop : Pop.ANY_POP_S); // '8' means (matching all) !
                }
                configPoPs = sbPops.toString();

                logger.debug("Fixed PoPs to: {} for {}", configPoPs, stations);
            }

            if (configPoPs.length() == nStations) {
                // valid length
                final List<Pop> listPoPs = ic.getInterferometer().getPops();
                final List<Pop> config = new ArrayList<Pop>(nStations);

                int nPops = 0;
                for (char ch : configPoPs.toCharArray()) {
                    // should accept '8' (matching all) ?
                    final int idx = Character.digit(ch, 10);
                    if (idx < 0) {
                        return null;
                    }
                    Pop p = null;
                    // '8' means (matching all) :
                    if (idx != Pop.ANY_POP_I) {
                        for (Pop pop : listPoPs) {
                            if (pop.getIndex() == idx) {
                                p = pop;
                                nPops++;
                                break;
                            }
                        }
                    }
                    config.add(p);
                }
                // check if all given numbers are valid (16 is invalid !) :
                if ((nPops != 0) && (config.size() == nStations)) {
                    return config;
                }
            }
        }
        return null;
    }

    /**
     * Try to find an invalid instrument configuration (A0 B0 C0) by testing all possible permutation of the stations.
     * This problem that can happen if
     * - ESO CfP changes
     * - we have errors in our configuration files
     *
     * for example : A0 B0 C0 is equivalent to C0 B0 A0
     *
     * @param insConf instrument configuration
     * @param stationConf stations (A0 B0 C0)
     * @return correct value for stations (C0 B0 A0) or null if no match found
     */
    public static String findInstrumentConfigurationStations(final FocalInstrumentConfiguration insConf, final String stationConf) {

        // trim to be sure (xml manually modified) :
        final String stationNames = stationConf.trim();

        // A0 B0 C0 is equivalent to C0 B0 A0
        final String[] stations = stationNames.split(" ");

        // number of stations in the given string:
        final int nStation = stations.length;

        // Avoid too many permutations
        if (nStation > MAX_NUM_STATIONS) {
            return null;
        }

        final FocalInstrument instrument = insConf.getFocalInstrument();

        if ((nStation < instrument.getNumberChannelsMin()) || (nStation > instrument.getNumberChannelsMax())) {
            if (logger.isDebugEnabled()) {
                logger.debug("Incompatible configuration [{}] for instrument {}", stationConf, instrument.getName());
            }
            return null;
        }

        // generate station combinations (indexes) :
        final List<int[]> iStations = CombUtils.generatePermutations(nStation);

        final StringBuilder sb = new StringBuilder(nStation * 4);

        int[] idx;
        // skip first permutation as it is equivalent to stationNames :
        for (int i = 1, j, size = iStations.size(); i < size; i++) {
            idx = iStations.get(i);

            for (j = 0; j < nStation; j++) {
                if (j > 0) {
                    sb.append(' ');
                }
                sb.append(stations[idx[j]]);
            }

            final String stationIds = sb.toString();
            // recycle :
            sb.setLength(0);

            if (logger.isDebugEnabled()) {
                logger.debug("trying instrument configuration: {}", stationIds);
            }

            // find station list corresponding to the station ids :
            final List<Station> stationList = getInstrumentConfigurationStations(insConf, stationIds);

            if (stationList != null) {
                return stationIds;
            }
        }
        return null;
    }

}
