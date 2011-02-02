/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservationManager.java,v 1.56 2011-02-02 17:42:50 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.55  2011/01/31 15:29:13  bourgesl
 * use WarningContainerEvent instead of shared warning in observation
 * modified fireWarningsReady(warningContainer) to use WarningContainerEvent
 *
 * Revision 1.54  2011/01/31 13:30:38  bourgesl
 * add and use setObservation and setObservationFile to clarify code
 * at initialisation, it does not create an observation anymore : use load or reset to have an observation and send observation events to observation listeners
 * updated java doc
 *
 * Revision 1.53  2011/01/28 16:32:36  mella
 * Add new observationEvents (CHANGED replaced by DO_UPDATE, REFRESH and REFRESH_UV)
 * Modify the observationListener interface
 *
 * Revision 1.52  2011/01/27 17:11:10  bourgesl
 * updated listeners in javadoc
 *
 * Revision 1.51  2011/01/26 17:22:04  bourgesl
 * added many comments (usages / listeners for events)
 * refactored version changes
 *
 * Revision 1.50  2011/01/25 12:29:37  bourgesl
 * fixed javadoc errors
 *
 * Revision 1.49  2011/01/21 16:22:38  bourgesl
 * fixed bug when loading files (target list are not refreshed) : use targetChangedEvent in changeObservation instead of changedEvent to indicate swing panels to refresh their target list
 * reset version to 1 in changeObservation
 * increment version in fireObservationRefresh
 *
 * Revision 1.48  2010/12/17 15:15:13  bourgesl
 * API simplifications
 *
 * Revision 1.47  2010/12/14 09:27:26  bourgesl
 * major refactoring to fire target change events on add/remove target/calibrator operations
 *
 * Revision 1.46  2010/12/10 17:15:40  bourgesl
 * added getDisplayTargets and modified how to update target user informations (copy)
 *
 * Revision 1.45  2010/11/30 15:52:25  bourgesl
 * added ObservationFileProcessor onLoad and onSave callbacks to check schema version and handle model conversion if needed
 *
 * Revision 1.44  2010/11/25 07:59:23  bourgesl
 * updated data model to add target user informations (description, calibrator flag, calibrator list per target ...)
 * support cloneable
 *
 * Revision 1.43  2010/11/23 16:55:43  bourgesl
 * removed import
 *
 * Revision 1.42  2010/11/19 16:55:59  bourgesl
 * remove replaceTarget
 *
 * Revision 1.41  2010/10/14 13:22:02  bourgesl
 * use CopyOnWriteArrayList for observation listeners
 *
 * Revision 1.40  2010/10/07 15:02:05  bourgesl
 * added load(reader) and fireEvent checks that the current thread is EDT
 *
 * Revision 1.39  2010/10/04 16:25:39  bourgesl
 * proper JAXB / IO exception handling
 *
 * Revision 1.38  2010/10/01 15:36:29  bourgesl
 * new event WARNING_READY
 * added setWarningContainer and fireWarningReady methods
 *
 * Revision 1.37  2010/09/26 12:47:40  bourgesl
 * better exception handling
 *
 * Revision 1.36  2010/09/20 14:45:36  bourgesl
 * better method ordering
 * removed static methods getTarget(observation ...) and getTargetConfiguration(observation ...)
 * javadoc updated
 *
 * Revision 1.35  2010/09/01 12:59:07  bourgesl
 * added fixInstrumentConfigurationStations to load files where the station list has a wrong ordering
 *
 * Revision 1.34  2010/07/22 15:45:43  bourgesl
 * added acquisition time in UV coverage and observation
 *
 * Revision 1.33  2010/07/22 14:31:55  bourgesl
 * added atmosphere quality in WhenSetting (optional)
 * added getSeeing() in AtmosphereQualityUtils
 * update ObservationManager
 *
 * Revision 1.32  2010/07/07 15:11:09  bourgesl
 * better defaultValues with minimal observation information (interferometer, instrument, stations ...)
 * new reset method to reset the GUI
 *
 * Revision 1.31  2010/06/23 12:53:48  bourgesl
 * added setOIFitsFile method and fire OIFits done event
 *
 * Revision 1.30  2010/06/17 10:02:51  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.29  2010/06/10 08:54:30  bourgesl
 * added time monitoring for fireEvent method
 *
 * Revision 1.28  2010/05/06 15:37:27  bourgesl
 * added TargetConfiguration to gather HA Min/Max, FT Mode and other future target related configuration
 *
 * Revision 1.27  2010/04/09 10:33:10  bourgesl
 * convert space character in simbad coords by double dot to have proper HMS / DMS format
 *
 * Revision 1.26  2010/04/09 10:21:09  bourgesl
 * modified Target type :
 * - RA/DEC in HMS/DMS
 * - added radial velocity SYSVEL VELDEF
 * - added object types OBJTYP
 * - added identifiers IDS
 *
 * Revision 1.25  2010/04/08 14:07:21  bourgesl
 * comments
 *
 * Revision 1.24  2010/04/02 14:39:34  bourgesl
 * javadoc
 *
 * Revision 1.23  2010/02/17 15:12:40  bourgesl
 * format
 *
 * Revision 1.22  2010/02/16 14:47:38  bourgesl
 * added replaceTarget and setTargets methods to update the edited models
 *
 * Revision 1.21  2010/02/08 17:00:35  bourgesl
 * moved several reference checks to ObservationManager
 *
 * Revision 1.20  2010/02/03 09:48:53  bourgesl
 * target model uvmap added on the uv coverage with zooming supported
 *
 * Revision 1.19  2010/01/20 16:18:37  bourgesl
 * observation form refactoring
 *
 * Revision 1.18  2010/01/15 16:13:16  bourgesl
 * added sampling periodicity
 *
 * Revision 1.17  2010/01/15 13:50:17  bourgesl
 * added logs on setters
 * supports instrumentMode is null
 *
 * Revision 1.16  2010/01/14 17:04:15  bourgesl
 * added instrument mode + min elevation in the observation settings
 *
 * Revision 1.15  2010/01/12 17:10:08  bourgesl
 * less log INFO outputs
 *
 * Revision 1.14  2010/01/08 16:51:18  bourgesl
 * initial uv coverage
 *
 * Revision 1.13  2010/01/04 15:42:47  bourgesl
 * added missing fields in Target : proper motion, parallax, magnitudes and spectral types (cds raw data)
 *
 * Revision 1.12  2009/12/15 16:32:44  bourgesl
 * added user PoP configuration based on PoP indices
 *
 * Revision 1.11  2009/12/07 15:18:00  bourgesl
 * Load observation action now refreshes the observation form completely
 *
 * Revision 1.10  2009/12/04 16:26:58  bourgesl
 * Added Load action in the menu bar (partially handled)
 *
 * Revision 1.9  2009/12/04 15:38:27  bourgesl
 * Added Save action in the menu bar
 *
 * Revision 1.8  2009/11/26 17:04:11  bourgesl
 * added observability plots options (night/detail / UTC/LST)
 * added base line limits
 *
 * Revision 1.7  2009/11/17 17:00:28  bourgesl
 * chosen instrument configuration propagated to observation
 *
 * Revision 1.6  2009/11/05 12:59:39  bourgesl
 * first simple source observability (only min elevation condition)
 *
 * Revision 1.5  2009/11/03 16:57:55  bourgesl
 * added observability plot with LST/UTC support containing only day/night/twilight zones
 *
 * Revision 1.4  2009/10/22 15:47:22  bourgesl
 * beginning of observability computation with jSkyCalc
 *
 * Revision 1.3  2009/10/20 15:50:16  bourgesl
 * add a target once simbad data are back
 *
 * Revision 1.2  2009/10/20 13:08:51  bourgesl
 * ObservationManager has methods to store observation properties
 *
 * Revision 1.1  2009/10/14 15:54:38  bourgesl
 * added basicObservationForm + CHARA.xml
 *
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.event.ObservationListener;
import fr.jmmc.aspro.model.event.ObservationEventType;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.event.ObservabilityEvent;
import fr.jmmc.aspro.model.event.ObservationEvent;
import fr.jmmc.aspro.model.event.UpdateObservationEvent;
import fr.jmmc.aspro.model.event.WarningContainerEvent;
import fr.jmmc.aspro.model.oi.AtmosphereQuality;
import fr.jmmc.aspro.model.oi.FocalInstrumentConfigurationChoice;
import fr.jmmc.aspro.model.oi.InterferometerConfigurationChoice;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.model.oi.WhenSetting;
import fr.jmmc.aspro.util.CombUtils;
import fr.jmmc.mcs.astro.star.Star;
import fr.jmmc.oitools.model.OIFitsFile;
import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.logging.Level;
import javax.swing.SwingUtilities;
import javax.xml.datatype.XMLGregorianCalendar;

/**
 * This class manages observation files i.e. user defined observation settings
 *
 * @author bourgesl
 */
public final class ObservationManager extends BaseOIManager {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.model.ObservationManager";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** flag to log a stack trace in method fireEvent() to debug events */
  private final static boolean DEBUG_FIRE_EVENT = false;
  /** singleton pattern */
  private static final ObservationManager instance = new ObservationManager();
  /* members */
  /** configuration manager */
  private final ConfigurationManager cm = ConfigurationManager.getInstance();
  /** observation listeners */
  private final CopyOnWriteArrayList<ObservationListener> listeners = new CopyOnWriteArrayList<ObservationListener>();
  /** main observation settings */
  private ObservationSetting mainObservation = null;
  /** associated file to the observation settings */
  private File observationFile = null;
  /** derived observation settings for computations */
  private ObservationSetting observation = null;

  /**
   * Return the ObservationManager singleton
   * @return ObservationManager singleton
   */
  public final static ObservationManager getInstance() {
    return instance;
  }

  /**
   * Private constructor
   */
  private ObservationManager() {
    super();
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
   * @param obs new main observation to set
   */
  private void setMainObservation(final ObservationSetting obs) {
    this.mainObservation = obs;
  }

  /**
   * TODO : update observation used for computations from main observation
   */
  private void synchronizeObservations() {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("synchronizeObservations : " + toString(getMainObservation()));
    }

    // clone observation to ensure consistency (Swing can modify observation while the worker thread is running) :
    final ObservationSetting obs = (ObservationSetting) getMainObservation().clone();

    // TODO MULTI-CONF : use multi observations instead of main observation ?

    setObservation(obs);
  }

  /**
   * Return the observation used for computations
   * 
   * TODO MULTI-CONF : analyze usages to see bad usages (main or compute)
   *
   * @return observation used for computations
   */
  public ObservationSetting getObservation() {
    return this.observation;
  }

  /**
   * Private : define the observation used for computations
   * @param obs new observation to use for computations
   */
  private void setObservation(final ObservationSetting obs) {
    this.observation = obs;
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
  }

  // --- MAIN FUNCTIONS --------------------------------------------------------
  /**
   * Reset the current observation
   *
   * @throws IllegalStateException if an invalid reference was found (interferometer / instrument / instrument configuration)
   */
  public void reset() throws IllegalStateException {
    logger.info("Reset observation");

    final ObservationSetting newObservation = new ObservationSetting();

    setObservationFile(null);

    changeObservation(newObservation);
  }

  /**
   * Load an observation from the given file
   * @param file file to load
   * 
   * @throws IOException if an I/O exception occured
   * @throws IllegalStateException if an invalid reference was found (interferometer / instrument / instrument configuration) or an unexpected exception occured
   * @throws IllegalArgumentException if the file is not an Observation
   */
  public void load(final File file) throws IOException, IllegalStateException, IllegalArgumentException {
    if (file != null) {
      if (logger.isLoggable(Level.INFO)) {
        logger.info("Load observation from : " + file);
      }
      final Object loaded = loadObject(file);

      if (!(loaded instanceof ObservationSetting)) {
        throw new IllegalArgumentException("The loaded file does not correspond to a valid Aspro2 file : " + file);
      }

      setObservationFile(file);

      final ObservationSetting newObservation = (ObservationSetting) loaded;

      // post load processing :
      ObservationFileProcessor.onLoad(newObservation);

      // ready to use :
      changeObservation(newObservation);
    }
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
      if (logger.isLoggable(Level.INFO)) {
        logger.info("Load observation from : " + reader);
      }
      final Object loaded = loadObject(reader);

      if (!(loaded instanceof ObservationSetting)) {
        throw new IllegalArgumentException("The loaded document does not correspond to a valid Aspro2 file");
      }

      final ObservationSetting newObservation = (ObservationSetting) loaded;

      // post load processing :
      ObservationFileProcessor.onLoad(newObservation);

      return newObservation;
    }
    return null;
  }

  /**
   * Change the current observation with the given one
   * and fire load and change events
   * @param newObservation observation to use
   * 
   * @throws IllegalStateException if an invalid reference was found (interferometer / instrument / instrument configuration)
   */
  private void changeObservation(final ObservationSetting newObservation) throws IllegalStateException {
    defineDefaults(newObservation);

    // change the current observation :
    setMainObservation(newObservation);

    // fire an observation load event :
    this.fireObservationLoaded();

    // fire change events :
    this.fireTargetChangedEvents();
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
      if (logger.isLoggable(Level.INFO)) {
        logger.info("Save observation to : " + file);
      }

      final ObservationSetting saveObservation = getMainObservation();

      // pre save processing :
      ObservationFileProcessor.onSave(saveObservation);

      saveObject(file, saveObservation);

      setObservationFile(file);
    }
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
   * Listeners : BasicObservationForm / SettingPanel / ObservabilityPanel / UVCoveragePanel
   */
  private void fireObservationLoaded() {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("fireObservationLoaded : " + toString(getMainObservation()));
    }

    fireEvent(new ObservationEvent(ObservationEventType.LOADED, getMainObservation()));
  }

  /**
   * This fires an observation target change event to all registered listeners.
   * Fired by fireTargetChangedEvents() when an observation is loaded or reset or the target list was modified
   *
   * Listeners : BasicObservationForm / SettingPanel / UVCoveragePanel
   */
  private void fireObservationTargetsChanged() {
    // Increment target version of the main observation :
    getMainObservation().getVersion().incTargetVersion();

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("fireObservationTargetsChanged : " + toString(getMainObservation()));
      logger.fine("observation version = " + getMainObservation().getVersion());
    }

    fireEvent(new ObservationEvent(ObservationEventType.TARGET_CHANGED, getMainObservation()));
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
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("fireObservationUpdate : " + toString(getMainObservation()));
    }

    final UpdateObservationEvent event = new UpdateObservationEvent(getMainObservation());

    fireEvent(event);

    if (forceRefresh) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("fireObservationUpdate : FORCE REFRESH - changed = " + event.getChanged());
      }
      event.setChanged(UpdateObservationEvent.ChangeType.MAIN);
    } else {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("fireObservationUpdate : changed = " + event.getChanged());
      }
    }

    // Handle event result :
    if (event.getChanged() != UpdateObservationEvent.ChangeType.NONE) {

      final ObservationVersion version = getMainObservation().getVersion();

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
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("observation version = " + version);
      }

      // then synchronize observations : main and used for computations :
      // note: version object is copied too :
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
    // use observation for computations :
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("fireObservationRefresh : " + toString(getObservation()));
    }

    fireEvent(new ObservationEvent(ObservationEventType.REFRESH, getObservation()));
  }

  /**
   * This fires an observation refresh event (UV) to all registered listeners.
   * Fired by fireObservationUpdate() <= [BasicObservationForm|UVCoveragePanel].fireObservationUpdateEvent()
   * when the observation changed only UV related information
   *
   * Listeners : BasicObservationForm / UVCoveragePanel
   */
  private void fireObservationRefreshUV() {
    // use observation for computations :
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("fireObservationRefreshUV : " + toString(getObservation()));
    }

    fireEvent(new ObservationEvent(ObservationEventType.REFRESH_UV, getObservation()));
  }

  /**
   * This fires an observability done event to all registered listeners.
   * Fired by setObservabilityData() <= ObservabilityPanel.ObservabilitySwingWorker.refreshUI() (EDT) when the observability is computed
   *
   * Listeners : UVCoveragePanel
   * @param observation observation used to compute observability data (consistency)
   * @param obsData observability data
   */
  public void fireObservabilityDone(final ObservationSetting observation, final ObservabilityData obsData) {
    // use given observation (corresponding to an observation for computations) :
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("fireObservabilityDone : " + toString(observation));
    }

    fireEvent(new ObservabilityEvent(observation, obsData));
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
    // use observation for computations :
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("fireWarningsReady : " + toString(getObservation()));
    }

    fireEvent(new WarningContainerEvent(getObservation(), warningContainer));
  }

  /**
   * This fires an OIFits done event to all registered listeners.
   * Fired by setOIFitsFile() <= UVCoveragePanel.UVCoverageSwingWorker.refreshUI() (EDT) when the OIFits is computed
   *
   * Listeners : SettingPanel / OIFitsPanel
   */
  private void fireOIFitsDone() {
    // use observation for computations :
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("fireOIFitsDone : " + toString(getObservation()));
    }

    fireEvent(new ObservationEvent(ObservationEventType.OIFITS_DONE, getObservation()));
  }

  /**
   * Send an event to the registered listeners.
   * Note : any new listener registered during the processing of this event, will not be called
   * @param event event
   */
  private void fireEvent(final ObservationEvent event) {
    // ensure events are fired by Swing EDT :
    if (!SwingUtilities.isEventDispatchThread()) {
      logger.log(Level.SEVERE, "invalid thread : use EDT", new Throwable());
    }
    if (DEBUG_FIRE_EVENT) {
      logger.log(Level.SEVERE, "FIRE " + event + " on " + event.getObservation().getVersion(), new Throwable());
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("fireEvent : " + event);
    }

    final long start = System.nanoTime();

    for (final ObservationListener listener : this.listeners) {
      listener.onProcess(event);
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("fireEvent : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");
    }
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
      if (logger.isLoggable(Level.FINEST)) {
        logger.finest("setWhen : " + newValue);
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
      if (logger.isLoggable(Level.FINEST)) {
        logger.finest("setNightRestriction : " + useNightLimits);
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
      if (logger.isLoggable(Level.FINEST)) {
        logger.finest("setAtmosphereQuality : " + atmQuality);
      }
      when.setAtmosphereQuality(atmQuality);
    }
    return changed;
  }

  // --- INTERFEROMETER --------------------------------------------------------
  /**
   * Set the minimum elevation (deg)
   * Used by BasicObservationForm.updateObservation()
   *
   * @param minElev minimum elevation value
   * @return true if the value changed
   */
  public boolean setMinElevation(final double minElev) {
    final InterferometerConfigurationChoice interferometerChoice = getMainObservation().getInterferometerConfiguration();

    final boolean changed = interferometerChoice.getMinElevation() != minElev;
    if (changed) {
      if (logger.isLoggable(Level.FINEST)) {
        logger.finest("setInterferometerMinElevation : " + minElev);
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
      if (logger.isLoggable(Level.FINEST)) {
        logger.finest("setInterferometerConfigurationName : " + name);
      }
      interferometerChoice.setName(name);
      interferometerChoice.setInterferometerConfiguration(this.cm.getInterferometerConfiguration(name));
    }
    return changed;
  }

  // --- INSTRUMENT ------------------------------------------------------------
  /**
   * Set the instrument configuration (instrument for a given interferometer + period)
   * and refresh the internal InstrumentConfiguration reference
   * Used by BasicObservationForm.updateObservation()
   *
   * @param name name of the instrument configuration
   * @return true if the value changed
   */
  public boolean setInstrumentConfigurationName(final String name) {
    final FocalInstrumentConfigurationChoice instrumentChoice = getMainObservation().getInstrumentConfiguration();

    final boolean changed = !name.equals(instrumentChoice.getName());
    if (changed) {
      if (logger.isLoggable(Level.FINEST)) {
        logger.finest("setInstrumentConfigurationName : " + name);
      }
      instrumentChoice.setName(name);
      instrumentChoice.setInstrumentConfiguration(this.cm.getInterferometerInstrumentConfiguration(
              getMainObservation().getInterferometerConfiguration().getName(), name));
    }
    return changed;
  }

  /**
   * Set the instrument station configuration (baseline for a given instrument)
   * and refresh the internal StationList reference
   * Used by BasicObservationForm.updateObservation()
   *
   * @param stations string representation of the station list (AA BB CC ...)
   * @return true if the value changed
   */
  public boolean setInstrumentConfigurationStations(final String stations) {
    final FocalInstrumentConfigurationChoice instrumentChoice = getMainObservation().getInstrumentConfiguration();

    final boolean changed = !stations.equals(instrumentChoice.getStations());
    if (changed) {
      if (logger.isLoggable(Level.FINEST)) {
        logger.finest("setInstrumentConfigurationStations : " + stations);
      }
      instrumentChoice.setStations(stations);
      instrumentChoice.setStationList(this.cm.getInstrumentConfigurationStations(
              getMainObservation().getInterferometerConfiguration().getName(),
              getMainObservation().getInstrumentConfiguration().getName(), stations));
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
    final FocalInstrumentConfigurationChoice instrumentChoice = getMainObservation().getInstrumentConfiguration();

    // pops can be null :
    final boolean changed = isChanged(pops, instrumentChoice.getPops());
    if (changed) {
      if (logger.isLoggable(Level.FINEST)) {
        logger.finest("setInstrumentConfigurationPoPs : " + pops);
      }
      instrumentChoice.setPops(pops);
      instrumentChoice.setPopList(this.cm.parseInstrumentPoPs(
              getMainObservation().getInterferometerConfiguration().getName(),
              getMainObservation().getInstrumentConfiguration().getName(), pops));
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
    final FocalInstrumentConfigurationChoice instrumentChoice = getMainObservation().getInstrumentConfiguration();

    // mode can be null :
    final boolean changed = isChanged(mode, instrumentChoice.getInstrumentMode());
    if (changed) {
      if (logger.isLoggable(Level.FINEST)) {
        logger.finest("setInstrumentMode : " + mode);
      }
      instrumentChoice.setInstrumentMode(mode);
      instrumentChoice.setFocalInstrumentMode(this.cm.getInstrumentMode(
              getMainObservation().getInterferometerConfiguration().getName(),
              getMainObservation().getInstrumentConfiguration().getName(), mode));
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
      if (logger.isLoggable(Level.FINEST)) {
        logger.finest("setInstrumentSamplingPeriod : " + samplingPeriod);
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
      if (logger.isLoggable(Level.FINEST)) {
        logger.finest("setInstrumentAcquisitionTime : " + obsDuration);
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
  public final boolean isCalibrator(final Target target) {
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
   * Add a target given its unique name and
   * fires a target change and an observation change event 
   * Note : it does not check anything on coordinates (cross matching)
   * @param name target name
   * @param star object to create the target object
   */
  public void addTarget(final String name, final Star star) {
    if (name != null && name.length() > 0) {
      final boolean changed = (getTarget(name) == null);
      if (changed) {
        if (logger.isLoggable(Level.FINEST)) {
          logger.finest("addTarget : " + name);
        }

        final Target t = new Target();
        t.setName(name);

        /*
        Strings = {DEC=+43 49 23.910, RA=05 01 58.1341, OTYPELIST=**,Al*,SB*,*,Em*,V*,IR,UV, SPECTRALTYPES=A8Iab:}
        Doubles = {PROPERMOTION_RA=0.18, PARALLAX=1.6, DEC_d=43.8233083, FLUX_J=1.88, PROPERMOTION_DEC=-2.31, FLUX_K=1.533, PARALLAX_err=1.16, FLUX_V=3.039, FLUX_H=1.702, RA_d=75.4922254}
         */

        // coordinates (deg) :
        t.setRA(star.getPropertyAsString(Star.Property.RA).replace(' ', ':'));
        t.setDEC(star.getPropertyAsString(Star.Property.DEC).replace(' ', ':'));
        t.setEQUINOX(AsproConstants.EPOCH_J2000);

        // Proper motion (mas/yr) (optional) :
        t.setPMRA(star.getPropertyAsDouble(Star.Property.PROPERMOTION_RA));
        t.setPMDEC(star.getPropertyAsDouble(Star.Property.PROPERMOTION_DEC));

        // Parallax (mas) (optional) :
        t.setPARALLAX(star.getPropertyAsDouble(Star.Property.PARALLAX));
        t.setPARAERR(star.getPropertyAsDouble(Star.Property.PARALLAX_err));

        // Magnitudes (optional) :
        t.setFLUXV(star.getPropertyAsDouble(Star.Property.FLUX_V));
        t.setFLUXI(star.getPropertyAsDouble(Star.Property.FLUX_I));
        t.setFLUXJ(star.getPropertyAsDouble(Star.Property.FLUX_J));
        t.setFLUXH(star.getPropertyAsDouble(Star.Property.FLUX_H));
        t.setFLUXK(star.getPropertyAsDouble(Star.Property.FLUX_K));
        t.setFLUXN(star.getPropertyAsDouble(Star.Property.FLUX_N));

        // Spectral types :
        t.setSPECTYP(star.getPropertyAsString(Star.Property.SPECTRALTYPES));

        // Object types :
        t.setOBJTYP(star.getPropertyAsString(Star.Property.OTYPELIST));

        // Radial velocity (km/s) (optional) :
        t.setSYSVEL(star.getPropertyAsDouble(Star.Property.RV));
        t.setVELTYP(star.getPropertyAsString(Star.Property.RV_DEF));

        // Identifiers :
        t.setIDS(star.getPropertyAsString(Star.Property.IDS));

        getTargets().add(t);

        // fire change events :
        this.fireTargetChangedEvents();
      }
    }
  }

  /**
   * Remove the given science target from the target list and
   * fires a target change and an observation change event 
   * @param target science target to remove
   */
  public void removeTarget(final Target target) {
    if (target != null) {

      // remove calibrators related to the science target :
      getTargetUserInfos().getCalibrators(target).clear();

      if (getTargets().remove(target)) {
        if (logger.isLoggable(Level.FINEST)) {
          logger.finest("removeTarget : " + target);
        }

        // fire change events :
        this.fireTargetChangedEvents();
      }
    }
  }

  /**
   * Remove the given calibrator target from the target list
   * and fires a target change and an observation change event 
   * @param calibrator calibrator target to remove
   */
  public void removeCalibrator(final Target calibrator) {
    if (calibrator != null) {

      final TargetUserInformations targetUserInfos = getTargetUserInfos();

      for (Target target : getTargets()) {
        targetUserInfos.removeCalibratorFromTarget(target, calibrator);
      }
      targetUserInfos.removeCalibrator(calibrator);

      // remove calibrator from target list that fires change events :
      removeTarget(calibrator);
    }
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
  private void fireTargetChangedEvents() {

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

    // haMin can be null :
    final boolean changed = isChanged(haMin, targetConf.getHAMin());
    if (changed) {
      if (logger.isLoggable(Level.FINEST)) {
        logger.finest("setTargetHAMin : " + haMin);
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

    // haMax can be null :
    final boolean changed = isChanged(haMax, targetConf.getHAMax());
    if (changed) {
      if (logger.isLoggable(Level.FINEST)) {
        logger.finest("setTargetHAMax : " + haMax);
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

    // special case : None
    final String mode = (AsproConstants.NONE.equals(ftMode)) ? null : ftMode;

    // ftMode can be null :
    final boolean changed = isChanged(mode, targetConf.getFringeTrackerMode());
    if (changed) {
      if (logger.isLoggable(Level.FINEST)) {
        logger.finest("setTargetFTMode : " + mode);
      }
      targetConf.setFringeTrackerMode(mode);
    }
    return changed;
  }

  // --- COMPUTATION RESULTS ---------------------------------------------------
  /**
   * Defines the OIFits structure in the observation for later reuse (Visiblity Explorer)
   * Used by UVCoveragePanel.UVCoverageSwingWorker.refreshUI()
   * 
   * @param oiFitsFile OIFits structure
   */
  public void setOIFitsFile(final OIFitsFile oiFitsFile) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("setOIFitsFile : " + oiFitsFile);
    }

    // TODO : remove
    getObservation().setOIFitsFile(oiFitsFile);

    if (oiFitsFile != null) {
      this.fireOIFitsDone();
    }
  }

  // --- INTERNAL METHODS ------------------------------------------------------
  /**
   * Define default values (empty child objects)
   * @param newObservation observation to modify
   * @throws IllegalStateException if an invalid reference was found (interferometer / instrument / instrument configuration)
   */
  private void defineDefaults(final ObservationSetting newObservation) throws IllegalStateException {
    if (newObservation.getName() == null || newObservation.getName().length() == 0) {
      newObservation.setName("default");
    }
    if (newObservation.getWhen() == null) {
      final WhenSetting when = new WhenSetting();
      when.setDate(getCalendar(new Date()));
      when.setNightRestriction(AsproConstants.DEFAULT_USE_NIGHT_LIMITS);

      newObservation.setWhen(when);
    }
    if (newObservation.getInterferometerConfiguration() == null) {
      final String defInterferometer = this.cm.getInterferometerNames().get(0);
      final String defInterferometerConfiguration = this.cm.getInterferometerConfigurationNames(defInterferometer).get(0);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("default Interferometer = " + defInterferometer);
        logger.fine("default InterferometerConfiguration = " + defInterferometerConfiguration);
      }

      final InterferometerConfigurationChoice interferometerChoice = new InterferometerConfigurationChoice();
      interferometerChoice.setName(defInterferometerConfiguration);
      interferometerChoice.setMinElevation(AsproConstants.DEFAULT_MIN_ELEVATION);

      newObservation.setInterferometerConfiguration(interferometerChoice);
    }
    if (newObservation.getInstrumentConfiguration() == null) {
      final String defInstrument = this.cm.getInterferometerInstrumentNames(newObservation.getInterferometerConfiguration().getName()).get(0);
      final String defInstrumentConfiguration = this.cm.getInstrumentConfigurationNames(newObservation.getInterferometerConfiguration().getName(), defInstrument).get(0);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("default Instrument = " + defInstrument);
        logger.fine("default InstrumentConfiguration = " + defInstrumentConfiguration);
      }

      final FocalInstrumentConfigurationChoice instrumentChoice = new FocalInstrumentConfigurationChoice();
      instrumentChoice.setName(defInstrument);
      instrumentChoice.setStations(defInstrumentConfiguration);

      newObservation.setInstrumentConfiguration(instrumentChoice);
    }

    // update references :
    // can throw IllegalStateException if an invalid reference was found :
    updateObservation(newObservation);
  }

  /**
   * Update observation with resolved references  (interferometer / instrument / instrument configuration ...)
   * @param obs observation to use
   * @throws IllegalStateException if an invalid reference was found (interferometer / instrument / instrument configuration)
   */
  private void updateObservation(final ObservationSetting obs) throws IllegalStateException {
    // ugly code to update all resolved references used on post load :
    final InterferometerConfigurationChoice interferometerChoice = obs.getInterferometerConfiguration();

    interferometerChoice.setInterferometerConfiguration(this.cm.getInterferometerConfiguration(interferometerChoice.getName()));

    if (interferometerChoice.getInterferometerConfiguration() == null) {
      throw new IllegalStateException("the interferometer configuration [" + interferometerChoice.getName() + "] is invalid !");
    }

    final FocalInstrumentConfigurationChoice instrumentChoice = obs.getInstrumentConfiguration();

    instrumentChoice.setInstrumentConfiguration(this.cm.getInterferometerInstrumentConfiguration(
            interferometerChoice.getName(), instrumentChoice.getName()));

    if (instrumentChoice.getInstrumentConfiguration() == null) {
      throw new IllegalStateException("the instrument [" + instrumentChoice.getName() + "] is invalid !");
    }

    instrumentChoice.setStationList(this.cm.getInstrumentConfigurationStations(
            interferometerChoice.getName(), instrumentChoice.getName(), instrumentChoice.getStations()));

    if (instrumentChoice.getStationList() == null) {
      if (!fixInstrumentConfigurationStations(interferometerChoice.getName(), instrumentChoice)) {
        throw new IllegalStateException("the instrument configuration [" + instrumentChoice.getStations() + "] is invalid !");
      }
    }

    // pops can be undefined :
    instrumentChoice.setPopList(this.cm.parseInstrumentPoPs(
            interferometerChoice.getName(), instrumentChoice.getName(), instrumentChoice.getPops()));

    // instrument mode can be undefined :
    instrumentChoice.setFocalInstrumentMode(this.cm.getInstrumentMode(
            interferometerChoice.getName(), instrumentChoice.getName(), instrumentChoice.getInstrumentMode()));

    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("updateObservation : " + toString(obs));
    }
  }

  /**
   * Try to fix an invalid instrument configuration (A0 B0 C0) by testing all possible permutation of the stations.
   * This problem that can happen if
   * - ESO CfP changes
   * - we have errors in our configuration files
   *
   * for example : A0 B0 C0 is equivalent to C0 B0 A0
   *
   * @param interferometerConfiguration interferometer configuration name
   * @param instrumentChoice instrument choice
   * @return true if instrument configuration is fixed
   */
  private boolean fixInstrumentConfigurationStations(final String interferometerConfiguration,
                                                     final FocalInstrumentConfigurationChoice instrumentChoice) {
    boolean res = false;

    // trim to be sure (xml manually modified) :
    final String stationNames = instrumentChoice.getStations().trim();

    if (logger.isLoggable(Level.INFO)) {
      logger.info("the instrument configuration [" + stationNames + "] is incorrect, trying to match a possible configuration ...");
    }

    // A0 B0 C0 is equivalent to C0 B0 A0
    final String[] stations = stationNames.split(" ");

    // number of stations in the string :
    final int nStation = stations.length;

    if (nStation < 2) {
      // bad value
      return false;
    }

    // generate station combinations (indexes) : :
    final List<int[]> iStations = CombUtils.generatePermutations(nStation);

    String stationIds = null;
    List<Station> stationList = null;

    final StringBuilder sb = new StringBuilder(16);

    int[] idx;
    // skip first permutation as it is equivalent to stationNames :
    for (int i = 1, j = 0, size = iStations.size(); i < size; i++) {
      idx = iStations.get(i);

      for (j = 0; j < nStation; j++) {
        if (j > 0) {
          sb.append(' ');
        }
        sb.append(stations[idx[j]]);
      }

      stationIds = sb.toString();
      // recycle :
      sb.setLength(0);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("trying instrument configuration [" + stationIds + "]");
      }

      // find station list corresponding to the station ids :
      stationList = this.cm.getInstrumentConfigurationStations(
              interferometerConfiguration, instrumentChoice.getName(), stationIds);

      if (stationList != null) {
        break;
      }
    }

    if (stationList != null) {
      res = true;

      instrumentChoice.setStations(stationIds);
      instrumentChoice.setStationList(stationList);

      if (logger.isLoggable(Level.INFO)) {
        logger.info("the correct instrument configuration is [" + stationIds + "]. Save your file to keep this modification");
      }
    }
    return res;
  }

  /**
   * Utility method to get a string representation of main observation settings
   * @param obs observation to use
   * @return string representation of main observation settings
   */
  public static String toString(final ObservationSetting obs) {
    final StringBuffer sb = new StringBuffer(255);
    sb.append("name : ").append(obs.getName());
    sb.append(" date : ").append(obs.getWhen().getDate());
    sb.append(" interferometer : ").append(obs.getInterferometerConfiguration().getName());
    // instrument :
    sb.append(" instrument : ").append(obs.getInstrumentConfiguration().getName());
    sb.append(" stations : ").append(obs.getInstrumentConfiguration().getStations());
    if (obs.getInstrumentConfiguration().getPops() != null) {
      sb.append(" pops : ").append(obs.getInstrumentConfiguration().getPops());
    }
    if (obs.getInstrumentConfiguration().getInstrumentMode() != null) {
      sb.append(" mode : ").append(obs.getInstrumentConfiguration().getInstrumentMode());
    }
    if (obs.hasTargets()) {
      sb.append(" targets : \n").append(obs.getTargets());
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
