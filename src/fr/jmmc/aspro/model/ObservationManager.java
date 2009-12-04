/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservationManager.java,v 1.10 2009-12-04 16:26:58 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.ObservationListener.ObservationEventType;
import fr.jmmc.aspro.model.oi.FocalInstrumentConfigurationChoice;
import fr.jmmc.aspro.model.oi.InterferometerConfigurationChoice;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.WhenSetting;
import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import java.util.logging.Level;
import javax.xml.datatype.XMLGregorianCalendar;

/**
 * This class manages observation files i.e. user defined observation settings
 * @author bourgesl
 */
public class ObservationManager extends BaseOIManager {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.model.ObservationManager";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** singleton pattern */
  private static ObservationManager instance = new ObservationManager();
  /* members */
  /** observation settings */
  private ObservationSetting observation = null;
  /** associated file to the observation settings */
  private File observationFile = null;
  /** observation listeners */
  private List<ObservationListener> listeners = new ArrayList<ObservationListener>();

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
    createObservation();
  }

  private void createObservation() {
    this.observation = new ObservationSetting();
    defineDefaults(this.observation);
  }

  private void defineDefaults(final ObservationSetting obs) {
    if (obs.getName() == null || obs.getName().length() == 0) {
      this.observation.setName("default");
    }
    if (obs.getWhen() == null) {
      obs.setWhen(new WhenSetting());
    }
    if (obs.getInstrumentConfiguration() == null) {
      obs.setInstrumentConfiguration(new FocalInstrumentConfigurationChoice());
    }
    if (obs.getInterferometerConfiguration() == null) {
      obs.setInterferometerConfiguration(new InterferometerConfigurationChoice());
    }
  }

  public void register(final ObservationListener listener) {
    this.listeners.add(listener);
  }

  public void unregister(final ObservationListener listener) {
    this.listeners.remove(listener);
  }

  public ObservationSetting getObservation() {
    return observation;
  }

  public File getObservationFile() {
    return observationFile;
  }

  public String toString(final ObservationSetting obs) {
    final StringBuffer sb = new StringBuffer();
    sb.append("name : ").append(obs.getName());
    sb.append(" when : ").append(obs.getWhen().getDate());
    sb.append(" interferometer : ").append(obs.getInterferometerConfiguration().getName());
    sb.append(" instrument : ").append(obs.getInstrumentConfiguration().getName());
    sb.append(" stations : ").append(obs.getInstrumentConfiguration().getStations());
    if (!obs.getTargets().isEmpty()) {
      sb.append(" targets : \n").append(obs.getTargets());
    }

    return sb.toString();
  }

  // API :
  /**
   * Set the observation date (no time)
   * @param date date to use
   * @return true if the date changed
   */
  public boolean setWhen(final Date date) {
    final WhenSetting when = getObservation().getWhen();

    final XMLGregorianCalendar newValue = getCalendar(date);

    boolean changed = !newValue.equals(when.getDate());
    if (changed) {
      when.setDate(newValue);
    }
    return changed;
  }

  public boolean setInterferometerConfigurationName(final String name) {
    final InterferometerConfigurationChoice interferometerChoice = getObservation().getInterferometerConfiguration();

    boolean changed = !name.equals(interferometerChoice.getName());
    if (changed) {
      interferometerChoice.setName(name);
      interferometerChoice.setInterferometerConfiguration(ConfigurationManager.getInstance().getInterferometerConfiguration(name));
    }
    return changed;
  }

  public boolean setInstrumentConfigurationName(final String name) {
    final FocalInstrumentConfigurationChoice instrumentChoice = getObservation().getInstrumentConfiguration();

    boolean changed = !name.equals(instrumentChoice.getName());
    if (changed) {
      instrumentChoice.setName(name);
      instrumentChoice.setInstrumentConfiguration(ConfigurationManager.getInstance().getInterferometerInstrumentConfiguration(
              getObservation().getInterferometerConfiguration().getName(), name));
    }
    return changed;
  }

  public boolean setInstrumentConfigurationStations(final String stations) {
    final FocalInstrumentConfigurationChoice instrumentChoice = getObservation().getInstrumentConfiguration();

    boolean changed = !stations.equals(instrumentChoice.getStations());
    if (changed) {
      instrumentChoice.setStations(stations);
      instrumentChoice.setStationList(ConfigurationManager.getInstance().getInstrumentConfigurationStations(
              getObservation().getInterferometerConfiguration().getName(), getObservation().getInstrumentConfiguration().getName(), stations));
    }
    return changed;
  }

  private void updateObservation() {
    // ugly code to update all resolved references used on post load :
    final InterferometerConfigurationChoice interferometerChoice = getObservation().getInterferometerConfiguration();

    interferometerChoice.setInterferometerConfiguration(ConfigurationManager.getInstance().getInterferometerConfiguration(interferometerChoice.getName()));

    final FocalInstrumentConfigurationChoice instrumentChoice = getObservation().getInstrumentConfiguration();

    instrumentChoice.setInstrumentConfiguration(ConfigurationManager.getInstance().getInterferometerInstrumentConfiguration(
              interferometerChoice.getName(), instrumentChoice.getName()));

    instrumentChoice.setStationList(ConfigurationManager.getInstance().getInstrumentConfigurationStations(
            interferometerChoice.getName(), instrumentChoice.getName(), instrumentChoice.getStations()));

  }

  /**
   * Add a target given its unique name
   * @param name target name
   * @param ra right ascension in degrees
   * @param dec declination in degrees
   * @return true if the target list changed
   */
  public boolean addTarget(final String name, final double ra, final double dec) {
    boolean changed = false;
    if (name != null && name.length() > 0) {
      changed = (getTarget(name) == null);
      if (changed) {
        final Target t = new Target();
        t.setName(name);
        t.setRA(ra);
        t.setDEC(dec);
        t.setEQUINOX(AsproConstants.EPOCH_J2000);
        // other fields (mag) ...
        getObservation().getTargets().add(t);
      }
    }
    return changed;
  }

  /**
   * Remove a target given its unique name
   * @param name target name
   * @return true if the target list changed
   */
  public boolean removeTarget(final String name) {
    boolean changed = false;
    if (name != null && name.length() > 0) {
      Target t;
      for (Iterator<Target> it = getObservation().getTargets().iterator(); it.hasNext();) {
        t = it.next();
        if (t.getName().equals(name)) {
          changed = true;
          it.remove();
          break;
        }
      }
    }
    return changed;
  }

  public Target getTarget(final String name) {
    for (Target t : getObservation().getTargets()) {
      if (t.getName().equals(name)) {
        return t;
      }
    }
    return null;
  }

  public Vector<String> getTargetNames() {
    final Vector<String> v = new Vector<String>();
    for (Target t : getObservation().getTargets()) {
      v.add(t.getName());
    }
    return v;
  }

  /**
   * This fires a changed observation event to all registered listeners
   */
  public void fireObservationChanged() {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("fireObservationChanged : " + toString(getObservation()));
    }

    // Call listeners with a copy of the listener list to avoid concurrent modification :
    for (ObservationListener listener : listeners.toArray(new ObservationListener[listeners.size()])) {
      listener.onProcess(ObservationEventType.CHANGED, getObservation());
    }
  }

  /**
   * Save the current observation in the given file
   * @param file file to save
   * @throws RuntimeException if the save operation failed
   */
  public void save(final File file) throws RuntimeException {
    if (file != null) {
      this.observationFile = file;

      if (logger.isLoggable(Level.INFO)) {
        logger.info("Save observation to : " + this.observationFile);
      }
      saveObject(this.observationFile, getObservation());
    }
  }

  /**
   * Load an observation from the given file
   * @param file file to load
   * @throws RuntimeException if the load operation failed
   */
  public void load(final File file) throws RuntimeException {
    if (file != null) {
      this.observationFile = file;

      if (logger.isLoggable(Level.INFO)) {
        logger.info("Load observation from : " + this.observationFile);
      }
      final Object loaded = loadObject(this.observationFile);

      if (!(loaded instanceof ObservationSetting)) {
        throw new RuntimeException("The file does not correspond to observation settings : " + file);
      }

      final ObservationSetting newObservation = (ObservationSetting) loaded;
      defineDefaults(newObservation);

      // change the current observation :
      this.observation = newObservation;

      // update references :
      updateObservation();

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("fireObservationLoaded : " + toString(getObservation()));
      }

      // Call listeners with a copy of the listener list to avoid concurrent modification :
      for (ObservationListener listener : listeners.toArray(new ObservationListener[listeners.size()])) {
        listener.onProcess(ObservationEventType.LOADED, getObservation());
      }
    }
  }
}
