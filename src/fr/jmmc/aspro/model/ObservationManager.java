/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservationManager.java,v 1.5 2009-11-03 16:57:55 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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

import fr.jmmc.aspro.model.oi.FocalInstrumentConfigurationChoice;
import fr.jmmc.aspro.model.oi.InterferometerConfigurationChoice;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.WhenSetting;
import fr.jmmc.aspro.service.ObservabilityService;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
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
  /** observation (single for now) */
  private ObservationSetting observation = null;
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
    this.observation.setName("default");
    this.observation.setWhen(new WhenSetting());
    this.observation.setInstrumentConfiguration(new FocalInstrumentConfigurationChoice());
    this.observation.setInterferometerConfiguration(new InterferometerConfigurationChoice());
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

  public String toString(final ObservationSetting obs) {
    final StringBuffer sb = new StringBuffer();
    sb.append("name : ").append(obs.getName());
    sb.append(" when : ").append(obs.getWhen().getDate());
    sb.append(" interferometer : ").append(obs.getInterferometerConfiguration().getName());
    sb.append(" instrument : ").append(obs.getInstrumentConfiguration().getName());
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
        t.setEQUINOX(2000f);
        // other fields (mag) ...
        getObservation().getTargets().add(t);
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
    final Vector v = new Vector();
    for (Target t : getObservation().getTargets()) {
      v.add(t.getName());
    }
    return v;
  }

  /**
   * Future model listener notify all pattern
   */
  public void fireObservationChanged() {
    logger.severe("fireObservationChanged : " + toString(getObservation()));

    final File tmp;
    try {
      tmp = File.createTempFile("observation", ".xml");
      tmp.deleteOnExit();

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("output file = " + tmp.getAbsolutePath());
      }

      save(tmp, getObservation());

    } catch (IOException ioe) {
      logger.log(Level.SEVERE, "unable to create temporary file", ioe);
    }

    // Call listeners with a copy of the listener list to avoid concurrent modification :
    for (ObservationListener listener : listeners.toArray(new ObservationListener[listeners.size()])) {
      listener.onChange(getObservation());
    }

  }
}
