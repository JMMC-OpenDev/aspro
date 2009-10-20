/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservationManager.java,v 1.2 2009-10-20 13:08:51 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
import fr.jmmc.aspro.model.oi.WhenSetting;
import java.util.Date;
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

  // members :
  /** observation (single for now) */
  private ObservationSetting observation = null;


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
  
  public ObservationSetting getObservation() {
    return observation;
  }

  public String toString(final ObservationSetting obs) {
    final StringBuffer sb = new StringBuffer();
    sb.append("name : ").append(obs.getName());
    sb.append(" when : ").append(obs.getWhen().getDate());
    sb.append(" interferometer : ").append(obs.getInterferometerConfiguration().getName());
    sb.append(" instrument : ").append(obs.getInstrumentConfiguration().getName());
    return sb.toString();
  }

  // API :
  
  public boolean setWhen(final Date date) {
    final WhenSetting when = getObservation().getWhen();

    final XMLGregorianCalendar newValue = getCalendar(date);

    boolean changed = !newValue.equals(when.getDate());
    if (changed) {
      when.setDate(newValue);
    }
    return changed;
  }

  public boolean setInterferometerName(final String name) {
    final InterferometerConfigurationChoice interferometerName = getObservation().getInterferometerConfiguration();

    boolean changed = !name.equals(interferometerName.getName());
    if (changed) {
      interferometerName.setName(name);
    }
    return changed;
  }

  public boolean setInstrumentName(final String name) {
    final FocalInstrumentConfigurationChoice instrumentName = getObservation().getInstrumentConfiguration();

    boolean changed = !name.equals(instrumentName.getName());
    if (changed) {
      instrumentName.setName(name);
    }
    return changed;
  }

  public void fireObservationChanged() {
    logger.severe("fireObservationChanged : " + toString(getObservation()));
  }
  
}
