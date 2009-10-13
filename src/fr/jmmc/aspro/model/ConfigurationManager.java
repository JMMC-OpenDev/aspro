/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ConfigurationManager.java,v 1.1 2009-10-13 16:04:14 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2009/09/21 15:38:51  bourgesl
 * initial jmcs gui + jaxb loader
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.FocalInstrumentConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.InterferometerSetting;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.logging.Level;

/**
 * This class manages configuration files for the Interferometer configurations
 * @author bourgesl
 */
public class ConfigurationManager extends BaseOIManager {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.config.ConfigurationManager";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /** singleton pattern */
  private static ConfigurationManager instance = new ConfigurationManager();

  // members :
  /** Map : id, interferometer description */
  private final Map<String, InterferometerDescription> interferometerDescriptions = new LinkedHashMap<String, InterferometerDescription>();
  /** Map : id, interferometer configuration */
  private final Map<String, InterferometerConfiguration> interferometerConfigurations = new LinkedHashMap<String, InterferometerConfiguration>();


  /**
   * Return the ConfigurationManager singleton
   * @return ConfigurationManager singleton
   */
  public static ConfigurationManager getInstance() {
    return instance;
  }

  /**
   * Private constructor
   */
  private ConfigurationManager() {
    super();
    initialize();
  }

  private void initialize() {
    // TODO : scan files and load them in the maps :
    final InterferometerSetting is = (InterferometerSetting)load("./VLTI.xml");

    addInterferometerSetting(is);

    logger.log(Level.SEVERE, "ConfigurationManager : descriptions   = " + getInterferometerDescriptions());
    logger.log(Level.SEVERE, "ConfigurationManager : configurations = " + getInterferometerConfigurations());
  }

  private void addInterferometerSetting(final InterferometerSetting is) {
    interferometerDescriptions.put(is.getDescription().getName(), is.getDescription());
    interferometerConfigurations.put(is.getConfiguration().getName(), is.getConfiguration());
  }

  public Map<String, InterferometerDescription> getInterferometerDescriptions() {
    return interferometerDescriptions;
  }

  public Map<String, InterferometerConfiguration> getInterferometerConfigurations() {
    return interferometerConfigurations;
  }

  public InterferometerDescription getInterferometerDescription(final String id) {
    return interferometerDescriptions.get(id);
  }

  public InterferometerConfiguration getInterferometerConfiguration(final String id) {
    return interferometerConfigurations.get(id);
  }



  /**
   * Test code
   */
  public static void main(final String[] args) {
    // path to scan :
    final Object loaded = ConfigurationManager.getInstance().load("./VLTI.xml");
    logger.log(Level.SEVERE, "ConfigurationManager : document root = " + loaded);


    final InterferometerSetting is = (InterferometerSetting) loaded;

    logger.log(Level.SEVERE, "Configuration : " + is.getConfiguration());
    logger.log(Level.SEVERE, "Configuration - interferometer : " + is.getConfiguration().getInterferometer());

    for (FocalInstrumentConfiguration ic : is.getConfiguration().getInstruments()) {
      logger.log(Level.SEVERE, "Configuration - instrument : " + ic.getFocalInstrument());
      logger.log(Level.SEVERE, "Configuration - instrument configurations : " + ic.getConfigurations());
    }


  }
}
