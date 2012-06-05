/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Vector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Configuration holder used to override default configuration
 *
 * @author bourgesl
 */
public final class Configuration {

  /** Map : id, interferometer description */
  private final Map<String, InterferometerDescription> interferometerDescriptions = new LinkedHashMap<String, InterferometerDescription>();
  /** Map : id, interferometer configuration */
  private final Map<String, InterferometerConfiguration> interferometerConfigurations = new LinkedHashMap<String, InterferometerConfiguration>();
  /** Map: id, interferometer configuration names */
  private final Map<String, Vector<String>> interferometerConfigurationNames = new HashMap<String, Vector<String>>();

  /**
   * Package visible constructor
   */
  Configuration() {
    super();
  }

  /**
   * Clear the complete configuration
   */
  void clear() {
    interferometerDescriptions.clear();
    interferometerConfigurations.clear();
    interferometerConfigurationNames.clear();
  }

  /**
   * Return the interferometer description map keyed by name
   * @return interferometer description map
   */
  public Map<String, InterferometerDescription> getInterferometerDescriptions() {
    return interferometerDescriptions;
  }

  /**
   * Return the interferometer configuration map keyed by name
   * @return interferometer configuration map
   */
  public Map<String, InterferometerConfiguration> getInterferometerConfigurations() {
    return interferometerConfigurations;
  }

  /**
   * Return the list of interferometer configurations associated to the given interferometer
   * @param interferometerName name of the interferometer
   * @return list of interferometer configurations
   */
  public Vector<String> getInterferometerConfigurationNames(final String interferometerName) {
    Vector<String> names = interferometerConfigurationNames.get(interferometerName);

    // lazily prepare map:
    if (names == null) {

      // add keys:
      for (InterferometerDescription id : getInterferometerDescriptions().values()) {
        interferometerConfigurationNames.put(id.getName(), new Vector<String>());
      }

      // ordered:
      for (InterferometerConfiguration ic : getInterferometerConfigurations().values()) {
        names = interferometerConfigurationNames.get(ic.getInterferometer().getName());
        if (names != null) {
          names.add(ic.getName());
        }
      }

      names = interferometerConfigurationNames.get(interferometerName);
    }
    return names;
  }
}
