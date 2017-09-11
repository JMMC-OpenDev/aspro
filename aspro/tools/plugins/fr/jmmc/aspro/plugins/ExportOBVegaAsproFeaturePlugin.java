/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.plugins;

import fr.jmmc.aspro.feature.AsproFeatures;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.oi.ObservationSetting;

/**
 *
 * @author bourgesl
 */
public abstract class ExportOBVegaAsproFeaturePlugin extends AsproFeaturePlugin {

  /**
   * Public constructor called by the PluginManager
   * @param plugin Aspro plugin implementation
   */
  public ExportOBVegaAsproFeaturePlugin(final AsproPlugin plugin) {
    super(AsproFeatures.ExportOBVega, plugin);
  }
  
  /**
   * Generate the Star List for the given observation and observability data
   * @param observation observation to use
   * @param obsData computed observability data using astronomical night (-18 deg) without night restrictions
   * @return complete star list as string or null if an error occured or the plugin execution is cancelled by the user
   */
  public abstract String generate(final ObservationSetting observation, final ObservabilityData obsData);
  
}
