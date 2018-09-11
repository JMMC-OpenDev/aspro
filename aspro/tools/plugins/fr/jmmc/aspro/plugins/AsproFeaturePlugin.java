/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.plugins;

import fr.jmmc.aspro.feature.AsproFeatures;
import java.util.logging.Logger;

/**
 * This abstract class describes Aspro feature implementation and lifecyle
 * @author bourgesl
 */
public abstract class AsproFeaturePlugin {

  /* members */
  /** Concrete class logger */
  protected final Logger logger = Logger.getLogger(getClass().getName());
  /** Aspro plugin feature */
  private final AsproFeatures feature;
  /** Aspro plugin implementation */
  private final AsproPlugin plugin;

  /**
   * Public constructor called by the PluginManager
   * @param feature Aspro plugin feature
   * @param plugin Aspro plugin implementation
   */
  public AsproFeaturePlugin(final AsproFeatures feature, final AsproPlugin plugin) {
    this.feature = feature;
    this.plugin = plugin;
  }

  /**
   * Initialize the plugin i.e. prepare its execution (resource allocation); this implementation does nothing.
   */
  public void initialize() {
    logger.fine("initialize");
  }

  /**
   * Stop the plugin i.e. free any allocated resources; this implementation does nothing.
   */
  public void stop() {
    logger.fine("stop");
  }

  /**
   * Return the Aspro plugin feature
   * @return Aspro plugin feature
   */
  public AsproFeatures getFeature() {
    return feature;
  }

  /**
   * Return the Aspro plugin implementation
   * @return Aspro plugin implementation
   */
  public AsproPlugin getPlugin() {
    return plugin;
  }
}
