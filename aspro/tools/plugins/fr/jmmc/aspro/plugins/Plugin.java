/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.plugins;

/**
 * This interface defines generic Plugin meta data i.e. not specific to any application
 * @author bourgesl
 */
public interface Plugin {

  /**
   * Get the unique plugin name
   * @return the plugin name
   */
  public String getName();

  /**
   * Get the minimal application version required to use this plugin
   * @return the minimal application version
   */
  public String getApplicationVersion();

  /**
   * Get the plugin version
   * @return the plugin version
   */
  public String getPluginVersion();

  /**
   * Get the plugin description
   * @return the plugin description
   */
  public String getDescription();

  /**
   * Get the plugin provider
   * @return the plugin provider
   */
  public String getProvider();

  /**
   * Get the plugin web page URL
   * @return the plugin web page URL
   */
  public String getWebPageURL();

  /**
   * Get the plugin contact email
   * @return the plugin contact email
   */
  public String getContact();
}
