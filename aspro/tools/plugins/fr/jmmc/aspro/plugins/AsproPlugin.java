/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.plugins;

import fr.jmmc.aspro.feature.AsproFeatures;
import java.util.List;
import java.util.Map;

/**
 * This interface extends the Plugin interface to add specific Aspro behaviour
 * @author bourgesl
 */
public interface AsproPlugin extends Plugin {
  /* TODO: should use enum instead of string but less portable ! */

  /* interferometer with its instruments */
  /** CHARA interferometer */
  String INTERFEROMETER_CHARA = "CHARA";
  /** CHARA CLASSIC instrument */
  String INSTRUMENT_CHARA_CLASSIC = "CLASSIC";
  /** CHARA CLIMB instrument */
  String INSTRUMENT_CHARA_CLIMB = "CLIMB";
  /** CHARA MIRC instrument */
  String INSTRUMENT_CHARA_MIRC = "MIRC";
  /** CHARA MIRC_5T instrument */
  String INSTRUMENT_CHARA_MIRC_5T = "MIRC_5T";
  /** CHARA MIRC_6T instrument */
  String INSTRUMENT_CHARA_MIRC_6T = "MIRC_6T";
  /** CHARA PAVO_2T instrument */
  String INSTRUMENT_CHARA_PAVO_2T = "PAVO_2T";
  /** CHARA PAVO_3T instrument */
  String INSTRUMENT_CHARA_PAVO_3T = "PAVO_3T";
  /** CHARA VEGA_2T instrument */
  String INSTRUMENT_CHARA_VEGA_2T = "VEGA_2T";
  /** CHARA VEGA_3T instrument */
  String INSTRUMENT_CHARA_VEGA_3T = "VEGA_3T";
  /** CHARA VEGA_4T instrument */
  String INSTRUMENT_CHARA_VEGA_4T = "VEGA_4T";
  /** VLTI interferometer */
  String INTERFEROMETER_VLTI = "VLTI";
  /** VLTI AMBER instrument */
  String INSTRUMENT_VLTI_AMBER = "AMBER";
  /** VLTI MIDI instrument */
  String INSTRUMENT_VLTI_MIDI = "MIDI";
  /** VLTI PIONIER instrument */
  String INSTRUMENT_VLTI_PIONIER = "PIONIER";

  /**
   * Get the optional list of interferometer identifiers when this plugin is applicable to; 
   * null value means that this plugin is applicable to any interferometer
   * 
   * @return the optional list of interferometer identifiers
   */
  public List<String> getApplicableInterferometer();

  /**
   * Get the optional list of instruments identifiers when this plugin is applicable to; 
   * null value means that this plugin is applicable to any interferometer
   * 
   * @return the optional list of interferometer identifiers
   */
  public List<String> getApplicableInstrument();

  /**
   * Get the map of AsproFeaturePlugin implementation classes keyed by AsproPluginFeatures enum value
   * @return map of AsproFeaturePlugin implementation classes keyed by AsproPluginFeatures enum value
   */
  public Map<AsproFeatures, Class<? extends AsproFeaturePlugin>> getAsproFeaturePlugins();
}
