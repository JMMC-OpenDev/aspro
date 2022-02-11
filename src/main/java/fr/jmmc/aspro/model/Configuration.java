/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Vector;

/**
 * Configuration holder used to override default configuration
 *
 * @author bourgesl
 */
public final class Configuration {

    /** Map : id, interferometer description (insertion order, reproductible) */
    private final Map<String, InterferometerDescription> interferometerDescriptions = new LinkedHashMap<String, InterferometerDescription>();
    /** interferometer names */
    private final Vector<String> interferometerNames = new Vector<String>();
    /** Map : id, interferometer configuration (insertion order, reproductible) */
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
        interferometerNames.clear();
        interferometerConfigurations.clear();
        interferometerConfigurationNames.clear();
    }

    /**
     * Return the interferometer description map keyed by name
     * @return interferometer description map
     */
    Map<String, InterferometerDescription> getInterferometerDescriptionMap() {
        return interferometerDescriptions;
    }

    /**
     * Return the interferometer description collection
     * @return interferometer description collection
     */
    public Collection<InterferometerDescription> getInterferometerDescriptions() {
        return getInterferometerDescriptionMap().values();
    }

    /**
     * Return the interferometer description for the given name
     * @param name interferometer name
     * @return interferometer description or null if not found
     */
    public InterferometerDescription getInterferometerDescription(final String name) {
        return getInterferometerDescriptionMap().get(name);
    }

    /**
     * @param name interferometer name
     * @return true if an interferometer description is present for the given name
     */
    public boolean hasInterferometerDescription(final String name) {
        return getInterferometerDescriptionMap().containsKey(name);
    }

    /**
     * Add the interferometer description for the given name
     * @param id interferometer description
     */
    public void addInterferometerDescription(final InterferometerDescription id) {
        getInterferometerDescriptionMap().put(id.getName(), id);
    }

    /**
     * Return the list of all interferometer names
     * @return list of all interferometer names
     */
    public Vector<String> getInterferometerNames() {
        Vector<String> names = interferometerNames;

        // lazily prepare list:
        if (names.isEmpty()) {
            // ordered:
            for (InterferometerDescription id : getInterferometerDescriptions()) {
                names.add(id.getName());
            }
        }
        return names;
    }

    /**
     * Return the interferometer configuration map keyed by name
     * @return interferometer configuration map
     */
    Map<String, InterferometerConfiguration> getInterferometerConfigurationMap() {
        return interferometerConfigurations;
    }

    /**
     * Return the interferometer configuration collection
     * @return interferometer configuration collection
     */
    public Collection<InterferometerConfiguration> getInterferometerConfigurations() {
        return interferometerConfigurations.values();
    }

    /**
     * Return the interferometer configuration for the given name
     * @param name interferometer configuration name
     * @return interferometer configuration or null if not found
     */
    public InterferometerConfiguration getInterferometerConfiguration(final String name) {
        return getInterferometerConfigurationMap().get(name);
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
            for (InterferometerDescription id : getInterferometerDescriptions()) {
                interferometerConfigurationNames.put(id.getName(), new Vector<String>());
            }

            // ordered:
            for (InterferometerConfiguration ic : getInterferometerConfigurations()) {
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
