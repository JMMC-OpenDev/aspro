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
    /** Map : id, interferometer configuration (insertion order, reproductible) */
    private final Map<String, InterferometerConfiguration> interferometerConfigurations = new LinkedHashMap<String, InterferometerConfiguration>();
    /* cached state */
    /** interferometer names */
    private Vector<String> interferometerNames = null;
    /** Map: id, interferometer configuration names */
    private Map<String, Vector<String>> interferometerConfigurationNames = null;

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
        interferometerNames = null;
        interferometerConfigurationNames = null;
    }

    /**
     * Return the interferometer description map keyed by name
     * @return interferometer description map
     */
    private Map<String, InterferometerDescription> getInterferometerDescriptionMap() {
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
     * @param interferometerName interferometer name
     * @return interferometer description or null if not found
     */
    public InterferometerDescription getInterferometerDescription(final String interferometerName) {
        return getInterferometerDescriptionMap().get(interferometerName);
    }

    /**
     * @param interferometerName interferometer name
     * @return true if an interferometer description is present for the given name
     */
    public boolean hasInterferometerDescription(final String interferometerName) {
        return getInterferometerDescriptionMap().containsKey(interferometerName);
    }

    /**
     * Add the interferometer description
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
        if (names == null) {
            names = new Vector<String>();
            // ordered:
            for (InterferometerDescription id : getInterferometerDescriptions()) {
                names.add(id.getName());
            }
            this.interferometerNames = names;
        }
        return names;
    }

    /**
     * Return the interferometer configuration map keyed by name
     * @return interferometer configuration map
     */
    private Map<String, InterferometerConfiguration> getInterferometerConfigurationMap() {
        return interferometerConfigurations;
    }

    /**
     * Return the interferometer configuration collection
     * @return interferometer configuration collection
     */
    public Collection<InterferometerConfiguration> getInterferometerConfigurations() {
        return getInterferometerConfigurationMap().values();
    }

    /**
     * Return the interferometer configuration for the given name
     * @param configurationName interferometer configuration name
     * @return interferometer configuration or null if not found
     */
    public InterferometerConfiguration getInterferometerConfiguration(final String configurationName) {
        return getInterferometerConfigurationMap().get(configurationName);
    }

    /**
     * @param configurationName interferometer configuration name
     * @return true if an interferometer configuration is present for the given name
     */
    public boolean hasInterferometerConfiguration(final String configurationName) {
        return getInterferometerConfigurationMap().containsKey(configurationName);
    }

    /**
     * Add the interferometer configuration
     * @see ConfigurationManager#computeConfigurationName(fr.jmmc.aspro.model.oi.InterferometerConfiguration) 
     * @param ic interferometer configuration
     */
    public void addInterferometerConfiguration(final InterferometerConfiguration ic) {
        getInterferometerConfigurationMap().put(ic.getName(), ic);
    }

    /**
     * Return the list of interferometer configurations associated to the given interferometer
     * @param interferometerName name of the interferometer
     * @return list of interferometer configurations
     */
    public Vector<String> getInterferometerConfigurationNames(final String interferometerName) {
        // lazy:
        if (interferometerConfigurationNames == null) {
            this.interferometerConfigurationNames = new HashMap<String, Vector<String>>();
        }

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

    @Override
    public String toString() {
        return "Configuration{"
                + "\ndescriptions: " + interferometerDescriptions
                + "\nconfigurations: " + interferometerConfigurations
                + "\n}";
    }
}
