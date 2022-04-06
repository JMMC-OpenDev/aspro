/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.Configurations;
import fr.jmmc.aspro.model.oi.InterferometerFile;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 *
 * @author bourgesl
 */
public final class UserConfigurations {

    /** Current revision of the Aspro User Configuration */
    public final static String CURRENT_VERSION = "2022.04";

    // members:
    /** current Configurations (xml) */
    private Configurations configurations = null;
    /** Map : file path, interferometer configuration (insertion order, reproductible) */
    private final Map<String, Configuration> loadedConfigurations = new LinkedHashMap<String, Configuration>(4);

    UserConfigurations() {
        super();
    }

    public Configurations getConfigurations() {
        return configurations;
    }

    public void setConfigurations(final Configurations configurations) {
        this.configurations = configurations;
    }

    public Configurations getOrCreateConfigurations() {
        if (!hasConfigurations()) {
            final Configurations conf = new Configurations();
            conf.setMinVersion(CURRENT_VERSION);
            conf.setChanged(true);
            setConfigurations(conf);
        }
        return getConfigurations();
    }

    public boolean hasConfigurations() {
        return (getConfigurations() != null);
    }

    public boolean isChanged() {
        return hasConfigurations() && getConfigurations().isChanged();
    }

    public void setChanged(boolean changed) {
        if (hasConfigurations()) {
            getConfigurations().setChanged(changed);
        }
    }

    public List<InterferometerFile> getInterferometerFiles() {
        return (hasConfigurations()) ? getConfigurations().getInterferometerFiles() : null;
    }

    public InterferometerFile getInterferometerFile(final String filePath) {
        final List<InterferometerFile> files = getInterferometerFiles();
        if (files != null) {
            for (InterferometerFile fileRef : files) {
                if (fileRef.getFile().equals(filePath)) {
                    return fileRef;
                }
            }
        }
        return null;
    }

    /**
     * Return the Configuration map keyed by file path
     * @return Configuration map
     */
    private Map<String, Configuration> getLoadedConfigurationsMap() {
        return loadedConfigurations;
    }

    /**
     * Get the Configuration given its file path
     * @param filePath file path
     * @return Configuration
     */
    public Configuration getLoadedConfiguration(final String filePath) {
        return getLoadedConfigurationsMap().get(filePath);
    }

    /**
     * Add the given Configuration for the given file path
     * @param filePath file path
     * @param configuration Configuration to add
     */
    public void addLoadedConfiguration(final String filePath, final Configuration configuration) {
        getLoadedConfigurationsMap().put(filePath, configuration);
    }

    /**
     * Remove the Configuration for the given file path
     * @param filePath file path
     * @return true if removed; false otherwise
     */
    public boolean removeLoadedConfiguration(final String filePath) {
        return (getLoadedConfigurationsMap().remove(filePath) != null);
    }

    @Override
    public String toString() {
        return "UserConfigurations{"
                + "changed: " + isChanged()
                + ", files: " + getInterferometerFiles()
                + "\nloadedConfigurations: " + loadedConfigurations
                + "\n}";
    }

}
