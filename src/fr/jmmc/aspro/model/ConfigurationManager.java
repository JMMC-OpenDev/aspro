/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.oi.AzEl;
import fr.jmmc.aspro.model.oi.Channel;
import fr.jmmc.aspro.model.oi.Configurations;
import fr.jmmc.aspro.model.oi.FocalInstrument;
import fr.jmmc.aspro.model.oi.FocalInstrumentConfiguration;
import fr.jmmc.aspro.model.oi.FocalInstrumentConfigurationItem;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.FringeTracker;
import fr.jmmc.aspro.model.oi.HorizonProfile;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.InterferometerSetting;
import fr.jmmc.aspro.model.oi.LonLatAlt;
import fr.jmmc.aspro.model.oi.Pop;
import fr.jmmc.aspro.model.oi.Position3D;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.StationLinks;
import fr.jmmc.aspro.service.GeocentricCoords;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class manages configuration files for the Interferometer configurations
 * @author bourgesl
 */
public final class ConfigurationManager extends BaseOIManager {

  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(ConfigurationManager.class.getName());
  /** Configurations file name */
  private static final String CONF_FILE = "AsproOIConfigurations.xml";
  /** singleton pattern */
  private static volatile ConfigurationManager instance = null;

  /* members */
  /** Map : id, interferometer description */
  private final Map<String, InterferometerDescription> interferometerDescriptions = new LinkedHashMap<String, InterferometerDescription>();
  /** Map : id, interferometer configuration */
  private final Map<String, InterferometerConfiguration> interferometerConfigurations = new LinkedHashMap<String, InterferometerConfiguration>();
  /** default horizon profile */
  private HorizonProfile defaultHorizon = null;

  /**
   * Return the ConfigurationManager singleton
   * @return ConfigurationManager singleton
   *
   * @throws IllegalStateException if the configuration files are not found or IO failure
   * @throws IllegalArgumentException if the load configuration failed
   */
  public static synchronized final ConfigurationManager getInstance()
          throws IllegalStateException, IllegalArgumentException {

    if (instance == null) {
      final ConfigurationManager cm = new ConfigurationManager();

      // can throw RuntimeException :
      cm.initialize();

      instance = cm;
    }
    return instance;
  }

  /**
   * Private constructor
   */
  private ConfigurationManager() {
    super();
  }

  /**
   * Initialize the configuration :
   * - load AsproOIConfigurations.xml to get configuration file paths
   * - load those files (InterferometerSetting)
   * - update interferometer description and configuration maps
   * 
   * @throws IllegalStateException if the configuration files are not found or IO failure
   * @throws IllegalArgumentException if the load configuration failed
   */
  private void initialize()
          throws IllegalStateException, IllegalArgumentException {

    final long start = System.nanoTime();

    final Configurations conf = (Configurations) loadObject(CONF_FILE);

    InterferometerSetting is;
    for (String fileName : conf.getFiles()) {
      logger.info("initialize : loading configuration file = {}", fileName);

      is = (InterferometerSetting) loadObject(fileName);

      addInterferometerSetting(is);
    }

    if (logger.isInfoEnabled()) {
      logger.info("initialize : duration = {} ms.", 1e-6d * (System.nanoTime() - start));
    }

    if (logger.isDebugEnabled()) {
      logger.debug("descriptions   : {}", getInterferometerDescriptions());
      logger.debug("configurations : {}", getInterferometerConfigurations());
    }
  }

  /**
   * Add a new interferometer configuration in the cache
   * and compute transient information (long/lat and max uv coverage)
   * @param is interferometer setting
   */
  private void addInterferometerSetting(final InterferometerSetting is) {

    final InterferometerDescription id = is.getDescription();

    // check if the interferometer is unique (name) :
    if (interferometerDescriptions.containsKey(id.getName())) {
      throw new IllegalStateException("The interferometer '" + id.getName() + "' is already present in the loaded configuration !");
    }

    computeInterferometerLocation(id);
    computeInstrumentWaveLengthRange(id);

    // check instrument modes (spectral channels):
    // TODO: handle properly spectral channels (rebinning):
    for (FocalInstrument instrument : id.getFocalInstruments()) {
      for (FocalInstrumentMode insMode : instrument.getModes()) {
        if (insMode.getNumberChannels() != null) {
          if (insMode.getSpectralChannels() == insMode.getNumberChannels()) {
            logger.warn("Instrument [{}] mode [{}] useless numberChannels: {}", new Object[]{
                      instrument.getName(), insMode.getName(), insMode.getNumberChannels()});
          } else {
            logger.warn("Instrument [{}] mode [{}] channel configuration: {} / {}", new Object[]{
                      instrument.getName(), insMode.getName(), insMode.getNumberChannels(), insMode.getSpectralChannels()});
          }
        }
      }
    }

    adjustStationHorizons(id.getStations());

    interferometerDescriptions.put(id.getName(), id);

    for (InterferometerConfiguration c : is.getConfigurations()) {
      interferometerConfigurations.put(getConfigurationName(c), c);

      computeBaselineUVWBounds(c);

      // reverse mapping :
      // declare interferometer configurations in the interferometer description
      is.getDescription().getConfigurations().add(c);
    }
  }

  /**
   * Compute the spherical coordinates for the interferometer
   * @param id interferometer description
   */
  private void computeInterferometerLocation(final InterferometerDescription id) {

    // Interferometer center :
    final Position3D center = id.getPosition();
    final LonLatAlt posSph = GeocentricCoords.getLonLatAlt(center);

    id.setPosSph(posSph);

    if (logger.isDebugEnabled()) {
      GeocentricCoords.dump(id.getName(), posSph);
    }
  }

  /**
   * Compute the lower and upper wave length of every instrument
   * @param id interferometer description
   */
  private void computeInstrumentWaveLengthRange(final InterferometerDescription id) {
    for (FocalInstrument instrument : id.getFocalInstruments()) {
      instrument.defineWaveLengthRange();

      if (logger.isDebugEnabled()) {
        logger.debug("Instrument [{}] - wavelengths [{} - {}]",
                new Object[]{instrument.getName(), instrument.getWaveLengthMin(), instrument.getWaveLengthMax()});
      }
    }
  }

  /**
   * Compute the min and max baseline length (m) using all instrument baselines of the given interferometer configuration
   * @param intConf interferometer configuration
   */
  private void computeBaselineUVWBounds(final InterferometerConfiguration intConf) {

    double maxUV = 0d;
    double minUV = Double.POSITIVE_INFINITY;

    final double[] minMax = new double[2];

    // for each instrument:
    for (FocalInstrumentConfiguration insConf : intConf.getInstruments()) {

      // for each instrument configuration:
      for (FocalInstrumentConfigurationItem c : insConf.getConfigurations()) {

        computeBaselineUVWBounds(c.getStations(), minMax);

        minUV = Math.min(minUV, minMax[0]);
        maxUV = Math.max(maxUV, minMax[1]);
      }
    }

    if (logger.isDebugEnabled()) {
      logger.debug("computeBaselineUVWBounds = [{} - {}] m for configuration {}", new Object[]{minUV, maxUV, intConf.getName()});
    }

    intConf.setMinBaseLine(minUV);
    intConf.setMaxBaseLine(maxUV);
  }

  /**
   * Compute the min and max baseline length (XY distance i.e. projected in the UV plane) using all possible baselines
   * for given stations
   * @param stations list of stations to determine baselines
   * @return min - max
   */
  public static double[] computeBaselineUVBounds(final List<Station> stations) {
    final double[] minMax = new double[2];

    computeBaselineUVBounds(stations, minMax);

    return minMax;
  }

  /**
   * Compute the min and max baseline length (XY distance i.e. projected in the UV plane) using all possible baselines
   * for given stations
   * @param stations list of stations to determine baselines
   * @param minMax double[min; max]
   */
  public static void computeBaselineUVBounds(final List<Station> stations, final double[] minMax) {
    double maxUV = 0d;
    double minUV = Double.POSITIVE_INFINITY;

    final int size = stations.size();

    double x, y, distXY;
    Station s1, s2;
    for (int i = 0; i < size; i++) {
      s1 = stations.get(i);
      for (int j = i + 1; j < size; j++) {
        s2 = stations.get(j);

        x = s2.getRelativePosition().getPosX() - s1.getRelativePosition().getPosX();
        y = s2.getRelativePosition().getPosY() - s1.getRelativePosition().getPosY();

        distXY = Math.sqrt(x * x + y * y);

        minUV = Math.min(minUV, distXY);
        maxUV = Math.max(maxUV, distXY);
      }
    }

    if (logger.isDebugEnabled()) {
      logger.debug("computeBaselineUVBounds = [{} - {}] m for stations {}", new Object[]{minUV, maxUV, stations});
    }

    minMax[0] = minUV;
    minMax[1] = maxUV;
  }

  /**
   * Compute the min and max baseline vector length (UVW) using all possible baselines
   * for given stations
   * @param stations list of stations to determine baselines
   * @param minMax double[min; max]
   */
  public static void computeBaselineUVWBounds(final List<Station> stations, final double[] minMax) {
    double maxUV = 0d;
    double minUV = Double.POSITIVE_INFINITY;

    final int size = stations.size();

    double x, y, z, distXYZ;
    Station s1, s2;
    for (int i = 0; i < size; i++) {
      s1 = stations.get(i);
      for (int j = i + 1; j < size; j++) {
        s2 = stations.get(j);

        x = s2.getRelativePosition().getPosX() - s1.getRelativePosition().getPosX();
        y = s2.getRelativePosition().getPosY() - s1.getRelativePosition().getPosY();
        z = s2.getRelativePosition().getPosZ() - s1.getRelativePosition().getPosZ();

        distXYZ = Math.sqrt(x * x + y * y + z * z);

        minUV = Math.min(minUV, distXYZ);
        maxUV = Math.max(maxUV, distXYZ);
      }
    }

    if (logger.isDebugEnabled()) {
      logger.debug("computeBaselineUVWBounds = [{} - {}] m for stations {}", new Object[]{minUV, maxUV, stations});
    }

    minMax[0] = minUV;
    minMax[1] = maxUV;
  }

  /**
   * Adjust the station horizons to respect the maximum elevation limit (85 deg for CHARA)
   * @param stations station to update
   */
  private void adjustStationHorizons(final List<Station> stations) {
    double maxElev;
    for (Station station : stations) {
      logger.debug("station: {}", station);

      // maximum elevation in degrees per telescope :
      maxElev = station.getTelescope().getMaxElevation();

      if (station.getHorizon() != null && !station.getHorizon().getPoints().isEmpty()) {
        // horizon is defined : check elevation
        for (AzEl point : station.getHorizon().getPoints()) {
          if (point.getElevation() > maxElev) {
            if (logger.isDebugEnabled()) {
              logger.debug("station: {} : fix point: {}", station, point);
            }
            point.setElevation(maxElev);
          }
        }

      } else {
        // missing horizon :
        if (defaultHorizon == null) {
          final HorizonProfile horizon = new HorizonProfile();
          final List<AzEl> points = horizon.getPoints();

          points.add(new AzEl(360d, 0d));
          points.add(new AzEl(0d, 0d));
          points.add(new AzEl(0d, maxElev));
          points.add(new AzEl(360d, maxElev));

          defaultHorizon = horizon;
        }

        logger.debug("station: {} use default horizon", station);

        // define fake horizon :
        station.setHorizon(defaultHorizon);
      }
    }
  }

  /**
   * Compute the name of the interferometer configuration according to the associated interferometer and the optional version.
   * Store this name in the interferometer configuration in the name field (xml id)
   * @param ic configuration
   * @return name of the interferometer configuration
   */
  private String getConfigurationName(final InterferometerConfiguration ic) {
    // compute configuration name if missing :
    String name = ic.getName();
    if (name == null) {
      // interferometer name is an id :
      name = ic.getInterferometer().getName();

      if (ic.getVersion() != null) {
        name += " " + ic.getVersion();
      }
      ic.setName(name);
    }
    return name;
  }

  /**
   * Return the interferometer description map keyed by name
   * @return interferometer description map
   */
  private Map<String, InterferometerDescription> getInterferometerDescriptions() {
    return interferometerDescriptions;
  }

  /**
   * Return the interferometer description for the given name
   * @param name interferometer name
   * @return interferometer description or null if not found
   */
  public InterferometerDescription getInterferometerDescription(final String name) {
    return getInterferometerDescriptions().get(name);
  }

  /**
   * Return the list of all interferometer names
   * @return list of all interferometer names
   */
  public Vector<String> getInterferometerNames() {
    final Vector<String> v = new Vector<String>(getInterferometerDescriptions().size());
    for (InterferometerDescription id : getInterferometerDescriptions().values()) {
      v.add(id.getName());
    }
    return v;
  }

  /**
   * Return the list of interferometer configurations associated to the given interferometer
   * @param interferometerName name of the interferometer
   * @return list of interferometer configurations
   */
  public Vector<String> getInterferometerConfigurationNames(final String interferometerName) {
    final InterferometerDescription id = getInterferometerDescription(interferometerName);
    if (id != null) {
      final Vector<String> v = new Vector<String>(id.getConfigurations().size());
      for (InterferometerConfiguration c : id.getConfigurations()) {
        v.add(c.getName());
      }
      return v;
    }
    return EMPTY_VECTOR;
  }

  /**
   * Return the switchyard links for the given station
   * @param id interferometer description
   * @param station station
   * @return switchyard links or null
   */
  public StationLinks getStationLinks(final InterferometerDescription id, final Station station) {
    if (id.getSwitchyard() != null) {
      for (StationLinks sl : id.getSwitchyard().getStationLinks()) {
        if (sl.getStation().equals(station)) {
          return sl;
        }
      }
    }
    return null;
  }

  /**
   * Indicate if the given interferometer has PoPs
   * @param interferometerName name of the interferometer
   * @return true if the interferometer description has PoPs
   */
  public boolean hasPoPs(final String interferometerName) {
    final InterferometerDescription id = getInterferometerDescription(interferometerName);
    if (id != null) {
      return !id.getPops().isEmpty();
    }
    return false;
  }

  /* InterferometerConfiguration */
  /**
   * Return the interferometer configuration map keyed by name
   * @return interferometer configuration map
   */
  private Map<String, InterferometerConfiguration> getInterferometerConfigurations() {
    return interferometerConfigurations;
  }

  /**
   * Return the interferometer configuration for the given name
   * @param name name of the interferometer configuration
   * @return interferometer configuration or null if not found
   */
  public InterferometerConfiguration getInterferometerConfiguration(final String name) {
    return getInterferometerConfigurations().get(name);
  }
  
  /**
   * Return the first instrument configuration having the given instrument name
   * @param instrumentName name of the instrument
   * @return interferometer configuration or null if not found
   */
  public InterferometerConfiguration getInterferometerConfigurationWithInstrument(final String instrumentName) {
    for (InterferometerConfiguration c : getInterferometerConfigurations().values()) {
      for (FocalInstrumentConfiguration ic : c.getInstruments()) {
        if (ic.getFocalInstrument().getName().equals(instrumentName)) {
          return c;
        }
      }
    }
    return null;
  }

  /**
   * Return the list of all instrument names available for the given configuration
   * @param configurationName name of the interferometer configuration
   * @return list of all instrument names
   */
  public Vector<String> getInterferometerInstrumentNames(final String configurationName) {
    final InterferometerConfiguration c = getInterferometerConfiguration(configurationName);
    if (c != null) {
      final Vector<String> v = new Vector<String>(c.getInstruments().size());
      for (FocalInstrumentConfiguration ic : c.getInstruments()) {
        v.add(ic.getFocalInstrument().getName());
      }
      return v;
    }
    return EMPTY_VECTOR;
  }

  /**
   * Return the instrument configuration for the given interferometer configuration and instrument name
   * @param configurationName name of the interferometer configuration
   * @param instrumentName name of the instrument
   * @return focal instrument configuration
   */
  public FocalInstrumentConfiguration getInterferometerInstrumentConfiguration(final String configurationName, final String instrumentName) {
    final InterferometerConfiguration c = getInterferometerConfiguration(configurationName);
    if (c != null) {
      for (FocalInstrumentConfiguration ic : c.getInstruments()) {
        if (ic.getFocalInstrument().getName().equals(instrumentName)) {
          return ic;
        }
      }
    }
    return null;
  }

  /**
   * Return the instrument for the given interferometer configuration and instrument name
   * @param configurationName name of the interferometer configuration
   * @param instrumentName name of the instrument
   * @return focal instrument
   */
  public FocalInstrument getInterferometerInstrument(final String configurationName, final String instrumentName) {
    final FocalInstrumentConfiguration ic = getInterferometerInstrumentConfiguration(configurationName, instrumentName);
    if (ic != null) {
      return ic.getFocalInstrument();
    }
    return null;
  }

  /**
   * Return the list of all instrument configuration names (station list) for the given interferometer configuration and instrument name
   * @param configurationName name of the interferometer configuration
   * @param instrumentName name of the instrument
   * @return list of all instrument configuration names
   */
  public Vector<String> getInstrumentConfigurationNames(final String configurationName, final String instrumentName) {
    final FocalInstrumentConfiguration ic = getInterferometerInstrumentConfiguration(configurationName, instrumentName);
    if (ic != null) {
      final Vector<String> v = new Vector<String>(ic.getConfigurations().size());
      for (FocalInstrumentConfigurationItem c : ic.getConfigurations()) {
        v.add(c.getName());
      }
      return v;
    }
    return EMPTY_VECTOR;
  }

  /**
   * Return the list of stations for the given interferometer configuration, instrument name and instrument configuration
   * @param configurationName name of the interferometer configuration
   * @param instrumentName name of the instrument
   * @param instrumentConfigurationName name of the instrument configuration
   * @return list of stations
   */
  public List<Station> getInstrumentConfigurationStations(final String configurationName, final String instrumentName, final String instrumentConfigurationName) {
    final FocalInstrumentConfiguration ic = getInterferometerInstrumentConfiguration(configurationName, instrumentName);
    if (ic != null) {
      for (FocalInstrumentConfigurationItem c : ic.getConfigurations()) {
        if (c.getName().equals(instrumentConfigurationName)) {
          return c.getStations();
        }
      }
    }
    return null;
  }

  /**
   * Return the (optional) list of instrument channels for the given interferometer configuration, instrument name and instrument configuration
   * @param configurationName name of the interferometer configuration
   * @param instrumentName name of the instrument
   * @param instrumentConfigurationName name of the instrument configuration
   * @return list of instrument channels or null if undefined
   */
  public List<Channel> getInstrumentConfigurationChannels(final String configurationName, final String instrumentName, final String instrumentConfigurationName) {
    final FocalInstrumentConfiguration ic = getInterferometerInstrumentConfiguration(configurationName, instrumentName);
    if (ic != null) {
      for (FocalInstrumentConfigurationItem c : ic.getConfigurations()) {
        if (c.getName().equals(instrumentConfigurationName)) {
          return c.getChannels();
        }
      }
    }
    return null;
  }

  /**
   * Return the (optional) list of Pops for the given interferometer configuration, instrument name and instrument configuration
   * @param configurationName name of the interferometer configuration
   * @param instrumentName name of the instrument
   * @param instrumentConfigurationName name of the instrument configuration
   * @return list of PoPs or null if undefined
   */
  public List<Pop> getInstrumentConfigurationPoPs(final String configurationName, final String instrumentName, final String instrumentConfigurationName) {
    final FocalInstrumentConfiguration ic = getInterferometerInstrumentConfiguration(configurationName, instrumentName);
    if (ic != null) {
      for (FocalInstrumentConfigurationItem c : ic.getConfigurations()) {
        if (c.getName().equals(instrumentConfigurationName)) {
          return c.getPops();
        }
      }
    }
    return null;
  }

  /**
   * Return the list of all fringe tracker modes available for the given interferometer configuration and instrument name
   * @param configurationName name of the interferometer configuration
   * @param instrumentName name of the instrument
   * @return list of all fringe tracker modes
   */
  public Vector<String> getFringeTrackerModes(final String configurationName, final String instrumentName) {
    final FocalInstrument ins = getInterferometerInstrument(configurationName, instrumentName);
    if (ins != null) {
      final boolean ftOptional = (ins.isFringeTrackerRequired() == null || !ins.isFringeTrackerRequired().booleanValue());
      final FringeTracker ft = ins.getFringeTracker();
      if (ft != null) {
        final Vector<String> v = new Vector<String>(ft.getModes().size() + ((ftOptional) ? 1 : 0));
        if (ftOptional) {
          v.add(AsproConstants.NONE);
        }
        v.addAll(ft.getModes());
        return v;
      }
    }
    return EMPTY_VECTOR;
  }

  /**
   * Return the default sampling time for the given interferometer configuration and instrument name
   * @param configurationName name of the interferometer configuration
   * @param instrumentName name of the instrument
   * @return default sampling time
   */
  public int getInstrumentSamplingTime(final String configurationName, final String instrumentName) {
    final FocalInstrument ins = getInterferometerInstrument(configurationName, instrumentName);
    if (ins != null) {
      return ins.getDefaultSamplingTime();
    }
    return -1;
  }

  /**
   * Return the list of all instrument modes (spectral configuration) for the given interferometer configuration and instrument name
   * @param configurationName name of the interferometer configuration
   * @param instrumentName name of the instrument
   * @return list of all instrument modes
   */
  public Vector<String> getInstrumentModes(final String configurationName, final String instrumentName) {
    final FocalInstrument ins = getInterferometerInstrument(configurationName, instrumentName);
    if (ins != null) {
      final Vector<String> v = new Vector<String>(ins.getModes().size());
      for (FocalInstrumentMode m : ins.getModes()) {
        v.add(m.getName());
      }
      return v;
    }
    return EMPTY_VECTOR;
  }

  /**
   * Return the instrument mode for the given interferometer configuration, instrument name and mode
   * @param configurationName name of the interferometer configuration
   * @param instrumentName name of the instrument
   * @param instrumentMode instrument mode
   * @return instrument mode or null
   */
  public FocalInstrumentMode getInstrumentMode(final String configurationName, final String instrumentName, final String instrumentMode) {
    if (instrumentMode != null && instrumentMode.length() > 0) {
      final FocalInstrument ins = getInterferometerInstrument(configurationName, instrumentName);
      if (ins != null) {
        for (FocalInstrumentMode m : ins.getModes()) {
          if (m.getName().equals(instrumentMode)) {
            return m;
          }
        }
      }
    }
    return null;
  }

  /**
   * Parse and return the list of PoPs for the given interferometer configuration, instrument name and Pops string.
   * The Pops string must only contain PoP indexes like '12', '111' or '541'.
   * The length of this string must respect the number of channels of the instrument.
   * @param configurationName name of the interferometer configuration
   * @param instrumentName name of the instrument
   * @param configPoPs Pops string
   * @return list of PoPs
   */
  public List<Pop> parseInstrumentPoPs(final String configurationName, final String instrumentName, final String configPoPs) {
    if (configPoPs != null && configPoPs.length() > 0) {
      final FocalInstrument ins = getInterferometerInstrument(configurationName, instrumentName);
      if (ins != null) {
        // number of channels :
        final int numChannels = ins.getNumberChannels();

        if (configPoPs.length() == numChannels) {
          // valid length :

          final InterferometerConfiguration c = getInterferometerConfiguration(configurationName);
          if (c != null) {
            final List<Pop> listPoPs = c.getInterferometer().getPops();

            final List<Pop> config = new ArrayList<Pop>(numChannels);

            int idx;
            for (char ch : configPoPs.toCharArray()) {
              idx = Character.digit(ch, 10);
              if (idx <= 0) {
                return null;
              }
              for (Pop pop : listPoPs) {
                if (pop.getIndex() == idx) {
                  config.add(pop);
                  break;
                }
              }
            }
            // check if all given numbers are valid (16 is invalid !) :
            if (config.size() == numChannels) {
              return config;
            }
          }
        }
      }
    }
    return null;
  }
}
