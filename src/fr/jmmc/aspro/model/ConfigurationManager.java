/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ConfigurationManager.java,v 1.10 2009-12-04 16:26:58 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.9  2009/12/04 15:38:27  bourgesl
 * Added Save action in the menu bar
 *
 * Revision 1.8  2009/11/20 16:55:47  bourgesl
 * Added Beam / Delay Line definition
 * ObservabilityService is stateless to simplify coding
 *
 * Revision 1.7  2009/11/17 17:00:28  bourgesl
 * chosen instrument configuration propagated to observation
 *
 * Revision 1.6  2009/11/05 12:59:39  bourgesl
 * first simple source observability (only min elevation condition)
 *
 * Revision 1.5  2009/10/22 15:47:22  bourgesl
 * beginning of observability computation with jSkyCalc
 *
 * Revision 1.4  2009/10/20 13:08:51  bourgesl
 * ObservationManager has methods to store observation properties
 *
 * Revision 1.3  2009/10/16 15:25:30  bourgesl
 * removed jaxb header + XYZ coords to Long/Lat/Alt for interferometer + stations
 *
 * Revision 1.2  2009/10/14 15:54:38  bourgesl
 * added basicObservationForm + CHARA.xml
 *
 * Revision 1.1  2009/10/13 16:04:14  bourgesl
 * Basic ConfigurationManager to load interferometer configuration file
 *
 * Revision 1.1  2009/09/21 15:38:51  bourgesl
 * initial jmcs gui + jaxb loader
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.Channel;
import fr.jmmc.aspro.model.oi.Configurations;
import fr.jmmc.aspro.model.oi.FocalInstrumentConfiguration;
import fr.jmmc.aspro.model.oi.FocalInstrumentConfigurationItem;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.InterferometerSetting;
import fr.jmmc.aspro.model.oi.LonLatAlt;
import fr.jmmc.aspro.model.oi.Position3D;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.StationLinks;
import fr.jmmc.aspro.service.GeocentricCoords;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.util.logging.Level;
import uk.ac.starlink.pal.Spherical;

/**
 * This class manages configuration files for the Interferometer configurations
 * @author bourgesl
 */
public class ConfigurationManager extends BaseOIManager {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.model.ConfigurationManager";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** Configurations file name */
  private static final String CONF_FILE = "AsproOIConfigurations.xml";
  /** singleton pattern */
  private static ConfigurationManager instance = new ConfigurationManager();
  // members :
  /** Map : id, interferometer description */
  private final Map<String, InterferometerDescription> interferometerDescriptions = new HashMap<String, InterferometerDescription>();
  /** Map : id, interferometer configuration */
  private final Map<String, InterferometerConfiguration> interferometerConfigurations = new HashMap<String, InterferometerConfiguration>();

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
    try {
      final Configurations conf = (Configurations) loadObject(CONF_FILE);

      InterferometerSetting is;
      for (String fileName : conf.getFiles()) {
        if (logger.isLoggable(Level.CONFIG)) {
          logger.config("ConfigurationManager : loading configuration file = " + fileName);
        }
        is = (InterferometerSetting) loadObject(fileName);

        addInterferometerSetting(is);
      }

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("ConfigurationManager : descriptions   = " + getInterferometerDescriptions());
        logger.fine("ConfigurationManager : configurations = " + getInterferometerConfigurations());
      }
    } catch (RuntimeException re) {
      logger.log(Level.SEVERE, "runtime failure : ", re);
    }
  }

  private void addInterferometerSetting(final InterferometerSetting is) {

    final InterferometerDescription d = is.getDescription();

    // check if the interferoemeter is unique :
    if (interferometerDescriptions.containsKey(d.getName())) {
      throw new IllegalStateException("This interferometer is already present in the loaded configuration !");
    }

    computeSphericalPositions(d);

    interferometerDescriptions.put(d.getName(), d);

    for (InterferometerConfiguration c : is.getConfigurations()) {
      interferometerConfigurations.put(getConfigurationName(c), c);

      // reverse mapping :
      is.getDescription().getConfigurations().add(c);
    }
  }

  /**
   * Compute the spherical coordinates for the interferometer and its stations
   * @param d interferometer description
   */
  private void computeSphericalPositions(final InterferometerDescription d) {

    Spherical sph;

    // interferometer center :
    final Position3D center = d.getPosition();
    sph = GeocentricCoords.getLonLatAlt(center);

    d.setPosSph(new LonLatAlt(sph.getLong(), sph.getLat(), sph.getRadial()));

    GeocentricCoords.dump(d.getName(), sph);

    final Position3D absPos = new Position3D();

    Position3D staPos;
    for (Station s : d.getStations()) {
      staPos = s.getRelativePosition();

      // change here the coordinates :
      // station coordinates are rotated in the horizontal plane
      // so X axis points to the local meridian instead of the Greenwich meridian (longitude)

      absPos.setPosX(center.getPosX() + staPos.getPosX());
      absPos.setPosY(center.getPosY() + staPos.getPosY());
      absPos.setPosZ(center.getPosZ() + staPos.getPosZ());

      sph = GeocentricCoords.getLonLatAlt(absPos);

      GeocentricCoords.dump(s.getName(), sph);

      s.setPosSph(new LonLatAlt(sph.getLong(), sph.getLat(), sph.getRadial()));
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

  // Getter / Setter / API :
  public Map<String, InterferometerDescription> getInterferometerDescriptions() {
    return interferometerDescriptions;
  }

  public InterferometerDescription getInterferometerDescription(final String name) {
    return interferometerDescriptions.get(name);
  }

  public Vector<String> getInterferometerNames() {
    final Vector<String> v = new Vector<String>();

    for (InterferometerDescription id : getInterferometerDescriptions().values()) {
      v.add(id.getName());
    }
    Collections.sort(v);
    return v;
  }

  public Vector<String> getInterferometerConfigurationNames(final String interferometerName) {
    final Vector<String> v = new Vector<String>();

    final InterferometerDescription id = getInterferometerDescription(interferometerName);
    if (id != null) {
      for (InterferometerConfiguration c : id.getConfigurations()) {
        v.add(c.getName());
      }
      Collections.sort(v);
    }
    return v;
  }


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


  public void getSwitchYardValue(final Station station, final Channel channel) {
    /* Is it channel or delay line ? */
  }

  public void getPopValue(final Station station, final Integer popIndex) {

  }

  /** InterferometerConfiguration */

  public Map<String, InterferometerConfiguration> getInterferometerConfigurations() {
    return interferometerConfigurations;
  }

  public InterferometerConfiguration getInterferometerConfiguration(final String name) {
    return interferometerConfigurations.get(name);
  }


  public Vector<String> getInterferometerInstrumentNames(final String configurationName) {
    final Vector<String> v = new Vector<String>();

    final InterferometerConfiguration c = getInterferometerConfiguration(configurationName);
    if (c != null) {
      for (FocalInstrumentConfiguration ic : c.getInstruments()) {
        v.add(ic.getFocalInstrument().getName());
      }
      Collections.sort(v);
    }
    return v;
  }

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

  public Vector<String> getInstrumentConfigurationNames(final String configurationName, final String instrumentName) {
    final Vector<String> v = new Vector<String>();

    final FocalInstrumentConfiguration ic = getInterferometerInstrumentConfiguration(configurationName, instrumentName);
    if (ic != null) {
      for (FocalInstrumentConfigurationItem c : ic.getConfigurations()) {
        v.add(c.getShortName());
      }
    }
    return v;
  }

  public List<Station> getInstrumentConfigurationStations(final String configurationName, final String instrumentName, final String instrumentConfigurationName) {
    final FocalInstrumentConfiguration ic = getInterferometerInstrumentConfiguration(configurationName, instrumentName);
    if (ic != null) {
      for (FocalInstrumentConfigurationItem c : ic.getConfigurations()) {
        if (c.getShortName().equals(instrumentConfigurationName)) {
          return c.getStations();
        }
      }
    }
    return null;
  }

  /**
   * Test code
   */
  public static void main(final String[] args) {
    final ConfigurationManager cm = ConfigurationManager.getInstance();

    logger.log(Level.SEVERE, "interferometer Names = " + cm.getInterferometerNames());

    InterferometerDescription d;

    Spherical sph;
    for (String id : cm.getInterferometerNames()) {
      logger.log(Level.SEVERE, "configuration Names = " + cm.getInterferometerConfigurationNames(id));

      d = cm.getInterferometerDescription(id);

      sph = GeocentricCoords.getLonLatAlt(d.getPosition());

      GeocentricCoords.dump(d.getName(), sph);
    }
  }
}
