/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.model.oi.AzEl;
import fr.jmmc.aspro.model.oi.HorizonProfile;
import fr.jmmc.aspro.model.oi.Station;
import java.awt.Polygon;
import java.awt.Shape;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class checks if an azimuth / elevation coordinate is over the telescope horizon
 * @author bourgesl
 */
public final class HorizonService {

  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(HorizonService.class.getName());
  /** singleton instance */
  private static final HorizonService instance = new HorizonService();

  /* members */
  /** cached horizon profiles : low memory impact */
  private final Map<String, Profile> cachedProfiles = new HashMap<String, Profile>(128);

  /**
   * Return the singleton
   * @return singleton
   */
  public static HorizonService getInstance() {
    return instance;
  }

  /**
   * Private constructor
   */
  private HorizonService() {
    /* no-op */
  }

  /**
   * Give the profile corresponding to the given station
   * @param interferometerName interferometer name used to compute a cache key (interferometer name - station name)
   * @param station station to use
   * @return profile or null if there is no horizon profile for the given station
   */
  public Profile getProfile(final String interferometerName, final Station station) {
    // profile key = '<interferometer> - <station>' :
    final String key = interferometerName + " - " + station.getName();
    Profile profile = cachedProfiles.get(key);

    if (profile == null) {
      final HorizonProfile p = station.getHorizon();

      // As VLT horizons have only integer coordinates, let's use a simple polygon :

      final List<AzEl> points = p.getPoints();

      if (points != null && points.size() > 0) {
        final int npoints = points.size() + 1;
        final int[] xpoints = new int[npoints];
        final int[] ypoints = new int[npoints];

        // Note : az is given in [0 - 360] range :
        // 0 = meridian (north-south) positive toward east

        int i = 0;
        for (AzEl point : points) {
          xpoints[i] = (int) point.getAzimuth();
          ypoints[i] = (int) point.getElevation();
          i++;
        }
        // close the polygon :
        final AzEl orig = points.get(0);
        xpoints[i] = (int) orig.getAzimuth();
        ypoints[i] = (int) orig.getElevation();

        final Polygon polygon = new Polygon(xpoints, ypoints, npoints);

        profile = new Profile(key, polygon);

        cachedProfiles.put(key, profile);
      }
    }

    return profile;
  }

  /**
   * Checks if the az/el position in hidden by the given horizon profile
   * @param profile profile horizon to check
   * @param az azimuth (deg : 0 = north, positive toward east)
   * @param elev elevation (deg)
   * @return true if no obstruction
   */
  public boolean checkProfile(final Profile profile, final double az, final double elev) {
    // adjust azimuth to be compatible with vlti profiles :

    // 0 = meridian (north-south) positive toward east :
    double adjAz = az - 180;

    if (adjAz < 0d) {
      adjAz += 360d;
    }
    return profile.check(adjAz, elev);
  }

  /**
   * Simple protected class to use the java shape API to check if a point is inside a polygon
   */
  protected final class Profile {

    /** profile name */
    private final String name;
    /** internal shape */
    private final Shape shape;

    /**
     * Protected constructor
     * @param name profile name
     * @param shape shape (horizon)
     */
    protected Profile(final String name, final Shape shape) {
      this.name = name;
      this.shape = shape;
    }

    /**
     * Check if the given azimuth and elevation is inside the observable shape
     * @param az azimuth (degrees)
     * @param elev elevation (degrees)
     * @return true if the given coordinates are inside the observable shape
     */
    public boolean check(final double az, final double elev) {
      return this.shape.contains(az, elev);
    }

    /**
     * Return the profile name
     * @return profile name
     */
    public String getName() {
      return name;
    }
  }
}
