/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: HorizonService.java,v 1.1 2009-11-23 16:49:17 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
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
public class HorizonService {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.service.HorizonService";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** singleton instance */
  private static HorizonService instance = new HorizonService();

  /* members */
  /** cached horizon profiles : low memory impact */
  private Map<String, Profile> cachedProfiles = new HashMap<String, Profile>();

  public static HorizonService getInstance() {
    return instance;
  }

  private HorizonService() {
    /* no-op */
  }

  /**
   * Give the profile corresponding to the given station
   * @param station station to use
   * @return profile or null if there is no horizon profile for the given station
   */
  public Profile getProfile(final String interferometerName, final Station station) {
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

        profile = new Profile(station.getName(), polygon);

        // Maybe the name of the station is not enough to guarantee the unicity for a long list of interferometers :
        cachedProfiles.put(key, profile);
      }
    }

    return profile;
  }

  public boolean checkProfile(final Profile profile, final double az, final double elev) {
    return profile.check(az, elev);
  }

  /**
   * Simple protected class to use the java shape API to check if a point is inside a polygon
   */
  protected class Profile {
    /** station name */
    private String name;
    /** internal shape */
    private Shape shape;

    protected Profile(final String stationName, final Shape shape) {
      this.name = stationName;
      this.shape = shape;
    }

    public boolean check(final double az, final double elev) {
      return this.shape.contains(az, elev);
    }

    public String getName() {
      return name;
    }

  }
}
