/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: InterferometerMapService.java,v 1.3 2010-06-30 14:54:45 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2010/06/17 10:02:50  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.1  2010/05/11 12:08:27  bourgesl
 * simple Interferometer Map (stations + baselines) automatically refreshed when the chosen baseline configuration changes
 *
 */
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.model.InterferometerMapData;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerConfigurationChoice;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.LonLatAlt;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.util.CombUtils;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.List;
import java.util.logging.Level;

/**
 * This service is stateless to compute the station positions (X,Y) and the chosen baselines
 * @author bourgesl
 */
public final class InterferometerMapService {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.service.InterferometerMapService";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** double formatter for baseline length */
  protected final static NumberFormat df2 = new DecimalFormat("0.00");

  /**
   * Forbidden constructor
   */
  private InterferometerMapService() {
    super();
  }

  /**
   * Main operation to compute the station positions (X,Y) and the chosen baselines
   *
   * @param observation observation settings
   * @return InterferometerMapData container
   */
  public static InterferometerMapData compute(final ObservationSetting observation) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("compute : " + observation);
    }
    final InterferometerMapData data = new InterferometerMapData();

    // stations :

    final InterferometerConfigurationChoice interferometerChoice = observation.getInterferometerConfiguration();

    final InterferometerConfiguration ic = interferometerChoice.getInterferometerConfiguration();

    if (ic != null) {
      final InterferometerDescription id = ic.getInterferometer();

      final LonLatAlt position = id.getPosSph();
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("Site Long : " + Math.toDegrees(position.getLongitude()));
        logger.fine("Site Lat  : " + Math.toDegrees(position.getLatitude()));
      }

      final double cosLat = Math.cos(position.getLatitude());
      final double sinLat = Math.sin(position.getLatitude());

      final List<Station> stationList = id.getStations();
      final int size = stationList.size();

      final String[] name = new String[size];
      final double[] diam = new double[size];
      final double[] mapX = new double[size];
      final double[] mapY = new double[size];

      double x, y, z;

      Station s;
      for (int i = 0; i < size; i++) {
        s = stationList.get(i);

        name[i] = s.getName();
        diam[i] = s.getTelescope().getDiameter();

        // convert XYZ station coordinates to XY plan :
        x = s.getRelativePosition().getPosX();
        y = s.getRelativePosition().getPosY();
        z = s.getRelativePosition().getPosZ();

        /*
        SP = SIN(PI*(LONLAT(2))/180.0D0)
        CP = COS(PI*(LONLAT(2))/180.0D0)
        XX = STATX(1)
        YY = STATY(1)
        ZZ = STATZ(1)

        x=YY
        y=ZZ*CP-XX*SP
         */

        mapX[i] = y;
        mapY[i] = z * cosLat - x * sinLat;
      }

      // relocate center :
      final double minX = getMin(mapX);
      final double maxX = getMax(mapX);
      relocate(mapX, minX + (maxX - minX) / 2d);

      final double minY = getMin(mapY);
      final double maxY = getMax(mapY);
      relocate(mapY, minY + (maxY - minY) / 2d);

      final double maxXY = Math.max((maxX - minX) / 2d, (maxY - minY) / 2d);

      // copy results :
      data.setMaxXY(maxXY);

      data.setStationName(name);
      data.setDiameter(diam);
      data.setStationX(mapX);
      data.setStationY(mapY);

      // Get chosen stations :
      final List<Station> stations = observation.getInstrumentConfiguration().getStationList();
      if (stations == null) {
        throw new IllegalStateException("the station list is null !");
      }

      final int len = stations.size();

      // nombre de baselines :
      final int blen = CombUtils.comb(len, 2);

      final String[] blName = new String[blen];
      final double[] blX1 = new double[blen];
      final double[] blY1 = new double[blen];
      final double[] blX2 = new double[blen];
      final double[] blY2 = new double[blen];

      double dist;
      Station s1, s2;
      int i1, i2;
      int n = 0;
      for (int i = 0; i < len; i++) {
        s1 = stations.get(i);
        i1 = data.getStationIndex(s1.getName());

        for (int j = i + 1; j < len; j++) {
          s2 = stations.get(j);
          i2 = data.getStationIndex(s2.getName());

          x = s2.getRelativePosition().getPosX() - s1.getRelativePosition().getPosX();
          y = s2.getRelativePosition().getPosY() - s1.getRelativePosition().getPosY();
          z = s2.getRelativePosition().getPosZ() - s1.getRelativePosition().getPosZ();

          dist = Math.sqrt(x * x + y * y + z * z);

          if (i1 != -1 && i2 != -1) {
            blName[n] = s1.getName() + "-" + s2.getName() + " " + df2.format(dist) + " m";
            blX1[n] = data.getStationX()[i1];
            blY1[n] = data.getStationY()[i1];
            blX2[n] = data.getStationX()[i2];
            blY2[n] = data.getStationY()[i2];

            n++;
          }
        }
      }

      // copy results :
      data.setBaselineName(blName);
      data.setBaselineStationX1(blX1);
      data.setBaselineStationY1(blY1);
      data.setBaselineStationX2(blX2);
      data.setBaselineStationY2(blY2);
    }

    return data;
  }

  private static final double getMin(final double[] values) {
    double min = Double.MAX_VALUE;
    for (double v : values) {
      if (min > v) {
        min = v;
      }
    }
    return min;
  }

  private static final double getMax(final double[] values) {
    double max = Double.MIN_VALUE;
    for (double v : values) {
      if (max < v) {
        max = v;
      }
    }
    return max;
  }

  private static final void relocate(final double[] values, final double center) {
    for (int i = 0, len = values.length; i < len; i++) {
      values[i] -= center;
    }
  }

}
