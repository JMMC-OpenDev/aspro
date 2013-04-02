/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.model.InterferometerMapData;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerConfigurationChoice;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.LonLatAlt;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.jmcs.util.FormatterUtils;
import fr.jmmc.oitools.util.CombUtils;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.List;
import net.jafama.FastMath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This service is stateless to compute the station positions (X,Y) and the chosen baselines
 * @author bourgesl
 */
public final class InterferometerMapService {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(InterferometerMapService.class.getName());
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
        if (logger.isDebugEnabled()) {
            logger.debug("compute: {}", ObservationManager.toString(observation));
        }
        final InterferometerMapData data = new InterferometerMapData();

        // stations :

        final InterferometerConfigurationChoice interferometerChoice = observation.getInterferometerConfiguration();

        final InterferometerConfiguration ic = interferometerChoice.getInterferometerConfiguration();

        if (ic != null) {
            final InterferometerDescription id = ic.getInterferometer();

            final LonLatAlt position = id.getPosSph();
            if (logger.isDebugEnabled()) {
                logger.debug("Site Long: {}", FastMath.toDegrees(position.getLongitude()));
                logger.debug("Site Lat : {}", FastMath.toDegrees(position.getLatitude()));
            }

            final double cosLat = FastMath.cos(position.getLatitude());
            final double sinLat = FastMath.sin(position.getLatitude());

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
            relocate(mapX, minX + 0.5d * (maxX - minX));

            final double minY = getMin(mapY);
            final double maxY = getMax(mapY);
            relocate(mapY, minY + 0.5d * (maxY - minY));

            final double maxXY = Math.max(0.5d * (maxX - minX), 0.5d * (maxY - minY));

            // copy results :
            data.setMaxXY(maxXY);

            data.setStationName(name);
            data.setDiameter(diam);
            data.setStationX(mapX);
            data.setStationY(mapY);

            // Get chosen stations :
            data.setStationNames(observation.getInstrumentConfiguration().getStations());

            final List<Station> stations = observation.getInstrumentConfiguration().getStationList();
            if (stations == null || stations.isEmpty()) {
                throw new IllegalStateException("The station list is empty !");
            }

            final int nStations = stations.size();

            // baseline count :
            final int blen = CombUtils.comb(nStations, 2);

            final String[] blName = new String[blen];
            final double[] blX1 = new double[blen];
            final double[] blY1 = new double[blen];
            final double[] blX2 = new double[blen];
            final double[] blY2 = new double[blen];

            final StringBuffer sb = new StringBuffer(32);

            double dist;
            Station s1, s2;
            int i1, i2;
            int n = 0;
            for (int i = 0; i < nStations; i++) {
                s1 = stations.get(i);
                i1 = data.getStationIndex(s1.getName());

                for (int j = i + 1; j < nStations; j++) {
                    s2 = stations.get(j);
                    i2 = data.getStationIndex(s2.getName());

                    x = s2.getRelativePosition().getPosX() - s1.getRelativePosition().getPosX();
                    y = s2.getRelativePosition().getPosY() - s1.getRelativePosition().getPosY();
                    z = s2.getRelativePosition().getPosZ() - s1.getRelativePosition().getPosZ();

                    dist = Math.sqrt(x * x + y * y + z * z);

                    if (i1 != -1 && i2 != -1) {
                        sb.setLength(0);
                        sb.append(s1.getName()).append('-').append(s2.getName()).append(' ');
                        FormatterUtils.format(df2, sb, dist).append(" m");

                        blName[n] = sb.toString();
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

    /**
     * Find the minimum value of the given array
     * @param values array to use
     * @return minimum value
     */
    private static final double getMin(final double[] values) {
        double min = Double.POSITIVE_INFINITY;
        for (double v : values) {
            if (min > v) {
                min = v;
            }
        }
        return min;
    }

    /**
     * Find the maximum value of the given array
     * @param values array to use
     * @return maximum value
     */
    private static final double getMax(final double[] values) {
        double max = Double.NEGATIVE_INFINITY;
        for (double v : values) {
            if (max < v) {
                max = v;
            }
        }
        return max;
    }

    /**
     * Relocate the given values with the given mean value
     * @param values values to relocate
     * @param center center or mean value
     */
    private static final void relocate(final double[] values, final double center) {
        for (int i = 0, len = values.length; i < len; i++) {
            values[i] -= center;
        }
    }
}
