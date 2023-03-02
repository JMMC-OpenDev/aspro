
/**
 * *****************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 * ****************************************************************************
 */
import fr.jmmc.aspro.model.oi.LonLatAlt;
import fr.jmmc.aspro.model.oi.Position3D;
import fr.jmmc.aspro.service.GeocentricCoords;
import fr.jmmc.jmcs.Bootstrapper;
import fr.jmmc.jmcs.util.FileUtils;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.text.NumberFormat;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.StringTokenizer;
import org.apache.commons.lang.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class converts the VLT switchyard to an XML fragment compliant with the aspro DM
 *
 * @author bourgesl
 */
public final class AsproGenCharaConfig {

    /** refractive index in air (from http://refractiveindex.info/?group=GASES&material=Air) */
    private final static double N_AIR = 1.00027316;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(AsproGenConfig.class.getName());

    /** interferometer enum */
    private enum INTERFEROMETER {
        /** VLTI (eso) */
        VLTI,
        /** CHARA array */
        CHARA,
        /** SUSI */
        SUSI,
        /** NPOI (experimental) */
        NPOI,
        /** MROI (future) */
        MROI,
        /* 1T sites */
        SINGLE_DISH
    };

    /**
     * Forbidden constructor
     */
    private AsproGenCharaConfig() {
        // no-op
    }

    /**
     * Convert horizontal coordinates to equatorial coordinates
     *
     * # XOFFSET - East offset in microns from S1 # YOFFSET - North offset in microns from S1 # ZOFFSET - vertical (+ is
     * up) offset in microns from S1
     *
     * @param station station name
     * @param latitude latitude of the interferometer (rad)
     * @param xOffset East offset in microns from S1
     * @param yOffset North offset in microns from S1
     * @param zOffset vertical (+ is up) offset in microns from S1
     * @param sb output buffer for xml output
     * @return equatorial coordinates
     */
    private static Position3D convertHorizToEquatorial(final String station, final double latitude,
                                                       final double xOffset, final double yOffset, final double zOffset,
                                                       final StringBuilder sb) {

        final double x = xOffset * 1e-6;
        final double y = yOffset * 1e-6;
        final double z = zOffset * 1e-6;

        // use geodetic latitude
        final double xx = -Math.sin(latitude) * y + Math.cos(latitude) * z;
        final double yy = x;
        final double zz = Math.cos(latitude) * y + Math.sin(latitude) * z;

        logger.info(station + " = (" + xx + ", " + yy + ", " + zz + ")");

        sb.append("      <relativePosition>\n");
        sb.append("        <posX>").append(xx).append("</posX>\n");
        sb.append("        <posY>").append(yy).append("</posY>\n");
        sb.append("        <posZ>").append(zz).append("</posZ>\n");
        sb.append("      </relativePosition>\n");

        return getPoint3D(xx, yy, zz);
    }

    /**
     * Compute CHARA fixed delay (AIRPATH + LIGHT)
     *
     * # AIRPATH - amount of airpath in microns using default beam #	Note that this assumes the default Beam dn default
     * Pop are used # LIGHT	- length of light pipe in microns #	Note that this assumes the default Beam dn default Pop are
     * used
     *
     * @param station station name
     * @param light length of light pipe in microns
     * @param airPath amount of airpath in microns using default beam
     * @param sb output buffer for xml output
     */
    private static void convertCHARAAirPath(final String station, final double light, final double airPath,
                                            final StringBuilder sb) {

        final double delay = (light + airPath * N_AIR) * 1e-6;

        logger.info(station + " = " + delay);

        sb.append("      <delayLineFixedOffset>").append(delay).append("</delayLineFixedOffset>\n");
    }

    /**
     * Compute CHARA PoP delay (POPX)
     *
     * # POPX	- Extra airpath to add when using POP X on this scope
     *
     * @param config station config
     * @param sb output buffer for xml output
     */
    private static void convertCHARAPoP(final Map<String, Double> config,
                                        final StringBuilder sb) {

        for (int i = 1; i <= 5; i++) {
            sb.append("      <popLink>\n");
            sb.append("        <pop>PoP").append(i).append("</pop>\n");
            sb.append("        <opticalLength>").append(config.get("POP" + (i)) * 1e-6).append("</opticalLength>\n");
            sb.append("      </popLink>\n");
        }
    }

    /**
     * Convert CHARA station configs to ASPRO station configurations
     *
     * @param stationConfigs station configs
     * @param sb buffer
     */
    private static void convertCHARAStations(final Map<String, Map<String, Double>> stationConfigs, final StringBuilder sb) {
        // CHARA CMA: 2021.10: support fibered stations (+ mobile telescope)

        final List<String> baseTelescopes = Arrays.asList("S1", "S2", "E1", "E2", "W1", "W2");

        final double[] lonlat = CHARAposition(sb);

        final double lat = Math.toRadians(lonlat[1]);

        String station;
        Map<String, Double> config;
        for (Map.Entry<String, Map<String, Double>> e : stationConfigs.entrySet()) {
            station = e.getKey();
            config = e.getValue();

            sb.append("    <station>\n");
            sb.append("      <name>").append(station).append("</name>\n");

            // Fix telescope reference depending on fixed station or not ?
            final boolean isFixedTel = (station.startsWith("F") || baseTelescopes.contains(station));
            sb.append("      <telescope>");
            sb.append(isFixedTel ? "T" : "T_mobile");
            sb.append("</telescope>\n");

            convertHorizToEquatorial(station, lat, config.get("XOFFSET"), config.get("YOFFSET"), config.get("ZOFFSET"), sb);

            convertCHARAAirPath(station, config.get("LIGHT"), config.get("AIRPATH"), sb);

            sb.append("\n");

            final boolean isFiberedTel = !baseTelescopes.contains(station);
            if (!isFiberedTel) {
                convertCHARAPoP(config, sb);
                convertCHARAHorizon(station, sb);
            }

            sb.append("    </station>\n\n");
        }
    }

    /**
     * Convert CHARA station configs to ASPRO switchyard
     *
     * @param stationConfigs station configs
     * @param sb buffer
     */
    private static void convertCHARASwitchyard(final Map<String, Map<String, Double>> stationConfigs, final StringBuilder sb) {

        sb.append("    <switchyard>\n");

        final double[] values = new double[6];

        String station;
        Map<String, Double> config;
        for (Map.Entry<String, Map<String, Double>> e : stationConfigs.entrySet()) {
            station = e.getKey();
            config = e.getValue();

            for (int i = 0; i < 6; i++) {
                values[i] = config.get("BEAM" + (i + 1)) * N_AIR * 1e-6;
            }

            convertCHARASwitchyardStation(station, values, sb);
        }

        sb.append("    </switchyard>\n\n");
    }

    /**
     * Convert CHARA station config to ASPRO stationLinks
     *
     * @param station station config
     * @param values values for beams
     * @param sb output buffer for xml output
     */
    private static void convertCHARASwitchyardStation(final String station, final double[] values,
                                                      final StringBuilder sb) {

        sb.append("      <stationLinks>\n");
        sb.append("        <station>").append(station).append("</station>\n");

        final NumberFormat nf = NumberFormat.getInstance(Locale.ENGLISH);
        nf.setMaximumFractionDigits(8);

        double value;
        for (int i = 0, size = values.length; i < size; i++) {
            value = values[i];

            sb.append("        <channelLink>\n");
            sb.append("          <channel>V").append(i + 1).append("</channel>\n");
            sb.append("          <opticalLength>").append(nf.format(value)).append("</opticalLength>\n");
            sb.append("        </channelLink>\n");
        }

        sb.append("      </stationLinks>\n\n");
    }

    /**
     * Load the CHARA config file (telescopes.chara)
     *
     * @param absFileName absolute file path to CHARA config file
     * @return chara station config
     *
     * # The USED labels and their data fields are: # # XOFFSET - East offset in microns from S1 # YOFFSET - North offset
     * in microns from S1 # ZOFFSET - vertical (+ is up) offset in microns from S1
     *
     * # AIRPATH - amount of airpath in microns using default beam #	Note that this assumes the default Beam dn default
     * Pop are used # INTERNAL- Pathlength (with default beam) for internal fringes # LIGHT	- length of light pipe in
     * microns #	Note that this assumes the default Beam dn default Pop are used
     *
     * # BEAMX - Extra airpath to add when using beam X on this scope # POPX	- Extra airpath to add when using POP X on
     * this scope
     */
    private static Map<String, Map<String, Double>> loadCHARAConfig(final String absFileName) {

        logger.info("loadCHARAConfig : " + absFileName);

        final List<String> labels = Arrays.asList(new String[]{
            "XOFFSET", "YOFFSET", "ZOFFSET", "AIRPATH", "LIGHT", "INTERNAL",
            "BEAM1", "BEAM2", "BEAM3", "BEAM4", "BEAM5", "BEAM6",
            "POP1", "POP2", "POP3", "POP4", "POP5"
        });

        final Map<String, Map<String, Double>> stationConfigs = new LinkedHashMap<String, Map<String, Double>>(16);

        // load data from file :
        BufferedReader reader = null;
        try {
            final File data = new File(absFileName);

            reader = new BufferedReader(new FileReader(data));

            // column separator :
            final String delimiter = " ";

            String name = null, key;
            Double value;
            Map<String, Double> current = null;

            StringTokenizer tok;
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.replaceAll("\\s+", " ").trim();
                if (line.length() > 0 && !line.startsWith("#")) {

                    if (name == null) {
                        // start station block :
                        name = line;
                        current = new LinkedHashMap<String, Double>(16);

//            logger.info("new station : " + name);
                        continue;
                    }

                    if ("END".equals(line)) {
                        // end station block :
                        stationConfigs.put(name, current);

                        logger.info("end station : " + name + " =\n" + current);

                        name = null;
                        current = null;

                        continue;
                    }

                    // Parse values :
//          logger.info("line = " + line);
                    tok = new StringTokenizer(line, delimiter);

                    if (tok.hasMoreTokens()) {
                        // key
                        key = tok.nextToken();

                        if (labels.contains(key)) {
                            // value (only first used) :
                            if (tok.hasMoreTokens()) {
                                value = Double.valueOf(tok.nextToken());

                                current.put(key, value);
                            }
                        }
                    }
                }
            }

        } catch (FileNotFoundException fnfe) {
            logger.error("File not found", fnfe);
        } catch (IOException ioe) {
            logger.error("IO failure", ioe);
        } finally {
            FileUtils.closeFile(reader);
        }
        return stationConfigs;
    }

    /**
     * Convert the CHARA config file (telescopes.chara)
     *
     * @param absFileName absolute file path to CHARA config file
     */
    private static void convertCHARAConfig(final String absFileName) {

        final Map<String, Map<String, Double>> stationConfigs = loadCHARAConfig(absFileName);

        final StringBuilder sb = new StringBuilder(12 * 1024);

        sb.append("<a:interferometerSetting>\n\n");
        sb.append("  <description>\n\n    <name>CHARA</name>\n\n");

        convertCHARAStations(stationConfigs, sb);

        convertCHARASwitchyard(stationConfigs, sb);

        sb.append("  </description>\n\n</a:interferometerSetting>\n");

        logger.info("Generated CHARA Configuration : " + sb.length() + "\n" + sb.toString());
    }

    /**
     * Compute the CHARA position (S1 coordinates)
     *
     * @param sb buffer
     * @return long and lat in degrees
     */
    private static double[] CHARAposition(final StringBuilder sb) {
//    #  Geodetic height of the CHARA reference plane [meters]:
//       HEIGHT  1725.21

        // S1 from telescopes.chara:
//    LONG    -118 3 25.31272
//    LAT       34 13 27.78130
        final double lonDeg = -(118.0 + 3.0 / 60.0 + 25.31272 / 3600.0);
        logger.info("CHARA longitude (deg) : " + lonDeg);

        final double latDeg = 34.0 + 13.0 / 60.0 + 27.78130 / 3600.0;
        logger.info("CHARA latitude (deg)  : " + latDeg);

        final double alt = 1725.21;

        final Position3D position = computeInterferometerPosition(lonDeg, latDeg, alt, sb);

        final LonLatAlt coords = GeocentricCoords.getLonLatAlt(position);

        logger.info("CHARA position : " + coords.toString());

        /*
        14:25:39.671 INFO  [main] AsproGenConfig - CHARA longitude (deg) : -118.0570313111111
        14:25:39.671 INFO  [main] AsproGenConfig - CHARA latitude (deg)  : 34.22438369444445
        14:25:39.671 INFO  [main] AsproGenConfig - position (x,y,z) : [-2483801.164432058, -4660154.224654438, 3568032.552411753]
        14:25:39.671 INFO  [main] AsproGenConfig - CHARA position : [-118:03:25.3127, +34:13:27.7813, 1725.209 m][-118.0570313111111, 34.22438369444445]
         */
        return new double[]{lonDeg, latDeg};
    }

    private static void convertCHARAHorizon(final String station, final StringBuilder sb) {

        double[] az;
        double[] el;

        if ("S1".equals(station)) {
            /*
            #------------new limit data (07-15-2022)-----------
            #S1
            s1az=[0,2,4,6,8,10,12,14,16,206,208,210,212,214,216,218,220,222,265,275,278,280,282,284,286,288,290,292,294,296,298,300,302,304,306,308,310,312,314,316,318,320,322,324,326,328,330,332,334,336,338,340,342,344,346,348,350,352,354,356,358]
            s1el=[31,30,29,27,25,23,21,21,18,18,20,20,21,23,22,20,19,18,18,18,25,28,29,29,30,30,33,33,34,35,37,37,37,39,41,41,40,41,40,40,39,39,40,41,41,40,41,42,42,42,41,41,40,40,40,40,38,33,32,32,32]
             */
            az = new double[]{0, 2, 4, 6, 8, 10, 12, 14, 16, 206, 208, 210, 212, 214, 216, 218, 220, 222, 265, 275, 278, 280, 282, 284, 286, 288, 290, 292, 294, 296, 298, 300, 302, 304, 306, 308, 310, 312, 314, 316, 318, 320, 322, 324, 326, 328, 330, 332, 334, 336, 338, 340, 342, 344, 346, 348, 350, 352, 354, 356, 358};
            el = new double[]{31, 30, 29, 27, 25, 23, 21, 21, 18, 18, 20, 20, 21, 23, 22, 20, 19, 18, 18, 18, 25, 28, 29, 29, 30, 30, 33, 33, 34, 35, 37, 37, 37, 39, 41, 41, 40, 41, 40, 40, 39, 39, 40, 41, 41, 40, 41, 42, 42, 42, 41, 41, 40, 40, 40, 40, 38, 33, 32, 32, 32};
        } else if ("S2".equals(station)) {
            /*
            #------------new limit data (07-15-2022)-----------
            #S2
            s2az=[0,10,15,20,25,30,90,100,110,115,120,125,165,170,175,180,234,236,238,240,242,244,246,248,250,252,254,256,258,260,262,288,290,291,292,294,296,298,300,302,304,306,308,310,312,314,316,318,320,322,324,326,328,330,332,334,336,338,340,342,344,346,348,350,352,354,356,358]
            s2el=[18,18,18,20,22,18,18,18,23,23,23,18,18,20,20,18,18,20,22,23,24,25,25,25,25,26,24,23,21,19,18,18,21,30,40,40,41,41,41,41,41,42,42,45,46,46,48,48,48,49,49,49,49,46,46,42,37,37,32,32,27,27,26,25,22,22,20,18]
             */
            az = new double[]{0,10,15,20,25,30,90,100,110,115,120,125,165,170,175,180,234,236,238,240,242,244,246,248,250,252,254,256,258,260,262,288,290,291,292,294,296,298,300,302,304,306,308,310,312,314,316,318,320,322,324,326,328,330,332,334,336,338,340,342,344,346,348,350,352,354,356,358};
            el = new double[]{18,18,18,20,22,18,18,18,23,23,23,18,18,20,20,18,18,20,22,23,24,25,25,25,25,26,24,23,21,19,18,18,21,30,40,40,41,41,41,41,41,42,42,45,46,46,48,48,48,49,49,49,49,46,46,42,37,37,32,32,27,27,26,25,22,22,20,18};
        } else if ("W1".equals(station)) {
            /*
            #------------new limit data (07-15-2022)-----------
            #W1
            w1az=[0,8,10,12,14,22,24,26,28,30,32,34,36,38,40,42,44,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100,102,104,106,108,110,112,114,116,118,120,122]
            w1el=[18,18,19,18,18,18,19,20,20,24,27,28,25,21,21,19,18,18,19,21,21,38,38,37,40,42,43,43,43,43,42,43,42,42,41,38,38,36,34,36,36,26,27,18]
             */
            az = new double[]{0,8,10,12,14,22,24,26,28,30,32,34,36,38,40,42,44,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100,102,104,106,108,110,112,114,116,118,120,122};
            el = new double[]{18,18,19,18,18,18,19,20,20,24,27,28,25,21,21,19,18,18,19,21,21,38,38,37,40,42,43,43,43,43,42,43,42,42,41,38,38,36,34,36,36,26,27,18};
        } else if ("W2".equals(station)) {
            /*
            #------------new limit data (07-15-2022)-----------
            #W2
            w2az=[0,10,12,14,16,18,20,22,24,26,28,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100,102,104,106,108,110,112,114,116,118,120,122,124,126,128,130,132,164,166,168,170,172,174,176]
            w2el=[18,18,20,20,18,18,20,20,21,21,18,18,18,21,22,24,24,24,26,26,25,28,28,28,27,25,24,24,23,23,22,21,23,23,24,25,25,25,24,23,22,19,18,18,21,21,21,20,19,18]
             */
            az = new double[]{0,10,12,14,16,18,20,22,24,26,28,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100,102,104,106,108,110,112,114,116,118,120,122,124,126,128,130,132,164,166,168,170,172,174,176};
            el = new double[]{18,18,20,20,18,18,20,20,21,21,18,18,18,21,22,24,24,24,26,26,25,28,28,28,27,25,24,24,23,23,22,21,23,23,24,25,25,25,24,23,22,19,18,18,21,21,21,20,19,18};
        } else if ("E1".equals(station)) {
            /*
            #------------new limit data (07-15-2022)-----------
            #E1
            e1az=[0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,245,248,250,252,268,270,272,274,276,278,280,282,284,286,288,290,292,294,296,298,300,302,304,306,308,310,312,314,316,318,320,322,324,332,333,334,336,338,340,342,344,346,350,352,354,356,358]
            e1el=[31,29,28,29,26,27,32,38,37,36,36,35,33,34,34,31,31,31,31,30,28,28,27,25,25,18,18,20,20,18,18,20,22,23,23,23,24,25,26,26,27,26,29,31,32,32,33,32,32,31,31,31,30,29,29,29,28,20,18,18,19,20,20,20,24,26,27,28,29,29,28,28,30]
             */
            az = new double[]{0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,245,248,250,252,268,270,272,274,276,278,280,282,284,286,288,290,292,294,296,298,300,302,304,306,308,310,312,314,316,318,320,322,324,332,333,334,336,338,340,342,344,346,350,352,354,356,358};
            el = new double[]{31,29,28,29,26,27,32,38,37,36,36,35,33,34,34,31,31,31,31,30,28,28,27,25,25,18,18,20,20,18,18,20,22,23,23,23,24,25,26,26,27,26,29,31,32,32,33,32,32,31,31,31,30,29,29,29,28,20,18,18,19,20,20,20,24,26,27,28,29,29,28,28,30};
        } else if ("E2".equals(station)) {
            /*
            #------------new limit data (07-15-2022)-----------
            #E2
            e2az=[0,18,20,22,24,26,28,30,32,34,36,38,40,43,168,170,172,174,176,178,245,246,248,250,252,254,256,258,260,262,264]
            e2el=[18,18,19,22,25,28,29,29,29,28,28,26,24,18,18,21,22,21,18,18,18,20,24,24,24,28,29,27,25,23,18]
             */
            az = new double[]{0,18,20,22,24,26,28,30,32,34,36,38,40,43,168,170,172,174,176,178,245,246,248,250,252,254,256,258,260,262,264};
            el = new double[]{18,18,19,22,25,28,29,29,29,28,28,26,24,18,18,21,22,21,18,18,18,20,24,24,24,28,29,27,25,23,18};
        } else {
            // missing station
            logger.info("No horizon for station " + station);
            az = el = null;
        }

        if ((az != null) && (el != null)) {
            if (az.length != el.length) {
                throw new IllegalStateException("Inconsistent length for azimuth and elevation arrays !");
            }
            int len = az.length - 1;
            // check az=0:
            if (az[0] != 0d) {
                throw new IllegalStateException("Invalid azimuth array [except 0 and 360 range] !");
            }

            // Fix start and end of az/el arrays (ie use max(elevation) for az=0 and az=360):
            int a = 0;
            for (int i = 0; i <= len; i++) {
                if (az[i] == 0d) {
                    a = i;
                } else {
                    break;
                }
            }
            System.out.println("el = " + el[a] + " a = " + Arrays.toString(Arrays.copyOfRange(az, 0, a + 1)));

            // fix last az:
            if (az[len] < 360d) {
                len++;
                az = Arrays.copyOf(az, len + 1);
                el = Arrays.copyOf(el, len + 1);
                // add last point:
                az[len] = 360d;
                el[len] = el[a];
                
            } else {
                throw new IllegalStateException("Invalid azimuth array [except 0 and 360 range] !");
            }
            
            int b = 0;
            for (int i = len; i >= 0; i--) {
                if (az[i] == 360d) {
                    b = i;
                } else {
                    break;
                }
            }
            System.out.println("el = " + el[b] + " b = " + Arrays.toString(Arrays.copyOfRange(az, b, len + 1)));

            // Fix elevation for az=0 and az=360 ie take max:
            double max = 0d;
            for (int i = 0; i <= a; i++) {
                max = Math.max(max, el[i]);
            }
            for (int i = b; i <= len; i++) {
                max = Math.max(max, el[i]);
            }
            System.out.println("max at (az = 0 or 360) = " + max);
            el[a] = el[b] = max;

            sb.append("      <horizon>\n"
                    + "        ");

            for (int i = a; i <= b; i++) {
                // output :
                sb.append("<point>");
                sb.append("<azimuth>").append(az[i]).append("</azimuth>");
                sb.append("<elevation>").append(el[i]).append("</elevation>");
                sb.append("</point>");
            }
            // close polygon:
            sb.append("<point>");
            sb.append("<azimuth>").append(360.0).append("</azimuth>");
            sb.append("<elevation>").append(90.0).append("</elevation>");
            sb.append("</point>");

            sb.append("<point>");
            sb.append("<azimuth>").append(0.0).append("</azimuth>");
            sb.append("<elevation>").append(90.0).append("</elevation>");
            sb.append("</point>");

            sb.append("\n      </horizon>\n");
        }
    }

    /**
     * Convert geodetic long/lat/alt to geocentric coordinates
     *
     * @param lon longitude in degrees
     * @param lat latitude in degrees
     * @param alt altitude in meters
     * @param sb buffer
     * @return positions (x,y,z)
     */
    private static Position3D computeInterferometerPosition(final double lon, final double lat, final double alt, final StringBuilder sb) {

        final Position3D geoXYZ = GeocentricCoords.getGeocentric(Math.toRadians(lon), Math.toRadians(lat), alt);

        logger.info("position (x,y,z) : " + geoXYZ);

        sb.append("    <position>\n");
        sb.append("      <posX>").append(geoXYZ.getPosX()).append("</posX>\n");
        sb.append("      <posY>").append(geoXYZ.getPosY()).append("</posY>\n");
        sb.append("      <posZ>").append(geoXYZ.getPosZ()).append("</posZ>\n");
        sb.append("    </position>\n\n");

        return geoXYZ;
    }

    private static Position3D getPoint3D(final double x, final double y, final double z) {
        final Position3D p = new Position3D();
        p.setPosX(x);
        p.setPosY(y);
        p.setPosZ(z);
        return p;
    }

    static double trimTo4Digits(final double value) {
        if (Double.isNaN(value)) {
            return value;
        }
        return (Math.round(1e4d * value)) / 1e4d;
    }

    static boolean contains(final String[] vals, final String val) {
        for (String v : vals) {
            if (v.equalsIgnoreCase(val)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Main entry point to generate configuration parts (xml)
     *
     * @param args unused
     */
    public static void main(final String[] args) {

        // invoke Bootstrapper method to initialize logback now:
        Bootstrapper.getState();

        final StringBuilder sb = new StringBuilder(128);

        final String userHome = SystemUtils.USER_HOME;

        final String asproTestPath = userHome + "/dev/aspro/src/test/resources/";

        // Active case:
        final INTERFEROMETER selected = INTERFEROMETER.CHARA;

        switch (selected) {
            case CHARA:
                CHARAposition(sb);
                convertCHARAConfig(asproTestPath + "telescopes.chara");
                break;
            default:
                logger.info("Use AsproGenConfig instead");
        }
    }
}
