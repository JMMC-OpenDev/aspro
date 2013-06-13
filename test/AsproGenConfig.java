
/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.oi.LonLatAlt;
import fr.jmmc.aspro.model.oi.Position3D;
import fr.jmmc.aspro.service.GeocentricCoords;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class converts the VLT switchyard to an XML fragment compliant with the aspro DM
 * 
 * @author bourgesl
 */
public final class AsproGenConfig {

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
    MROI
  };

  /**
   * Forbidden constructor
   */
  private AsproGenConfig() {
    // no-op
  }

  /**
   * Convert the ASPRO 1 VLTI switchyard :
   * 165.8497 167.1097 -1000000 -1000000 173.8097 175.0697 -1000000 -1000000 U1
   * @param absFileName absolute file path to ASPRO 1 VLTI switchyard file
   */
  private static void convertSwitchYard(final String absFileName) {

    logger.info("convertSwitchYard : " + absFileName);

    final StringBuilder sb = new StringBuilder(16384);
    sb.append("<switchyard>\n");

    // number of columns filled with double values :
    final int maxCols = 8;
    // column separator :
    final String delimiter = " ";

    // load data from file :
    BufferedReader reader = null;
    try {
      final File data = new File(absFileName);

      reader = new BufferedReader(new FileReader(data));

      int i = 0;
      String line;
      StringTokenizer tok;
      // outputs :
      String station = null;
      double[] values = new double[maxCols];

      while ((line = reader.readLine()) != null) {
        tok = new StringTokenizer(line, delimiter);

        i = 0;
        station = null;
        while (tok.hasMoreTokens()) {
          if (i < maxCols) {
            values[i] = Double.parseDouble(tok.nextToken());
          } else {
            // station name :
            station = tok.nextToken();
            break;
          }
          i++;
        }

        if (station != null) {
          /*
           System.out.println("station : " + station);
           System.out.println("values : " + Arrays.toString(values));
           */
          /*
           <stationLinks>
           <station>U1</station>
           <channelLink>
           <channel>Channel1</channel>
           <opticalLength>165.8497</opticalLength>
           </channelLink>
           ...
           </stationLinks>
           */

          // output :
          sb.append("<stationLinks>\n");
          sb.append("<station>").append(station).append("</station>\n");

          double value;
          for (i = 0; i < maxCols; i++) {

            value = values[i];

            // skip blanking value (-1000000) :
            if (value != -1000000) {
              sb.append("<channelLink>\n");
              sb.append("<channel>Channel").append(i + 1).append("</channel>\n");
              sb.append("<opticalLength>").append(value).append("</opticalLength>\n");
              sb.append("</channelLink>\n");
            }

          }

          sb.append("</stationLinks>\n");
        }
      }

    } catch (FileNotFoundException fnfe) {
      logger.error("File not found", fnfe);
    } catch (IOException ioe) {
      logger.error("IO failure", ioe);
    } finally {
      if (reader != null) {
        try {
          reader.close();
        } catch (IOException ioe) {
          logger.error("IO failure", ioe);
        }
      }
    }

    sb.append("</switchyard>\n");

    logger.info("convertSwitchYard : output :\n" + sb.toString());
  }

  /**
   * Convert ASPRO 1 VLTI horizons for the given station :
   360.000000000000       0.000000000000000E+000
   235.000000000000       0.000000000000000E+000
   235.000000000000        8.00000000000000
   224.000000000000        8.00000000000000
   224.000000000000       0.000000000000000E+000
   ...
   * @param station station name
   * @param absFileName absolute file path of the ASPRO 1 station profile
   * @param sb output buffer for xml output
   */
  private static void convertHorizon(final String station, final String absFileName, final StringBuilder sb) {

    sb.append("<station>\n");
    sb.append("<name>").append(station).append("</name>\n");
    sb.append("<horizon>\n");

    // number of columns filled with double values :
    final int maxCols = 2;
    // column separator :
    final String delimiter = " ";

    // load data from file :
    BufferedReader reader = null;
    try {
      final File data = new File(absFileName);

      reader = new BufferedReader(new FileReader(data));

      int i = 0;
      String line;
      StringTokenizer tok;
      // outputs :
      double[] values = new double[maxCols];

      while ((line = reader.readLine()) != null) {

        // replace multiple delimiters :

        tok = new StringTokenizer(line, delimiter);

        i = 0;
        while (tok.hasMoreTokens()) {
          if (i < maxCols) {
            values[i] = Double.parseDouble(tok.nextToken());
          }
          i++;
        }
        /*
         System.out.println("values : " + Arrays.toString(values));
         */

        // output :
        sb.append("<point>");
        sb.append("<azimuth>").append(values[0]).append("</azimuth>");
        sb.append("<elevation>").append(values[1]).append("</elevation>");
        sb.append("</point>");
      }

    } catch (FileNotFoundException fnfe) {
      logger.error("File not found", fnfe);
    } catch (IOException ioe) {
      logger.error("IO failure", ioe);
    } finally {
      if (reader != null) {
        try {
          reader.close();
        } catch (IOException ioe) {
          logger.error("IO failure", ioe);
        }
      }
    }

    sb.append("</horizon>\n");
    sb.append("</station>\n");

  }

  /**
   * Convert horizontal coordinates to equatorial coordinates
   *
   * # XOFFSET - East offset in microns from S1
   * # YOFFSET - North offset in microns from S1
   * # ZOFFSET - vertical (+ is up) offset in microns from S1
   *
   * @param station station name
   * @param latitude latitude of the interferometer (rad)
   * @param xOffset East offset in microns from S1
   * @param yOffset North offset in microns from S1
   * @param zOffset vertical (+ is up) offset in microns from S1
   * @param sb output buffer for xml output
   */
  private static void convertHorizToEquatorial(final String station, final double latitude,
          final double xOffset, final double yOffset, final double zOffset,
          final StringBuilder sb) {

    final double x = xOffset * 1e-6d;
    final double y = yOffset * 1e-6d;
    final double z = zOffset * 1e-6d;

    final double xx = -Math.sin(latitude) * y + Math.cos(latitude) * z;
    final double yy = x;
    final double zz = Math.cos(latitude) * y + Math.sin(latitude) * z;

    logger.info(station + " = (" + xx + ", " + yy + ", " + zz + ")");

    sb.append("      <relativePosition>\n");
    sb.append("        <posX>").append(xx).append("</posX>\n");
    sb.append("        <posY>").append(yy).append("</posY>\n");
    sb.append("        <posZ>").append(zz).append("</posZ>\n");
    sb.append("      </relativePosition>\n");
  }

  /**
   * Compute CHARA fixed delay (AIRPATH + LIGHT)
   *
   # AIRPATH - amount of airpath  in microns using default beam
   #	    Note that this assumes the default Beam dn default Pop are used
   # INTERNAL- Pathlength (with default beam) for internal fringes
   # LIGHT	  - length of light pipe in microns
   #	    Note that this assumes the default Beam dn default Pop are used
   *
   * @param station station name
   * @param light length of light pipe in microns
   * @param airPath amount of airpath  in microns using default beam
   * @param internal Pathlength (with default beam) for internal fringes
   * @param sb output buffer for xml output
   */
  public static void convertCHARAAirPath(final String station, final double light, final double airPath, final double internal,
          final StringBuilder sb) {

    final double delay = (light + airPath + internal) * 1e-6d;

    logger.info(station + " = " + delay);

    sb.append("      <delayLineFixedOffset>").append(delay).append("</delayLineFixedOffset>\n");
  }

  /**
   * Compute CHARA PoP delay (POPX)
   *
   # POPX	  - Extra airpath to add when using POP X on this scope
   *
   * @param config station config
   * @param sb output buffer for xml output
   */
  public static void convertCHARAPoP(final Map<String, Double> config,
          final StringBuilder sb) {

    for (int i = 1; i <= 5; i++) {
      sb.append("      <popLink>\n");
      sb.append("        <pop>PoP").append(i).append("</pop>\n");
      sb.append("        <opticalLength>").append(config.get("POP" + i) * 1e-6d).append("</opticalLength>\n");
      sb.append("      </popLink>\n");
    }
  }

  /**
   * Convert CHARA station configs to ASPRO station configurations
   * @param stationConfigs station configs
   * @param sb buffer
   */
  private static void convertCHARAStations(final Map<String, Map<String, Double>> stationConfigs, final StringBuilder sb) {

    final double[] lonlat = CHARAposition(sb);

    final double lat = Math.toRadians(lonlat[1]);

    String station;
    Map<String, Double> config;
    for (Map.Entry<String, Map<String, Double>> e : stationConfigs.entrySet()) {
      station = e.getKey();
      config = e.getValue();

      sb.append("    <station>\n");
      sb.append("      <name>").append(station).append("</name>\n");
      sb.append("      <telescope>T</telescope>\n");

      convertHorizToEquatorial(station, lat, config.get("XOFFSET"), config.get("YOFFSET"), config.get("ZOFFSET"), sb);

      convertCHARAAirPath(station, config.get("LIGHT"), config.get("AIRPATH"), config.get("INTERNAL"), sb);

      sb.append("\n");

      convertCHARAPoP(config, sb);

      sb.append("    </station>\n\n");
    }

    /*
     S1          0.000000000   0.000000000     0.00000000    0.0000000
     *
     S1 = (0.0, 0.0, 0.0) 0.0
     *
     * 
     *
     S2        -18.354945174    -5.748393059         28.128580103         4.532654762 (ASPRO 1)
     S2 = (-18.35494517183226,  -5.7483930590000005, 28.128580104400015)
     S2 = (-18.364916206804995, -5.7444145760000005, 28.12661922364987)   4.74371459
     *
     S2 = (-18.360473037751714, -5.746854437,        28.12397435836417)   4.690179333000001
     S2 = (-18.360473037751714, -5.746854437,        28.12397435836417)   5.065469333 (+ INTERNAL)
     S2 = (-18.360473037751714, -5.746854437,        28.12397435836417)   5.065469333
     *
     *
     *
     *
     *
     E1       -176.947579603    125.349003099       249.643974581        15.309717902 (ASPRO 1)
     E1 = (-176.94757958628557, 125.349003099,      249.64397459290652)
     E1 = (-176.95914597777053, 125.33313329999999, 249.6252027666933)   15.504650597999998
     *
     E1 = (-176.95372938066825, 125.333989819,      249.63388874133827)  15.513547531999999
     E1 = (-176.95372938066825, 125.333989819,      249.63388874133827)  15.513547531999999 (+ INTERNAL)
     E1 = (-176.95372938066825, 125.333989819,      249.63388874133827)  15.513547531999999
     *
     *
     *
     E2       -154.003121030    70.400864470       221.444997474         26.721359853 (ASPRO 1)
     E2 = (-154.00312101526163, 70.40086447,       221.44499748445782)
     E2 = (-154.01434025673439, 70.3891451,        221.43497871824562)   26.372594395
     *
     E2 = (-154.00873148624743, 70.39660711799999, 221.43708278416645)   26.370872645
     E2 = (-154.00873148624743, 70.39660711799999, 221.43708278416645)   26.743470664999997 (+ INTERNAL)
     E2 = (-154.00873148624743, 70.39660711799999, 221.43708278416645)   26.743470664999997
     *
     *
     *
     W1       -130.620014605    -175.068583489       172.774093891       29.087218284 (ASPRO 1)
     W1 = (-130.6200145937469,  -175.068583489,      172.7740938996687)
     W1 = (-130.59792281771624, -175.0684101,        172.79538958138997) 29.157963303
     *
     W1 = (-130.58878748879556, -175.07333221099998, 172.79336516697768) 29.122251552999998
     W1 = (-130.58878748879556, -175.07333221099998, 172.79336516697768) 29.122251552999998 (+ INTERNAL)
     W1 = (-130.58878748879556, -175.07333221099998, 172.79336516697768) 29.122251552999998
     * 
     *
     *
     W2       -111.736598481    -69.088236186  165.067607370            -5.413000000 !was "-8.654293636???" (ASPRO 1)
     W2 = (-111.73659847358483, -69.088236186, 165.0676073825641)
     W2 = (-111.72810585237123, -69.0845925,   165.08924273662112)      -8.450162002
     *
     W2 = (-111.72648023453111, -69.093582796, 165.08103431717993)      -8.461862557
     W2 = (-111.72648023453111, -69.093582796, 165.08103431717993)      -8.079204556999999 (+ INTERNAL)
     W2 = (-111.72648023453111, -69.093582796, 165.08103431717993)      -8.079204556999999
     */
  }

  /**
   * Convert CHARA station configs to ASPRO switchyard
   * @param stationConfigs station configs
   * @param sb buffer
   */
  public static void convertCHARASwitchyard(final Map<String, Map<String, Double>> stationConfigs, final StringBuilder sb) {

    sb.append("    <switchyard>\n");

    final double[] values = new double[6];

    String station;
    Map<String, Double> config;
    for (Map.Entry<String, Map<String, Double>> e : stationConfigs.entrySet()) {
      station = e.getKey();
      config = e.getValue();

      for (int i = 0; i < 6; i++) {
        values[i] = config.get("BEAM" + (i + 1)) * 1e-6d;
      }

      convertCHARASwitchyardStation(station, values, sb);
    }

    sb.append("    </switchyard>\n\n");
  }

  /**
   * Convert CHARA station config to ASPRO stationLinks
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
   * @param absFileName absolute file path to CHARA config file
   * @return chara station config
   *
   # The USED labels and their data fields are:
   #
   # XOFFSET - East offset in microns from S1
   # YOFFSET - North offset in microns from S1
   # ZOFFSET - vertical (+ is up) offset in microns from S1
  
   # AIRPATH - amount of airpath  in microns using default beam
   #	    Note that this assumes the default Beam dn default Pop are used
   # INTERNAL- Pathlength (with default beam) for internal fringes
   # LIGHT	  - length of light pipe in microns
   #	    Note that this assumes the default Beam dn default Pop are used
  
   # BEAMX   - Extra airpath to add when using beam X on this scope
   # POPX	  - Extra airpath to add when using POP X on this scope
   */
  private static Map<String, Map<String, Double>> loadCHARAConfig(final String absFileName) {

    logger.info("loadCHARAConfig : " + absFileName);

    final List<String> labels = Arrays.asList(new String[]{
              "XOFFSET", "YOFFSET", "ZOFFSET", "AIRPATH", "LIGHT", "INTERNAL",
              "BEAM1", "BEAM2", "BEAM3", "BEAM4", "BEAM5", "BEAM6",
              "POP1", "POP2", "POP3", "POP4", "POP5"
            });

    final Map<String, Map<String, Double>> stationConfigs = new LinkedHashMap<String, Map<String, Double>>();

    // load data from file :
    BufferedReader reader = null;
    try {
      final File data = new File(absFileName);

      reader = new BufferedReader(new FileReader(data));

      // column separator :
      final String delimiter = " ";

      String name = null;
      String key = null;
      Double value = null;
      Map<String, Double> current = null;

      StringTokenizer tok;
      String line;
      while ((line = reader.readLine()) != null) {
        line = line.replaceAll("\\s+", " ").trim();
        if (line.length() > 0 && !line.startsWith("#")) {

          if (name == null) {
            // start station block :
            name = line;
            current = new LinkedHashMap<String, Double>();

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
      if (reader != null) {
        try {
          reader.close();
        } catch (IOException ioe) {
          logger.error("IO failure", ioe);
        }
      }
    }
    return stationConfigs;
  }

  /**
   * Convert the CHARA config file (telescopes.chara)
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
   * @param sb buffer
   * @return long and lat in degrees
   */
  private static double[] CHARAposition(final StringBuilder sb) {
    // mean coordinates from CHARA_plan (system.chara):
//    #  CHARA mean longitude and latitude from GPS
//       LONG  -118 03 26.574
//       LAT     34 13 34.110
//    #  Geodetic height of the CHARA reference plane [meters]:
//       HEIGHT  1725.21

    // S1 from telescopes.chara:
//    LONG    -118 3 25.31272
//    LAT       34 13 27.78130
    final double lonDeg = -(118d + 3d / 60d + 25.31272d / 3600d);
    logger.info("CHARA longitude (deg) : " + lonDeg);

    final double latDeg = 34d + 13d / 60d + 27.78130d / 3600d;
    logger.info("CHARA latitude (deg)  : " + latDeg);

    final double alt = 1725.21d;

    computeInterferometerPosition(lonDeg, latDeg, alt, sb);

    return new double[]{lonDeg, latDeg};
  }

  private static void VLTIPosition() {

    final StringBuilder sb = new StringBuilder(128);

    // ASPRO1: 17/04/2012:
    final double lonDeg = -70.40498688D;
    final double latDeg = -24.62743941D;
    logger.info("VLTI longitude / latitude (deg) : " + lonDeg + " - " + latDeg);

    final double alt = 2681d;
    final double[] pos = computeInterferometerPosition(lonDeg, latDeg, alt, sb);

    final Position3D position = new Position3D();
    position.setPosX(pos[0]);
    position.setPosY(pos[1]);
    position.setPosZ(pos[2]);

    final LonLatAlt coords = GeocentricCoords.getLonLatAlt(position);

    logger.info("VLTI position : " + coords.toString());

    /*    
     10:55:19.679 [main] INFO  AsproGenConfig - VLTI longitude / latitude (deg) : -70.40498688 - -24.62743941
     10:55:19.685 [main] INFO  AsproGenConfig - position (x,y,z) : 1942014.1545180853, -5455311.818167002, -2654530.4375114734
     10:55:19.690 [main] INFO  AsproGenConfig - VLTI position : [-70:24:17,953, -24:37:38,782, 2681.0000000009313 m]
     */
    logger.info("Generated VLTI position:\n" + sb.toString());
  }

  private static void MROIposition() {

    final StringBuilder sb = new StringBuilder(128);

    final double lonDeg = -(107d + 11d / 60d + 05.12d / 3600d);
    logger.info("MROI longitude (deg) : " + lonDeg);

    final double latDeg = 33d + 58d / 60d + 47.6d / 3600d;
    logger.info("MROI latitude (deg) : " + latDeg);

    final double alt = 3200d;
    computeInterferometerPosition(lonDeg, latDeg, alt, sb);

    logger.info("Generated MROI position:\n" + sb.toString());
  }

  /**
   * Convert geodetic long/lat/alt to geocentric coordinates
   * @param lon longitude in degrees
   * @param lat latitude in degrees
   * @param alt altitude in meters
   * @param sb buffer
   * @return positions (x,y,z)
   */
  private static double[] computeInterferometerPosition(final double lon, final double lat, final double alt, final StringBuilder sb) {

    final double theta = Math.toRadians(90d - lat);
    final double phi = Math.toRadians(lon);

    final double r = AsproConstants.EARTH_RADIUS + alt;

    final double x = r * Math.sin(theta) * Math.cos(phi);
    final double y = r * Math.sin(theta) * Math.sin(phi);
    final double z = r * Math.cos(theta);

    logger.info("position (x,y,z) : " + x + ", " + y + ", " + z);

    sb.append("    <position>\n");
    sb.append("      <posX>").append(x).append("</posX>\n");
    sb.append("      <posY>").append(y).append("</posY>\n");
    sb.append("      <posZ>").append(z).append("</posZ>\n");
    sb.append("    </position>\n\n");

    return new double[]{x, y, z};
  }

  /**
   * Convert the ASPRO 1 station file :
   *  W0  0			 0		 0		   0
   *  W1 -1.058755762353468	-7.2772199998144 1.570859050973913  7.52
   *  W2 -2.1386491936416	-14.937289999797 3.173079724470378  15.42
   *  W3 -3.163870843607306	-22.209509999956 4.694184747335298  22.92
   *
   * 165.8497 167.1097 -1000000 -1000000 173.8097 175.0697 -1000000 -1000000 U1
   * @param absFileName absolute file path to ASPRO 1 VLTI switchyard file
   */
  private static void convertStationFile(final String absFileName) {

    logger.info("convertStationFile : " + absFileName);

    final StringBuilder sb = new StringBuilder(16384);

    // number of columns filled with double values :
    final int maxCols = 5;
    // column separator :
    final String delimiter = " ";

    // load data from file :
    BufferedReader reader = null;
    try {
      final File data = new File(absFileName);

      reader = new BufferedReader(new FileReader(data));

      int i = 0;
      String line;
      StringTokenizer tok;
      // outputs :
      String station = null;
      double[] values = new double[maxCols];

      while ((line = reader.readLine()) != null) {
        line = line.replaceAll("\\s+", delimiter);
        tok = new StringTokenizer(line, delimiter);

        i = 0;
        station = null;
        while (tok.hasMoreTokens()) {
          if (i == 0) {
            // station name :
            station = tok.nextToken();
          } else if (i < maxCols) {
            values[i] = Double.parseDouble(tok.nextToken());
          } else {
            break;
          }
          i++;
        }

        if (station != null) {
          /*
           System.out.println("station : " + station);
           System.out.println("values : " + Arrays.toString(values));
           */
          /*
           <station>
           <name>S1</name>
           <telescope>T</telescope>
           <relativePosition>
           <posX>0.0</posX>
           <posY>0.0</posY>
           <posZ>0.0</posZ>
           </relativePosition>
           <delayLineFixedOffset>0.0</delayLineFixedOffset>
           ...
           </station>
           */

          // output :
          sb.append("<station>\n");
          sb.append("<name>").append(station).append("</name>\n");
          sb.append("<telescope>T</telescope>\n");

          sb.append("<relativePosition>\n");
          sb.append("<posX>").append(values[1]).append("</posX>\n");
          sb.append("<posY>").append(values[2]).append("</posY>\n");
          sb.append("<posZ>").append(values[3]).append("</posZ>\n");
          sb.append("</relativePosition>\n");

          sb.append("<delayLineFixedOffset>").append(values[4]).append("</delayLineFixedOffset>\n");

          sb.append("</station>\n");
        }
      }

    } catch (FileNotFoundException fnfe) {
      logger.error("File not found", fnfe);
    } catch (IOException ioe) {
      logger.error("IO failure", ioe);
    } finally {
      if (reader != null) {
        try {
          reader.close();
        } catch (IOException ioe) {
          logger.error("IO failure", ioe);
        }
      }
    }

    logger.info("convertStationFile : output :\n" + sb.toString());
  }

  /**
   * Load the SUSI config file (SUSI.txt)
   * @param absFileName absolute file path to SUSI config file
   * @return susi station config
   *
   */
  private static Map<String, Map<String, Double>> loadSUSIConfig(final String absFileName) {

    logger.info("loadSUSIConfig : " + absFileName);

    final List<String> labels = Arrays.asList(new String[]{
              "S1", "S2", "S3", "S4",
              "N1", "N3", "N4"
            });

    final String[] columns = new String[]{
      "index", "XOFFSET", "YOFFSET", "ZOFFSET", "WEIGHT", "DIAMETER"
    };

    final Map<String, Map<String, Double>> stationConfigs = new LinkedHashMap<String, Map<String, Double>>();

    // load data from file :
    BufferedReader reader = null;
    try {
      final File data = new File(absFileName);

      reader = new BufferedReader(new FileReader(data));

      // column separator :
      final String delimiter = " ";

      String name = null;
      String key = null;
      Double value = null;
      Map<String, Double> current = null;

      StringTokenizer tok;
      String line;
      while ((line = reader.readLine()) != null) {

        line = line.replaceAll("\\s+", " ").trim();

        // Parse values :
        logger.info("line = " + line);

        tok = new StringTokenizer(line, delimiter);

        if (tok.hasMoreTokens()) {
          // key
          key = tok.nextToken();

          if (labels.contains(key)) {
            name = key;

            logger.info("new station : " + name);
            current = new LinkedHashMap<String, Double>();

            int i = 0;
            // value (only first used) :
            while (tok.hasMoreTokens()) {
              key = columns[i];
              value = Double.valueOf(tok.nextToken());

              logger.info("value : " + value);

              current.put(key, value);
              i++;
            }

            // end station block :
            stationConfigs.put(name, current);

            logger.info("end station : " + name + " =\n" + current);
          }
        }
      }

    } catch (FileNotFoundException fnfe) {
      logger.error("File not found", fnfe);
    } catch (IOException ioe) {
      logger.error("IO failure", ioe);
    } finally {
      if (reader != null) {
        try {
          reader.close();
        } catch (IOException ioe) {
          logger.error("IO failure", ioe);
        }
      }
    }
    return stationConfigs;
  }

  /**
   * Convert SUSI station configs to ASPRO station configurations
   * @param stationConfigs station configs
   * @param sb buffer
   */
  private static void convertSUSIStations(final Map<String, Map<String, Double>> stationConfigs, final StringBuilder sb) {

    final double[] lonlat = SUSIposition(sb);

    final double lat = Math.toRadians(lonlat[1]);

    double x, y, z, offset;
    String station;
    Map<String, Double> config;
    for (Map.Entry<String, Map<String, Double>> e : stationConfigs.entrySet()) {
      station = e.getKey();
      config = e.getValue();

      sb.append("    <station>\n");
      sb.append("      <name>").append(station).append("</name>\n");
      sb.append("      <telescope>T</telescope>\n");

      x = config.get("XOFFSET");
      y = config.get("YOFFSET");
      z = config.get("ZOFFSET");

      convertHorizToEquatorial(station, lat, 1e6d * x, 1e6d * y, 1e6d * z, sb);

      // norm of baseline vector:
      offset = Math.sqrt(x * x + y * y + z * z);

      if (station.startsWith("N")) {
        offset -= 2.5d;
      } else {
        offset += 2.5d;
      }

      sb.append("      <delayLineFixedOffset>").append((int) Math.round(10d * offset) / 10d).append("</delayLineFixedOffset>\n");
      sb.append("    </station>\n\n");
    }
  }

  /**
   * Convert the SUSI config file (SUSI.txt)
   * @param absFileName absolute file path to SUSI config file
   */
  private static void convertSUSIConfig(final String absFileName) {

    final Map<String, Map<String, Double>> stationConfigs = loadSUSIConfig(absFileName);

    final StringBuilder sb = new StringBuilder(12 * 1024);

    sb.append("<a:interferometerSetting>\n\n");
    sb.append("  <description>\n\n    <name>SUSI</name>\n\n");

    convertSUSIStations(stationConfigs, sb);

    sb.append("  </description>\n\n</a:interferometerSetting>\n");

    logger.info("Generated SUSI Configuration : " + sb.length() + "\n" + sb.toString());
  }

  /**
   * Compute the SUSI position (S1 coordinates)
   * @param sb buffer
   * @return long and lat in degrees
   */
  private static double[] SUSIposition(final StringBuilder sb) {
    final double lonDeg = 149.548224972222d;
    logger.info("SUSI longitude (deg) : " + lonDeg);

    final double latDeg = -30.322273888889d;
    logger.info("SUSI latitude (deg) : " + latDeg);

    final double alt = 210d;
    computeInterferometerPosition(lonDeg, latDeg, alt, sb);

    logger.info("Generated SUSI position:\n" + sb.toString());

    return new double[]{lonDeg, latDeg};
  }


  /**
   * Load the NPOI config file (NPOI.txt)
   * @param absFileName absolute file path to NPOI config file
   * @return susi station config
   *
   */
  private static Map<String, Map<String, Double>> loadNPOIConfig(final String absFileName) {

    logger.info("loadNPOIConfig : " + absFileName);

    final List<String> labels = Arrays.asList(new String[]{
              "AC", "AE", "AW", "AN", 
              "W7", "E6"
            });
    
    final String[] columns = new String[]{
      "XOFFSET", "YOFFSET", "ZOFFSET"
    };

    final Map<String, Map<String, Double>> stationConfigs = new LinkedHashMap<String, Map<String, Double>>();

    // load data from file :
    BufferedReader reader = null;
    try {
      final File data = new File(absFileName);

      reader = new BufferedReader(new FileReader(data));

      // column separator :
      final String delimiter = " ";

      String name = null;
      String key = null;
      Double value = null;
      Map<String, Double> current = null;

      StringTokenizer tok;
      String line;
      while ((line = reader.readLine()) != null) {

        line = line.replaceAll("\\s+", " ").trim();

        // Parse values :
        logger.info("line = " + line);

        tok = new StringTokenizer(line, delimiter);

        if (tok.hasMoreTokens()) {
          // key
          key = tok.nextToken();

          if (labels.contains(key)) {
            name = key;

            logger.info("new station : " + name);
            current = new LinkedHashMap<String, Double>();

            int i = 0;
            // value (only first used) :
            while (tok.hasMoreTokens()) {
              key = columns[i];
              value = Double.valueOf(tok.nextToken());

              logger.info("value : " + value);

              current.put(key, value);
              i++;
            }

            // end station block :
            stationConfigs.put(name, current);

            logger.info("end station : " + name + " =\n" + current);
          }
        }
      }

    } catch (FileNotFoundException fnfe) {
      logger.error("File not found", fnfe);
    } catch (IOException ioe) {
      logger.error("IO failure", ioe);
    } finally {
      if (reader != null) {
        try {
          reader.close();
        } catch (IOException ioe) {
          logger.error("IO failure", ioe);
        }
      }
    }
    return stationConfigs;
  }

  /**
   * Convert SUSI station configs to ASPRO station configurations
   * @param stationConfigs station configs
   * @param sb buffer
   */
  private static void convertNPOIStations(final Map<String, Map<String, Double>> stationConfigs, final StringBuilder sb) {

    final double[] lonlat = NPOIposition(sb);

    final double lat = Math.toRadians(lonlat[1]);

    double x, y, z, offset;
    String station;
    Map<String, Double> config;
    for (Map.Entry<String, Map<String, Double>> e : stationConfigs.entrySet()) {
      station = e.getKey();
      config = e.getValue();

      sb.append("    <station>\n");
      sb.append("      <name>").append(station).append("</name>\n");
      sb.append("      <telescope>T</telescope>\n");

      x = config.get("XOFFSET");
      y = config.get("YOFFSET");
      z = config.get("ZOFFSET");

      convertHorizToEquatorial(station, lat, 1e6d * x, 1e6d * y, 1e6d * z, sb);

      // norm of baseline vector:
      offset = Math.sqrt(x * x + y * y + z * z);
/*
      if (station.startsWith("N")) {
        offset -= 2.5d;
      } else {
        offset += 2.5d;
      }
*/
      sb.append("      <delayLineFixedOffset>").append((int) Math.round(10d * offset) / 10d).append("</delayLineFixedOffset>\n");
      sb.append("    </station>\n\n");
    }
  }
  
  /**
   * Compute the NPOI position
   */
  private static double[] NPOIposition(final StringBuilder sb) {

    final double lonDeg = -111.535d;
    logger.info("NPOI longitude (deg) : " + lonDeg);

    final double latDeg = 35.09666d;
    logger.info("NPOI latitude (deg) : " + latDeg);

    final double alt = 2200.66d;
    computeInterferometerPosition(lonDeg, latDeg, alt, sb);

    logger.info("Generated NPOI position:\n" + sb.toString());

    return new double[]{lonDeg, latDeg};
  }

  /**
   * Convert the NPOI config file (NPOI.txt)
   * @param absFileName absolute file path to NPOI config file
   */
  private static void convertNPOIConfig(final String absFileName) {

    final Map<String, Map<String, Double>> stationConfigs = loadNPOIConfig(absFileName);

    final StringBuilder sb = new StringBuilder(12 * 1024);

    sb.append("<a:interferometerSetting>\n\n");
    sb.append("  <description>\n\n    <name>NPOI</name>\n\n");

    convertNPOIStations(stationConfigs, sb);

    sb.append("  </description>\n\n</a:interferometerSetting>\n");

    logger.info("Generated NPOI Configuration : " + sb.length() + "\n" + sb.toString());
  }
  
  /**
   * Main entry point to generate configuration parts (xml)
   * @param args unused
   */
  public static void main(final String[] args) {
    final String asproPath = "/home/bourgesl/dev/aspro1/etc/";

    final INTERFEROMETER selected = INTERFEROMETER.NPOI;

    switch (selected) {
      case VLTI:
        VLTIPosition();

        convertSwitchYard(asproPath + "VLT.switchyard");

        final String[] vltStations = {
          "U1", "U2", "U3", "U4", "A0", "A1", "B0", "B1", "B2", "B3", "B4", "B5",
          "C0", "C1", "C2", "C3", "D0", "D1", "D2", "E0", "G0", "G1", "G2", "H0",
          "I1", "J1", "J2", "J3", "J4", "J5", "J6", "K0", "L0", "M0"};

        final StringBuilder sb = new StringBuilder(65535);

        for (String station : vltStations) {
          convertHorizon(station, asproPath + station + ".horizon", sb);
        }
        logger.info("convertHorizons : \n" + sb.toString());
        break;
      case CHARA:
        convertCHARAConfig("/home/bourgesl/dev/aspro/test/telescopes.chara");
        break;
      case SUSI:
        convertSUSIConfig("/home/bourgesl/dev/aspro/test/SUSI.txt");
        break;
      case NPOI:
        convertNPOIConfig("/home/bourgesl/dev/aspro/test/NPOI.txt");
        break;
      case MROI:
        MROIposition();

        convertStationFile(asproPath + "MROI.stations");
        break;

      default:
        logger.info("unsupported interferometer : " + selected);
    }
  }
}
