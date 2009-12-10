
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.StringTokenizer;
import java.util.logging.Level;

/**
 * This class converts the VLT switchyard to an XML fragment compliant with the aspro DM
 * 
 * @author bourgesl
 */
public class AsproGenConfig {

  /** Class Name */
  private static final String className_ = "AsproGenConfig";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /**
   * VLT switchyard :
   * 165.8497 167.1097 -1000000 -1000000 173.8097 175.0697 -1000000 -1000000 U1
   */
  private static void convertSwitchYard(final String absFileName) {

    logger.severe("convertSwitchYard : " + absFileName);

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
      logger.log(Level.SEVERE, null, fnfe);
    } catch (IOException ioe) {
      logger.log(Level.SEVERE, null, ioe);
    } finally {
      if (reader != null) {
        try {
          reader.close();
        } catch (IOException ex) {
          logger.log(Level.SEVERE, null, ex);
        }
      }
    }

    sb.append("</switchyard>\n");

    logger.severe("convertSwitchYard : output :\n" + sb.toString());
  }

  /**
   * VLT horizon :
  360.000000000000       0.000000000000000E+000
  235.000000000000       0.000000000000000E+000
  235.000000000000        8.00000000000000
  224.000000000000        8.00000000000000
  224.000000000000       0.000000000000000E+000
  ...
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
        /*
        <horizon>
        <point>
        <azimuth></azimuth>
        <elevation></elevation>
        </point>
        </horizon>
         */

        // output :
        sb.append("<point>");
        sb.append("<azimuth>").append(values[0]).append("</azimuth>");
        sb.append("<elevation>").append(values[1]).append("</elevation>");
        sb.append("</point>");
      }

    } catch (FileNotFoundException fnfe) {
      logger.log(Level.SEVERE, null, fnfe);
    } catch (IOException ioe) {
      logger.log(Level.SEVERE, null, ioe);
    } finally {
      if (reader != null) {
        try {
          reader.close();
        } catch (IOException ex) {
          logger.log(Level.SEVERE, null, ex);
        }
      }
    }

    sb.append("</horizon>\n");
    sb.append("</station>\n");

  }

  private static void convertHorizToEquatorial(final String name, final double latitude, final double xOffset,final double yOffset, final double zOffset) {

    final double x = xOffset * 1e-6d;
    final double y = yOffset * 1e-6d;
    final double z = zOffset * 1e-6d;
    
    double xx, yy,zz;

    xx = - Math.sin(latitude) * y + Math.cos(latitude) * z;
    yy = x;
    zz = Math.cos(latitude) * y + Math.sin(latitude) * z;

    logger.severe(name + " = (" + xx + ", " + yy + ", " +zz +")");
  }

  private static void convertCHARAStations() {

    // CHARA :
    //LAT 34 13 27.78130

    final double latDeg = 34d + 13d / 60d + 27.78130d / 3600d;
    logger.severe("chara lat (deg) : " + latDeg);

    final double lat = Math.toRadians(latDeg);

    double XOFFSET, YOFFSET, ZOFFSET;

    // #S2
    XOFFSET = -5748393.059d;
    YOFFSET = 33581340.790d;
    ZOFFSET = 643880.188d;
    convertHorizToEquatorial("S2", lat, XOFFSET, YOFFSET, ZOFFSET);

    /*
S2        -18.354945174  -5.748393059    28.128580103   4.532654762
S2 = (-18.35494517183226, -5.7483930590000005, 28.128580104400015)
     */

    // #E1
    XOFFSET = 125349003.099d;
    YOFFSET = 305937513.274d;
    ZOFFSET = -5898978.192d;
    convertHorizToEquatorial("E1", lat, XOFFSET, YOFFSET, ZOFFSET);

    /*
E1       -176.947579603  125.349003099  249.643974581  15.309717902
E1 = (-176.94757958628557, 125.349003099, 249.64397459290652)
     */

    // #E2
    XOFFSET = 70400864.470d;
    YOFFSET = 269716660.932d;
    ZOFFSET = -2787652.960d;
    convertHorizToEquatorial("E2", lat, XOFFSET, YOFFSET, ZOFFSET);

    /*
E2       -154.003121030   70.400864470  221.444997474  26.721359853
E2 = (-154.00312101526163, 70.40086447, 221.44499748445782)
     */

    // #W1
XOFFSET = -175068583.489d;
YOFFSET = 216322063.446d;
ZOFFSET = -10827769.936d;
    convertHorizToEquatorial("W1", lat, XOFFSET, YOFFSET, ZOFFSET);

    /*
W1       -130.620014605 -175.068583489  172.774093891  29.087218284
W1 = (-130.6200145937469, -175.068583489, 172.7740938996687)
     */

    // #W2
    XOFFSET = -69088236.186d;
    YOFFSET = 199329322.146d;
    ZOFFSET = 451418.063d;
    convertHorizToEquatorial("W2", lat, XOFFSET, YOFFSET, ZOFFSET);

    /*
W2       -111.736598481  -69.088236186  165.067607370  -5.413000000 !was "-8.654293636???"
W2 = (-111.73659847358483, -69.088236186, 165.0676073825641)
     */

  }


  /**
   * Test code
   */
  public static void main(final String[] args) {
    if (false) {
      final String asproPath = "/home/bourgesl/dev/aspro-NEW/etc/";
      convertSwitchYard(asproPath + "VLT.switchyard");

      final String[] vltStations = {
        "U1", "U2", "U3", "U4", "A0", "A1", "B0", "B1", "B2", "B3", "B4", "B5",
        "C0", "C1", "C2", "C3", "D0", "D1", "D2", "E0", "G0", "G1", "G2", "H0",
        "I1", "J1", "J2", "J3", "J4", "J5", "J6", "K0", "L0", "M0"};

      final StringBuilder sb = new StringBuilder(65535);

      for (String station : vltStations) {
        convertHorizon(station, asproPath + station + ".horizon", sb);
      }
      logger.severe("convertHorizons : \n" + sb.toString());
    }

    convertCHARAStations();
  }
}
