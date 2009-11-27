
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Arrays;
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

  /**
   * Test code
   */
  public static void main(final String[] args) {
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
}
