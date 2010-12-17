/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ExportOBVega.java,v 1.8 2010-12-17 15:08:33 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.7  2010/10/04 16:25:25  bourgesl
 * proper IO exception handling
 *
 * Revision 1.6  2010/06/25 14:17:21  bourgesl
 * refactoring due to changes done in AstroSkyCalc and AstroSkyCalcObservation
 *
 * Revision 1.5  2010/06/17 10:02:51  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.4  2010/06/07 16:03:48  bourgesl
 * minimum elevation changed to 30 degrees
 *
 * Revision 1.3  2010/05/31 09:45:00  bourgesl
 * restored fluxes H and J
 *
 * Revision 1.2  2010/05/27 10:09:48  bourgesl
 * inverted flux H and J to be closer to VEGA_PLAN output
 * removed SEVERE traces
 *
 * Revision 1.1  2010/05/26 15:30:54  bourgesl
 * new CHARA Vega Star List generation (OB like)
 *
 */
package fr.jmmc.aspro.ob;

import edu.dartmouth.AstroSkyCalcObservation;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.oi.Channel;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Pop;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.service.ObservabilityService;
import fr.jmmc.aspro.util.FileUtils;
import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.List;
import java.util.logging.Level;

/**
 * This class implements the Star List generation for the CHARA Vega instrument.
 *
 * @author bourgesl
 */
public class ExportOBVega {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.ob.ExportOBVega";
  /** Class logger */
  protected static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** double formatter for magnitudes */
  protected final static NumberFormat df2 = new DecimalFormat("0.00");
  /** default value for undefined magnitude = 99 */
  public final static double UNDEFINED_MAGNITUDE = 99d;
  /** default value for undefined angular diameter = 99 */
  public final static double UNDEFINED_DIAMETER = 99d;
  /** HD catalog identifier (SimBad) */
  public final static String HD_CATALOG = "HD";

  /* CHARA Vega StarList File constants */
  /** field separator */
  public final static String SEPARATOR = " ";
  /** Undefined value = OFF */
  public final static String UNDEFINED = "OFF";
  /** Star - observation = OBSV */
  public final static String STAR_OBSV = "OBSV";
  /** Star - target = TARGET */
  public final static String STAR_TARGET = "TARGET";
  /** Default XOPLE reference = 0 */
  public final static String DEFAULT_XOPLE_REF = "0";
  /**
   * Default Vega setup
   *   1.Tracking detector (Red/Blue) = R
   *   2.Slit selection = W070H4
   *   3.Grating selection = 300
   *   4.Camera = RB
   *   5.Lambda = 656.0
   *   6.polar choice (Off, ) = OFF
   *   7.Red density = OPEN
   *   8.Blue density = OPEN
   *   9.Record configuration = RB
   *   10.Nb block = 20
   *   11.No Image = 1000
   */
  public final static String DEFAULT_VEGA_SETUP = "R W070H4 300 RB 656.0 OFF OPEN OPEN RB 20 1000";

  /**
   * Forbidden constructor
   */
  protected ExportOBVega() {
    // no-op
  }

  /**
   * Generate the Star List for the current observation.
   * @param file file to save
   * @throws IOException if an I/O exception occured while writing the observing block
   */
  public static void process(final File file) throws IOException {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("generate file : " + file);
    }

    // get observation :
    final ObservationSetting observation = ObservationManager.getInstance().getObservation();

    // Compute Observability data with min elevation = 30 deg and without night restrictions :
    final ObservabilityService os = new ObservabilityService(observation, AsproConstants.OB_MIN_ELEVATION);
    final ObservabilityData obsData = os.compute();

    // Prepare the Chara Setup according to the Pop configuration :
    final String charaSetup = prepareCharaSetup(observation, obsData.getBestPops().getPopList());

    // Prepare the baseline suffix for observation name :
    final String baseLine = observation.getInstrumentConfiguration().getStations().replaceAll(" ", "");

    // Generate the file content :
    final StringBuilder sb = new StringBuilder(512);

    // TODO CALS : we should use the display targets to have both science targets with their calibrators

    // fill the buffer per target :
    for (Target target : observation.getTargets()) {
      processTarget(sb, obsData, target, baseLine, charaSetup);
    }

    final String document = sb.toString();

    // Finally, write the file :
    FileUtils.writeFile(file, document);
  }

  /**
   *
   *   Star information
   *   1.OBSV
   *   2.TARGET or REFERENCE
   *   3.Name of observation (string)
   *   4.HD number (string)
   *   5.Spectral Type (string)
   *   6.V magnitude (float)
   *   7.J magnitude (float)
   *   8.H magnitude (float)
   *   9.K magnitude (float)
   *   10.Angular diameter (float, mas)
   *   11.right ascension (hh:mm:ss.ss)
   *   12.declination (dd:mm:ss.s)
   *   13.proper motion alpha (float, mas/yr)
   *   14.proper motion delta (float, mas/yr)
   *   15.parallax (float, mas)
   *   16.Minimum Hour Angle (z>30° & OPLE)
   *   17.Maximum Hour Angle (z>30° & OPLE)
   *
   * For example : OBSV TARGET epsOriS1S2 HD37128 B0Iab: 1.70 2.41 2.19 2.27 0.77 5:36:12.81 -1:12:6.9 1.49 -1.06 2.43 -3.42 3.42 
   *
   * @param sb string buffer
   * @param obsData observability results
   * @param target target to process
   * @param baseLine baseline like S1S2 ...
   * @param charaSetup chara setup
   */
  public static void processTarget(final StringBuilder sb, final ObservabilityData obsData, final Target target, final String baseLine, final String charaSetup) {

    final String hdId = target.getIdentifier(HD_CATALOG);
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("target : " + target.getName() + " - " + hdId);
    }

    // Get target HA ranges :
    final List<Range> rangesHA = getTargetHARange(obsData, target);
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("rangesHA : " + rangesHA);
    }

    // Skip unobservable target ?
    // Skip target without HD identifier ?

    if (hdId != null && rangesHA != null) {

      // Star information
      //   1.OBSV
      sb.append(STAR_OBSV).append(SEPARATOR);
      //   2.TARGET or REFERENCE
      sb.append(STAR_TARGET).append(SEPARATOR);
      // 3.Name of observation (string) = target name (only alpha and digit characters) + Base Line
      final String altName = target.getName().replaceAll("[^a-zA-Z_0-9]", "");
      sb.append(altName).append(baseLine).append(SEPARATOR);
      // 4.HD number (string)
      sb.append(hdId.replace(" ", "")).append(SEPARATOR);
      // 5.Spectral Type (string)
      final String spectralType = (target.getSPECTYP() != null && target.getSPECTYP().length() > 0) ? target.getSPECTYP() : SEPARATOR;
      sb.append(spectralType).append(SEPARATOR);
      // 6.V magnitude (float)
      sb.append(df2.format(getMagnitude(target.getFLUXV()))).append(SEPARATOR);
      // 7.J magnitude (float)
      sb.append(df2.format(getMagnitude(target.getFLUXJ()))).append(SEPARATOR);
      // 8.H magnitude (float)
      sb.append(df2.format(getMagnitude(target.getFLUXH()))).append(SEPARATOR);
      // 9.K magnitude (float)
      sb.append(df2.format(getMagnitude(target.getFLUXK()))).append(SEPARATOR);
      // 10.Angular diameter (float, mas)
      sb.append(df2.format(UNDEFINED_DIAMETER)).append(SEPARATOR);

      // convert RA/DEC :
      final String[] raDec = AstroSkyCalcObservation.toString(target.getRADeg(), 2, target.getDECDeg(), 1);

      // 11.right ascension (hh:mm:ss.ss)
      sb.append(raDec[0]).append(SEPARATOR);
      // 12.declination (dd:mm:ss.s)
      sb.append(raDec[1].replace("+", "")).append(SEPARATOR);

      // 13.proper motion alpha (float, mas/yr)
      final double pmRA = (target.getPMRA() != null) ? target.getPMRA().doubleValue() : 0d;
      sb.append(df2.format(pmRA)).append(SEPARATOR);
      // 14.proper motion delta (float, mas/yr)
      final double pmDEC = (target.getPMDEC() != null) ? target.getPMDEC().doubleValue() : 0d;
      sb.append(df2.format(pmDEC)).append(SEPARATOR);

      // 15.parallax (float, mas)
      final double parallax = (target.getPARALLAX() != null) ? target.getPARALLAX().doubleValue() : 0d;
      sb.append(df2.format(parallax)).append(SEPARATOR);

      // How to handle the special case of multiple ranges ?

      // 16.Minimum Hour Angle (z>30° & OPLE)
      final double haMin = Range.getMinimum(rangesHA);
      sb.append(df2.format(haMin)).append(SEPARATOR);

      // 17.Maximum Hour Angle (z>30° & OPLE)
      final double haMax = Range.getMaximum(rangesHA);
      sb.append(df2.format(haMax)).append(SEPARATOR);

      // Chara Setup
      sb.append(charaSetup);
      // Vega Setup
      sb.append(DEFAULT_VEGA_SETUP);
      // End of line :
      sb.append(FileUtils.LINE_SEPARATOR);
    }
  }

  /**
   * Return the observable HA ranges restricted by the user defined HA Min / Max
   * @param obsData observability results
   * @param target target to use
   * @return observable HA ranges
   */
  private static List<Range> getTargetHARange(final ObservabilityData obsData, final Target target) {
    final StarData starData = obsData.getStarData(target.getName());
    if (starData != null) {
      final List<Range> obsRangesHA = starData.getObsRangesHA();

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("obsRangesHA = " + obsRangesHA);
      }

      if (obsRangesHA != null) {
        // target is observable :

        double haMin = AsproConstants.HA_MIN;
        double haMax = AsproConstants.HA_MAX;

        // HA Min / Max :
        final TargetConfiguration targetConf = target.getConfiguration();
        if (targetConf != null) {
          if (targetConf.getHAMin() != null) {
            haMin = targetConf.getHAMin().doubleValue();
          }
          if (targetConf.getHAMax() != null) {
            haMax = targetConf.getHAMax().doubleValue();
          }
        }
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("ha min    : " + haMin);
          logger.fine("ha max    : " + haMax);
        }

        final List<Range> limRangesHA = Range.restrictRange(obsRangesHA, haMin, haMax);

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("limRangesHA = " + limRangesHA);
        }
        return limRangesHA;
      }
    }
    return null;
  }

  /**
   * Determine the Chara Setup :
   *   1.XOPLE for reference
   *   2.T1 (E1, E2, S1, S2, W1, W2)
   *   3.Pop1 (POP1, POP2, POP3, POP4, POP5)
   *   4.Switchyard 1 (V1, V2, V3, V4)
   *   5.T2
   *   6.Pop2
   *   7.Switchyard 2
   *   8.T3
   *   9.Pop3
   *   10.Switchyard 3
   *   11.T4
   *   12.Pop4
   *   13.Switchyard 4
   *
   * For example : 15 S1 POP4 V2 S2 POP5 V1 OFF OFF OFF OFF OFF OFF 
   *
   * @param observation current observation
   * @param popList list of PoPs
   * @return string representing the Chara Setup
   */
  private static String prepareCharaSetup(final ObservationSetting observation, final List<Pop> popList) {

    // Get chosen stations :
    final List<Station> stations = observation.getInstrumentConfiguration().getStationList();

    // CHARA : predefined channel per station for a specific base line :
    final List<Channel> relatedChannels = ConfigurationManager.getInstance().getInstrumentConfigurationChannels(
            observation.getInterferometerConfiguration().getName(),
            observation.getInstrumentConfiguration().getName(),
            observation.getInstrumentConfiguration().getStations());

    final int nStations = stations.size();

    // Compute the Chara Setup :
    final StringBuilder sb = new StringBuilder(64);

    sb.append(DEFAULT_XOPLE_REF).append(SEPARATOR);

    String telescope, pop, switchyard;

    for (int i = 0; i < 4; i++) {
      /*
       * Telescope (E1, E2, S1, S2, W1, W2)
       * Pop (POP1, POP2, POP3, POP4, POP5)
       * Switchyard (V1, V2, V3, V4)
       */
      if (i < nStations) {
        telescope = stations.get(i).getName();
        pop = popList.get(i).getName().toUpperCase();
        switchyard = relatedChannels.get(i).getName();
      } else {
        telescope = UNDEFINED;
        pop = UNDEFINED;
        switchyard = UNDEFINED;
      }
      sb.append(telescope).append(SEPARATOR).append(pop).append(SEPARATOR).append(switchyard).append(SEPARATOR);
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Chara Setup : " + sb.toString());
    }

    return sb.toString();
  }

  /**
   * Return the given magnitude or the undefined magnitude value
   * @param mag magnitude
   * @return given magnitude or the undefined magnitude value
   */
  protected static double getMagnitude(final Double mag) {
    if (mag != null) {
      return mag.doubleValue();
    }
    return UNDEFINED_MAGNITUDE;
  }
}
