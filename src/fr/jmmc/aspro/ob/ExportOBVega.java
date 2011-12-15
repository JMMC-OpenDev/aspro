/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
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
import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.model.oi.TargetInformation;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.service.ObservabilityService;
import fr.jmmc.jmcs.util.FileUtils;
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

  /** Class logger */
  protected static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(ExportOBVega.class.getName());
  /** double formatter for magnitudes */
  protected final static NumberFormat df2 = new DecimalFormat("0.00");
  /** default value for undefined angular diameter = 99 */
  public final static Double UNDEFINED_DIAMETER = Double.valueOf(99d);
  /** HD catalog identifier (Simbad) */
  public final static String HD_CATALOG = "HD";
  /** PIVOT identifier (TARGET/IDS) */
  public final static String PIVOT_IDENTIFIER = "PIVOT";

  /* CHARA Vega StarList File constants */
  /** field separator */
  public final static String SEPARATOR = " ";
  /** Undefined value = OFF */
  public final static String UNDEFINED = "OFF";
  /** Star - observation = OBSV */
  public final static String STAR_OBSV = "OBSV";
  /** Science target = TARGET */
  public final static String SCI_TARGET = "TARGET";
  /** Calibrator target = REFERENCE */
  public final static String CAL_TARGET = "REFERENCE";
  /** Calibrator prefix = Cal */
  public final static String CAL_PREFIX = "Cal";
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

    // use main observation :
    final ObservationSetting observation = ObservationManager.getInstance().getMainObservation();

    // Compute Observability data using astronomical night (-18 deg) without night restrictions :
    final ObservabilityService os = new ObservabilityService(observation, true);
    final ObservabilityData obsData = os.compute();

    // Prepare the Chara Setup according to the Pop configuration :
    final String charaSetup = prepareCharaSetup(observation, obsData.getBestPops().getPopList());

    // Prepare the baseline suffix for observation name :
    final String baseLine = observation.getInstrumentConfiguration().getStations().replaceAll(" ", "");

    // Generate the file content :
    final StringBuilder sb = new StringBuilder(512);

    // get target user informations to have proper target ordering (science + calibrators):
    final TargetUserInformations targetUserInfos = observation.getOrCreateTargetUserInfos();

    String scienceTargetName;
    String scienceTargetId;
    TargetInformation targetInfo;
    List<Target> calibrators;
    int calIndex;

    // fill the buffer per target :
    for (Target target : observation.getTargets()) {

      // first science targets:
      if (!targetUserInfos.isCalibrator(target)) {
        
        // Get ID PIVOT of the science target (null)
        scienceTargetId = getPivotIdentifier(target);

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("target : " + target.getName() + " - PIVOT: " + scienceTargetId);
        }
        
        scienceTargetName = processTarget(sb, obsData, target, baseLine, charaSetup, 0, null, scienceTargetId);

        if (scienceTargetName != null) {
          // science OB processed:
          targetInfo = targetUserInfos.getTargetInformation(target);
          if (targetInfo != null) {
            calibrators = targetInfo.getCalibrators();

            if (!calibrators.isEmpty()) {
              // then calibrator targets:
              calIndex = 0;
              for (Target calibrator : calibrators) {
                calIndex++;
                processTarget(sb, obsData, calibrator, baseLine, charaSetup, calIndex, scienceTargetName, scienceTargetId);
              }
            }
          }
        }
      }
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
   * Add also Default Vega setup
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
   * 
   * Add at the end ID_PIVOT or '-' if null
   *
   * @param sb string buffer
   * @param obsData observability results
   * @param target target to process
   * @param baseLine baseline like S1S2 ...
   * @param charaSetup chara setup
   * @param calibratorIndex 0 means science target, >= 1 indicates the calibrator position in the calibrator list
   * @param scienceTargetName science target name to form the name of this observation block
   * @param scienceTargetId science target identifier to use for this observation block
   * @return science target name
   */
  public static String processTarget(final StringBuilder sb, final ObservabilityData obsData, final Target target, final String baseLine, final String charaSetup,
                                     final int calibratorIndex, final String scienceTargetName, final String scienceTargetId) {

    String result = null;

    final boolean isCalibrator = (calibratorIndex > 0);

    final String hdId = target.getIdentifier(HD_CATALOG);
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("target : " + target.getName() + " - " + hdId);
    }
    if (hdId == null) {
      logger.warning("Missing HD identifier for target [" + target + "]");
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
      sb.append((isCalibrator) ? CAL_TARGET : SCI_TARGET).append(SEPARATOR);
      // 3.Name of observation (string) = science target name (only alpha and digit characters) + (codes Cal1, Cal2, Chk) + Base Line
      if (isCalibrator) {
        sb.append(scienceTargetName).append(CAL_PREFIX).append(calibratorIndex);
      } else {
        result = target.getName().replaceAll(AsproConstants.REGEXP_INVALID_TEXT_CHARS, "");
        sb.append(result);
      }
      sb.append(baseLine).append(SEPARATOR);
      // 4.HD number (string)
      sb.append(hdId.replace(" ", "")).append(SEPARATOR);
      // 5.Spectral Type (string)
      final String spectralType = (target.getSPECTYP() != null && target.getSPECTYP().length() > 0) ? target.getSPECTYP() : SEPARATOR;
      sb.append(spectralType.replaceAll(" ", "_")).append(SEPARATOR);
      // 6.V magnitude (float)
      sb.append(df2.format(getMagnitude(target.getFLUXV()))).append(SEPARATOR);
      // 7.J magnitude (float)
      sb.append(df2.format(getMagnitude(target.getFLUXJ()))).append(SEPARATOR);
      // 8.H magnitude (float)
      sb.append(df2.format(getMagnitude(target.getFLUXH()))).append(SEPARATOR);
      // 9.K magnitude (float)
      sb.append(df2.format(getMagnitude(target.getFLUXK()))).append(SEPARATOR);

      // 10.Angular diameter (float, mas)
      Double diameter = null;
      if (isCalibrator) {
        diameter = target.getDiameter(SpectralBand.V);
      }
      if (diameter == null) {
        diameter = UNDEFINED_DIAMETER;
      }
      
      sb.append(df2.format(diameter)).append(SEPARATOR);

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
      
      // Identifier PIVOT if defined:
      if (scienceTargetId != null) {
        sb.append(SEPARATOR).append(scienceTargetId);
      }
      
      // End of line :
      sb.append(FileUtils.LINE_SEPARATOR);
    }
    return result;
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
   * Return magnitude value or -99 if the magnitude is null
   * @param mag magnitude or null
   * @return magnitude value or -99 if the magnitude is null
   */
  protected static double getMagnitude(final Double mag) {
    if (mag != null) {
      return mag.doubleValue();
    }
    return AsproConstants.UNDEFINED_MAGNITUDE;
  }
  
  /**
   * Extract the PIVOT identifier from TARGET/IDS
   * @param target target to use
   * @return PIVOT identifier
   */
  private static String getPivotIdentifier(final Target target) {
    String id = target.getIdentifier(PIVOT_IDENTIFIER);
    if (id == null) {
      return null;
    }
    id = id.substring(PIVOT_IDENTIFIER.length());
    return id.trim();
  }
}
