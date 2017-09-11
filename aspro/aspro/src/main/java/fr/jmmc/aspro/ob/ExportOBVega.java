/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.ob;

import edu.dartmouth.AstroSkyCalcObservation;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.oi.Channel;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Pop;
import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetInformation;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.jmcs.util.StringUtils;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.List;
import org.apache.commons.lang.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class implements the Star List generation for the CHARA Vega instrument.
 *
 * @author bourgesl
 */
public final class ExportOBVega {

  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(ExportOBVega.class.getName());
  /** double formatter for magnitudes */
  private final static NumberFormat df2 = new DecimalFormat("0.00");
  /** default value for undefined angular diameter = 99 */
  public final static Double UNDEFINED_DIAMETER = Double.valueOf(99d);
  /** HD catalog identifier (Simbad) */
  public final static String HD_CATALOG = "HD";
  /** PIVOT identifier (TARGET/IDS) */
  public final static String PIVOT_IDENTIFIER = "PIVOT";

  /* CHARA Vega StarList File constants */
  /** field separator ( )*/
  public final static String SEPARATOR = " ";
  /** end of line character (\n) */
  public final static String END_OF_LINE = SystemUtils.LINE_SEPARATOR;
  /** line separator (****) */
  public final static String LINE_SEPARATOR = "****" + END_OF_LINE;
  /** file separator (####) */
  public final static String END_OF_FILE = "####" + END_OF_LINE;
  /** Undefined value = OFF */
  public final static String UNDEFINED = "OFF";

  /* Observation OB */
  /** observation = OBSV */
  public final static String OB_OBS = "OBSV";
  /** Science target = TARGET */
  public final static String SCI_TARGET = "TARGET";
  /** Calibrator target = REFERENCE */
  public final static String CAL_TARGET = "REFERENCE";
  /** Calibrator prefix = Cal */
  public final static String CAL_PREFIX = "Cal";
  /** Default XOPLE reference = 0 */
  public final static String DEFAULT_XOPLE_REF = "0";

  /* Calibration OB */
  /** Spectral calibration = CALI SPEC */
  public final static String OB_CAL = "CALI SPEC";
  /* Vega Setup StarList File constants */
  /** Default #BLOCS for calibration (1) */
  public final static String DEFAULT_BLOCS_CALIB = "1";
  /** Default #FRAMES for calibration (4000) */
  public final static String DEFAULT_FRAMES_CALIB = "4000";

  /* Instrument mode Parameters */
  /** Parameter TRACKING */
  public final static String PARAM_TRACKING = "TRACKING";
  /** Parameter SLIT */
  public final static String PARAM_SLIT = "SLIT";
  /** Parameter GRATING */
  public final static String PARAM_GRATING = "GRATING";
  /** Parameter CAMERA */
  public final static String PARAM_CAMERA = "CAMERA";
  /** Parameter LAMBDA */
  public final static String PARAM_LAMBDA = "LAMBDA";
  /** Parameter SPIN */
  public final static String PARAM_SPIN = "SPIN";
  /** Parameter RED FILTER */
  public final static String PARAM_RED_FILTER = "RED FILTER";
  /** Parameter BLUE FILTER */
  public final static String PARAM_BLUE_FILTER = "BLUE FILTER";
  /** Parameter RECORD */
  public final static String PARAM_RECORD = "RECORD";
  /** Parameter BLOCKS */
  public final static String PARAM_BLOCKS = "BLOCKS";
  /** Parameter FRAMES */
  public final static String PARAM_FRAMES = "FRAMES";

  /**
   * Forbidden constructor
   */
  protected ExportOBVega() {
    // no-op
  }

  /**
   * Generate the Star List for the current observation into the given buffer
   * @param sb buffer to fill
   * @param observation observation to use
   * @param obsData computed observability data using astronomical night (-18 deg) without night restrictions
   */
  public static void generate(final StringBuilder sb, final ObservationSetting observation, final ObservabilityData obsData) {

    // Prepare the Chara Setup according to the Pop configuration :
    final String charaSetup = prepareCharaSetup(observation, obsData.getBestPops().getPops());

    // Prepare the Vega Setup according to the instrument mode :
    final String vegaSetup = prepareVegaSetup(observation);

    // Prepare the baseline suffix for observation name :
    final String baseLine = StringUtils.removeWhiteSpaces(observation.getInstrumentConfiguration().getStations());

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

        if (logger.isDebugEnabled()) {
          logger.debug("target: {} - PIVOT: {}", target.getName(), scienceTargetId);
        }

        scienceTargetName = processTarget(sb, obsData, target, baseLine, charaSetup, vegaSetup, 0, null, scienceTargetId);

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
                processTarget(sb, obsData, calibrator, baseLine, charaSetup, vegaSetup, calIndex, scienceTargetName, scienceTargetId);
              }
            }
          }
        }
      }
    }

    sb.append(LINE_SEPARATOR);

    // Spectral calibration OB:
    processCalib(sb, observation);

    sb.append(END_OF_FILE);
  }

  /**
   * Add an observation OB (target or calibrator) to the current star list
   * 1/ Star information:
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
   * 2/ Chara Setup:
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
   * 3/ Vega Setup:
   *   1. Tracking detector (Red/Blue) = R
   *   2. Slit selection = W070H4
   *   3. Grating selection = 300
   *   4. Camera = RB
   *   5. Lambda = 656.0
   *   6. polar choice (Off, ) = OFF
   *   7. Red density = OPEN
   *   8. Blue density = OPEN
   *   9. Record configuration = RB
   *   10.Nb block = 20
   *   11.No Image = 1000
   *
   * For example: "R W070H4 300 RB 656.2 OFF OPEN OPEN RB 20 1000"
   * 
   * 4/ Add at the end the optional ID_PIVOT
   *
   * @param sb string buffer
   * @param obsData observability results
   * @param target target to process
   * @param baseLine baseline like S1S2 ...
   * @param charaSetup chara setup
   * @param vegaSetup vega setup
   * @param calibratorIndex 0 means science target, >= 1 indicates the calibrator position in the calibrator list
   * @param scienceTargetName science target name to form the name of this observation block
   * @param scienceTargetId science target identifier to use for this observation block or null if undefined
   * @return science target name
   */
  public static String processTarget(final StringBuilder sb, final ObservabilityData obsData, final Target target,
          final String baseLine, final String charaSetup, final String vegaSetup,
          final int calibratorIndex, final String scienceTargetName, final String scienceTargetId) {

    String result = null;

    final boolean isCalibrator = (calibratorIndex > 0);

    final String hdId = target.getIdentifier(HD_CATALOG);
    if (logger.isDebugEnabled()) {
      logger.debug("target: {} - HD: {}", target.getName(), hdId);
    }
    if (hdId == null) {
      logger.warn("Missing HD identifier for target [{}]", target);
    }

    // Get target HA ranges :
    final List<Range> rangesHA = getTargetHARange(obsData, target);
    if (logger.isDebugEnabled()) {
      logger.debug("rangesHA: {}", rangesHA);
    }

    // Skip unobservable target ?
    // Skip target without HD identifier ?

    if (hdId != null && rangesHA != null) {

      // Star information
      //   1.OBSV
      sb.append(OB_OBS).append(SEPARATOR);
      //   2.TARGET or REFERENCE
      sb.append((isCalibrator) ? CAL_TARGET : SCI_TARGET).append(SEPARATOR);
      // 3.Name of observation (string) = science target name (only alpha and digit characters) + (codes Cal1, Cal2, Chk) + Base Line
      if (isCalibrator) {
        sb.append(scienceTargetName).append(CAL_PREFIX).append(calibratorIndex);
      } else {
        result = StringUtils.removeNonAlphaNumericChars(target.getName());
        sb.append(result);
      }
      sb.append(baseLine).append(SEPARATOR);
      // 4.HD number (string)
      sb.append(hdId.replace(" ", "")).append(SEPARATOR);
      // 5.Spectral Type (string)
      final String spectralType = (target.getSPECTYP() != null && target.getSPECTYP().length() > 0) ? target.getSPECTYP() : SEPARATOR;
      sb.append(StringUtils.replaceWhiteSpacesByUnderscore(spectralType)).append(SEPARATOR);
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
      sb.append(vegaSetup);

      // Identifier PIVOT if defined:
      if (scienceTargetId != null) {
        sb.append(SEPARATOR).append(scienceTargetId);
      }

      // End of line :
      sb.append(END_OF_LINE);
    }
    return result;
  }

  /**
   * Add the spectral calibration OB to the current star list
   *
   * For example : CALI SPEC R1867 W070H4 1800 RB 867.5 OFF OPEN OPEN RB 1 4000
   * 
   * 1. CALI
   * 2. SPEC or FLAT
   * 3. Name of observation: réseau (1800=R1, 300=R2, 100=R3)et la longueur d’onde centrale
   * 4. Slit selection = W070H4
   * 5. Grating selection = 300
   * 6. Camera = RB
   * 7. Lambda = 656.0
   * 8. polar choice (Off, ) = OFF
   * 9. Red density = OPEN
   * 10.Blue density = OPEN
   * 11.Record configuration = RB
   * 12.Nb block = 1
   * 13.No Image = 4000
   *
   * @param sb string buffer
   * @param observation main observation
   */
  private static void processCalib(final StringBuilder sb, final ObservationSetting observation) {

    final FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();

    // 1. CALI
    // 2. SPEC
    sb.append(OB_CAL).append(SEPARATOR);
    
    // 3. Name of observation: réseau (1800=R1, 300=R2, 100=R3)et la longueur d’onde centrale = mode name
    sb.append(insMode.getName()).append(SEPARATOR);

    // 4. Slit selection = W070H4
    sb.append(getInstrumentModeParameter(insMode, PARAM_SLIT)).append(SEPARATOR);
    // 5. Grating selection = 300
    sb.append(getInstrumentModeParameter(insMode, PARAM_GRATING)).append(SEPARATOR);
    // 6. Camera = RB
    // 02/09/2013: R instead of RB for R2720 ONLY (HACK):
    String camera = getInstrumentModeParameter(insMode, PARAM_CAMERA);
    if ("R2720".equals(insMode.getName())) {
      camera = "R";
    }
    sb.append(camera).append(SEPARATOR);
    // 7. Lambda = 656.0
    sb.append(getInstrumentModeParameter(insMode, PARAM_LAMBDA)).append(SEPARATOR);
    // 8. polar choice (Off, ) = OFF
    sb.append(getInstrumentModeParameter(insMode, PARAM_SPIN)).append(SEPARATOR);
    // 9. Red density = OPEN
    // 29/10/2012: 2.0 instead of OPEN for R2720 ONLY (HACK):
    String redFilter = getInstrumentModeParameter(insMode, PARAM_RED_FILTER);
    if ("R2720".equals(insMode.getName())) {
      redFilter = "2.0";
    }
    sb.append(redFilter).append(SEPARATOR);
    // 10.Blue density = OPEN
    sb.append(getInstrumentModeParameter(insMode, PARAM_BLUE_FILTER)).append(SEPARATOR);
    // 11.Record configuration = RB
    sb.append(getInstrumentModeParameter(insMode, PARAM_RECORD)).append(SEPARATOR);
    // 12.Nb block = 1
    sb.append(DEFAULT_BLOCS_CALIB).append(SEPARATOR);
    // 13.No Image = 4000
    sb.append(DEFAULT_FRAMES_CALIB);

    // End of line :
    sb.append(END_OF_LINE);
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

      if (logger.isDebugEnabled()) {
        logger.debug("obsRangesHA: {}", obsRangesHA);
      }

      if (obsRangesHA != null) {
        // target is observable :
        return obsRangesHA;
      }
    }
    return null;
  }

  /**
   * Determine the Vega Setup:
   *   1. Tracking detector (Red/Blue) = R
   *   2. Slit selection = W070H4
   *   3. Grating selection = 300
   *   4. Camera = RB
   *   5. Lambda = 656.0
   *   6. polar choice (Off, ) = OFF
   *   7. Red density = OPEN
   *   8. Blue density = OPEN
   *   9. Record configuration = RB
   *   10.Nb block = 20
   *   11.No Image = 1000
   *
   * For example: "R W070H4 300 RB 656.2 OFF OPEN OPEN RB 20 1000"
   *
   * @param observation current main observation
   * @return string representing the Vega Setup
   */
  private static String prepareVegaSetup(final ObservationSetting observation) {

    final FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();

    // Compute the Chara Setup :
    final StringBuilder sb = new StringBuilder(48);

    // 1.Tracking detector (Red/Blue) = R
    sb.append(getInstrumentModeParameter(insMode, PARAM_TRACKING)).append(SEPARATOR);
    // 2.Slit selection = W070H4
    sb.append(getInstrumentModeParameter(insMode, PARAM_SLIT)).append(SEPARATOR);
    // 3.Grating selection = 300
    sb.append(getInstrumentModeParameter(insMode, PARAM_GRATING)).append(SEPARATOR);
    // 4.Camera = RB
    sb.append(getInstrumentModeParameter(insMode, PARAM_CAMERA)).append(SEPARATOR);
    // 5.Lambda = 656.0
    sb.append(getInstrumentModeParameter(insMode, PARAM_LAMBDA)).append(SEPARATOR);
    // 6.polar choice (Off, ) = OFF
    sb.append(getInstrumentModeParameter(insMode, PARAM_SPIN)).append(SEPARATOR);
    // 7.Red density = OPEN
    sb.append(getInstrumentModeParameter(insMode, PARAM_RED_FILTER)).append(SEPARATOR);
    // 8.Blue density = OPEN
    sb.append(getInstrumentModeParameter(insMode, PARAM_BLUE_FILTER)).append(SEPARATOR);
    // 9.Record configuration = RB
    sb.append(getInstrumentModeParameter(insMode, PARAM_RECORD)).append(SEPARATOR);
    // 10.Nb block = 20
    sb.append(getInstrumentModeParameter(insMode, PARAM_BLOCKS)).append(SEPARATOR);
    // 11.No Image = 1000
    sb.append(getInstrumentModeParameter(insMode, PARAM_FRAMES)).append(SEPARATOR);

    if (logger.isDebugEnabled()) {
      logger.debug("Chara Setup: {}", sb.toString());
    }

    return sb.toString();
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
   * @param pops array of PoPs
   * @return string representing the Chara Setup
   */
  private static String prepareCharaSetup(final ObservationSetting observation, final Pop[] pops) {

    // Get chosen stations :
    final List<Station> stations = observation.getInstrumentConfiguration().getStationList();

    // CHARA : predefined channel per station for a specific base line :
    List<Channel> relatedChannels = ConfigurationManager.getInstance().getInstrumentConfigurationChannels(
            observation.getInterferometerConfiguration().getName(),
            observation.getInstrumentConfiguration().getName(),
            observation.getInstrumentConfiguration().getStations());

    // if no channels defined, use any interferometer channels:
    if (relatedChannels == null || relatedChannels.isEmpty()) {
      relatedChannels = observation.getInterferometerConfiguration().getInterferometerConfiguration().getInterferometer().getChannels();

      logger.info("No predefined channels for baseline [{}]; use default interferometer channels: {}", stations, relatedChannels);
    }

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
        pop = pops[i].getName().toUpperCase();
        switchyard = relatedChannels.get(i).getName();
      } else {
        telescope = UNDEFINED;
        pop = UNDEFINED;
        switchyard = UNDEFINED;
      }
      sb.append(telescope).append(SEPARATOR).append(pop).append(SEPARATOR).append(switchyard).append(SEPARATOR);
    }

    if (logger.isDebugEnabled()) {
      logger.debug("Chara Setup: {}", sb.toString());
    }

    return sb.toString();
  }

  /**
   * Return magnitude value or -99 if the magnitude is null
   * @param mag magnitude or null
   * @return magnitude value or -99 if the magnitude is null
   */
  private static double getMagnitude(final Double mag) {
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

  /**
   * Return the parameter value of the given name in the given instrument mode
   * @param insMode instrument mode
   * @param name parameter name
   * @return parameter value
   * @throws IllegalStateException if the parameter value is undefined
   */
  private static String getInstrumentModeParameter(final FocalInstrumentMode insMode, final String name) throws IllegalStateException {
    final String value = insMode.getParameterValue(name);
    if (value == null) {
      throw new IllegalStateException("Missing value for the parameter '" + name + "' for the instrument mode ['" + insMode + "'] !");
    }
    return value;
  }
}
