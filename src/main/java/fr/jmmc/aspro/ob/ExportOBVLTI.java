/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.ob;

import edu.dartmouth.AstroSkyCalcObservation;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.observability.DateTimeInterval;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.oi.AdaptiveOptics;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.model.oi.TargetInformation;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.model.oi.Telescope;
import fr.jmmc.aspro.model.util.SpectralBandUtils;
import fr.jmmc.aspro.service.ObservabilityService;
import fr.jmmc.jmal.Band;
import fr.jmmc.jmcs.util.FileUtils;
import fr.jmmc.jmcs.data.MimeType;
import fr.jmmc.jmcs.util.StringUtils;
import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class generates the common part of observing blocks for the VLTI
 * @author bourgesl
 */
public class ExportOBVLTI {

  /** Class logger */
  protected static final Logger logger = LoggerFactory.getLogger(ExportOBVLTI.class.getName());
  /** P2PP file prefix for science targets */
  public static final String OB_SCIENCE = "SCI";
  /** P2PP file prefix for calibrator targets */
  public static final String OB_CALIBRATOR = "CAL";
  /** P2PP version used for LST interval conversion */
  protected final static int P2PP_VERSION = 3;
  /** double formatter for magnitudes */
  protected final static NumberFormat df1 = new DecimalFormat("0.0");
  /** double formatter for magnitudes */
  protected final static NumberFormat df2 = new DecimalFormat("0.##");
  /** double formatter for magnitudes */
  protected final static NumberFormat df3 = new DecimalFormat("0.000");
  /** double formatter for PM */
  protected final static NumberFormat df6 = new DecimalFormat("0.000000");
  /** ESO date/time formatter */
  private final static DateFormat timeFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
  /** absolute_times_list template value */
  public final static String VAL_ABS_TIME_LIST = "{<DATE_START> <DATE_END> 0}";
  /** start date keyword for absolute_times_list template value */
  public final static String VAL_ABS_TIME_LIST_DATE_START = "<DATE_START>";
  /** end date keyword for absolute_times_list template value */
  public final static String VAL_ABS_TIME_LIST_DATE_END = "<DATE_END>";
  /** category value for SCIENCE OB */
  public final static String VAL_CATEGORY_SCIENCE = "SCIENCE";
  /** category value for CALIBRATOR OB */
  public final static String VAL_CATEGORY_CALIBRATOR = "CALIB";

  /* keywords */
  /** keyword - name */
  public final static String KEY_NAME = "<NAME>";
  /** keyword - absolute_times_list */
  public final static String KEY_ABS_TIME_LIST = "<ABS-TIME-LIST>";
  /** keyword - STTimeIntervals */
  public final static String KEY_LST_INTERVALS = "<LST-INTERVALS>";
  /** keyword - comments */
  public final static String KEY_COMMENTS = "<COMMENTS>";
  /** keyword - ra */
  public final static String KEY_RA = "<RA>";
  /** keyword - dec */
  public final static String KEY_DEC = "<DEC>";
  /** keyword - propRA */
  public final static String KEY_PM_RA = "<PMRA>";
  /** keyword - propDEC */
  public final static String KEY_PM_DEC = "<PMDEC>";
  /** keyword - TARGET.NAME */
  public final static String KEY_TARGET_NAME = "<TARGET-NAME>";
  /** keyword - Baseline */
  public final static String KEY_BASE_LINE = "<BASE-LINE>";
  /** keyword - category (SCIENCE or CALIB) */
  public final static String KEY_CATEGORY = "<CATEGORY>";
  /** keyword - calibrator OB (InstrumentComments) */
  public final static String KEY_CALIBRATOR_OB = "<CALIBRATOR-OB>";
  /** keyword - OBSERVATION.DESCRIPTION.NAME */
  public final static String KEY_OB_NAME = "<OB-NAME>";

  /* keywords common to AMBER and MIDI */
  /** keyword - xxx.HMAG */
  public final static String KEY_HMAG = "<HMAG>";
  /** keyword - TEL.COU.MAG or COU.GS.MAG */
  public final static String KEY_COUDE_GS_MAG = "<COUDE-GS-MAG>";

  /**
   * Forbidden constructor
   */
  protected ExportOBVLTI() {
    // no-op
  }

  /**
   * Generate the OB file for the given target.
   * According to the instrument defined in the observation, it uses ExportOBAmber, ExportOBMidi or ExportOBPionier.
   * @param file file to save
   * @param observation observation to use
   * @param os observability service with computed data
   * @param target target to process
   *
   * @throws IllegalStateException if the template file is not found
   * @throws IllegalArgumentException if the instrument is not supported (only AMBER/MIDI/VEGA)
   * @throws IOException if an I/O exception occured while writing the observing block
   */
  public static void process(final File file, final ObservationSetting observation,
          final ObservabilityService os,
          final Target target) throws IllegalStateException, IllegalArgumentException, IOException {

    if (logger.isDebugEnabled()) {
      logger.debug("process target {} to file: ", target.getName(), file);
    }

    if (observation != null && target != null) {
      // Dispatch to AMBER / MIDI / PIONIER classes :
      final String instrumentName = observation.getInstrumentConfiguration().getName();

      if (AsproConstants.INS_AMBER.equals(instrumentName)) {
        ExportOBAmber.generate(file, observation, os, target);
      } else if (AsproConstants.INS_MIDI.equals(instrumentName)) {
        ExportOBMidi.generate(file, observation, os, target);
      } else if (instrumentName.startsWith(AsproConstants.INS_PIONIER)) {
          // TODO: handle PIONIER_2 (new OBs)
        ExportOBPionier.generate(file, observation, os, target);
      } else {
        throw new IllegalArgumentException("Aspro 2 can not generate an Observing Block for this instrument [" + instrumentName + "] !");
      }
    }
  }

  /**
   * Process the common part of the given template for the given target
   * @param template OB template
   * @param fileName OB file name
   * @param observation observation settings
   * @param os observability service with computed data
   * @param target target to process
   * @return processed template
   */
  protected static String processCommon(final String template, final String fileName,
          final ObservationSetting observation,
          final ObservabilityService os,
          final Target target) {
    if (logger.isDebugEnabled()) {
      logger.debug("processCommon: {}", target.getName());
    }

    // get OB template :
    String document = template;

    // Set name :
    // remove obx extension :
    String name = FileUtils.getFileNameWithoutExtension(fileName);

    // maximum length :
    if (name.length() > 32) {
      name = name.substring(0, 31);
    }

    document = document.replaceFirst(KEY_NAME, name); // 32 chars max

    // --- Calibrator OB ---
    final TargetUserInformations targetUserInfos = observation.getTargetUserInfos();

    String calibratorOBFileName = "";
    if (targetUserInfos != null && !targetUserInfos.isCalibrator(target)) {
      // use first calibrator in calibrator list :
      final TargetInformation targetInfo = targetUserInfos.getTargetInformation(target);
      if (targetInfo != null) {
        final List<Target> calibrators = targetInfo.getCalibrators();
        if (!calibrators.isEmpty()) {
          final Target firstCalibrator = calibrators.get(0);
          calibratorOBFileName = FileUtils.getFileNameWithoutExtension(generateOBFileName(firstCalibrator));
        }
      }
    }

    // Define the calibrator OB using the first calibrator :
    document = document.replaceFirst(KEY_CALIBRATOR_OB, calibratorOBFileName);

    // --- OB category ---
    document = document.replaceFirst(KEY_CATEGORY,
            (targetUserInfos != null && targetUserInfos.isCalibrator(target)) ? VAL_CATEGORY_CALIBRATOR : VAL_CATEGORY_SCIENCE);


    // --- Date / Time constraints ---

    document = processDateTime(document, observation, os, target);

    // --- Target information ---

    // comments = spectral type :
    document = document.replaceFirst(KEY_COMMENTS, (target.getSPECTYP() != null) ? target.getSPECTYP() : "");

    // convert RA/DEC (mas) up to 3 digits :
    final String[] raDec = AstroSkyCalcObservation.toString(target.getRADeg(), target.getDECDeg());

    document = document.replaceFirst(KEY_RA, raDec[0]);
    document = document.replaceFirst(KEY_DEC, raDec[1]);

    // --- OB NAME ---
    document = processObservationName(document, observation, raDec, target);

    // PMRA / PMDEC (optional) converted to arcsec/year :
    document = document.replaceFirst(KEY_PM_RA,
            df6.format((target.getPMRA() != null) ? target.getPMRA().doubleValue() / 1000d : 0d));
    document = document.replaceFirst(KEY_PM_DEC,
            df6.format((target.getPMDEC() != null) ? target.getPMDEC().doubleValue() / 1000d : 0d));

    // replace invalid characters (i.e. not alpha numeric) :
    final String altName = StringUtils.replaceNonAlphaNumericCharsByUnderscore(target.getName());
    document = document.replaceFirst(KEY_TARGET_NAME, altName);

    // Later : atmosphere / seeing

    // Base line :
    document = document.replaceFirst(KEY_BASE_LINE, getBaseLine(observation));

    return document;
  }

  /**
   * Return base line in eso p2pp format 'AA-BB-CC'
   * @param observation observation settings
   * @return base line
   */
  private static String getBaseLine(final ObservationSetting observation) {

    final StringBuilder sb = new StringBuilder(16);

    for (Station s : observation.getInstrumentConfiguration().getStationList()) {
      sb.append(s.getName()).append('-');
    }
    sb.deleteCharAt(sb.length() - 1);

    return sb.toString();
  }

  /**
   * Process the Date / Time constraints for the given target
   * @param doc OB document
   * @param observation observation settings
   * @param os observability service with computed data
   * @param target target to process
   * @return processed template
   */
  private static String processDateTime(String doc, final ObservationSetting observation,
          final ObservabilityService os, final Target target) {

    String document = doc;
    String lstTimeIntervals = "";
    String absTimeList = "";

    // Compute Observability data with min elevation = 30 deg (date and night restrictions depend on the current observation) :
    final ObservabilityData obsData = os.getData();

    final StarData starData = obsData.getStarData(target.getName());
    if (starData != null) {
      final List<Range> obsRangesHA = starData.getObsRangesHA();

      if (logger.isDebugEnabled()) {
        logger.debug("obsRangesHA: {}", obsRangesHA);
      }

      if (obsRangesHA != null) {
        // target is observable :
        final List<DateTimeInterval> lstRanges = os.convertHARangesToDateInterval(obsRangesHA, starData.getPrecRA());
        if (logger.isDebugEnabled()) {
          logger.debug("lst ranges: {}", lstRanges);
        }

        if (lstRanges != null) {
          // STTimeIntervals are expressed in seconds
          // "4980:17760;" (P2PP v2.x)
          // "{0 31320 0} {78120 86400 0} " (P2PP v3.x)

          lstTimeIntervals = convertLstRanges(lstRanges);

          // if night restriction is active, LST ranges are only valid for the observation date :
          if (observation.getWhen().isNightRestriction()) {
            // get observation date as GregorianCalendar :
            final GregorianCalendar cal = observation.getWhen().getDate().toGregorianCalendar();
            if (logger.isDebugEnabled()) {
              logger.debug("obs Date: {}", cal);
            }
            // define Date constraint in eso date format is '2010-05-04T00:00:00' :

            // {<DATE_START> <DATE_END> 0}
            absTimeList = VAL_ABS_TIME_LIST.replaceFirst(VAL_ABS_TIME_LIST_DATE_START, timeFormatter.format(cal.getTime()));

            cal.add(Calendar.DATE, 1);

            absTimeList = absTimeList.replaceFirst(VAL_ABS_TIME_LIST_DATE_END, timeFormatter.format(cal.getTime()));
          }
        }
      }
    }

    // replace values :
    if (logger.isDebugEnabled()) {
      logger.debug("lstTimeIntervals: {}", lstTimeIntervals);
    }

    document = document.replaceFirst(KEY_LST_INTERVALS, lstTimeIntervals);

    if (logger.isDebugEnabled()) {
      logger.debug("absTimeList: {}", absTimeList);
    }

    document = document.replaceFirst(KEY_ABS_TIME_LIST, absTimeList);

    return document;
  }

  /**
   * Process the Observation name :
   * [RA DEC Hmag Vmag SpecType TargetName] = [HHMM [+/-]DDMM <INS_BAND>=0.0 <AO_BAND>=0.0 <SPEC_TYPE> <NAME>]
   * @param document OB document
   * @param observation observation settings
   * @param raDec formatted RA/DEC
   * @param target target to use
   * @return processed template
   */
  private static String processObservationName(String document, final ObservationSetting observation,
          final String[] raDec, final Target target) {

    // get instrument band :
    final FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();
    if (insMode == null) {
      throw new IllegalStateException("The instrumentMode is empty !");
    }

    final double lambda = insMode.getWaveLength();

    if (logger.isDebugEnabled()) {
      logger.debug("lambda: {}", lambda);
    }

    final Band band = Band.findBand(lambda);
    final SpectralBand insBand = SpectralBandUtils.findBand(band);

    if (logger.isDebugEnabled()) {
      logger.debug("insBand: {}", insBand);
    }

    // get AO band :
    final List<Station> stations = observation.getInstrumentConfiguration().getStationList();

    // All telescopes in a configuration have the same AO system :
    final Telescope tel = stations.get(0).getTelescope();

    // AO :
    final AdaptiveOptics ao = tel.getAdaptiveOptics();

    final SpectralBand aoBand = (ao != null) ? ao.getBand() : null;
    if (logger.isDebugEnabled()) {
      logger.debug("aoBand: {}", aoBand);
    }

    // extract instrument and AO fluxes :
    final Double insMag = target.getFlux(insBand);
    final Double aoMag = target.getFlux(aoBand);

    // Generate Observation Name :
    final StringBuilder sb = new StringBuilder(32);
    // HHMM
    sb.append(raDec[0], 0, 2).append(raDec[0], 3, 5).append(' ');
    // [+/-]DDMM
    sb.append(raDec[1], 0, 3).append(raDec[1], 4, 6).append(' ');

    sb.append(insBand.name()).append('=');
    sb.append(df1.format(ExportOBVLTI.getMagnitude(insMag)));

    if (aoBand != null) {
      sb.append(' ');
      sb.append(aoBand.name()).append('=');
      sb.append(df1.format(ExportOBVLTI.getMagnitude(aoMag)));
    }

    sb.append(' ');
    sb.append(StringUtils.replaceNonAlphaNumericCharsByUnderscore(target.getName()));

    // Spectral type at the end as it can exceed maximum length: 
    if (target.getSPECTYP() != null) {
      sb.append(' ');
      sb.append(StringUtils.replaceWhiteSpacesByUnderscore(target.getSPECTYP()));
    }

    String OBName = sb.toString();

    // maximum length :
    if (OBName.length() > 32) {
      OBName = OBName.substring(0, 31);
    }

    // replace values :
    if (logger.isDebugEnabled()) {
      logger.debug("OBName: {}", OBName);
    }

    return document.replaceFirst(KEY_OB_NAME, OBName); // 32 chars max
  }

  /**
   * Convert LST date intervals to STTimeIntervals format (depending on P2PP_VERSION)
   * @param dateIntervals LST intervals
   * @return String value
   */
  private static String convertLstRanges(final List<DateTimeInterval> dateIntervals) {
    final StringBuilder sb = new StringBuilder(32);

    final Calendar cal = new GregorianCalendar();

    for (DateTimeInterval interval : dateIntervals) {
      convertLstRange(interval, sb, cal);
    }

    return sb.toString();
  }

  /**
   * Convert one LST date interval to STTimeIntervals format (depending on P2PP_VERSION)
   * @param interval LST interval to convert
   * @param sb string buffer to append to
   * @param cal temporary calendar instance
   */
  private static void convertLstRange(final DateTimeInterval interval, final StringBuilder sb, final Calendar cal) {

    if (P2PP_VERSION == 2) {
      // "4980:17760;" i.e. 'date1_in_seconds:date2_in_seconds;...' (P2PP v2.x)

      cal.setTime(interval.getStartDate());
      sb.append(convertDateToSeconds(cal, 1)).append(':'); // upper minute

      cal.setTime(interval.getEndDate());
      sb.append(convertDateToSeconds(cal, -1));

      sb.append(';'); // interval separator

    } else if (P2PP_VERSION == 3) {
      // "{0 31320 0} {78120 86400 0} " (P2PP v3.x)

      final Date start = interval.getStartDate();
      final Date end = interval.getEndDate();

      if (end.before(start)) {
        // single interval over midnight splitted into two intervals:

        sb.append("{0 "); // from midnight [00:00]

        cal.setTime(end);
        sb.append(convertDateToSeconds(cal, -1)); // lower minute

        sb.append(" 0} {");

        cal.setTime(start);
        sb.append(convertDateToSeconds(cal, 1)); // upper minute

        sb.append(" 86400 0}"); // until midnight [00:00]

      } else {
        sb.append('{');

        cal.setTime(start);
        sb.append(convertDateToSeconds(cal, 1)).append(' '); // upper minute

        cal.setTime(end);
        sb.append(convertDateToSeconds(cal, -1)); // lower minute

        sb.append(" 0}");
      }

      sb.append(' '); // interval separator

    } else {
      throw new IllegalStateException("Unsupported P2PP_VERSION: " + P2PP_VERSION);
    }
  }

  /**
   * Convert a calendar time to seconds rounding to the lower or upper minute
   * according to the given sign
   * @param cal calendar time
   * @param sign +/- 1
   * @return seconds
   */
  private static int convertDateToSeconds(final Calendar cal, final int sign) {

    final int h = cal.get(Calendar.HOUR_OF_DAY);
    final int m = cal.get(Calendar.MINUTE);

    return h * 3600 + (m + sign) * 60;
  }

  /**
   * Return magnitude value or -99 if the magnitude is null
   * @param mag magnitude or null
   * @return magnitude value or -99 if the magnitude is null
   */
  public static double getMagnitude(final Double mag) {
    if (mag != null) {
      return mag.doubleValue();
    }
    return AsproConstants.UNDEFINED_MAGNITUDE;
  }

  /**
   * Generate the Observing block file name using the given target
   * @param target target to use
   * @return Observing block file name
   */
  public static String generateOBFileName(final Target target) {

    final ObservationManager om = ObservationManager.getInstance();

    // use main observation :
    final ObservationSetting observation = om.getMainObservation();

    // get instrument band :
    final FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();
    if (insMode == null) {
      throw new IllegalStateException("The instrumentMode is empty !");
    }

    final TargetConfiguration targetConf = target.getConfiguration();

    final boolean useFT = (targetConf != null && targetConf.getFringeTrackerMode() != null);

    final String suffix = insMode.getName() + '_' + (useFT ? "FT" : "noFT");
    final String prefix = om.isCalibrator(target) ? OB_CALIBRATOR : OB_SCIENCE;

    return observation.generateFileName(target.getName(), prefix, suffix, MimeType.OBX.getExtension());
  }
}
