/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.ob;

import edu.dartmouth.AstroSkyCalcObservation;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.gui.action.ExportOBVLTIAction;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.observability.DateTimeInterval;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.model.oi.TargetInformation;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.service.ObservabilityService;
import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.logging.Level;
import javax.xml.datatype.XMLGregorianCalendar;

/**
 * This class generates the common part of observing blocks for the VLTI
 * @author bourgesl
 */
public class ExportOBVLTI {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.ob.ExportOBVLTI";
  /** Class logger */
  protected static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** P2PP file prefix for science targets */
  public static final String OB_SCIENCE = "SCI";
  /** P2PP file prefix for calibrator targets */
  public static final String OB_CALIBRATOR = "CAL";
  /** double formatter for magnitudes */
  protected final static NumberFormat df2 = new DecimalFormat("0.##");
  /** double formatter for magnitudes */
  protected final static NumberFormat df3 = new DecimalFormat("0.000");
  /** double formatter for PM */
  protected final static NumberFormat df6 = new DecimalFormat("0.000000");
  /** absolute_times_list template value */
  public final static String VAL_ABS_TIME_LIST = "{<DATE>T00:00:00 <DATE>T00:00:00 1}";
  /** date keyword for absolute_times_list template value */
  public final static String VAL_ABS_TIME_LIST_DATE = "<DATE>";
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
   * @param target target to process
   *
   * @throws IllegalStateException if the template file is not found
   * @throws IllegalArgumentException if the instrument is not supported (only AMBER/MIDI/VEGA)
   * @throws IOException if an I/O exception occured while writing the observing block
   */
  public final static void process(final File file, final ObservationSetting observation, final Target target) throws IllegalStateException, IllegalArgumentException, IOException {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("process " + target.getName() + " to " + file);
    }

    if (observation != null && target != null) {
      // Dispatch to AMBER / MIDI / PIONIER classes :
      final String instrumentName = observation.getInstrumentConfiguration().getName();

      if (AsproConstants.INS_AMBER.equals(instrumentName)) {
        ExportOBAmber.generate(file, observation, target);
      } else if (AsproConstants.INS_MIDI.equals(instrumentName)) {
        ExportOBMidi.generate(file, observation, target);
      } else if (AsproConstants.INS_PIONIER.equals(instrumentName)) {
        ExportOBPionier.generate(file, observation, target);
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
   * @param target target to process
   * @return processed template
   */
  protected final static String processCommon(final String template, final String fileName,
                                              final ObservationSetting observation, final Target target) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("processCommon " + target.getName());
    }

    // get OB template :
    String document = template;

    // Set name :
    String name = fileName;
    // remove obx extension :
    int pos = name.lastIndexOf('.');
    if (pos != -1) {
      name = name.substring(0, pos);
    }
    // maximum length :
    if (name.length() > 32) {
      name = name.substring(0, 31);
    }

    document = document.replaceFirst(KEY_NAME, name);

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
          calibratorOBFileName = generateOBFileName(firstCalibrator);

          // remove obx extension :
          pos = calibratorOBFileName.lastIndexOf('.');
          if (pos != -1) {
            calibratorOBFileName = calibratorOBFileName.substring(0, pos);
          }
        }
      }
    }

    // Define the calibrator OB using the first calibrator :
    document = document.replaceFirst(KEY_CALIBRATOR_OB, calibratorOBFileName);

    // --- OB category ---
    document = document.replaceFirst(KEY_CATEGORY,
            (targetUserInfos != null && targetUserInfos.isCalibrator(target)) ? VAL_CATEGORY_CALIBRATOR : VAL_CATEGORY_SCIENCE);


    // --- Date / Time constraints ---

    document = processDateTime(document, observation, target);

    // --- Target information ---

    // comments = spectral type :
    document = document.replaceFirst(KEY_COMMENTS, target.getSPECTYP());

    // convert RA/DEC (mas) up to 3 digits :
    final String[] raDec = AstroSkyCalcObservation.toString(target.getRADeg(), target.getDECDeg());

    document = document.replaceFirst(KEY_RA, raDec[0]);
    document = document.replaceFirst(KEY_DEC, raDec[1]);

    // PMRA / PMDEC (optional) converted to arcsec/year :
    document = document.replaceFirst(KEY_PM_RA,
            df6.format((target.getPMRA() != null) ? target.getPMRA().doubleValue() / 1000d : 0d));
    document = document.replaceFirst(KEY_PM_DEC,
            df6.format((target.getPMDEC() != null) ? target.getPMDEC().doubleValue() / 1000d : 0d));

    // replace invalid characters (i.e. not alpha numeric) :
    final String altName = target.getName().replaceAll(AsproConstants.REGEXP_INVALID_TEXT_CHARS, "_");
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
  public final static String getBaseLine(final ObservationSetting observation) {

    final StringBuilder sb = new StringBuilder();

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
   * @param target target to process
   * @return processed template
   */
  private final static String processDateTime(String doc, final ObservationSetting observation, final Target target) {

    String document = doc;
    String lstTimeIntervals = "";
    String absTimeList = "";

    // Compute Observability data with min elevation = 30 deg (date and night restrictions depend on the current observation) :
    final ObservabilityService os = new ObservabilityService(observation, target, AsproConstants.OB_MIN_ELEVATION);
    final ObservabilityData obsData = os.compute();

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

        final List<DateTimeInterval> lstRanges = os.convertHARangesToDateInterval(limRangesHA, starData.getPrecRA());
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("lst ranges = " + lstRanges);
        }

        if (!lstRanges.isEmpty()) {
          // STTimeIntervals are expressed in seconds
          // sample "4980:17760;"

          lstTimeIntervals = convertLstRanges(lstRanges);

          // if night restriction is active, LST ranges are only valid for the observation date :
          if (observation.getWhen().isNightRestriction()) {
            // get observation date :
            final XMLGregorianCalendar cal = observation.getWhen().getDate();
            if (logger.isLoggable(Level.FINE)) {
              logger.fine("obs Date = " + cal);
            }

            // define Date constraint in eso date format is '2010-05-04' :

            // {<DATE>T00:00:00 <DATE>T00:00:00 1}
            absTimeList = VAL_ABS_TIME_LIST.replaceAll(VAL_ABS_TIME_LIST_DATE, cal.toString());
          }
        }
      }
    }

    // replace values :
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("lstTimeIntervals = " + lstTimeIntervals);
    }

    document = document.replaceAll(KEY_LST_INTERVALS, lstTimeIntervals);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("absTimeList = " + absTimeList);
    }

    document = document.replaceAll(KEY_ABS_TIME_LIST, absTimeList);

    return document;
  }

  /**
   * Convert LST date intervals to STTimeIntervals format i.e. 'date1_in_seconds:date2_in_seconds;...'
   * @param dateIntervals LST intervals
   * @return String value
   */
  private final static String convertLstRanges(final List<DateTimeInterval> dateIntervals) {
    final StringBuilder sb = new StringBuilder(32);

    final Calendar cal = new GregorianCalendar();

    for (DateTimeInterval interval : dateIntervals) {
      cal.setTime(interval.getStartDate());
      // upper minute :
      sb.append(convertDateToSeconds(cal, 1)).append(':');

      cal.setTime(interval.getEndDate());
      // lower minute :
      sb.append(convertDateToSeconds(cal, -1)).append(';');
    }

    return sb.toString();
  }

  /**
   * Convert a calendar time to seconds rounding to the lower or upper minute
   * according to the given sign
   * @param cal calendar time
   * @param sign +/- 1
   * @return seconds
   */
  private final static int convertDateToSeconds(final Calendar cal, final int sign) {

    final int h = cal.get(Calendar.HOUR_OF_DAY);
    final int m = cal.get(Calendar.MINUTE);

    return h * 3600 + (m + sign) * 60;
  }

  /**
   * Return magnitude value or -99 if the magnitude is null
   * @param mag magnitude or null
   * @return magnitude value or -99 if the magnitude is null
   */
  public final static double getMagnitude(final Double mag) {
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
      throw new IllegalStateException("the instrumentMode is empty !");
    }

    final TargetConfiguration targetConf = target.getConfiguration();

    final boolean useFT = (targetConf != null && targetConf.getFringeTrackerMode() != null);

    final String suffix = insMode.getName() + '_' + (useFT ? "FT" : "noFT");
    final String prefix = om.isCalibrator(target) ? OB_CALIBRATOR : OB_SCIENCE;

    return observation.generateFileName(target.getName(), prefix, suffix, ExportOBVLTIAction.OBX_EXT);
  }
}
