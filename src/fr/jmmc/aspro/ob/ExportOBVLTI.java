/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ExportOBVLTI.java,v 1.3 2010-05-05 14:31:31 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2010/04/14 13:09:59  bourgesl
 * first minimal OB for MIDI
 *
 * Revision 1.1  2010/04/13 15:55:14  bourgesl
 * extracted common part for VLTI
 *
 * Revision 1.6  2010/04/13 14:06:04  bourgesl
 * fixed Name keyword without file name extension and 32 character maximum length
 *
 * Revision 1.5  2010/04/12 14:33:18  bourgesl
 * restored RA/DEC conversion to have proper P2PP format
 * Proper motion converted to arcsec/year
 *
 * Revision 1.4  2010/04/09 10:22:26  bourgesl
 * use RA/DEC in HMS/DMS instead of angle conversion (deg)
 *
 * Revision 1.3  2010/04/08 14:08:23  bourgesl
 * changed missing value for fluxes to -99
 *
 * Revision 1.2  2010/04/06 08:31:44  bourgesl
 * fixed classloader issue with JNLP
 *
 * Revision 1.1  2010/04/02 10:07:35  bourgesl
 * simple OB generation for AMBER
 *
 */
package fr.jmmc.aspro.ob;

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.observability.DateTimeInterval;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.service.ObservabilityService;
import java.io.File;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
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
  protected static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** double formatter for magnitudes */
  protected final static NumberFormat df3 = new DecimalFormat("0.000");
  /** double formatter for PM */
  protected final static NumberFormat df6 = new DecimalFormat("0.000000");
  /** minimum elevation for LST Time ranges = 30 deg */
  public final static double VLTI_MIN_ELEV = 30d;
  /** absolute_times_list template value */
  public final static String VAL_ABS_TIME_LIST = "{<DATE>T00:00:00 <DATE>T00:00:00 1}";
  /** date keyword for absolute_times_list template value */
  public final static String VAL_ABS_TIME_LIST_DATE = "<DATE>";

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
   * According to the instrument defined in the observation, it uses ExportOBAmber or ExportOBMidi.
   * @param file file to save
   * @param targetName target to process
   * @param haMin HA min in decimal hours
   * @param haMax HA max in decimal hours
   */
  public static void process(final File file, final String targetName, final double haMin, final double haMax) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("process " + targetName + " to " + file);
    }

    // get observation and target :
    final ObservationSetting observation = ObservationManager.getInstance().getObservation();
    final Target target = ObservationManager.getTarget(observation, targetName);

    // Dispatch to AMBER or MIDI classes :
    final String instrumentName = observation.getInstrumentConfiguration().getName();

    if (AsproConstants.INS_AMBER.equals(instrumentName)) {
      ExportOBAmber.generate(file, observation, target, haMin, haMax);
    } else if (AsproConstants.INS_MIDI.equals(instrumentName)) {
      ExportOBMidi.generate(file, observation, target, haMin, haMax);
    } else {
      throw new IllegalArgumentException("The application can not generate an Observing Block for this instrument [" + instrumentName + "] !");
    }
  }

  /**
   * Process the common part of the given template for the given target
   * @param template OB template
   * @param fileName OB file name
   * @param observation observation settings
   * @param target target to process
   * @param haMin HA min in decimal hours
   * @param haMax HA max in decimal hours
   * @return processed template
   */
  protected static String processCommon(final String template, final String fileName, final ObservationSetting observation, final Target target,
                                        final double haMin, final double haMax) {
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

    // --- Date / Time constraints ---

    document = processDateTime(document, observation, target, haMin, haMax);

    // --- Target information ---

    // comments = spectral type :
    document = document.replaceFirst(KEY_COMMENTS, target.getSPECTYP());

    // convert RA/DEC with mas up to 3 digits :
    final String[] raDec = AstroSkyCalc.toString(target.getRADeg(), target.getDECDeg());

    document = document.replaceFirst(KEY_RA, raDec[0]);
    document = document.replaceFirst(KEY_DEC, raDec[1]);

    // PMRA / PMDEC (optional) converted to arcsec/year :
    document = document.replaceFirst(KEY_PM_RA,
            df6.format((target.getPMRA() != null) ? target.getPMRA().doubleValue() / 1000d : 0d));
    document = document.replaceFirst(KEY_PM_DEC,
            df6.format((target.getPMDEC() != null) ? target.getPMDEC().doubleValue() / 1000d : 0d));

    // replace invalid characters (i.e. not alpha numeric) :
    final String altName = target.getName().replaceAll("[^a-zA-Z_0-9]", "_");
    document = document.replaceFirst(KEY_TARGET_NAME, altName);

    // Later : atmosphere / seeing

    // Base line :
    document = document.replaceFirst(KEY_BASE_LINE, getBaseLine(observation));

    return document;
  }

  private static String getBaseLine(final ObservationSetting observation) {

    final StringBuilder sb = new StringBuilder();

    for (Station s : observation.getInstrumentConfiguration().getStationList()) {
      sb.append(s.getName()).append("-");
    }
    sb.deleteCharAt(sb.length() - 1);

    return sb.toString();
  }

  /**
   * Process the Date / Time constraints for the given target
   * @param document OB document
   * @param observation observation settings
   * @param target target to process
   * @param haMin HA min in decimal hours
   * @param haMax HA max in decimal hours
   * @return processed template
   */
  private static String processDateTime(String document, final ObservationSetting observation, final Target target,
                                        final double haMin, final double haMax) {

    String lstTimeIntervals = "";
    String absTimeList = "";

    // Compute Observability data with min elevation = 30 deg (date and night restrictions depend on the current observation) :
    final ObservabilityService os = new ObservabilityService(observation, target, VLTI_MIN_ELEV);
    final ObservabilityData obsData = os.compute();

    final StarData starData = obsData.getStarData(target.getName());
    if (starData != null) {
      final List<Range> obsRangesHA = starData.getObsRangesHA();

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("obsRangesHA = " + obsRangesHA);
      }

      if (obsRangesHA != null) {
        // target is observable :

        final List<Range> limRangesHA = restrictRange(obsRangesHA, haMin, haMax);

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

  private static List<Range> restrictRange(final List<Range> ranges, final double min, final double max) {
    final List<Range> intervals = new ArrayList<Range>(ranges.size());

    double start, end;
    for (Range range : ranges) {
      start = range.getMin();
      end = range.getMax();

      if (start >= min) {
        if (end <= max) {
          // interval in inside [min;max]
          intervals.add(range);
        } else {
          if (start > max) {
            // two points over max : skip
          } else {
            // end occurs after max :
            intervals.add(new Range(start, max));
          }
        }
      } else {
        // start occurs before min :
        if (end < min) {
          // two points before min : skip
        } else if (end > max) {
          // two points overlapping [min;max] : keep
          intervals.add(new Range(min, max));
        } else {
          intervals.add(new Range(min, end));
        }
      }
    }
    return intervals;
  }

  /**
   * Convert LST date intervals to STTimeIntervals format i.e. 'date1_in_seconds:date2_in_seconds;...'
   * @param dateIntervals LST intervals
   * @return String value
   */
  private static String convertLstRanges(final List<DateTimeInterval> dateIntervals) {
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
  private static int convertDateToSeconds(final Calendar cal, final int sign) {

    final int h = cal.get(Calendar.HOUR_OF_DAY);
    final int m = cal.get(Calendar.MINUTE);

    return h * 3600 + (m + sign) * 60;
  }
}
