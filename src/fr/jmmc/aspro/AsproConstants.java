/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro;

import java.util.Arrays;
import java.util.Vector;

/**
 * This class gathers main constant values
 * @author bourgesl
 */
public interface AsproConstants {

  /** Warning Logger */
  /** jmmc status log */
  public final static String ASPRO_WARNING_LOG = "fr.jmmc.aspro.warning";
  /* JMMC applications */
  /** SearchCal samp.name keyword */
  public final static String SEARCHCAL_SAMP_NAME = "SearchCal";
  /** chart : enables the zoom in / out */
  public final static boolean ENABLE_ZOOM = true;

  /* ASTRO constants */
  /** EPOCH J2000 */
  public final static float EPOCH_J2000 = 2000.f;
  /** fixed earth radius constant from ASPRO Fortran code */
  public final static double EARTH_RADIUS = 6367435d;
  /** micrometres to meter */
  public final static double MICRO_METER = 1e-6;

  /* Hour angle ranges */
  /** minimum decimal hour angle = -12h */
  public final static double HA_MIN = -12D;
  /** maximum decimal hour angle = +12h */
  public final static double HA_MAX = 12D;

  /* UI Defaults */
  /** default minimum elevation = 45 degrees */
  public static final double DEFAULT_MIN_ELEVATION = 45d;
  /** default maximum elevation = 85 degrees */
  public static final double DEFAULT_MAX_ELEVATION = 85d;
  /** default observation duration per calibrated point = 300 seconds i.e. 5 minutes */
  public static final double DEFAULT_OBSERVATION_DURATION = 300d;
  /** default value for checkbox Night Limit = true */
  public static final boolean DEFAULT_USE_NIGHT_LIMITS = true;
  /** default moon separation check = 20 degrees */
  public static final double DEFAULT_MOON_SEPARATION_CHECK = 20d;
  /** default image LUT */
  public final static String DEFAULT_IMAGE_LUT = "aspro";
  /** default image size */
  public final static Integer DEFAULT_IMAGE_SIZE = Integer.valueOf(512);
  /** image size choices */
  public final static Vector<Integer> IMAGE_SIZES = new Vector<Integer>(Arrays.asList(
          new Integer[]{
            Integer.valueOf(256),
            Integer.valueOf(384),
            DEFAULT_IMAGE_SIZE,
            Integer.valueOf(768),
            Integer.valueOf(1024),
            Integer.valueOf(1536),
            Integer.valueOf(2048),}));
  /** no value for combo boxes */
  public static final String NONE = "None";

  /* time references */
  /** LST time reference */
  public static final String TIME_LST = "L.S.T.";
  /** UTC time reference */
  public static final String TIME_UTC = "U.T.C.";
  /** HA time reference */
  public static final String TIME_HA = "H.A.";
  /** list of choosable time references */
  public static final Vector<String> TIME_CHOICES = new Vector<String>(Arrays.asList(new String[]{TIME_LST, TIME_UTC}));

  /* instrument names for specific features */
  /** VLTI AMBER */
  public static final String INS_AMBER = "AMBER";
  /** VLTI MIDI */
  public static final String INS_MIDI = "MIDI";
  /** VLTI PIONIER */
  public static final String INS_PIONIER = "PIONIER";
  /** CHARA VEGA (2T/3T) */
  public static final String INS_VEGA = "VEGA_";
  /** JMMC legal notice on plots */
  public static final String JMMC_ANNOTATION = "Made by ASPRO 2/JMMC ";
  /** suffix for calibrator names */
  public static final String CAL_SUFFIX = " (cal)";
  /** default value for undefined magnitude = -99 */
  public final static double UNDEFINED_MAGNITUDE = -99d;
  /** label to display when multiple configurations are in use (file names, chart titles ...) */
  public static final String MULTI_CONF = "MULTI CONFIGURATION";
  /** regular expression used to match characters different than alpha/numeric/+/- */
  public static final String REGEXP_INVALID_TEXT_CHARS = "[^a-zA-Z_\\+\\-0-9]";
}
