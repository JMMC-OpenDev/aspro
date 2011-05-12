/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AsproConstants.java,v 1.24 2011-02-25 16:51:18 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.23  2011/02/24 17:10:22  bourgesl
 * added multi conf label
 *
 * Revision 1.22  2011/01/07 13:20:23  bourgesl
 * added UNDEFINED_MAGNITUDE
 *
 * Revision 1.21  2010/12/15 13:37:03  bourgesl
 * added PIONIER instrument
 *
 * Revision 1.20  2010/10/22 13:31:37  bourgesl
 * added preference for Time LST/UTC
 *
 * Revision 1.19  2010/10/14 14:18:37  bourgesl
 * added max elevation set to 85 degrees
 *
 * Revision 1.18  2010/10/14 10:58:03  bourgesl
 * Fixed bug related to sampling periodicity : use the instrument default sampling time when an invalid value is detected
 *
 * Revision 1.17  2010/10/08 12:31:09  bourgesl
 * added suffix for calibrators
 *
 * Revision 1.16  2010/09/15 13:52:55  bourgesl
 * added JMMC copyright on plot
 *
 * Revision 1.15  2010/07/22 15:45:43  bourgesl
 * added acquisition time in UV coverage and observation
 *
 * Revision 1.14  2010/06/09 12:48:05  bourgesl
 * removed DEBUG_MODE constant (useless)
 *
 * Revision 1.13  2010/06/07 16:03:48  bourgesl
 * minimum elevation changed to 30 degrees
 *
 * Revision 1.12  2010/05/26 15:28:26  bourgesl
 * added VEGA instrument constant
 *
 * Revision 1.11  2010/05/21 14:27:18  bourgesl
 * added default values for Model Image Lut & Size
 *
 * Revision 1.10  2010/05/07 11:34:31  bourgesl
 * set debug mode to false
 *
 * Revision 1.9  2010/05/06 15:37:50  bourgesl
 * added HA_MIN/MAX
 *
 * Revision 1.8  2010/04/14 13:09:59  bourgesl
 * first minimal OB for MIDI
 *
 * Revision 1.7  2010/04/02 09:21:27  bourgesl
 * added AMBER and None constants
 *
 * Revision 1.6  2010/02/04 17:05:06  bourgesl
 * UV bounds are coming from UVCoverageService
 *
 * Revision 1.5  2010/01/20 16:18:38  bourgesl
 * observation form refactoring
 *
 * Revision 1.4  2010/01/15 13:49:32  bourgesl
 * comments
 *
 * Revision 1.3  2010/01/08 16:51:18  bourgesl
 * initial uv coverage
 *
 ******************************************************************************/
package fr.jmmc.aspro;

/**
 * This class gathers main constant values
 * @author bourgesl
 */
public interface AsproConstants {

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
  /** minimum elevation for OB generation = 30 degrees */
  public static final double OB_MIN_ELEVATION = 30d;
  /** default observation duration per calibrated point = 300 seconds i.e. 5 minutes */
  public static final double DEFAULT_OBSERVATION_DURATION = 300d;
  /** default value for checkbox Night Limit = true */
  public static final boolean DEFAULT_USE_NIGHT_LIMITS = true;
  /** default image LUT */
  public final static String DEFAULT_IMAGE_LUT = "aspro";
  /** default image size */
  public final static Integer DEFAULT_IMAGE_SIZE = Integer.valueOf(512);
  /** image size choices */
  public final static Integer[] IMAGE_SIZES = {Integer.valueOf(256), DEFAULT_IMAGE_SIZE, Integer.valueOf(1024)};
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
  public static final String[] TIME_CHOICES = new String[]{TIME_LST, TIME_UTC};

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
