/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AsproConstants.java,v 1.11 2010-05-21 14:27:18 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
 */
package fr.jmmc.aspro;

/**
 * This class gathers main constant values
 * @author bourgesl
 */
public interface AsproConstants {

  /* DEV */
  /** debug mode flag */
  public final static boolean DEBUG_MODE = false;
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
  /** default minimum elevation = 20 degrees */
  public static final double DEFAULT_MIN_ELEVATION = 20d;
  /** default sampling periodicity = 40 minutes */
  public static final double DEFAULT_SAMPLING_PERIOD = 40d;
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

  /* instrument names for specific features */
  /** VLTI AMBER */
  public static final String INS_AMBER = "AMBER";
  /** VLTI MIDI */
  public static final String INS_MIDI = "MIDI";
}
