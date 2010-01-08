/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AsproConstants.java,v 1.3 2010-01-08 16:51:18 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro;

/**
 * This class gathers main constant values
 * @author bourgesl
 */
public class AsproConstants {

  /** debug mode flag */
  public final static boolean DEBUG_MODE = true;

  /** chart : enables the zoom in / out */
  public final static boolean ENABLE_ZOOM = true;
  
  /** EPOCH J2000 */
  public final static float EPOCH_J2000 = 2000.f;
  /** fixed earth radius constant from ASPRO Fortran code */
  public final static double EARTH_RADIUS = 6367435d;

  /** default minimum elevation = 20 degrees */
  public static final double DEFAULT_MIN_ELEVATION = 20d;

}
