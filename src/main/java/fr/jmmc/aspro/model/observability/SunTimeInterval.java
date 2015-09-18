/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.observability;

import java.util.Date;

/**
 * This class contains a simple date/time interval corresponding to DAY / NIGHT / TWILIGHTS (ASTRO / NAUTIC / CIVIL).
 * @author bourgesl
 */
public final class SunTimeInterval extends DateTimeInterval {

  /**
   * Type of SunTimeInterval (DAY / NIGHT / TWILIGHTS (ASTRO / NAUTIC / CIVIL))
   */
  public enum SunType {

    /** DAY */
    Day(0),
    /** NIGHT */
    Night(-18),
    /** ASTRONOMICAL TWILIGHT */
    AstronomicalTwilight(-15),
    /** NAUTICAL TWILIGHT */
    NauticalTwilight(-9),
    /** CIVIL TWILIGHT */
    CivilTwilight(-3);
    /** sun elevation used to determine night */
    private final int sunElevation;

    /**
     * Constructor with sun elevation
     * @param sunElevation sun elevation
     */
    private SunType(final int sunElevation) {
      this.sunElevation = sunElevation;
    }

    /**
     * Return true if that type can be considered as night using the given criteria
     * @param limit sun type considered as night
     * @return true if that type can be considered as night
     */
    public boolean isNight(final SunType limit) {
      return getSunElevation() - limit.getSunElevation() <= 0;
    }

    /**
     * Return the mean sun elevation
     * @return sun elevation
     */
    public int getSunElevation() {
      return sunElevation;
    }
  }
  /** type of the interval : DAY / NIGHT / TWILIGHTS (ASTRO / NAUTIC / CIVIL) */
  private final SunType type;

  /**
   * Public constructor
   * @param startDate starting date/time
   * @param endDate ending date/time
   * @param type type of the interval
   */
  public SunTimeInterval(final Date startDate, final Date endDate, final SunType type) {
    super(startDate, endDate);
    this.type = type;
  }

  /**
   * Return the type of the interval
   * @return type of the interval
   */
  public SunType getType() {
    return type;
  }
}
