/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AstroAlmanacTime.java,v 1.1 2011-04-22 15:34:24 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/07/22 12:31:51  bourgesl
 * refactoring to be used also for Moon events
 *
 * Revision 1.2  2010/01/08 16:51:18  bourgesl
 * initial uv coverage
 *
 */
package edu.dartmouth;

/**
 * This class describe the julian date of a Sun/Moon event (twilight, set or rise)
 * @author bourgesl
 */
public final class AstroAlmanacTime implements Comparable<AstroAlmanacTime> {

  /** type of almanac event */
  public enum AlmanacType {

    /** Sun Astronomical Twilight (before rise) */
    SunTwl18Rise,
    /** Sun Nautical Twilight (before rise) */
    SunTwl12Rise,
    /** Sun Civil Twilight (before rise) */
    SunTwl06Rise,
    /** Sun Rise */
    SunRise,
    /** Sun Set */
    SunSet,
    /** Sun Civil Twilight (after set) */
    SunTwl06Set,
    /** Sun Nautical Twilight (after set) */
    SunTwl12Set,
    /** Sun Astronomical Twilight (after set) */
    SunTwl18Set,
    /** Moon Rise */
    MoonRise,
    /** Moon Set */
    MoonSet,
    /** Night center */
    Midnight
  }
  /** julian date */
  private final double jd;
  /** event type */
  private final AlmanacType type;

  /**
   * Protected constructor
   * @param jd julian date of the almanac event
   * @param type type of the almanac event
   */
  protected AstroAlmanacTime(final double jd, final AlmanacType type) {
    super();
    this.jd = jd;
    this.type = type;
  }

  /**
   * Return the julian date of the almanac event
   * @return julian date of the almanac event
   */
  public double getJd() {
    return jd;
  }

  /**
   * Return the type of the almanac event
   * @return type of the almanac event
   */
  public AlmanacType getType() {
    return type;
  }

  /**
   * Override equals() required by Set implementation
   * @param obj object to test
   * @return true if both type and jd are equals
   */
  @Override
  public boolean equals(final Object obj) {
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final AstroAlmanacTime other = (AstroAlmanacTime) obj;
    if (this.jd != other.jd) {
      return false;
    }
    if (this.type != other.type) {
      return false;
    }
    return true;
  }

  /**
   * Override hashCode() required by Set implementation
   * @return composed hash code based on jd and type fields
   */
  @Override
  public int hashCode() {
    int hash = 7;
    hash = 37 * hash + (int) (Double.doubleToLongBits(this.jd) ^ (Double.doubleToLongBits(this.jd) >>> 32));
    hash = 37 * hash + this.type.hashCode();
    return hash;
  }

  public int compareTo(final AstroAlmanacTime t) {
    return Double.compare(this.jd, t.jd);
  }

  /**
   * Return a string representation
   * @return 'type @ jd'
   */
  @Override
  public String toString() {
    return getType() + " @ " + getJd();
  }
}
