/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AlmanacTime.java,v 1.1 2010-07-22 12:31:51 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2010/01/08 16:51:18  bourgesl
 * initial uv coverage
 *
 */
package edu.dartmouth;

/**
 * This class describe the julian date of a Sun/Moon event (twilight, set or rise)
 * @author bourgesl
 */
public final class AlmanacTime implements Comparable<AlmanacTime> {

  /** type of almanac event */
  public enum AlmanacType {

    /** Sun Twilight (before rise) */
    SunTwlRise,
    /** Sun Rise */
    SunRise,
    /** Sun Twilight (after set) */
    SunTwlSet,
    /** Sun Set */
    SunSet,
    /** Moon Rise */
    MoonRise,
    /** Moon Set */
    MoonSet;
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
  protected AlmanacTime(final double jd, final AlmanacType type) {
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

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final AlmanacTime other = (AlmanacTime) obj;
    if (this.jd != other.jd) {
      return false;
    }
    if (this.type != other.type) {
      return false;
    }
    return true;
  }

  @Override
  public int hashCode() {
    int hash = 7;
    hash = 37 * hash + (int) (Double.doubleToLongBits(this.jd) ^ (Double.doubleToLongBits(this.jd) >>> 32));
    hash = 37 * hash + this.type.hashCode();
    return hash;
  }

  public int compareTo(final AlmanacTime t) {
    return Double.compare(this.jd, t.jd);
  }
}
