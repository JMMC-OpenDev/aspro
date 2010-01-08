/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SunAlmanachTime.java,v 1.2 2010-01-08 16:51:18 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package edu.dartmouth;

/**
 * This class describe the julian date of a Sun event (twilight, set or rise)
 * @author bourgesl
 */
public class SunAlmanachTime implements Comparable<SunAlmanachTime> {

  public enum SunAlmanachType {
    SunTwlRise,
    SunRise,
    SunTwlSet,
    SunSet;
  }
  /** julian date */
  private double jd;
  /** event type */
  private final SunAlmanachType type;

  protected SunAlmanachTime(final double jd, final SunAlmanachType type) {
    super();
    this.jd = jd;
    this.type = type;
  }

  protected void setJd(final double jd) {
    this.jd = jd;
  }

  public double getJd() {
    return jd;
  }

  public SunAlmanachType getType() {
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
    final SunAlmanachTime other = (SunAlmanachTime) obj;
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

  public int compareTo(SunAlmanachTime t) {
    return Double.compare(this.jd, t.jd);
  }
}
