/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AstroAlmanac.java,v 1.1 2011-04-22 15:34:24 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package edu.dartmouth;

import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

/**
 * This class gathers both almanac times for sun and moon objects
 * @author bourgesl
 */
public final class AstroAlmanac {

  /** Midnight unique sorted JD time stamps */
  private final Set<AstroAlmanacTime> midnights = new TreeSet<AstroAlmanacTime>();
  /** Sun unique sorted JD time stamps */
  private final Set<AstroAlmanacTime> sunTimes = new TreeSet<AstroAlmanacTime>();
  /** Moon unique sorted JD time stamps */
  private final Set<AstroAlmanacTime> moonTimes = new TreeSet<AstroAlmanacTime>();

  /**
   * Protected constructor
   */
  protected AstroAlmanac() {
    super();
  }

  /**
   * Return the Midnight unique sorted JD time stamps
   * @return Midnight unique sorted JD time stamps
   */
  public Set<AstroAlmanacTime> getMidnights() {
    return midnights;
  }

  /**
   * Return the Sun unique sorted JD time stamps
   * @return Sun unique sorted JD time stamps
   */
  public Set<AstroAlmanacTime> getSunTimes() {
    return sunTimes;
  }

  /**
   * Return the Moon unique sorted JD time stamps
   * @return Moon unique sorted JD time stamps
   */
  public Set<AstroAlmanacTime> getMoonTimes() {
    return moonTimes;
  }

  /**
   * Return a string representation
   * @return string representation
   */
  @Override
  public String toString() {
    return "midnights:\n" + getMidnights()
            + "\nsun times:\n" + getSunTimes()
            + "\nmoon times:\n" + getMoonTimes();
  }

  /**
   * Translate the given source time collection into the given dest time collection
   * @param jdOffset julian day offset to add
   * @param source source time collection
   * @param dest dest time collection
   */
  public static void translate(final double jdOffset, final Set<AstroAlmanacTime> source, final Set<AstroAlmanacTime> dest) {
    AstroAlmanacTime time, newTime;
    for (Iterator<AstroAlmanacTime> it = source.iterator(); it.hasNext();) {
      time = it.next();

      newTime = new AstroAlmanacTime(time.getJd() + jdOffset, time.getType());
      dest.add(newTime);
    }
  }
}
