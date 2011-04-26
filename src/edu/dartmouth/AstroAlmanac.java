/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AstroAlmanac.java,v 1.2 2011-04-26 15:53:38 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2011/04/22 15:34:24  bourgesl
 * new JMMC classes to manage almanac data
 *
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
    return "sun times:\n" + getSunTimes()
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
