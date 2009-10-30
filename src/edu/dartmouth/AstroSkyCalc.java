/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.dartmouth;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.oi.LonLatAlt;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeSet;
import java.util.logging.Level;

/**
 * This class uses JSkyCalc to perform several astronomical computations
 * 
 * @author bourgesl
 */
public class AstroSkyCalc {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.AstroSkyCalc";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  public enum SunAlmanachType {

    SunTwlRise,
    SunRise,
    SunTwlSet,
    SunSet;
  }
  /** site location */
  private Site site;
  /** date time info */
  private InstantInTime time;
  /** time / site info */
  private WhenWhere when;
  /** target info */
  private Observation observation;

  public AstroSkyCalc() {
    // nothing to do
  }

  /**
   * Define the observation site from the longitude, latitude and altitude coordinates (geographic)
   * @param name site name
   * @param position longitude (rad), latitude (rad) and altitude (m) coordinates
   */
  public void defineSite(final String name, final LonLatAlt position) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Site Long : " + Math.toDegrees(position.getLongitude()));
      logger.fine("Site Lat  : " + Math.toDegrees(position.getLatitude()));
    }

    // note : the given longitude is hours west in jSkyCalc :
    this.site = new Site(name,
            -rad2hours(position.getLongitude()),
            Math.toDegrees(position.getLatitude()),
            position.getAltitude());

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Site dump : " + this.site.name +
              "\ntz offset : " + this.site.stdz +
              "\nlongitude : " + this.site.longit.RoundedLongitString(1, ":", true) +
              "\nlatitude  : " + this.site.lat.RoundedDecString(0, ":"));
    }
  }

  /**
   * Define the observation date with a gregorian date (lenient) in UTC at 00:00
   * @param year year
   * @param month month from [1-12]
   * @param day day in [1-31]
   */
  public void defineDate(final int year, final int month, final int day) {
    // yyyy mm dd hh:mm
    final String dateTime = year + " " + month + " " + day + " 00:00";

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("dateTime : " + dateTime);
    }

    // UTC time :
    this.time = new InstantInTime(dateTime, this.site.stdz, this.site.use_dst, true);

    this.when = new WhenWhere(this.time, this.site);
    dumpWhen(this.when, "When");

// Moon fraction illumination fraction :
//    this.when.moonillum

  }

  /**
   * Find the jd time value corresponding to the lst = 0 for the current date
   *
   * Note : accurate +/-12 h within the given date
   *
   * @return jd corresponding to the lst = 0 for the current date
   */
  public double findLst0() {

    // decimal hours :
    double error = this.when.sidereal;
    int n = 0;

    while (error > 1e-3 && n < 9) {

      if (error > 12d) {
        error = 24d - error;
      } else {
        error *= -1d;
      }

      this.when.ChangeWhen(this.time.jd + (error / 24.d));

//    dumpWhen(this.when, "When");

      // next pass :
      error = this.when.sidereal;
      n++;
    }

    dumpWhen(this.when, "LST 0");

    return this.time.jd;
  }

  private void dumpWhen(final WhenWhere ww, final String label) {
    if (logger.isLoggable(Level.FINE)) {
      final InstantInTime t = ww.when;
      logger.fine(label + " dump : " + t.jd +
              "\nUT : " + t.UTDate.day +
              "/" + t.UTDate.month +
              "/" + t.UTDate.year +
              " " + t.UTDate.timeofday.hour +
              ":" + t.UTDate.timeofday.minute +
              ":" + t.UTDate.timeofday.second +
              "\nlst : " + ww.siderealobj.RoundedRAString(3, ":"));
    }
  }

  public void findSunRiseSet() {
    final TreeSet<SunAlmanachTime> ts = new TreeSet<SunAlmanachTime>();

    double jd0 = this.time.jd;
    double jd1 = this.time.jd + 1d;
    addAlmanach(ts, jd0 - 1d);
    addAlmanach(ts, jd0);
    addAlmanach(ts, jd1);

    final List<SunAlmanachTime> sorted = new ArrayList(ts);

    // find indexes inside the lst range [jd0;jd1] :
    int i0 = -1;
    int i1 = -1;
    SunAlmanachTime st;
    WhenWhere ww;
    for (int i = 0, len = sorted.size(); i < len; i++) {
      st = sorted.get(i);
      ww = st.ww;

//      dumpWhen(ww, st.type.name());

      if (ww.when.jd >= jd0) {
        if (i0 == -1) {
          // include previous event :
          i0 = i - 1;
        }
      }

      if (ww.when.jd > jd1) {
        // include next event :
        i1 = i;
        break;
      }
    }

    logger.fine("filtered sun events :");

    final List<SunAlmanachTime> result = new ArrayList<SunAlmanachTime>();

    for (int i = i0; i <= i1; i++) {
      st = sorted.get(i);
      ww = st.ww;

      dumpWhen(ww, st.type.name());

      result.add(st);
    }

  }

  private void addAlmanach(final TreeSet<SunAlmanachTime> ts, final double jd) {
    final NightlyAlmanac na = new NightlyAlmanac(new WhenWhere(jd, this.site));

    ts.add(new SunAlmanachTime(na.morningTwilight, SunAlmanachType.SunTwlRise));
    ts.add(new SunAlmanachTime(na.sunrise, SunAlmanachType.SunRise));
    ts.add(new SunAlmanachTime(na.sunset, SunAlmanachType.SunSet));
    ts.add(new SunAlmanachTime(na.eveningTwilight, SunAlmanachType.SunTwlSet));
  }

  public void defineTarget(final double ra, final double dec) {

    // RA (decimal hours), DEC (degrees)
    final Celest target = new Celest(deg2hours(ra), dec, AsproConstants.EPOCH_J2000);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Target [RA/DEC/EPOCH] :" + target.checkstring());
    }

    this.observation = new Observation(this.when, target);
  }

  public LonLatAlt getTargetPosition(final double jd) {

    this.observation.w.ChangeWhen(jd);
    this.observation.ComputeSky();

    if (logger.isLoggable(Level.FINE)) {
      dumpWhen(this.observation.w, "Target");
      logger.fine("ha|alt : " + this.observation.ha.degrees() + " " + this.observation.altitude);
//      logger.fine("azimuth  : " + this.observation.azimuth);
//      logger.fine("parallac : " + this.observation.parallactic);
    }

    return new LonLatAlt(this.observation.ha.degrees(), this.observation.altitude, 0d);
  }

  public double rad2hours(final double angrad) {
    return deg2hours(Math.toDegrees(angrad));
  }

  public double deg2hours(final double angdeg) {
    return angdeg / 15.0d;
  }

  private static class SunAlmanachTime implements Comparable<SunAlmanachTime> {

    /** julian date */
    private final double jd;
    /** internal when date */
    private final WhenWhere ww;
    /** event type */
    private final SunAlmanachType type;

    protected SunAlmanachTime(final WhenWhere when, final SunAlmanachType type) {
      this.ww = when;
      this.jd = when.when.jd;
      this.type = type;
    }

    protected double getJd() {
      return jd;
    }

    protected SunAlmanachType getType() {
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
}
