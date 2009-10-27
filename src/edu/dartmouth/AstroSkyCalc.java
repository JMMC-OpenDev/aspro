/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.dartmouth;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.oi.LonLatAlt;
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
  /** site location */
  private Site site;
  /** date time info */
  private InstantInTime time;
  /** time / site info */
  private WhenWhere when;

  public AstroSkyCalc() {
    // nothing to do
  }

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
              "\nlongitude : " + this.site.longit.RoundedLongitString(1, ":", true) +
              "\nlatitude  : " + this.site.lat.RoundedDecString(0, ":"));
    }
  }

  public void defineDate(final int year, final int month, final int day) {
    // yyyy mm dd hh:mm
    final String dateTime = year + " " + month + " " + day + " 12:00";

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("dateTime : " + dateTime);
    }

    // UTC time :
    this.time = new InstantInTime(dateTime, site.stdz, site.use_dst, true);

    this.when = new WhenWhere(time, site);
    dumpWhen();

  }

  private void dumpWhen() {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("WhenWhere dump : " + time.jd +
              "\nUT : " + time.localDate.day +
              "/" + time.localDate.month +
              "/" + time.localDate.year +
              " " + time.localDate.timeofday.hour +
              ":" + time.localDate.timeofday.minute +
              ":" + time.localDate.timeofday.second +
              "\nlst : " + when.siderealobj.RoundedRAString(3, ":"));
    }
  }

  public void setJd(final double jd) {
    this.when.ChangeWhen(jd);

    dumpWhen();
  }

  /**
   * Find the jd time value corresponding to the lst = 0 for a given date
   * @return jd
   */
  public double findLst0() {

    // decimal hours :
    double error = when.sidereal;
    int n = 0;

    while (error > 1e-3 && n < 9) {

      if (error > 12d) {
        error = 24d - error;
      } else {
        error *= -1d;
      }

      setJd(time.jd + (error / 24.d));

      // next pass :
      error = when.sidereal;
      n++;
    }
    return time.jd;
  }

  public void defineTarget(final double ra, final double dec) {

    // TBC

    // RA (decimal hours), DEC (degrees)
    final Celest target = new Celest(deg2hours(ra), dec, AsproConstants.EPOCH_J2000);

    System.out.println("Target [RA/DEC/EPOCH] :" + target.checkstring());

    final Observation obs = new Observation(when, target);

    obs.ComputeSky();

    System.out.println("Observation dump :");

    System.out.printf("ha       : %f\n", obs.ha.degrees());
    System.out.printf("altitude : %f\n", obs.altitude);
    System.out.printf("azimuth  : %f\n", obs.azimuth);
    System.out.printf("parallac : %f\n", obs.parallactic);

  }

  public double rad2hours(final double angrad) {
    return deg2hours(Math.toDegrees(angrad));
  }

  public double deg2hours(final double angdeg) {
    return angdeg / 15.0d;
  }
}
