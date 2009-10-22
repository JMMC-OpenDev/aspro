/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.dartmouth;

import fr.jmmc.aspro.model.oi.LonLatAlt;

/**
 * This class uses JSkyCalc to perform several astronomical computations
 * 
 * @author bourgesl
 */
public class AstroSkyCalc {

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
    System.out.println("Site Long : " + Math.toDegrees(position.getLongitude()));
    System.out.println("Site Lat  : " + Math.toDegrees(position.getLatitude()));

    // note : longitude is hours west in jSkyCalc :
    this.site = new Site(name,
                          - angle2hours(position.getLongitude()),
                          Math.toDegrees(position.getLatitude()),
                          position.getAltitude());

    System.out.println("Site dump :");

    this.site.dumpsite();
  }

  public void defineDate(final int year, final int month, final int day) {

    // yyyy mm dd hh:mm
    final String dateTime = "2009 09 25 00:00";

    // UTC time :
    this.time = new InstantInTime(dateTime, site.stdz, site.use_dst, true);

    when = new WhenWhere(time, site);

    System.out.println("WhenWhere dump :");
    when.dump();

    System.out.printf("sun  alt : %f2\n", when.altsun);
    System.out.printf("sun  az  : %f\n", when.azsun);

    System.out.printf("moon alt : %f\n", when.altmoon);
    System.out.printf("moon az  : %f\n", when.azmoon);

  }

  public double angle2hours(final double angle) {
    return Math.toDegrees(angle) / 15.0d;
  }

}
