package edu.dartmouth;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
modify it for their own purposes, provided that credit is given to the author
in a prominent place.  For the present program that means that the green
title and author banner appearing on the main window must not be removed,
and may not be altered without premission of the author. */
public final class Site implements Cloneable {

  // Complicated class holding much data about sites, which at this point
  // are all `canned'.
  String name;          // name of site
  Latitude lat;         // geographic Latitude
  Longitude longit;     // Longitude
  double stdz;          // time zone offset from UT, decimal hrs
  int use_dst;          // code for DST use, 0 = none, 1 = US, etc.
  String timezone_name; // name of timezone e.g. "Mountain"
  String zone_abbrev;   // one-letter abbreviation of timezone
  double elevsea;       // elevation above sea level, meters
  double elevhoriz;     // elevation above the typical local horizon, m.

//   Site(HashMap<String, Site> sitelist, String sitename) {
//      Reload(sitelist, sitename);
//   }
//   void Reload(Site [] sitelist, string sitename) {
//        Site tempsite = sitelist.get(sitename);
//        name = tempsite.name;
//        longit = new Longitude(tempsite.longit.value);
//        lat = new Latitude(tempsite.lat.value);
//        stdz = tempsize.stdz;
//        use_dst = tempsite.use_dst;
//        timezone_name = tempsite.timezone_name;
//        zone_abbrev = tempsite.zone_abbrev;
//        elevsea = tempsite.elevsea;
//        elevhoriz = tempsite.elev_horiz;
//   }
  void Reload(final Site s) {
    name = s.name;
    longit = s.longit.clone();
    lat = s.lat.clone();
    stdz = s.stdz;
    use_dst = s.use_dst;
    timezone_name = s.timezone_name;
    zone_abbrev = s.zone_abbrev;
    elevsea = s.elevsea;
    elevhoriz = s.elevhoriz;
  }

  /**
   * Laurent Bourges [PATCH] :
   * Special constructor with numerical arguments and using GMT time zone (no DST)
   * @param name site name
   * @param lon Longitude given in decimal hours
   * @param lat Latitude given in degrees
   * @param alt altitude given in meters
   */
  Site(final String name, final double lon, final double lat, final double alt) {
    this.name = name;
    this.longit = new Longitude(lon);
    this.lat = new Latitude(lat);
    /** approximate the time zone offset : used by nightly almanach */
    this.stdz = Math.floor(lon);
    // no daylight saving time :
    this.use_dst = 0;
    this.timezone_name = "";
    this.zone_abbrev = "";
    this.elevsea = alt;
    this.elevhoriz = alt;
  }

  Site(final String[] sitepars) {
    // for(int j = 0; j < 9; j++) System.out.printf("%d %s\n",j,sitepars[j]);
    name = sitepars[0];
    // System.out.printf("%s %s",sitepars[0],sitepars[1]);
    // System.out.printf("-> %f\n",Double.parseDouble(sitepars[1].trim()));
    longit = new Longitude(Double.parseDouble(sitepars[1].trim()));
    lat = new Latitude(Double.parseDouble(sitepars[2].trim()));
    stdz = Double.parseDouble(sitepars[3].trim());
    use_dst = Integer.parseInt(sitepars[4].trim());
    timezone_name = sitepars[5];
    zone_abbrev = sitepars[6];
    elevsea = Double.parseDouble(sitepars[7].trim());
    elevhoriz = Double.parseDouble(sitepars[8].trim());
  }

  void dumpsite() {  // for diagnostics
    System.out.printf("%s\n", name);
    System.out.printf("longitude %s\n", longit.roundedLongitString(1, ":", true));
    System.out.printf("latitude  %s\n", lat.roundedDecString(0, ":"));
    System.out.printf("Zone offset from UT %6.3f hours\n", stdz);
  }

  @Override
  public boolean equals(Object arg) {
    if ((arg != null) && (arg instanceof Site)) {
      Site ss = (Site) arg;
      if (!(ss.name.equals(name))) {
        return false;
      }
      if (ss.lat.value != lat.value) {
        return false;
      }
      if (ss.longit.value != longit.value) {
        return false;
      }
      if (ss.stdz != stdz) {
        return false;
      }
      // close enough ...
    }
    return true;
  }

  public Site clone() {
    try {
      final Site copy = (Site) super.clone();
      copy.lat = lat.clone();
      copy.longit = longit.clone();
      return copy;
    } catch (CloneNotSupportedException e) {
      throw new Error("This should never happen!");
    }
  }
}
