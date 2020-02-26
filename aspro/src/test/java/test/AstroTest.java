/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package test;

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.TimeRef;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.LonLatAlt;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/**
 *
 * @author bourgesl
 */
public class AstroTest {

    private static AstroSkyCalc prepareSite() {
        final ConfigurationManager cm = ConfigurationManager.getInstance();

        final InterferometerConfiguration intConf = cm.getInterferometerConfiguration("VLTI Period 105");

        final InterferometerDescription interferometer = intConf.getInterferometer();

        final LonLatAlt position = interferometer.getPosSph();

        /** sky calc instance */
        final AstroSkyCalc sc = new AstroSkyCalc();

        // define site :
        sc.defineSite(interferometer.getName(), position, "GMT");

        System.out.println("Site Long: " + Math.toDegrees(position.getLongitude()));
        System.out.println("Site Lat : " + Math.toDegrees(position.getLatitude()));
        System.out.println("Site Alt : " + position.getAltitude());
        /*
HIERARCH ESO ISS GEOELEV = 2669.0 / VLTI ground site height above WGS84 [m].
HIERARCH ESO ISS GEOLAT = -24.62743941 / VLTI site latitude (UV zero) [deg].
HIERARCH ESO ISS GEOLON = -70.40498688 / VLTI site longitude (UV zero) [deg].        
        
Site Long: -70.40498688
Site Lat : -24.62743941
Site Alt : 2669.0       
         */
        return sc;
    }

    public static void main(String[] args) {

        // Set the default locale to en-US locale (for Numerical Fields "." ",")
        Locale.setDefault(Locale.US);

        // Set the default timezone to GMT to handle properly the date in UTC :
        TimeZone.setDefault(TimeZone.getTimeZone("GMT"));

        final AstroSkyCalc sc = prepareSite();

        sc.defineDate(2019, 1, 1);

        /*
MJD-OBS   58627.07295558   Obs start
DATE-OBS   2019-05-24T01:45:12   Observing date
UTC   6301.000   [s] 01:45:00.000 UTC
LST   47333.932   [s] 13:08:53.932 LST
        
        
Date LST: Thu May 23 13:08:57 GMT 2019
Date UTC: Fri May 24 01:45:03 GMT 2019
        
https://ssd.jpl.nasa.gov/tc.cgi        
A.D. 2019-May-24 01:45:03.36        
         */
        final double mjd = 58627.07295558;
//        final double mjd = 58024.34324004;

        /*
MJD-OBS =   58024.34324004 / Obs start
DATE-OBS= '2017-09-28T08:14:26' / Observing date
UTC     =        29655.000 / [s] 08:14:15.000 UTC
LST     =        14509.852 / [s] 04:01:49.852 LST
        
Date LST: Tue Sep 26 04:01:51 GMT 2017
Date UTC: Thu Sep 28 08:14:16 GMT 2017        
         */
        double jd = mjd + AstroSkyCalc.MJD_REF;

        System.out.println("jd: " + jd);

        Date d = sc.toDate(jd, TimeRef.LST);
        System.out.println("Date LST: " + d);

        d = sc.toDate(jd, TimeRef.UTC);
        System.out.println("Date UTC: " + d);

        final double lst = sc.toLst(jd);
        System.out.println("LST hours: " + lst);

        // convert into JD (within night):
        jd = sc.convertLstToJD(lst);
        System.out.println("jd (today): " + jd);

        d = sc.toDate(jd, TimeRef.LST);
        System.out.println("Date LST: " + d);

        d = sc.toDate(jd, TimeRef.UTC);
        System.out.println("Date UTC: " + d);
    }
}
