/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package edu.dartmouth;

import fr.jmmc.aspro.model.TimeRef;
import edu.dartmouth.AstroAlmanacTime.AlmanacType;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.oitools.model.range.Range;
import fr.jmmc.oitools.model.range.RangeFactory;
import fr.jmmc.aspro.model.oi.LonLatAlt;
import fr.jmmc.aspro.util.AngleUtils;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;
import net.jafama.FastMath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class uses JSkyCalc to perform several astronomical computations (sun ephemerids, LST/UTC time, JD time ...)
 *
 * @author bourgesl
 */
public final class AstroSkyCalc {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(AstroSkyCalc.class.getName());
    /** time ratio : sideral rate = LST / SOLAR ratio */
    public final static double SIDERAL_RATIO = Const.SID_RATE;
    /** time ratio : jd to LST hours */
    public final static double JD_TO_LST = 24d * SIDERAL_RATIO;
    /** time ratio : LST to jd hours */
    public final static double LST_TO_JD = 1d / JD_TO_LST;
    /** one Day in LST expressed in JD day */
    public final static double LST_DAY_IN_JD = 1d / SIDERAL_RATIO;
    /** half a day in LST expressed in JD day */
    public final static double HALF_LST_DAY_IN_JD = LST_DAY_IN_JD / 2d;
    /** Modified Juliean day reference */
    public final static double MJD_REF = 2400000.5d;
    /** 1 second in decimal hour */
    private final static double SEC_IN_DEC_HOUR = 1d / 3600d;
    /** millisecond in decimal hour */
    private final static double MSEC_IN_DEC_HOUR = SEC_IN_DEC_HOUR / 1000d;
    /** minimal precision value in decimal hour */
    private final static double PREC_IN_DEC_HOUR = MSEC_IN_DEC_HOUR / 10d;
    /** cached log trace disabled */
    private final static boolean DO_TRACE = false;

    /* members */
    /** cached log debug enabled */
    private final boolean isLogDebug = logger.isDebugEnabled();
    /** site location */
    Site site;
    /** site timezone (handling the Daylight Saving Time) */
    private TimeZone siteTZ = null;
    /** jd corresponding to LT=24:00:00 for the observation date */
    private double jdMidnight;
    /** jd corresponding to LST=00:00:00 for the observation date */
    private double jdLst0;
    /** date correspondign to LST=00:00:00 */
    private Calendar dateLst0;
    /** temporary WhenWhere instance used by calendar conversion */
    private WhenWhere wwCal = null;
    /** temporary Calendar instance used by calendar conversion */
    private final Calendar tmpCal = new GregorianCalendar();

    /**
     * Public Constructor
     */
    public AstroSkyCalc() {
        // nothing to do
    }

    /**
     * Return a new site from the longitude, latitude and altitude coordinates (geographic)
     * @param name site name
     * @param position longitude (rad), latitude (rad) and altitude (m) coordinates
     * @return new site
     */
    public static Site createSite(final String name, final LonLatAlt position) {
        if (logger.isDebugEnabled()) {
            logger.debug("Site Long : {}", FastMath.toDegrees(position.getLongitude()));
            logger.debug("Site Lat  : {}", FastMath.toDegrees(position.getLatitude()));
        }

        // note : the given longitude is hours west in jSkyCalc :
        // stdz = longitude (dec hours) is also the GMT offset (UTC)
        final Site site = new Site(name,
                -AngleUtils.rad2hours(position.getLongitude()),
                FastMath.toDegrees(position.getLatitude()),
                position.getAltitude());

        if (logger.isDebugEnabled()) {
            logger.debug("Site dump: {}\ntz offset: {}\nlongitude: {}\nlatitude  : {}",
                    site.name, site.stdz,
                    site.longit.roundedLongitString(1, ":", true),
                    site.lat.roundedDecString(0, ":"));
        }
        return site;
    }

    /**
     * Define the observation site from the longitude, latitude and altitude coordinates (geographic)
     * @param name site name
     * @param position longitude (rad), latitude (rad) and altitude (m) coordinates
     * @param timezone timezone of the observatory (used to convert to Local Time)
     */
    public void defineSite(final String name, final LonLatAlt position, final String timezone) {
        this.site = createSite(name, position);
        // timezone (handling the Daylight Saving Time):
        final String timezoneID = (timezone != null) ? timezone : "GMT"; // GMT if unknown
        this.siteTZ = TimeZone.getTimeZone(timezoneID);
        if (logger.isDebugEnabled()) {
            logger.debug("site timezone: {} => {}", timezoneID, getTimezoneID());
        }
    }

    /**
     * Return the time zone offset from UT, decimal hrs
     * @return time zone offset from UT, decimal hrs
     */
    public double getSiteStdZone() {
        return this.site.stdz;
    }

    /**
     * Return a long standard time name of this {@code TimeZone} suitable for
     * presentation to the user in the default locale.
     * @return the human-readable name of this time zone in the default locale (US).
     */
    public String getTimezoneID() {
        return this.siteTZ.getID(); // DisplayName();
    }

    /**
     * Define the observation date with a gregorian date (lenient)
     * and return the jd time value corresponding to LST=00:00:00 for the current date
     * @param year year
     * @param month month from [1-12]
     * @param day day in [1-31]
     * @return jd corresponding to LST=00:00:00 for the current date night
     */
    public double defineDate(final int year, final int month, final int day) {
        // find midnight first:
        final String dateTime = year + " " + month + " " + day + " 24:00";

        logger.debug("dateTime : {}", dateTime);

        // Define date as 24:00 (local time) :
        final WhenWhere ww = new WhenWhere(new InstantInTime(dateTime, this.site.stdz, this.site.use_dst, false), this.site);

        if (isLogDebug) {
            AstroSkyCalc.dumpWhen(ww, "LocalTime at 24:00");
        }

        // Define the julian date corresponding to LT=24:00:00 for the given date :
        // i.e. midnight (local time):
        this.jdMidnight = ww.when.jd;

        if (isLogDebug) {
            logger.debug("jdMidnight : {}", this.jdMidnight);
        }

        // Find the julian date corresponding to the LST origin LST=00:00:00 before the given midnight:
        // note: lst0 is in range [-24 (lst); -0] relative to midnight to have LST[0-24]:
        // TODO : explain why:
        // case 1: JD in [LST0; LST24] frame => LST[-12;36] (target HA in [-12; +12])
        // case 2: JD in [Midnight - 12 (midday 1); Midnight + 12 (midday2)] frame ...
        final InstantInTime lst0 = findLst0(this.jdMidnight - HALF_LST_DAY_IN_JD);

        this.jdLst0 = lst0.jd;

        if (isLogDebug) {
            logger.debug("jdLst0 = {}", this.jdLst0);
        }

        // define the date at LST origin to have it when converting LST time in java dates (date + time):
        this.dateLst0 = new GregorianCalendar(lst0.UTDate.year, lst0.UTDate.month - 1, lst0.UTDate.day);

        if (isLogDebug) {
            logger.debug("dateLst0 = ", this.dateLst0.getTime());
        }

        return this.jdLst0;
    }

    /**
     * Return the jd time value corresponding to the current system date/time
     * @return jd time value
     */
    public double getCurrentJd() {
        if (isLogDebug) {
            logger.debug("Current date/time : {}", new Date());
        }

        // Get current  (local time) :
        final WhenWhere ww = new WhenWhere(new InstantInTime(this.site.stdz, this.site.use_dst), this.site);

        if (isLogDebug) {
            AstroSkyCalc.dumpWhen(ww, "CurrentTime");
        }

        return ww.when.jd;
    }

    /**
     * Return the jd time value corresponding to LT=24:00:00 for the observation date
     *
     * @return jd corresponding to LT=24:00:00 for the observation date
     */
    public double getJdMidnight() {
        return this.jdMidnight;
    }

    /**
     * Return the jd time value corresponding to LST=00:00:00 for the current date
     * @see #findJdForLst0(double)
     *
     * @return jd corresponding to LST=00:00:00 for the current date
     */
    public double getJdForLst0() {
        return this.jdLst0;
    }

    /**
     * Find the jd date corresponding to LST=00:00:00 arround the given jd date
     *
     * Note : accurate +/-12 h within the given jd date
     *
     * @param jd julian date
     *
     * @return jd corresponding to LST=00:00:00 for the given jd date
     */
    public double findJdForLst0(final double jd) {
        return findLst0(jd).jd;
    }

    /**
     * Find when LST=00:00:00 arround the given jd date
     *
     * Note : accurate +/-12 h within the given date
     *
     * @param jd julian date
     *
     * @return instant corresponding to LST=00:00:00 for the given jd date
     */
    private InstantInTime findLst0(final double jd) {

        final WhenWhere ww = new WhenWhere(jd, this.site, false);

        double error;
        double sign;

        // decimal hours :
        double lst;

        for (int n = 0;; n++) {
            lst = ww.sidereal;

            if (lst > 12d) {
                error = 24d - lst;
                sign = 1d;
            } else {
                error = lst;
                sign = -1d;
            }

            // test if error is less than 1ms, then exit loop :
            // absolute lst value must be positive i.e. inside [0;1s] :
            if (error < MSEC_IN_DEC_HOUR && lst < SEC_IN_DEC_HOUR || n > 10) {
                break;
            }

            // avoid too small step (double precision limit) :
            if (error < PREC_IN_DEC_HOUR) {
                error = PREC_IN_DEC_HOUR;
            }

            // adjust jd :
            ww.changeWhen(ww.when.jd + sign * error * Const.HOUR_IN_DAY);

//      dumpWhen(ww, "When");
        }

        if (isLogDebug) {
            AstroSkyCalc.dumpWhen(ww, "LST=00:00:00");
        }

        return ww.when;
    }

    /**
     * Convert a julian date to a gregorian date (precise up to the second) in LST
     * @param jd julian date
     * @return Date object
     */
    public Date toDateLST(final double jd) {
        return toDate(jd, TimeRef.LST);
    }

    /**
     * Convert a julian date to a gregorian date (precise up to the second) in LST or UTC
     * @param jd julian date
     * @param timeRef time reference (LST, UTC or Local)
     * @return Date object
     */
// TODO: remove synchronization when safe !    
    public synchronized Date toDate(final double jd, final TimeRef timeRef) {
        // use temporary GregorianCalendar instance:
        final Calendar cal = toCalendar(jd, (timeRef == TimeRef.LST), tmpCal);
        final Date date = cal.getTime();

        if (timeRef == TimeRef.LOCAL) {
            // Convert UTC to Local time and modify date:
            convertUTCToLocal(date, siteTZ);
        }

        return date;
    }

    /**
     * Return the time offset in milliseconds between the given UT date and the local date (DST)
     * @param utDate UT date
     * @return time offset in milliseconds between the given UT date and the local date
     */
    public long getDateOffset(final Date utDate) {
        final long milliseconds_initial = utDate.getTime();

        convertUTCToLocal(utDate, siteTZ);

        return milliseconds_initial - utDate.getTime();
    }

    /**
     * Convert a julian date to a gregorian date (precise up to the second) in LST or UTC
     * and adjust the date field to be within range [dateMin; dateMax]
     * @param jd julian date
     * @param timeRef time reference (LST, UTC or Local)
     * @param dateMin lower date/time
     * @param dateMax upper date/time
     * @return Date object within range [dateMin; dateMax]
     */
// TODO: remove synchronization when safe !    
    public synchronized Date toDate(final double jd, final TimeRef timeRef, final Date dateMin, final Date dateMax) {
        // convert JD to LST/UT date/time:
        final Calendar cal = toCalendar(jd, (timeRef == TimeRef.LST));

        // roll +/- 1 day to be within plot range:
        final Date date = convertCalendarToDate(cal, timeRef, dateMin, dateMax);

        if (timeRef == TimeRef.LOCAL) {
            // Convert UTC to Local time and modify date:
            convertUTCToLocal(date, siteTZ);
        }

        return date;
    }

    /**
     * Convert a julian date to a gregorian calendar (precise up to the second) in LST or UTC
     * @param jd julian date
     * @param useLst flag to use LST (true) or UTC reference (false)
     * @return Calendar object
     */
    public Calendar toCalendar(final double jd, final boolean useLst) {
        return toCalendar(jd, useLst, null);
    }

    /**
     * Convert the given date as 'MM/DD'
     * @param sb buffer to append into
     * @param date date to convert
     */
    public void toDateString(final StringBuilder sb, final Date date) {
        // use temporary GregorianCalendar instance:
        tmpCal.setTime(date);

        // write date as 'MM/DD':
        final int month = tmpCal.get(Calendar.MONTH) + 1;
        if (month < 10) {
            sb.append('0');
        }
        sb.append(month);
        sb.append('/');

        final int day = tmpCal.get(Calendar.DAY_OF_MONTH);
        if (day < 10) {
            sb.append('0');
        }
        sb.append(day);
    }

    /**
     * Convert a julian date to the LST decimal hours (precise up to the second)
     * @param jd julian date
     * @return LST decimal hours
     */
    public double toLst(final double jd) {
        // avoid new instances:
        if (wwCal == null) {
            wwCal = new WhenWhere(jd, this.site, false);
        } else {
            wwCal.changeWhen(jd);
        }
        return wwCal.siderealobj.value;
    }

    /**
     * Convert a julian date to a gregorian calendar (precise up to the second) in LST or UTC
     * @param jd julian date
     * @param useLst flag to select LST (true) or UTC reference (false)
     * @param cal calendar instance to use
     * @return Calendar object
     */
// TODO: remove synchronization when safe !    
    private synchronized Calendar toCalendar(final double jd, final boolean useLst, final Calendar cal) {
        // avoid new instances:
        if (wwCal == null) {
            wwCal = new WhenWhere(jd, this.site, false);
        } else {
            wwCal.changeWhen(jd);
        }

        /* 
         * Notes :
         * - month is in range [0;11] in java Calendar
         * - milliseconds are set to 0 to be able to compare date instances
         */
        // The default TimeZone is already set to UTC (GMT):
        final Calendar calendar;
        if (useLst) {
            final RA sidereal = wwCal.siderealobj;

            // use the observation date as the LST has only time:
            if (cal == null) {
                calendar = new GregorianCalendar(this.dateLst0.get(Calendar.YEAR), this.dateLst0.get(Calendar.MONTH), this.dateLst0.get(Calendar.DAY_OF_MONTH),
                        sidereal.sex.hour, sidereal.sex.minute, (int) Math.round(sidereal.sex.second));
            } else {
                // use given calendar instance:
                cal.clear();
                cal.set(this.dateLst0.get(Calendar.YEAR), this.dateLst0.get(Calendar.MONTH), this.dateLst0.get(Calendar.DAY_OF_MONTH),
                        sidereal.sex.hour, sidereal.sex.minute, (int) Math.round(sidereal.sex.second));

                calendar = cal;
            }

            double days = (jd - this.jdLst0) * SIDERAL_RATIO;

            // tricky code here to ensure correct rounding error handling (inclusive):
            int ndays = 0;
            if (days > 0d) {
                // Add 1ms (precision):
                days += MSEC_IN_DEC_HOUR;
                if (days > 1d) {
                    ndays = (int) Math.floor(days);
                    if (calendar.get(Calendar.DAY_OF_MONTH) != this.dateLst0.get(Calendar.DAY_OF_MONTH)) {
                        // calendar has already one day more (rounding issue occuring when sidereal time = 23.9999...):
                        ndays--;
                    }
                }
            } else if (days < 0d) {
                ndays = -1;
                // Add 1ms (precision):
                days += MSEC_IN_DEC_HOUR;
                if (days < -1d) {
                    ndays += (int) Math.ceil(days);
                }
            }
            if (ndays != 0) {
                calendar.add(Calendar.DAY_OF_YEAR, ndays);
            }

        } else {
            final GenericCalDat utDate = wwCal.when.UTDate;

            if (cal == null) {
                calendar = new GregorianCalendar(utDate.year, utDate.month - 1, utDate.day,
                        utDate.timeofday.hour, utDate.timeofday.minute, (int) Math.round(utDate.timeofday.second));
            } else {
                // use given calendar instance:
                cal.clear();
                cal.set(utDate.year, utDate.month - 1, utDate.day,
                        utDate.timeofday.hour, utDate.timeofday.minute, (int) Math.round(utDate.timeofday.second));

                calendar = cal;
            }
        }

        return calendar;
    }

    /**
     * Log the time info (UTC + sideral time)
     * @param ww WhenWhere instance
     * @param label message to log
     */
    static void dumpWhen(final WhenWhere ww, final String label) {
        if (logger.isDebugEnabled()) {
            final InstantInTime t = ww.when;
            logger.debug("{} dump : {}\nUT : {}/{}/{} {}:{}:{}\nlst : {}",
                    label, t.jd, t.UTDate.day, t.UTDate.month, t.UTDate.year,
                    t.UTDate.timeofday.hour, t.UTDate.timeofday.minute, t.UTDate.timeofday.second,
                    ww.siderealobj.roundedRAString(3, ":"));
        }
    }

    /**
     * Return the almanac for the proper night (arround midnight).
     * Moreover, sun times are duplicated and translated to -1Day, +1Day and +2Day
     * to be able to have night limits among LST[-12;+36]
     * @return almanac
     */
    public AstroAlmanac getAlmanac() {
        // Get the almanac for the proper night (arround midnight):
        final AstroAlmanac almanacNight = new AstroAlmanac();

        // MIDNIGHT :
        addAlmanacTimes(this.jdMidnight, almanacNight);

        if (isLogDebug) {
            logger.debug("almanacNight: {}", almanacNight);
        }

        // Notes :
        // 1. Lst0 is arround midnight (+/- 12h):
        // 2. targets RA are in range [0;24]
        // 3. HA are in range [-12;12]
        // LST frame is in range [-12;36]
        // For midnight : in range [-24;48]
        // As target azEl is determined using JD and not LST (angle / decimal hours), it
        // requires to have bigger LST range.
        // Translates sun times (-1,1,2 days) only:
        final AstroAlmanac almanac = new AstroAlmanac();

        Set<AstroAlmanacTime> refTimes;

        // SUN:
        refTimes = almanacNight.getSunTimes();
        final Set<AstroAlmanacTime> sunTimes = almanac.getSunTimes();

        // MIDNIGHT - 1DAY :
        AstroAlmanac.translate(-LST_DAY_IN_JD, refTimes, sunTimes);
        // MIDNIGHT :
        sunTimes.addAll(refTimes);
        // MIDNIGHT + 1DAY :
        AstroAlmanac.translate(+LST_DAY_IN_JD, refTimes, sunTimes);
        // MIDNIGHT + 2DAY :
        AstroAlmanac.translate(2d * LST_DAY_IN_JD, refTimes, sunTimes);

        // Hack on moon to have still fake moon ranges (rise / set):
        // findMoonRiseSet returns ranges if (rise / set) couples are present!
        // note: will be helpful as moon distance check is implemented to avoid calculations when moon is set !
        // MOON:
        refTimes = almanacNight.getMoonTimes();
        final Set<AstroAlmanacTime> moonTimes = almanac.getMoonTimes();
        // MIDNIGHT - 1DAY :
        AstroAlmanac.translate(-LST_DAY_IN_JD, refTimes, moonTimes);
        // MIDNIGHT :
        moonTimes.addAll(refTimes);
        // MIDNIGHT + 1DAY :
        AstroAlmanac.translate(+LST_DAY_IN_JD, refTimes, moonTimes);
        // MIDNIGHT + 2DAY :
        AstroAlmanac.translate(2d * LST_DAY_IN_JD, refTimes, moonTimes);

        if (isLogDebug) {
            logger.debug("almanac: {}", almanac);
        }

        // Trim redundant times:
        // sun:
        refTimes.clear();
        refTimes.addAll(sunTimes);
        sunTimes.clear();
        AstroAlmanac.trim(refTimes, sunTimes);
        // moon:
        refTimes.clear();
        refTimes.addAll(moonTimes);
        moonTimes.clear();
        AstroAlmanac.trim(refTimes, moonTimes);

        if (isLogDebug) {
            logger.debug("almanac trimmed: {}", almanac);
        }

        return almanac;
    }

    /**
     * Add the sun and moon information as AstroAlmanacTime objects for the given date
     * @param jd julian date
     * @param almanac almanac instance set to store the AstroAlmanacTime objects
     */
    private void addAlmanacTimes(final double jd, final AstroAlmanac almanac) {
        final WhenWhere ww = new WhenWhere(jd, this.site, false);

        final NightlyAlmanac na = new NightlyAlmanac(ww, true);

        // Get midnight value to compute the jd boundaries:
        final double jd0 = na.jdMidnight - HALF_LST_DAY_IN_JD;
        final double jd1 = na.jdMidnight + HALF_LST_DAY_IN_JD;

        // fix sun ranges:
        fixRiseSet(na.sunriseTwilight18, na.sunsetTwilight18, jd0, jd1);
        fixRiseSet(na.sunriseTwilight12, na.sunsetTwilight12, jd0, jd1);
        fixRiseSet(na.sunriseTwilight06, na.sunsetTwilight06, jd0, jd1);
        fixRiseSet(na.sunrise, na.sunset, jd0, jd1);
        // fix moon ranges:
        fixRiseSet(na.moonrise, na.moonset, jd0, jd1);

        // add sun times:
        final Set<AstroAlmanacTime> sunTimes = almanac.getSunTimes();

        sunTimes.add(new AstroAlmanacTime(na.sunriseTwilight18.when.jd, AlmanacType.SunTwl18Rise));
        sunTimes.add(new AstroAlmanacTime(na.sunriseTwilight12.when.jd, AlmanacType.SunTwl12Rise));
        sunTimes.add(new AstroAlmanacTime(na.sunriseTwilight06.when.jd, AlmanacType.SunTwl06Rise));
        sunTimes.add(new AstroAlmanacTime(na.sunrise.when.jd, AlmanacType.SunRise));
        sunTimes.add(new AstroAlmanacTime(na.sunset.when.jd, AlmanacType.SunSet));
        sunTimes.add(new AstroAlmanacTime(na.sunsetTwilight06.when.jd, AlmanacType.SunTwl06Set));
        sunTimes.add(new AstroAlmanacTime(na.sunsetTwilight12.when.jd, AlmanacType.SunTwl12Set));
        sunTimes.add(new AstroAlmanacTime(na.sunsetTwilight18.when.jd, AlmanacType.SunTwl18Set));

        // add moon times:
        final Set<AstroAlmanacTime> moonTimes = almanac.getMoonTimes();

        moonTimes.add(new AstroAlmanacTime(na.moonrise.when.jd, AlmanacType.MoonRise));
        moonTimes.add(new AstroAlmanacTime(na.moonset.when.jd, AlmanacType.MoonSet));
    }

    private static void fixRiseSet(final WhenWhere rise, final WhenWhere set, final double jd0, final double jd1) {
        final double jdRise = rise.when.jd;
        final double jdSet = set.when.jd;

        if (Double.isInfinite(jdRise) || Double.isInfinite(jdSet)) {
            // equals to +Inf = impossible range
            // equals to -Inf: inclusive range

            if (jdRise == Double.POSITIVE_INFINITY) {
                rise.when.jd = jd1;
            } else if (jdRise == Double.NEGATIVE_INFINITY) {
                rise.when.jd = jd0;
            }

            if (jdSet == Double.POSITIVE_INFINITY) {
                set.when.jd = jd0;
            } else if (jdSet == Double.NEGATIVE_INFINITY) {
                set.when.jd = jd1;
            }
        }
    }

    /**
     * Return the jd ranges when moon is observable in range [jdLower; jdUpper] +/- 12h
     * @param almanac almanac instance
     * @param jdLower lower jd
     * @param jdUpper upper jd
     * @return list of jd ranges when moon is over the horizon
     */
    public List<Range> findMoonRiseSet(final AstroAlmanac almanac, final double jdLower, final double jdUpper) {
        final double jd0 = jdLower - HALF_LST_DAY_IN_JD;
        final double jd1 = jdUpper + HALF_LST_DAY_IN_JD;

        final List<AstroAlmanacTime> sorted = new ArrayList<AstroAlmanacTime>(almanac.getMoonTimes());
        final List<Range> ranges = new ArrayList<Range>(2);

        AstroAlmanacTime moonFrom, moonTo;
        double jdFrom, jdTo;

        for (int i = 0, size = sorted.size() - 1; i < size; i++) {
            moonFrom = sorted.get(i);

            if (moonFrom.getType() == AstroAlmanacTime.AlmanacType.MoonRise) {
                moonTo = sorted.get(i + 1);

                jdFrom = moonFrom.getJd();
                jdTo = moonTo.getJd();

                // Keep intervals that are inside or overlapping the range [jd0; jd1] :
                if ((jdFrom >= jd0 && jdFrom <= jd1) || (jdTo >= jd0 && jdTo <= jd1)
                        || (jdFrom < jd0 && jdTo > jd1)) {

                    if (isLogDebug) {
                        logger.debug("Range[{} - {}]", jdFrom, jdTo);
                    }

                    // adjust range limits :
                    if (jdFrom < jd0) {
                        jdFrom = jd0;
                    }
                    if (jdTo > jd1) {
                        jdTo = jd1;
                    }

                    ranges.add(new Range(jdFrom, jdTo));
                }
            }
        }

        if (isLogDebug) {
            logger.debug("moon ranges: {}", ranges);
        }

        return ranges;
    }

    /**
     * Return the maximum of the moon illumination fraction for the given jd ranges
     * @param moonRanges jd ranges
     * @return maximum of the moon illumination fraction
     */
    public double getMaxMoonIllum(final List<Range> moonRanges) {
        if (moonRanges == null) {
            return 0d;
        }
        double maxIllum = 0d;

        for (final Range range : moonRanges) {
            maxIllum = Math.max(maxIllum, moonIllum(range.getMin()));
            maxIllum = Math.max(maxIllum, moonIllum(range.getCenter()));
            maxIllum = Math.max(maxIllum, moonIllum(range.getMax()));
        }
        return maxIllum;
    }

    /**
     * Return the moon illumination fraction for the given julian date
     * @param jd julian date
     * @return moon illumination fraction
     */
    private double moonIllum(final double jd) {
        final WhenWhere wwMoon = new WhenWhere(jd, this.site);
        wwMoon.computeSunMoon();

        if (isLogDebug) {
            logger.debug("moon zenith = {}", toDateLST(jd));
            logger.debug("alt illum   = {}", wwMoon.altmoon);
            logger.debug("moon illum  = {}", wwMoon.moonillum);
        }

        return wwMoon.moonillum;
    }

    /**
     * Convert HA ranges to JD ranges in range [LST0 - 12; LST0 + 36] from the given list into the destination list
     * @param ranges HA range list to convert
     * @param destRanges JD range list to append into
     * @param precRA precessed target right ascension in decimal hours
     * @param rangeFactory Factory used to create Range instances
     */
    public void convertHAToJDRanges(final List<Range> ranges, final List<Range> destRanges, final double precRA,
                                    final RangeFactory rangeFactory) {
        if (ranges != null) {
            for (int i = 0, size = ranges.size(); i < size; i++) {
                destRanges.add(convertHAToJDRange(ranges.get(i), precRA, rangeFactory));
            }
        }
    }

    /**
     * Convert HA ranges to JD ranges in range [LST0 - 12; LST0 + 36] from the given list of ranges list into the destination list
     * @param rangesList list of HA ranges list to convert
     * @param destRanges JD range list to append into
     * @param precRA precessed target right ascension in decimal hours
     * @param rangeFactory Factory used to create Range instances
     */
    public void convertHAToJDRangesList(final List<List<Range>> rangesList, final List<Range> destRanges, final double precRA,
                                        final RangeFactory rangeFactory) {
        if (rangesList != null) {
            for (int i = 0, size = rangesList.size(); i < size; i++) {
                convertHAToJDRanges(rangesList.get(i), destRanges, precRA, rangeFactory);
            }
        }
    }

    /**
     * Convert an HA range to a JD range.
     * Returned JD values are in range [LST0 - 12; LST0 + 36]
     *
     * @param rangeHA given in hour angle (dec hours)
     * @param precRA precessed target right ascension in decimal hours
     * @param rangeFactory Factory used to create Range instances
     * @return JD range
     */
    public Range convertHAToJDRange(final Range rangeHA, final double precRA, final RangeFactory rangeFactory) {
        final double ha1 = rangeHA.getMin();
        final double ha2 = rangeHA.getMax();

        if (DO_TRACE) {
            logger.trace("ha1 = {}", ha1);
            logger.trace("ha2 = {}", ha2);
        }

        final double jd1 = convertHAToJD(ha1, precRA);
        final double jd2 = convertHAToJD(ha2, precRA);

        if (DO_TRACE) {
            logger.trace("jd1 = {}", toDateLST(jd1));
            logger.trace("jd2 = {}", toDateLST(jd2));
        }

        return rangeFactory.valueOf(jd1, jd2);
    }

    /**
     * Convert JD ranges to HA ranges from the given list into the destination list
     * @param ranges JD range list to convert
     * @param destRanges HA range list to append into
     * @param precRA precessed target right ascension in decimal hours
     * @param rangeFactory Factory used to create Range instances
     */
    public void convertJDToHARanges(final List<Range> ranges, final List<Range> destRanges, final double precRA,
                                    final RangeFactory rangeFactory) {
        if ((ranges != null) && !ranges.isEmpty()) {
            Range rangeHA;
            for (int i = 0, size = ranges.size(); i < size; i++) {
                rangeHA = convertJDToHARange(ranges.get(i), precRA, rangeFactory);
                if (rangeHA != null) {
                    destRanges.add(rangeHA);
                }
            }
        }
    }

    /**
     * Convert a JD range to an HA range but keep only ranges with an HA in [-12;12]
     * @param rangeJD JD range
     * @param precRA precessed target right ascension in decimal hours
     * @param rangeFactory Factory used to create Range instances
     * @return HA range in [-12;12]
     */
    public Range convertJDToHARange(final Range rangeJD, final double precRA, final RangeFactory rangeFactory) {
        final double jd1 = rangeJD.getMin();
        final double jd2 = rangeJD.getMax();

        if (DO_TRACE) {
            logger.trace("jd1 = {}", toDateLST(jd1));
            logger.trace("jd2 = {}", toDateLST(jd2));
        }

        double ha1 = convertJDToHA(jd1, precRA);
        double ha2 = convertJDToHA(jd2, precRA);

        if (ha1 < AsproConstants.HA_MIN) {
            ha1 = AsproConstants.HA_MIN;
        }
        if (ha1 > AsproConstants.HA_MAX) {
            // invalid range :
            return null;
        }
        if (ha2 < AsproConstants.HA_MIN) {
            // invalid range :
            return null;
        }
        if (ha2 > AsproConstants.HA_MAX) {
            ha2 = AsproConstants.HA_MAX;
        }

        if (DO_TRACE) {
            logger.trace("ha1 = {} h", ha1);
            logger.trace("ha2 = {} h", ha2);
        }

        return (rangeFactory != null) ? rangeFactory.valueOf(ha1, ha2) : new Range(ha1, ha2);
    }

    /**
     * Convert LST decimal hours in Julian day using internal LST0 reference.
     * @param lst decimal hours
     * @return JD value
     */
    public double convertLstToJD(final double lst) {
        // apply the sideral / solar ratio :
        return this.jdLst0 + AstroSkyCalc.lst2jd(lst);
    }

    /**
     * Convert a decimal hour angle in Julian day using internal LST0 reference.
     * Returned JD value is in range [LST0 - 12; LST0 + 36]
     * @param ha decimal hour angle
     * @param precRA precessed target right ascension in decimal hours
     * @return JD value
     */
    public double convertHAToJD(final double ha, final double precRA) {
        return convertLstToJD(precRA + ha);
    }

    /**
     * Convert a Julian day in decimal hour angle
     * Note : HA is not resticted to [-12;12]
     * @param jd date time
     * @param precRA precessed target right ascension in decimal hours
     * @return ha decimal hour angle
     */
    public double convertJDToHA(final double jd, final double precRA) {
        // apply the sideral / solar ratio :
        final double lst = AstroSkyCalc.jd2lst(jd - this.jdLst0);

        return lst - precRA;
    }

    /**
     * Check and return an hour angle within boundaries [-12;12]
     * @param haIn input hour angle
     * @return hour angle within boundaries [-12;12]
     */
    public static double checkHA(double haIn) {
        double ha = haIn;
        while (ha < AsproConstants.HA_MIN) {
            ha += 24.0;
        }
        while (ha > AsproConstants.HA_MAX) {
            ha -= 24.0;
        }
        return ha;
    }

    // static methods :
    /**
     * Convert lst hours to jd hours
     * @param lst dec hours in LST
     * @return dec hours in JD
     */
    public static double lst2jd(final double lst) {
        return lst * LST_TO_JD;
    }

    /**
     * Convert jd hours to lst hours
     * @param jd dec hours
     * @return dec hours in LST
     */
    public static double jd2lst(final double jd) {
        return jd * JD_TO_LST;
    }

    /**
     * Convert solar duration to lst duration
     * @param solar solar duration
     * @return lst duration
     */
    public static double solar2lst(final double solar) {
        return solar * SIDERAL_RATIO;
    }

    /**
     * Return the modified julian day from the given julian day
     * @param jd julian day
     * @return modified julian day
     */
    public static double mjd(final double jd) {
        return jd - MJD_REF;
    }

    /**
     * Return the true airmass for a given elevation (degrees).
     * @param elev elevation in degrees [0;90]
     * @return true airmass for a given elevation
     */
    public static double airmass(final double elev) {
        final double airmass = Spherical.true_airmass(elev);

        // return NaN for elevation = 0 deg:
        return (airmass < 0.0) ? Double.NaN : airmass;
    }

    /**
     * Convert the given calendar to a date within range [dateMin; dateMax]
     * @param cal date/time to convert
     * @param timeRef time reference (LST, UTC or Local) for given date range
     * @param dateMin lower date/time
     * @param dateMax upper date/time
     * @return date/time within range [dateMin; dateMax]
     */
    private Date convertCalendarToDate(final Calendar cal, final TimeRef timeRef, final Date dateMin, final Date dateMax) {
        // Note: use Calendar.roll to only fix date field

        // time in UTC:
        final long timeInMillis;

        if (timeRef == TimeRef.LOCAL) {
            final Date date = cal.getTime();
            // Convert UTC to Local time and modify date:
            convertUTCToLocal(date, siteTZ);
            // Fix time to compare with date ranges:
            timeInMillis = date.getTime();
        } else {
            timeInMillis = cal.getTimeInMillis();
        }

        if (timeInMillis >= dateMin.getTime()) {

            if (timeInMillis > dateMax.getTime()) {
                // after date max :

                // return [cal - 1 day]
                cal.roll(Calendar.DATE, false);
            }

        } else {
            // before date min:

            // return [cal + 1 day]
            cal.roll(Calendar.DATE, true);
        }
        return cal.getTime();
    }

    private static void convertUTCToLocal(final Date date, final TimeZone localTZ) {
        if (localTZ == null) {
            throw new IllegalStateException("Site timezone is undefined !");
        }
        if (DO_TRACE) {
            logger.debug("date[UTC]: {}", date);
        }
        // number of milliseconds since January 1, 1970, 00:00:00 GMT (UTC)
        long milliseconds = date.getTime();

        // add the amount of time in milliseconds to add to UTC 
        // to get standard time in this time zone
        milliseconds += localTZ.getRawOffset();

        // If Daylight Saving Time:
        if (localTZ.inDaylightTime(date)) {
            // add the amount of time to be added to local standard time
            // to get local wall clock time
            milliseconds += localTZ.getDSTSavings();
        }
        date.setTime(milliseconds);

        if (DO_TRACE) {
            logger.debug("date[{}]: {}", localTZ.getID(), date);
        }
    }
}
