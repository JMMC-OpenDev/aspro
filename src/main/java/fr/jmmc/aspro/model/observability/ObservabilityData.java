/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.observability;

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.model.TimeRef;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.ObservationVersion;
import fr.jmmc.oitools.model.range.Range;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.observability.SunTimeInterval.SunType;
import fr.jmmc.aspro.model.oi.Target;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class contains the results of the Observability service
 * @author bourgesl
 */
public final class ObservabilityData {

    /** observation version */
    private final ObservationVersion version;

    /* input parameters */
    /** time reference indicating if the timestamps are expressed in LST, UTC or Local time */
    private final TimeRef timeRef;
    /** flag to find baseline limits */
    private final boolean doBaseLineLimits;
    /** flag to produce detailed output with all BL / horizon / rise intervals per target */
    private final boolean doDetailedOutput;
    /** flag to center the plot arround midnight */
    private final boolean doCenterMidnight;
    /** twilight considered as night limit */
    private final SunType twilightNightLimit;

    /* observability plot data */
    /** human-readable name of the observatory time zone in the default locale (US) */
    private String timezoneID = null;
    /** starting date */
    private Date dateMin = null;
    /** ending date */
    private Date dateMax = null;
    /** midnight date */
    private Date dateMidnight = null;
    /** day range as string */
    private String dayRangeLabel = null;
    /** flag indicating a DST change within starting and ending date */
    private boolean dstChange = false;
    /** starting date as julian date */
    private double jdMin = 0d;
    /** ending date as julian date */
    private double jdMax = 0d;
    /** list of sun time intervals */
    private List<SunTimeInterval> sunIntervals = null;
    /** moon illumination fraction (percent) */
    private double moonIllumPercent = 0d;
    /** target list (useful for baseline limits) */
    private List<Target> targets;
    /** map of StarObservabilityData list keyed by target name */
    private final Map<String, List<StarObservabilityData>> mapStarVisibilities = new HashMap<String, List<StarObservabilityData>>();
    /** flag indicating to use user PoPs */
    private boolean userPops;
    /** optional best PoPs combination */
    private PopCombination bestPops;
    /** optional best PoPs combinations */
    private List<PopCombination> bestPopList;
    /** optional good PoPs combinations */
    private List<PopCombination> betterPopList;
    /** warning container */
    private final WarningContainer warningContainer = new WarningContainer();

    /* other useful data for UV coverage */
    /** AstroSkyCalc instance useful to convert HA in LST or UTC */
    private AstroSkyCalc dateCalc = null;
    /** configuration */
    private String stationNames;
    /** beam list */
    private List<Beam> beams = null;
    /** base line list */
    private List<BaseLine> baseLines = null;
    /** map of StarData keyed by target name */
    private final Map<String, StarData> mapStarDatas = new HashMap<String, StarData>();

    /**
     * Public Constructor
     * @param version observation version
     * @param timeRef time reference (LST, UTC or Local)
     * @param doDetailedOutput flag to produce detailed output with all BL / horizon / rise intervals per target
     * @param doBaseLineLimits flag to find base line limits
     * @param doCenterMidnight flag to center the plot arround midnight
     * @param twilightNightLimit twilight considered as night limit
     */
    public ObservabilityData(final ObservationVersion version, final TimeRef timeRef, final boolean doDetailedOutput, final boolean doBaseLineLimits,
                             final boolean doCenterMidnight, final SunType twilightNightLimit) {
        this.version = version;
        this.timeRef = timeRef;
        this.doDetailedOutput = doDetailedOutput;
        this.doBaseLineLimits = doBaseLineLimits;
        this.doCenterMidnight = doCenterMidnight;
        this.twilightNightLimit = twilightNightLimit;
    }

    /* version */
    /**
     * Return the observation version
     * @return observation version
     */
    public ObservationVersion getVersion() {
        return version;
    }

    /* inputs */
    /**
     * Return the time reference indicating if the timestamps are expressed in LST, UTC or Local time
     * @return time reference
     */
    public TimeRef getTimeRef() {
        return timeRef;
    }

    /**
     * Return the flag to find baseline limits
     * @return flag to find baseline limits
     */
    public boolean isDoBaseLineLimits() {
        return doBaseLineLimits;
    }

    /**
     * Return the flag to produce detailed output with all BL / horizon / rise intervals per target
     * @return flag to produce detailed output with all BL / horizon / rise intervals per target
     */
    public boolean isDoDetailedOutput() {
        return doDetailedOutput;
    }

    /**
     * Return the flag to center the plot arround midnight
     * @return flag to center the plot arround midnight
     */
    public boolean isDoCenterMidnight() {
        return doCenterMidnight;
    }

    /**
     * Return the twilight considered as night limit
     * @return twilight considered as night limit
     */
    public SunType getTwilightNightLimit() {
        return twilightNightLimit;
    }

    /* outputs */
    /**
     * Return the human-readable name of the observatory time zone in the default locale (US)
     * @return human-readable name of the observatory time zone in the default locale (US)
     */
    public String getTimezoneID() {
        return timezoneID;
    }

    /**
     * Define the human-readable name of the observatory time zone in the default locale (US)
     * @param timezoneID human-readable name of the observatory time zone in the default locale (US)
     */
    public void setTimezoneID(final String timezoneID) {
        this.timezoneID = timezoneID;
    }

    /**
     * Return the starting date as julian date
     * @return starting date as julian date
     */
    public double getJdMin() {
        return jdMin;
    }

    /**
     * Define the starting date as julian date
     * @param jdMin starting date as julian date
     */
    public void setJdMin(final double jdMin) {
        this.jdMin = jdMin;
        this.dateMin = jdToDate(jdMin);
    }

    /**
     * Return the ending date as julian date
     * @return ending date as julian date
     */
    public double getJdMax() {
        return jdMax;
    }

    /**
     * Define the ending date as julian date
     * @param jdMax ending date as julian date
     */
    public void setJdMax(final double jdMax) {
        this.jdMax = jdMax;
        this.dateMax = jdToDate(jdMax);
    }

    /**
     * Return the starting date
     * @return starting date
     */
    public Date getDateMin() {
        return dateMin;
    }

    /**
     * Return the ending date
     * @return ending date
     */
    public Date getDateMax() {
        return dateMax;
    }

    /**
     * Return the midnight date
     * @return midnight date
     */
    public Date getDateMidnight() {
        return dateMidnight;
    }

    /**
     * Define the midnight date
     * @param jdMidnight midnight as julian date
     * @return flag indicating a DST change within starting and ending date
     */
    public boolean defineDateMidnight(final double jdMidnight) {
        // Use jdMin to be the DAY before DST change:
        final Date utDateMin = dateCalc.toDate(jdMin, TimeRef.UTC);

        final long offDateMin = dateCalc.getDateOffset(utDateMin);

        double jdOffset = 0.0;

        if (offDateMin != 0l) {
            jdOffset = (dateCalc.getSiteStdZone() - (offDateMin / 3600000L)) / 24.0;
        }

        final double jdMidnightFixed = jdMidnight - jdOffset;

        // 00:00:00 (DD+1):
        this.dateMidnight = dateCalc.toDate(jdMidnightFixed, timeRef);

        defineDayRange(jdMidnightFixed);

        // Use jdMax to be the DAY after DST change:
        final Date utDateMax = dateCalc.toDate(jdMax, TimeRef.UTC);

        final long offDateMax = dateCalc.getDateOffset(utDateMax);

        this.dstChange = (offDateMin != offDateMax);

        return dstChange;
    }

    private void defineDayRange(final double jdMidnightFixed) {
        final StringBuilder sb = new StringBuilder(14);

        // get local midnight date:
        final Date date = dateCalc.toDate(jdMidnightFixed, TimeRef.LOCAL);
        final long midLocal = date.getTime();

        // DAY before midnight - 1h0m1s :
        date.setTime(midLocal - 3601000L);
        dateCalc.toDateString(sb, date);

        sb.append(" - ");

        // DAY after midnight + 1h0m1s :
        date.setTime(midLocal + 3601000L);
        dateCalc.toDateString(sb, date);

        this.dayRangeLabel = sb.toString();
    }

    /**
     * Return the day range as string
     * @return day range as string
     */
    public String getDayRangeLabel() {
        return dayRangeLabel;
    }

    /**
     * Return the flag indicating a DST change within starting and ending date
     * @return flag indicating a DST change within starting and ending date
     */
    public boolean isDstChange() {
        return dstChange;
    }

    /**
     * Return the list of sun time intervals
     * @return list of sun time intervals
     */
    public List<SunTimeInterval> getSunIntervals() {
        return sunIntervals;
    }

    /**
     * Define the list of sun time intervals
     * @param sunIntervals list of sun time intervals
     */
    public void setSunIntervals(final List<SunTimeInterval> sunIntervals) {
        this.sunIntervals = sunIntervals;
    }

    /**
     * Return the moon illumination fraction (percent)
     * @return moon illumination fraction (percent)
     */
    public double getMoonIllumPercent() {
        return moonIllumPercent;
    }

    /**
     * Define the moon illumination fraction (percent)
     * @param moonIllumPercent moon illumination fraction (percent)
     */
    public void setMoonIllumPercent(final double moonIllumPercent) {
        this.moonIllumPercent = moonIllumPercent;
    }

    /**
     * Return the target list (useful for baseline limits)
     * @return target list (useful for baseline limits)
     */
    public List<Target> getTargets() {
        return targets;
    }

    /**
     * Define the target list (useful for baseline limits)
     * @param targets target list (useful for baseline limits)
     */
    public void setTargets(final List<Target> targets) {
        this.targets = targets;
    }

    /**
     * Add the StarObservabilityData list for the given target name
     * @param name target name
     * @param starVis StarObservabilityData list to add
     */
    public void addStarVisibilities(final String name, final List<StarObservabilityData> starVis) {
        this.mapStarVisibilities.put(name, starVis);
    }

    /**
     * Return the map of StarObservabilityData list keyed by target name
     * @return map of StarObservabilityData list keyed by target name
     */
    public Map<String, List<StarObservabilityData>> getMapStarVisibilities() {
        return this.mapStarVisibilities;
    }

    /**
     * Return the flag indicating to use user PoPs
     * @return flag indicating to use user PoPs
     */
    public boolean isUserPops() {
        return userPops;
    }

    /**
     * Define the flag indicating to use user PoPs
     * @param userPops flag indicating to use user PoPs
     */
    public void setUserPops(final boolean userPops) {
        this.userPops = userPops;
    }

    /**
     * Return the optional best PoPs combination
     * @return optional best PoPs combination
     */
    public PopCombination getBestPops() {
        return bestPops;
    }

    /**
     * Define the optional best PoPs combination
     * @param bestPops optional best PoPs combination
     */
    public void setBestPops(final PopCombination bestPops) {
        this.bestPops = bestPops;
    }

    public List<PopCombination> getBestPopList() {
        return bestPopList;
    }

    public void setBestPopList(List<PopCombination> bestPopList) {
        this.bestPopList = bestPopList;
    }

    public List<PopCombination> getBetterPopList() {
        return betterPopList;
    }

    public void setBetterPopList(List<PopCombination> betterPopList) {
        this.betterPopList = betterPopList;
    }

    /* other useful data for UV coverage */
    /**
     * Return the AstroSkyCalc instance useful to convert HA in LST or UTC
     * @return AstroSkyCalc instance useful to convert HA in LST or UTC
     */
    public AstroSkyCalc getDateCalc() {
        return dateCalc;
    }

    /**
     * Define the AstroSkyCalc instance
     * @param dateCalc AstroSkyCalc instance
     */
    public void setDateCalc(final AstroSkyCalc dateCalc) {
        this.dateCalc = dateCalc;
    }

    /**
     * Return the configuration
     * @return configuration
     */
    public String getStationNames() {
        return stationNames;
    }

    /**
     * Define the configuration
     * @param stationNames configuration
     */
    public void setStationNames(final String stationNames) {
        this.stationNames = stationNames;
    }

    /**
     * Return the beam list
     * @return beam list
     */
    public List<Beam> getBeams() {
        return beams;
    }

    /**
     * Define the beam list
     * @param beams beam list
     */
    public void setBeams(final List<Beam> beams) {
        this.beams = beams;
    }

    /**
     * Return the base line list
     * @return base line list
     */
    public List<BaseLine> getBaseLines() {
        return baseLines;
    }

    /**
     * Define the base line list
     * @param baseLines base line list
     */
    public void setBaseLines(final List<BaseLine> baseLines) {
        this.baseLines = baseLines;
    }

    /**
     * Add the given StarData
     * @param starData StarData to add
     */
    public void addStarData(final StarData starData) {
        this.mapStarDatas.put(starData.getName(), starData);
    }

    /**
     * Return the StarData for the given target name
     * @param name target name
     * @return StarData or null if not found
     */
    public StarData getStarData(final String name) {
        return this.mapStarDatas.get(name);
    }

    /**
     * Return the warning container
     * @return warning container
     */
    public WarningContainer getWarningContainer() {
        return warningContainer;
    }

    /**
     * Fix a JD date into LST range [0;24]
     *
     * @param jd date to fix
     * @return fixed jd in range
     */
    public double getJDInLstRange(final double jd) {
        if (jd >= jdMin) {
            if (jd <= jdMax) {
                // JD in [jdLst0;jdLst24]
                return jd;
            }
            // over LST 24 :
            // return [jd - day]
            return jd - AstroSkyCalc.LST_DAY_IN_JD;
        }
        // start occurs before LST 0h :
        // return [jd + day]
        return jd + AstroSkyCalc.LST_DAY_IN_JD;
    }

    /**
     * Get the JD offset into the night range given a JD date 
     *
     * @param jd date to fix
     * @return offset
     */
    public double getJDOffset(final double jd) {
        if (jd >= jdMin) {
            if (jd <= jdMax) {
                // JD in [jdLst0;jdLst24]
                return 0.0;
            }
            // over LST 24 :
            // return [- day]
            return -AstroSkyCalc.LST_DAY_IN_JD;
        }
        // start occurs before LST 0h :
        // return [+ day]
        return +AstroSkyCalc.LST_DAY_IN_JD;
    }

    /**
     * Convert a JD date to a date within LST range [0;24]
     *
     * @param jd date to convert
     * @return date
     */
    public Date convertJDToDate(final double jd) {
        // fix JD in LST range [0; 24] in order to have accurate date:
        return jdToDateInDateRange(getJDInLstRange(jd));
    }

    /**
     * Convert a JD value to a Date Object (LST or UTC)
     * within range [jdLst0;jdLst24]<=>[DateMin;DateMax]
     * @see #useLST
     * @see #jdToDate(double)
     * @param jd julian day
     * @return Date Object (LST or UTC)
     */
    public Date jdToDateInDateRange(final double jd) {
        // adjust range limits :
        if (jd <= jdMin) {
            return dateMin;
        }
        if (jd >= jdMax) {
            return dateMax;
        }
        return jdToDate(jd);
    }

    /**
     * Convert a JD value to a Date Object (see TimeReference)
     * @see #useLST
     * @param jd julian day
     * @return Date Object (TimeReference)
     */
    public Date jdToDate(final double jd) {
        return dateCalc.toDate(jd, timeRef);
    }

    /**
     * Return the jd at the middle of the range [jdLower; jdUpper]
     * @return jd at the middle of the range
     */
    public double jdCenter() {
        return 0.5d * (jdMin + jdMax);
    }

    /**
     * Convert a JD range list to a date interval with respect for the LST range [0;24]
     * 
     * @param rangesJD range list to convert
     * @param intervals interval list where new date intervals will be added
     */
    public void convertRangesToDateIntervals(final List<Range> rangesJD, final List<DateTimeInterval> intervals) {
        if ((rangesJD != null) && !rangesJD.isEmpty()) {
            for (int i = 0, len = rangesJD.size(); i < len; i++) {
                convertRangeToDateInterval(rangesJD.get(i), intervals);
            }
            if (intervals.size() > 1) {
                // merge contiguous date ranges :
                DateTimeInterval.merge(intervals);
            }
        }
    }

    /**
     * Convert a JD range to a date interval with respect for the LST range [0;24]
     * Note : due to HA limit [+/-12h], the converted JD / Date ranges
     * can have a discontinuity on the date axis !
     *
     * @param rangeJD range to convert
     * @param intervals interval list where new date intervals will be added
     */
    private void convertRangeToDateInterval(final Range rangeJD, final List<DateTimeInterval> intervals) {
        final double jdStart = rangeJD.getMin();
        final double jdEnd = rangeJD.getMax();

        if (jdStart >= jdMin) {

            if (jdEnd <= jdMax) {

                // single interval [jdStart;jdEnd]
                intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart), jdToDateInDateRange(jdEnd)));

            } else {
                if (jdStart > jdMax) {
                    // two points over LST 24 :

                    // single interval [jdStart - day;jdEnd - day]
                    intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart - AstroSkyCalc.LST_DAY_IN_JD),
                            jdToDateInDateRange(jdEnd - AstroSkyCalc.LST_DAY_IN_JD)));

                } else {
                    // end occurs after LST 24 :

                    // interval [jdStart;jdLst24]
                    intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart), getDateMax()));

                    // add the second interval [jdLst0;jdEnd - day]
                    intervals.add(new DateTimeInterval(getDateMin(), jdToDateInDateRange(jdEnd - AstroSkyCalc.LST_DAY_IN_JD)));
                }
            }

        } else {
            // start occurs before LST 0h :

            if (jdEnd < jdMin) {
                // two points before LST 0h :

                // single interval [jdStart + day;jdEnd + day]
                intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart + AstroSkyCalc.LST_DAY_IN_JD),
                        jdToDateInDateRange(jdEnd + AstroSkyCalc.LST_DAY_IN_JD)));

            } else {
                // interval [jdLst0;jdEnd]
                intervals.add(new DateTimeInterval(getDateMin(), jdToDateInDateRange(jdEnd)));

                // add the second interval [jdStart + day;jdLst24]
                intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart + AstroSkyCalc.LST_DAY_IN_JD), getDateMax()));
            }
        }
    }

}
