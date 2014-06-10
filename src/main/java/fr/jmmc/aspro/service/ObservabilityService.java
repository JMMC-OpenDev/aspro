/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import ch.qos.logback.classic.Level;
import edu.dartmouth.AstroAlmanac;
import edu.dartmouth.AstroAlmanacTime;
import edu.dartmouth.AstroSkyCalc;
import edu.dartmouth.AstroSkyCalcObservation;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.HorizonShape;
import fr.jmmc.aspro.model.BestPoPsObservabilityContext;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.ObservabilityContext;
import fr.jmmc.aspro.model.observability.DateTimeInterval;
import fr.jmmc.aspro.model.observability.GroupedPopObservabilityData;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.observability.PopCombination;
import fr.jmmc.aspro.model.observability.PopObservabilityData;
import fr.jmmc.aspro.model.observability.SharedPopCombination;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.observability.StarObservabilityData;
import fr.jmmc.aspro.model.observability.SunTimeInterval;
import fr.jmmc.aspro.model.observability.SunTimeInterval.SunType;
import fr.jmmc.aspro.model.observability.TargetPositionDate;
import fr.jmmc.aspro.model.oi.AzEl;
import fr.jmmc.aspro.model.oi.Channel;
import fr.jmmc.aspro.model.oi.ChannelLink;
import fr.jmmc.aspro.model.oi.DelayLine;
import fr.jmmc.aspro.model.oi.DelayLineRestriction;
import fr.jmmc.aspro.model.oi.DelayLineRestrictionThrow;
import fr.jmmc.aspro.model.oi.FluxCondition;
import fr.jmmc.aspro.model.oi.FocalInstrument;
import fr.jmmc.aspro.model.oi.FocalInstrumentConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.MoonPointingRestriction;
import fr.jmmc.aspro.model.oi.MoonRestriction;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Operator;
import fr.jmmc.aspro.model.oi.Pop;
import fr.jmmc.aspro.model.oi.PopLink;
import fr.jmmc.aspro.model.oi.Position3D;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.StationLinks;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.model.oi.Telescope;
import fr.jmmc.aspro.service.pops.BestPopsEstimator;
import fr.jmmc.aspro.service.pops.BestPopsEstimatorFactory;
import fr.jmmc.aspro.service.pops.BestPopsEstimatorFactory.Algorithm;
import fr.jmmc.aspro.service.pops.Criteria;
import fr.jmmc.aspro.util.TestUtils;
import fr.jmmc.jmcs.logging.LoggingService;
import fr.jmmc.jmcs.util.SpecialChars;
import fr.jmmc.jmcs.util.concurrent.InterruptedJobException;
import fr.jmmc.jmcs.util.concurrent.ParallelJobExecutor;
import fr.jmmc.oitools.util.CombUtils;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import javax.xml.datatype.XMLGregorianCalendar;
import net.jafama.FastMath;
import fr.jmmc.jmcs.util.CollectionUtils;
import fr.jmmc.jmcs.util.FormatterUtils;
import fr.jmmc.jmcs.util.ObjectUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This service determines the observability of a list of targets given an observation setting
 *
 * @author bourgesl
 */
public final class ObservabilityService {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(ObservabilityService.class.getName());
    /** Class logger */
    private static final Logger loggerTasks = LoggerFactory.getLogger(ObservabilityService.class.getName() + "Tasks");
    /** flag to show range factory statistics */
    private final static boolean SHOW_RANGE_FACTORY_STATS = false;
    /** flag to show best pops statistics */
    private final static boolean SHOW_BEST_POPS_STATS = false;
    /** flag to show task statistics */
    private final static boolean SHOW_TASK_STATS = false;
    /** flag to slow down the service to detect concurrency problems */
    private final static boolean DEBUG_SLOW_SERVICE = false;
    /** number of best Pops displayed in warnings */
    private final static int MAX_POPS_IN_WARNING = 15;
    /** jd step = 1 minute */
    private final double JD_STEP = (1d / 60d) / 24d;
    /** Moon separation margin = 0.5 arcmin for uncertainty */
    private final double MOON_SEPARATION_MARGIN = 0.5d / 60d;
    /** Jmcs Parallel Job executor */
    private static final ParallelJobExecutor jobExecutor = ParallelJobExecutor.getInstance();
    /** shared cache of Pop combinations keyed by 'interferometer_nBL' */
    private static final Map<String, List<SharedPopCombination>> popCombinationCache = new HashMap<String, List<SharedPopCombination>>(16);
    /** shared InterruptedJobException instance */
    private static final InterruptedJobException ije = new InterruptedJobException("ObservabilityService.compute: interrupted");

    /* members */
    /** cached log debug enabled */
    private final boolean isLogDebug = logger.isDebugEnabled();
    /** reused string buffer instance */
    private final StringBuffer shared_sb = new StringBuffer(128);
    /** temporary list of ranges to merge */
    private ArrayList<Range> tmpRanges = null;
    /** main observability context (RangeFactory) */
    private final ObservabilityContext obsCtx = new ObservabilityContext(15); // 6T
    /** observability context dedicated to best Pops estimators (temporary variables) */
    private BestPoPsObservabilityContext bpObsCtx = null;

    /* output */
    /** observability data */
    private final ObservabilityData data;

    /* inputs */
    /** observation settings used  (read-only copy of the modifiable observation) */
    private final ObservationSetting observation;
    /** indicates if the timestamps are expressed in LST or in UTC */
    private final boolean useLST;
    /** flag to find baseline limits */
    private final boolean doBaseLineLimits;
    /** flag to produce detailed output with all BL / horizon / rise intervals per target */
    private final boolean doDetailedOutput;
    /** flag to center the plot arround midnight */
    private final boolean doCenterMidnight;
    /** twilight considered as night limit */
    private final SunType twilightNightLimit;
    /** Best Pops algorithm */
    private final Algorithm bestPopsAlgorithm;
    /** optional Best Pops criteria on sigma */
    private final Criteria bestPopEstimatorCriteriaSigma;
    /** optional Best Pops criteria on average weight */
    private final Criteria bestPopEstimatorCriteriaAverageWeight;

    /* internal */
    /** double formatter for moon separation */
    private final NumberFormat df1 = new DecimalFormat("0.0");
    /** 24h date formatter like in france */
    private final DateFormat timeFormatter = DateFormat.getTimeInstance(DateFormat.SHORT, Locale.FRANCE);
    /** Get the current thread to check if the computation is interrupted */
    private final Thread currentThread = Thread.currentThread();
    /** sky calc instance */
    private final AstroSkyCalc sc = new AstroSkyCalc();
    /** observation sky calc instance */
    private final AstroSkyCalcObservation sco = new AstroSkyCalcObservation();
    /** lower jd arround the observation date */
    private double jdLower;
    /** upper jd arround the observation date */
    private double jdUpper;
    /** flag to enable the observability restriction due to the night */
    private boolean useNightLimit;
    /** Night ranges defined in julian day corresponding to the given date [0;24H] */
    private List<Range> nightOnlyRanges = null;
    /** Night ranges defined in julian day between [-12; +36H] */
    private List<Range> nightLimits = null;
    /** minimum of elevation to observe any target (deg) */
    private double minElev = Double.NaN;
    /** interferometer description */
    private InterferometerDescription interferometer = null;
    /** focal instrument description */
    private FocalInstrument instrument = null;
    /** flag to indicate that a station has an horizon profile */
    private boolean hasHorizon = false;
    /** flag to indicate that pops are used */
    private boolean hasPops = false;
    /** flag to indicate that wind restriction is used */
    private boolean hasWindRestriction = false;
    /** flag to indicate that delay lines may have custom maximum throw in switchyard (ie per station ~ VLTI VCM limits) */
    private boolean hasSwitchyardDelayLineMaxThrow = false;
    /** beam list */
    private List<Beam> beams = null;
    /** base line list */
    private List<BaseLine> baseLines = null;
    /** W ranges corresponding to the base line list */
    private List<Range> wRanges = null;
    /** W ranges (VCM limit) corresponding to the delay line restriction and base line list */
    private List<List<Range>> wRangesVcms = null;
    /** list of Pop combinations with pop delays per baseline */
    private List<PopCombination> popCombinations = null;
    /** flag to disable the observability restriction due to the night */
    private boolean ignoreUseNightLimit = false;
    /** azimuth expressed in [0; 360 deg] */
    private Double windAzimuth = null;
    /** discarded azimuth ranges due to wind direction */
    private List<Range> azimuthRanges = null;
    /** optional moon pointing restrictions */
    private MoonPointingRestriction moonPointingRestriction = null;

    static {
        if (SHOW_TASK_STATS || SHOW_BEST_POPS_STATS) {
            LoggingService.setLoggerLevel(loggerTasks, Level.DEBUG);
        }
    }

    /**
     * Constructor.
     * Note : This service is statefull so it can not be reused by several calls.
     *
     * @param observation observation settings
     * @param useLST indicates if the timestamps are expressed in LST or in UTC
     * @param doDetailedOutput flag to produce detailed output with all BL / horizon / rise intervals per target
     * @param doBaseLineLimits flag to find base line limits
     * @param doCenterMidnight flag to center JD range arround midnight
     * @param twilightNightLimit twilight considered as night limit
     * @param bestPopsAlgorithm Best Pops algorithm
     * @param bestPopEstimatorCriteriaSigma optional Best Pops criteria on sigma
     * @param bestPopEstimatorCriteriaAverageWeight optional Best Pops criteria on average weight
     */
    public ObservabilityService(final ObservationSetting observation,
                                final boolean useLST, final boolean doDetailedOutput, final boolean doBaseLineLimits,
                                final boolean doCenterMidnight, final SunType twilightNightLimit,
                                final Algorithm bestPopsAlgorithm,
                                final Criteria bestPopEstimatorCriteriaSigma, final Criteria bestPopEstimatorCriteriaAverageWeight) {

        // Inputs :
        this.observation = observation;
        this.useLST = useLST;
        this.doDetailedOutput = doDetailedOutput;
        this.doBaseLineLimits = doBaseLineLimits;
        this.doCenterMidnight = doCenterMidnight;
        this.twilightNightLimit = twilightNightLimit;

        // Best Pops estimator preferences:
        this.bestPopsAlgorithm = bestPopsAlgorithm;
        this.bestPopEstimatorCriteriaSigma = bestPopEstimatorCriteriaSigma;
        this.bestPopEstimatorCriteriaAverageWeight = bestPopEstimatorCriteriaAverageWeight;

        // estimate the observability data corresponding to the observation version :
        this.data = new ObservabilityData(observation.getVersion(), useLST, doDetailedOutput, doBaseLineLimits, doCenterMidnight, twilightNightLimit);
    }

    /**
     * Specific Constructor to prepare VLTI Observing blocks (LST)
     * Note : This service is statefull so it can not be reused by several calls.
     *
     * @param observation observation settings
     */
    public ObservabilityService(final ObservationSetting observation) {
        this(observation, false);
    }

    /**
     * Specific Constructor to prepare Observing blocks (LST) using astronomical night (-18 deg) ignoring night restrictions (OB VEGA only)
     * Note : This service is statefull so it can not be reused by several calls.
     *
     * @param observation observation settings
     * @param ignoreNightLimits true to disable the observability restriction due to the night
     */
    public ObservabilityService(final ObservationSetting observation, final boolean ignoreNightLimits) {
        // use LST without using night center:
        this(observation, true, false, false, false, SunType.Night,
                Preferences.getInstance().getBestPopsAlgorithm(),
                Preferences.getInstance().getBestPopsCriteriaSigma(),
                Preferences.getInstance().getBestPopsCriteriaAverageWeight());

        // ignore night restrictions :
        this.ignoreUseNightLimit = ignoreNightLimits;
    }

    /**
     * Test if the current thread is interrupted: if true then throw an InterruptedJobException
     * @throws InterruptedJobException if the current thread is interrupted
     */
    private void checkInterrupted() throws InterruptedJobException {
        if (this.currentThread.isInterrupted()) {
            throw ije;
        }
    }

    /**
     * Main operation to determine the source observability for a given interferometer configuration
     *
     * @return ObservabilityData container
     */
    public ObservabilityData compute() {
        if (isLogDebug) {
            logger.debug("\n\n--------------------------------------------------------------------------------\n\n");
            logger.debug("compute: {}", this.observation);
        }

        // fast interrupt:
        checkInterrupted();

        // Start the computations :
        final long start = System.nanoTime();

        // Get interferometer / instrument :
        prepareObservation();

        // Define target list :
        final List<Target> targets;
        if (this.doBaseLineLimits) {
            targets = generateTargetsForBaseLineLimits();
        } else {
            targets = this.observation.getTargets();
        }

        // define the targets anyway :
        this.data.setTargets(targets);

        // define site :
        this.sc.defineSite(this.interferometer.getName(), this.interferometer.getPosSph());
        this.sco.defineSite(this.sc);

        // define observation date :
        // find the appropriate night (LST range):
        this.defineObservationRange();

        // fast interrupt:
        checkInterrupted();

        // 2 - Observability per target :
        if (targets == null || targets.isEmpty()) {
            logger.debug("No target defined.");
        } else {
            // Prepare the beams (station / channel / delay line) :
            prepareBeams();

            // Prepare the base line (XYZ vector, wRange) :
            prepareBaseLines();

            // Prepare the pops :
            if (this.hasPops) {
                preparePopCombinations();
            }

            // fast interrupt:
            checkInterrupted();

            // Find the observability intervals for the target list :
            findObservability(targets);

            // dump star visibilities :
            if (isLogDebug) {
                logger.debug("Star observability intervals:");

                for (List<StarObservabilityData> soList : this.data.getMapStarVisibilities().values()) {
                    logger.debug("{}", CollectionUtils.toString(soList));
                }
            }
        }

        // fast interrupt:
        checkInterrupted();

        if (DEBUG_SLOW_SERVICE) {
            TestUtils.busyWait(2000l);

            // fast interrupt:
            checkInterrupted();
        }

        logger.info("compute : duration = {} ms.", 1e-6d * (System.nanoTime() - start));

        if (SHOW_RANGE_FACTORY_STATS) {
            obsCtx.dumpStats();
        }

        return this.data;
    }

    /**
     * Define the observation date and determine the jd bounds (lst0 - lst24 or arround midnight)
     */
    private void defineObservationRange() {

        this.data.setDateCalc(this.sc);

        // define date :
        final XMLGregorianCalendar cal = this.observation.getWhen().getDate();

        // find the julian date corresponding to the LST origin LST=00:00:00 for the given date :
        this.jdLower = this.sc.defineDate(cal.getYear(), cal.getMonth(), cal.getDay());

        // find the julian date corresponding to LST=00:00:00 next day:
        this.jdUpper = this.sc.findJdForLst0(this.jdLower + AstroSkyCalc.LST_DAY_IN_JD);

        if (isLogDebug) {
            logger.debug("jdLst0:  {}", this.jdLower);
            logger.debug("jdLst24: {}", this.jdUpper);
        }

        this.data.setJdMin(this.jdLower);
        this.data.setJdMax(this.jdUpper);

        this.data.setDateMin(jdToDate(this.jdLower));
        this.data.setDateMax(jdToDate(this.jdUpper));

        if (isLogDebug) {
            logger.debug("date min: {}", this.data.getDateMin());
            logger.debug("date max: {}", this.data.getDateMax());
        }

        // fast interrupt:
        checkInterrupted();

        // 1 - Find the day / twlight / night zones :
        if (this.useNightLimit) {
            // initialize night limits anyway:
            this.nightLimits = new ArrayList<Range>(2);
            this.nightOnlyRanges = new ArrayList<Range>(2);

            if (this.doCenterMidnight) {
                final double jdMidnight = this.sc.getJdMidnight();

                if (isLogDebug) {
                    logger.debug("jdMidnight: {}", jdMidnight);
                }

                // adjust the jd bounds :
                this.jdLower = jdMidnight - AstroSkyCalc.HALF_LST_DAY_IN_JD;
                this.jdUpper = jdMidnight + AstroSkyCalc.HALF_LST_DAY_IN_JD;

                if (isLogDebug) {
                    logger.debug("jdLower: {}", this.jdLower);
                    logger.debug("jdUpper: {}", this.jdUpper);
                }

                this.data.setJdMin(this.jdLower);
                this.data.setJdMax(this.jdUpper);

                this.data.setDateMin(jdToDate(this.jdLower));
                this.data.setDateMax(jdToDate(this.jdUpper));

                if (isLogDebug) {
                    logger.debug("date min: {}", this.data.getDateMin());
                    logger.debug("date max: {}", this.data.getDateMax());
                }
            }

            //  sun rise/set with twilight : see NightlyAlmanac
            final AstroAlmanac almanac = this.sc.getAlmanac();

            // get sun times arround midnight (-1 day; + 2 days):
            final List<AstroAlmanacTime> sunTimes = new ArrayList<AstroAlmanacTime>(almanac.getSunTimes());

            // Extract night limits in [LST0 -12; LST0 + 36] and sun intervals in range [jdLower; jdUpper]:
            processSunAlmanach(sunTimes);

            // moon rise/set arround night:
            final List<Range> moonRiseRanges = this.sc.findMoonRiseSet(almanac, this.jdLower, this.jdUpper);

            if (isLogDebug) {
                logger.debug("moonRiseRanges: {}", moonRiseRanges);
            }

            // compute moon illumination over night:
            final List<Range> obsMoonRanges = new ArrayList<Range>(6);
            obsMoonRanges.addAll(this.nightOnlyRanges);
            obsMoonRanges.addAll(moonRiseRanges);

            final List<Range> moonRanges = Range.intersectRanges(obsMoonRanges, 2, obsCtx);

            if (isLogDebug) {
                logger.debug("moonRanges: {}", moonRanges);
            }

            // Moon filters:
            // 1- use ranges in [LST0; LST0 + 24] over night ranges
            // 2- moon Illum in jdLower / jdUpper
            final double moonIllum = this.sc.getMaxMoonIllum(moonRanges);

            this.data.setMoonIllumPercent(100d * moonIllum);

            // recycle ranges & lists:
            obsCtx.recycleRangesAndList(moonRanges);
            obsCtx.recycleRangesAndList(moonRiseRanges);

            if (isLogDebug) {
                logger.debug("moon illum: {}", moonIllum);
            }
        }
    }

    /**
     * Find the observability ranges for the complete list of targets
     * @param targets target list
     */
    private void findObservability(final List<Target> targets) {
        // display baseline, (beams), (pops), DL as information:
        final StringBuffer sb = getBuffer();
        sb.append("Baseline: ");
        for (Beam b : this.beams) {
            sb.append(b.getStation().getName()).append(' ');
        }
        if (!this.interferometer.getChannels().isEmpty() && this.interferometer.getSwitchyard() != null) {
            sb.append("- Beams: ");
            for (Beam b : this.beams) {
                sb.append(b.getChannel().getName()).append(' ');
            }
        }
        if (this.hasPops && (this.popCombinations.size() == 1)) {
            // user defined Pop combination:
            sb.append("- PoPs: ").append(this.popCombinations.get(0).getIdentifier()).append(' ');
        }
        sb.append("- Delaylines: ");
        for (Beam b : this.beams) {
            sb.append(b.getDelayLine().getName()).append(' ');
        }
        addInformation(sb.toString());

        if (this.hasPops) {
            // PoPs : Compatible Mode if no user defined Pop Combination :
            if (targets.size() > 1 && this.popCombinations.size() > 1) {
                // Objective : find the pop combination that maximize the observability of the complete list of target

                // Start the Best Pops algorithm :
                final long start = System.nanoTime();

                final PopCombination bestPopCombination = findCompatiblePoPs(targets);

                // fast interrupt:
                checkInterrupted();

                if (loggerTasks.isDebugEnabled()) {
                    loggerTasks.debug("findCompatiblePoPs : duration = {} ms.", 1e-6d * (System.nanoTime() - start));
                }

                if (bestPopCombination == null) {
                    // no Pop compatible:
                    this.popCombinations.clear();

                    this.bpObsCtx.setPopCombs(new PopCombination[0]);

                } else {
                    this.data.setBestPops(bestPopCombination);

                    // use the user defined PoPs configuration :
                    this.popCombinations.clear();
                    this.popCombinations.add(bestPopCombination);

                    // Use arrays instead of List for performance:
                    this.bpObsCtx.setPopCombs(new PopCombination[1]);
                    this.popCombinations.toArray(this.bpObsCtx.getPopCombs());
                }
            }
        }
        // Start the Best Pops algorithm :
        final long start = System.nanoTime();

        for (Target target : targets) {

            // fast interrupt:
            checkInterrupted();

            findTargetObservability(target);

        } // for Target

        if (loggerTasks.isDebugEnabled()) {
            loggerTasks.debug("findObservability(targets) : duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        }
    }

    /**
     * Find the best Pop combination for the given list of targets
     * @param targets list of targets
     * @return best Pop combination or null if none exists
     */
    @SuppressWarnings("unchecked")
    private PopCombination findCompatiblePoPs(final List<Target> targets) {
        final int nTargets = targets.size();

        // First filter targets that never rise:
        final List<Target> riseTargets = new ArrayList<Target>(nTargets);
        final List<double[]> infoTargets = new ArrayList<double[]>(nTargets); // Ra, Dec, HaElev

        final double jdCenter = jdCenter();
        double[] raDec;
        double precRA, precDEC, haElev;

        for (Target target : targets) {
            // get Target coordinates precessed to jd :
            raDec = this.sco.defineTarget(jdCenter, target.getRADeg(), target.getDECDeg(), target.getPMRA(), target.getPMDEC());

            // precessed target right ascension in decimal hours :
            precRA = raDec[0];

            // precessed target declination in degrees :
            precDEC = raDec[1];

            // Find LST range corresponding to the rise / set of the target :
            haElev = this.sco.getHAForElevation(precDEC, this.minElev);

            // target rise :
            if (haElev > 0d) {
                riseTargets.add(target);
                infoTargets.add(new double[]{precRA, precDEC, haElev});
            }

            // reset current target :
            this.sco.reset();
        }

        final int nRiseTargets = riseTargets.size();

        if (nRiseTargets == 0) {
            // no target observable !
            return null;
        }

        if (isLogDebug) {
            logger.debug("nRiseTargets: {}", nRiseTargets);
        }

        // Find Best Pops:
        final int sizeCb = this.popCombinations.size();

        final Map<String, GroupedPopObservabilityData> popMap = new HashMap<String, GroupedPopObservabilityData>(Math.max(128, sizeCb / 10));

        // enable parallel jobs if many targets and pop combinations:
        // 3T represents 125 pop combinations:
        // note: in multi conf: disable parallelism because jobExecutor is used to compute Observability per configuration
        final int nTh = (!jobExecutor.isWorkerThread() && (sizeCb * nRiseTargets >= 100)) ? jobExecutor.getMaxParallelJob() : 1;

        if (isLogDebug) {
            logger.debug("findCompatiblePoPs: {} rise targets using {} threads", nRiseTargets, nTh);
        }

        // Prepare thread context variables:
        final BestPoPsObservabilityContext[] bpObsCtxThreads = new BestPoPsObservabilityContext[nTh];
        final int[][] nTaskThreads = (SHOW_BEST_POPS_STATS) ? new int[nTh][16] : null; // cache line padding

        for (int t = 0; t < nTh; t++) {
            bpObsCtxThreads[t] = (t == 0) ? this.bpObsCtx : new BestPoPsObservabilityContext(this.bpObsCtx);
        }

        // chunks:
        /*
         * 3T:   125 => 1 chunk
         * 4T:   625 => 1 chunk
         * 5T:  3125 => 1 chunk per thread
         * 6T: 15625 => 1 chunk per thread
         */
        final int nChunks = (sizeCb > 10000) ? Math.min(8, nTh) : (sizeCb > 1000) ? Math.min(4, nTh) : 1;

        final int[] fromThreads = new int[nChunks];
        final int[] endThreads = new int[nChunks];
        final int stepCb = sizeCb / nChunks;

        for (int c = 0; c < nChunks; c++) {
            fromThreads[c] = c * stepCb;
            endThreads[c] = fromThreads[c] + stepCb;
        }
        endThreads[nChunks - 1] = sizeCb;

        if (isLogDebug) {
            logger.debug("findCompatiblePoPs: {} chunks - size = {}", nChunks, stepCb);
        }

        // computation tasks = 1 job per target and chunk (work stealing):
        final Callable<?>[] jobs = new Callable<?>[nRiseTargets * nChunks];

        // create estimate tasks:
        for (int c = 0, j = 0; c < nChunks; c++) {
            // Pop combination chunks:
            final int fromCb = fromThreads[c];
            final int endCb = endThreads[c];

            for (int i = 0; i < nRiseTargets; i++) {
                // target index to be processed by this task:
                final int targetIndex = i;

                jobs[j++] = new Callable<List<PopObservabilityData>>() {
                    /**
                     * Called by the ParallelJobExecutor to perform task computation
                     */
                    @Override
                    public List<PopObservabilityData> call() {

                        // Get thread index to get appropriate thread vars:
                        final int threadIndex = ParallelJobExecutor.currentThreadIndex(nTh);

                        if (SHOW_TASK_STATS) {
                            loggerTasks.debug("Thread[{}]: target {} - from {} to {}", threadIndex, targetIndex, fromCb, endCb);
                        }

                        // If new threads are created, array access may be a problem (thread conflicts):
                        // ConcurrentLinkedQueue could be used to provide obs context switchs !!
                        // or thread local but initialization / cleanup is not simple
                        final BestPoPsObservabilityContext bpObsCtxLocal = bpObsCtxThreads[threadIndex];

                        final Target target = riseTargets.get(targetIndex);
                        final double[] info = infoTargets.get(targetIndex); // Ra, Dec, HaElev

                        // data partitioning so no synchronization required:
                        final List<PopObservabilityData> popDataList = findPoPsForTargetObservability(
                                target, bpObsCtxLocal, info[0], info[1], info[2], fromCb, endCb);

                        // fast interrupt (multi threading):
                        if (Thread.currentThread().isInterrupted()) {
                            return null;
                        }

                        if (SHOW_BEST_POPS_STATS) {
                            nTaskThreads[threadIndex][0]++;
                        }

                        return popDataList;
                    }
                };
            }
        }

        // execute jobs in parallel:
        final List<List<PopObservabilityData>> targetPopDataListResults
                                               = (List<List<PopObservabilityData>>) jobExecutor.forkAndJoin("ObservabilityService.findCompatiblePoPs", jobs, nTh > 1);

        if (SHOW_BEST_POPS_STATS || SHOW_RANGE_FACTORY_STATS) {
            for (int t = 0; t < nTh; t++) {
                if (SHOW_BEST_POPS_STATS) {
                    logger.info("Thread[{}] done: {} processed targets", t, nTaskThreads[t][0]);
                }

                if (SHOW_RANGE_FACTORY_STATS) {
                    bpObsCtxThreads[t].dumpStats();
                }
            }
        }

        // pop data per target
        final Set<String> uniqueTargets = new HashSet<String>(nTargets);
        GroupedPopObservabilityData popMergeData;
        List<PopObservabilityData> flatPopDataList;
        PopObservabilityData popData;
        String key;

        for (final List<PopObservabilityData> targetPopDataList : targetPopDataListResults) {
            // targetPopDataList can be null if the target never rises or is incompatible (skip) :
            if (targetPopDataList != null) {
                uniqueTargets.add(targetPopDataList.get(0).getTargetName());

                // rearrange results :
                for (int i = 0, size = targetPopDataList.size(); i < size; i++) {
                    popData = targetPopDataList.get(i);
                    key = popData.getPopCombination().getIdentifier();

                    popMergeData = popMap.get(key);

                    if (popMergeData == null) {
                        flatPopDataList = new ArrayList<PopObservabilityData>(nRiseTargets);
                        popMergeData = new GroupedPopObservabilityData(popData.getPopCombination(), flatPopDataList);
                        popMap.put(key, popMergeData);
                    } else {
                        flatPopDataList = popMergeData.getPopDataList();
                    }

                    // add result :
                    flatPopDataList.add(popData);
                }
            }
        } // for Target result

        if (popMap.isEmpty()) {
            addWarning("Impossible to find a PoPs combination compatible with any observable target");
            return null;
        }

        // Convert the map to a list :
        final List<GroupedPopObservabilityData> popMergeList = new ArrayList<GroupedPopObservabilityData>(popMap.values());
        targetPopDataListResults.clear();
        popMap.clear();

        // merged results per PoP combination :
        if (isLogDebug) {
            logger.debug("Complete GroupedPopData : {}", CollectionUtils.toString(popMergeList));
        }

        // find the maximum count of observable targets per PoPs combination :
        // This avoids to have no result at all if all targets can not be observed
        // with the same PoPs combination
        int targetSize;
        int maxObsTarget = 0;

        for (GroupedPopObservabilityData pm : popMergeList) {
            targetSize = pm.getPopDataList().size();

            if (targetSize > maxObsTarget) {
                maxObsTarget = targetSize;
            }
        }

        final int nObsTarget = uniqueTargets.size();

        if (isLogDebug) {
            logger.debug("Observable targets : max: {} - total: {}", maxObsTarget, nObsTarget);
        }

        if (maxObsTarget != nObsTarget) {
            final StringBuffer sb = getBuffer();
            sb.append("Impossible to find a PoPs combination compatible with all observable targets (").append(maxObsTarget);
            sb.append(" / ").append(nObsTarget).append(")");
            addWarning(sb.toString());
        }

        // filter to keep only results for all valid targets :
        for (final Iterator<GroupedPopObservabilityData> it = popMergeList.iterator(); it.hasNext();) {
            popMergeData = it.next();

            // Use all possible pop results having at least the good number of targets:
            if (popMergeData.getPopDataList().size() != maxObsTarget) {
                it.remove();
            }
        }

        PopCombination bestPopCombination = null;

        if (!popMergeList.isEmpty()) {
            if (isLogDebug) {
                logger.debug("Filtered GroupedPopData : {}", CollectionUtils.toString(popMergeList));
            }

            // fast interrupt:
            checkInterrupted();

            // Get a new Grouped Best Pops estimator:
            final BestPopsEstimator estimator = getGroupedBestPopsEstimator();

            // estimator to maximize observability for all observable targets :
            for (GroupedPopObservabilityData pm : popMergeList) {
                pm.estimateData(estimator);
            }

            // Sort pop merge data according to its estimator :
            Collections.sort(popMergeList);

            if (isLogDebug) {
                logger.debug("Sorted GroupedPopData: {}", CollectionUtils.toString(popMergeList));
            }

            // maximum length for this Pop Combination :
            final int end = popMergeList.size() - 1;

            final GroupedPopObservabilityData popBestData = popMergeList.get(end);

            if (popMergeList.size() > 1) {
                // Observability Pops results:
                final List<PopCombination> bestPoPList = new ArrayList<PopCombination>(MAX_POPS_IN_WARNING);
                final List<PopCombination> betterPoPList = new ArrayList<PopCombination>(MAX_POPS_IN_WARNING);

                final StringBuilder sbBestPops = new StringBuilder(128);
                final StringBuilder sbBetterPops = new StringBuilder(128);
                sbBestPops.append("Equivalent Best PoPs found: ");

                // find all equivalent pop combinations :
                for (int i = end, n = 0; i >= 0 && n < MAX_POPS_IN_WARNING; i--, n++) {
                    final GroupedPopObservabilityData pm = popMergeList.get(i);
                    if (popBestData.getEstimation() == pm.getEstimation()) {
                        sbBestPops.append(pm.getPopCombination().getIdentifier()).append(' ');
                        bestPoPList.add(pm.getPopCombination());
                    } else {
                        sbBetterPops.append(pm.getPopCombination().getIdentifier()).append(' ');
                        betterPoPList.add(pm.getPopCombination());
                    }
                }

                this.data.setBestPopList(bestPoPList);
                this.data.setBetterPopList(betterPoPList);

                addInformation(sbBestPops.toString());

                if (sbBetterPops.length() > 0) {
                    sbBetterPops.insert(0, "Next good PoPs: ");
                    addInformation(sbBetterPops.toString());
                }
            }

            bestPopCombination = popBestData.getPopCombination();
        }

        return bestPopCombination;
    }

    /**
     * Return the list of Pop Observability Data i.e. for every PoP combination,
     * give the intervals (hour angles) for all base lines compatible with wMin < w(h) < wMax,
     * wMin and wMax are given by wRanges.
     *
     * @param target target to use
     * @param bpObsCtxLocal observability context (local context ie thread)
     * @param precRA precessed RA in degrees
     * @param precDEC precessed DEC in degrees
     * @param haElev rise/set ha
     * @param fromCb index of the first PopCombination to evaluate (inclusive)
     * @param endCb index of the last PopCombination to evaluate (exclusive)
     * 
     * @return list of Pop observability data
     */
    private List<PopObservabilityData> findPoPsForTargetObservability(final Target target,
                                                                      final BestPoPsObservabilityContext bpObsCtxLocal,
                                                                      final double precRA, final double precDEC, final double haElev,
                                                                      final int fromCb, final int endCb) {

        // Note: this method can be called by multiple threads (so ensure thread safety and interruption)
        // For all PoP combinations : find the HA interval merged with the HA Rise/set interval
        // list of observability data associated to a pop combination :
        List<PopObservabilityData> popDataList = null;

        // target rise :
        if (haElev > 0d) {
            // HA Min/Max range:
            final Range haLimits = getTargetHALimits(target, bpObsCtxLocal);

            // rise/set range RESTRICTED by HA Min/Max constraints:
            final Range rangeHARiseSet = bpObsCtxLocal.valueOf(checkHA(haLimits.getMin(), haElev), checkHA(haLimits.getMax(), haElev));

            // update pop estimator related to target:
            bpObsCtxLocal.setPopEstimator(getBestPopsEstimator(target, haElev, bpObsCtxLocal));

            final List<Range> rangesTarget = bpObsCtxLocal.getList();
            rangesTarget.add(rangeHARiseSet);

            if (this.useNightLimit) {
                this.sc.convertJDToHARanges(this.nightLimits, rangesTarget, precRA);
            }

            popDataList = getPopObservabilityData(target.getName(), FastMath.toRadians(precDEC), rangesTarget, true,
                    bpObsCtxLocal, true, fromCb, endCb);

            // recycle ranges & lists:
            bpObsCtxLocal.recycleRangesAndList(rangesTarget);
            // recycle range:
            bpObsCtxLocal.recycleRange(haLimits);

        } else {
            if (isLogDebug) {
                logger.debug("Target never rise: {}", target);
            }
        }

        return popDataList;
    }

    /**
     * Finds the observability ranges for the given target
     * @param target target to use
     */
    private void findTargetObservability(final Target target) {
        final String targetName = target.getName();

        if (isLogDebug) {
            logger.debug("findTargetObservability: target '{}'", targetName);
        }

        final int sizeBL = this.baseLines.size();

        final int listSize = (this.doDetailedOutput) ? (4 + sizeBL) : 1;
        final List<StarObservabilityData> starVisList = new ArrayList<StarObservabilityData>(listSize);
        this.data.addStarVisibilities(targetName, starVisList);

        final StarObservabilityData starObs = new StarObservabilityData(targetName, StarObservabilityData.TYPE_STAR);
        // add the result to have also unobservable targets :
        starVisList.add(starObs);

        final StarData starData = new StarData(target.getName());
        this.data.addStarData(starData);

        // get Target coordinates precessed to JD CENTER and define target to get later az/alt positions from JSkyCalc :
        final double[] raDec = this.sco.defineTarget(jdCenter(), target.getRADeg(), target.getDECDeg(), target.getPMRA(), target.getPMDEC());

        // precessed target right ascension in decimal hours :
        final double precRA = raDec[0];

        // precessed target declination in degrees :
        final double precDEC = raDec[1];

        if (isLogDebug) {
            logger.debug("target[{}] {} - precessed: {}", target.getName(),
                    AstroSkyCalcObservation.asString(target.getRADeg(), target.getDECDeg()),
                    AstroSkyCalcObservation.asString(15d * precRA, precDEC));
        }

        // define transit date (HA = 0):
        starObs.setTransitDate(convertJDToDate(this.sc.convertHAToJD(0d, precRA)));

        // Find LST range corresponding to the rise / set of the target :
        final double haElev = this.sco.getHAForElevation(precDEC, this.minElev);

        // update Star Data :
        starData.setPrecRA(precRA);
        starData.setPrecDEC(precDEC);
        starData.setHaElev(haElev);

        // target rise :
        if (haElev > 0d) {
            final ObservabilityContext ctx = obsCtx;

            // rise/set range WITHOUT HA Min/Max constraints:
            final Range rangeHARiseSet = ctx.valueOf(-haElev, haElev);

            if (isLogDebug) {
                logger.debug("rangeHARiseSet: {}", rangeHARiseSet);
            }

            // HA intervals for every base line:
            List<List<Range>> rangesHABaseLines;

            // VLTI: VCM restrictions:
            final List<DelayLineRestriction> delayLineRestrictions = this.interferometer.getDelayLineRestrictions();
            final int nDLRestrictions = delayLineRestrictions.size();

            // HA intervals for every base line taking into account the custom maximum throw in switchyard (ie per station ~ VLTI VCM limits):
            boolean checkDLMaxThrow = false;
            List<List<Range>> rangesHABaseLinesVcm;
            final List<List<List<Range>>> rangesHABaseLinesVcms = (this.hasSwitchyardDelayLineMaxThrow)
                    ? new ArrayList<List<List<Range>>>(nDLRestrictions) : null;

            if (this.hasPops) {
                // update pop estimator related to target:
                this.bpObsCtx.setPopEstimator(getBestPopsEstimator(target, haElev, ctx));

                // handle here all possible combinations for POPs :
                // keep only the POPs that maximize the DL+rise intersection ...
                final List<Range> rangesTarget = ctx.getList();
                rangesTarget.add(rangeHARiseSet);

                List<Range> nightsLimitsHA = null;
                if (this.useNightLimit) {
                    nightsLimitsHA = ctx.getList();
                    this.sc.convertJDToHARanges(this.nightLimits, nightsLimitsHA, precRA);
                    rangesTarget.addAll(nightsLimitsHA);
                }

                rangesHABaseLines = findHAIntervalsWithPops(FastMath.toRadians(precDEC), rangesTarget, starObs);
                // recycle ranges & lists:
                if (nightsLimitsHA != null) {
                    ctx.recycleRangesAndList(nightsLimitsHA);
                }
                ctx.recycleList(rangesTarget);
                /* 
                 * TODO: handle Pupil handling limits (DL / VCM) for CHARA ? 
                 */
            } else {

                final double[] w = ctx.getW();
                final double[] ha = ctx.getHa();
                final double[] haValues = ctx.getHaValues();

                // Get intervals (HA) compatible with all base lines:
                rangesHABaseLines = DelayLineService.findHAIntervals(FastMath.toRadians(precDEC),
                        this.baseLines, this.wRanges, ha, haValues, w, ctx);

                // rangesHABaseLines can be null if the thread was interrupted :
                checkInterrupted();

                if (this.hasSwitchyardDelayLineMaxThrow) {
                    List<Range> wRangesVcm;

                    for (int k = 0; k < nDLRestrictions; k++) {
                        wRangesVcm = this.wRangesVcms.get(k);
                        rangesHABaseLinesVcm = null;

                        if (!wRangesVcm.isEmpty()) {
                            // Get intervals (HA) compatible with all base lines using the DL throw for this VCM limit:
                            rangesHABaseLinesVcm = DelayLineService.findHAIntervals(FastMath.toRadians(precDEC),
                                    this.baseLines, wRangesVcm, ha, haValues, w, ctx);

                            // rangesHABaseLinesVcm can be null if the thread was interrupted :
                            checkInterrupted();
                        }

                        rangesHABaseLinesVcms.add(rangesHABaseLinesVcm);

                        if (!checkDLMaxThrow) {
                            boolean doWarn = (wRangesVcm.isEmpty());

                            if (!doWarn) {
                                // Only do checks if some ranges are different for at least 1 delay line:
                                List<Range> ranges, rangesVcm;

                                for (int i = 0; i < sizeBL; i++) {
                                    ranges = rangesHABaseLines.get(i);
                                    rangesVcm = rangesHABaseLinesVcm.get(i);

                                    if (!Range.equals(ranges, rangesVcm)) {
                                        doWarn = true;
                                        break;
                                    }
                                }
                            }

                            if (doWarn) {
                                checkDLMaxThrow = true;

                                final StringBuffer sb = getBuffer();
                                sb.append("Pupil correction problem: ").append(delayLineRestrictions.get(k).getDescription());
                                sb.append(" pressure limit exceeded.");
                                this.addWarning(sb.toString());
                            }
                        }
                    }
                }
            }

            // rangesHABaseLines / rangesHABaseLinesVcmLow / rangesHABaseLinesVcmHigh can be null if the thread was interrupted :
            checkInterrupted();

            // convert HA range to JD range in range [LST0 - 12; LST0 + 36]
            final Range rangeJDRiseSet = this.sc.convertHAToJDRange(rangeHARiseSet, precRA, ctx);
            // rise/set range as list:
            final List<Range> rangesJDRiseSet = ctx.getList();
            rangesJDRiseSet.add(rangeJDRiseSet);

            // For now : only VLTI/CHARA has horizon profiles :
            boolean checkJDHorizon = false;
            List<Range> rangesJDHorizon = null;
            if (this.hasHorizon) {
                // check horizon profiles inside rise/set range :
                rangesJDHorizon = checkHorizonProfile(precDEC, rangeJDRiseSet);

                if (isLogDebug) {
                    logger.debug("rangesJDHz: {}", rangesJDHorizon);
                }

                // fast interrupt:
                checkInterrupted();

                if (rangesJDHorizon == null) {
                    // means JD Rise/Set:
                    rangesJDHorizon = rangesJDRiseSet;
                } else {
                    checkJDHorizon = true;
                }
            }

            // Check Moon restriction
            boolean checkJDMoon = false;
            List<Range> rangesJDMoon = null;
            if (this.useNightLimit && this.moonPointingRestriction != null) {
                rangesJDMoon = checkMoonRestriction(target, precDEC, rangeJDRiseSet);

                if (isLogDebug) {
                    logger.debug("rangesJDMoon: {}", rangesJDMoon);
                }

                // fast interrupt:
                checkInterrupted();

                if (rangesJDMoon == null) {
                    // means JD Rise/Set:
                    rangesJDMoon = rangesJDRiseSet;
                } else {
                    checkJDMoon = true;
                }
            }

            // Check wind restriction:
            boolean checkJDWind = false;
            List<Range> rangesJDWind = null;
            if (this.hasWindRestriction) {
                // check target azimuth inside rise/set range :
                rangesJDWind = checkWindRestriction(precDEC, rangeJDRiseSet);

                if (isLogDebug) {
                    logger.debug("rangesJDWind: {}", rangesJDWind);
                }

                // fast interrupt:
                checkInterrupted();

                if (rangesJDWind == null) {
                    // means JD Rise/Set:
                    rangesJDWind = rangesJDRiseSet;
                } else {
                    checkJDWind = true;
                }
            }

            // observable ranges (jd) :
            if (tmpRanges == null) {
                tmpRanges = new ArrayList<Range>(sizeBL * 2 + 5);
            }
            final ArrayList<Range> obsRanges = tmpRanges;
            int nValid;

            if (this.doDetailedOutput) {
                // Add Rise/Set :
                final StarObservabilityData soRiseSet = new StarObservabilityData(targetName, "Rise/Set", StarObservabilityData.TYPE_RISE_SET);
                // get target position (ha, az, el) at range boundaries:
                getTargetPosition(soRiseSet, rangesJDRiseSet, precRA, precDEC, false);
                starVisList.add(soRiseSet);

                // convert JD ranges to date ranges :
                convertRangesToDateIntervals(rangesJDRiseSet, soRiseSet.getVisible());

                if (rangesJDHorizon != null) {
                    // Add Horizon :
                    final StarObservabilityData soHorizon = new StarObservabilityData(targetName, "Horizon", StarObservabilityData.TYPE_HORIZON);
                    // get target position (ha, az, el) at range boundaries:
                    getTargetPosition(soHorizon, rangesJDHorizon, precRA, precDEC, false);
                    starVisList.add(soHorizon);

                    // convert JD ranges to date ranges :
                    convertRangesToDateIntervals(rangesJDHorizon, soHorizon.getVisible());
                }

                if (rangesJDMoon != null) {
                    // Add Moon separation :
                    final StarObservabilityData soMoon = new StarObservabilityData(targetName, "Moon Sep.", StarObservabilityData.TYPE_MOON_DIST);
                    // get target position (ha, az, el) at range boundaries:
                    getTargetPosition(soMoon, rangesJDMoon, precRA, precDEC, false);
                    starVisList.add(soMoon);

                    // convert JD ranges to date ranges :
                    convertRangesToDateIntervals(rangesJDMoon, soMoon.getVisible());
                }

                if (rangesJDWind != null) {
                    // Add Horizon :
                    final StarObservabilityData soWind = new StarObservabilityData(targetName, "Wind", StarObservabilityData.TYPE_WIND);
                    // get target position (ha, az, el) at range boundaries:
                    getTargetPosition(soWind, rangesJDWind, precRA, precDEC, false);
                    starVisList.add(soWind);

                    // convert JD ranges to date ranges :
                    convertRangesToDateIntervals(rangesJDWind, soWind.getVisible());
                }

                // Add ranges per BL :
                if (!rangesHABaseLines.isEmpty()) {
                    BaseLine baseLine;
                    List<Range> ranges;
                    StarObservabilityData soBl;
                    final List<Range> jdRangesBL = ctx.getList();
                    final List<Range> jdRangesVcm = ctx.getList();
                    List<String> vcmLimits;
                    List<List<DateTimeInterval>> visibleVcmLimits;
                    List<Range> vcmCompatibleRanges, vcmComplementRanges;

                    for (int i = 0; i < sizeBL; i++) {
                        baseLine = this.baseLines.get(i);
                        ranges = rangesHABaseLines.get(i);

                        obsRanges.clear();
                        jdRangesBL.clear();
                        this.sc.convertHAToJDRanges(ranges, jdRangesBL, precRA, ctx);
                        obsRanges.addAll(jdRangesBL);

                        if (isLogDebug) {
                            logger.debug("baseLine: {}", baseLine);
                            logger.debug("JD ranges: {}", obsRanges);
                        }

                        soBl = new StarObservabilityData(targetName, baseLine.getName(), StarObservabilityData.TYPE_BASE_LINE + i);
                        // get target position (ha, az, el) at range boundaries:
                        getTargetPosition(soBl, obsRanges, precRA, precDEC, false);
                        starVisList.add(soBl);

                        // convert JD ranges to date ranges :
                        convertRangesToDateIntervals(obsRanges, soBl.getVisible());

                        if (isLogDebug) {
                            logger.debug("Date ranges: {}", soBl.getVisible());
                        }

                        // Show VCM limits as overlay:
                        if (checkDLMaxThrow) {
                            visibleVcmLimits = new ArrayList<List<DateTimeInterval>>(3);
                            vcmLimits = new ArrayList<String>(3);

                            for (int k = 0; k < nDLRestrictions; k++) {
                                vcmCompatibleRanges = null;

                                // Get Vcm ranges per baseline:
                                rangesHABaseLinesVcm = rangesHABaseLinesVcms.get(k);

                                // may be null:
                                if (rangesHABaseLinesVcm != null) {
                                    ranges = rangesHABaseLinesVcm.get(i);

                                    // Restrict observability ranges:
                                    nValid = 1;
                                    obsRanges.clear();
                                    obsRanges.addAll(jdRangesBL);

                                    // flatten and convert HA ranges to JD range:
                                    jdRangesVcm.clear();
                                    this.sc.convertHAToJDRanges(ranges, jdRangesVcm, precRA, ctx);
                                    obsRanges.addAll(jdRangesVcm);
                                    nValid += 1;

                                    vcmCompatibleRanges = ctx.intersectRanges(obsRanges, nValid);
                                    // recycle ranges:
                                    ctx.recycleRanges(jdRangesVcm);
                                }
                                if (isLogDebug) {
                                    logger.debug("vcmCompatibleRanges: {}", vcmCompatibleRanges);
                                }

                                if (vcmCompatibleRanges != null) {
                                    // Find observability complement ie substract vcm ranges to observability ranges:
                                    nValid = 1;
                                    obsRanges.clear();
                                    obsRanges.addAll(jdRangesBL);
                                    obsRanges.addAll(vcmCompatibleRanges);

                                    // may be null if no complement ie vcm ranges = observability ranges:
                                    vcmComplementRanges = ctx.intersectRanges(obsRanges, nValid);
                                    // recycle ranges:
                                    ctx.recycleRangesAndList(vcmCompatibleRanges);
                                } else {
                                    // no compatible range ie use full ranges:
                                    vcmComplementRanges = jdRangesBL;
                                }
                                if (isLogDebug) {
                                    logger.debug("vcmComplementRanges: {}", vcmComplementRanges);
                                }

                                if (vcmComplementRanges != null) {
                                    // Keep observability ranges with VCM Low restrictions:

                                    // get target position (ha, az, el) at range boundaries:
                                    getTargetPosition(soBl, vcmComplementRanges, precRA, precDEC, false);

                                    // convert JD ranges to date ranges :
                                    final List<DateTimeInterval> visibleVcmLowLimits = new ArrayList<DateTimeInterval>(3);
                                    convertRangesToDateIntervals(vcmComplementRanges, visibleVcmLowLimits);

                                    // recycle ranges:
                                    if (vcmComplementRanges != jdRangesBL) {
                                        ctx.recycleRangesAndList(vcmComplementRanges);
                                    }

                                    vcmLimits.add(delayLineRestrictions.get(k).getDescription());
                                    visibleVcmLimits.add(visibleVcmLowLimits);
                                }
                            }

                            if (!vcmLimits.isEmpty()) {
                                soBl.setVisibleVcmLimits(vcmLimits, visibleVcmLimits);
                            }
                        }
                        // recycle ranges:
                        ctx.recycleRanges(jdRangesBL);
                    }
                    // recycle ranges & lists:
                    ctx.recycleList(jdRangesBL);
                    ctx.recycleList(jdRangesVcm);
                }
            }

            final List<Range> finalRangesHardLimits;

            if ((sizeBL != 0) && rangesHABaseLines.isEmpty()) {
                finalRangesHardLimits = null;
            } else {
                // Merge then all JD intervals :
                // nValid = nBL [dl] + 1 [rise or horizon] + 1 if night limits
                nValid = sizeBL + 1;
                obsRanges.clear();

                // flatten and convert HA ranges to JD range :
                final List<Range> rangesJDBaseLines = ctx.getList();
                this.sc.convertHAToJDRangesList(rangesHABaseLines, rangesJDBaseLines, precRA, ctx);
                obsRanges.addAll(rangesJDBaseLines);

                // add horizon (including rise/set):
                obsRanges.addAll(rangesJDHorizon);

                // Intersect with night limits:
                if (this.useNightLimit) {
                    obsRanges.addAll(this.nightLimits);
                    nValid++;
                }

                if (isLogDebug) {
                    logger.debug("obsRanges: {}", obsRanges);
                }

                // fast interrupt:
                checkInterrupted();

                // finally : merge intervals :
                finalRangesHardLimits = ctx.intersectRanges(obsRanges, nValid);
                // recycle ranges & lists:
                ctx.recycleRangesAndList(rangesJDBaseLines);
            }

            // recycle ranges:
            ctx.recycleAll(rangesHABaseLines);
            if (checkJDHorizon) {
                ctx.recycleRangesAndList(rangesJDHorizon);
            }

            if (isLogDebug) {
                logger.debug("finalRangesHardLimits: {}", finalRangesHardLimits);
            }

            // store merge result as date intervals:
            if (finalRangesHardLimits != null) {

                // Show VCM limits as overlay:
                if (checkDLMaxThrow) {
                    final List<Range> jdRangesVcm = ctx.getList();
                    final List<List<DateTimeInterval>> visibleVcmLimits = new ArrayList<List<DateTimeInterval>>(3);
                    final List<String> vcmLimits = new ArrayList<String>(3);

                    List<Range> vcmCompatibleRanges, vcmComplementRanges;

                    for (int k = 0; k < nDLRestrictions; k++) {
                        vcmCompatibleRanges = null;

                        // Get Vcm ranges per baseline:
                        rangesHABaseLinesVcm = rangesHABaseLinesVcms.get(k);

                        // may be null:
                        if (rangesHABaseLinesVcm != null) {
                            // Restrict observability ranges:
                            nValid = 1;
                            obsRanges.clear();
                            obsRanges.addAll(finalRangesHardLimits);

                            // flatten and convert HA ranges to JD range:
                            jdRangesVcm.clear();
                            this.sc.convertHAToJDRangesList(rangesHABaseLinesVcm, jdRangesVcm, precRA, ctx);
                            obsRanges.addAll(jdRangesVcm);
                            nValid += sizeBL;

                            vcmCompatibleRanges = ctx.intersectRanges(obsRanges, nValid);
                            // recycle ranges:
                            ctx.recycleRanges(jdRangesVcm);
                        }
                        if (isLogDebug) {
                            logger.debug("vcmCompatibleRanges: {}", vcmCompatibleRanges);
                        }

                        if (vcmCompatibleRanges != null) {
                            // Find observability complement ie substract vcm ranges to observability ranges:
                            nValid = 1;
                            obsRanges.clear();
                            obsRanges.addAll(finalRangesHardLimits);
                            obsRanges.addAll(vcmCompatibleRanges);

                            // may be null if no complement ie vcm ranges = observability ranges:
                            vcmComplementRanges = ctx.intersectRanges(obsRanges, nValid);
                            // recycle ranges & list:
                            ctx.recycleRangesAndList(vcmCompatibleRanges);

                        } else {
                            // no compatible range ie use full ranges:
                            vcmComplementRanges = finalRangesHardLimits;
                        }
                        if (isLogDebug) {
                            logger.debug("vcmComplementRanges: {}", vcmComplementRanges);
                        }

                        if (vcmComplementRanges != null) {
                            // Keep observability ranges with VCM Low restrictions:

                            // get target position (ha, az, el) at range boundaries:
                            getTargetPosition(starObs, vcmComplementRanges, precRA, precDEC, false);

                            final List<DateTimeInterval> visibleVcmLowLimits = new ArrayList<DateTimeInterval>(3);

                            // convert JD ranges to date ranges :
                            convertRangesToDateIntervals(vcmComplementRanges, visibleVcmLowLimits);
                            // recycle ranges & list:
                            if (vcmComplementRanges != finalRangesHardLimits) {
                                ctx.recycleRangesAndList(vcmComplementRanges);
                            }

                            vcmLimits.add(delayLineRestrictions.get(k).getDescription());
                            visibleVcmLimits.add(visibleVcmLowLimits);
                        }
                    }

                    if (!vcmLimits.isEmpty()) {
                        starObs.setVisibleVcmLimits(vcmLimits, visibleVcmLimits);
                    }

                    // recycle ranges & lists:
                    ctx.recycleList(jdRangesVcm);
                }

                // fast interrupt:
                checkInterrupted();

                // rise/set range WITHOUT HA Min/Max constraints:
                final Range haLimits = getTargetHALimits(target, ctx);

                final boolean doSoftLimits = checkJDMoon || checkJDWind || rangeHARiseSet.contains(haLimits.getMin()) || rangeHARiseSet.contains(haLimits.getMax());

                if (isLogDebug) {
                    logger.debug("checkJDMoon: {}", checkJDMoon);
                    logger.debug("checkJDWind: {}", checkJDWind);
                    logger.debug("doSoftLimits: {}", doSoftLimits);
                }

                final List<Range> finalRanges;

                if (doSoftLimits) {
                    // Restrict observability ranges:
                    nValid = 1;
                    obsRanges.clear();
                    obsRanges.addAll(finalRangesHardLimits);

                    // convert HA Limits to JD range in range [LST0 - 12; LST0 + 36]
                    final Range rangeJDHALimits = this.sc.convertHAToJDRange(haLimits, precRA, ctx);
                    obsRanges.add(rangeJDHALimits);
                    nValid++;

                    // Intersect with moon separation ranges :
                    if (checkJDMoon) {
                        obsRanges.addAll(rangesJDMoon);
                        nValid++;
                    }

                    // Intersect with wind ranges :
                    if (checkJDWind) {
                        obsRanges.addAll(rangesJDWind);
                        nValid++;
                    }

                    // fast interrupt:
                    checkInterrupted();

                    final List<Range> restrictedRanges = ctx.intersectRanges(obsRanges, nValid);
                    if (isLogDebug) {
                        logger.debug("restrictedRanges: {}", restrictedRanges);
                    }
                    // recycle range:
                    ctx.recycleRange(rangeJDHALimits);

                    if (Range.equals(finalRangesHardLimits, restrictedRanges)) {
                        finalRanges = finalRangesHardLimits;
                        // recycle ranges & list:
                        ctx.recycleRangesAndList(restrictedRanges);
                    } else {
                        if (restrictedRanges == null) {
                            finalRanges = DelayLineService.EMPTY_RANGE_LIST;
                        } else {
                            finalRanges = restrictedRanges;
                        }

                        // get target position (ha, az, el) at range boundaries:
                        getTargetPosition(starObs, finalRangesHardLimits, precRA, precDEC, false);

                        // Keep observability ranges without HA restrictions:
                        final List<DateTimeInterval> visibleNoSoftLimits = new ArrayList<DateTimeInterval>(3);

                        // convert JD ranges to date ranges :
                        convertRangesToDateIntervals(finalRangesHardLimits, visibleNoSoftLimits);

                        // TODO: define soft limit description (moon, wind or HA limits)
                        starObs.setVisibleNoSoftLimits(visibleNoSoftLimits);
                    }

                } else {
                    finalRanges = finalRangesHardLimits;
                }
                // recycle range:
                ctx.recycleRange(haLimits);

                // fast interrupt:
                checkInterrupted();

                // get detailled target position (ha, az, el) for the current target (ticks):
                getTargetPosition(starObs, finalRanges, precRA, precDEC, true);

                // convert JD ranges to date ranges :
                convertRangesToDateIntervals(finalRanges, starObs.getVisible());

                // update Star Data :
                final List<Range> haObsRanges = new ArrayList<Range>(2);
                this.sc.convertJDToHARanges(finalRanges, haObsRanges, precRA);
                if (isLogDebug) {
                    logger.debug("HA observability: {}", haObsRanges);
                }
                if (!haObsRanges.isEmpty()) {
                    starData.setObsRangesHA(haObsRanges);
                }

                // recycle ranges & lists:
                if (finalRanges != finalRangesHardLimits) {
                    ctx.recycleRangesAndList(finalRanges);
                }
                ctx.recycleRangesAndList(finalRangesHardLimits);

            } else {
                if (isLogDebug) {
                    logger.debug("Target not observable: {}", target);
                }
                final StringBuffer sb = getBuffer();
                sb.append("Target [").append(targetName).append("] is not observable");
                addInformation(sb.toString());
            }

            // recycle ranges:
            ctx.recycleRange(rangeHARiseSet);
            // recycle ranges & lists:
            if (checkJDMoon) {
                ctx.recycleRangesAndList(rangesJDMoon);
            }
            if (checkJDWind) {
                ctx.recycleRangesAndList(rangesJDWind);
            }
            ctx.recycleRangesAndList(rangesJDRiseSet);
            if (rangesHABaseLinesVcms != null) {
                for (int k = 0; k < nDLRestrictions; k++) {
                    ctx.recycleAll(rangesHABaseLinesVcms.get(k));
                }
            }

        } else {
            if (isLogDebug) {
                logger.debug("Target never rise: {}", target);
            }
            final StringBuffer sb = getBuffer();
            sb.append("Target [").append(targetName).append("] is not observable (never rise)");
            addInformation(sb.toString());
        }

        // reset current target :
        this.sco.reset();
    }

    /**
     * Return the intervals (hour angles) for all base lines compatible with wMin < w(h) < wMax,
     * wMin and wMax are given by wRanges.
     *
     * This method is similar to DelayLineService.findHAIntervals(...) but it takes into account the PoPs i.e.
     * it finds the best PoP combination to maximize the HA interval (delay line + rise/set)
     *
     * @param dec target declination (rad)
     * @param rangesTarget HA ranges for target rise/set and night limits
     * @param starObs star observability bean to set the final PoP combination
     * @return intervals (hour angles) or null if thread interrupted
     */
    private List<List<Range>> findHAIntervalsWithPops(final double dec, final List<Range> rangesTarget, final StarObservabilityData starObs) {
        final int sizeCb = this.popCombinations.size();

        // flag to have extra information about DL (even if unobservable target):
        final boolean doSkipDL = !this.doDetailedOutput;

        // First Pass :
        // For all PoP combinations : find the HA interval merged with the HA Rise/set interval
        // list of observability data associated to a pop combination :
        final List<PopObservabilityData> popDataList = getPopObservabilityData(starObs.getTargetName(), dec, rangesTarget, doSkipDL, this.bpObsCtx, false);

        // Current pop observability :
        PopObservabilityData popData;

        // Find the PoP combination that gives the longest HA interval :
        if (popDataList != null) {

            if (sizeCb > 1) {
                // Sort pop observability data according to estimation (weighted max length):
                Collections.sort(popDataList);

                if (isLogDebug) {
                    logger.debug("Sorted PopData: {}", CollectionUtils.toString(popDataList));
                }
            }

            // maximum length for this Pop Combination :
            final int end = popDataList.size() - 1;

            final PopObservabilityData popBestData = popDataList.get(end);

            if (sizeCb > 1) {
                // Observability Pops results:
                final List<PopCombination> bestPoPList = new ArrayList<PopCombination>(MAX_POPS_IN_WARNING);
                final List<PopCombination> betterPoPList = new ArrayList<PopCombination>(MAX_POPS_IN_WARNING);

                final StringBuilder sbBestPops = new StringBuilder(128);
                final StringBuilder sbBetterPops = new StringBuilder(128);
                sbBestPops.append("Equivalent Best PoPs found: ");

                // find all equivalent pop combinations :
                for (int i = end, n = 0; i >= 0 && n < MAX_POPS_IN_WARNING; i--, n++) {
                    popData = popDataList.get(i);
                    if (popBestData.getMaxLength() == popData.getMaxLength()) {
                        sbBestPops.append(popData.getPopCombination().getIdentifier()).append(' ');
                        bestPoPList.add(popData.getPopCombination());
                    } else {
                        sbBetterPops.append(popData.getPopCombination().getIdentifier()).append(' ');
                        betterPoPList.add(popData.getPopCombination());
                    }
                }

                this.data.setBestPopList(bestPoPList);
                this.data.setBetterPopList(betterPoPList);

                addInformation(sbBestPops.toString());

                if (sbBetterPops.length() > 0) {
                    sbBetterPops.insert(0, "Next good PoPs: ");
                    addInformation(sbBetterPops.toString());
                }
            }

            if (this.data.getBestPops() == null) {
                this.data.setBestPops(popBestData.getPopCombination());
            }

            return popBestData.getRangesBL();
        }
        return Collections.emptyList();
    }

    /**
     * Return the list of Pop Observability Data i.e. for every PoP combination,
     * give the intervals (hour angles) for all base lines compatible with wMin < w(h) < wMax,
     * wMin and wMax are given by wRanges.
     *
     * This method is similar to DelayLineService.findHAIntervals(...) but it takes into account the PoPs i.e.
     * it finds the best PoP combination to maximize the HA interval (delay line + rise/set)
     *
     * @param targetName name of the target
     * @param dec target declination (rad)
     * @param rangesTarget HA ranges for target rise/set and night limits
     * @param doSkipDL skip pop combination if any DL is unobservable (useful for performance and detailed output)
     * @param obsCtxLocal observability context (local context ie thread)
     * @param clearRangesBL true to free rangesBL; false otherwise
     * @return list of Pop Observability Data or null if thread interrupted
     */
    private List<PopObservabilityData> getPopObservabilityData(final String targetName, final double dec, final List<Range> rangesTarget,
                                                               final boolean doSkipDL, final BestPoPsObservabilityContext obsCtxLocal, final boolean clearRangesBL) {
        return getPopObservabilityData(targetName, dec, rangesTarget, doSkipDL, obsCtxLocal, clearRangesBL, 0, obsCtxLocal.getPopCombs().length);
    }

    /**
     * Return the list of Pop Observability Data i.e. for every PoP combination,
     * give the intervals (hour angles) for all base lines compatible with wMin < w(h) < wMax,
     * wMin and wMax are given by wRanges.
     *
     * This method is similar to DelayLineService.findHAIntervals(...) but it takes into account the PoPs i.e.
     * it finds the best PoP combination to maximize the HA interval (delay line + rise/set)
     *
     * @param targetName name of the target
     * @param dec target declination (rad)
     * @param rangesTarget HA ranges for target rise/set and night limits
     * @param doSkipDL skip pop combination if any DL is unobservable (useful for performance and detailed output)
     * @param bpObsCtxLocal observability context (local context ie thread)
     * @param clearRangesBL true to free rangesBL; false otherwise
     * @param fromCb index of the first PopCombination to evaluate (inclusive)
     * @param endCb index of the last PopCombination to evaluate (exclusive)
     * @return list of Pop Observability Data or null if thread interrupted
     */
    private List<PopObservabilityData> getPopObservabilityData(final String targetName, final double dec, final List<Range> rangesTarget,
                                                               final boolean doSkipDL, final BestPoPsObservabilityContext bpObsCtxLocal, final boolean clearRangesBL,
                                                               final int fromCb, final int endCb) {

        // Note: this method can be called by multiple threads (so ensure thread safety and interruption)
        final long start = System.nanoTime();

        // local vars for performance:
        final boolean isDebug = isLogDebug;
        final Thread currentTh = Thread.currentThread(); // multi threading
        final BestPopsEstimator estimator = bpObsCtxLocal.getPopEstimator();
        // list of observability data associated to a pop combination :
        final List<PopObservabilityData> popDataList = bpObsCtxLocal.getPopDataList();

        final double cosDec = FastMath.cos(dec);
        final double sinDec = FastMath.sin(dec);

        // Use arrays instead of List for performance:
        final PopCombination[] popCombs = bpObsCtxLocal.getPopCombs();
        final BaseLine[] bls = bpObsCtxLocal.getBaseLines();
        final Range[] wRangeArray = bpObsCtxLocal.getWRanges();

        final int sizeBL = bls.length;

        // precompute W extrema per baseline:
        final double[] w = bpObsCtxLocal.getW();
        final double[][] wExtremas = new double[sizeBL][2];

        // For every Base Line :
        for (int i = 0; i < sizeBL; i++) {
            final BaseLine bl = bls[i];

            final double[] wExtrema = DelayLineService.findWExtrema(cosDec, sinDec, bl, w);

            if (isDebug) {
                logger.debug("wExtrema[{}] = {}", bl.getName(), Arrays.toString(wExtrema));
            }

            wExtremas[i] = (wExtrema != null) ? Arrays.copyOf(wExtrema, 2) : null;
        }

        // fast interrupt (multi threading):
        if (currentTh.isInterrupted()) {
            return null;
        }

        // For all PoP combinations : find the HA interval merged with the HA Rise/set interval
        // Current pop observability :
        PopObservabilityData popData;

        // list of HA ranges for all base lines :
        final List<List<Range>> rangesBL = new ArrayList<List<Range>>(sizeBL);

        // Current list of HA ranges for a base line :
        List<Range> ranges;

        // w range using the pop offset for a given base line :
        final Range wRangeWithOffset = new Range();

        final double[] ha = bpObsCtxLocal.getHa();
        final double[] haValues = bpObsCtxLocal.getHaValues();

        PopCombination popComb;
        Range wRange;
        double[] popOffsets;
        double offset;

        final int nValid = sizeBL + 1 + ((this.useNightLimit) ? 1 : 0);

        // flag to skip DL evaluation when the target is not observable for at least one DL
        boolean skip;

        int nIter = 0;

        // For every Pop Combination in chunk[fromCb; endCb]:
        for (int i, k = fromCb; k < endCb; k++) {
            popComb = popCombs[k];
            popOffsets = popComb.getPopOffsets();

            // fast interrupt (multi threading):
            if (currentTh.isInterrupted()) {
                return null;
            }

            skip = false;

            // For every Base Line :
            for (i = 0; i < sizeBL; i++) {
                wRange = wRangeArray[i];
                offset = popOffsets[i];

                // adjust w range with the current pop combination's Offset :
                wRangeWithOffset.set(wRange.getMin() + offset, wRange.getMax() + offset);

                ranges = DelayLineService.findHAIntervalsForBaseLine(cosDec, sinDec, bls[i], wExtremas[i], wRangeWithOffset, ha, haValues, bpObsCtxLocal);

                if (ranges.isEmpty() && doSkipDL) {
                    // this base line is incompatible with that W range :
                    skip = true;
                    break;
                }
                rangesBL.add(ranges);
            }

            if (skip) {
                if (!rangesBL.isEmpty()) {
                    // recycle memory (to avoid GC):
                    bpObsCtxLocal.recycleAll(rangesBL);
                }
            } else {
                if (SHOW_BEST_POPS_STATS) {
                    nIter++;
                }

                // note : rangesTarget contains both rise/set intervals + night limits in HA
                bpObsCtxLocal.resetAndAddInFlatRangeLimits(rangesTarget);

                // merge the baseline ranges with the target intervals to estimate observability:
                popData = PopObservabilityData.estimate(targetName, popComb, rangesBL, nValid, bpObsCtxLocal, estimator, doSkipDL, clearRangesBL);

                if (popData != null) {
                    // skip pop solutions outside Rise/Set HA range:
                    popDataList.add(popData);
                }
            }

            rangesBL.clear();
        }

        if (SHOW_BEST_POPS_STATS && nIter > 1) {
            loggerTasks.info("getPopObservabilityData - iter = {} - size= {} : duration = {} ms.", nIter, popDataList.size(), 1e-6d * (System.nanoTime() - start));
        }

        if (popDataList.isEmpty()) {
            return null;
        }

        // as popDataList belongs to the observability context, return a list copy:
        final List<PopObservabilityData> popDataListResult = new ArrayList<PopObservabilityData>(popDataList);
        popDataList.clear();

        return popDataListResult;
    }

    /**
     * Check the horizon profiles for all stations given the target rise/set range (JD)
     * @param precDEC precessed DEC in degrees
     * @param jdRiseSet target rise/set range (JD)
     * @return list of observable ranges (no obstruction)
     */
    private List<Range> checkHorizonProfile(final double precDEC, final Range jdRiseSet) {

        // Note: as JD ranges are in [LST0 -12; LST0 + 36], sampled jds are fixed by getJDInLstRange(jd) 
        // in LST range [0; 24] in order to have accurate target position
        final boolean isDebug = isLogDebug; // local var

        // output :
        final ObservabilityContext ctx = obsCtx;
        List<Range> ranges = ctx.getList();

        // Prepare profiles :
        final HorizonService hs = HorizonService.getInstance();

        final int nBeams = this.beams.size();
        final HorizonShape[] profiles = new HorizonShape[nBeams];

        for (int i = 0; i < nBeams; i++) {
            final Beam b = this.beams.get(i);
            profiles[i] = hs.getProfile(this.interferometer.getName(), b.getStation());
        }

        // prepare cosDec/sinDec:
        final double dec = FastMath.toRadians(precDEC);
        final double cosDec = FastMath.cos(dec);
        final double sinDec = FastMath.sin(dec);

        final double jdMin = jdRiseSet.getMin();
        final double jdMax = jdRiseSet.getMax();

        final AzEl azEl = new AzEl();
        HorizonShape profile;
        boolean visible;
        boolean last = false;

        Range range = ctx.valueOf(0d, 0d);

        for (double jd = jdMin, jdIn; jd < jdMax; jd += JD_STEP) {

            // fast interrupt:
            checkInterrupted();

            // fix JD in LST range [0; 24] in order to have accurate target position:
            jdIn = getJDInLstRange(jd);

            this.sco.getTargetPosition(cosDec, sinDec, jdIn, azEl);

            visible = true;

            // For every beam (station) :
            // check if there is no horizon obstruction :
            for (int i = 0; i < nBeams; i++) {
                profile = profiles[i];

                if (!hs.checkProfile(profile, azEl.getAzimuth(), azEl.getElevation())) {
                    visible = false;

                    if (isDebug) {
                        logger.debug("Target hidden by horizon profile = {} [{} {}]",
                                profile.getName(), azEl.getAzimuth(), azEl.getElevation());
                    }

                    break;
                }
            }

            // manage intervals :
            if (visible) {
                if (!last) {
                    last = true;
                    // start point
                    range.setMin(jd);
                }
            } else {
                if (last) {
                    last = false;
                    // end point
                    range.setMax(jd);
                    ranges.add(range);
                    range = ctx.valueOf(0d, 0d);
                }
            }
        } // jd

        // close last interval if opened :
        if (range.getMin() > 0d) {
            range.setMax(jdMax);
            ranges.add(range);
        } else {
            // recycle range:
            ctx.recycleRange(range);
        }
        if (ranges.isEmpty()) {
            // recycle list:
            ctx.recycleList(ranges);
            ranges = DelayLineService.EMPTY_RANGE_LIST;
        } else if (ranges.size() == 1 && jdRiseSet.equals(ranges.get(0))) {
            // recycle ranges & list:
            ctx.recycleRangesAndList(ranges);
            // no restriction:
            return null;
        }

        return ranges;
    }

    /**
     * Check the wind restriction given the target rise/set range (JD)
     * @param precDEC precessed DEC in degrees
     * @param jdRiseSet target rise/set range (JD)
     * @return list of observable ranges (no obstruction) or null if no restriction
     */
    private List<Range> checkWindRestriction(final double precDEC, final Range jdRiseSet) {

        // Note: as JD ranges are in [LST0 -12; LST0 + 36], sampled jds are fixed by getJDInLstRange(jd) 
        // in LST range [0; 24] in order to have accurate target position
        // output :
        final ObservabilityContext ctx = obsCtx;
        List<Range> ranges = ctx.getList();

        // prepare cosDec/sinDec:
        final double dec = FastMath.toRadians(precDEC);
        final double cosDec = FastMath.cos(dec);
        final double sinDec = FastMath.sin(dec);

        final double jdMin = jdRiseSet.getMin();
        final double jdMax = jdRiseSet.getMax();

        final AzEl azEl = new AzEl();
        boolean visible;
        boolean last = false;

        Range range = ctx.valueOf(0d, 0d);

        for (double jd = jdMin, jdIn; jd < jdMax; jd += JD_STEP) {

            // fast interrupt:
            checkInterrupted();

            // fix JD in LST range [0; 24] in order to have accurate target position:
            jdIn = getJDInLstRange(jd);

            this.sco.getTargetPosition(cosDec, sinDec, jdIn, azEl);

            visible = true;

            // Check pointing restrictions according to the wind direction:
            if (Range.contains(this.azimuthRanges, azEl.getAzimuth())) {

                if (isLogDebug) {
                    logger.debug("Target pointing discarded by wind direction = {} [{}]", azEl.getAzimuth(), this.windAzimuth);
                }

                visible = false;
            }

            // manage intervals :
            if (visible) {
                if (!last) {
                    last = true;
                    // start point
                    range.setMin(jd);
                }
            } else {
                if (last) {
                    last = false;
                    // end point
                    range.setMax(jd);
                    ranges.add(range);
                    range = ctx.valueOf(0d, 0d);
                }
            }
        } // jd

        // close last interval if opened :
        if (range.getMin() > 0d) {
            range.setMax(jdMax);
            ranges.add(range);
        } else {
            // recycle range:
            ctx.recycleRange(range);
        }
        if (ranges.isEmpty()) {
            // recycle list:
            ctx.recycleList(ranges);
            ranges = DelayLineService.EMPTY_RANGE_LIST;
        } else if (ranges.size() == 1 && jdRiseSet.equals(ranges.get(0))) {
            // recycle ranges:
            ctx.recycleRangesAndList(ranges);
            // no restriction:
            return null;
        }

        return ranges;
    }

    /**
     * Check the moon restriction given the target rise/set range (JD) (FLI threshold and object magnitude)
     * @param target target to test (name and flux V used)
     * @param precDEC precessed DEC in degrees
     * @param jdRiseSet target rise/set range (JD)
     * @return list of observable ranges (no restriction) or null if no restriction
     */
    private List<Range> checkMoonRestriction(final Target target, final double precDEC, final Range jdRiseSet) {

        // Note: as JD ranges are in [LST0 -12; LST0 + 36], sampled jds are fixed by getJDInLstRange(jd) 
        // in LST range [0; 24] in order to have accurate moon position
        // get FLI on the coming night (only):
        final double fli = this.data.getMoonIllumPercent();

        // Add 5% margin for quick check:
        final double warningThreshold = this.moonPointingRestriction.getWarningThreshold();
        final double testThreshold = warningThreshold * 1.05d;

        // output :
        List<Range> ranges;

        // prepare cosDec/sinDec:
        final double dec = FastMath.toRadians(precDEC);
        final double cosDec = FastMath.cos(dec);
        final double sinDec = FastMath.sin(dec);

        final double jdMin = jdRiseSet.getMin();
        final double jdMax = jdRiseSet.getMax();
        final double jdMid = 0.5d * (jdMin + jdMax);

        boolean doCheck = false;

        // First check if moon and target are close enough to check for the complete rise/set interval:
        double separation;

        // check at jd min:
        separation = getMoonSeparation(cosDec, sinDec, jdMin);

        if (separation <= testThreshold) {
            doCheck = true;
        } else {
            // check at jd mid:
            separation = getMoonSeparation(cosDec, sinDec, jdMid);

            if (separation <= testThreshold) {
                doCheck = true;
            } else {
                // check at jd max:
                separation = getMoonSeparation(cosDec, sinDec, jdMax);

                if (separation <= testThreshold) {
                    doCheck = true;
                }
            }
        }

        if (doCheck) {
            // keep only the applicable rule: test fli (global) then flux (target related)
            MoonRestriction appliedRule = null;

            final List<MoonRestriction> moonRestrictionList = this.moonPointingRestriction.getRestrictions();
            MoonRestriction restriction;

            for (int i = 0, len = moonRestrictionList.size(); i < len; i++) {
                restriction = moonRestrictionList.get(i);

                final Double ruleFli = restriction.getFli();
                if (ruleFli != null) {
                    // inverse condition:
                    if (fli < ruleFli.doubleValue()) {
                        if (isLogDebug) {
                            logger.debug("skip rule (fli = {} < {})", fli, ruleFli);
                        }
                        // skip rule
                        continue;
                    }
                }
                final FluxCondition fluxCond = restriction.getFlux();
                if (fluxCond != null) {
                    final Double flux = target.getFlux(fluxCond.getBand());

                    final double fluxValue = (flux == null) ? Double.POSITIVE_INFINITY : flux; // consider FAINT target if undefined

                    if (fluxCond.getOp() == Operator.LOWER) {
                        // inverse condition:
                        if (fluxValue > fluxCond.getValue()) {
                            if (isLogDebug) {
                                logger.debug("skip rule NOT(flux = {} LOWER  {})", fluxValue, fluxCond.getValue());
                            }
                            // skip rule
                            continue;
                        }
                    } else {
                        // Operator.HIGHER:
                        // inverse condition:
                        if (fluxValue <= fluxCond.getValue()) {
                            if (isLogDebug) {
                                logger.debug("skip rule NOT(flux = {} HIGHER {})", fluxValue, fluxCond.getValue());
                            }
                            // skip rule
                            continue;
                        }
                    }
                }

                // keep or overwrite applied rule:
                if ((appliedRule == null) || (restriction.getSeparation() > appliedRule.getSeparation())) {
                    appliedRule = restriction;
                }
            }

            logger.debug("appliedRule: {}", appliedRule);

            // Process Rise/Set HA range:
            final ObservabilityContext ctx = obsCtx;
            ranges = ctx.getList();

            double minSeparation = Double.POSITIVE_INFINITY;
            double minJd = 0d;

            boolean visible;
            boolean last = false;

            Range range = ctx.valueOf(0d, 0d);

            for (double jd = jdMin; jd < jdMax; jd += JD_STEP) {

                // fast interrupt:
                checkInterrupted();

                visible = true;

                // check at jd (internally fix JD in LST range [0; 24]):
                separation = getMoonSeparation(cosDec, sinDec, jd);

                if (separation < minSeparation) {
                    minSeparation = separation;
                    minJd = jd;
                }

                // use uncertainty:
                separation -= MOON_SEPARATION_MARGIN;

                // evaluate applied rule:
                if ((appliedRule != null) && (separation < appliedRule.getSeparation())) {
                    visible = false;
                }

                // manage intervals :
                if (visible) {
                    if (!last) {
                        last = true;
                        // start point
                        range.setMin(jd);
                    }
                } else {
                    if (last) {
                        last = false;
                        // end point
                        range.setMax(jd);
                        ranges.add(range);
                        range = ctx.valueOf(0d, 0d);
                    }
                }
            } // jd

            // close last interval if opened :
            if (range.getMin() > 0d) {
                range.setMax(jdMax);
                ranges.add(range);
            } else {
                // recycle range:
                ctx.recycleRange(range);
            }
            if (ranges.isEmpty()) {
                // recycle list:
                ctx.recycleList(ranges);
                ranges = DelayLineService.EMPTY_RANGE_LIST;
            }

            // check again the true warning threshold:
            if (minSeparation < warningThreshold) {
                // add warning:
                final StringBuffer sb = getBuffer();
                sb.append("Moon separation is ");
                FormatterUtils.format(df1, sb, minSeparation).append(" deg at ");
                FormatterUtils.format(timeFormatter, sb, convertJDToDate(minJd));
                sb.append(" for target [").append(target.getName()).append("]<br> Please check pointing restrictions.");

                this.addWarning(sb.toString());
            }

        } else {
            // moon is too far:
            ranges = null;
        }

        return ranges;
    }

    /**
     * Return the moon separation in degrees of the current target at the given julian date
     * @param cosDec cosinus of target declination
     * @param sinDec sinus of target declination
     * @param jd julian date
     * @return moon separation in degrees or +INFINITY if moon is not visible
     */
    private double getMoonSeparation(final double cosDec, final double sinDec, final double jd) {
        // fix JD in LST range [0; 24] in order to have accurate moon position:
        final double jdIn = getJDInLstRange(jd);
        return this.sco.getMoonSeparation(cosDec, sinDec, jdIn);
    }

    /**
     * Compute discarded target azimuth ranges by telescope pointing restrictions due to the wind direction
     * @param windAzimuth wind direction as azimuth in degrees (0 to north)
     * @param windAzimuthalRestriction angle in degrees forbidden
     * @return discarded target azimuth ranges or null
     */
    private static List<Range> getAzimuthRange(final Double windAzimuth, final Double windAzimuthalRestriction) {
        if (windAzimuth == null || windAzimuthalRestriction == null || windAzimuthalRestriction < 0d) {
            return null;
        }
        final List<Range> ranges = new ArrayList<Range>(2);
        double azMin, azMax;

        // discarded pointing azimuth opposite to wind direction:
        final double discardedAzimuth = (windAzimuth < 180d) ? windAzimuth + 180d : windAzimuth - 180d;

        azMin = discardedAzimuth - windAzimuthalRestriction;
        azMax = discardedAzimuth + windAzimuthalRestriction;

        if (azMin >= 0d && azMax <= 360d) {
            ranges.add(new Range(azMin, azMax));
        } else {
            if (azMin < 0d) {

                if (azMax < 0d) {
                    azMin += 360d;
                    azMax += 360d;

                    ranges.add(new Range(Math.min(azMin, azMax), Math.max(azMin, azMax)));
                } else {
                    ranges.add(new Range(azMin + 360d, 360d));
                    ranges.add(new Range(0d, azMax));
                }
            } else {
                ranges.add(new Range(azMin, 360d));
                ranges.add(new Range(0d, azMax - 360d));
            }
        }
        return ranges;
    }

    /**
     * Use the observation to define the interferometer and instrument configurations and check if the interferometer has PoPs
     */
    private void prepareObservation() {
        // If min elevation was defined in constructor, skip observation's value :
        if (Double.isNaN(this.minElev)) {
            this.minElev = this.observation.getInterferometerConfiguration().getMinElevation();
        }

        this.useNightLimit = this.observation.getWhen().isNightRestriction();

        if (this.doBaseLineLimits || this.ignoreUseNightLimit) {
            // ignore night limits :
            this.useNightLimit = false;
        }

        final InterferometerConfiguration intConf = this.observation.getInterferometerConfiguration().getInterferometerConfiguration();

        this.interferometer = intConf.getInterferometer();

        final FocalInstrumentConfiguration insConf = this.observation.getInstrumentConfiguration().getInstrumentConfiguration();

        this.instrument = insConf.getFocalInstrument();

        if (isLogDebug) {
            logger.debug("interferometer: {}", this.interferometer.getName());
            logger.debug("instrument: {}", this.instrument.getName());
        }

        // check Pops :
        this.hasPops = !this.interferometer.getPops().isEmpty();

        if (isLogDebug) {
            logger.debug("hasPops: {}", this.hasPops);
        }

        // wind restriction only if night limits are enabled :
        if (this.useNightLimit) {
            this.windAzimuth = this.observation.getWhen().getWindAzimuth();
            final Double windAzimuthalRestriction = this.interferometer.getWindPointingRestriction();

            this.azimuthRanges = getAzimuthRange(this.windAzimuth, windAzimuthalRestriction);
            this.hasWindRestriction = this.azimuthRanges != null;

            if (isLogDebug) {
                logger.debug("windAzimuth: {}", windAzimuth);
                logger.debug("windRestriction: {}", windAzimuthalRestriction);
                logger.debug("azimuthRanges: {}", azimuthRanges);
            }
        }
    }

    /**
     * Define the beams from the user station list, associates a channel to the beam (first channel not used) and a delay line.
     * Computes the fixed offset for the beam from the station fixed offset and the interferometer switchyard
     * @throws IllegalStateException if the station list is undefined
     */
    private void prepareBeams() throws IllegalStateException {
        // Get chosen stations :
        final List<Station> stations = this.observation.getInstrumentConfiguration().getStationList();
        if (stations == null || stations.isEmpty()) {
            throw new IllegalStateException("The station list is empty !");
        }

        if (isLogDebug) {
            logger.debug("stations: {}", stations);
        }

        this.data.setStationNames(this.observation.getInstrumentConfiguration().getStations());

        // All telescopes have the same properties for a given baseline :
        final Telescope tel = stations.get(0).getTelescope();

        final MoonPointingRestriction mpr = tel.getMoonPointingRestriction();
        if (mpr != null) {
            this.moonPointingRestriction = mpr;
        }

        final int nBeams = stations.size();

        // find the optional channels associated to the stations in the instrument configuration :
        // CHARA : predefined channel per station for a specific base line :
        final List<Channel> relatedChannels = ConfigurationManager.getInstance().getInstrumentConfigurationChannels(
                this.observation.getInterferometerConfiguration().getName(),
                this.observation.getInstrumentConfiguration().getName(),
                this.observation.getInstrumentConfiguration().getStations());

        if (isLogDebug) {
            logger.debug("relatedChannels: {}", relatedChannels);
        }

        final int nRelChannels = relatedChannels.size();
        final boolean useRelatedChannels = nRelChannels > 0;

        if (useRelatedChannels && nBeams != nRelChannels) {
            throw new IllegalStateException("The number of associated channels does not match the station list : " + stations + " <> " + relatedChannels);
        }

        // find the optional delay lines associated to the stations in the instrument configuration :
        // VLTI : predefined delay line per station for a specific base line :
        final List<DelayLine> relatedDLs = ConfigurationManager.getInstance().getInstrumentConfigurationDelayLines(
                this.observation.getInterferometerConfiguration().getName(),
                this.observation.getInstrumentConfiguration().getName(),
                this.observation.getInstrumentConfiguration().getStations());

        if (isLogDebug) {
            logger.debug("relatedDelayLines: {}", relatedDLs);
        }

        final int nRelDLs = relatedDLs.size();
        final boolean useRelatedDLs = nRelDLs > 0;

        if (useRelatedDLs && nBeams != nRelDLs) {
            throw new IllegalStateException("The number of associated delay lines does not match the station list : " + stations + " <> " + relatedDLs);
        }

        this.beams = new ArrayList<Beam>(nBeams);

        // used delay lines:
        final HashSet<DelayLine> dlSet = new HashSet<DelayLine>(nBeams);
        // used channels:
        final HashSet<Channel> channelSet = new HashSet<Channel>(nBeams);

        Channel selectedChannel;
        DelayLine selectedDelayLine;

        // Beams are defined in the same ordering than stations :
        for (int i = 0; i < nBeams; i++) {
            Station station = stations.get(i);
            Beam beam = new Beam(station);

            // predefined Channel (CHARA) :
            if (useRelatedChannels && i < nRelChannels) {
                selectedChannel = relatedChannels.get(i);
                // check that given channel is not already used by another beam:
                if (!channelSet.contains(selectedChannel)) {
                    channelSet.add(selectedChannel);
                    beam.setChannel(selectedChannel);
                }
            }

            // predefined DL (VLTI) :
            if (useRelatedDLs && i < nRelDLs) {
                selectedDelayLine = relatedDLs.get(i);
                // check that given DL is not already used by another beam:
                if (!dlSet.contains(selectedDelayLine)) {
                    dlSet.add(selectedDelayLine);
                    beam.setDelayLine(selectedDelayLine);
                }
            }

            this.beams.add(beam);

            if (station.getHorizon() != null && !station.getHorizon().getPoints().isEmpty()) {
                this.hasHorizon = true;
            }
        }

        // Get all possible delay Lines:
        final List<DelayLine> delayLines = this.interferometer.getDelayLines();
        final int nDelayLines = delayLines.size();

        // Has switchyard ?
        if (!this.interferometer.getChannels().isEmpty() && this.interferometer.getSwitchyard() != null) {
            // Case Interferometer with a switchyard (VLTI and CHARA) :

            // VLTI: VCM restrictions:
            final List<DelayLineRestriction> delayLineRestrictions = this.interferometer.getDelayLineRestrictions();
            final int nDLRestrictions = delayLineRestrictions.size();

            // 2 cases ;
            // CHARA : predefined channel per station for a specific base line
            // VLTI : find any available channel for every station
            //        OR predefined channel and delay line per station for a specific base line (Period >= 94)
            // 1- Associate a channel / DL to the beam :
            StationLinks sl;
            for (Beam b : this.beams) {
                // for each station, get the possible channels in the switchyard configuration:
                sl = ConfigurationManager.getInstance().getStationLinks(this.interferometer, b.getStation());

                // VLTI : find an available channel for every station:
                if (useRelatedChannels && b.getDelayLine() != null) {
                    // check that the switchyard contains a channel link for the combination [channel | DL]:
                    boolean matchDl = false;
                    for (ChannelLink cl : sl.getChannelLinks()) {
                        if (ObjectUtils.areEquals(b.getChannel(), cl.getChannel()) && b.getDelayLine().equals(cl.getDelayLine())) {
                            matchDl = true;
                            break;
                        }
                    }
                    if (!matchDl) {
                        logger.warn("Invalid DL for station {} / channel {} / delay line {}", b.getStation(), b.getChannel(), b.getDelayLine());
                        dlSet.remove(b.getDelayLine());
                        b.setDelayLine(null);
                        channelSet.remove(b.getChannel());
                        b.setChannel(null);
                    }
                }
                if (!useRelatedChannels || b.getChannel() == null) {
                    for (ChannelLink cl : sl.getChannelLinks()) {
                        if (!channelSet.contains(cl.getChannel())) {
                            // check that optional associated DL is not used:
                            if ((cl.getDelayLine() == null)
                                    || (b.getDelayLine() == null && !dlSet.contains(cl.getDelayLine()))
                                    || (cl.getDelayLine().equals(b.getDelayLine()))) {

                                if (cl.getDelayLine() != null) {
                                    // set DL if not defined:
                                    dlSet.add(cl.getDelayLine());
                                    b.setDelayLine(cl.getDelayLine());
                                }
                                channelSet.add(cl.getChannel());

                                // use this channel for the beam :
                                b.setChannel(cl.getChannel());
                                break;
                            }
                        }
                    }
                }
                if (b.getChannel() == null) {
                    throw new IllegalStateException("Unable to associate a channel to every station [" + stations + "].");
                }

                // Use the channel link corresponding to the beam channel:
                for (ChannelLink cl : sl.getChannelLinks()) {
                    if (cl.getChannel().equals(b.getChannel())) {
                        if ((cl.getDelayLine() != null) && (!cl.getDelayLine().equals(b.getDelayLine()))) {
                            // DL does not match, try another channel link:
                            continue;
                        }
                        // optical path = switchyard + station fixed offset
                        b.addToOpticalPathLength(cl.getOpticalLength());

                        // fixed offset (CHARA) :
                        if (b.getStation().getDelayLineFixedOffset() != null) {
                            b.addToOpticalPathLength(b.getStation().getDelayLineFixedOffset());
                        }

                        // set the optional DL maximum throws from switchyard (VLTI VCM limits):
                        if (nDLRestrictions != 0) {
                            this.hasSwitchyardDelayLineMaxThrow = true;
                            final List<DelayLineRestrictionThrow> delayLineThrows = new ArrayList<DelayLineRestrictionThrow>(nDLRestrictions);

                            // fix missing or bad ordering of restrictions
                            // as interferometer.getDelayLineRestrictions():
                            for (DelayLineRestriction restriction : delayLineRestrictions) {
                                DelayLineRestrictionThrow dlThrow = null;

                                for (DelayLineRestrictionThrow clThrow : cl.getDelayLineThrows()) {
                                    if (restriction.equals(clThrow.getRestriction())) {
                                        dlThrow = clThrow;
                                        break;
                                    }
                                }
                                delayLineThrows.add(dlThrow);
                            }

                            b.setDelayLineThrows(delayLineThrows);
                        }

                        if (isLogDebug) {
                            logger.debug("station = {} - channel = {}", b.getStation(), b.getChannel().getName());
                            logger.debug("switchyard = {} - fixed offset = {}", cl.getOpticalLength(), b.getStation().getDelayLineFixedOffset());
                            logger.debug("total OPL: {}", b.getOpticalPathLength());
                        }
                        break;
                    }
                }
            } // for loop on beams

            // 2- Associate a delay line to the beam if missing:
            // Use any Delay line available except if the delay line has a prefered station (CHARA E1 shorter than others)
            for (Beam b : this.beams) {
                // check if DL is empty (VLTI can have DL set from baseline or channel links):
                if (b.getDelayLine() == null) {
                    // first find the delay line dedicated to one station (CHARA):
                    Station beamStation = b.getStation();

                    selectedDelayLine = null;

                    for (DelayLine dl : delayLines) {
                        if (beamStation.equals(dl.getStation())) {
                            if (!dlSet.contains(dl)) {
                                selectedDelayLine = dl;
                            }
                            break;
                        }
                    }

                    // If undefined, use any available delay line (VLTI):
                    if (selectedDelayLine == null) {
                        for (DelayLine dl : delayLines) {
                            if (!dlSet.contains(dl)) {
                                selectedDelayLine = dl;
                                break;
                            }
                        }
                    }

                    if (selectedDelayLine != null) {
                        dlSet.add(selectedDelayLine);
                        b.setDelayLine(selectedDelayLine);
                    } else {
                        // To be checked
                        throw new IllegalStateException("Impossible to associate a delay line to the beam [" + b + "].");
                    }
                }
            }

        } else {
            // Simpler interferometer : no channel definition nor switchyard = SUSI

            // Simple association between DL / Station = DL_n linked to Station_n
            // Use only the fixed offset of the station :
            int i = 0;
            for (Beam b : this.beams) {
                if (i < nDelayLines) {
                    b.setDelayLine(delayLines.get(i));
                    if (nBeams > 1) {
                        if (b.getStation().getDelayLineFixedOffset() == null) {
                            throw new IllegalStateException("Missing fixed offset for station [" + b.getStation() + "].");
                        }
                        b.addToOpticalPathLength(b.getStation().getDelayLineFixedOffset());
                    }
                } else {
                    // no delay line ?
                    throw new IllegalStateException("Impossible to associate a delay line to the beam [" + b + "].");
                }
                i++;
            }
        }

        if (isLogDebug) {
            // dump beam information:
            int i = 0;
            for (Beam b : this.beams) {
                logger.debug("beam [{}]: {}", i, b.toString());
                i++;
            }
        }

        this.data.setBeams(this.beams);

        if (isLogDebug) {
            logger.debug("Beams: {}", this.beams);
        }
    }

    /**
     * Generate all PoP combinations and offsets for the given number of beams
     */
    private void preparePopCombinations() {

        final int nBeams = this.beams.size();
        final int sizeBL = this.baseLines.size();

        final List<PopCombination> popCombs;

        // Get chosen PoPs :
        final List<Pop> userPoPs = this.observation.getInstrumentConfiguration().getPopList();

        if (userPoPs == null || userPoPs.size() != nBeams) {
            // Compute key:
            final StringBuilder sb = new StringBuilder(16);
            sb.append(this.interferometer.getName()).append('_').append(nBeams);

            final String cacheKey = sb.toString();
            sb.setLength(0);

            List<SharedPopCombination> sharedPopCombinations;

            // synchronize on cache (concurrency issues):
            synchronized (popCombinationCache) {
                // is in cache ?
                sharedPopCombinations = popCombinationCache.get(cacheKey);

                // lazy instanciation:
                if (sharedPopCombinations == null) {

                    if (isLogDebug) {
                        logger.debug("computing pops combinations [{}]", cacheKey);
                    }

                    // Generate all PoP combinations for the given number of beams :
                    final List<Pop> pops = this.interferometer.getPops();

                    // Generate all tuples :
                    final List<int[]> tuples = CombUtils.generateTuples(pops.size(), nBeams);

                    sharedPopCombinations = new ArrayList<SharedPopCombination>(tuples.size());

                    for (int[] tuple : tuples) {
                        final Pop[] comb = new Pop[nBeams];
                        for (int i = 0; i < nBeams; i++) {
                            comb[i] = pops.get(tuple[i]);
                        }
                        sharedPopCombinations.add(SharedPopCombination.newInstance(comb, sb));
                    }

                    popCombinationCache.put(cacheKey, sharedPopCombinations);
                }
            } // synchronized

            // fast interrupt:
            checkInterrupted();

            // Create the Pops combination:
            popCombs = new ArrayList<PopCombination>(sharedPopCombinations.size());

            for (SharedPopCombination popComb : sharedPopCombinations) {
                popCombs.add(new PopCombination(popComb));
            }

            this.data.setUserPops(false);

        } else {
            // use the user defined PoPs configuration :
            final Pop[] comb = new Pop[nBeams];
            userPoPs.toArray(comb);

            final PopCombination userComb = new PopCombination(SharedPopCombination.newInstance(comb, new StringBuilder(nBeams)));
            popCombs = new ArrayList<PopCombination>(1);
            popCombs.add(userComb);

            this.data.setUserPops(true);
            this.data.setBestPops(userComb);

            if (isLogDebug) {
                logger.debug("User PoPs: {}", userComb);
            }
        }

        // Use arrays instead of List for performance:
        final Beam[] bs = new Beam[nBeams];
        this.beams.toArray(bs);

        Beam b1, b2;
        Pop p1, p2;

        Pop[] pops;
        double[] popOffsets;
        double t;

        int i, j, k;

        // Fills the Pops offsets:
        for (PopCombination popComb : popCombs) {
            pops = popComb.getPops();

            popOffsets = new double[sizeBL];
            k = 0;

            // use indices to get Pop associated to the station :
            for (i = 0; i < nBeams; i++) {
                for (j = i + 1; j < nBeams; j++) {
                    b1 = bs[i];
                    p1 = pops[i];

                    b2 = bs[j];
                    p2 = pops[j];

                    // Note : the beams are in the same order as the stations :
                    // optical path difference = difference of pops delays :
                    t = getPopOpticalLength(b1.getStation(), p1) - getPopOpticalLength(b2.getStation(), p2);

                    popOffsets[k++] = t;
                }
            }
            popComb.setPopOffsets(popOffsets);
        }

        // update member:
        this.popCombinations = popCombs;

        // Prepare observability context:
        this.bpObsCtx = prepareContext();
    }

    /**
     * Prepare the observability context
     * @return new observability context
     */
    private BestPoPsObservabilityContext prepareContext() {
        final int sizeBL = this.baseLines.size();

        // Prepare observability context:
        final BestPoPsObservabilityContext ctx = new BestPoPsObservabilityContext(sizeBL);

        // Use arrays instead of List for performance:
        ctx.setPopCombs(new PopCombination[this.popCombinations.size()]);
        this.popCombinations.toArray(ctx.getPopCombs());

        ctx.setBaseLines(new BaseLine[sizeBL]);
        this.baseLines.toArray(ctx.getBaseLines());

        ctx.setWRanges(new Range[sizeBL]);
        this.wRanges.toArray(ctx.getWRanges());

        return ctx;
    }

    /**
     * Find the optical length between the given station and the PoP
     * @param station used station
     * @param pop used Pop
     * @return optical length
     *
     * @throws IllegalStateException if bad configuration (missing pop value in switchyard)
     */
    private double getPopOpticalLength(final Station station, final Pop pop) throws IllegalStateException {
        final List<PopLink> popLinks = station.getPopLinks();

        PopLink pl;
        for (int i = 0, size = popLinks.size(); i < size; i++) {
            pl = popLinks.get(i);
            if (pl.getPop() == pop) {
                return pl.getOpticalLength();
            }
        }
        throw new IllegalStateException("No pop value for couple (" + station.getName() + " - " + pop + ") !");
    }

    /**
     * Define the base lines using the predefined beams and computes the range [W min; W max]
     * corresponding to the fixed optical path difference +- the delay line throw
     */
    private void prepareBaseLines() {
        final int nBeams = this.beams.size();

        // baseline count:
        final int sizeBL = CombUtils.comb(nBeams, 2);
        this.baseLines = new ArrayList<BaseLine>(sizeBL);
        this.wRanges = new ArrayList<Range>(sizeBL);

        // VLTI: VCM restrictions:
        final int nDLRestrictions;

        if (this.hasSwitchyardDelayLineMaxThrow) {
            nDLRestrictions = this.interferometer.getDelayLineRestrictions().size();
            this.wRangesVcms = new ArrayList<List<Range>>(nDLRestrictions);
            for (int k = 0; k < nDLRestrictions; k++) {
                this.wRangesVcms.add(new ArrayList<Range>(sizeBL));
            }
        } else {
            nDLRestrictions = 0;
            this.wRangesVcms = null;
        }

        final StringBuffer sb = getBuffer();
        Beam b1, b2;
        Position3D pos1, pos2;
        final boolean[] ignoreRestrictions = new boolean[nDLRestrictions];
        DelayLineRestrictionThrow throw1, throw2;

        double x, y, z, opd, wMin, wMax;
        BaseLine bl;

        for (int i = 0; i < nBeams; i++) {
            for (int j = i + 1; j < nBeams; j++) {
                b1 = this.beams.get(i);
                b2 = this.beams.get(j);

                pos1 = b1.getStation().getRelativePosition();
                pos2 = b2.getStation().getRelativePosition();

                x = pos1.getPosX() - pos2.getPosX();
                y = pos1.getPosY() - pos2.getPosY();
                z = pos1.getPosZ() - pos2.getPosZ();

                // a Base line defines only the geometry:
                sb.setLength(0);
                sb.append(b1.getStation().getName()).append('-').append(b2.getStation().getName());
                bl = new BaseLine(sb.toString(), b1, b2, x, y, z);
                this.baseLines.add(bl);

                // optical path difference:
                opd = b1.getOpticalPathLength() - b2.getOpticalPathLength();

                // 1 DL per telescope: DLs may be not equivalent (different throw):
                wMin = opd - b2.getDelayLine().getMaximumThrow();
                wMax = opd + b1.getDelayLine().getMaximumThrow();

                // the W range contains the W limits 
                this.wRanges.add(new Range(wMin, wMax));

                if (isLogDebug) {
                    logger.debug("baseline: {}", bl);
                    logger.debug("opd: {}", opd);
                    logger.debug("wRanges: [{} - {}]", wMin, wMax);
                }

                if (this.hasSwitchyardDelayLineMaxThrow) {
                    // beam delayLineThrows may be null but have the same ordering 
                    // as interferometer.getDelayLineRestrictions():
                    for (int k = 0; k < nDLRestrictions; k++) {
                        // skip invalid restriction:
                        if (!ignoreRestrictions[k]) {
                            // W range contains the W low limits (VCM low limit) for the corresponding base line:
                            throw1 = b1.getDelayLineThrows().get(k);
                            throw2 = b2.getDelayLineThrows().get(k);

                            if ((throw1 == null) || (throw2 == null)) {
                                ignoreRestrictions[k] = true;

                                if (isLogDebug) {
                                    logger.debug("skip invalid restriction [{}]",
                                            this.interferometer.getDelayLineRestrictions().get(k).getName());
                                }

                                // prune wRange list:
                                this.wRangesVcms.get(k).clear();
                                continue;
                            }

                            wMin = opd - throw2.getValue();
                            wMax = opd + throw1.getValue();

                            this.wRangesVcms.get(k).add(new Range(wMin, wMax));

                            if (isLogDebug) {
                                logger.debug("wRangesVcms [{}]: [{} - {}]", throw1.getRestriction().getName(), wMin, wMax);
                            }
                        }
                    }
                }
            }
        }

        if (this.hasSwitchyardDelayLineMaxThrow) {
            // check if VCM restrictions have any impact ie wRanges <> wRangesVcms
            boolean different = false;
            for (int k = 0; k < nDLRestrictions; k++) {
                if (!Range.equals(this.wRanges, this.wRangesVcms.get(k))) {
                    different = true;
                    break;
                }
            }
            if (!different) {
                logger.debug("disable delay line restriction checks.");
                // disable delay line restriction checks:
                this.hasSwitchyardDelayLineMaxThrow = false;
                this.wRangesVcms = null;
            }
        }

        this.data.setBaseLines(this.baseLines);
    }

    /**
     * Process the sun time stamps to have both night limits in the LST range [0;24] +/- 12h
     * and all intervals (day/night/twilights) in the jd bounds [jdLower; jdUpper]
     * @param sunTimes jd sun almanach times arround midnight (-1 day; + 2 days)
     */
    private void processSunAlmanach(final List<AstroAlmanacTime> sunTimes) {
        List<SunTimeInterval> intervals = null;

        if (sunTimes != null && !sunTimes.isEmpty()) {
            final boolean isDebug = isLogDebug; // local var

            final int nbInterval = sunTimes.size() - 1;
            intervals = new ArrayList<SunTimeInterval>(9);

            // LST0 reference used to convert HA in JD:
            final double jdLst0 = this.sc.getJdForLst0();
            // lower LST bound = LST0 - 12h
            final double jdLstLower = jdLst0 - AstroSkyCalc.HALF_LST_DAY_IN_JD;
            // upper LST bound = LST0 + 24h + 12h
            final double jdLstUpper = jdLst0 + 3d * AstroSkyCalc.HALF_LST_DAY_IN_JD;

            if (isDebug) {
                logger.debug("jdLst bounds = [{} - {}]", jdLstLower, jdLstUpper);
            }

            AstroAlmanacTime stFrom, stTo;
            double jdFrom, jdTo;
            Date from, to;
            SunType type;

            for (int i = 0; i < nbInterval; i++) {
                stFrom = sunTimes.get(i);
                stTo = sunTimes.get(i + 1);

                jdFrom = stFrom.getJd();
                jdTo = stTo.getJd();

                switch (stFrom.getType()) {
                    case SunTwl18Rise:
                        type = SunType.AstronomicalTwilight;
                        break;
                    case SunTwl12Rise:
                        type = SunType.NauticalTwilight;
                        break;
                    case SunTwl06Rise:
                        type = SunType.CivilTwilight;
                        break;
                    case SunRise:
                        type = SunType.Day;
                        break;
                    case SunSet:
                        type = SunType.CivilTwilight;
                        break;
                    case SunTwl06Set:
                        type = SunType.NauticalTwilight;
                        break;
                    case SunTwl12Set:
                        type = SunType.AstronomicalTwilight;
                        break;
                    case SunTwl18Set:
                        type = SunType.Night;
                        break;
                    default:
                        type = null;
                }

                if (type != null && type.isNight(this.twilightNightLimit)) {
                    // Keep nights that are inside or overlapping the range [jdLstLower; jdLstUpper] range :
                    if ((jdFrom >= jdLstLower && jdFrom <= jdLstUpper) || (jdTo >= jdLstLower && jdTo <= jdLstUpper)) {
                        this.nightLimits.add(new Range(jdFrom, jdTo));
                    }

                    // Keep the night part inside or overlapping the range [jdLower; jdUpper] range used to compute moon illumination: */
                    if ((jdFrom >= this.jdLower && jdFrom <= this.jdUpper) || (jdTo >= this.jdLower && jdTo <= this.jdUpper)) {
                        this.nightOnlyRanges.add(new Range(Math.max(jdFrom, this.jdLower), Math.min(jdTo, this.jdUpper)));
                    }
                }

                // Keep intervals that are inside or overlapping the range [jdLower; jdUpper] range :
                if ((jdFrom >= this.jdLower && jdFrom <= this.jdUpper) || (jdTo >= this.jdLower && jdTo <= this.jdUpper)) {

                    if (isDebug) {
                        logger.debug("Range[{} - {}] : {}", jdFrom, jdTo, type);
                    }

                    from = jdToDateInDateRange(jdFrom);
                    to = jdToDateInDateRange(jdTo);

                    if (isDebug) {
                        logger.debug("SunInterval[{} - {}] : {}", from, to, type);
                    }

                    intervals.add(new SunTimeInterval(from, to, type));
                }
            }

            // merge contiguous intervals (already ordered):
            Range.union(this.nightLimits);
            Range.union(this.nightOnlyRanges);

            if (isDebug) {
                logger.debug("nightLimits: {}", this.nightLimits);
                logger.debug("nightOnlyRanges: {}", this.nightOnlyRanges);
            }
        }

        this.data.setSunIntervals(intervals);
    }

    /**
     * Convert a JD range list to a date interval with respect for the LST range [0;24]
     * 
     * @param rangesJD range list to convert
     * @param intervals interval list where new date intervals will be added
     */
    private void convertRangesToDateIntervals(final List<Range> rangesJD, final List<DateTimeInterval> intervals) {
        if (rangesJD != null) {
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

        if (jdStart >= this.jdLower) {

            if (jdEnd <= this.jdUpper) {

                // single interval [jdStart;jdEnd]
                intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart), jdToDateInDateRange(jdEnd)));

            } else {

                if (jdStart > this.jdUpper) {
                    // two points over LST 24 :

                    // single interval [jdStart - day;jdEnd - day]
                    intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart - AstroSkyCalc.LST_DAY_IN_JD),
                            jdToDateInDateRange(jdEnd - AstroSkyCalc.LST_DAY_IN_JD)));

                } else {
                    // end occurs after LST 24 :

                    // interval [jdStart;jdLst24]
                    intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart), this.data.getDateMax()));

                    // add the second interval [jdLst0;jdEnd - day]
                    intervals.add(new DateTimeInterval(this.data.getDateMin(), jdToDateInDateRange(jdEnd - AstroSkyCalc.LST_DAY_IN_JD)));
                }
            }

        } else {
            // start occurs before LST 0h :

            if (jdEnd < this.jdLower) {
                // two points before LST 0h :

                // single interval [jdStart + day;jdEnd + day]
                intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart + AstroSkyCalc.LST_DAY_IN_JD),
                        jdToDateInDateRange(jdEnd + AstroSkyCalc.LST_DAY_IN_JD)));

            } else {
                // interval [jdLst0;jdEnd]
                intervals.add(new DateTimeInterval(this.data.getDateMin(), jdToDateInDateRange(jdEnd)));

                // add the second interval [jdStart + day;jdLst24]
                intervals.add(new DateTimeInterval(jdToDateInDateRange(jdStart + AstroSkyCalc.LST_DAY_IN_JD), this.data.getDateMax()));
            }
        }
    }

    /**
     * Fix a JD date into LST range [0;24]
     *
     * @param jd date to fix
     * @return date
     */
    private double getJDInLstRange(final double jd) {
        if (jd >= this.jdLower) {
            if (jd <= this.jdUpper) {
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
     * Convert a JD date to a date within LST range [0;24]
     *
     * @param jd date to convert
     * @return date
     */
    private Date convertJDToDate(final double jd) {
        // fix JD in LST range [0; 24] in order to have accurate date:
        return jdToDateInDateRange(getJDInLstRange(jd));
    }

    /**
     * Convert a JD value to a Date Object (LST or UTC)
     * within range [jdLst0;jdLst24]]<=>[DateMin;DateMax]
     * @see #useLST
     * @see #jdToDate(double)
     * @param jd julian day
     * @return Date Object (LST or UTC)
     */
    private Date jdToDateInDateRange(final double jd) {
        // adjust range limits :
        if (jd <= this.jdLower) {
            return this.data.getDateMin();
        }
        if (jd >= this.jdUpper) {
            return this.data.getDateMax();
        }
        return jdToDate(jd);
    }

    /**
     * Convert a JD value to a Date Object (LST or UTC)
     * @see #useLST
     * @param jd julian day
     * @return Date Object (LST or UTC)
     */
    private Date jdToDate(final double jd) {
        return this.sc.toDate(jd, this.useLST);
    }

    /**
     * Generate a target list for the base line limits (every 5 deg)
     * @return target list
     */
    private List<Target> generateTargetsForBaseLineLimits() {

        final double obsLat = FastMath.toDegrees(this.interferometer.getPosSph().getLatitude());

        final int decStep = 2;

        int decMin = decStep * (int) Math.round((obsLat - 90d + this.minElev) / decStep);
        decMin = Math.max(decMin, -90);

        int decMax = decStep * (int) Math.round((obsLat + 90 - this.minElev) / decStep);
        decMax = Math.min(decMax, 90);

        final StringBuffer sb = getBuffer();

        final List<Target> targets = new ArrayList<Target>((decMax - decMin) / decStep + 1);
        Target t;
        for (int i = decMax; i >= decMin; i -= decStep) {
            t = new Target();

            // delta = n (deg)
            sb.append(SpecialChars.DELTA_UPPER).append(" = ");
            if (i >= 0) {
                /* always print sign '+' as DEC is typically within range [-90; 90] */
                sb.append('+');
            }
            sb.append(i);
            t.setName(sb.toString());
            sb.setLength(0); // recycle 

            // 12:00:00
            t.setRADeg(180d);
            t.setDECDeg(i);
            t.setEQUINOX(AsproConstants.EPOCH_J2000);

            targets.add(t);
        }
        return targets;
    }

    /**
     * Define HA / azimuth / elevation information about observability ranges and transit for the current target
     * @param starObs star observability data
     * @param obsRangeJD observability ranges in JD
     * @param precRA precessed RA in decimal hours
     * @param precDEC precessed DEC in degrees
     * @param doDetails flag to enable detailled (ticks and transit information)
     */
    private void getTargetPosition(final StarObservabilityData starObs, final List<Range> obsRangeJD,
                                   final double precRA, final double precDEC,
                                   final boolean doDetails) {

        // Note: as JD ranges are in [LST0 -12; LST0 + 36], jds are fixed by getJDInLstRange(jd) 
        // in LST range [0; 24] in order to have accurate target position
        final Map<Date, TargetPositionDate> targetPositions = starObs.getTargetPositions();

        // prepare cosDec/sinDec:
        final double dec = FastMath.toRadians(precDEC);
        final double cosDec = FastMath.cos(dec);
        final double sinDec = FastMath.sin(dec);

        final AzEl azEl = new AzEl();

        double jd;

        if (doDetails) {
            final int minElevation = (int) Math.round(this.minElev);

            // internal ticks for elevation :
            for (int elevation = 20; elevation <= 80; elevation += 20) {
                if (elevation > minElevation) {
                    final double haElev = this.sco.getHAForElevation(precDEC, elevation);

                    if (haElev > 0d) {
                        jd = this.sc.convertHAToJD(-haElev, precRA);
                        if (Range.contains(obsRangeJD, jd)) {
                            addTargetPosition(targetPositions, cosDec, sinDec, azEl, jd, doDetails);
                        }

                        jd = this.sc.convertHAToJD(haElev, precRA);
                        if (Range.contains(obsRangeJD, jd)) {
                            addTargetPosition(targetPositions, cosDec, sinDec, azEl, jd, doDetails);
                        }
                    }
                }
            }

            // tick for transit :
            jd = this.sc.convertHAToJD(0d, precRA);
            if (Range.contains(obsRangeJD, jd)) {
                addTargetPosition(targetPositions, cosDec, sinDec, azEl, jd, doDetails);
            }
        }

        // ticks for observability intervals (limits):
        for (int i = 0, len = obsRangeJD.size(); i < len; i++) {
            Range range = obsRangeJD.get(i);
            jd = range.getMin();
            addTargetPosition(targetPositions, cosDec, sinDec, azEl, jd, doDetails);

            jd = range.getMax();
            addTargetPosition(targetPositions, cosDec, sinDec, azEl, jd, doDetails);
        }

        if (isLogDebug) {
            logger.debug("elevations : {}", CollectionUtils.toString(targetPositions.values()));
        }
    }

    /**
     * Add new target position (azimuth and elevation) for the given target and julian date
     * @param targetPositions map of target positions keyed by date
     * @param cosDec cosinus of target declination
     * @param sinDec sinus of target declination
     * @param azEl AzEl instance
     * @param jd julian date
     * @param showTicks true to show ticks at this date
     */
    private void addTargetPosition(final Map<Date, TargetPositionDate> targetPositions,
                                   final double cosDec, final double sinDec,
                                   final AzEl azEl, final double jd,
                                   final boolean showTicks) {

        // fix JD in LST range [0; 24] in order to have accurate target position:
        final double jdIn = getJDInLstRange(jd);
        final double ha = this.sco.getTargetPosition(cosDec, sinDec, jdIn, azEl);
        final Date date = jdToDateInDateRange(jdIn);

        if (showTicks || !targetPositions.containsKey(date)) {
            targetPositions.put(date, new TargetPositionDate(date, ha,
                    (int) Math.round(azEl.getAzimuth()), (int) Math.round(azEl.getElevation()), showTicks));
        }
    }

    /**
     * Convert the given list of HA ranges to date intervals in LST (used by Export OB only)
     *
     * Note : date intervals that are over (00:00 or 24:00) are merged.
     * For example : 22:00->24:00 and 00:00->01:00 returns 22:00->01:00
     *
     * @see fr.jmmc.aspro.ob.ExportOBVLTI#processDateTime(String, ObservationSetting, Target)
     *
     * @param ranges HA ranges
     * @param precRA precessed target right ascension in decimal hours
     * @return date intervals
     */
    public List<DateTimeInterval> convertHARangesToDateInterval(final List<Range> ranges, final double precRA) {
        if (ranges != null) {
            final List<Range> jdRanges = new ArrayList<Range>(ranges.size());
            for (Range range : ranges) {
                jdRanges.add(this.sc.convertHAToJDRange(range, precRA, obsCtx));
            }

            final List<DateTimeInterval> dateIntervals = new ArrayList<DateTimeInterval>(jdRanges.size() + 2);
            // convert JD ranges to date ranges:
            convertRangesToDateIntervals(jdRanges, dateIntervals);

            // Replace the 24:00:00 date by 00:00:00 to merge contiguous intervals in LST :
            final Date lst0 = getData().getDateMin();
            final Date lst24 = getData().getDateMax();

            boolean found = false;
            DateTimeInterval interval;
            for (final ListIterator<DateTimeInterval> it = dateIntervals.listIterator(); it.hasNext();) {
                interval = it.next();
                if (lst24.equals(interval.getEndDate())) {
                    found = true;
                    it.set(new DateTimeInterval(interval.getStartDate(), lst0));
                }
            }

            if (found && dateIntervals.size() > 1) {
                // sort the intervals in reverse order:
                Collections.sort(dateIntervals);
                Collections.reverse(dateIntervals);

                // merge contiguous date ranges :
                DateTimeInterval.mergeSorted(dateIntervals);

                if (dateIntervals.size() > 1) {
                    Collections.sort(dateIntervals);
                }
            }

            if (isLogDebug) {
                logger.debug("dateIntervals: {}", dateIntervals);
            }
            return dateIntervals;
        }
        return null;
    }

    /**
     * Return the jd at the middle of the range [jdLower; jdUpper]
     * @return jd at the middle of the range
     */
    private double jdCenter() {
        return 0.5d * (this.jdLower + this.jdUpper);
    }

    /**
     * Return the observability data (available after compute has been invoked)
     * @return observability data
     */
    public ObservabilityData getData() {
        return this.data;
    }

    /**
     * Add a warning message in the OIFits file
     * @param msg message to add
     */
    private void addWarning(final String msg) {
        this.data.getWarningContainer().addWarningMessage(msg);
    }

    /**
     * Add an information message in the OIFits file
     * @param msg message to add
     */
    private void addInformation(final String msg) {
        this.data.getWarningContainer().addInformationMessage(msg);
    }

    /**
     * Return the target HA min/max limits
     * @param target target to use
     * @param obsCtxLocal observability context (local context ie thread)
     * @return HA range
     */
    private Range getTargetHALimits(final Target target, final ObservabilityContext obsCtxLocal) {
        /** HA min in decimal hours */
        double haMin = AsproConstants.HA_MIN;
        /** HA max in decimal hours */
        double haMax = AsproConstants.HA_MAX;

        // HA Min / Max :
        final TargetConfiguration targetConf = target.getConfiguration();
        if (targetConf != null) {
            if (targetConf.getHAMin() != null) {
                haMin = targetConf.getHAMin().doubleValue();
            }
            if (targetConf.getHAMax() != null) {
                haMax = targetConf.getHAMax().doubleValue();
            }
        }
        if (isLogDebug) {
            logger.debug("ha min: {}", haMin);
            logger.debug("ha max: {}", haMax);
        }

        return obsCtxLocal.valueOf(haMin, haMax);
    }

    /**
     * Check the given hour angle inside the [-haElev; haElev]
     * @param ha ha to check
     * @param haElev rise/set ha
     * @return ha or -haElev or haElev
     */
    private static double checkHA(final double ha, final double haElev) {
        if (ha < -haElev) {
            return -haElev;
        }
        if (ha > haElev) {
            return haElev;
        }
        return ha;
    }

    /**
     * Return new best PoPs estimator according to user preferences and given target
     * @param target target to use (HA Limits estimator)
     * @param haElev rise/set ha
     * @param bpObsCtxLocal observability context (local context ie thread)
     * @return new best PoPs estimator
     */
    private BestPopsEstimator getBestPopsEstimator(final Target target, final double haElev, final ObservabilityContext bpObsCtxLocal) {
        // update pop estimator related to target:
        final BestPopsEstimator bestPopsEstimator;
        switch (this.bestPopsAlgorithm) {
            default:
            case Simple:
                bestPopsEstimator = BestPopsEstimatorFactory.getSimpleEstimator();
                break;
            case Transit:
                bestPopsEstimator = BestPopsEstimatorFactory.getTransitEstimator(this.bestPopEstimatorCriteriaSigma, this.bestPopEstimatorCriteriaAverageWeight);
                break;
            case HALimits:
                final Range haLimits = getTargetHALimits(target, bpObsCtxLocal);
                // keep haCenter in [-haElev; haElev] range:
                final double haCenter = checkHA(haLimits.getCenter(), haElev);
                if (isLogDebug) {
                    logger.debug("haCenter: {} - haElev: {} - haLimits: {}", haCenter, haElev, haLimits);
                }
                // recycle range:
                bpObsCtxLocal.recycleRange(haLimits);

                bestPopsEstimator = BestPopsEstimatorFactory.getHALimitsEstimator(haCenter, this.bestPopEstimatorCriteriaSigma, this.bestPopEstimatorCriteriaAverageWeight);
                break;
        }
        return bestPopsEstimator;
    }

    /**
     * Return new grouped best PoPs estimator according to user preferences
     * @return new grouped best PoPs estimator
     */
    private BestPopsEstimator getGroupedBestPopsEstimator() {
        final BestPopsEstimator bestPopsEstimator;
        switch (this.bestPopsAlgorithm) {
            default:
            case Simple:
                bestPopsEstimator = BestPopsEstimatorFactory.getSimpleEstimator();
                break;
            case Transit:
            case HALimits:
                // only criteriaAverageWeight is useful:
                bestPopsEstimator = BestPopsEstimatorFactory.getTransitEstimator(this.bestPopEstimatorCriteriaSigma, this.bestPopEstimatorCriteriaAverageWeight);
                break;
        }
        return bestPopsEstimator;
    }

    private StringBuffer getBuffer() {
        final StringBuffer sb = this.shared_sb;
        sb.setLength(0);
        return sb;
    }
}
