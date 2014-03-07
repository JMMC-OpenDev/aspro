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
import fr.jmmc.aspro.model.ObservabilityContext;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.RangeFactory;
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
    /** flag to slow down the service to detect concurrency problems */
    private final static boolean DEBUG_SLOW_SERVICE = false;
    /** flag to show range factory statistics */
    private final static boolean SHOW_RANGE_FACTORY_STATS = false;
    /** flag to show best pops statistics */
    private final static boolean SHOW_BEST_POPS_STATS = false;
    /** flag to show task statistics */
    private final static boolean SHOW_TASK_STATS = false;
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
    /** simple RangeFactory (stateless) */
    private static final RangeFactory defaultRangeFactory = new SimpleRangeFactory();

    /* members */
    /** cached log debug enabled */
    private final boolean isLogDebug = logger.isDebugEnabled();

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
    /** Night ranges defined in julian day */
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
    /** beam list */
    private List<Beam> beams = null;
    /** base line list */
    private List<BaseLine> baseLines = null;
    /** W ranges corresponding to the base line list */
    private List<Range> wRanges = null;
    /** list of Pop combinations with pop delays per baseline */
    private List<PopCombination> popCombinations = null;
    /** flag to disable the observability restriction due to the night */
    private boolean ignoreUseNightLimit = false;
    /** estimate the observability context (temporary variables) */
    private ObservabilityContext obsCtx = null;
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
            throw new InterruptedJobException("ObservabilityService.compute: interrupted");
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

        defaultRangeFactory.reset();

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
            defaultRangeFactory.dumpStats();
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
            obsMoonRanges.addAll(this.nightLimits);
            obsMoonRanges.addAll(moonRiseRanges);

            final List<Range> moonRanges = Range.intersectRanges(obsMoonRanges, 2, defaultRangeFactory);

            if (isLogDebug) {
                logger.debug("moonRanges: {}", moonRanges);
            }

            // Moon filters:
            // 1- use ranges in  [LST0 -12; LST0 + 36] over night ranges
            // 2- moon Illum in jdLower / jdUpper
            final double moonIllum = this.sc.getMaxMoonIllum(moonRanges);

            this.data.setMoonIllumPercent(100d * moonIllum);

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
        if (this.hasPops) {
            // show baseline and beams in warnings:
            final StringBuilder sbConf = new StringBuilder(64);
            sbConf.append("Baseline: ");
            for (Beam b : this.beams) {
                sbConf.append(b.getStation().getName()).append(" ");
            }

            sbConf.append("- Beams: ");
            for (Beam b : this.beams) {
                sbConf.append(b.getChannel().getName()).append(" ");
            }

            if (this.popCombinations.size() == 1) {
                // user defined Pop combination:
                sbConf.append("- PoPs: ").append(this.popCombinations.get(0).getIdentifier());
            }

            addInformation(sbConf.toString());

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

                    this.obsCtx.setPopCombs(new PopCombination[0]);

                } else {
                    this.data.setBestPops(bestPopCombination);

                    // use the user defined PoPs configuration :
                    this.popCombinations.clear();
                    this.popCombinations.add(bestPopCombination);

                    // Use arrays instead of List for performance:
                    this.obsCtx.setPopCombs(new PopCombination[1]);
                    this.popCombinations.toArray(this.obsCtx.getPopCombs());
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
        final ObservabilityContext[] obsCtxThreads = new ObservabilityContext[nTh];
        final int[][] nTaskThreads = (SHOW_BEST_POPS_STATS) ? new int[nTh][16] : null; // cache line padding

        for (int t = 0; t < nTh; t++) {
            obsCtxThreads[t] = (t == 0) ? this.obsCtx : new ObservabilityContext(this.obsCtx);
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
                        final ObservabilityContext obsCtxLocal = obsCtxThreads[threadIndex];

                        final Target target = riseTargets.get(targetIndex);
                        final double[] info = infoTargets.get(targetIndex); // Ra, Dec, HaElev

                        // data partitioning so no synchronization required:
                        final List<PopObservabilityData> popDataList = findPoPsForTargetObservability(
                                target, obsCtxLocal, info[0], info[1], info[2], fromCb, endCb);

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
                    obsCtxThreads[t].dumpStats();
                }
            }
        }

        // pop data per target
        final Set<String> uniqueTargets = new HashSet<String>(nTargets);
        GroupedPopObservabilityData popMergeData;
        List<PopObservabilityData> flatPopDataList;
        String key;

        for (final List<PopObservabilityData> targetPopDataList : targetPopDataListResults) {
            // targetPopDataList can be null if the target never rises or is incompatible (skip) :
            if (targetPopDataList != null) {
                uniqueTargets.add(targetPopDataList.get(0).getTargetName());

                // rearrange results :
                for (PopObservabilityData popData : targetPopDataList) {
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
            addWarning("Impossible to find a PoPs combination compatible with all observable targets (" + maxObsTarget + " / " + nObsTarget + ")");
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
                        sbBestPops.append(pm.getPopCombination().getIdentifier()).append(" ");
                        bestPoPList.add(pm.getPopCombination());
                    } else {
                        sbBetterPops.append(pm.getPopCombination().getIdentifier()).append(" ");
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
     * @param obsCtxLocal observability context (local context ie thread)
     * @param precRA precessed RA in degrees
     * @param precDEC precessed DEC in degrees
     * @param haElev rise/set ha
     * @param fromCb index of the first PopCombination to evaluate (inclusive)
     * @param endCb index of the last PopCombination to evaluate (exclusive)
     * 
     * @return list of Pop observability data
     */
    private List<PopObservabilityData> findPoPsForTargetObservability(final Target target,
                                                                      final ObservabilityContext obsCtxLocal,
                                                                      final double precRA, final double precDEC, final double haElev,
                                                                      final int fromCb, final int endCb) {

        // Note: this method can be called by multiple threads (so ensure thread safety and interruption)
        // For all PoP combinations : find the HA interval merged with the HA Rise/set interval
        // list of observability data associated to a pop combination :
        List<PopObservabilityData> popDataList = null;

        // target rise :
        if (haElev > 0d) {
            // HA Min/Max range:
            final Range haLimits = getTargetHALimits(target);

            // rise/set range RESTRICTED by HA Min/Max constraints:
            final Range rangeHARiseSet = new Range(checkHA(haLimits.getMin(), haElev), checkHA(haLimits.getMax(), haElev));

            // update pop estimator related to target:
            obsCtxLocal.setPopEstimator(getBestPopsEstimator(target, haElev));

            final List<Range> rangesTarget = new ArrayList<Range>(4);
            rangesTarget.add(rangeHARiseSet);

            if (this.useNightLimit) {
                final List<Range> haNightLimits = new ArrayList<Range>(2);

                Range rangeHA;
                for (Range rangeJD : this.nightLimits) {
                    rangeHA = this.sc.convertJDToHARange(rangeJD, precRA);
                    if (rangeHA != null) {
                        haNightLimits.add(rangeHA);
                    }
                }

                if (isLogDebug) {
                    logger.debug("HA night limits: {}", haNightLimits);
                }
                rangesTarget.addAll(haNightLimits);
            }

            popDataList = getPopObservabilityData(target.getName(), FastMath.toRadians(precDEC), rangesTarget, true, obsCtxLocal, true, fromCb, endCb);

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

        final int listSize = (this.doDetailedOutput) ? (4 + this.baseLines.size()) : 1;
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

        // define transit date (HA = 0) :
        starObs.setTransitDate(convertJDToDate(this.sc.convertHAToJD(0d, precRA)));

        // Find LST range corresponding to the rise / set of the target :
        final double haElev = this.sco.getHAForElevation(precDEC, this.minElev);

        // update Star Data :
        starData.setPrecRA(precRA);
        starData.setPrecDEC(precDEC);
        starData.setHaElev(haElev);

        // target rise :
        if (haElev > 0d) {
            // rise/set range WITHOUT HA Min/Max constraints:
            final Range rangeHARiseSet = new Range(-haElev, haElev);

            if (isLogDebug) {
                logger.debug("rangeHARiseSet: {}", rangeHARiseSet);
            }

            // HA intervals for every base line :
            List<List<Range>> rangesHABaseLines;

            if (this.hasPops) {

                // update pop estimator related to target:
                this.obsCtx.setPopEstimator(getBestPopsEstimator(target, haElev));

                // handle here all possible combinations for POPs :
                // keep only the POPs that maximize the DL+rise intersection ...
                final List<Range> rangesTarget = new ArrayList<Range>(4);
                rangesTarget.add(rangeHARiseSet);

                if (this.useNightLimit) {
                    final List<Range> haNightLimits = new ArrayList<Range>(2);

                    Range rangeHA;
                    for (Range rangeJD : this.nightLimits) {
                        rangeHA = this.sc.convertJDToHARange(rangeJD, precRA);
                        if (rangeHA != null) {
                            haNightLimits.add(rangeHA);
                        }
                    }

                    if (isLogDebug) {
                        logger.debug("HA night limits: {}", haNightLimits);
                    }
                    rangesTarget.addAll(haNightLimits);
                }

                rangesHABaseLines = findHAIntervalsWithPops(FastMath.toRadians(precDEC), rangesTarget, starObs);

            } else {
                // Get intervals (HA) compatible with all base lines :
                rangesHABaseLines = DelayLineService.findHAIntervals(FastMath.toRadians(precDEC), this.baseLines, this.wRanges, defaultRangeFactory);
            }

            // rangesHABaseLines can be null if the thread was interrupted :
            checkInterrupted();

            // convert HA range to JD range in range [LST0 - 12; LST0 + 12]
            final Range rangeJDRiseSet = this.sc.convertHAToJDRange(rangeHARiseSet, precRA);

            // For now : only VLTI/CHARA has horizon profiles :
            List<Range> rangesJDHorizon = null;
            if (this.hasHorizon) {
                // check horizon profiles inside rise/set range :
                rangesJDHorizon = checkHorizonProfile(precDEC, rangeJDRiseSet);

                if (isLogDebug) {
                    logger.debug("rangesJDHz: {}", rangesJDHorizon);
                }

                // fast interrupt:
                checkInterrupted();
            }

            // Check Moon restriction:
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
                    rangesJDMoon = Arrays.asList(new Range[]{rangeJDRiseSet});
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
                    rangesJDWind = Arrays.asList(new Range[]{rangeJDRiseSet});
                } else {
                    checkJDWind = true;
                }
            }

            // observable ranges (jd) :
            final List<Range> obsRanges = new ArrayList<Range>(13);

            if (this.doDetailedOutput) {

                // Add Rise/Set :
                final StarObservabilityData soRiseSet = new StarObservabilityData(targetName, "Rise/Set", StarObservabilityData.TYPE_RISE_SET);
                // get target position (ha, az, el) at range boundaries:
                getTargetPosition(soRiseSet, Arrays.asList(new Range[]{rangeJDRiseSet}), precRA, precDEC, false);
                starVisList.add(soRiseSet);

                // convert JD ranges to date ranges :
                convertRangeToDateInterval(rangeJDRiseSet, soRiseSet.getVisible());
                if (soRiseSet.getVisible().size() > 1) {
                    // merge contiguous date ranges :
                    DateTimeInterval.merge(soRiseSet.getVisible());
                }

                if (rangesJDHorizon != null) {
                    // Add Horizon :
                    final StarObservabilityData soHz = new StarObservabilityData(targetName, "Horizon", StarObservabilityData.TYPE_HORIZON);
                    // get target position (ha, az, el) at range boundaries:
                    getTargetPosition(soHz, rangesJDHorizon, precRA, precDEC, false);
                    starVisList.add(soHz);

                    // convert JD ranges to date ranges :
                    for (Range range : rangesJDHorizon) {
                        convertRangeToDateInterval(range, soHz.getVisible());
                    }
                    if (soHz.getVisible().size() > 1) {
                        // merge contiguous date ranges :
                        DateTimeInterval.merge(soHz.getVisible());
                    }
                }

                if (rangesJDMoon != null) {
                    // Add Moon separation :
                    final StarObservabilityData soMoon = new StarObservabilityData(targetName, "Moon Sep.", StarObservabilityData.TYPE_MOON_DIST);
                    // get target position (ha, az, el) at range boundaries:
                    getTargetPosition(soMoon, rangesJDMoon, precRA, precDEC, false);
                    starVisList.add(soMoon);

                    // convert JD ranges to date ranges :
                    for (Range range : rangesJDMoon) {
                        convertRangeToDateInterval(range, soMoon.getVisible());
                    }
                    if (soMoon.getVisible().size() > 1) {
                        // merge contiguous date ranges :
                        DateTimeInterval.merge(soMoon.getVisible());
                    }
                }

                if (rangesJDWind != null) {
                    // Add Horizon :
                    final StarObservabilityData soWind = new StarObservabilityData(targetName, "Wind", StarObservabilityData.TYPE_WIND);
                    // get target position (ha, az, el) at range boundaries:
                    getTargetPosition(soWind, rangesJDWind, precRA, precDEC, false);
                    starVisList.add(soWind);

                    // convert JD ranges to date ranges :
                    for (Range range : rangesJDWind) {
                        convertRangeToDateInterval(range, soWind.getVisible());
                    }
                    if (soWind.getVisible().size() > 1) {
                        // merge contiguous date ranges :
                        DateTimeInterval.merge(soWind.getVisible());
                    }
                }

                // Add ranges per BL :
                if (!rangesHABaseLines.isEmpty()) {
                    BaseLine baseLine;
                    List<Range> ranges;
                    StarObservabilityData soBl;
                    for (int i = 0, size = this.baseLines.size(); i < size; i++) {
                        baseLine = this.baseLines.get(i);
                        ranges = rangesHABaseLines.get(i);

                        if (ranges != null) {
                            for (Range range : ranges) {
                                obsRanges.add(this.sc.convertHAToJDRange(range, precRA));
                            }
                        }
                        if (isLogDebug) {
                            logger.debug("baseLine: {}", baseLine);
                            logger.debug("JD ranges: {}", obsRanges);
                        }

                        soBl = new StarObservabilityData(targetName, baseLine.getName(), StarObservabilityData.TYPE_BASE_LINE + i);
                        // get target position (ha, az, el) at range boundaries:
                        getTargetPosition(soBl, obsRanges, precRA, precDEC, false);
                        starVisList.add(soBl);

                        // convert JD ranges to date ranges :
                        for (Range range : obsRanges) {
                            convertRangeToDateInterval(range, soBl.getVisible());
                        }
                        if (soBl.getVisible().size() > 1) {
                            // merge contiguous date ranges :
                            DateTimeInterval.merge(soBl.getVisible());
                        }

                        if (isLogDebug) {
                            logger.debug("Date ranges: {}", soBl.getVisible());
                        }

                        obsRanges.clear();
                    }
                }
            }

            // Merge then all JD intervals :
            // nValid = nBL [dl] + 1 [rise or horizon] + 1 if night limits
            int nValid = this.baseLines.size() + 1;

            // flatten and convert HA ranges to JD range :
            for (List<Range> ranges : rangesHABaseLines) {
                if (ranges != null) {
                    for (Range range : ranges) {
                        obsRanges.add(this.sc.convertHAToJDRange(range, precRA));
                    }
                }
            }

            if (rangesJDHorizon != null) {
                obsRanges.addAll(rangesJDHorizon);
            } else {
                // Check Shadowing for every stations ?
                obsRanges.add(rangeJDRiseSet);
            }

            // Intersect with night limits :
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
            final List<Range> finalRangesHardLimits = Range.intersectRanges(obsRanges, nValid, defaultRangeFactory);

            if (isLogDebug) {
                logger.debug("finalRangesHardLimits: {}", finalRangesHardLimits);
            }

            // store merge result as date intervals :
            if (finalRangesHardLimits != null) {
                // rise/set range WITHOUT HA Min/Max constraints:
                final Range haLimits = getTargetHALimits(target);

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

                    // convert HA Limits to JD range in range [LST0 - 12; LST0 + 12]
                    final Range rangeJDHALimits = this.sc.convertHAToJDRange(haLimits, precRA);
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

                    final List<Range> restrictedRanges = Range.intersectRanges(obsRanges, nValid, defaultRangeFactory);

                    if (isLogDebug) {
                        logger.debug("restrictedRanges: {}", restrictedRanges);
                    }

                    if (Range.equals(finalRangesHardLimits, restrictedRanges)) {
                        finalRanges = finalRangesHardLimits;
                    } else {
                        if (restrictedRanges == null) {
                            finalRanges = Collections.emptyList();
                        } else {
                            finalRanges = restrictedRanges;
                        }

                        // Keep observability ranges without HA restrictions:
                        final List<DateTimeInterval> visibleNoSoftLimits = new ArrayList<DateTimeInterval>(3);

                        // convert JD ranges to date ranges :
                        for (Range range : finalRangesHardLimits) {
                            convertRangeToDateInterval(range, visibleNoSoftLimits);
                        }
                        if (visibleNoSoftLimits.size() > 1) {
                            // merge contiguous date ranges :
                            DateTimeInterval.merge(visibleNoSoftLimits);
                        }
                        starObs.setVisibleNoSoftLimits(visibleNoSoftLimits);
                    }

                } else {
                    finalRanges = finalRangesHardLimits;
                }

                // fast interrupt:
                checkInterrupted();

                // get detailled target position (ha, az, el) for the current target (ticks):
                getTargetPosition(starObs, finalRanges, precRA, precDEC, true);

                // convert JD ranges to date ranges :
                for (Range range : finalRanges) {
                    convertRangeToDateInterval(range, starObs.getVisible());
                }
                if (starObs.getVisible().size() > 1) {
                    // merge contiguous date ranges :
                    DateTimeInterval.merge(starObs.getVisible());
                }

                // update Star Data :
                final List<Range> haObsRanges = new ArrayList<Range>(2);

                Range rangeHA;
                for (Range rangeJD : finalRanges) {
                    rangeHA = this.sc.convertJDToHARange(rangeJD, precRA);
                    if (rangeHA != null) {
                        haObsRanges.add(rangeHA);
                    }
                }

                if (isLogDebug) {
                    logger.debug("HA observability: {}", haObsRanges);
                }
                if (!haObsRanges.isEmpty()) {
                    starData.setObsRangesHA(haObsRanges);
                }
            } else {
                if (isLogDebug) {
                    logger.debug("Target not observable: {}", target);
                }
                addInformation("Target [" + targetName + "] is not observable");
            }

        } else {
            if (isLogDebug) {
                logger.debug("Target never rise: {}", target);
            }
            addInformation("Target [" + targetName + "] is not observable (never rise)");
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
        final List<PopObservabilityData> popDataList = getPopObservabilityData(starObs.getTargetName(), dec, rangesTarget, doSkipDL, this.obsCtx, false);

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
                        sbBestPops.append(popData.getPopCombination().getIdentifier()).append(" ");
                        bestPoPList.add(popData.getPopCombination());
                    } else {
                        sbBetterPops.append(popData.getPopCombination().getIdentifier()).append(" ");
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
                                                               final boolean doSkipDL, final ObservabilityContext obsCtxLocal, final boolean clearRangesBL) {
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
     * @param obsCtxLocal observability context (local context ie thread)
     * @param clearRangesBL true to free rangesBL; false otherwise
     * @param fromCb index of the first PopCombination to evaluate (inclusive)
     * @param endCb index of the last PopCombination to evaluate (exclusive)
     * @return list of Pop Observability Data or null if thread interrupted
     */
    private List<PopObservabilityData> getPopObservabilityData(final String targetName, final double dec, final List<Range> rangesTarget,
                                                               final boolean doSkipDL, final ObservabilityContext obsCtxLocal, final boolean clearRangesBL,
                                                               final int fromCb, final int endCb) {

        // Note: this method can be called by multiple threads (so ensure thread safety and interruption)
        final long start = System.nanoTime();

        // local vars for performance:
        final boolean isDebug = isLogDebug;
        final Thread currentTh = Thread.currentThread(); // multi threading
        final BestPopsEstimator estimator = obsCtxLocal.getPopEstimator();
        // list of observability data associated to a pop combination :
        final List<PopObservabilityData> popDataList = obsCtxLocal.getPopDataList();

        final double cosDec = FastMath.cos(dec);
        final double sinDec = FastMath.sin(dec);

        // Use arrays instead of List for performance:
        final PopCombination[] popCombs = obsCtxLocal.getPopCombs();
        final BaseLine[] bls = obsCtxLocal.getBaseLines();
        final Range[] wRangeArray = obsCtxLocal.getWRanges();

        final int sizeBL = bls.length;

        // precompute W extrema per baseline:
        final double[][] wExtremas = new double[sizeBL][2];

        // For every Base Line :
        for (int i = 0; i < sizeBL; i++) {
            final BaseLine bl = bls[i];

            final double[] wExtrema = DelayLineService.findWExtrema(cosDec, sinDec, bl);

            if (isDebug) {
                logger.debug("wExtrema[{}] = {}", bl.getName(), Arrays.toString(wExtrema));
            }

            wExtremas[i] = wExtrema;
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

        final double[] ha = new double[2];
        final double[] haValues = new double[6];

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

                ranges = DelayLineService.findHAIntervalsForBaseLine(cosDec, sinDec, bls[i], wExtremas[i], wRangeWithOffset, ha, haValues, obsCtxLocal);

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
                    obsCtxLocal.recycleAll(rangesBL);
                }
            } else {

                if (SHOW_BEST_POPS_STATS) {
                    nIter++;
                }

                // note : rangesTarget contains both rise/set intervals + night limits in HA
                obsCtxLocal.resetAndAddInFlatRangeLimits(rangesTarget);

                // merge the baseline ranges with the target intervals to estimate observability:
                popData = PopObservabilityData.estimate(targetName, popComb, rangesBL, nValid, obsCtxLocal, estimator, doSkipDL, clearRangesBL);

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
        final boolean isDebug = isLogDebug; // local var

        // output :
        final List<Range> ranges = new ArrayList<Range>(3);

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

        Range range = new Range();

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
                    range = new Range();
                }
            }
        } // jd

        // close last interval if opened :
        if (range.getMin() > 0d) {
            range.setMax(jdMax);
            ranges.add(range);
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
        // output :
        final List<Range> ranges = new ArrayList<Range>(2);

        // prepare cosDec/sinDec:
        final double dec = FastMath.toRadians(precDEC);
        final double cosDec = FastMath.cos(dec);
        final double sinDec = FastMath.sin(dec);

        final double jdMin = jdRiseSet.getMin();
        final double jdMax = jdRiseSet.getMax();

        final AzEl azEl = new AzEl();
        boolean visible;
        boolean last = false;

        Range range = new Range();

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
                    range = new Range();
                }
            }
        } // jd

        // close last interval if opened :
        if (range.getMin() > 0d) {
            range.setMax(jdMax);
            ranges.add(range);
        }

        if (ranges.size() == 1 && ranges.get(0).equals(jdRiseSet)) {
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

        // get FLI on current night:
        final double fli = this.data.getMoonIllumPercent();

        // Add 5% margin for quick check:
        final double warningThreshold = this.moonPointingRestriction.getWarningThreshold();
        final double testThreshold = warningThreshold * 1.05d;

        // output :
        final List<Range> ranges;

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
                        if (logger.isDebugEnabled()) {
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
                            if (logger.isDebugEnabled()) {
                                logger.debug("skip rule NOT(flux = {} LOWER  {})", fluxValue, fluxCond.getValue());
                            }
                            // skip rule
                            continue;
                        }
                    } else {
                        // Operator.HIGHER:
                        // inverse condition:
                        if (fluxValue <= fluxCond.getValue()) {
                            if (logger.isDebugEnabled()) {
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
            ranges = new ArrayList<Range>(2);

            double minSeparation = Double.POSITIVE_INFINITY;
            double minJd = 0d;

            boolean visible;
            boolean last = false;

            Range range = new Range();

            for (double jd = jdMin; jd < jdMax; jd += JD_STEP) {

                // fast interrupt:
                checkInterrupted();

                visible = true;

                // check at jd:
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
                        range = new Range();
                    }
                }
            } // jd

            // close last interval if opened :
            if (range.getMin() > 0d) {
                range.setMax(jdMax);
                ranges.add(range);
            }

            // check again the true warning threshold:
            if (minSeparation < warningThreshold) {
                // add warning:

                // TODO: use Format.format(val, StringBuffer) instead:
                this.addWarning("Moon separation is " + df1.format(minSeparation)
                        + " deg at " + timeFormatter.format(convertJDToDate(minJd))
                        + " for target [" + target.getName() + "]<br> Please check pointing restrictions.");
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
        // fix JD in LST range [0; 24] in order to have accurate target position:
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
                /* check that given channel is not already used by another beam */
                if (!channelSet.contains(selectedChannel)) {
                    channelSet.add(selectedChannel);
                    beam.setChannel(selectedChannel);
                }
            }

            // predefined DL (VLTI) :
            if (useRelatedDLs && i < nRelDLs) {
                selectedDelayLine = relatedDLs.get(i);
                /* check that given DL is not already used by another beam */
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

        // Get all possible channels:
        final List<Channel> channels = this.interferometer.getChannels();

        // Get all possible delay Lines:
        final List<DelayLine> delayLines = this.interferometer.getDelayLines();

        final int nDelayLines = delayLines.size();

        // Has switchyard ?
        if (!channels.isEmpty() && this.interferometer.getSwitchyard() != null) {
            // Case Interferometer with a switchyard (VLTI and CHARA) :

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
                                    
                                    // set the DL maximum throw (VCM soft limit):
                                    b.setMaximumThrow(cl.getMaximumThrow());
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
                            /* DL does not match, try another channel link */
                            continue;
                        }
                        // optical path = switchyard + station fixed offset
                        b.addOpticalLength(cl.getOpticalLength());

                        // fixed offset (CHARA) :
                        if (b.getStation().getDelayLineFixedOffset() != null) {
                            b.addOpticalLength(b.getStation().getDelayLineFixedOffset());
                        }

                        if (isLogDebug) {
                            logger.debug("station = {} - channel = {}", b.getStation(), b.getChannel().getName());
                            logger.debug("switchyard = {} - fixed offset = {}", cl.getOpticalLength(), b.getStation().getDelayLineFixedOffset());
                            logger.debug("total: {}", b.getOpticalLength());
                        }
                        break;
                    }
                }
            } // for loop on beams

            // 2- Associate a delay line to the beam if missing:
            // Use any Delay line available except if the delay line has a prefered station (CHARA E1 shorter than others)
            for (Beam b : this.beams) {
                /* check if DL is empty (VLTI can have DL set from baseline or channel links) */
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
                    if (b.getStation().getDelayLineFixedOffset() == null) {
                        throw new IllegalStateException("Missing fixed offset for station [" + b.getStation() + "].");
                    }
                    b.addOpticalLength(b.getStation().getDelayLineFixedOffset());
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

        // TODO: KILL
        int i = 0;
        for (Beam b : this.beams) {
            logger.info("beam [{}]: {}", i, b.toString());
            i++;
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
        this.obsCtx = prepareContext();
    }

    /**
     * Prepare the observability context
     * @return new observability context
     */
    private ObservabilityContext prepareContext() {
        final int sizeBL = this.baseLines.size();

        // Prepare observability context:
        final ObservabilityContext ctx = new ObservabilityContext(sizeBL);

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

        // baseline count :
        final int blen = CombUtils.comb(nBeams, 2);

        this.baseLines = new ArrayList<BaseLine>(blen);
        this.wRanges = new ArrayList<Range>(blen);

        Beam b1, b2;

        double x, y, z, t, wMin, wMax;
        for (int i = 0; i < nBeams; i++) {
            for (int j = i + 1; j < nBeams; j++) {
                b1 = this.beams.get(i);
                b2 = this.beams.get(j);

                x = b1.getStation().getRelativePosition().getPosX() - b2.getStation().getRelativePosition().getPosX();
                y = b1.getStation().getRelativePosition().getPosY() - b2.getStation().getRelativePosition().getPosY();
                z = b1.getStation().getRelativePosition().getPosZ() - b2.getStation().getRelativePosition().getPosZ();

                // a Base line defines only the geometry :
                this.baseLines.add(new BaseLine(b1, b2, x, y, z));

                t = b1.getOpticalLength() - b2.getOpticalLength();

                // 2 DL for 2 telescopes => double throw :
                // note : for now, all DLs are equivalent (same throw) :
                wMin = t - b2.getDelayLine().getMaximumThrow();
                wMax = t + b1.getDelayLine().getMaximumThrow();

                // the range contains the w delay limits for the corresponding base line :
                this.wRanges.add(new Range(wMin, wMax));
                
                /* TODO: VCM limit in term of wSoftRange (soft limit) */
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

            if (isDebug) {
                logger.debug("nightLimits: {}", this.nightLimits);
            }
        }

        this.data.setSunIntervals(intervals);
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

            } else {
                // over LST 24 :

                // return [jd - day]
                return jd - AstroSkyCalc.LST_DAY_IN_JD;
            }

        } else {
            // start occurs before LST 0h :

            // return [jd + day]
            return jd + AstroSkyCalc.LST_DAY_IN_JD;
        }
    }

    /**
     * Convert a JD date to a date within LST range [0;24]
     *
     * @param jd date to convert
     * @return date
     */
    private Date convertJDToDate(final double jd) {
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
        } else {
            if (jd >= this.jdUpper) {
                return this.data.getDateMax();
            }
            return jdToDate(jd);
        }
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

        final List<Target> targets = new ArrayList<Target>((decMax - decMin) / decStep + 1);
        Target t;
        for (int i = decMax; i >= decMin; i -= decStep) {
            t = new Target();
            // delta = n (deg)
            t.setName(SpecialChars.DELTA_UPPER + " = " + Integer.toString(i));
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
                            addTargetPosition(targetPositions, cosDec, sinDec, azEl, jd);
                        }

                        jd = this.sc.convertHAToJD(haElev, precRA);
                        if (Range.contains(obsRangeJD, jd)) {
                            addTargetPosition(targetPositions, cosDec, sinDec, azEl, jd);
                        }
                    }
                }
            }

            // tick for transit :
            jd = this.sc.convertHAToJD(0d, precRA);
            if (Range.contains(obsRangeJD, jd)) {
                addTargetPosition(targetPositions, cosDec, sinDec, azEl, jd);
            }
        }

        // ticks for observability intervals (limits):
        for (Range range : obsRangeJD) {
            jd = range.getMin();
            addTargetPosition(targetPositions, cosDec, sinDec, azEl, jd);

            jd = range.getMax();
            addTargetPosition(targetPositions, cosDec, sinDec, azEl, jd);
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
     */
    private void addTargetPosition(final Map<Date, TargetPositionDate> targetPositions, final double cosDec, final double sinDec, final AzEl azEl, final double jd) {
        // fix JD in LST range [0; 24] in order to have accurate target position:
        final double jdIn = getJDInLstRange(jd);

        final double ha = this.sco.getTargetPosition(cosDec, sinDec, jdIn, azEl);

        final Date date = convertJDToDate(jd);
        targetPositions.put(date, new TargetPositionDate(date, ha, (int) Math.round(azEl.getAzimuth()), (int) Math.round(azEl.getElevation())));
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
                jdRanges.add(this.sc.convertHAToJDRange(range, precRA));
            }

            final List<DateTimeInterval> dateIntervals = new ArrayList<DateTimeInterval>(jdRanges.size() + 2);
            // convert JD ranges to date ranges :
            for (Range range : jdRanges) {
                convertRangeToDateInterval(range, dateIntervals);
            }
            if (dateIntervals.size() > 1) {
                // merge contiguous date ranges :
                DateTimeInterval.merge(dateIntervals);
            }

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
                // sort the intervals in reverse order :
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
     * @return HA range
     */
    private Range getTargetHALimits(final Target target) {
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

        return new Range(haMin, haMax);
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
     * @return new best PoPs estimator
     */
    private BestPopsEstimator getBestPopsEstimator(final Target target, final double haElev) {
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
                final Range haLimits = getTargetHALimits(target);
                // keep haCenter in [-haElev; haElev] range:
                final double haCenter = checkHA(haLimits.getCenter(), haElev);

                if (isLogDebug) {
                    logger.debug("haCenter: {} - haElev: {} - haLimits: {}", haCenter, haElev, haLimits);
                }

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

    /**
     * Simple RangeFactory implementation that estimate new Range or List<Range> instances
     */
    private final static class SimpleRangeFactory implements RangeFactory {

        /** created range instances */
        private int createdRanges = 0;
        /** created List<Range> instances */
        private int createdRangeLists = 0;

        /**
         * Return a Range instance with given minimum and maximum value
         * @param min minimum value
         * @param max maximum value
         * @return Range instance
         */
        public Range valueOf(final double min, final double max) {
            ++createdRanges;
            return new Range(min, max);
        }

        /**
         * Return a List<Range> instance
         * @return List<Range> instance
         */
        public List<Range> getList() {
            ++createdRangeLists;
            return new ArrayList<Range>(3); // max 3 intervals per BL
        }

        /**
         * Reset the factory state
         */
        public void reset() {
            createdRanges = 0;
            createdRangeLists = 0;
        }

        /**
         * Dump the factory statistics
         */
        public void dumpStats() {
            logger.info("RangeFactory: {} created ranges - {} created lists.", createdRanges, createdRangeLists);
        }
    }
}
