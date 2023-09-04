/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import edu.dartmouth.AstroSkyCalc;
import edu.dartmouth.AstroSkyCalcObservation;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.oitools.model.range.Range;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.observability.TargetPointInfo;
import fr.jmmc.aspro.model.oi.AzEl;
import fr.jmmc.aspro.model.oi.FocalInstrumentConfiguration;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.model.uvcoverage.UVBaseLineData;
import fr.jmmc.aspro.model.uvcoverage.UVCoverageData;
import fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData;
import fr.jmmc.aspro.util.AngleUtils;
import fr.jmmc.aspro.util.TestUtils;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import net.jafama.DoubleWrapper;
import net.jafama.FastMath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This service is dedicated to compute the UV tracks for a given target
 * @author bourgesl
 */
public final class UVCoverageService {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(UVCoverageService.class.getName());
    /** flag to slow down the service to detect concurrency problems */
    private final static boolean DEBUG_SLOW_SERVICE = false;
    /** safety limit for the number of sampled HA points = 500 */
    public static final int MAX_HA_POINTS = 500;
    /** 1s precision in HA */
    public static final double HA_PRECISION = 1.0 / 3600d;

    /* members */

 /* output */
    /** uv coverage data */
    private final UVCoverageData data;

    /* inputs */
    /** observation settings used  (read-only copy of the modifiable observation) */
    private final ObservationSetting observation;
    /** computed Observability Data (read-only) */
    private final ObservabilityData obsData;
    /** target to use */
    private final String targetName;
    /** maximum U or V coordinate in rad-1 (corrected by the minimal wavelength) */
    private double uvMaxFreq;
    /** flag to compute the UV support */
    private final boolean doUVSupport;
    /** true to use instrument bias; false to compute only theoretical error */
    private final boolean useInstrumentBias;
    /** flag to add gaussian noise to OIFits data */
    private final boolean doDataNoise;
    /** OIFits options */
    private final OIFitsProducerOptions options;
    /** cosinus wrapper used by FastMath.sinAndCos() */
    private final DoubleWrapper cw = new DoubleWrapper();
    /* internal */
    /** Get the current thread to check if the computation is interrupted */
    private final Thread currentThread = Thread.currentThread();
    /** hour angle step (see samplingPeriod) */
    private double haStep;
    /** observation time expressed in hour angle (see acquisitionTime) */
    private double haObsTime;
    /** reference wavelength of the selected instrument mode (meter) */
    private double lambda;
    /** lower wavelength of the selected instrument mode (meter) */
    private double lambdaMin;
    /** upper wavelength of the selected instrument mode (meter) */
    private double lambdaMax;
    /** selected target */
    private Target target = null;
    /** selected instrument mode */
    private FocalInstrumentMode instrumentMode = null;
    /** observation sky calc instance */
    private final AstroSkyCalcObservation sco = new AstroSkyCalcObservation();

    /* reused observability data */
    /** sky calc instance */
    private AstroSkyCalc sc = null;
    /** beam list */
    private List<Beam> beams = null;
    /** base line list */
    private List<BaseLine> baseLines = null;
    /** star data */
    private StarData starData = null;

    /**
     * Constructor.
     * Note : This service is statefull so it can not be reused by several calls.
     *
     * @param observation observation settings
     * @param obsData computed observability data
     * @param targetName target name
     * @param uvMax U-V max in meter
     * @param doUVSupport flag to compute the UV support
     * @param useInstrumentBias true to use instrument bias; false to compute only theoretical error
     * @param doDataNoise enable data noise
     * @param options OIFits options
     */
    public UVCoverageService(final ObservationSetting observation, final ObservabilityData obsData, final String targetName,
                             final double uvMax, final boolean doUVSupport, final boolean useInstrumentBias, final boolean doDataNoise,
                             final OIFitsProducerOptions options) {

        this.observation = observation;
        this.obsData = obsData;
        this.targetName = targetName;
        this.uvMaxFreq = uvMax;
        this.doUVSupport = doUVSupport;
        this.useInstrumentBias = useInstrumentBias;
        this.doDataNoise = doDataNoise;
        this.options = options;

        // create the uv coverage data corresponding to the observation version :
        this.data = new UVCoverageData(observation.getVersion());
    }

    /**
     * Main operation to compute the UV tracks for a given target
     *
     * @return UVCoverageData container
     */
    public UVCoverageData compute() {
        if (logger.isDebugEnabled()) {
            logger.debug("compute: {}", this.observation);
        }

        // Start the computations :
        final long start = System.nanoTime();

        // Get instrument and observability data :
        prepareObservation();

        if (this.starData != null) {
            // Note : for Baseline limits, the starData is null
            // (target observability is not available) :

            // target name :
            this.data.setTargetName(this.targetName);

            // reference wave length :
            this.data.setLambda(this.lambda);

            // Is the target visible :
            if (this.starData.getHaElev() > 0d) {
                // define site :
                this.sco.defineSite(this.sc);

                // get Target coordinates precessed to JD CENTER and define target to get later az/alt positions from JSkyCalc :
                this.sco.defineTarget(this.obsData.jdCenter(), target.getRADeg(), target.getDECDeg(), target.getPMRA(), target.getPMDEC());

                if (this.doUVSupport) {
                    computeUVSupport();
                }

                computeObservableUV();

                // fast interrupt :
                if (this.currentThread.isInterrupted()) {
                    return null;
                }

                // prepare OIFits computation :
                createOIFits();

                // reset current target :
                this.sco.reset();
            }

            // fast interrupt :
            if (this.currentThread.isInterrupted()) {
                return null;
            }
            if (logger.isDebugEnabled()) {
                logger.debug("UV coordinate maximum: {}", this.uvMaxFreq);
            }

            // uv Max = max base line / minimum wave length
            this.data.setUvMaxFreq(this.uvMaxFreq);

            // fast interrupt :
            if (this.currentThread.isInterrupted()) {
                return null;
            }

        } // starData is defined

        // fast interrupt :
        if (this.currentThread.isInterrupted()) {
            return null;
        }

        if (DEBUG_SLOW_SERVICE) {
            TestUtils.busyWait(2000l);

            if (this.currentThread.isInterrupted()) {
                return null;
            }
        }

        logger.info("compute : duration = {} ms.", 1e-6d * (System.nanoTime() - start));

        return this.data;
    }

    /**
     * Compute UV tracks using only rise/set intervals
     */
    private void computeUVSupport() {

        final double haElev = this.starData.getHaElev();

        // 1 minute is fine to get pretty ellipse :
        final double step = 1.0 / 60d;

        final int nPoints = (int) Math.round(2.0 * haElev / step) + 1;

        // precessed target declination in rad :
        final double precDEC = FastMath.toRadians(this.starData.getPrecDEC());

        // compute once cos/sin DEC:
        final double cosDec = FastMath.cos(precDEC);
        final double sinDec = FastMath.sin(precDEC);

        final double invLambda = 1.0 / this.lambda;

        final List<BaseLine> _baseLines = this.baseLines;
        final int sizeBL = _baseLines.size();
        final List<UVBaseLineData> targetUVRiseSet = new ArrayList<UVBaseLineData>(sizeBL);

        BaseLine baseLine;
        /* U,V coordinates corrected with central wavelength */
        double[] u;
        double[] v;

        final DoubleWrapper _cw = cw;
        double cosHa, sinHa;

        for (int i = 0, j; i < sizeBL; i++) {
            baseLine = _baseLines.get(i);

            u = new double[nPoints];
            v = new double[nPoints];

            j = 0;

            for (double ha = -haElev; ha <= haElev; ha += step) {

                sinHa = FastMath.sinAndCos(AngleUtils.hours2rad(ha), _cw); // cw holds cosine
                cosHa = _cw.value;

                // Baseline projected vector (m) :
                u[j] = CalcUVW.computeU(baseLine, cosHa, sinHa);
                v[j] = CalcUVW.computeV(cosDec, sinDec, baseLine, cosHa, sinHa);

                // wavelength correction :
                // Spatial frequency (xLambda) :
                u[j] *= invLambda;
                v[j] *= invLambda;

                j++;
            }

            targetUVRiseSet.add(
                    new UVBaseLineData(baseLine.getName(), j, u, v)
            );

            // fast interrupt :
            if (this.currentThread.isInterrupted()) {
                return;
            }
        }

        this.data.setTargetUVRiseSet(targetUVRiseSet);
    }

    /**
     * Compute UV points (observable) inside HA min/max ranges
     */
    private void computeObservableUV() {

        final List<Range> obsRangesHA = this.starData.getObsRangesHA();

        if (logger.isDebugEnabled()) {
            logger.debug("obsRangesHA: {}", obsRangesHA);
        }

        if (obsRangesHA != null) {

            // use observable HA bounds:
            final Double haLower = Range.getMinimum(obsRangesHA);
            final Double haUpper = Range.getMaximum(obsRangesHA);

            if (haLower != null && haUpper != null) {

                final double haMin = haLower.doubleValue();
                final double haMax = haUpper.doubleValue();

                if (logger.isDebugEnabled()) {
                    logger.debug("HA min/Max: {} - {}", haMin, haMax);
                    logger.debug("HA ObsTime: {}", haObsTime);
                }

                // precessed target right ascension in rad :
                final double precRA = this.starData.getPrecRA();

                // precessed target declination in rad :
                final double precDEC = FastMath.toRadians(this.starData.getPrecDEC());

                // compute once cos/sin DEC:
                final double cosDec = FastMath.cos(precDEC);
                final double sinDec = FastMath.sin(precDEC);

                final AzEl azEl = new AzEl();

                // Fix JD offset at haMin:
                final double jdOffset = this.obsData.getJDOffset(this.sc.convertHAToJD(haMin, precRA));

                // step is given in solar time so correct by the sideral / solar ratio:
                final double step = AstroSkyCalc.solar2lst(this.haStep);

                // estimate the number of HA points :
                final int capacity = (int) Math.round((haMax - haMin) / step) + 1;

                // First pass : find observable HA values :
                // use safety limit to avoid out of memory errors :
                final int haLen = (capacity > MAX_HA_POINTS) ? MAX_HA_POINTS : capacity;
                final TargetPointInfo[] ptInfos = new TargetPointInfo[haLen];

                Range obsRange;
                TargetPointInfo targetPointInfo;
                int j = 0;

                // Traverse all observable HA range to find possible observing blocks (haObsTime duration):
                for (double ha = haMin; ha <= haMax; ha += step) {

                    // check HA start:
                    if ((obsRange = Range.find(obsRangesHA, ha, HA_PRECISION)) != null) {
                        // check HA end:
                        if (obsRange.contains(ha + haObsTime, HA_PRECISION)) {

                            targetPointInfo = createTargetInfo(cosDec, sinDec, precRA, azEl, ha, jdOffset);
                            if (logger.isDebugEnabled()) {
                                logger.debug("info: {}", targetPointInfo);
                            }

                            ptInfos[j++] = targetPointInfo;

                            // check safety limit :
                            if (j >= MAX_HA_POINTS) {
                                addWarning("Too many HA points (" + capacity + "), check your sampling periodicity. Only " + MAX_HA_POINTS + " samples computed");
                                break;
                            }
                        } else if (logger.isDebugEnabled()) {
                            logger.debug("Observation HA range [{}; {}]end exceed observable range: {}", ha, ha + haObsTime, obsRange);
                        }
                    }
                }

                // correct number of HA points :
                final int nPoints = j;

                this.data.setNPoints(nPoints);

                // check if there is at least one observable HA :
                if (nPoints == 0) {
                    addWarning("Check your HA min/max settings. There is no observable HA");
                    return;
                }

                // Copy only valid data points:
                final TargetPointInfo[] targetPointInfos = new TargetPointInfo[nPoints];
                System.arraycopy(ptInfos, 0, targetPointInfos, 0, nPoints);

                this.data.setTargetPointInfos(targetPointInfos);

                if (logger.isDebugEnabled()) {
                    logger.debug("lambdaMin: {}", lambdaMin);
                    logger.debug("lambdaMax: {}", lambdaMax);
                }

                // Second pass : extract UV values for HA points :
                final double invLambdaMin = 1.0 / lambdaMin;
                final double invLambdaMax = 1.0 / lambdaMax;

                final List<BaseLine> _baseLines = this.baseLines;
                final int sizeBL = _baseLines.size();
                final List<UVRangeBaseLineData> targetUVObservability = new ArrayList<UVRangeBaseLineData>(sizeBL);

                BaseLine baseLine;

                /* pure U,V coordinates (m) */
                double[] u;
                double[] v;
                /* U,V coordinates corrected with minimal wavelength */
                double[] uWMin;
                double[] vWMin;
                /* U,V coordinates corrected with maximal wavelength */
                double[] uWMax;
                double[] vWMax;

                final DoubleWrapper _cw = cw;
                double cosHa, sinHa;

                for (int i = 0; i < sizeBL; i++) {
                    baseLine = _baseLines.get(i);

                    u = new double[nPoints];
                    v = new double[nPoints];
                    uWMin = new double[nPoints];
                    vWMin = new double[nPoints];
                    uWMax = new double[nPoints];
                    vWMax = new double[nPoints];

                    for (j = 0; j < nPoints; j++) {

                        sinHa = FastMath.sinAndCos(
                                AngleUtils.hours2rad(targetPointInfos[j].getHa()),
                                _cw); // cw holds cosine
                        cosHa = _cw.value;

                        // Baseline projected vector (m) :
                        u[j] = CalcUVW.computeU(baseLine, cosHa, sinHa);
                        v[j] = CalcUVW.computeV(cosDec, sinDec, baseLine, cosHa, sinHa);

                        // wavelength correction :
                        // Spatial frequency (rad-1) :
                        uWMin[j] = u[j] * invLambdaMin;
                        vWMin[j] = v[j] * invLambdaMin;

                        uWMax[j] = u[j] * invLambdaMax;
                        vWMax[j] = v[j] * invLambdaMax;
                    }

                    targetUVObservability.add(
                            new UVRangeBaseLineData(baseLine, nPoints, u, v, uWMin, vWMin, uWMax, vWMax)
                    );

                    // fast interrupt :
                    if (this.currentThread.isInterrupted()) {
                        return;
                    }
                }

                this.data.setTargetUVObservability(targetUVObservability);
            }
        }
    }

    /**
     * Define the baselines, star data and instrument mode's wavelengths
     * @throws IllegalStateException if the instrument mode is undefined
     */
    private void prepareObservation() throws IllegalStateException {
        // Get AstroSkyCalc instance :
        this.sc = this.obsData.getDateCalc();
        // Get beams :
        this.beams = this.obsData.getBeams();
        // Get baselines :
        this.baseLines = this.obsData.getBaseLines();
        // Copy baseLines :
        this.data.setBaseLines(this.obsData.getBaseLines());

        // Get starData for the selected target name :
        this.starData = this.obsData.getStarData(this.targetName);

        // get current target :
        this.target = this.observation.getTarget(this.targetName);

        if (logger.isDebugEnabled()) {
            logger.debug("starData: {}", this.starData);
            logger.debug("target: {}", this.target);
        }

        // use lower wavelength of all instrument modes:
        final double instrumentMinWaveLength = AsproConstants.MICRO_METER
                * this.observation.getInstrumentConfiguration().getInstrumentConfiguration().getFocalInstrument().getWaveLengthMin();

        this.instrumentMode = this.observation.getInstrumentConfiguration().getFocalInstrumentMode();
        if (instrumentMode == null) {
            throw new IllegalStateException("The instrumentMode is empty !");
        }

        // use target's wavelength ref instead of default value:
        this.lambda = instrumentMode.getEffWaveLengthRef();

        boolean useWavelengthRangeRestriction = instrumentMode.isWavelengthRangeRestriction();
        double effBand = instrumentMode.getEffWaveLengthBandRef();

        if (target != null) {
            final TargetConfiguration targetConf = target.getConfiguration();

            if (targetConf != null && targetConf.getInstrumentWaveLengthRef() != null) {
                lambda = targetConf.getInstrumentWaveLengthRef();
            }

            if (targetConf != null && targetConf.getFringeTrackerMode() != null) {
                final String ftMode = targetConf.getFringeTrackerMode();

                // TODO: handle FT modes properly: GroupTrack is hard coded !
                // disable wavelength restrictions if FT enabled (basic GRA4MAT support, TODO: refine wavelength ranges for GRA4MAT)
                useWavelengthRangeRestriction = !((ftMode != null) && !ftMode.startsWith("GroupTrack") && (instrumentMode.getFtWaveLengthBandRef() == null));
                if (useWavelengthRangeRestriction && (instrumentMode.getFtWaveLengthBandRef() != null)) {
                    effBand = instrumentMode.getFtWaveLengthBandRef();
                }
            }
        }

        // Get wavelength range for the selected instrument mode :
        final Range wlRange = new Range();
        lambda = AsproConstants.MICRO_METER * instrumentMode.getEffectiveWavelengthRange(lambda, useWavelengthRangeRestriction, effBand, wlRange);
        lambdaMin = AsproConstants.MICRO_METER * wlRange.getMin();
        lambdaMax = AsproConstants.MICRO_METER * wlRange.getMax();

        if (logger.isDebugEnabled()) {
            logger.debug("instrumentMode: {}", instrumentMode.getName());
            logger.debug("lambda:    {}", this.lambda);
            logger.debug("wlRange:   {}", wlRange);
        }

        // hour angle step in decimal hours :
        this.haStep = this.observation.getInstrumentConfiguration().getSamplingPeriod() / 60d;

        if (logger.isDebugEnabled()) {
            logger.debug("ha step: {}", this.haStep);
        }

        // get acquisition time to ensure sampled HA intervals [HA; HA+obsTime] is within observable range
        haObsTime = observation.getInstrumentConfiguration().getAcquisitionTime().doubleValue() / 3600d;

        // Adjust the user uv Max = max base line / lower wavelength of the selected instrument
        // note : use the lower wave length of the instrument to
        // - make all uv segment visible
        // - avoid to much model computations (when the instrument mode changes)
        this.uvMaxFreq /= instrumentMinWaveLength;

        if (logger.isDebugEnabled()) {
            // Define precisely the maxUV for maxBaselines:
            final FocalInstrumentConfiguration insConf = observation.getInstrumentConfiguration().getInstrumentConfiguration();

            final double maxBaseLines = ConfigurationManager.getInstrumentConfigurationMaxBaseline(insConf,
                    observation.getInstrumentConfiguration().getStations());

            logger.debug("instrument configuration: {}; baseline max = {}", observation.getInstrumentConfiguration().getStations(), maxBaseLines);
            logger.debug("uvMaxFreq: {}", this.uvMaxFreq);
        }
    }

    /**
     * Create the OIFits structure (array, target, wave lengths and visibilities)
     */
    private void createOIFits() {
        if (this.data.getTargetUVObservability() == null) {
            addWarning("OIFits data not available");
        } else {
            // thread safety : TODO: observation can change ... extract observation info in prepare ??
            if (target != null) {
                // Create the OIFitsCreatorService / NoiseService :

                // note: OIFitsCreatorService parameter dependencies:
                // observation {target, instrumentMode}
                // obsData {beams, baseLines, starData, sc (DateCalc)}
                // parameter: supersamplingOIFits, doDataNoise, useInstrumentBias
                // results: computeObservableUV {HA, targetUVObservability} {obsData + observation{haMin/haMax, instrumentMode {lambdaMin, lambdaMax}}}
                // and warning container
                try {
                    final OIFitsCreatorService oiFitsCreator = new OIFitsCreatorService(this.observation, target,
                            this.beams, this.baseLines,
                            this.useInstrumentBias, this.doDataNoise,
                            this.options,
                            this.data.getTargetPointInfos(), this.data.getTargetUVObservability(),
                            this.sc, this.data.getWarningContainer());

                    // TODO: create elsewhere the OIFitsCreatorService:
                    this.data.setOiFitsCreator(oiFitsCreator);

                    // get noise service to compute noise on model image (if enabled):
                    this.data.setNoiseService(oiFitsCreator.getNoiseService());
                } catch (IllegalArgumentException iae) {
                    addWarning(iae.getMessage());
                }
            }
        }
    }

    /**
     * Add a warning message in the OIFits file
     * @param msg message to add
     */
    private void addWarning(final String msg) {
        this.data.getWarningContainer().addWarning(msg);
    }

    /**
     * Compute UV points (observable) inside HA min/max ranges
     * @param observation observation settings
     * @param obsData computed observability data
     * @param starData star data
     * @param ha hour angle to compute UV points
     * @return list of uv point couples corresponding to the target observability
     * 
     * @throws IllegalStateException if the instrument mode is undefined
     */
    public static List<UVRangeBaseLineData> computeUVPoints(final ObservationSetting observation,
                                                            final ObservabilityData obsData, final StarData starData,
                                                            final double ha) throws IllegalStateException {

        // Compute UV points at given HA:
        final List<Range> obsRangesHA = starData.getObsRangesHA();

        if (logger.isDebugEnabled()) {
            logger.debug("obsRangesHA: {}", obsRangesHA);
        }

        final List<UVRangeBaseLineData> targetUVObservability;
        Range obsRange = null;

        if (obsRangesHA != null && (obsRange = Range.find(obsRangesHA, ha, HA_PRECISION)) != null) {

            // Prepare informations:
            // Get baselines :
            final List<BaseLine> baseLines = obsData.getBaseLines();

            final FocalInstrumentMode instrumentMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();
            if (instrumentMode == null) {
                throw new IllegalStateException("The instrumentMode is empty !");
            }

            if (logger.isDebugEnabled()) {
                logger.debug("instrumentMode: {}", instrumentMode.getName());
            }

            // get current target :
            final Target target = observation.getTarget(starData.getName());

            // use target's wavelength ref instead of default value:
            double lambda = instrumentMode.getEffWaveLengthRef();

            boolean useWavelengthRangeRestriction = instrumentMode.isWavelengthRangeRestriction();
            double effBand = instrumentMode.getEffWaveLengthBandRef();

            if (target != null) {
                final TargetConfiguration targetConf = target.getConfiguration();

                if (targetConf != null && targetConf.getInstrumentWaveLengthRef() != null) {
                    lambda = targetConf.getInstrumentWaveLengthRef();
                }

                if (targetConf != null && targetConf.getFringeTrackerMode() != null) {
                    final String ftMode = targetConf.getFringeTrackerMode();

                    // TODO: handle FT modes properly: GroupTrack is hard coded !
                    // disable wavelength restrictions if FT enabled (basic GRA4MAT support, TODO: refine wavelength ranges for GRA4MAT)
                    // but wrong if FT is disabled by noise service:
                    useWavelengthRangeRestriction = !((ftMode != null) && !ftMode.startsWith("GroupTrack") && (instrumentMode.getFtWaveLengthBandRef() == null));
                    if (useWavelengthRangeRestriction && (instrumentMode.getFtWaveLengthBandRef() != null)) {
                        effBand = instrumentMode.getFtWaveLengthBandRef();
                    }
                }
            }

            // Get wavelength range for the selected instrument mode :
            final Range wlRange = new Range();
            instrumentMode.getEffectiveWavelengthRange(lambda, useWavelengthRangeRestriction, effBand, wlRange);
            final double lambdaMin = AsproConstants.MICRO_METER * wlRange.getMin();
            final double lambdaMax = AsproConstants.MICRO_METER * wlRange.getMax();

            if (logger.isDebugEnabled()) {
                logger.debug("lambdaMin: {}", lambdaMin);
                logger.debug("lambdaMax: {}", lambdaMax);
            }

            final double invLambdaMin = 1.0 / lambdaMin;
            final double invLambdaMax = 1.0 / lambdaMax;

            // extract UV values for HA point:
            // precessed target declination in rad :
            final double precDEC = FastMath.toRadians(starData.getPrecDEC());

            // compute once cos/sin DEC:
            final double cosDec = FastMath.cos(precDEC);
            final double sinDec = FastMath.sin(precDEC);

            final int sizeBL = baseLines.size();
            targetUVObservability = new ArrayList<UVRangeBaseLineData>(sizeBL);

            BaseLine baseLine;

            // compute once cos/sin HA:
            final double haRad = AngleUtils.hours2rad(ha);
            final double sinHa = FastMath.sin(haRad);
            final double cosHa = FastMath.cos(haRad);

            for (int i = 0; i < sizeBL; i++) {
                baseLine = baseLines.get(i);

                // Baseline projected vector (m) :
                /* pure U,V coordinates (m) */
                final double u = CalcUVW.computeU(baseLine, cosHa, sinHa);
                final double v = CalcUVW.computeV(cosDec, sinDec, baseLine, cosHa, sinHa);

                // wavelength correction :
                // Spatial frequency (rad-1) :
                /* U,V coordinates corrected with minimal wavelength */
                final double uWMin = u * invLambdaMin;
                final double vWMin = v * invLambdaMin;

                /* U,V coordinates corrected with maximal wavelength */
                final double uWMax = u * invLambdaMax;
                final double vWMax = v * invLambdaMax;

                targetUVObservability.add(
                        new UVRangeBaseLineData(baseLine, u, v, uWMin, vWMin, uWMax, vWMax)
                );
            }

        } else {
            if (logger.isDebugEnabled()) {
                logger.debug("Observation HA [{}] out of observable range: {}", ha, obsRange);
            }
            targetUVObservability = null;
        }
        return targetUVObservability;
    }

    /**
     * Create a new target point information (azimuth and elevation) for the current target and julian date
     * @param cosDec cosinus of target declination
     * @param sinDec sinus of target declination
     * @param precRA precessed RA of target
     * @param azEl AzEl instance
     * @param ha current hour angle
     * @param jdOffset offset on julian date to stay within night range
     * @return target point information
     */
    private TargetPointInfo createTargetInfo(final double cosDec, final double sinDec,
                                             final double precRA,
                                             final AzEl azEl,
                                             final double ha,
                                             final double jdOffset) {

        // fix JD in night range in order to have accurate date:
        final double jd = this.sc.convertHAToJD(ha, precRA) + jdOffset;

        this.sco.getTargetPosition(cosDec, sinDec, jd, azEl);

        final Date date = this.sc.toDate(jd, this.obsData.getTimeRef()); // LST or GMT

        final double airmass = AstroSkyCalc.airmass(azEl.getElevation());

        return new TargetPointInfo(jd, ha, date, azEl.getAzimuth(), azEl.getElevation(), airmass);
    }
}
