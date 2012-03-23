/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.uvcoverage.UVCoverageData;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.model.uvcoverage.UVBaseLineData;
import fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData;
import fr.jmmc.aspro.util.AngleUtils;
import fr.jmmc.aspro.util.TestUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

/**
 * This service is dedicated to compute the UV tracks for a given target
 * @author bourgesl
 */
public final class UVCoverageService {

  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(UVCoverageService.class.getName());
  /** flag to slow down the service to detect concurrency problems */
  private final static boolean DEBUG_SLOW_SERVICE = false;
  /** safety limit for the number of sampled HA points = 500 */
  public static final int MAX_HA_POINTS = 500;

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
  /** maximum U or V coordinate (corrected by the minimal wavelength) */
  private double uvMax;
  /** flag to compute the UV support */
  private final boolean doUVSupport;
  /** flag to add gaussian noise to OIFits data */
  private final boolean doDataNoise;

  /* internal */
  /** Get the current thread to check if the computation is interrupted */
  private final Thread currentThread = Thread.currentThread();
  /** hour angle step (see samplingPeriod) */
  private double haStep;
  /** lower wavelength of the selected instrument (meter) */
  private double instrumentMinWaveLength;
  /** minimal wavelength of the selected instrument mode (meter) */
  private double lambdaMin;
  /** central wavelength of the selected instrument mode (meter) */
  private double lambda;
  /** maximal wavelength of the selected instrument mode (meter) */
  private double lambdaMax;
  /** number of spectral channels (used by OIFits) */
  private int nSpectralChannels;
  /** HA min in decimal hours */
  private double haMin = AsproConstants.HA_MIN;
  /** HA max in decimal hours */
  private double haMax = AsproConstants.HA_MAX;

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
   * @param doDataNoise enable data noise
   */
  public UVCoverageService(final ObservationSetting observation, final ObservabilityData obsData, final String targetName,
          final double uvMax, final boolean doUVSupport, final boolean doDataNoise) {
    this.observation = observation;
    this.obsData = obsData;
    this.targetName = targetName;
    this.uvMax = uvMax;
    this.doUVSupport = doUVSupport;
    this.doDataNoise = doDataNoise;

    // create the uv coverage data corresponding to the observation version :
    this.data = new UVCoverageData(observation.getVersion());
  }

  /**
   * Main operation to compute the UV tracks for a given target
   *
   * @return UVCoverageData container
   */
  public UVCoverageData compute() {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("compute : " + this.observation);
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

      // central wave length :
      this.data.setLambda(this.lambda);

      // Is the target visible :
      if (this.starData.getHaElev() <= 0d) {
        addWarning("The target [" + this.targetName + "] is not observable (never rise)");
      } else {

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
      }

      // fast interrupt :
      if (this.currentThread.isInterrupted()) {
        return null;
      }
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("UV coordinate maximum = [" + this.uvMax + "]");
      }

      // uv Max = max base line / minimum wave length
      this.data.setUvMax(this.uvMax);

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

    if (logger.isLoggable(Level.INFO)) {
      logger.info("compute : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");
    }

    return this.data;
  }

  /**
   * Compute UV tracks using only rise/set intervals
   */
  private void computeUVSupport() {

    final double haElev = this.starData.getHaElev();

    // 1 minute is fine to get pretty ellipse :
    final double step = 1d / 60d;

    final int nPoints = (int) Math.round(2d * haElev / step) + 1;

    // precessed target declination in rad :
    final double precDEC = Math.toRadians(this.starData.getPrecDEC());

    final double cosDec = Math.cos(precDEC);
    final double sinDec = Math.sin(precDEC);

    final int sizeBL = this.baseLines.size();
    final List<UVBaseLineData> targetUVRiseSet = new ArrayList<UVBaseLineData>(sizeBL);

    UVBaseLineData uvData;
    BaseLine baseLine;
    /* U,V coordinates corrected with central wavelength */
    double[] u;
    double[] v;

    double haRad;

    for (int i = 0, j = 0; i < sizeBL; i++) {
      baseLine = this.baseLines.get(i);

      uvData = new UVBaseLineData(baseLine.getName());

      u = new double[nPoints];
      v = new double[nPoints];

      j = 0;

      for (double ha = -haElev; ha <= haElev; ha += step) {

        haRad = AngleUtils.hours2rad(ha);

        // Baseline projected vector (m) :
        u[j] = CalcUVW.computeU(baseLine, haRad);
        v[j] = CalcUVW.computeV(cosDec, sinDec, baseLine, haRad);

        // wavelength correction :

        // Spatial frequency (rad-1) :
        u[j] /= this.lambda;
        v[j] /= this.lambda;

        j++;
      }

      uvData.setNPoints(j);
      uvData.setU(u);
      uvData.setV(v);

      targetUVRiseSet.add(uvData);

      // fast interrupt :
      if (this.currentThread.isInterrupted()) {
        return;
      }
    }

    this.data.setTargetUVRiseSet(targetUVRiseSet);
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
   * Compute UV points (observable) inside HA min/max ranges
   */
  private void computeObservableUV() {

    final List<Range> obsRangesHA = this.starData.getObsRangesHA();

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("obsRangesHA = " + obsRangesHA);
    }

    if (obsRangesHA == null) {
      addWarning("The target [" + this.targetName + "] is not observable");
    } else {

      final double haElev = this.starData.getHaElev();

      final double haLower = checkHA(this.haMin, haElev);
      final double haUpper = checkHA(this.haMax, haElev);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("HA min/Max = " + haLower + " - " + haUpper);
      }

      final double step = this.haStep;

      // estimate the number of HA points :
      final int capacity = (int) Math.round((haUpper - haLower) / step) + 1;

      // First pass : find observable HA values :

      // use safety limit to avoid out of memory errors :
      final double[] haValues = new double[(capacity > MAX_HA_POINTS) ? MAX_HA_POINTS : capacity];

      int j = 0;
      for (double ha = haLower; ha <= haUpper; ha += step) {

        // check HA :
        if (checkObservability(ha, obsRangesHA)) {
          haValues[j] = ha;
          j++;

          // check safety limit :
          if (j >= MAX_HA_POINTS) {
            addWarning("Too many HA points (" + capacity + "), check your sampling periodicity. Only " + MAX_HA_POINTS + " samples computed");
            break;
          }
        }
      }

      // correct number of HA points :
      final int nPoints = j;

      // check if there is at least one observable HA :
      if (nPoints == 0) {
        addWarning("Check your HA min/max settings. There is no observable HA");
        return;
      }

      final double[] HA = new double[nPoints];
      System.arraycopy(haValues, 0, HA, 0, nPoints);

      this.data.setHA(HA);

      // Second pass : extract UV values for HA points :

      // precessed target declination in rad :
      final double precDEC = Math.toRadians(this.starData.getPrecDEC());

      final double cosDec = Math.cos(precDEC);
      final double sinDec = Math.sin(precDEC);

      final int sizeBL = this.baseLines.size();
      final List<UVRangeBaseLineData> targetUVObservability = new ArrayList<UVRangeBaseLineData>(sizeBL);

      UVRangeBaseLineData uvData;
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

      double haRad;

      for (int i = 0; i < sizeBL; i++) {
        baseLine = this.baseLines.get(i);

        uvData = new UVRangeBaseLineData(baseLine);

        u = new double[nPoints];
        v = new double[nPoints];
        uWMin = new double[nPoints];
        vWMin = new double[nPoints];
        uWMax = new double[nPoints];
        vWMax = new double[nPoints];

        for (j = 0; j < nPoints; j++) {
          haRad = AngleUtils.hours2rad(HA[j]);

          // Baseline projected vector (m) :
          u[j] = CalcUVW.computeU(baseLine, haRad);
          v[j] = CalcUVW.computeV(cosDec, sinDec, baseLine, haRad);

          // wavelength correction :

          // Spatial frequency (rad-1) :
          uWMin[j] = u[j] / this.lambdaMin;
          vWMin[j] = v[j] / this.lambdaMin;

          uWMax[j] = u[j] / this.lambdaMax;
          vWMax[j] = v[j] / this.lambdaMax;
        }

        uvData.setNPoints(nPoints);
        uvData.setU(u);
        uvData.setV(v);
        uvData.setUWMin(uWMin);
        uvData.setVWMin(vWMin);
        uvData.setUWMax(uWMax);
        uvData.setVWMax(vWMax);

        targetUVObservability.add(uvData);

        // fast interrupt :
        if (this.currentThread.isInterrupted()) {
          return;
        }
      }

      this.data.setTargetUVObservability(targetUVObservability);
    }
  }

  /**
   * Check if the given hour angle is observable
   * @param ha decimal hour angle
   * @param obsRangesHA observable ranges
   * @return true if observable
   */
  private boolean checkObservability(final double ha, final List<Range> obsRangesHA) {
    for (Range range : obsRangesHA) {
      if (ha >= range.getMin() && ha <= range.getMax()) {
        return true;
      }
    }
    return false;
  }

  /**
   * Define the baselines, star data and instrument mode's wavelengths
   * @throws IllegalStateException if the instrument mode is undefined
   */
  private void prepareObservation() throws IllegalStateException {
    // Get AstroSkyCalc instance :
    this.sc = this.obsData.getDateCalc();
    // Copy station names :
    this.data.setStationNames(this.obsData.getStationNames());
    // Get beams :
    this.beams = this.obsData.getBeams();
    // Get baselines :
    this.baseLines = this.obsData.getBaseLines();
    // Copy baseLines :
    this.data.setBaseLines(this.obsData.getBaseLines());

    // Get starData for the selected target name :
    this.starData = this.obsData.getStarData(this.targetName);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("starData : " + this.starData);
    }

    // Get lower wavelength for the selected instrument:
    this.instrumentMinWaveLength = AsproConstants.MICRO_METER
            * this.observation.getInstrumentConfiguration().getInstrumentConfiguration().getFocalInstrument().getWaveLengthMin();

    final FocalInstrumentMode insMode = this.observation.getInstrumentConfiguration().getFocalInstrumentMode();
    if (insMode == null) {
      throw new IllegalStateException("The instrumentMode is empty !");
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("instrumentMode : " + insMode.getName());
    }

    // Get wavelength range for the selected instrument mode :
    this.lambdaMin = AsproConstants.MICRO_METER * insMode.getWaveLengthMin();
    this.lambdaMax = AsproConstants.MICRO_METER * insMode.getWaveLengthMax();
    this.lambda = AsproConstants.MICRO_METER * insMode.getWaveLength();

    this.nSpectralChannels = insMode.getEffectiveNumberOfChannels();

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("lambdaMin : " + this.lambdaMin);
      logger.fine("lambda    : " + this.lambda);
      logger.fine("lambdaMax : " + this.lambdaMax);
      logger.fine("nChannels : " + this.nSpectralChannels);
    }

    // HA Min / Max :
    final TargetConfiguration targetConf = this.observation.getTargetConfiguration(targetName);
    if (targetConf != null) {
      if (targetConf.getHAMin() != null) {
        this.haMin = targetConf.getHAMin().doubleValue();
      }
      if (targetConf.getHAMax() != null) {
        this.haMax = targetConf.getHAMax().doubleValue();
      }
    }
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("ha min    : " + this.haMin);
      logger.fine("ha max    : " + this.haMax);
    }

    // hour angle step in decimal hours :
    this.haStep = this.observation.getInstrumentConfiguration().getSamplingPeriod() / 60d;

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("ha step   : " + this.haStep);
    }

    // Adjust the user uv Max = max base line / minimum wave length
    // note : use the minimum wave length of the instrument to 
    // - make all uv segment visible
    // - avoid to much model computations (when the instrument mode changes)
    this.uvMax /= this.instrumentMinWaveLength;

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Corrected uvMax : " + this.uvMax);
    }
  }

  /**
   * Create the OIFits structure (array, target, wave lengths and visibilities)
   */
  private void createOIFits() {
    final List<UVRangeBaseLineData> targetUVObservability = this.data.getTargetUVObservability();

    if (targetUVObservability == null) {
      addWarning("OIFits data not available");
    } else {
      // thread safety : TODO: observation can change ... extract observation info in prepare ??

      // get current target :
      final Target target = this.observation.getTarget(this.targetName);

      if (target != null) {
        // Create the OIFitsCreatorService / NoiseService :
        
        // note: OIFitsCreatorService parameter dependencies:
        // observation {target, instrumentMode {lambdaMin, lambdaMax, nSpectralChannels}}
        // obsData {beams, baseLines, starData, sc (DateCalc)}
        // parameter: doDataNoise
        // results: computeObservableUV {HA, targetUVObservability} and warning container
        
        final OIFitsCreatorService oiFitsCreator = new OIFitsCreatorService(this.observation, target,
                this.beams, this.baseLines, this.lambdaMin, this.lambdaMax, this.nSpectralChannels, this.doDataNoise,
                this.data.getHA(), targetUVObservability, this.starData.getPrecRA(), this.sc,
                this.data.getWarningContainer());

        // TODO: create elsewhere the OIFitsCreatorService:
        this.data.setOiFitsCreator(oiFitsCreator);

        if (oiFitsCreator.isDoNoise()) {
          this.data.setNoiseService(oiFitsCreator.getNoiseService());
        }
      }
    }
  }

  /**
   * Add a warning message in the OIFits file
   * @param msg message to add
   */
  private void addWarning(final String msg) {
    this.data.getWarningContainer().addWarningMessage(msg);
  }
}
