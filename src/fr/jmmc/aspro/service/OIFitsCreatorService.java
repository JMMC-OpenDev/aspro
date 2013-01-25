/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.oi.FocalInstrument;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Position3D;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.Telescope;
import fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData;
import fr.jmmc.aspro.util.ComplexUtils;
import fr.jmmc.jmal.ALX;
import fr.jmmc.jmal.complex.Complex;
import fr.jmmc.jmal.complex.ImmutableComplex;
import fr.jmmc.jmal.complex.MutableComplex;
import fr.jmmc.jmal.model.ModelComputeContext;
import fr.jmmc.jmal.model.ModelFunctionComputeContext;
import fr.jmmc.jmal.model.ModelManager;
import fr.jmmc.jmal.model.VisNoiseService;
import fr.jmmc.jmal.model.targetmodel.Model;
import fr.jmmc.jmal.util.ThreadLocalRandom;
import fr.jmmc.jmcs.util.concurrent.ParallelJobExecutor;
import fr.jmmc.oitools.OIFitsConstants;
import fr.jmmc.oitools.model.OIArray;
import fr.jmmc.oitools.model.OIFitsChecker;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIT3;
import fr.jmmc.oitools.model.OITarget;
import fr.jmmc.oitools.model.OIVis;
import fr.jmmc.oitools.model.OIVis2;
import fr.jmmc.oitools.model.OIWavelength;
import fr.jmmc.oitools.util.CombUtils;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import org.ivoa.util.concurrent.ThreadExecutors;
import org.ivoa.util.timer.TimerFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class contains the code to create OIFits structure from the current observation and performs noise modeling
 * @author bourgesl
 */
public final class OIFitsCreatorService {

  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(OIFitsCreatorService.class.getName());
  /** target Id */
  private final static short TARGET_ID = (short) 1;
  /** enable the OIFits validation */
  private final static boolean DO_VALIDATE = false;
  /** flag to show compute task statistics */
  private final static boolean SHOW_COMPUTE_STATS = false;
  /** threshold to use parallel jobs for user models (32 UV points) */
  private final static int JOB_THRESHOLD_USER_MODELS = 32;
  /** threshold to use parallel jobs for experimental noise sampling (2000 UV points) */
  private final static int JOB_THRESHOLD_NOISE_SAMPLES = 2000;
  /** number of complex visiblity samples to compute standard deviation (experimental instrument) */
  private final static int N_SAMPLES = 100;
  /** Jmcs Parallel Job executor */
  private static final ParallelJobExecutor jobExecutor = ParallelJobExecutor.getInstance();

  /* members */
  /* input */
  /** observation settings used  (read-only copy of the modifiable observation) */
  private final ObservationSetting observation;
  /** selected target */
  private final Target target;

  /* reused observability data */
  /** beam list */
  private final List<Beam> beams;
  /** number of beams */
  private final int nBeams;
  /** base line list */
  private final List<BaseLine> baseLines;
  /** number of baselines */
  private final int nBaseLines;
  /** precessed target right ascension in decimal hours */
  private final double precRA;
  /** sky calc instance */
  private final AstroSkyCalc sc;

  /* reused uv coverage data */
  /** minimal wavelength */
  private final double lambdaMin;
  /** maximal wavelength */
  private final double lambdaMax;
  /** number of wavelengths = number of spectral channels */
  private int nWaveLengths;
  /** observable decimal hour angles */
  private final double[] obsHa;
  /** number of points = number of observable hour angles */
  private final int nHAPoints;
  /** list of uv point couples corresponding to the target observability */
  private final List<UVRangeBaseLineData> targetUVObservability;

  /* output */
  /** oifits structure */
  private OIFitsFile oiFitsFile = null;

  /* internal */
  /** target has model (analytical or user model) */
  private final boolean hasModel;
  /** flag = true if returned errors are valid */
  private final boolean errorValid;
  /** flag to add gaussian noise to OIFits data; true if parameter doDataNoise = true and noise parameters are valid */
  private final boolean doNoise;
  /** interferometer name */
  private String arrayName = null;
  /** instrument name */
  private String instrumentName = null;
  /** instrument experimental flag */
  private boolean instrumentExperimental = false;
  /** wavelengths */
  private double[] waveLengths;
  /** beam mapping */
  private Map<Beam, Short> beamMapping = null;
  /** baseline mapping */
  private Map<BaseLine, short[]> baseLineMapping = null;
  /** integration time (s) */
  private double integrationTime = 300d;
  /** noise service */
  private final NoiseService noiseService;
  /** internal computed complex visibility [row][waveLength] */
  private Complex[][] visComplex = null;
  /** internal complex visibility error [row][waveLength] */
  private Complex[][] visError = null;
  /** random generator */
  private final Random random = new Random();

  /**
   * Protected constructor
   * @param observation observation settings
   * @param target target to process
   * @param beams beam list
   * @param baseLines base line list
   * @param lambdaMin minimal wavelength (m)
   * @param lambdaMax maximal wavelength (m)
   * @param nSpectralChannels number of spectral channels
   * @param doDataNoise flag to add gaussian noise to OIFits data
   * @param obsHa observable decimal hour angles
   * @param targetUVObservability list of UV coordinates per baseline
   * @param precRA precessed target right ascension in decimal hours
   * @param sc sky calc instance
   * @param warningContainer container for warning messages
   */
  protected OIFitsCreatorService(final ObservationSetting observation,
          final Target target,
          final List<Beam> beams,
          final List<BaseLine> baseLines,
          final double lambdaMin, final double lambdaMax,
          final int nSpectralChannels,
          final boolean doDataNoise,
          final double[] obsHa,
          final List<UVRangeBaseLineData> targetUVObservability,
          final double precRA,
          final AstroSkyCalc sc,
          final WarningContainer warningContainer) {
    this.observation = observation;
    this.target = target;
    this.beams = beams;
    this.nBeams = this.beams.size();
    this.baseLines = baseLines;
    this.nBaseLines = this.baseLines.size();
    this.lambdaMin = lambdaMin;
    this.lambdaMax = lambdaMax;
    this.nWaveLengths = nSpectralChannels;
    this.obsHa = obsHa;
    this.nHAPoints = this.obsHa.length;
    this.targetUVObservability = targetUVObservability;
    this.precRA = precRA;
    this.sc = sc;

    if (observation.getInstrumentConfiguration().getAcquisitionTime() != null) {
      this.integrationTime = observation.getInstrumentConfiguration().getAcquisitionTime().doubleValue();
    }

    // Prepare the noise service :

    // note: NoiseService parameter dependencies:
    // observation {target}
    // parameter: warningContainer
    this.noiseService = new NoiseService(this.observation, target, warningContainer);

    this.hasModel = target.hasModel();

    this.errorValid = this.noiseService.isValid();

    // do noise :
    this.doNoise = (doDataNoise && this.errorValid);
  }

  /**
   * Create the OIFits structure with OI_ARRAY, OI_TARGET, OI_WAVELENGTH, OI_VIS tables
   * @return OIFits structure
   */
  public OIFitsFile createOIFits() {

    // Start the computations :
    final long start = System.nanoTime();

    // create a new EMPTY OIFits structure :
    this.oiFitsFile = new OIFitsFile();

    this.arrayName = this.observation.getInterferometerConfiguration().getName();

    final FocalInstrument instrument = this.observation.getInstrumentConfiguration().getInstrumentConfiguration().getFocalInstrument();

    this.instrumentName = instrument.getName();
    this.instrumentExperimental = (instrument.isExperimental() != null) ? instrument.isExperimental().booleanValue() : false;

    if (logger.isDebugEnabled()) {
      logger.debug("arrName: {}", this.arrayName);
      logger.debug("insName: {}", this.instrumentName);
      logger.debug("experimental: {}", this.instrumentExperimental);
    }

    // Create beams and base line mappings :
    this.beamMapping = createBeamMapping(this.beams);
    this.baseLineMapping = createBaseLineMapping(this.beamMapping, this.baseLines);

    // OI_ARRAY :
    this.createOIArray();

    // OI_TARGET :
    this.createOITarget();

    // OI_WAVELENGTH :
    this.createOIWaveLength();

    // Compute complex visibilities :
    this.computeModelVisibilities();

    // fast interrupt :
    if (Thread.currentThread().isInterrupted()) {
      return null;
    }

    // OI_VIS :
    this.createOIVis();

    // fast interrupt :
    if (Thread.currentThread().isInterrupted()) {
      return null;
    }

    // OI_VIS2 :
    this.createOIVis2();

    // fast interrupt :
    if (Thread.currentThread().isInterrupted()) {
      return null;
    }

    // OI_T3 :
    this.createOIT3();

    // fast interrupt :
    if (Thread.currentThread().isInterrupted()) {
      return null;
    }

    // free computed complex visibilities :
    this.visComplex = null;

    // remove the OI_VIS table for instruments that do not produce such results (PIONIER):
    if (AsproConstants.INS_PIONIER.equals(this.instrumentName)) {
      // Remove OI_VIS table if instrument is PIONIER:
      final OIVis vis = this.oiFitsFile.getOiVis()[0];

      this.oiFitsFile.removeOiTable(vis);
    }

    logger.info("createOIFits: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

    if (DO_VALIDATE) {
      final OIFitsChecker checker = new OIFitsChecker();
      this.oiFitsFile.check(checker);

      // validation results
      if (logger.isInfoEnabled()) {
        logger.info("createOIFits: validation results\n{}", checker.getCheckReport());
      }
    }

    // fast interrupt :
    if (Thread.currentThread().isInterrupted()) {
      return null;
    }

    return this.oiFitsFile;
  }

  /**
   * Create the OI_ARRAY table
   *
   * Note : station indexes are given according to the beam list ordering starting from 1
   */
  private void createOIArray() {

    // Create OI_ARRAY table :
    final OIArray oiArray = new OIArray(this.oiFitsFile, this.nBeams);

    // Array Name :
    oiArray.setArrName(this.arrayName);

    // Position :
    oiArray.setFrame(OIFitsConstants.KEYWORD_FRAME_GEOCENTRIC);

    final InterferometerConfiguration ic = this.observation.getInterferometerConfiguration().getInterferometerConfiguration();
    if (ic != null) {
      final Position3D position = ic.getInterferometer().getPosition();

      oiArray.setArrayXYZ(new double[]{position.getPosX(), position.getPosY(), position.getPosZ()});
    }

    // Stations :
    int i = 0;
    Telescope tel;
    Station station;
    for (final Beam b : this.beams) {
      station = b.getStation();

      tel = station.getTelescope();
      oiArray.getTelName()[i] = tel.getName();
      oiArray.getDiameter()[i] = (float) tel.getDiameter();

      oiArray.getStaName()[i] = station.getName();
      oiArray.getStaIndex()[i] = (short) (i + 1);

      // TODO : rotate the horizontal position to geocentric :
      final Position3D position = station.getRelativePosition();

      oiArray.getStaXYZ()[i] = new double[]{position.getPosX(), position.getPosY(), position.getPosZ()};

      i++;
    }

    this.oiFitsFile.addOiTable(oiArray);
  }

  /**
   * Create the OI_TARGET table
   *
   * Note : target index is 1
   */
  private void createOITarget() {

    // Create OI_TARGET table :
    final OITarget oiTarget = new OITarget(this.oiFitsFile, 1);
    oiTarget.getTargetId()[0] = TARGET_ID;
    oiTarget.getTarget()[0] = this.target.getName();

    // Coordinates RA/DEC :
    oiTarget.getRaEp0()[0] = this.target.getRADeg();
    oiTarget.getDecEp0()[0] = this.target.getDECDeg();
    oiTarget.getEquinox()[0] = this.target.getEQUINOX();

    // Missing RA/DEC errors :
    oiTarget.getRaErr()[0] = 0d;
    oiTarget.getDecErr()[0] = 0d;

    // Radial velocity :
    if (this.target.getSYSVEL() != null) {
      // convert km/s in m/s :
      oiTarget.getSysVel()[0] = this.target.getSYSVEL().doubleValue() * 1e3;
    }
    oiTarget.getVelTyp()[0] = OIFitsConstants.UNKNOWN_VALUE;
    // Use VELTYP (mostly undefined) :
    oiTarget.getVelDef()[0] = OIFitsConstants.COLUMN_VELDEF_OPTICAL;

    // Proper motion :
    if (this.target.getPMRA() != null && this.target.getPMDEC() != null) {
      // convert mas/year in deg/year :
      oiTarget.getPmRa()[0] = this.target.getPMRA().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES;
      oiTarget.getPmDec()[0] = this.target.getPMDEC().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES;
    }

    // Missing PM RA/DEC errors :
    oiTarget.getPmRaErr()[0] = 0d;
    oiTarget.getPmDecErr()[0] = 0d;

    // Parallax :
    if (this.target.getPARALLAX() != null && this.target.getPARAERR() != null) {
      // convert mas in deg :
      oiTarget.getParallax()[0] = (float) (this.target.getPARALLAX().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES);
      oiTarget.getParaErr()[0] = (float) (this.target.getPARAERR().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES);
    }

    // Spectral type :
    if (this.target.getSPECTYP() != null && this.target.getSPECTYP().length() > 0) {
      oiTarget.getSpecTyp()[0] = target.getSPECTYP();
    } else {
      oiTarget.getSpecTyp()[0] = "";
    }

    this.oiFitsFile.addOiTable(oiTarget);
  }

  /**
   * Create the OI_WAVELENGTH table
   */
  private void createOIWaveLength() {

    // Create OI_WAVELENGTH table :
    final OIWavelength waves = new OIWavelength(this.oiFitsFile, this.nWaveLengths);
    waves.setInsName(this.instrumentName);

    final double step = (this.lambdaMax - this.lambdaMin) / this.nWaveLengths;

    this.waveLengths = new double[this.nWaveLengths];

    final float[] effWave = waves.getEffWave();
    final float[] effBand = waves.getEffBand();

    // effective wavelength corresponds to the channel center:
    double waveLength = this.lambdaMin + 0.5d * step;

    for (int i = 0; i < this.nWaveLengths; i++) {
      this.waveLengths[i] = waveLength;

      effWave[i] = (float) waveLength;
      effBand[i] = (float) step;

      waveLength += step;
    }

    this.oiFitsFile.addOiTable(waves);
  }

  /**
   * Compute complex visibilities using the target model (analytical or user model)
   * and store this data in local reference table
   */
  private void computeModelVisibilities() {

    if (this.hasModel) {

      /** Get the current thread to check if the computation is interrupted */
      final Thread currentThread = Thread.currentThread();

      // local vars:
      final int nWLen = nWaveLengths;
      final int nBl = nBaseLines;
      final int nHA = nHAPoints;
      final NoiseService ns = noiseService;

      // TODO: compare directFT vs FFT + interpolation

      final long start = System.nanoTime();

      final boolean useAnalyticalModel = this.target.hasAnalyticalModel();

      // fast interrupt :
      if (currentThread.isInterrupted()) {
        return;
      }

      final int nRows = nHA * nBl;
      final int nPoints = nRows * nWLen;

      logger.info("computeModelVisibilities: {} nRows - {} nWLen", nRows, nWLen);
      logger.info("computeModelVisibilities: {} points - please wait ...", nPoints);

      logger.info("computeModelVisibilities: {} bytes for 1 complex array", 2 * 8 * nPoints); // 1 complex (2 double)

      // Allocate data array for complex visibility and error :
      final MutableComplex[][] cmVis = new MutableComplex[nRows][];

      // Iterate on rows :
      for (int k = 0; k < nRows; k++) {
        cmVis[k] = createArray(nWLen + 4); // cache line padding (Complex = 2 double = 16 bytes) so 4 complex = complete cache line
      }

      // Allocate data array for spatial frequencies:
      final double[][] ufreq = new double[nRows][nWLen];
      final double[][] vfreq = new double[nRows][nWLen];

      logger.info("computeModelVisibilities: {} bytes for uv freq array", 2 * 8 * nPoints); // 2 double

      // Iterate on baselines :
      for (int i, j = 0, k, l; j < nBl; j++) {

        final UVRangeBaseLineData uvBL = this.targetUVObservability.get(j);

        // Iterate on HA points :
        for (i = 0; i < nHA; i++) {

          // UV coords (m) :
          final double u = uvBL.getU()[i];
          final double v = uvBL.getV()[i];

          k = nBl * i + j;

          final double[] uRow = ufreq[k];
          final double[] vRow = vfreq[k];

          // prepare spatial frequencies :
          for (l = 0; l < nWLen; l++) {
            uRow[l] = u / waveLengths[l];
            vRow[l] = v / waveLengths[l];
          }
        }
      }

      // Compute complex visibility using the target model:

      if (useAnalyticalModel) {
        // Clone models and normalize fluxes :
        final List<Model> normModels = ModelManager.normalizeModels(this.target.getModels());

        // Analytical models: no parallelization:
        final ModelManager modelManager = ModelManager.getInstance();

        // model computation context
        final ModelComputeContext context = modelManager.prepareModels(normModels, nWLen);

        // Iterate on rows:
        for (int k = 0; k < nRows; k++) {

          // Compute complex visibility using the target model:
          copyArray(modelManager.computeModels((ModelFunctionComputeContext) context, ufreq[k], vfreq[k]), cmVis[k]);

          if (currentThread.isInterrupted()) {
            // fast interrupt :
            return;
          }

        } // rows

      } else {
        // Get prepared user model data:
        if (!target.getUserModel().isModelDataReady()) {
          return;
        }

        // TODO: use all image collection not the first one only:
        final UserModelData modelData = target.getUserModel().getModelDataList().get(0);

        // TODO: determine which images to use according to wavelength range:


        // TODO: use a preference

        final UserModelService.MathMode mathMode = UserModelService.MathMode.FAST;

        // Accuracy tests (see OIFitsWriterTest) on GRAVITY so VISAMPERR/VISPHIERR are only 'sampled':
        // FAST provides good accuracy on complete OIFits:
        /*                
         WARNING: WARN:  Column[VISAMP]	    Max Absolute Error=3.122502256758253E-17	Max Relative Error=2.943534976436331E-14
         WARNING: WARN:  Column[VISAMPERR]	Max Absolute Error=0.0013869817652312072	Max Relative Error=0.0997510362307679
         WARNING: WARN:  Column[VISPHI]	    Max Absolute Error=1.8474111129762605E-12	Max Relative Error=3.3158630356109147E-12
         WARNING: WARN:  Column[VISPHIERR]	Max Absolute Error=19.19686940833244	Max Relative Error=0.9113901536192872
         WARNING: WARN:  Column[VIS2DATA]	  Max Absolute Error=1.5178830414797062E-18	Max Relative Error=5.915784768102743E-14
         WARNING: WARN:  Column[VIS2ERR]	  Max Absolute Error=5.421010862427522E-20	Max Relative Error=4.471809116292319E-16
         WARNING: WARN:  Column[T3AMP]	    Max Absolute Error=9.740878893424454E-21	Max Relative Error=3.613928543746403E-14
         WARNING: WARN:  Column[T3AMPERR]	  Max Absolute Error=3.441071348220595E-22	Max Relative Error=3.6192130029721625E-14
         WARNING: WARN:  Column[T3PHI]	    Max Absolute Error=1.8332002582610585E-12	Max Relative Error=1.6154567952731343E-13
         */
        // QUICK provides only a preview (not accurate on VISPHI / T3PHI:
        /*
         WARNING: WARN:  Column[VISDATA]	  Max Absolute Error=0.21170902252197266	Max Relative Error=155.32336222596265
         WARNING: WARN:  Column[VISERR]	    Max Absolute Error=1.7113983631134033E-5	Max Relative Error=5.123198196063256E-4
         WARNING: WARN:  Column[VISAMP]	    Max Absolute Error=0.012389325449663476	Max Relative Error=0.9827257597222762
         WARNING: WARN:  Column[VISAMPERR]	Max Absolute Error=4.3704479622192665E-4	Max Relative Error=0.24928702639103537
         WARNING: WARN:  Column[VISPHI]	    Max Absolute Error=357.76365704000574	Max Relative Error=132.85957062222084
         WARNING: WARN:  Column[VISPHIERR]	Max Absolute Error=54.185925961293876	Max Relative Error=0.9643291278418655
         WARNING: WARN:  Column[VIS2DATA]	  Max Absolute Error=4.3211132008532375E-4	Max Relative Error=3199.047758503112
         WARNING: WARN:  Column[VIS2ERR]	  Max Absolute Error=7.815369519747367E-9	Max Relative Error=6.780968471059581E-5
         WARNING: WARN:  Column[T3AMP]	    Max Absolute Error=1.8693429580914967E-7	Max Relative Error=0.11262751096020436
         WARNING: WARN:  Column[T3AMPERR]	  Max Absolute Error=4.423268697588574E-11	Max Relative Error=0.0016543021640061009
         WARNING: WARN:  Column[T3PHI]	    Max Absolute Error=6.734992735788779	Max Relative Error=6.813763216810152
         */

        logger.info("computeModelVisibilities: MathMode = {}.", mathMode);

        /*
         [bourgesl@jmmc-laurent ~]$ lscpu
         Architecture:          x86_64
         CPU(s):                4
         Thread(s) par coeur :  2
         Coeur(s) par support CPU :2
         CPU MHz :              2800.000
         L1d cache :            32K
         L1i cache :            32K
         L2 cache :             256K
         L3 cache :             4096K
         */

        // enable parallel jobs if many points using user model:
        final int nTh = (!jobExecutor.isWorkerThread() && (nPoints > JOB_THRESHOLD_USER_MODELS)) ? jobExecutor.getMaxParallelJob() : 1;

        // Prepare thread context variables:
        final int[][] nTaskThreads = (SHOW_COMPUTE_STATS) ? new int[nTh][16] : null; // cache line padding



        // This will change for each image in the Fits cube:
        final int n1D = modelData.getNData(); // data, xfreq, yfreq

        logger.info("computeModelVisibilities: {} bytes for image arrays", 4 * n1D); // (float) array

        final boolean doBench = false;

        if (doBench) {
          // timer warmup:
          TimerFactory.resetTimers();
        }

        // evaluate chunk size:
//        for (int n = 40000; n >= 5000; n -= 5000) {
        for (int n = 600 * 1000; n > 4000; n = (3 * n) / 4) {
          /*
           * -open /home/bourgesl/ASPRO2/BENCH_MODEL_BIG.asprox
           * java -server -Xms256m -Xmx256m -verbose:gc
           Timer [chunk[600000] - ms] [6]	{num = 6 :	min = 6243.15402,	avg = 6384.94846,	max = 6645.87379,	acc = 38309.69081}
           Timer [chunk[450000] - ms] [6]	{num = 6 :	min = 6203.26475,	avg = 6235.64234,	max = 6261.62575,	acc = 37413.85405}
           Timer [chunk[337500] - ms] [6]	{num = 6 :	min = 6201.22245,	avg = 6246.97156,	max = 6315.11635,	acc = 37481.8294}
           Timer [chunk[253125] - ms] [6]	{num = 6 :	min = 6230.08723,	avg = 6360.00076,	max = 6743.4357,	acc = 38160.00458}
           Timer [chunk[189843] - ms] [6]	{num = 6 :	min = 6145.51441,	avg = 6178.14009,	max = 6240.0414,	acc = 37068.84055}
           Timer [chunk[142382] - ms] [6]	{num = 6 :	min = 6153.0717,	avg = 6225.52145,	max = 6350.81124,	acc = 37353.12873}
           Timer [chunk[106786] - ms] [6]	{num = 6 :	min = 6110.36145,	avg = 6132.26537,	max = 6146.3951,	acc = 36793.59222}
           Timer [chunk[80089] - ms] [6]	{num = 6 :	min = 6113.14398,	avg = 6154.84745,	max = 6218.87338,	acc = 36929.08472}
           Timer [chunk[60066] - ms] [6]	{num = 6 :	min = 6111.26703,	avg = 6162.30571,	max = 6219.9817,	acc = 36973.83429}
           Timer [chunk[45049] - ms] [6]	{num = 6 :	min = 6137.05735,	avg = 6190.03549,	max = 6285.07611,	acc = 37140.21295}
           Timer [chunk[33786] - ms] [6]	{num = 6 :	min = 6129.09484,	avg = 6167.5245,	max = 6235.43153,	acc = 37005.14704}
           Timer [chunk[25339] - ms] [6]	{num = 6 :	min = 6115.01552,	avg = 6148.93023,	max = 6239.73482,	acc = 36893.58139}
           Timer [chunk[19004] - ms] [6]	{num = 6 :	min = 6105.3263,	avg = 6150.67354,	max = 6275.43552,	acc = 36904.04126}
           Timer [chunk[14253] - ms] [6]	{num = 6 :	min = 6115.31962,	avg = 6135.68165,	max = 6176.77583,	acc = 36814.08992}
           Timer [chunk[10689] - ms] [6]	{num = 6 :	min = 6091.29831,	avg = 6201.2638,	max = 6406.23302,	acc = 37207.58283}
           Timer [chunk[8016] - ms] [6]	{num = 6 :	min = 6093.65906,	avg = 6121.05512,	max = 6244.3136,	acc = 36726.33073}
           Timer [chunk[6012] - ms] [6]	{num = 6 :	min = 6086.97264,	avg = 6135.02741,	max = 6323.50799,	acc = 36810.16451}
           Timer [chunk[4509] - ms] [6]	{num = 6 :	min = 6088.02502,	avg = 6105.74722,	max = 6148.93475,	acc = 36634.48333}
           * 
           * java -client -Xms256m -Xmx256m -verbose:gc
           Timer [chunk[600000] - ms] [6]	{num = 6 :	min = 6224.30511,	avg = 6606.00973,	max = 6955.55131,	acc = 39636.0584}
           Timer [chunk[450000] - ms] [6]	{num = 6 :	min = 6168.78036,	avg = 6242.96843,	max = 6335.26505,	acc = 37457.81058}
           Timer [chunk[337500] - ms] [6]	{num = 6 :	min = 6154.05134,	avg = 6250.50528,	max = 6489.83068,	acc = 37503.03168}
           Timer [chunk[253125] - ms] [6]	{num = 6 :	min = 6126.73288,	avg = 6156.01418,	max = 6195.99444,	acc = 36936.08509}
           Timer [chunk[189843] - ms] [6]	{num = 6 :	min = 6218.94806,	avg = 6453.99665,	max = 6619.456,	acc = 38723.97991}
           Timer [chunk[142382] - ms] [6]	{num = 6 :	min = 6353.57777,	avg = 6402.73572,	max = 6464.20086,	acc = 38416.41431}
           Timer [chunk[106786] - ms] [6]	{num = 6 :	min = 6355.4546,	avg = 6377.30765,	max = 6421.21122,	acc = 38263.8459}
           Timer [chunk[80089] - ms] [6]	{num = 6 :	min = 6339.65153,	avg = 6392.74165,	max = 6482.51913,	acc = 38356.44995}
           Timer [chunk[60066] - ms] [6]	{num = 6 :	min = 6286.62151,	avg = 6331.83599,	max = 6348.88081,	acc = 37991.01597}
           Timer [chunk[45049] - ms] [6]	{num = 6 :	min = 6260.96024,	avg = 6328.66796,	max = 6417.38881,	acc = 37972.00781}
           Timer [chunk[33786] - ms] [6]	{num = 6 :	min = 6283.12942,	avg = 6311.33091,	max = 6345.63726,	acc = 37867.98545}
           Timer [chunk[25339] - ms] [6]	{num = 6 :	min = 6247.72033,	avg = 6287.26846,	max = 6312.76508,	acc = 37723.6108}
           Timer [chunk[19004] - ms] [6]	{num = 6 :	min = 6235.30377,	avg = 6262.0295,	max = 6288.65923,	acc = 37572.17702}
           Timer [chunk[14253] - ms] [6]	{num = 6 :	min = 6223.88945,	avg = 6294.78818,	max = 6373.82307,	acc = 37768.72909}
           Timer [chunk[10689] - ms] [6]	{num = 6 :	min = 6220.66759,	avg = 6289.95525,	max = 6345.2369,	acc = 37739.73151}
           */

          if (doBench) {
            TimerFactory.resetTimers();
          }

          for (int s = 0; s <= 5; s++) {
//            {
//              {
            // best is:
//            final int n = 20000;

            if (doBench) {
              // reset:
              for (int k = 0; k < nRows; k++) {
                cmVis[k] = createArray(nWLen + 4); // cache line padding (Complex = 2 double = 16 bytes) so 4 complex = complete cache line
              }

              if (SHOW_COMPUTE_STATS) {
                for (int t = 0; t < nTh; t++) {
                  nTaskThreads[t][0] = 0;
                }
              }

              if (currentThread.isInterrupted()) {
                // fast interrupt :
                return;
              }

              System.gc();
              if (!ThreadExecutors.sleep(100l)) {
                // fast interrupt :
                return;
              }
            }

            final long start2 = System.nanoTime();

            int chunk = n * 3;
//        int chunk = 90 * 1000; // 6000 ok
//        final int chunk = 45 * 1000; // 6000 ok
//        final int chunk = 10 * 1000 * 1000; // 6000 ok
//        final int chunk = 6000; // 6000 ok


            final int nChunks = 1 + n1D / chunk;

            logger.info("computeModelVisibilities: {} chunks", nChunks);

            // note: chunk must a multiple of 3 !
            chunk = 3 * ((n1D / nChunks) / 3);

            logger.info("computeModelVisibilities: {} bytes for chunk", (chunk > n1D) ? 4 * n1D : 4 * chunk);// (float) array
            logger.info("computeModelVisibilities: chunk = {}", chunk);

            final int[] fromThreads = new int[nChunks];
            final int[] endThreads = new int[nChunks];

            for (int c = 0; c < nChunks; c++) {
              fromThreads[c] = c * chunk;
              endThreads[c] = fromThreads[c] + chunk;
            }
            endThreads[nChunks - 1] = n1D;


//            logger.info("computeModelVisibilities: from {} - to {}", Arrays.toString(fromThreads), Arrays.toString(endThreads));

            // computation tasks = 1 job per row and chunk (work stealing):
            final Runnable[] jobs = new Runnable[nRows * nChunks];

            // Important: have jobs a multiple of nTh to maximize parallelism !

            logger.info("computeModelVisibilities: {} jobs", jobs.length);

            // TODO: process wavelengths by chunks too:
            final int from = 0;
            final int end = nWLen;

            // create tasks:
            for (int c = 0, j = 0; c < nChunks; c++) {
              // Image chunks:
              final int fromData = fromThreads[c];
              final int endData = endThreads[c];

              for (int k = 0; k < nRows; k++) {
                // rows index to be processed by this task:
                final int rowIndex = k;
                final float[] data1D = modelData.getData1D();
                final double[] uRow = ufreq[rowIndex];
                final double[] vRow = vfreq[rowIndex];
                final MutableComplex[] cmVisRow = cmVis[rowIndex];

                jobs[j++] = new Runnable() {
                  /**
                   * Called by the ParallelJobExecutor to perform task computation
                   */
                  @Override
                  public void run() {

//                    logger.info("Thread[{}]: row {} - data from {} to {} - lambda from {} to {}", ParallelJobExecutor.currentThreadIndex(nTh), rowIndex, fromData, endData, from, end);

                    // Compute complex visibility using the target model:
//                final long start = System.nanoTime();

                    // compute complex visibilities :
                    UserModelService.computeModel(data1D, fromData, endData, uRow, vRow, cmVisRow, from, end, mathMode);

                    // fast interrupt:
/*                
                     if (Thread.currentThread().isInterrupted()) {
                     return;
                     }
                     */
                    if (SHOW_COMPUTE_STATS) {
                      // Get thread index to get appropriate thread vars:
                      final int threadIndex = ParallelJobExecutor.currentThreadIndex(nTh);
                      nTaskThreads[threadIndex][0]++;
                    }

//                logger.info("Thread[{}]: row {} done in: duration = {} ms.", threadIndex, rowIndex, 1e-6d * (System.nanoTime() - start));
                  }
                };
              }
            }

            // execute jobs in parallel or using current thread if only one job (throws InterruptedJobException if interrupted):
            jobExecutor.forkAndJoin("OIFitsCreatorService.computeModelVisibilities", jobs);

            TimerFactory.getSimpleTimer("chunk[" + n + "]", TimerFactory.UNIT.ms).addMilliSeconds(start2, System.nanoTime());

            if (SHOW_COMPUTE_STATS) {
              for (int t = 0; t < nTh; t++) {
                logger.info("Thread[{}] done: {} processed jobs", t, nTaskThreads[t][0]);
              }
            }

            if (!doBench) {
              break;
            }
          }

          if (doBench && !TimerFactory.isEmpty()) {
            logger.info("computeModelVisibilities: FINAL statistics: {}", TimerFactory.dumpTimers());
          }

          if (!doBench) {
            break;
          }
        }
      }

      logger.info("computeModelVisibilities: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

      if (currentThread.isInterrupted()) {
        // fast interrupt :
        return;
      }

      final Complex[][] cVisError = new Complex[nRows][nWLen];
      final Complex[][] cVis = new Complex[nRows][];

      // Iterate on rows :
      for (int k = 0, l; k < nRows; k++) {
        // Iterate on wave lengths :
        for (l = 0; l < nWLen; l++) {
          // complex visibility error or Complex.NaN:
          cVisError[k][l] = ns.computeVisComplexError(cmVis[k][l].abs());
        }

        cVis[k] = convertArray(cmVis[k], nWLen); // immutable for safety
      }

      this.visComplex = cVis;
      this.visError = cVisError;
    }
  }

  /**
   * Convert mutable complex array to immutable complex array for safety reasons
   * @param array mutable complex array
   * @param length array length
   * @return immutable complex array
   */
  private static Complex[] convertArray(final MutableComplex[] array, final int length) {
    if (array == null) {
      return null;
    }
    final Complex[] result = new Complex[length];

    for (int i = length - 1; i >= 0; i--) {
      result[i] = new ImmutableComplex(array[i]); // immutable complex for safety
    }
    return result;
  }

  /**
   * Copy mutable complex array
   * @param array mutable complex array
   * @param dest mutable complex array
   */
  private static void copyArray(final MutableComplex[] array, final MutableComplex[] dest) {
    if (array == null) {
      return;
    }
    final int length = array.length;

    for (int i = length - 1; i >= 0; i--) {
      dest[i].updateComplex(array[i]);
    }
  }

  /**
   * Create a mutable complex array
   * @param length array length
   * @return mutable complex array
   */
  private static MutableComplex[] createArray(final int length) {
    final MutableComplex[] result = new MutableComplex[length];

    for (int i = length - 1; i >= 0; i--) {
      result[i] = new MutableComplex();
    }
    return result;
  }

  /**
   * Create the OI_VIS table using internal computed visComplex data
   */
  private void createOIVis() {
    final long start = System.nanoTime();

    /** Get the current thread to check if the computation is interrupted */
    final Thread currentThread = Thread.currentThread();

    // test if the instrument is AMBER to use dedicated diffVis algorithm :
    final boolean isAmber = AsproConstants.INS_AMBER.equals(this.instrumentName);

    // Create OI_VIS table :
    final OIVis vis = new OIVis(this.oiFitsFile, this.instrumentName, this.nHAPoints * this.nBaseLines);
    vis.setArrName(this.arrayName);

    // Compute UTC start date from first HA :
    final Calendar calObs = this.sc.toCalendar(this.sc.convertHAToJD(this.obsHa[0], this.precRA), false);

    final String dateObs = calendarToString(calObs);
    vis.setDateObs(dateObs);

    // Columns :
    final short[] targetIds = vis.getTargetId();
    final double[] times = vis.getTime();
    final double[] mjds = vis.getMjd();
    final double[] intTimes = vis.getIntTime();

    final float[][][] visData = vis.getVisData();
    final float[][][] visErr = vis.getVisErr();

    final double[][] visAmp = vis.getVisAmp();
    final double[][] visAmpErr = vis.getVisAmpErr();

    final double[][] visPhi = vis.getVisPhi();
    final double[][] visPhiErr = vis.getVisPhiErr();

    final double[] uCoords = vis.getUCoord();
    final double[] vCoords = vis.getVCoord();

    final short[][] staIndexes = vis.getStaIndex();
    final boolean[][] flags = vis.getFlag();

    // complex visiblity with noise (sigma = visError)
    final Complex[][] visComplexNoisy = new Complex[vis.getNbRows()][this.nWaveLengths];

    final int nPoints = this.nHAPoints * this.nBaseLines * this.nWaveLengths;
    final boolean doNoiseSampling = (this.hasModel && this.errorValid && !isAmber && this.instrumentExperimental);

    if (doNoiseSampling) {
      logger.info("createOIVis: {} points - experimental instrument: VisAmp/Phi errors computed using {} random complex visiblities", nPoints, N_SAMPLES);
    }

    // enable parallel jobs if many points using user model:
    final int nJobs = (doNoiseSampling && nPoints > JOB_THRESHOLD_NOISE_SAMPLES) ? jobExecutor.getMaxParallelJob() : 1;

    // computation tasks:
    final Runnable[] jobs = new Runnable[nJobs];

    // create tasks:
    for (int i = 0; i < nJobs; i++) {
      final int jobIndex = i;

      jobs[i] = new Runnable() {
        @Override
        public void run() {
          // random instance dedicated to this thread:
          final Random threadRandom = ThreadLocalRandom.current();

          // vars:
          double jd, time, mjd;
          double u, v;
          double visRe, visIm, flux, vAmp, vPhi, visErrRe, visErrIm;

          double diff, ampSquareDiffAcc, phiSquareDiffAcc;
          final MutableComplex visComplexSample = new MutableComplex();

          // Iterate on HA points :
          for (int i = 0, j, k, l, n; i < nHAPoints; i++) {

            // jd from HA :
            jd = sc.convertHAToJD(obsHa[i], precRA);

            // UTC :
            time = calendarToTime(sc.toCalendar(jd, false), calObs);

            // modified julian day :
            mjd = AstroSkyCalc.mjd(jd);

            j = 0;

            // Iterate on baselines :
            for (final UVRangeBaseLineData uvBL : targetUVObservability) {

              k = nBaseLines * i + j;

              // job parallelism:
              if (k % nJobs == jobIndex) {

                // target id
                targetIds[k] = TARGET_ID;

                // UTC :
                times[k] = time;

                // modified julian day :
                mjds[k] = mjd;

                // integration time (s) :
                intTimes[k] = integrationTime;

                // UV coords (m) :
                u = uvBL.getU()[i];
                v = uvBL.getV()[i];

                uCoords[k] = u;
                vCoords[k] = v;

                // if target has models and errors are valid, then complex visibility are computed :
                if (!hasModel || !errorValid) {
                  // Invalid => NaN value :

                  // Iterate on wave lengths :
                  for (l = 0; l < nWaveLengths; l++) {
                    visData[k][l][0] = Float.NaN;
                    visData[k][l][1] = Float.NaN;

                    visErr[k][l][0] = Float.NaN;
                    visErr[k][l][1] = Float.NaN;

                    visAmp[k][l] = Double.NaN;
                    visAmpErr[k][l] = Double.NaN;

                    visPhi[k][l] = Double.NaN;
                    visPhiErr[k][l] = Double.NaN;

                    // mark this value as invalid :
                    flags[k][l] = true;
                  }

                } else {

                  // Iterate on wave lengths :
                  for (l = 0; l < nWaveLengths; l++) {

                    // pure complex visibility data :
                    visRe = visComplex[k][l].getReal();
                    visIm = visComplex[k][l].getImaginary();

                    // pure visibility amplitude :
                    vAmp = visComplex[k][l].abs();

                    // pure correlated fluxes or NaN:
                    flux = noiseService.computeCorrelatedFlux(vAmp);

                    // complex visibility error : visErrRe = visErrIm = visAmpErr / SQRT(2) or Complex.NaN :
                    visErrRe = visError[k][l].getReal();
                    visErrIm = visError[k][l].getImaginary();

                    // error on correlated fluxes :
                    visErr[k][l][0] = (float) (flux * visErrRe);
                    visErr[k][l][1] = (float) (flux * visErrIm);

                    if (doNoise) {
                      // add gaussian noise with sigma = visErrRe / visErrIm :
                      visComplexNoisy[k][l] = new ImmutableComplex(
                              visRe + VisNoiseService.gaussianNoise(threadRandom, visErrRe),
                              visIm + VisNoiseService.gaussianNoise(threadRandom, visErrIm)); // immutable complex for safety
                    } else {
                      visComplexNoisy[k][l] = visComplex[k][l];
                    }

                    // store pure (0..1) or noisy correlated fluxes (NaN if no flux):
                    visData[k][l][0] = (float) (flux * visComplexNoisy[k][l].getReal());
                    visData[k][l][1] = (float) (flux * visComplexNoisy[k][l].getImaginary());

                    if (!isAmber) {
                      if (instrumentExperimental) {

                        // For experimental instruments: VisAmp/Phi are only amplitude and phase of complex visibility and errors are undefined:
                        visAmp[k][l] = visComplexNoisy[k][l].abs();
                        visPhi[k][l] = Math.toDegrees(visComplexNoisy[k][l].getArgument());

                        // use NSamples random visComplex realisations to compute standard deviation for both visAmp / visPhi:

                        // pure visibility phase :
                        vPhi = visComplex[k][l].getArgument();

                        // howto ensure that errors on Im / Re are inside error disk ?
                        ampSquareDiffAcc = 0d;
                        phiSquareDiffAcc = 0d;

                        for (n = 0; n < N_SAMPLES; n++) {
                          // update nth sample:
                          visComplexSample.updateComplex(
                                  visRe + VisNoiseService.gaussianNoise(threadRandom, visErrRe),
                                  visIm + VisNoiseService.gaussianNoise(threadRandom, visErrIm));

                          // compute square distance to visAmp mean:
                          diff = visComplexSample.abs() - vAmp;

                          ampSquareDiffAcc += diff * diff;

                          // compute square distance to visPhi mean:
                          diff = visComplexSample.getArgument() - vPhi;

                          phiSquareDiffAcc += diff * diff;
                        }

                        // standard deviation on vis amplitude:
                        visAmpErr[k][l] = Math.sqrt(ampSquareDiffAcc / (N_SAMPLES - 1));

                        // standard deviation on vis phase:
                        visPhiErr[k][l] = Math.toDegrees(Math.sqrt(phiSquareDiffAcc / (N_SAMPLES - 1)));

                      } else {
                        // Waiting for explanations on every instrument processing to compute VisAmp/Phi :
                        // following values are considered as invalid :
                        visAmp[k][l] = Double.NaN;
                        visAmpErr[k][l] = Double.NaN;

                        visPhi[k][l] = Double.NaN;
                        visPhiErr[k][l] = Double.NaN;
                      }
                    }

                    // mark this value as valid only if error is valid :
                    flags[k][l] = !errorValid;
                  }
                }

                // station indexes :
                staIndexes[k] = baseLineMapping.get(uvBL.getBaseLine());
              }

              // increment j:
              j++;

              // fast interrupt :
              if (currentThread.isInterrupted()) {
                return;
              }

            } // baselines
          } // HA
        }
      };
    }

    // execute jobs in parallel or using current thread if only one job (throws InterruptedJobException if interrupted):
    jobExecutor.forkAndJoin("OIFitsCreatorService.createOIVis", jobs);

    /* Compute visAmp / visPhi as amber does */
    if (isAmber && this.hasModel && this.errorValid) {
      OIFitsAMBERService.amdlibFakeAmberDiffVis(vis, visComplexNoisy, this.visError, this.waveLengths);
    }

    this.oiFitsFile.addOiTable(vis);

    if (logger.isDebugEnabled()) {
      logger.debug("createOIVis: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
    }
  }

  /**
   * Create the OI_VIS2 table using internal computed visComplex data
   */
  private void createOIVis2() {
    final long start = System.nanoTime();

    // Get OI_VIS table :
    final OIVis vis = this.oiFitsFile.getOiVis()[0];
    final int nRows = vis.getNbRows();

    // Create OI_VIS2 table :
    final OIVis2 vis2 = new OIVis2(this.oiFitsFile, this.instrumentName, nRows);
    vis2.setArrName(this.arrayName);
    vis2.setDateObs(vis.getDateObs());

    // Columns :
    System.arraycopy(vis.getTargetId(), 0, vis2.getTargetId(), 0, nRows);
    System.arraycopy(vis.getTime(), 0, vis2.getTime(), 0, nRows);
    System.arraycopy(vis.getMjd(), 0, vis2.getMjd(), 0, nRows);
    System.arraycopy(vis.getIntTime(), 0, vis2.getIntTime(), 0, nRows);

    final double[][] vis2Data = vis2.getVis2Data();
    final double[][] vis2Err = vis2.getVis2Err();

    final boolean[][] flags = vis2.getFlag();

    // vars:
    double visRe, visIm;
    double v2, v2Err;

    for (int k = 0, l; k < nRows; k++) {

      // if target has models, then complex visibility are computed :
      if (!this.hasModel) {

        // Iterate on wave lengths :
        for (l = 0; l < this.nWaveLengths; l++) {
          vis2Data[k][l] = Double.NaN;
          vis2Err[k][l] = Double.NaN;

          // mark this value as invalid :
          flags[k][l] = true;
        }

      } else {

        // Iterate on wave lengths :
        for (l = 0; l < this.nWaveLengths; l++) {
          // pure complex visibility data :
          visRe = this.visComplex[k][l].getReal();
          visIm = this.visComplex[k][l].getImaginary();

          // pure square visibility :
          v2 = Math.pow(visRe, 2d) + Math.pow(visIm, 2d);
          vis2Data[k][l] = v2;

          // square visibility error :
          v2Err = this.noiseService.computeVis2Error(Math.sqrt(v2));
          vis2Err[k][l] = v2Err;

          if (this.doNoise) {
            // add gaussian noise with sigma = err :
            vis2Data[k][l] += VisNoiseService.gaussianNoise(this.random, v2Err);
          }

          // mark this value as valid only if error is valid :
          flags[k][l] = !this.errorValid;
        }
      }
    }

    System.arraycopy(vis.getUCoord(), 0, vis2.getUCoord(), 0, nRows);
    System.arraycopy(vis.getVCoord(), 0, vis2.getVCoord(), 0, nRows);

    System.arraycopy(vis.getStaIndex(), 0, vis2.getStaIndex(), 0, nRows);

    this.oiFitsFile.addOiTable(vis2);

    if (logger.isDebugEnabled()) {
      logger.debug("createOIVis2: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
    }
  }

  /**
   * Create the OI_T3 table
   */
  private void createOIT3() {

    if (this.nBeams < 3) {
      return;
    }

    final long start = System.nanoTime();

    // number of triplets :
    final List<int[]> iTriplets = CombUtils.generateCombinations(this.nBeams, 3);

    final int nTriplets = iTriplets.size();
    if (nTriplets == 0) {
      return;
    }

    final List<Triplet> triplets = new ArrayList<Triplet>(nTriplets);
    for (int[] idx : iTriplets) {
      triplets.add(Triplet.create(idx, this.baseLineMapping));
    }

    if (logger.isDebugEnabled()) {
      logger.debug("triplets: {}", triplets);
    }

    // Get OI_VIS table :
    final OIVis vis = this.oiFitsFile.getOiVis()[0];

    // Create OI_T3 table :
    final OIT3 t3 = new OIT3(this.oiFitsFile, this.instrumentName, this.nHAPoints * nTriplets);
    t3.setArrName(this.arrayName);
    t3.setDateObs(vis.getDateObs());

    // OI_VIS Columns :
    final double[] visTimes = vis.getTime();
    final double[] visMjds = vis.getMjd();

    final double[] visUCoords = vis.getUCoord();
    final double[] visVCoords = vis.getVCoord();

    final short[][] visStaIndexes = vis.getStaIndex();

    // OI_T3 Columns :
    final short[] t3TargetIds = t3.getTargetId();
    final double[] t3Times = t3.getTime();
    final double[] t3Mjds = t3.getMjd();
    final double[] intTimes = t3.getIntTime();

    final double[][] t3Amp = t3.getT3Amp();
    final double[][] t3AmpErr = t3.getT3AmpErr();

    final double[][] t3Phi = t3.getT3Phi();
    final double[][] t3PhiErr = t3.getT3PhiErr();

    final double[] t3U1Coords = t3.getU1Coord();
    final double[] t3V1Coords = t3.getV1Coord();

    final double[] t3U2Coords = t3.getU2Coord();
    final double[] t3V2Coords = t3.getV2Coord();

    final short[][] t3StaIndexes = t3.getStaIndex();

    final boolean[][] flags = t3.getFlag();

    // The following code use some hypothesis on the OI_VIS table as defined in createOIVis()

    // 1 - the number of rows per HA point corresponds to the number of baselines.
    // 2 - OI_VIS rows have the same ordering than the list of baselines per HA points.

    // vars :
    Complex[] visData12, visData23, visData13;
    Complex vis12, vis23, vis31;
    double u12, v12, u23, v23;
    double errPhi, errAmp, rand;

    // temporary mutable complex:
    final MutableComplex t3Data = new MutableComplex();

    int[] relPos;
    int pos;

    // Iterate on HA points :
    for (int i = 0, j, k, l, vp; i < this.nHAPoints; i++) {

      // position in OI_VIS HA row group :
      vp = this.nBaseLines * i;

      j = 0;

      // Iterate on baselines :
      for (Triplet triplet : triplets) {

        k = nTriplets * i + j;

        // target id
        t3TargetIds[k] = TARGET_ID;

        // UTC :
        t3Times[k] = visTimes[vp];

        // modified julian day :
        t3Mjds[k] = visMjds[vp];

        // integration time (s) :
        intTimes[k] = this.integrationTime;

        // Use relative positions to get the 3 complex vectors (AB, BC, AC)
        relPos = triplet.getRelativePosition();

        // Find baseline AB = 12 :
        pos = relPos[0];

        if (logger.isDebugEnabled()) {
          logger.debug("vis baseline: {}", Arrays.toString(visStaIndexes[vp + pos]));
          logger.debug("T3  baseline: {}", Arrays.toString(triplet.getBaselineIndexes()[0]));
        }

        // pure complex visibility data :
        visData12 = (this.hasModel) ? this.visComplex[vp + pos] : null;
        u12 = visUCoords[vp + pos];
        v12 = visVCoords[vp + pos];

        // Find baseline BC = 23 :
        pos = relPos[1];

        if (logger.isDebugEnabled()) {
          logger.debug("vis baseline: {}", Arrays.toString(visStaIndexes[vp + pos]));
          logger.debug("T3  baseline: {}", Arrays.toString(triplet.getBaselineIndexes()[1]));
        }

        // pure complex visibility data :
        visData23 = (this.hasModel) ? this.visComplex[vp + pos] : null;
        u23 = visUCoords[vp + pos];
        v23 = visVCoords[vp + pos];

        // Find baseline AC = 13 :
        pos = relPos[2];

        if (logger.isDebugEnabled()) {
          logger.debug("vis baseline: {}", Arrays.toString(visStaIndexes[vp + pos]));
          logger.debug("T3  baseline: {}", Arrays.toString(triplet.getBaselineIndexes()[2]));
        }

        if (logger.isDebugEnabled()) {
          logger.debug("UV 13    = ({}, {})", visUCoords[vp + pos], visVCoords[vp + pos]);
          logger.debug("UV 12+23 = ({}, {})", (u12 + u23), (v12 + v23));
        }

        // pure complex visibility data :
        visData13 = (this.hasModel) ? this.visComplex[vp + pos] : null;

        // if target has models, then complex visibility are computed :
        if (!this.hasModel) {

          // Iterate on wave lengths :
          for (l = 0; l < this.nWaveLengths; l++) {
            t3Amp[k][l] = Double.NaN;
            t3AmpErr[k][l] = Double.NaN;

            t3Phi[k][l] = Double.NaN;
            t3PhiErr[k][l] = Double.NaN;

            // mark this value as invalid :
            flags[k][l] = true;
          }

        } else {

          // Iterate on wave lengths :
          for (l = 0; l < this.nWaveLengths; l++) {

            // baseline AB = 12 :
            vis12 = visData12[l];

            // baseline BC = 23
            vis23 = visData23[l];

            // baseline AC = 13 => conjuguate 31 (im = -im)
            vis31 = visData13[l].conjugate();

            // Compute RE/IM bispectrum with C12*C23*~C13 :
            ComplexUtils.bispectrum(vis12, vis23, vis31, t3Data);

            // amplitude :
            t3Amp[k][l] = t3Data.abs();

            // phase [-PI;PI] in degrees :
            t3Phi[k][l] = Math.toDegrees(t3Data.getArgument());

            // phase closure error (rad) :
            errPhi = this.noiseService.computeT3PhiError(vis12.abs(), vis23.abs(), vis31.abs());

            // amplitude error t3AmpErr = t3Amp * t3PhiErr :
            errAmp = t3Amp[k][l] * errPhi;
            t3AmpErr[k][l] = errAmp;

            // convert errPhi in degrees :
            errPhi = Math.toDegrees(errPhi);
            t3PhiErr[k][l] = errPhi;

            if (this.doNoise) {
              // use same random number for the 2 values (sigma = 1) :
              rand = VisNoiseService.gaussianNoise(this.random, 1d);

              // add gaussian noise with sigma = errAmp :
              t3Amp[k][l] += rand * errAmp;
              // add gaussian noise with sigma = errPhi :
              t3Phi[k][l] += rand * errPhi;
            }

            // mark this value as valid only if error is valid :
            flags[k][l] = !this.errorValid;
          }
        }

        // UV 1 coords (m) :
        t3U1Coords[k] = u12;
        t3V1Coords[k] = v12;

        // UV 2 coords (m) :
        t3U2Coords[k] = u23;
        t3V2Coords[k] = v23;

        // station indexes :
        t3StaIndexes[k] = triplet.getTripletIndexes();

        j++;
      }
    }

    this.oiFitsFile.addOiTable(t3);

    if (logger.isDebugEnabled()) {
      logger.debug("createOIT3: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
    }
  }

  /**
   * Return the beam mapping
   * @param beams beam list
   * @return beam mapping
   */
  private static Map<Beam, Short> createBeamMapping(final List<Beam> beams) {
    // Create Beam - index mapping :
    final Map<Beam, Short> beamMapping = new HashMap<Beam, Short>(beams.size());

    // Note : as Beam.hashCode is not implemented, the map acts as an IdentityMap (pointer equality)
    int i = 0;
    for (Beam b : beams) {
      beamMapping.put(b, Short.valueOf((short) (i + 1)));
      i++;
    }

    return beamMapping;
  }

  /**
   * Return the station indexes (2) per base line mapping (ordered)
   * @param beamMapping beam mapping
   * @param baseLines base line list
   * @return baseline station indexes
   */
  private static Map<BaseLine, short[]> createBaseLineMapping(final Map<Beam, Short> beamMapping, final List<BaseLine> baseLines) {
    // Create BaseLine - indexes mapping :
    final Map<BaseLine, short[]> baseLineIndexes = new LinkedHashMap<BaseLine, short[]>(baseLines.size());

    for (BaseLine bl : baseLines) {
      baseLineIndexes.put(bl, new short[]{
                beamMapping.get(bl.getBeam1()).shortValue(),
                beamMapping.get(bl.getBeam2()).shortValue()
              });
    }

    if (logger.isDebugEnabled()) {
      logger.debug("BaseLine indexes = ");
      for (short[] idx : baseLineIndexes.values()) {
        logger.debug("\t{} {}", idx[0], idx[1]);
      }
    }

    return baseLineIndexes;
  }

  /**
   * Convert UTC time in seconds
   * @param cal UTC time
   * @param calObs UTC start date of observation
   * @return UTC time in seconds
   */
  private static double calendarToTime(final Calendar cal, final Calendar calObs) {
    final double time = 3600d * cal.get(Calendar.HOUR_OF_DAY)
            + 60d * cal.get(Calendar.MINUTE)
            + cal.get(Calendar.SECOND);

    if (cal.get(Calendar.DAY_OF_MONTH) != calObs.get(Calendar.DAY_OF_MONTH)) {
      // observation is over 2 UTC days = observation starts before midnight and ends after :
      return 86400d + time;
    }
    return time;
  }

  /**
   * Convert UTC date to string 'YYYY-MM-DD'
   * @param cal UTC date
   * @return string representation
   */
  private static String calendarToString(final Calendar cal) {
    final StringBuilder sb = new StringBuilder(12);
    sb.append(cal.get(Calendar.YEAR)).append('-');

    final int month = cal.get(Calendar.MONTH) + 1;
    if (month < 10) {
      sb.append('0');
    }
    sb.append(month).append('-');

    final int day = cal.get(Calendar.DAY_OF_MONTH);
    if (day < 10) {
      sb.append('0');
    }
    sb.append(day);

    return sb.toString();


  }

  /**
   * Simple object representing a triplet (3 beams A, B, C) with corresponding baselines and table indexes
   */
  private final static class Triplet {

    /** station indexes (1 .. nBeams) */
    private final short[] tripletIndexes;
    /** baseline indexes (3 couples) */
    private final short[][] baselineIndexes;
    /** relative row positions in OI_VIS table (3) */
    private final int[] relativePosition;

    /**
     * Triplet factory method
     * @param idx 0-based indexes
     * @param baseLineMapping baseline mapping
     * @return triplet instance
     */
    static Triplet create(final int[] idx, final Map<BaseLine, short[]> baseLineMapping) {

      final short[] tIndexes = new short[3];
      for (int i = 0; i < 3; i++) {
        tIndexes[i] = (short) (idx[i] + 1);
      }

      // for tIndexes = [123] i.e. ABC
      // couples gives { 12 13 23 } i.e. AB AC BC

      // 3 couples :
      final short[][] bIndexes = new short[3][2];

      for (int i = 0, n = 0; i < 3; i++) {
        for (int j = i + 1; j < 3; j++) {
          bIndexes[n][0] = tIndexes[i];
          bIndexes[n][1] = tIndexes[j];
          n++;
        }
      }

      // Permutations to have { 12 23 13 } i.e. AB BC AC
      // i.e. exchange 2 and 3
      final short[] tmp = bIndexes[1];
      bIndexes[1] = bIndexes[2];
      bIndexes[2] = tmp;

      // Find relative positions in baseline ordering :
      final int[] pos = new int[3];

      final short[][] orderedbaseLineIndexes = baseLineMapping.values().toArray(new short[baseLineMapping.size()][2]);
      final int size = orderedbaseLineIndexes.length;

      short[] find, other;
      for (int n = 0; n < 3; n++) {
        find = bIndexes[n];
        pos[n] = -1;
        for (int i = 0; i < size; i++) {
          other = orderedbaseLineIndexes[i];

          if (Arrays.equals(find, other)) {
            pos[n] = i;
            break;
          }
        }
        if (pos[n] == -1) {
          throw new IllegalStateException("Impossible to find couple [" + find[0] + find[1] + "]");
        }
      }

      return new Triplet(tIndexes, bIndexes, pos);
    }

    /**
     * Protected constructor
     * @param tIndexes station indexes
     * @param bIndexes baseline indexes
     * @param pos relative row positions
     */
    private Triplet(final short[] tIndexes, final short[][] bIndexes, final int[] pos) {
      this.tripletIndexes = tIndexes;

      // 3 couples :
      this.baselineIndexes = bIndexes;

      // 3 positions :
      this.relativePosition = pos;
    }

    /**
     * Return the station indexes
     * @return station indexes (3)
     */
    public short[] getTripletIndexes() {
      return tripletIndexes;
    }

    /**
     * Return the baseline indexes (3 couples)
     * @return baseline indexes
     */
    public short[][] getBaselineIndexes() {
      return baselineIndexes;
    }

    /**
     * Return the relative row positions in OI_VIS table (3)
     * @return relative row positions
     */
    public int[] getRelativePosition() {
      return relativePosition;
    }

    /**
     * Return a string representation for this triplet
     * @return string representation
     */
    @Override
    public String toString() {
      final StringBuilder sb = new StringBuilder(32);
      sb.append("Triplet[");

      for (short s : this.tripletIndexes) {
        sb.append(s);
      }
      sb.append("]{ ");

      for (short[] b : this.baselineIndexes) {
        sb.append(b[0]).append(b[1]).append(' ');
      }

      sb.append("} = [ ");

      for (int i : this.relativePosition) {
        sb.append(i).append(' ');
      }

      sb.append(']');
      return sb.toString();
    }
  }

  /**
   * Return the flag to add gaussian noise to OIFits data; true if parameter doDataNoise = true and noise parameters are valid
   * @return flag to add gaussian noise to OIFits data; true if parameter doDataNoise = true and noise parameters are valid
   */
  public boolean isDoNoise() {
    return doNoise;
  }

  /**
   * Return true if returned errors are valid
   * @return true if returned errors are valid
   */
  public boolean isErrorValid() {
    return errorValid;
  }

  /**
   * Return the noise service
   * @return noise service
   */
  public NoiseService getNoiseService() {
    return noiseService;
  }
}
