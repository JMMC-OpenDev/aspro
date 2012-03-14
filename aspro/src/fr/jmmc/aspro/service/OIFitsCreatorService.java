/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Position3D;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.Telescope;
import fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData;
import fr.jmmc.aspro.util.CombUtils;
import fr.jmmc.aspro.util.ComplexUtils;
import fr.jmmc.jmal.ALX;
import fr.jmmc.jmal.complex.Complex;
import fr.jmmc.jmal.complex.ImmutableComplex;
import fr.jmmc.jmal.complex.MutableComplex;
import fr.jmmc.jmal.model.ModelComputeContext;
import fr.jmmc.jmal.model.ModelFunctionComputeContext;
import fr.jmmc.jmal.model.ModelManager;
import fr.jmmc.jmal.model.targetmodel.Model;
import fr.jmmc.jmcs.util.concurrent.ParallelJobExecutor;
import fr.jmmc.oitools.OIFitsConstants;
import fr.jmmc.oitools.image.FitsImage;
import fr.jmmc.oitools.model.OIArray;
import fr.jmmc.oitools.model.OIFitsChecker;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIT3;
import fr.jmmc.oitools.model.OITarget;
import fr.jmmc.oitools.model.OIVis;
import fr.jmmc.oitools.model.OIVis2;
import fr.jmmc.oitools.model.OIWavelength;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Future;
import java.util.logging.Level;

/**
 * This class contains the code to create OIFits structure from the current observation and performs noise modeling
 * @author bourgesl
 */
public final class OIFitsCreatorService {

  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(OIFitsCreatorService.class.getName());
  /** target Id */
  private final static short TARGET_ID = (short) 1;
  /** enable the OIFits validation */
  private final static boolean DO_VALIDATE = false;
  /** threshold to use parallel jobs (32 UV points) */
  private final static int JOB_THRESHOLD = 32;
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
    this.instrumentName = this.observation.getInstrumentConfiguration().getName();

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("arrName : " + this.arrayName);
      logger.fine("insName : " + this.instrumentName);
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

    if (logger.isLoggable(Level.INFO)) {
      logger.info("createOIFits : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");
    }

    if (DO_VALIDATE) {
      final OIFitsChecker checker = new OIFitsChecker();
      this.oiFitsFile.check(checker);

      // validation results
      if (logger.isLoggable(Level.INFO)) {
        logger.info("createOIFits : validation results\n" + checker.getCheckReport());
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
    double waveLength = this.lambdaMin + step / 2d;

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

      // TODO: compare directFT vs FFT + interpolation

      final long start = System.nanoTime();

      final boolean useAnalyticalModel = this.target.hasAnalyticalModel();

      final ModelManager modelManager;
      // model computation context              
      final ModelComputeContext context;

      if (useAnalyticalModel) {
        // Analytical model:
        modelManager = ModelManager.getInstance();

        // Clone models and normalize fluxes :
        final List<Model> normModels = ModelManager.normalizeModels(this.target.getModels());

        // prepare models once for all:
        context = modelManager.prepareModels(normModels, this.nWaveLengths);

      } else {
        // User Model:
        modelManager = null;

        // Get preloaded and prepared fits image:
        final FitsImage fitsImage = target.getUserModel().getFitsImage();

        if (fitsImage == null) {
          return;
        }

        // prepare models once for all:
        context = UserModelService.prepareModel(fitsImage, this.nWaveLengths);
      }

      // fast interrupt :
      if (currentThread.isInterrupted()) {
        return;
      }

      final int nPoints = this.nHAPoints * this.nBaseLines * this.nWaveLengths;

      if (logger.isLoggable(Level.INFO)) {
        logger.info("computeModelVisibilities: " + nPoints + " points - please wait ...");
      }


      // Allocate data array for complex visibility and error :
      final Complex[][] cVis = new Complex[this.nHAPoints * this.nBaseLines][this.nWaveLengths];
      final Complex[][] cVisError = new Complex[this.nHAPoints * this.nBaseLines][this.nWaveLengths];


      // enable parallel jobs if many points using user model:
      final int nJobs = (!useAnalyticalModel && nPoints > JOB_THRESHOLD) ? jobExecutor.getMaxParallelJob() : 1;

      // computation tasks:
      final Runnable[] jobs = new Runnable[nJobs];

      // create tasks:
      for (int i = 0; i < nJobs; i++) {
        final int jobIndex = i;

        final ModelComputeContext jobContext;
        if (i == 0) {
          jobContext = context;
        } else {
          jobContext = (useAnalyticalModel) ? ModelManager.cloneContext((ModelFunctionComputeContext) context)
                  : UserModelService.cloneContext((UserModelComputeContext) context);
        }

        jobs[i] = new Runnable() {

          @Override
          public void run() {
            // spatial frequencies for a single HA (spectral dispersion):
            final double[] ufreq = new double[nWaveLengths];
            final double[] vfreq = new double[nWaveLengths];

            double u, v;

            // Iterate on HA points :
            for (int i = 0, j = 0, k = 0, l = 0; i < nHAPoints; i++) {

              j = 0;

              // Iterate on baselines :
              for (final UVRangeBaseLineData uvBL : targetUVObservability) {
                k = nBaseLines * i + j;

                // job parallelism:
                if (k % nJobs == jobIndex) {

                  // UV coords (m) :
                  u = uvBL.getU()[i];
                  v = uvBL.getV()[i];

                  // Compute complex visibility using the target model:

                  // prepare spatial frequencies :
                  for (l = 0; l < nWaveLengths; l++) {
                    ufreq[l] = u / waveLengths[l];
                    vfreq[l] = v / waveLengths[l];
                  }

                  // compute complex visibilities :
                  if (useAnalyticalModel) {
                    cVis[k] = convertArray(modelManager.computeModels((ModelFunctionComputeContext) jobContext, ufreq, vfreq));
                  } else {
                    cVis[k] = convertArray(UserModelService.computeModel((UserModelComputeContext) jobContext, ufreq, vfreq));
                  }

                  if (cVis[k] == null || currentThread.isInterrupted()) {
                    // fast interrupt :
                    return;
                  }

                  // Iterate on wave lengths :
                  for (l = 0; l < nWaveLengths; l++) {
                    // complex visibility error or Complex.NaN:
                    cVisError[k][l] = noiseService.computeVisComplexError(cVis[k][l].abs());
                  }
                }

                // increment j:
                j++;
              } // baselines
            } // HA
          }
        };
      }

      if (nJobs > 1) {
        // execute jobs in parallel:
        final Future<?>[] futures = jobExecutor.fork(jobs);

        logger.fine("wait for jobs to terminate ...");

        jobExecutor.join("OIFitsCreatorService.computeModelVisibilities", futures);

      } else {
        // execute the single task using the current thread:
        jobs[0].run();
      }

      logger.info("computeModelVisibilities: duration = " + (1e-6d * (System.nanoTime() - start)) + " ms.");

      this.visComplex = cVis;
      this.visError = cVisError;
    }
  }

  /**
   * Convert mutable complex array to immutable complex array for safety reasons
   * @param array mutable complex array
   * @return immutable complex array
   */
  private static Complex[] convertArray(final MutableComplex[] array) {
    final int length = array.length;
    final Complex[] result = new Complex[length];

    for (int i = length - 1; i >= 0; i--) {
      result[i] = new ImmutableComplex(array[i]); // immutable complex for safety
    }
    return result;
  }

  /**
   * Create the OI_VIS table using internal computed visComplex data
   */
  private void createOIVis() {

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

    // vars:
    double jd;
    double u, v;
    double visRe, visIm, flux, vAmp, visErrRe, visErrIm;

    // complex visiblity with noise (sigma = visError)
    final Complex[][] visComplexNoisy = new Complex[vis.getNbRows()][this.nWaveLengths];

    // Iterate on HA points :
    for (int i = 0, j = 0, k = 0, l = 0; i < this.nHAPoints; i++) {

      j = 0;

      // Iterate on baselines :
      for (final UVRangeBaseLineData uvBL : this.targetUVObservability) {

        k = this.nBaseLines * i + j;

        // target id
        targetIds[k] = TARGET_ID;

        // jd from HA :
        jd = this.sc.convertHAToJD(this.obsHa[i], this.precRA);

        // UTC :
        times[k] = calendarToTime(this.sc.toCalendar(jd, false), calObs);

        // modified julian day :
        mjds[k] = AstroSkyCalc.mjd(jd);

        // integration time (s) :
        intTimes[k] = this.integrationTime;

        // UV coords (m) :
        u = uvBL.getU()[i];
        v = uvBL.getV()[i];

        uCoords[k] = u;
        vCoords[k] = v;

        // if target has models and errors are valid, then complex visibility are computed :
        if (!this.hasModel || !this.errorValid) {
          // Invalid => NaN value :

          // Iterate on wave lengths :
          for (l = 0; l < this.nWaveLengths; l++) {
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
          for (l = 0; l < this.nWaveLengths; l++) {

            // pure complex visibility data :
            visRe = this.visComplex[k][l].getReal();
            visIm = this.visComplex[k][l].getImaginary();

            // pure visibility amplitude :
            vAmp = this.visComplex[k][l].abs();

            // pure correlated fluxes or NaN:
            flux = this.noiseService.computeCorrelatedFlux(vAmp);

            // complex visibility error : visErrRe = visErrIm = visAmpErr / SQRT(2) or Complex.NaN :
            visErrRe = this.visError[k][l].getReal();
            visErrIm = this.visError[k][l].getImaginary();

            // error on correlated fluxes :
            visErr[k][l][0] = (float) (flux * visErrRe);
            visErr[k][l][1] = (float) (flux * visErrIm);

            if (this.doNoise) {
              // add gaussian noise with sigma = visErrRe / visErrIm :
              visComplexNoisy[k][l] = new ImmutableComplex(
                      visRe + this.noiseService.gaussianNoise(visErrRe),
                      visIm + this.noiseService.gaussianNoise(visErrIm)); // immutable complex for safety
            } else {
              visComplexNoisy[k][l] = this.visComplex[k][l];
            }

            // store pure (0..1) or noisy correlated fluxes (NaN if no flux):
            visData[k][l][0] = (float) (flux * visComplexNoisy[k][l].getReal());
            visData[k][l][1] = (float) (flux * visComplexNoisy[k][l].getImaginary());

            if (!isAmber) {
              // Waiting for explanations on every instrument processing to compute VisAmp/Phi :
              // following values are considered as invalid :
              visAmp[k][l] = Double.NaN;
              visAmpErr[k][l] = Double.NaN;

              visPhi[k][l] = Double.NaN;
              visPhiErr[k][l] = Double.NaN;
            }

            // mark this value as valid only if error is valid :
            flags[k][l] = !this.errorValid;
          }
        }

        // station indexes :
        staIndexes[k] = this.baseLineMapping.get(uvBL.getBaseLine());

        j++;
      }
    }

    /* Compute visAmp / visPhi as amber does */
    if (isAmber && this.hasModel && this.errorValid) {
      OIFitsAMBERService.amdlibFakeAmberDiffVis(vis, visComplexNoisy, this.visError, this.waveLengths);
    }

    this.oiFitsFile.addOiTable(vis);
  }

  /**
   * Create the OI_VIS2 table using internal computed visComplex data
   */
  private void createOIVis2() {
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

    for (int k = 0, l = 0; k < nRows; k++) {

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
            vis2Data[k][l] += this.noiseService.gaussianNoise(v2Err);
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
  }

  /**
   * Create the OI_T3 table
   */
  private void createOIT3() {

    if (this.nBeams < 3) {
      return;
    }

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

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("triplets  = " + triplets);
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
    for (int i = 0, j = 0, k = 0, l = 0, vp = 0; i < this.nHAPoints; i++) {

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

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("vis baseline = " + Arrays.toString(visStaIndexes[vp + pos]));
          logger.fine("T3  baseline = " + Arrays.toString(triplet.getBaselineIndexes()[0]));
        }

        // pure complex visibility data :
        visData12 = (this.hasModel) ? this.visComplex[vp + pos] : null;
        u12 = visUCoords[vp + pos];
        v12 = visVCoords[vp + pos];

        // Find baseline BC = 23 :
        pos = relPos[1];

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("vis baseline = " + Arrays.toString(visStaIndexes[vp + pos]));
          logger.fine("T3  baseline = " + Arrays.toString(triplet.getBaselineIndexes()[1]));
        }

        // pure complex visibility data :
        visData23 = (this.hasModel) ? this.visComplex[vp + pos] : null;
        u23 = visUCoords[vp + pos];
        v23 = visVCoords[vp + pos];

        // Find baseline AC = 13 :
        pos = relPos[2];

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("vis baseline = " + Arrays.toString(visStaIndexes[vp + pos]));
          logger.fine("T3  baseline = " + Arrays.toString(triplet.getBaselineIndexes()[2]));
        }

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("UV 13    = " + visUCoords[vp + pos] + ", " + visVCoords[vp + pos]);
          logger.fine("UV 12+23 = " + (u12 + u23) + ", " + (v12 + v23));
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
              rand = this.noiseService.gaussianNoise(1d);

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

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("BaseLine indexes = ");
      for (short[] idx : baseLineIndexes.values()) {
        logger.fine(" " + idx[0] + " " + idx[1]);
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
