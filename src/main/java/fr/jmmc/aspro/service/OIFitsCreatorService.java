/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.observability.TargetPointInfo;
import fr.jmmc.aspro.model.oi.FocalInstrument;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.LonLatAlt;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Position3D;
import fr.jmmc.aspro.model.oi.SpectralSetup;
import fr.jmmc.aspro.model.oi.SpectralSetupQuantity;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.Telescope;
import fr.jmmc.aspro.model.oi.UserModel;
import fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData;
import fr.jmmc.aspro.service.UserModelService.MathMode;
import fr.jmmc.aspro.util.StatUtils;
import fr.jmmc.aspro.util.StatUtils.ComplexDistribution;
import static fr.jmmc.aspro.util.StatUtils.N_SAMPLES;
import static fr.jmmc.aspro.util.StatUtils.SAMPLING_FACTOR_MEAN;
import static fr.jmmc.aspro.util.StatUtils.SAMPLING_FACTOR_VARIANCE;
import fr.jmmc.jmal.ALX;
import fr.jmmc.jmal.complex.Complex;
import fr.jmmc.jmal.complex.MutableComplex;
import fr.jmmc.jmcs.gui.task.InterruptableThread;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.SpecialChars;
import fr.jmmc.oitools.OIFitsConstants;
import fr.jmmc.oitools.meta.OIFitsStandard;
import fr.jmmc.oitools.model.DataModel;
import fr.jmmc.oitools.model.OIArray;
import fr.jmmc.oitools.model.OIFitsChecker;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIT3;
import fr.jmmc.oitools.model.OITarget;
import fr.jmmc.oitools.model.OIVis;
import fr.jmmc.oitools.model.OIVis2;
import fr.jmmc.oitools.model.OIWavelength;
import fr.jmmc.oitools.util.CombUtils;
import fr.nom.tam.fits.FitsException;
import java.io.IOException;
import static java.lang.Math.PI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;
import java.util.Map;
import net.jafama.FastMath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class contains the code to create OIFits structure from the current observation and performs noise modeling
 * 
 * @author bourgesl
 */
public final class OIFitsCreatorService extends AbstractOIFitsProducer {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(OIFitsCreatorService.class.getName());
    /** target Id */
    private final static short TARGET_ID = (short) 1;
    /** enable the OIFits validation */
    private final static boolean DO_VALIDATE_OIFITS = false;
    /** enable DEBUG mode */
    private final static boolean DEBUG_SNR = false;
    /** enable Differential visibility in the OI_VIS table */
    private final static boolean ENABLE_DIFF_VIS = true;

    /* members */

 /* reused observability data */
    /** beam list */
    private final List<Beam> beams;
    /** number of beams */
    private final int nBeams;
    /** base line list */
    private final List<BaseLine> baseLines;
    /** number of baselines */
    private final int nBaseLines;
    /** sky calc instance */
    private final AstroSkyCalc sc;

    /* reused uv coverage data */
    /** number of uv points */
    private final int nObsPoints;
    /** target information for each uv point couples */
    private final TargetPointInfo[] targetPointInfos;
    /** list of uv point couples corresponding to the target observability */
    private final List<UVRangeBaseLineData> targetUVObservability;

    /* internal */
    private ObservationSetting cObservation = null;
    private UserModel cUserModel = null;

    /** selected instrument mode */
    private FocalInstrumentMode instrumentMode = null;
    /** flag indicating if the target model wavelengths are compatible with the instrument mode */
    private boolean isModelWLValid = true;
    /** true to use instrument bias; false to compute only theoretical error */
    private final boolean useInstrumentBias;
    /** interferometer description */
    private InterferometerDescription interferometer = null;
    /** instrument name */
    private String instrumentName = null;
    /** arrname keyword value (interferometer name) */
    private String arrNameKeyword = null;
    /** insname keyword value (instrument_LAMBDA1-LAMBDA2-Nch) */
    private String insNameKeyword = null;
    /** instrument differential visibility support */
    private boolean instrumentVisDiff = false;
    /** Station mapping */
    private Map<Station, Short> stationMapping = null;
    /** beam mapping */
    private Map<Beam, Short> beamMapping = null;
    /** baseline mapping */
    private Map<BaseLine, short[]> baseLineMapping = null;
    /** integration time (s) */
    private double integrationTime = 300d;

    /**
     * Protected constructor
     * @param observation observation settings
     * @param target target to process
     * @param beams beam list
     * @param baseLines base line list
     * @param useInstrumentBias true to use instrument bias; false to compute only theoretical error
     * @param doDataNoise flag to add gaussian noise to OIFits data
     * @param supersampling OIFits supersampling preference
     * @param mathMode OIFits MathMode preference
     * @param targetPointInfos target information for each uv point couples
     * @param targetUVObservability list of UV coordinates per baseline
     * @param sc sky calc instance
     * @param warningContainer container for warning messages
     */
    protected OIFitsCreatorService(final ObservationSetting observation,
                                   final Target target,
                                   final List<Beam> beams,
                                   final List<BaseLine> baseLines,
                                   final boolean useInstrumentBias,
                                   final boolean doDataNoise,
                                   final int supersampling,
                                   final MathMode mathMode,
                                   final TargetPointInfo[] targetPointInfos,
                                   final List<UVRangeBaseLineData> targetUVObservability,
                                   final AstroSkyCalc sc,
                                   final WarningContainer warningContainer) throws IllegalArgumentException {

        super(target, supersampling, mathMode);

        this.beams = beams;
        this.nBeams = this.beams.size();
        this.baseLines = baseLines;
        this.nBaseLines = this.baseLines.size();
        this.nObsPoints = targetPointInfos.length;
        this.targetPointInfos = targetPointInfos;
        this.targetUVObservability = targetUVObservability;
        this.sc = sc;

        if (observation.getInstrumentConfiguration().getAcquisitionTime() != null) {
            this.integrationTime = observation.getInstrumentConfiguration().getAcquisitionTime().doubleValue();
        }

        // use Bias:
        this.useInstrumentBias = useInstrumentBias;

        // Prepare with observation:
        // note: may adjust wavelengths:
        prepare(observation, warningContainer);

        // Prepare the noise service with exact wavelengths:
        // note: NoiseService parameter dependencies:
        // observation {target}
        // parameter: warningContainer
        final NoiseService ns = new NoiseService(observation, target, targetPointInfos, useInstrumentBias, warningContainer,
                this.waveLengths, this.waveBands);

        // do not generate errors for the DEMO interferometer
        if (!ns.isValid() || "DEMO".equalsIgnoreCase(this.arrNameKeyword)) {
            this.noiseService = null;
        } else {
            this.noiseService = ns;
        }

        // do noise :
        this.doNoise = (doDataNoise && (this.noiseService != null));
    }

    /**
     * Prepare OIFits keywords and the instrumental spectral configuration and may add warning messages
     * @param observation observation to use
     * @param warningContainer warning container to use if needed
     */
    private void prepare(final ObservationSetting observation, final WarningContainer warningContainer) throws IllegalArgumentException {

        final InterferometerConfiguration intConf = observation.getInterferometerConfiguration().getInterferometerConfiguration();

        this.interferometer = intConf.getInterferometer();

        // Use interferometer name (VLTI, CHARA ...) and not 'VLTI Period 91':
        this.arrNameKeyword = this.interferometer.getName();

        final FocalInstrument instrument = observation.getInstrumentConfiguration().getInstrumentConfiguration().getFocalInstrument();

        // use alias or real instrument name:
        this.instrumentName = instrument.getAliasOrName();
        this.instrumentVisDiff = (instrument.isVisDiff()!= null) ? instrument.isVisDiff().booleanValue() : false;

        if (logger.isDebugEnabled()) {
            logger.debug("arrNameKeyword: {}", this.arrNameKeyword);
            logger.debug("instrumentName: {}", this.instrumentName);
            logger.debug("instrumentVisDiff: {}", this.instrumentVisDiff);
        }

        final FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();
        if (insMode == null) {
            throw new IllegalStateException("The instrumentMode is empty !");
        }
        this.instrumentMode = insMode;

        prepareInstrumentMode(warningContainer);

        // user model if defined:
        final UserModel userModel = (!target.hasAnalyticalModel()) ? target.getUserModel() : null;

        if (userModel != null && userModel.isFileValid()) {
            // validate image against the given observation:
            ObservationManager.validateUserModel(observation, userModel);

            // prepare later image according to apodization
            this.cObservation = observation;
            this.cUserModel = userModel;
        }
    }

    /**
     * Prepare the instrumental spectral configuration: Wavelengths and may add warning messages
     * @param warningContainer warning container to use if needed
     */
    private void prepareInstrumentMode(final WarningContainer warningContainer) {
        if (logger.isDebugEnabled()) {
            logger.debug("instrumentMode: {}", instrumentMode.getName());
        }

        // Get wavelength range for the selected instrument mode :
        final double lambdaMin = AsproConstants.MICRO_METER * instrumentMode.getWaveLengthMin();
        final double lambdaMax = AsproConstants.MICRO_METER * instrumentMode.getWaveLengthMax();

        // TODO: handle properly spectral channels (rebinning):
        int nWaveLengths = instrumentMode.getSpectralChannels();

        if (logger.isDebugEnabled()) {
            logger.debug("lambdaMin: {}", lambdaMin);
            logger.debug("lambdaMax: {}", lambdaMax);
            logger.debug("nChannels: {}", nWaveLengths);
        }

        // prepare wavelengths independently of the user model Fits cube (wavelengths):
        final double waveBand;

        if (instrumentMode.getTable() != null) {
            final SpectralSetup table = instrumentMode.getTable();
            this.waveLengths = table.getAndScaleColumn(SpectralSetupQuantity.LAMBDA, AsproConstants.MICRO_METER);
            this.waveBands = table.getAndScaleColumn(SpectralSetupQuantity.DELTA_LAMBDA, AsproConstants.MICRO_METER);
            waveBand = StatUtils.mean(this.waveBands);
        } else {
            // TODO: handle variable bandwdith:
            waveBand = (lambdaMax - lambdaMin) / nWaveLengths;
            this.waveLengths = computeWaveLengths(lambdaMin, waveBand, nWaveLengths);
            this.waveBands = new double[nWaveLengths];
            Arrays.fill(waveBands, waveBand);
        }

        // Initial Wavelength information:
        String firstChannel = Double.toString(convertWL(this.waveLengths[0]));
        String lastChannel = (nWaveLengths > 1) ? Double.toString(convertWL(this.waveLengths[nWaveLengths - 1])) : null;

        addInformation(warningContainer, this.instrumentName + " instrument mode: "
                + nWaveLengths + " channels "
                + '[' + firstChannel + ((lastChannel != null) ? (" - " + lastChannel) : "") + " " + SpecialChars.UNIT_MICRO_METER + "] "
                + "(band: " + convertWL(waveBand) + " " + SpecialChars.UNIT_MICRO_METER + ')');

        // keep number of channels:
        final int nChannels = nWaveLengths;

        // Keep only spectral channels where user model is defined:
        // note: Instrument spectral channels (waveLengths, waveBand, lambdaMin, lambdaMax) can be modified by this method:
        this.isModelWLValid = prepareUserModel(warningContainer);

        // refresh:
        nWaveLengths = this.waveLengths.length;

        // adjust used spectral channels in information and log:
        if (nWaveLengths != nChannels) {
            // Wavelength information:
            firstChannel = Double.toString(convertWL(this.waveLengths[0]));
            lastChannel = (nWaveLengths > 1) ? Double.toString(convertWL(this.waveLengths[nWaveLengths - 1])) : null;

            addWarning(warningContainer, "Restricted instrument mode: "
                    + this.waveLengths.length + " channels "
                    + '[' + firstChannel + ((lastChannel != null) ? (" - " + lastChannel) : "") + " " + SpecialChars.UNIT_MICRO_METER + "] ");
        }

        this.insNameKeyword = this.instrumentName + '_' + firstChannel + ((lastChannel != null) ? ("-" + lastChannel) : "") + '-' + nWaveLengths + "ch";

        if (logger.isDebugEnabled()) {
            logger.debug("insNameKeyword: {}", insNameKeyword);
        }
    }

    private void prepareUserModel(final ObservationSetting observation, final UserModel userModel) throws IllegalArgumentException {
        if (userModel != null && userModel.isFileValid()) {
            boolean valid = false;
            try {
                // Inhibits thread interrupt:
                InterruptableThread.setThreadMayInterruptIfRunning(false);

                ObservationManager.validateOrPrepareUserModel(observation, userModel);

                // model is valid:
                valid = true;

            } catch (FitsException fe) {
                throw new IllegalArgumentException("Could not read file: " + userModel.getFile(), fe);
            } catch (IOException ioe) {
                throw new IllegalArgumentException("Could not read file: " + userModel.getFile(), ioe);
            } catch (IllegalArgumentException iae) {
                throw iae;
            } finally {
                // anyway, update the valid flag:
                userModel.setFileValid(valid);

                // Restore thread interrupt, maybe now:
                InterruptableThread.setThreadMayInterruptIfRunning(true);
            }
        }
    }

    /**
     * Create the OIFits structure with OI_ARRAY, OI_TARGET, OI_WAVELENGTH, OI_VIS tables
     * @return OIFits structure
     */
    public OIFitsFile createOIFits() throws IllegalArgumentException {
        if (!this.isModelWLValid) {
            // invalid user model against instrumental spectral configuration:
            return null;
        }
        if (cUserModel != null) {
            // defered user model preparation for proper apodization according to current observation (telescope):
            prepareUserModel(cObservation, cUserModel);

            // fast interrupt :
            if (Thread.currentThread().isInterrupted()) {
                return null;
            }
        }

        // Start the computations :
        final long start = System.nanoTime();

        // create a new EMPTY OIFits structure :
        this.oiFitsFile = new OIFitsFile(OIFitsStandard.VERSION_1);

        // Create station mappings:
        this.stationMapping = createStationMapping(this.interferometer.getStations());

        // Create beam and base line mappings :
        this.beamMapping = createBeamMapping(this.stationMapping, this.beams);
        this.baseLineMapping = createBaseLineMapping(this.beamMapping, this.baseLines);

        // OI_ARRAY :
        this.createOIArray();

        // OI_TARGET :
        this.createOITarget();

        // OI_WAVELENGTH :
        this.createOIWaveLength();

        // Compute complex visibilities:
        if (!this.computeModelVisibilities()) {
            // unable to compute complex visiblities so abort:
            return null;
        }

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
        if (this.instrumentName.startsWith(AsproConstants.INS_PIONIER)) {
            // Remove OI_VIS table if instrument is PIONIER:
            final OIVis vis = this.oiFitsFile.getOiVis()[0];

            this.oiFitsFile.removeOiTable(vis);
        }

        logger.info("createOIFits: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

        if (DO_VALIDATE_OIFITS) {
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

        // Analyze the OIFits file to get its configuration:
        this.oiFitsFile.analyze();

        return this.oiFitsFile;
    }

    /**
     * Create the OI_ARRAY table
     *
     * Note : station indexes are given according to the beam list ordering starting from 1
     */
    private void createOIArray() {

        // Create OI_ARRAY table :
        final OIArray oiArray = new OIArray(this.oiFitsFile, this.interferometer.getStations().size());

        // Array Name :
        oiArray.setArrName(this.arrNameKeyword);

        // Position :
        oiArray.setFrame(OIFitsConstants.KEYWORD_FRAME_GEOCENTRIC);

        Position3D position = this.interferometer.getPosition();

        oiArray.setArrayXYZ(position.getPosX(), position.getPosY(), position.getPosZ());

        // Stations :
        final LonLatAlt posCenter = this.interferometer.getPosSph();
        final Position3D geocentricPos = new Position3D();

        int i = 0;
        Telescope tel;

        for (final Station station : this.interferometer.getStations()) {

            tel = station.getTelescope();
            oiArray.getTelName()[i] = tel.getName();
            oiArray.getDiameter()[i] = (float) tel.getDiameter();

            oiArray.getStaName()[i] = station.getName();
            oiArray.getStaIndex()[i] = this.stationMapping.get(station).shortValue();

            // rotate the equatorial local coordinates to geocentric frame:
            GeocentricCoords.convertEquatorialToGeocentric(station.getRelativePosition(), geocentricPos, posCenter);

            oiArray.setStaXYZ(i, geocentricPos.getPosX(), geocentricPos.getPosY(), geocentricPos.getPosZ());
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
            // toShort km/s in m/s :
            oiTarget.getSysVel()[0] = this.target.getSYSVEL().doubleValue() * 1e3;
        }
        oiTarget.getVelTyp()[0] = OIFitsConstants.UNKNOWN_VALUE;
        // Use VELTYP (mostly undefined) :
        oiTarget.getVelDef()[0] = OIFitsConstants.COLUMN_VELDEF_OPTICAL;

        // Proper motion :
        if (this.target.getPMRA() != null && this.target.getPMDEC() != null) {
            // toShort mas/year in deg/year :
            oiTarget.getPmRa()[0] = this.target.getPMRA().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES;
            oiTarget.getPmDec()[0] = this.target.getPMDEC().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES;
        }

        // Missing PM RA/DEC errors :
        oiTarget.getPmRaErr()[0] = 0d;
        oiTarget.getPmDecErr()[0] = 0d;

        // Parallax :
        if (this.target.getPARALLAX() != null && this.target.getPARAERR() != null) {
            // toShort mas in deg :
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
        final int nWaveLengths = this.waveLengths.length;
        final OIWavelength waves = new OIWavelength(this.oiFitsFile, nWaveLengths);
        waves.setInsName(this.insNameKeyword);

        final float[] effWave = waves.getEffWave();
        final float[] effBand = waves.getEffBand();

        for (int i = 0; i < nWaveLengths; i++) {
            effWave[i] = (float) this.waveLengths[i];
            effBand[i] = (float) this.waveBands[i];
        }

        this.oiFitsFile.addOiTable(waves);
    }

    @Override
    protected UVFreqTable computeSpatialFreqTable(final double[] sampleWaveLengths) {

        final int nWLen = sampleWaveLengths.length;

        final int nBl = nBaseLines;
        final int nObs = nObsPoints;

        final int nRows = nObs * nBl;

        final UVFreqTable freqTable = new UVFreqTable(nBl, nRows, nWLen);

        // Allocate data array for spatial frequencies:
        final double[][] ufreq = freqTable.ufreq;
        final double[][] vfreq = freqTable.vfreq;

        // index of the observation point (HA):
        final int[] ptIdx = freqTable.ptIdx;

        // new block to limit variable scope:
        // Inverse wavelength:
        final double[] invWaveLengths = new double[nWLen];
        for (int l = 0; l < nWLen; l++) {
            invWaveLengths[l] = 1d / sampleWaveLengths[l];
        }

        // Iterate on baselines :
        for (int i, j = 0, k, l; j < nBl; j++) {

            final UVRangeBaseLineData uvBL = this.targetUVObservability.get(j);

            // Iterate on observable UV points :
            for (i = 0; i < nObs; i++) {
                k = nBl * i + j;
                // define point index:
                ptIdx[k] = i;

                // u/v freqs ...
                final double[] uRow = ufreq[k];
                final double[] vRow = vfreq[k];

                // UV coords (m) :
                final double u = uvBL.getU()[i];
                final double v = uvBL.getV()[i];

                // prepare spatial frequencies :
                for (l = 0; l < nWLen; l++) {
                    uRow[l] = u * invWaveLengths[l];
                    vRow[l] = v * invWaveLengths[l];
                }
            }
        }
        return freqTable;
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

        // generate squared correlated fluxes (VIS2DATA):
        final boolean useComplexVis = isAmber
                || AsproConstants.INS_GRAVITY.equals(this.instrumentName)
                || this.instrumentName.startsWith(AsproConstants.INS_MATISSE);

        // Update the data model before calling new OIVis():
        DataModel.setOiVisComplexSupport(useComplexVis);

        // Create OI_VIS table :
        final OIVis vis = new OIVis(this.oiFitsFile, this.insNameKeyword, this.nObsPoints * this.nBaseLines);
        vis.setArrName(this.arrNameKeyword);

        // Get target information for each UV point:
        final TargetPointInfo[] obsPointInfos = this.targetPointInfos;

        // Compute UTC start date of the first point :
        final Calendar calObs = this.sc.toCalendar(obsPointInfos[0].getJd(), false);

        final String dateObs = calendarToString(calObs);
        vis.setDateObs(dateObs);

        // Columns :
        final short[] targetIds = vis.getTargetId();
        final double[] times = vis.getTime();
        final double[] mjds = vis.getMJD();
        final double[] intTimes = vis.getIntTime();

        final float[][][] visData = vis.getVisData();
        final float[][][] visErr = vis.getVisErr();

        final boolean useVisData = (visData != null && visErr != null);

        final double[][] visAmp = vis.getVisAmp();
        final double[][] visAmpErr = vis.getVisAmpErr();

        final double[][] visPhi = vis.getVisPhi();
        final double[][] visPhiErr = vis.getVisPhiErr();

        final double[] uCoords = vis.getUCoord();
        final double[] vCoords = vis.getVCoord();

        final short[][] staIndexes = vis.getStaIndex();
        final boolean[][] flags = vis.getFlag();

        final int nWaveLengths = this.waveLengths.length;

        // complex visiblity with noise (sigma = visError):
        final MutableComplex visComplexNoisy = new MutableComplex();

        final NoiseService ns = this.noiseService;

        if (this.hasModel && (ns != null) && !isAmber && this.instrumentVisDiff) {
            logger.info("createOIVis: {} VisAmp/Phi errors computed using {} random complex visiblities",
                    ENABLE_DIFF_VIS ? "Differential" : "Absolute", N_SAMPLES);
        }

        // vars:
        double jd, time, mjd;
        double u, v;
        double visRe, visIm, flux, visErrCplx;
        double vamp, vphi, errAmp, errPhi;

        // distribution samples:
        double[] distRe = null, distIm = null;

        // try resampling:
        double re, im, sample, diff;
        double vamp_sum, vamp_sum_diff, vamp_sum_diff_square;
        double re_sum, im_sum, cos_phi, sin_phi;
        double s_vamp_mean, s_vamp_err;
        double vphi_sum_diff, vphi_sum_diff_square;
        double s_vphi_mean, s_vphi_err;

        final double[] re_samples = new double[N_SAMPLES];
        final double[] im_samples = new double[N_SAMPLES];
        final double[] vamp_samples = new double[N_SAMPLES];
        final double[] vphi_samples = new double[N_SAMPLES];
        int[] visRndIdxRow = null;
        boolean doFlag;

        boolean doNormVisDiff = false;

        // Use mutable complex carefully:
        final double normFactorWL = 1.0 / (nWaveLengths - 1);

        final MutableComplex cpxVisSum = new MutableComplex();
        final MutableComplex cpxVisRef = new MutableComplex();
        final MutableComplex cpxVisDiff = new MutableComplex();

        // Iterate on observable UV points :
        for (int i = 0, j, k, l, n; i < nObsPoints; i++) {

            // jd at the observed uv point:
            jd = obsPointInfos[i].getJd();

            // UTC :
            time = calendarToTime(sc.toCalendar(jd, false), calObs);

            // modified julian day :
            mjd = AstroSkyCalc.mjd(jd);

            j = 0;

            // Iterate on baselines :
            for (final UVRangeBaseLineData uvBL : targetUVObservability) {

                k = nBaseLines * i + j;

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

                // if target has a model, then complex visibility are computed :
                if (!hasModel) {
                    // Invalid => NaN value :

                    // Iterate on wave lengths :
                    for (l = 0; l < nWaveLengths; l++) {
                        if (useVisData) {
                            visData[k][l][0] = Float.NaN;
                            visData[k][l][1] = Float.NaN;

                            visErr[k][l][0] = Float.NaN;
                            visErr[k][l][1] = Float.NaN;
                        }

                        visAmp[k][l] = Double.NaN;
                        visAmpErr[k][l] = Double.NaN;

                        visPhi[k][l] = Double.NaN;
                        visPhiErr[k][l] = Double.NaN;

                        // mark this value as invalid :
                        flags[k][l] = true;
                    }

                } else {
                    if (ns != null) {
                        // Get the complex distribution for this row:
                        distRe = visRndDist[k].getSamples()[0];
                        distIm = visRndDist[k].getSamples()[1];

                        // Get proper index:
                        visRndIdxRow = this.visRndIdx[k];
                    }

                    if (!isAmber && ENABLE_DIFF_VIS) {
                        // Compute Vref (complex) as mean(V) along wavelength axis:

                        // reset:
                        cpxVisSum.updateComplex(0d, 0d);

                        // Iterate on wave lengths :
                        for (l = 0; l < nWaveLengths; l++) {
                            /* sum all R and I */
                            cpxVisSum.add(visComplex[k][l]); // sum of all pure complex visibilities
                        }
                        // note: normalization is done below ie substract mean(cpxVis)
                    }

                    // Iterate on wave lengths :
                    for (l = 0; l < nWaveLengths; l++) {
                        // Get pure complex visibility:
                        visComplexNoisy.updateComplex(visComplex[k][l]);

                        doFlag = visSnrFlag[k][l];

                        // Define first visData / visErr:
                        if (ns == null) {
                            if (useVisData) {
                                visData[k][l][0] = Float.NaN;
                                visData[k][l][1] = Float.NaN;

                                visErr[k][l][0] = Float.NaN;
                                visErr[k][l][1] = Float.NaN;
                            }
                        } else {
                            // pure complex visibility data :
                            visRe = this.visComplex[k][l].getReal();
                            visIm = this.visComplex[k][l].getImaginary();

                            // complex visibility error : visErrRe = visErrIm = visAmpErr or Complex.NaN :
                            visErrCplx = visError[k][l];

                            if (doNoise) {
                                // Use the corresponding sample:
                                visComplexNoisy.updateComplex(
                                        visRe + (visErrCplx * distRe[visRndIdxRow[l]]),
                                        visIm + (visErrCplx * distIm[visRndIdxRow[l]])
                                ); // immutable complex for safety
                            }

                            if (useVisData) {
                                // pure correlated fluxes or NaN:
                                flux = ns.getCorrelatedFluxWeight(i, l);

                                // store pure (0..1) or noisy correlated fluxes (NaN if no flux):
                                visData[k][l][0] = (float) (flux * visComplexNoisy.getReal());
                                visData[k][l][1] = (float) (flux * visComplexNoisy.getImaginary());

                                // error on correlated fluxes :
                                visErr[k][l][0] = (float) (flux * visErrCplx);
                                visErr[k][l][1] = (float) (flux * visErrCplx);
                            }
                        }

                        if (!isAmber) {
                            if (instrumentVisDiff) {
                                if (ns == null) {
                                    if (ENABLE_DIFF_VIS) {
                                        re = visComplex[k][l].getReal();
                                        im = visComplex[k][l].getImaginary();

                                        /* then construct Cref by substracting current R and I
                                         * at that Wlen and make the arithmetic mean */
                                        cpxVisRef.updateComplex(
                                                normFactorWL * (cpxVisSum.getReal() - re),
                                                normFactorWL * (cpxVisSum.getImaginary() - im)
                                        );
                                        // VisDiff = CNop / CRef
                                        cpxVisDiff.set(re, im).divide(cpxVisRef);

                                        vamp = cpxVisDiff.abs();
                                        vphi = cpxVisDiff.getArgument();
                                    } else {
                                        vamp = visComplex[k][l].abs();
                                        vphi = visComplex[k][l].getArgument();
                                    }
                                    errAmp = errPhi = Double.NaN;

                                } else {
                                    // pure complex visibility data :
                                    visRe = visComplex[k][l].getReal();
                                    visIm = visComplex[k][l].getImaginary();

                                    // complex visibility error : visErrRe = visErrIm = visAmpErr or Complex.NaN :
                                    visErrCplx = visError[k][l];

                                    // pure visibility amplitude:
                                    if (ENABLE_DIFF_VIS) {
                                        re = visRe;
                                        im = visIm;

                                        /* then construct Cref by substracting current R and I
                                         * at that Wlen and make the arithmetic mean */
                                        cpxVisRef.updateComplex(
                                                normFactorWL * (cpxVisSum.getReal() - re),
                                                normFactorWL * (cpxVisSum.getImaginary() - im)
                                        );
                                        // VisDiff = CNop / CRef
                                        cpxVisDiff.set(re, im).divide(cpxVisRef);

                                        vamp = cpxVisDiff.abs();
                                    } else {
                                        // pure visibility amplitude:
                                        vamp = Math.sqrt(visRe * visRe + visIm * visIm); // TODO: remove bias
                                    }

                                    // note: if (V2 - bias) is negative then vamp must be NaN (no error)
                                    // pure visibility phase (unused):
                                    vphi = (visIm != 0.0) ? FastMath.atan2(visIm, visRe) : 0.0;

                                    // Sampling complex visibilities:
                                    vamp_sum = vamp_sum_diff = vamp_sum_diff_square = 0.0;
                                    re_sum = im_sum = 0.0;

                                    // 1. compute average(complex visibility):
                                    // bivariate distribution (complex normal):
                                    for (n = 0; n < N_SAMPLES; n++) {
                                        // update nth sample:
                                        re = visRe + (visErrCplx * distRe[n]);
                                        im = visIm + (visErrCplx * distIm[n]);

                                        // amplitude:
                                        if (ENABLE_DIFF_VIS) {
                                            /* then construct Cref by substracting current R and I
                                             * at that Wlen and make the arithmetic mean */
                                            cpxVisRef.updateComplex(
                                                    normFactorWL * (cpxVisSum.getReal() - re),
                                                    normFactorWL * (cpxVisSum.getImaginary() - im)
                                            );
                                            // VisDiff = CNop / CRef
                                            cpxVisDiff.set(re, im).divide(cpxVisRef);

                                            re = cpxVisDiff.getReal();
                                            im = cpxVisDiff.getImaginary();
                                        }
                                        // average complex value:
                                        re_samples[n] = re;
                                        im_samples[n] = im;

                                        re_sum += re;
                                        im_sum += im;

                                        // phase in [-PI; PI]:
                                        sample = (im != 0.0) ? FastMath.atan2(im, re) : 0.0;
                                        vphi_samples[n] = sample;
                                    }

                                    // mean(vphi):
                                    s_vamp_mean = Math.sqrt(re_sum * re_sum + im_sum * im_sum);
                                    s_vphi_mean = (im_sum != 0.0) ? FastMath.atan2(im_sum, re_sum) : 0.0;

                                    // rotate by -phi:
                                    cos_phi = re_sum / s_vamp_mean;
                                    sin_phi = im_sum / s_vamp_mean;

                                    vphi_sum_diff = vphi_sum_diff_square = 0.0;

                                    // compute angle variance and amplitude:
                                    for (n = 0; n < N_SAMPLES; n++) {
                                        // phase in [-PI; PI]:
                                        // Compensated-summation variant for better numeric precision:
                                        // check if diff is [-PI; PI]:
                                        diff = distanceAngle(vphi_samples[n], s_vphi_mean);
                                        vphi_sum_diff += diff;
                                        vphi_sum_diff_square += diff * diff;

                                        // Correct amplitude by estimated phase:
                                        // Amp = Re { C * phasor(-phi) }
                                        sample = re_samples[n] * cos_phi + im_samples[n] * sin_phi; // -phi => + imaginary part in complex mult
                                        vamp_samples[n] = sample;

                                        // Compensated-summation variant for better numeric precision:
                                        vamp_sum += sample;
                                        diff = sample - vamp;
                                        vamp_sum_diff += diff;
                                        vamp_sum_diff_square += diff * diff;
                                    }

                                    // mean(vamp):
                                    s_vamp_mean = SAMPLING_FACTOR_MEAN * vamp_sum;

                                    // error(vamp):
                                    // note: this algorithm ensures correctness (stable) even if the mean used in diff is wrong !
                                    s_vamp_err = Math.sqrt(
                                            SAMPLING_FACTOR_VARIANCE * (vamp_sum_diff_square - (SAMPLING_FACTOR_MEAN * (vamp_sum_diff * vamp_sum_diff)))
                                    );

                                    // error(vphi):
                                    // note: this algorithm ensures correctness (stable) even if the mean used in diff is wrong !
                                    s_vphi_err = Math.sqrt(
                                            SAMPLING_FACTOR_VARIANCE * (vphi_sum_diff_square - (SAMPLING_FACTOR_MEAN * (vphi_sum_diff * vphi_sum_diff)))
                                    );

                                    if (DEBUG) {
                                        logger.info("Sampling[" + N_SAMPLES + "] snr=" + (s_vamp_mean / s_vamp_err) + " AMP "
                                                + " avg= " + s_vamp_mean + " vamp= " + vamp + " ratio: " + (s_vamp_mean / vamp)
                                                + " stddev= " + s_vamp_err + " errAmp= " + visErrCplx + " ratio: " + (s_vamp_err / visErrCplx)
                                        );
                                        logger.info("Sampling[" + N_SAMPLES + "] snr=" + (s_vphi_mean / s_vphi_err) + " PHI "
                                                + " avg= " + s_vphi_mean + " vphi= " + vphi + " ratio: " + ((PI + s_vphi_mean) / (PI + vphi))
                                                + " stddev= " + s_vphi_err
                                        );
                                    }

                                    if (this.doNoise) {
                                        // Use the corresponding sample among vis, vis2, t3 on any baselines !
                                        final int nSample = visRndIdxRow[l];
                                        vamp = vamp_samples[nSample];
                                        vphi = vphi_samples[nSample];
                                        doNormVisDiff = ENABLE_DIFF_VIS;
                                    } else if (DO_USE_SAMPLED_MEAN) {
                                        vamp = s_vamp_mean;
                                        vphi = s_vphi_mean;
                                        doNormVisDiff = ENABLE_DIFF_VIS;
                                    }
                                    errAmp = s_vamp_err;
                                    errPhi = s_vphi_err;
                                }

                                if (useInstrumentBias && (ns != null)) {
                                    errPhi = ns.computeBiasedVisPhiError(vphi, errPhi);
                                    errAmp = Math.max(errAmp, errPhi);
                                }

                                // Set values:
                                visAmp[k][l] = vamp;
                                visAmpErr[k][l] = errAmp;

                                // toShort errPhi in degrees :
                                visPhi[k][l] = FastMath.toDegrees(vphi);
                                visPhiErr[k][l] = FastMath.toDegrees(errPhi);

                                // mark this value as valid only if observables are not NaN, error is valid and SNR is OK:
                                if (!doFlag && (Double.isNaN(visAmp[k][l]) || Double.isNaN(visAmpErr[k][l])
                                        || Double.isNaN(visPhi[k][l]) || Double.isNaN(visPhiErr[k][l]))) {
                                    doFlag = true;
                                }

                            } else {
                                // Waiting for explanations on every instrument processing to compute VisAmp/Phi:
                                // following values are considered as invalid :
                                visAmp[k][l] = Double.NaN;
                                visAmpErr[k][l] = Double.NaN;

                                visPhi[k][l] = Double.NaN;
                                visPhiErr[k][l] = Double.NaN;
                            }
                        }
                        flags[k][l] = doFlag;
                    }
                }

                // station indexes :
                staIndexes[k] = baseLineMapping.get(uvBL.getBaseLine());

                // increment j:
                j++;

                // fast interrupt :
                if (currentThread.isInterrupted()) {
                    return;
                }

            } // baselines
        } // HA

        /* Compute visAmp / visPhi as amber does */
        if (isAmber && this.hasModel && (ns != null)) {
            OIFitsAMBERService.amdlibFakeAmberDiffVis(vis, visComplex, this.visError, nWaveLengths, this.visRndDist);
            // TODO: generate noisy samples if doNoise
        } else if (doNormVisDiff) {

            /* Normalize differential visibilities to 1 */
            for (int k = 0, l; k < visAmp.length; k++) {
                vamp_sum = 0.0;

                for (l = 0; l < nWaveLengths; l++) {
                    vamp_sum += visAmp[k][l];
                }
                vamp_sum /= nWaveLengths; // mean

                for (l = 0; l < nWaveLengths; l++) {
                    visAmp[k][l] /= vamp_sum;
                    visAmpErr[k][l] /= vamp_sum;
                }
            }
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

        // generate squared correlated fluxes:
        final boolean useExtraVis2 = AsproConstants.INS_GRAVITY.equals(this.instrumentName)
                || this.instrumentName.startsWith(AsproConstants.INS_MATISSE)
                || this.instrumentName.startsWith(AsproConstants.INS_SPICA);

        // Update the data model before calling new OIVis():
        DataModel.setOiVis2ExtraSupport(useExtraVis2);

        // Get OI_VIS table :
        final OIVis vis = this.oiFitsFile.getOiVis()[0];
        final int nRows = vis.getNbRows();
        final int nWaveLengths = this.waveLengths.length;

        // Create OI_VIS2 table :
        final OIVis2 vis2 = new OIVis2(this.oiFitsFile, this.insNameKeyword, nRows);
        vis2.setArrName(this.arrNameKeyword);
        vis2.setDateObs(vis.getDateObs());

        // Columns :
        System.arraycopy(vis.getTargetId(), 0, vis2.getTargetId(), 0, nRows);
        System.arraycopy(vis.getTime(), 0, vis2.getTime(), 0, nRows);
        System.arraycopy(vis.getMJD(), 0, vis2.getMJD(), 0, nRows);
        System.arraycopy(vis.getIntTime(), 0, vis2.getIntTime(), 0, nRows);

        final double[][] vis2Data = vis2.getVis2Data();
        final double[][] vis2Err = vis2.getVis2Err();

        final double[][] vis2CorrSq = (useExtraVis2) ? vis2.getCorrSq() : null;
        final double[][] vis2CorrSqErr = (useExtraVis2) ? vis2.getCorrSqErr() : null;

        final double[][] vis2Phot = (useExtraVis2) ? vis2.getPhot() : null;
        final double[][] vis2PhotErr = (useExtraVis2) ? vis2.getPhotErr() : null;

        final boolean[][] flags = vis2.getFlag();

        final double[][] snrData = (DEBUG_SNR) ? new double[nWaveLengths][nRows] : null;

        final NoiseService ns = this.noiseService;

        // vars:
        double visRe, visIm, errCVis, bias;
        double v2, v2Err;
        double phot = Double.NaN, errPhot = Double.NaN;
        double sqCorr = Double.NaN, errSqCorr = Double.NaN;

        // distribution samples:
        double[] distRe = null, distIm = null;

        // try resampling:
        double re, im, sample, diff;
        double v2_sum, v2_sum_diff, v2_sum_diff_square;
        double s_v2_mean, s_v2_err, snr;

        final double[] v2_samples = new double[N_SAMPLES];
        int[] visRndIdxRow = null;
        boolean doFlag;

        // Iterate on observable UV points :
        for (int i = 0, j, k, l, n; i < this.nObsPoints; i++) {
            j = 0;

            // Iterate on baselines :
            for (final UVRangeBaseLineData uvBL : targetUVObservability) {
                k = nBaseLines * i + j;

                // if target has models, then complex visibility are computed :
                if (!this.hasModel) {
                    // Iterate on wave lengths :
                    for (l = 0; l < nWaveLengths; l++) {
                        vis2Data[k][l] = Double.NaN;
                        vis2Err[k][l] = Double.NaN;

                        if (useExtraVis2) {
                            vis2CorrSq[k][l] = Double.NaN;
                            vis2CorrSqErr[k][l] = Double.NaN;

                            vis2Phot[k][l] = Double.NaN;
                            vis2PhotErr[k][l] = Double.NaN;
                        }

                        // mark this value as invalid :
                        flags[k][l] = true;

                        if (DEBUG_SNR) {
                            snrData[l][k] = Double.NaN;
                        }
                    }
                } else {
                    if (ns != null) {
                        // Get the complex distribution for this row:
                        distRe = visRndDist[k].getSamples()[0];
                        distIm = visRndDist[k].getSamples()[1];

                        // Get proper index:
                        visRndIdxRow = this.visRndIdx[k];
                    }

                    // Iterate on wave lengths :
                    for (l = 0; l < nWaveLengths; l++) {
                        // pure complex visibility data :
                        visRe = this.visComplex[k][l].getReal();
                        visIm = this.visComplex[k][l].getImaginary();

                        // pure square visibility :
                        v2 = visRe * visRe + visIm * visIm;

                        doFlag = visSnrFlag[k][l];

                        if (ns == null) {
                            phot = Double.NaN;
                            errPhot = Double.NaN;
                            v2Err = Double.NaN;
                            snr = Double.NaN;
                        } else {

                            if (useExtraVis2) {
                                phot = ns.getNbPhotPhoto(i, l);
                                errPhot = ns.getErrorPhotPhoto(i, l);

                                // TODO: avoid recomputation twice (in cvis + vis2) ?
                                ns.computeVis2Error(i, l, v2);

                                sqCorr = ns.getSqCorrFlux(i, l);
                                errSqCorr = ns.getErrorSqCorrFlux(i, l);
                            }

                            if (doFlag) {
                                // use theoretical value & error if SNR < 3 !

                                // square visibility error :
                                v2Err = ns.computeVis2Error(i, l, v2);

                                if (this.doNoise) {
                                    // update nth sample:
                                    v2 += v2Err * distRe[visRndIdxRow[l]];
                                }

                            } else {
                                // Sampling complex visibilities:

                                // complex visibility error : visErrRe = visErrIm = visAmpErr or Complex.NaN :
                                errCVis = visError[k][l];

                                if (this.doNoise) {
                                    bias = ns.getVis2Bias(i, l);
                                    // Remove v2 bias:
                                    v2 -= bias;
                                } else {
                                    // do not debias V2 for theoretical values:
                                    bias = 0.0;
                                }

                                v2_sum = v2_sum_diff = v2_sum_diff_square = 0.0;

                                // bivariate distribution (complex normal):
                                for (n = 0; n < N_SAMPLES; n++) {
                                    // update nth sample:
                                    re = visRe + (errCVis * distRe[n]);
                                    im = visIm + (errCVis * distIm[n]);

// TODO: use sum(re), sum(im)                                    
                                    // compute debiased V2 = re^2 + im^2 - bias:
                                    sample = re * re + im * im - bias;
                                    v2_samples[n] = sample;

                                    // Compensated-summation variant for better numeric precision:
                                    v2_sum += sample;
                                    diff = sample - v2; // no bias (both removed)
                                    v2_sum_diff += diff;
                                    v2_sum_diff_square += diff * diff;
                                }

                                // mean(V2):
                                s_v2_mean = SAMPLING_FACTOR_MEAN * v2_sum;

                                // error(V2):
                                // note: this algorithm ensures correctness (stable) even if the mean used in diff is wrong !
                                s_v2_err = Math.sqrt(
                                        SAMPLING_FACTOR_VARIANCE * (v2_sum_diff_square - (SAMPLING_FACTOR_MEAN * (v2_sum_diff * v2_sum_diff)))
                                );

                                if (DEBUG) {
                                    v2 += bias; // remove bias correction
                                    // square visibility error :
                                    v2Err = ns.computeVis2Error(i, l, v2);

                                    // logger.info("Bias vs Error: bias = " + bias + " error = "+v2Err + "ratio: " + bias / v2Err);
                                    logger.info("Sampling[" + N_SAMPLES + "] snr=" + (s_v2_mean / s_v2_err) + " V2"
                                            + " (err(re,im)= " + errCVis + ")"
                                            + " avg= " + s_v2_mean + " V2= " + v2 + " ratio: " + (s_v2_mean / v2)
                                            + " stddev= " + s_v2_err + " err(V2)= " + v2Err + " ratio: " + (s_v2_err / v2Err)
                                    );
                                    v2 -= bias; // restore bias correction
                                }

                                if (this.doNoise) {
                                    // Use the corresponding sample:
                                    v2 = v2_samples[visRndIdxRow[l]];
                                } else if (DO_USE_SAMPLED_MEAN) {
                                    v2 = s_v2_mean;
                                }
                                v2Err = s_v2_err;
                            }

                            if (useInstrumentBias && (ns != null)) {
                                v2Err = ns.computeBiasedVis2Error(v2, v2Err);
                            }

                            if (DEBUG_SNR) {
                                snr = Math.abs(v2 / v2Err);
                            }
                        }

                        // Set values:
                        vis2Data[k][l] = v2;
                        vis2Err[k][l] = v2Err;

                        if (useExtraVis2) {
                            vis2CorrSq[k][l] = sqCorr;
                            vis2CorrSqErr[k][l] = errSqCorr;

                            vis2Phot[k][l] = phot;
                            vis2PhotErr[k][l] = errPhot;
                        }

                        // mark this value as valid only if observables are not NaN, error is valid and SNR is OK:
                        if (!doFlag && (Double.isNaN(vis2Data[k][l]) || Double.isNaN(vis2Err[k][l]))) {
                            doFlag = true;
                        }
                        flags[k][l] = doFlag;

                        if (DEBUG_SNR) {
                            snrData[l][k] = snr;
                        }
                    }
                }

                // increment j:
                j++;
            }
        }

        System.arraycopy(vis.getUCoord(), 0, vis2.getUCoord(), 0, nRows);
        System.arraycopy(vis.getVCoord(), 0, vis2.getVCoord(), 0, nRows);

        System.arraycopy(vis.getStaIndex(), 0, vis2.getStaIndex(), 0, nRows);

        this.oiFitsFile.addOiTable(vis2);

        if (DEBUG_SNR) {
            final double[][] snrWL = new double[3][nWaveLengths];

            // SNR stats per channel:
            int nValid = 0;

            for (int l = 0; l < nWaveLengths; l++) {
                snrWL[0][l] = StatUtils.mean(snrData[l]);
                snrWL[1][l] = StatUtils.min(snrData[l]);
                snrWL[2][l] = StatUtils.max(snrData[l]);

                logger.info("SNR[" + NumberUtils.trimTo3Digits(waveLengths[l] / AsproConstants.MICRO_METER) + "]: average = " + NumberUtils.trimTo3Digits(snrWL[0][l])
                        + " min = " + NumberUtils.trimTo3Digits(snrWL[1][l])
                        + " max = " + NumberUtils.trimTo3Digits(snrWL[2][l])
                );

                // check mean(SNR):
                if (snrWL[0][l] < SNR_THRESHOLD) {
                    snrWL[0][l] = Double.NaN;
                } else {
                    nValid++;
                }
            }
            if (nValid != 0) {
                logger.info("SNR: average = " + NumberUtils.trimTo3Digits(StatUtils.mean(snrWL[0]))
                        + " min = " + NumberUtils.trimTo3Digits(StatUtils.min(snrWL[1]))
                        + " max = " + NumberUtils.trimTo3Digits(StatUtils.max(snrWL[2]))
                        + " - [" + nValid + " / " + nWaveLengths + "] channels with SNR >= " + SNR_THRESHOLD);
            }

            // TODO: add information in status log ...
        }
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
        final short[][] orderedbaseLineIndexes = baseLineMapping.values().toArray(new short[baseLineMapping.size()][2]);

        for (int[] idx : iTriplets) {
            triplets.add(Triplet.create(idx, this.beams, this.stationMapping, orderedbaseLineIndexes));
        }

        if (logger.isDebugEnabled()) {
            logger.debug("triplets: {}", triplets);
        }

        // Get OI_VIS table :
        final OIVis vis = this.oiFitsFile.getOiVis()[0];

        // Create OI_T3 table :
        final OIT3 t3 = new OIT3(this.oiFitsFile, this.insNameKeyword, this.nObsPoints * nTriplets);
        t3.setArrName(this.arrNameKeyword);
        t3.setDateObs(vis.getDateObs());

        // OI_VIS Columns :
        final double[] visTimes = vis.getTime();
        final double[] visMjds = vis.getMJD();

        final double[] visUCoords = vis.getUCoord();
        final double[] visVCoords = vis.getVCoord();

        final short[][] visStaIndexes = vis.getStaIndex();

        // OI_T3 Columns :
        final short[] t3TargetIds = t3.getTargetId();
        final double[] t3Times = t3.getTime();
        final double[] t3Mjds = t3.getMJD();
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

        final NoiseService ns = this.noiseService;

        // The following code use some hypothesis on the OI_VIS table as defined in createOIVis()
        // 1 - the number of rows per HA point corresponds to the number of baselines.
        // 2 - OI_VIS rows have the same ordering than the list of baselines per HA points.
        // vars :
        Complex cvis12, cvis23, cvis13;
        Complex[] visData12, visData23, visData13;
        double[] visErr12, visErr23, visErr13;
        ComplexDistribution dist12, dist23, dist13;
        boolean[] visSnrFlag12, visSnrFlag23, visSnrFlag13;
        double u12, v12, u23, v23;
        double t3amp, t3phi, errAmp, errPhi;

        // temporary mutable complex:
        int[] relPos;
        int pos;

        // resampling:
        double re_sum, im_sum, cos_phi, sin_phi;
        double t3amp_sum, t3amp_sum_diff, t3amp_sum_diff_square;
        double s_t3amp_mean, s_t3amp_err;
        double t3phi_sum_diff, t3phi_sum_diff_square;
        double s_t3phi_mean, s_t3phi_err;

        final double[] re_samples = new double[N_SAMPLES];
        final double[] im_samples = new double[N_SAMPLES];
        final double[] t3amp_samples = new double[N_SAMPLES];
        final double[] t3phi_samples = new double[N_SAMPLES];
        int[] visRndIdxRow;
        boolean doFlag;

        double[] distRe_12 = null, distIm_12 = null;
        double[] distRe_23 = null, distIm_23 = null;
        double[] distRe_31 = null, distIm_31 = null;

        double visRe12, visIm12, visErrCplx12;
        double visRe23, visIm23, visErrCplx23;
        double visRe31, visIm31, visErrCplx31;

        double t3Re, t3Im, sample, diff;
        double sRe12, sIm12, sRe23, sIm23, sRe31, sIm31;

        final int nWaveLengths = this.waveLengths.length;

        // Iterate on observable UV points :
        for (int i = 0, j, k, l, vp, n; i < this.nObsPoints; i++) {

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

                /*
                Note: use complex visibility error WITHOUT photometric error (closure phase is not affected by photometry)
                 */
                // pure complex visibility data :
                visData12 = (this.hasModel) ? this.visComplex[vp + pos] : null;
                visErr12 = (this.hasModel) ? this.visErrorNoPhot[vp + pos] : null;
                dist12 = this.visRndDist[vp + pos];
                visSnrFlag12 = this.visSnrFlag[vp + pos];
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
                visErr23 = (this.hasModel) ? this.visErrorNoPhot[vp + pos] : null;
                dist23 = this.visRndDist[vp + pos];
                visSnrFlag23 = this.visSnrFlag[vp + pos];
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
                visErr13 = (this.hasModel) ? this.visErrorNoPhot[vp + pos] : null;
                dist13 = this.visRndDist[vp + pos];
                visSnrFlag13 = this.visSnrFlag[vp + pos];

                // Get proper index:
                // note: visRndIdx[12] = visRndIdx[23] = visRndIdx[13] by construction
                visRndIdxRow = this.visRndIdx[vp + pos];

                // if target has models, then complex visibility are computed :
                if (!this.hasModel) {

                    // Iterate on wave lengths :
                    for (l = 0; l < nWaveLengths; l++) {
                        t3Amp[k][l] = Double.NaN;
                        t3AmpErr[k][l] = Double.NaN;

                        t3Phi[k][l] = Double.NaN;
                        t3PhiErr[k][l] = Double.NaN;

                        // mark this value as invalid :
                        flags[k][l] = true;
                    }

                } else {
                    if (ns != null) {
                        // Get the complex distributions for the 3 rows:
                        distRe_12 = dist12.getSamples()[0];
                        distIm_12 = dist12.getSamples()[1];

                        distRe_23 = dist23.getSamples()[0];
                        distIm_23 = dist23.getSamples()[1];

                        distRe_31 = dist13.getSamples()[0];
                        distIm_31 = dist13.getSamples()[1];

                        if (dist12 == dist23 || dist23 == dist13 || dist12 == dist13) {
                            logger.warn("Bad distribution associations !");
                        }
                    }

                    // Iterate on wave lengths :
                    for (l = 0; l < nWaveLengths; l++) {

                        // baseline AB = 12 :
                        cvis12 = visData12[l];
                        visRe12 = cvis12.getReal();
                        visIm12 = cvis12.getImaginary();

                        // baseline BC = 23
                        cvis23 = visData23[l];
                        visRe23 = cvis23.getReal();
                        visIm23 = cvis23.getImaginary();

                        // baseline AC = 13 => conjugate for 31 (im = -im)
                        cvis13 = visData13[l];
                        visRe31 = cvis13.getReal();
                        // conjugate for 31 (im = -im)
                        visIm31 = -cvis13.getImaginary();

                        // Compute RE/IM bispectrum with C12*C23*~C13 :
                        t3Re = visRe12 * visRe23 * visRe31 - visRe12 * visIm23 * visIm31 - visIm12 * visRe23 * visIm31 - visIm12 * visIm23 * visRe31;
                        t3Im = visRe12 * visRe23 * visIm31 + visRe12 * visIm23 * visRe31 + visIm12 * visRe23 * visRe31 - visIm12 * visIm23 * visIm31;

                        // pure amplitude :
                        t3amp = Math.sqrt(t3Re * t3Re + t3Im * t3Im);

                        // pure phase [-PI;PI] in degrees :
                        t3phi = (t3Im != 0.0) ? FastMath.atan2(t3Im, t3Re) : 0.0;

                        doFlag = visSnrFlag12[l] || visSnrFlag23[l] || visSnrFlag13[l];

                        if (ns == null) {
                            errAmp = errPhi = Double.NaN;
                        } else {
                            if (doFlag) {
                                // use theoretical value & error if SNR < 3 !                            

                                // phase closure error (rad) :
                                errPhi = ns.computeT3PhiError(i, l, cvis12.abs(), cvis23.abs(), cvis13.abs()); // abs(c13) = abs(c31)

                                // amplitude error t3AmpErr = t3Amp * t3PhiErr :
                                errAmp = t3amp * errPhi;

                                if (this.doNoise) {
                                    // use the first distribution (sigma = 1):
                                    final int nSample = visRndIdxRow[l];

                                    // TODO: add noise in a proper manner:
                                    // add gaussian noise with sigma = errAmp :
                                    t3amp += errAmp * distRe_12[nSample];
                                    // add gaussian noise with sigma = errPhi :
                                    t3phi += errPhi * distIm_12[nSample];
                                }

                            } else {
                                // Sampling complex visibilities:
                                // complex visibility errors : visErrRe = visErrIm = visAmpErr or Complex.NaN :
                                visErrCplx12 = visErr12[l];
                                visErrCplx23 = visErr23[l];
                                visErrCplx31 = visErr13[l];

                                t3amp_sum = t3amp_sum_diff = t3amp_sum_diff_square = 0.0;
                                re_sum = im_sum = 0.0;

                                // bivariate distribution (complex normal):
                                for (n = 0; n < N_SAMPLES; n++) {
                                    // update nth sample:

                                    // baseline AB = 12 :
                                    sRe12 = visRe12 + (visErrCplx12 * distRe_12[n]);
                                    sIm12 = visIm12 + (visErrCplx12 * distIm_12[n]);

                                    // baseline BC = 23
                                    sRe23 = visRe23 + (visErrCplx23 * distRe_23[n]);
                                    sIm23 = visIm23 + (visErrCplx23 * distIm_23[n]);

                                    // baseline CA = 31
                                    sRe31 = visRe31 + (visErrCplx31 * distRe_31[n]);
                                    // Use conjuguate on distribution too:
                                    sIm31 = visIm31 - (visErrCplx31 * distIm_31[n]);

                                    // Compute RE/IM bispectrum with C12*C23*~C13 :
                                    // see ComplexUtils.bispectrum(vis12, vis23, vis31, t3Data);
                                    t3Re = sRe12 * sRe23 * sRe31 - sRe12 * sIm23 * sIm31 - sIm12 * sRe23 * sIm31 - sIm12 * sIm23 * sRe31;
                                    t3Im = sRe12 * sRe23 * sIm31 + sRe12 * sIm23 * sRe31 + sIm12 * sRe23 * sRe31 - sIm12 * sIm23 * sIm31;

                                    // average complex value:
                                    re_samples[n] = t3Re;
                                    im_samples[n] = t3Im;

                                    re_sum += t3Re;
                                    im_sum += t3Im;

                                    // phase in [-PI; PI]:
                                    sample = (t3Im != 0.0) ? FastMath.atan2(t3Im, t3Re) : 0.0;
                                    t3phi_samples[n] = sample;
                                }

                                // mean(T3phi):
                                s_t3amp_mean = Math.sqrt(re_sum * re_sum + im_sum * im_sum);
                                s_t3phi_mean = (im_sum != 0.0) ? FastMath.atan2(im_sum, re_sum) : 0.0;

                                // rotate by -phi:
                                cos_phi = re_sum / s_t3amp_mean;
                                sin_phi = im_sum / s_t3amp_mean;

                                t3phi_sum_diff = t3phi_sum_diff_square = 0.0;

                                // compute angle variance and amplitude:
                                for (n = 0; n < N_SAMPLES; n++) {
                                    // phase in [-PI; PI]:
                                    // Compensated-summation variant for better numeric precision:
                                    // check if diff is [-PI; PI]:
                                    diff = distanceAngle(t3phi_samples[n], s_t3phi_mean);
                                    t3phi_sum_diff += diff;
                                    t3phi_sum_diff_square += diff * diff;

                                    // Correct amplitude by estimated phase:
                                    // Amp = Re { C * phasor(-phi) }
                                    sample = re_samples[n] * cos_phi + im_samples[n] * sin_phi; // -phi => + imaginary part in complex mult
                                    t3amp_samples[n] = sample;

                                    // Compensated-summation variant for better numeric precision:
                                    t3amp_sum += sample;
                                    diff = sample - t3amp;
                                    t3amp_sum_diff += diff;
                                    t3amp_sum_diff_square += diff * diff;
                                }

                                // mean(T3amp):
                                s_t3amp_mean = SAMPLING_FACTOR_MEAN * t3amp_sum;

                                // error(t3amp):
                                // note: this algorithm ensures correctness (stable) even if the mean used in diff is wrong !
                                s_t3amp_err = Math.sqrt(
                                        SAMPLING_FACTOR_VARIANCE * (t3amp_sum_diff_square - (SAMPLING_FACTOR_MEAN * (t3amp_sum_diff * t3amp_sum_diff)))
                                );

                                // error(t3phi):
                                // note: this algorithm ensures correctness (stable) even if the mean used in diff is wrong !
                                s_t3phi_err = Math.sqrt(
                                        SAMPLING_FACTOR_VARIANCE * (t3phi_sum_diff_square - (SAMPLING_FACTOR_MEAN * (t3phi_sum_diff * t3phi_sum_diff)))
                                );

                                if (DEBUG) {
                                    // phase closure error (rad) :
                                    errPhi = ns.computeT3PhiError(i, l, cvis12.abs(), cvis23.abs(), cvis13.abs()); // abs(c13) = abs(c31)

                                    // amplitude error t3AmpErr = t3Amp * t3PhiErr :
                                    errAmp = t3amp * errPhi;

                                    logger.info("Sampling[" + N_SAMPLES + "] snr=" + Math.abs(s_t3amp_mean / s_t3amp_err) + " (low: " + doFlag + ") AMP "
                                            + " avg= " + s_t3amp_mean + " T3amp= " + t3amp + " ratio: " + (s_t3amp_mean / t3amp)
                                            + " stddev= " + s_t3amp_err + " errAmp= " + errAmp + " ratio: " + (s_t3amp_err / errAmp)
                                    );
                                    logger.info("Sampling[" + N_SAMPLES + "] snr=" + Math.abs(s_t3phi_mean / s_t3phi_err) + " (low: " + doFlag + ") PHI "
                                            + " avg= " + s_t3phi_mean + " T3phi= " + t3phi + " diff: " + distanceAngle(s_t3phi_mean, t3phi)
                                            + " stddev= " + s_t3phi_err + " errPhi= " + errPhi + " ratio: " + (s_t3phi_err / errPhi)
                                    );
                                }

                                if (this.doNoise) {
                                    // Use the corresponding sample among vis, vis2, t3 on any baselines !
                                    final int nSample = visRndIdxRow[l];
                                    t3amp = t3amp_samples[nSample];
                                    t3phi = t3phi_samples[nSample];
                                } else if (DO_USE_SAMPLED_MEAN) {
                                    t3amp = s_t3amp_mean;
                                    t3phi = s_t3phi_mean;
                                }
                                errAmp = s_t3amp_err;
                                errPhi = s_t3phi_err;
                            }

                            if (useInstrumentBias && (ns != null)) {
                                errPhi = ns.computeBiasedT3PhiError(t3phi, errPhi);
                                errAmp = Math.max(errAmp, errPhi);
                            }
                        }

                        // Set values:
                        t3Amp[k][l] = t3amp;
                        t3AmpErr[k][l] = errAmp;

                        // toShort errPhi in degrees :
                        t3Phi[k][l] = FastMath.toDegrees(t3phi);
                        t3PhiErr[k][l] = FastMath.toDegrees(errPhi);

                        // mark this value as valid only if observables are not NaN, error is valid and SNR is OK:
                        if (!doFlag && (Double.isNaN(t3Amp[k][l]) || Double.isNaN(t3AmpErr[k][l])
                                || Double.isNaN(t3Phi[k][l]) || Double.isNaN(t3PhiErr[k][l]))) {
                            doFlag = true;
                        }
                        flags[k][l] = doFlag;
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
     * Return true to use instrument bias; false to compute only theoretical error
     * @return true to use instrument bias; false to compute only theoretical error
     */
    public boolean isUseInstrumentBias() {
        return useInstrumentBias;
    }

}
