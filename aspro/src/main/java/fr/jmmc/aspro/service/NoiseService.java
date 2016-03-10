/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.oi.AdaptiveOptics;
import fr.jmmc.aspro.model.oi.AtmosphereQuality;
import fr.jmmc.aspro.model.oi.FocalInstrument;
import fr.jmmc.aspro.model.oi.FringeTracker;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.model.oi.Telescope;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.observability.TargetPointInfo;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.FocalInstrumentSetup;
import fr.jmmc.aspro.model.oi.ObservationSequence;
import fr.jmmc.aspro.model.oi.SpectralSetup;
import fr.jmmc.aspro.model.oi.SpectralSetupColumn;
import fr.jmmc.aspro.model.oi.SpectralSetupQuantity;
import fr.jmmc.aspro.model.util.AtmosphereQualityUtils;
import fr.jmmc.aspro.model.util.SpectralBandUtils;
import fr.jmmc.jmal.Band;
import fr.jmmc.jmal.model.VisNoiseService;
import fr.jmmc.jmcs.util.NumberUtils;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.List;
import net.jafama.FastMath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class performs the noise modelling of visibility data (error and noise)
 * from the current observation
 *
 * Note : this code is inspired by the Aspro/tasks/lib/noise_lib.f90
 *
 * @author bourgesl
 */
public final class NoiseService implements VisNoiseService {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(NoiseService.class.getName());
    /** Planck's constant in standard units (6.6262e-34) */
    public final static double H_PLANCK = 6.62606896e-34d;
    /** Speed of light (2.99792458e8) */
    public final static double C_LIGHT = 2.99792458e8d;
    
    /** flag to use the new numeric approach = statistical distributions */
    public final static boolean USE_DISTRIB_APPROACH = true;
    
    /** enable bound checks on iPoint / iChannel */
    private final static boolean DO_CHECKS = false;

    /** maximum error on Vis to avoid excessively large errors */
    private final static double MAX_ERR_V = 3.0;
    /** maximum error on Vis2 to avoid excessively large errors */
    private final static double MAX_ERR_V2 = MAX_ERR_V * MAX_ERR_V;

    /* members */
    /** instrument name */
    private String instrumentName = null;

    /* interferometer parameters */
    /** telescope in use */
    private Telescope telescope = null;
    /** Telescope Diameter (m) */
    private double telDiam = Double.NaN;
    /** Number of telescopes interfering as double */
    private double nbTel = Double.NaN;

    /* adaptive optics parameters */
    /** seeing (arc sec) */
    private double seeing = Double.NaN;
    /** Number of Actuators of AO */
    private int nbOfActuators = 0;
    /** AO band */
    private SpectralBand aoBand;

    /* instrument parameters */
 /* fixed values (spectrally independent) */
    /** Total Acquisition time per observed u,v point (s) */
    private double totalObsTime = Double.NaN;
    /** Detector individual integration time (s) */
    private double dit = Double.NaN;
    /** Detector readout noise */
    private double ron = Double.NaN;
    /** Detector is non-linear above (to avoid saturation/n-on-linearity) */
    private double detectorSaturation = Double.NaN;
    /** Detector quantum efficiency (optional ie 1.0 by default) */
    private double quantumEfficiency = Double.NaN;

    /** flag to use the photometry */
    private boolean usePhotometry = false;
    /** flag to use the strehl correction */
    private boolean useStrehlCorrection = true;

    /* varying values (spectrally dependent) */
    /** instrument band */
    private Band insBand;
    /** instrument band corresponding to target mags */
    private SpectralBand insTargetBand;

    /** fraction of flux going into the interferometric channel */
    private double fracFluxInInterferometry = Double.NaN;
    /** fraction of flux going into the photometric channel */
    private double fracFluxInPhotometry = Double.NaN;

    /** number of pixels to code all fringes together (interferometric channel) */
    private double nbPixInterf = Double.NaN;
    /** number of pixels to code each photometric channel */
    private double nbPixPhoto = Double.NaN;
    /** ratio photometry exposures per photometric channel (chopping) */
    private double ratioPhotoPerBeam = Double.NaN;

    /** (W) Transmission of interferometer+instrument at observed wavelength */
    private double[] transmission = null;
    /** (W) Instrumental Visibility [0.0-1.0] */
    private double[] instrumentalVisibility = null;
    /** (W) Number of thermal photon (background) per beam per second (no strehl, no QE) */
    private double[] nbPhotThermal = null;

    /* bias */
    /** true to use instrument bias; false to compute only theoretical error */
    private final boolean useInstrumentBias;
    /** Typical Vis2 Bias (absolute) */
    private double instrumentalVis2Bias = 0d;
    /** Typical Phase/Phase Closure Bias (rad) */
    private double instrumentalPhaseBias = 0d;
    /** flag to use the Vis2 Calibration Bias */
    private boolean useVis2CalibrationBias = false;
    /** Vis2 Calibration Bias (proportional to square visibility) */
    private double instrumentVis2CalibrationBias = 0d;

    /* instrument mode parameters */
    /** number of spectral channels */
    private final int nSpectralChannels;
    /** index of central spectral channel */
    private final int iMidChannel;
    /** spectral channel central wavelength */
    private final double[] waveLengths;
    /** spectral channel widths */
    private final double[] waveBands;

    /* fringe tracker parameters */
    /** Fringe Tracker is Present */
    private boolean fringeTrackerPresent = false;
    /** Fringe Tracking Mode i.e. not group tracking (FINITO) */
    private boolean fringeTrackingMode = false;
    /** Fringe Tracker (multiplicative) Instrumental Visibility */
    private double fringeTrackerInstrumentalVisibility = Double.NaN;
    /** Fringe Tracker Usage limit (mag) */
    private double fringeTrackerLimit = Double.NaN;
    /** Fringe Tracker Max Integration Time (s) */
    private double fringeTrackerMaxIntTime = Double.NaN;
    /** FT band */
    private SpectralBand ftBand;

    /* object parameters */
    /** Magnitude in Observing Band (see lambda) (mag) */
    /* TODO: use band range (previous / next) to interpolate flux (SED like) ? */
    private double objectMag = Double.NaN;
    /** Magnitude in Fringe Tracker's Band of FT ref star (mag) */
    private double fringeTrackerMag = Double.NaN;
    /** Magnitude in V Band (for AO performances / for strehl) (mag) */
    private double adaptiveOpticsMag = Double.NaN;
    /** target information for each uv point couples */
    private final TargetPointInfo[] targetPointInfos;
    /** number of uv points */
    private final int nPoints;
    /** index of central uv point */
    private final int iMidPoint;

    /* internal */
    /** container for warning messages */
    private final WarningContainer warningContainer;
    /** flag to indicate that a parameter is invalid in order the code to return errors as NaN values */
    private boolean invalidParameters = false;

    /** total instrumental visibility (with FT if any) */
    private double[] vinst;

    /* cached intermediate constants */
    /** error correction = 1 / SQRT(total frame) */
    private double totFrameCorrection;
    /** noise computation parameters per uv point */
    private NoiseWParams[] params = null;
    /* varying values (spectrally dependent) */
    /** (W) number of thermal photons per telescope */
    private double[] nbPhotTherm = null;

    /**
     * Protected constructor
     * @param observation observation settings used (read-only copy of the modifiable observation)
     * @param target target to use
     * @param targetPointInfos target information for each uv point couples
     * @param useInstrumentBias true to use instrument bias; false to compute only theoretical error
     * @param warningContainer container for warning messages
     * @param waveLengths concrete wavelength values (spectral channel central value) in meters
     * @param waveBands concrete spectral channel bandwidths in meters
     */
    protected NoiseService(final ObservationSetting observation,
                           final Target target,
                           final TargetPointInfo[] targetPointInfos,
                           final boolean useInstrumentBias,
                           final WarningContainer warningContainer,
                           final double[] waveLengths,
                           final double[] waveBands) {

        this.useInstrumentBias = useInstrumentBias;
        this.warningContainer = warningContainer;

        // Get spectral channels:
        this.nSpectralChannels = waveLengths.length;
        this.iMidChannel = this.nSpectralChannels / 2;
        this.waveLengths = waveLengths;
        this.waveBands = waveBands;

        this.targetPointInfos = targetPointInfos;
        this.nPoints = this.targetPointInfos.length;
        this.iMidPoint = this.nPoints / 2;

        if (logger.isDebugEnabled()) {
            logger.debug("spectralChannels              : {}", nSpectralChannels);
            logger.debug("nMidChannel                   : {}", iMidChannel);
            logger.debug("waveLengths                   : {}", Arrays.toString(waveLengths));
            logger.debug("waveBands                     : {}", Arrays.toString(waveBands));
        }

        // extract parameters in observation and configuration :
        prepareInterferometer(observation);
        prepareInstrument(observation);
        prepareFringeTracker(observation, target);
        prepareTarget(target);
        initParameters();
    }

    /**
     * Prepare interferometer and AO parameters (related to telescopes so to each configuration)
     * @param observation observation settings
     */
    private void prepareInterferometer(final ObservationSetting observation) {

        final List<Station> stations = observation.getInstrumentConfiguration().getStationList();

        this.nbTel = stations.size();

        // All telescopes have the same diameter for a given baseline :
        this.telescope = stations.get(0).getTelescope();

        this.telDiam = telescope.getDiameter();

        // AO :
        final AdaptiveOptics ao = telescope.getAdaptiveOptics();
        if (ao != null) {
            this.aoBand = ao.getBand();
            this.nbOfActuators = ao.getNumberActuators();
        } else {
            // by default: compute strehl ratio on V band with only 1 actuator ?
            this.aoBand = SpectralBand.V;
            this.nbOfActuators = 1;
        }
        
        // TODO: add AO mag limits like FT
        // V < 15 for UTs (MACAO)
        // V < 11 for ATs (STRAP)

        // Seeing :
        final AtmosphereQuality atmQual = observation.getWhen().getAtmosphereQuality();
        if (atmQual != null) {
            this.seeing = AtmosphereQualityUtils.getSeeing(atmQual);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("nbTel                         : {}", nbTel);
            logger.debug("telDiam                       : {}", telDiam);
            logger.debug("aoBand                        : {}", aoBand);
            logger.debug("nbOfActuators                 : {}", nbOfActuators);
            logger.debug("seeing                        : {}", seeing);
        }

        // TODO: get atm spectra here ...
    }

    /**
     * Prepare instrument and mode parameters
     * @param observation observation settings
     */
    private void prepareInstrument(final ObservationSetting observation) {

        if (observation.getInstrumentConfiguration().getAcquisitionTime() != null) {
            this.totalObsTime = observation.getInstrumentConfiguration().getAcquisitionTime().doubleValue();
        }

        final FocalInstrument instrument = observation.getInstrumentConfiguration().getInstrumentConfiguration().getFocalInstrument();

        final FocalInstrumentSetup insSetup = observation.getInstrumentConfiguration().getFocalInstrumentMode().getSetupRef();

        // use alias or real instrument name:
        this.instrumentName = instrument.getAliasOrName();

        this.dit = insSetup.getDit();
        this.ron = insSetup.getRon();
        this.detectorSaturation = insSetup.getDetectorSaturation();
        this.quantumEfficiency = insSetup.getQuantumEfficiency();

        // fractions:
        this.fracFluxInInterferometry = insSetup.getFracFluxInInterferometry();
        this.fracFluxInPhotometry = insSetup.getFracFluxInPhotometry();

        this.nbPixInterf = insSetup.getNbPixInterferometry();
        this.nbPixPhoto = insSetup.getNbPixPhotometry(); // can be 0

        this.useStrehlCorrection = insSetup.isUseStrehlCorrection();
        this.usePhotometry = (fracFluxInPhotometry > 0.0 && nbPixPhoto > 0.0);

        // Use Sequence to get both time & beam ratios:
        final ObservationSequence sequence = insSetup.getSequence();

        this.ratioPhotoPerBeam = sequence.getRatioPhotoPerBeam();

        final double ratioTime = sequence.getRatioTime();
        final double ratioDeadTime = sequence.getRatioDeadTime();
        
        final double effectiveFrameTime = ratioTime * this.dit + ratioDeadTime * insSetup.getDeadtimeExposure();
        // + insSetup.getDeadtimeSequence(); // TODO: chopping switch is 2 times per SECOND not per sequence !

        final double ratioTimeInterfero = this.dit / effectiveFrameTime;

        if (logger.isDebugEnabled()) {
            logger.debug("ratioPhotoPerBeam: " + ratioPhotoPerBeam);
            logger.debug("ratioTime: " + ratioTime);
            logger.debug("ratioDeadTime: " + ratioDeadTime);
            logger.debug("effectiveFrameTime: " + effectiveFrameTime);
            logger.debug("efficiency (%): " + (100.0 * ratioTimeInterfero));
            logger.debug("totalObsTime                  : {}", totalObsTime);
            logger.debug("totalOBTime                   : {}", totalObsTime / ratioTimeInterfero);
        }

        if (this.useInstrumentBias) {
            /* Convert Phase bias to radians */
            this.instrumentalPhaseBias = FastMath.toRadians(insSetup.getInstrumentPhaseBias());

            /* Get Vis bias (percents) */
            final double instrumentalVisBias = getPercents(insSetup.getInstrumentVisibilityBias());

            final Double vis2CalibrationBias = insSetup.getInstrumentVis2CalibrationBias();
            if (vis2CalibrationBias == null) {
                this.useVis2CalibrationBias = false;
                this.instrumentVis2CalibrationBias = 0d;

                /* note: instrumentalVis2Bias = instrumentalVisBias (few percents) as there is no specific calibration bias */
                /* Correct Vis2 bias = 1/4 * visBias (peak to peak ~ 4 sigma) */
                this.instrumentalVis2Bias = 0.25 * instrumentalVisBias;
            } else {
                this.useVis2CalibrationBias = true;

                /* Get Vis2 calibration bias (percents) (peak to peak ~ 4 sigma) */
                this.instrumentVis2CalibrationBias = 0.25 * getPercents(vis2CalibrationBias.doubleValue());

                /* Correct Vis2 bias = 1/4 * visBias^2 (peak to peak ~ 4 sigma) */
                this.instrumentalVis2Bias = 0.25 * instrumentalVisBias * instrumentalVisBias;
            }
        } else {
            // disable instrumental bias:
            this.useVis2CalibrationBias = false;
        }

        final FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();
        if (insMode == null) {
            throw new IllegalStateException("The instrumentMode is empty !");
        }

        final SpectralSetup table = insMode.getTable();
        int firstIdx = -1;
        int lastIdx = -1;

        if (table != null) {
            // check WLen range:-
            final double[] lambda = table.getAndScaleColumn(SpectralSetupQuantity.LAMBDA, 1e-6);

            final double lambdaMin = this.waveLengths[0];
            final double lambdaMax = this.waveLengths[nSpectralChannels - 1];

            for (int i = 0; i < lambda.length; i++) {
                if (Math.abs(lambda[i] - lambdaMin) < 1e-15) {
                    firstIdx = i;
                    break;
                }
            }

            for (int i = lambda.length - 1; i >= 0; i--) {
                if (Math.abs(lambda[i] - lambdaMax) < 1e-15) {
                    lastIdx = i + 1; // exclusive
                    break;
                }
            }

            if (firstIdx == -1 || lastIdx == -1) {
                throw new IllegalStateException("Invalid range within spectral table !");
            }
            // check the number of channels is consistent:
            if ((lastIdx - firstIdx) != nSpectralChannels) {
                throw new IllegalStateException("Inconsistent channel count: " + (lastIdx - firstIdx) + " expected : " + nSpectralChannels + " !");
            }

            // Get table data:
            SpectralSetupColumn col;
            // transmission:
            col = table.getColumn(SpectralSetupQuantity.TRANSMISSION, telescope);
            if (col == null) {
                col = table.getColumn(SpectralSetupQuantity.TRANSMISSION);
            }
            if (col != null) {
                this.transmission = Arrays.copyOfRange(col.getValues(), firstIdx, lastIdx);
            }
            // visibility:
            col = table.getColumn(SpectralSetupQuantity.VISIBILITY, telescope);
            if (col == null) {
                col = table.getColumn(SpectralSetupQuantity.VISIBILITY);
            }
            if (col != null) {
                this.instrumentalVisibility = Arrays.copyOfRange(col.getValues(), firstIdx, lastIdx);
            }
            
            // nb photon thermal per beam per second (background):
            col = table.getColumn(SpectralSetupQuantity.NB_PHOTON_THERMAL, telescope);
            if (col == null) {
                col = table.getColumn(SpectralSetupQuantity.NB_PHOTON_THERMAL);
            }
            if (col != null) {
                this.nbPhotThermal = Arrays.copyOfRange(col.getValues(), firstIdx, lastIdx);
            }
        }

        if (this.transmission == null) {
            this.transmission = new double[nSpectralChannels];
            Arrays.fill(this.transmission, insSetup.getTransmission());
        }
        if (this.instrumentalVisibility == null) {
            this.instrumentalVisibility = new double[nSpectralChannels];
            Arrays.fill(this.instrumentalVisibility, insSetup.getInstrumentVisibility());
        }

        if (logger.isDebugEnabled()) {
            logger.debug("instrumentName                : {}", instrumentName);
            logger.debug("instrumentSetup               : {}", insSetup.getName());
            logger.debug("totalObsTime                  : {}", totalObsTime);
            logger.debug("dit                           : {}", dit);
            logger.debug("ron                           : {}", ron);
            logger.debug("detectorSaturation            : {}", detectorSaturation);
            logger.debug("quantumEfficiency             : {}", quantumEfficiency);
            logger.debug("fracFluxInInterferometry      : {}", fracFluxInInterferometry);
            logger.debug("fracFluxInPhotometry          : {}", fracFluxInPhotometry);
            logger.debug("nbPixInterf                   : {}", nbPixInterf);
            logger.debug("nbPixPhoto                    : {}", nbPixPhoto);
            logger.debug("usePhotometry                 : {}", usePhotometry);
            logger.debug("useStrehlCorrection           : {}", useStrehlCorrection);
            logger.debug("transmission                  : {}", Arrays.toString(transmission));
            logger.debug("instrumentalVisibility        : {}", Arrays.toString(instrumentalVisibility));
            logger.debug("nbPhotThermal                 : {}", Arrays.toString(nbPhotThermal));
            logger.debug("useInstrumentBias             : {}", useInstrumentBias);
            logger.debug("instrumentalVisibilityBias    : {}", instrumentalVis2Bias);
            logger.debug("instrumentalPhaseBias         : {}", instrumentalPhaseBias);
            logger.debug("useVis2CalibrationBias        : {}", useVis2CalibrationBias);
            logger.debug("instrumentVis2CalibrationBias : {}", instrumentVis2CalibrationBias);
        }
    }

    /**
     * Prepare fringe tracker parameters
     * @param observation observation settings
     * @param target target to use
     */
    private void prepareFringeTracker(final ObservationSetting observation, final Target target) {
        final TargetConfiguration targetConf = target.getConfiguration();

        if (targetConf != null && targetConf.getFringeTrackerMode() != null) {
            final FocalInstrument instrument = observation.getInstrumentConfiguration().getInstrumentConfiguration().getFocalInstrument();

            final FringeTracker ft = instrument.getFringeTracker();
            if (ft != null) {
                this.fringeTrackerPresent = true;
                // TODO: handle FT modes properly: GroupTrack is hard coded !
                this.fringeTrackingMode = !"GroupTrack".equalsIgnoreCase(targetConf.getFringeTrackerMode());
                this.fringeTrackerInstrumentalVisibility = ft.getInstrumentVisibility();
                this.fringeTrackerLimit = ft.getMagLimit();
                this.fringeTrackerMaxIntTime = ft.getMaxIntegration();
                this.ftBand = ft.getBand();
            }
        }

        if (logger.isDebugEnabled()) {
            logger.debug("fringeTrackerPresent          : {}", fringeTrackerPresent);
        }
        if (fringeTrackerPresent) {
            if (logger.isDebugEnabled()) {
                logger.debug("fringeTrackingMode            : {}", fringeTrackingMode);
                logger.debug("fringeTrackerInstrumentalVisibility : {}", fringeTrackerInstrumentalVisibility);
                logger.debug("fringeTrackerLimit            : {}", fringeTrackerLimit);
                logger.debug("fringeTrackerMaxIntTime       : {}", fringeTrackerMaxIntTime);
                logger.debug("ftBand                        : {}", ftBand);
            }
        }
    }

    /**
     * Prepare object parameters
     * @param target target to use
     */
    private void prepareTarget(final Target target) {
        Double flux;

        // Get band from wavelength range:
        final double lambdaMin = waveLengths[0];
        final double lambdaMax = waveLengths[nSpectralChannels - 1];
        final double lambdaMid = 0.5 * (lambdaMin + lambdaMax);

        // TODO: use band range to cover lambdaMin / lambdaMax:
        // JHK or LM or BVR
        final Band band = findBand(lambdaMid / AsproConstants.MICRO_METER); // microns
        this.insBand = band;
        this.insTargetBand = SpectralBandUtils.findBand(band);

        if (logger.isDebugEnabled()) {
            logger.debug("lambdaMin                     : {}", lambdaMin);
            logger.debug("lambda                        : {}", lambdaMid);
            logger.debug("lambdaMax                     : {}", lambdaMax);
            logger.debug("insBand                       : {}", insBand);
            logger.debug("insTargetBand                 : {}", insTargetBand);
        }

        // If a flux / magnitude is missing => user message
        // and it is impossible to compute any error
        // TODO: handle B|R vs V for VEGA:
        flux = target.getFlux(insTargetBand);
        this.objectMag = (flux != null) ? flux.doubleValue() : Double.NaN;
        if (logger.isDebugEnabled()) {
            logger.debug("objectMag                     : {}", objectMag);
        }

        if (fringeTrackerPresent) {
            flux = target.getFlux(ftBand);
            this.fringeTrackerMag = (flux != null) ? flux.doubleValue() : Double.NaN;
            if (logger.isDebugEnabled()) {
                logger.debug("fringeTrackerMag              : {}", fringeTrackerMag);
            }
        }

        flux = target.getFlux(aoBand);
        this.adaptiveOpticsMag = (flux != null) ? flux.doubleValue() : Double.NaN;
        if (logger.isDebugEnabled()) {
            logger.debug("adaptiveOpticsMag             : {}", adaptiveOpticsMag);
        }

        if (Double.isNaN(objectMag)
                || (fringeTrackerPresent && Double.isNaN(fringeTrackerMag))
                || Double.isNaN(adaptiveOpticsMag)) {
            // missing magnitude
            this.invalidParameters = true;

            addWarning(AsproConstants.WARN_MISSING_MAGS + " on target [" + target.getName() + "] in following bands: "
                    + (Double.isNaN(objectMag) ? insTargetBand : "")
                    + (fringeTrackerPresent && Double.isNaN(fringeTrackerMag) ? ftBand : "")
                    + (Double.isNaN(adaptiveOpticsMag) ? aoBand : ""));
        }
    }

    /**
     * Initialise other parameters
     */
    void initParameters() {
        // fast return if invalid configuration :
        if (invalidParameters) {
            return;
        }

        final int nObs = nPoints; // = targetPointInfos.length

        // Pass 1: find maximum flux per channel taking into account the target elevation:
        // strehl is spectrally dependent:
        final double[][] strehlPerChannel;

        if (useStrehlCorrection) {
            strehlPerChannel = new double[nObs][];

            for (int n = 0; n < nObs; n++) {
                double elevation = targetPointInfos[n].getElevation();

                strehlPerChannel[n] = Band.strehl(adaptiveOpticsMag, waveLengths, telDiam, seeing, nbOfActuators, elevation);

                if (logger.isDebugEnabled()) {
                    logger.debug("elevation                     : {}", elevation);
                    logger.debug("strehl ratio                  : {}", Arrays.toString(strehlPerChannel[n]));
                }
            }
        } else {
            strehlPerChannel = null;
        }

        final int nWLen = nSpectralChannels;

        // total number of thermal photons per spectral channel:
        final double[] nbTotalPhotTherm;

        if (this.nbPhotThermal == null) {
            nbTotalPhotTherm = null;
        } else {
            nbTotalPhotTherm = new double[nWLen];

            for (int i = 0; i < nWLen; i++) {
                // Per beam Per second:
                nbTotalPhotTherm[i] = this.nbPhotThermal[i] * quantumEfficiency;
            }
        }

        // Target flux per spectral channel per second per m^2:
        final double[] fluxSrcPerChannel = computeTargetFlux();

        // total number of science photons per spectral channel per second per telescope:
        final double[][] nbTotalPhot = new double[nObs][nWLen];

        final double telSurface = Math.PI * FastMath.pow2(0.5 * telDiam);

        // maximum number of science photons per channel and per second:
        double maxNbTotalPhotPerSec = 0.0;
        double nbPhot;

        for (int n = 0; n < nObs; n++) {

            for (int i = 0; i < nWLen; i++) {
                // Per second:
                nbPhot = (telSurface * quantumEfficiency) * transmission[i] * fluxSrcPerChannel[i];

                if (useStrehlCorrection) {
                    nbPhot *= strehlPerChannel[n][i];
                }

                nbTotalPhot[n][i] = nbPhot;

                if (nbTotalPhotTherm != null) {
                    // add thermal photons per sec:
                    nbPhot += nbTotalPhotTherm[i];
                }

                if (nbPhot > maxNbTotalPhotPerSec) {
                    maxNbTotalPhotPerSec = nbPhot;
                }
            }
            if (logger.isDebugEnabled()) {
                logger.debug("elevation                     : {}", targetPointInfos[n].getElevation());
                logger.debug("nbTotalPhotPerSec             : {}", Arrays.toString(nbTotalPhot[n]));
            }
        }

        if (logger.isDebugEnabled()) {
            logger.debug("maxNbTotalPhotPerSec          : {}", maxNbTotalPhotPerSec);
        }

        // dit used by this observation:
        double obsDit = dit;

        // maximum number of expected photoevents in dit (for all telescopes):
        final double maxTotalPhot = (nbTel * maxNbTotalPhotPerSec) * obsDit;

        // fraction of total interferometric flux in the peak pixel :
// TODO: adjust that code as both science and thermal photons are varying on spectral channels
// it may saturate few pixels ie do not divide by nbPixInterf as it means average(photons) not MAX !
// It is correct if 1 pixel = 1 spectral channel:
        final double peakflux = fracFluxInInterferometry * maxTotalPhot / nbPixInterf;

        if (logger.isDebugEnabled()) {
            logger.debug("maxTotalPhot                  : {}", maxTotalPhot);
            logger.debug("peakflux                      : {}", peakflux);
        }

        final int nbFrameToSaturation;
        if (detectorSaturation < peakflux) {
            // the dit is too long
            obsDit *= detectorSaturation / peakflux;

            addWarning("DIT too long (saturation). Adjusting it to (possibly impossible): " + formatTime(obsDit));

            nbFrameToSaturation = 1;
        } else {
            nbFrameToSaturation = (int) Math.floor(detectorSaturation / peakflux);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("dit (no saturation)           : {}", obsDit);
            logger.debug("nbFrameToSaturation           : {}", nbFrameToSaturation);
        }

        // Adjust instrumental visibility:
        this.vinst = instrumentalVisibility;

        if (fringeTrackerPresent) {
            if ((fringeTrackerMag <= fringeTrackerLimit) && (nbFrameToSaturation > 1)) {
                // correct instrumental visibility:
                // TODO: may be depend on the spectral band (H != K != N) ?
                for (int i = 0; i < this.vinst.length; i++) {
                    this.vinst[i] *= fringeTrackerInstrumentalVisibility;
                }

                if (logger.isDebugEnabled()) {
                    logger.debug("vinst (FT)                    : {}", Arrays.toString(vinst));
                }

                if (fringeTrackingMode) {
                    // FT is asked, can work, and is useful (need to integrate longer)
                    obsDit = Math.min(obsDit * nbFrameToSaturation, totalObsTime);
                    obsDit = Math.min(obsDit, fringeTrackerMaxIntTime);

                    if (logger.isDebugEnabled()) {
                        logger.debug("dit (FT)                      : {}", obsDit);
                    }

                    addInformation("Observation can take advantage of FT. Adjusting DIT to: " + formatTime(obsDit));
                } else {
                    addInformation("Observation can take advantage of FT (Group track).");
                }
            } else {
                addWarning("Observation can not use FT (magnitude limit or saturation).");

                if (logger.isDebugEnabled()) {
                    logger.debug("vinst (noFT)                    : {}", Arrays.toString(vinst));
                }
            }
        }

        // TODO: handle dead time = dit for MATISSE
        // total number of frames:
        final double nbFrames = totalObsTime / obsDit;

        // total frame correction = 1 / SQRT(nFrames):
        this.totFrameCorrection = 1.0 / Math.sqrt(nbFrames);

        if (logger.isDebugEnabled()) {
            logger.debug("nbFrames                      : {}", nbFrames);
            logger.debug("totFrameCorrection            : {}", totFrameCorrection);
        }

        // 2nd pass: obsDit is known = integration time (setup)
        this.nbPhotTherm = new double[nWLen];

        if (nbTotalPhotTherm != null) {
            for (int i = 0; i < nWLen; i++) {
                // corrected total number of thermal photons using the final observation dit per telescope:
                nbPhotTherm[i] = nbTotalPhotTherm[i] * obsDit;
            }

            if (logger.isDebugEnabled()) {
                logger.debug("nbPhotTherm              : {}", Arrays.toString(nbPhotTherm));
            }
        }

        this.params = new NoiseWParams[nObs];

        for (int n = 0; n < nObs; n++) {

            // give back the two useful values for the noise estimate :
            final NoiseWParams param = new NoiseWParams(nWLen);
            this.params[n] = param;

            final double[] nbPhotInterf = param.nbPhotInterf;
            final double[] nbPhotPhoto = param.nbPhotPhoto;

            for (int i = 0; i < nWLen; i++) {

                // corrected total number of photons using the final observation dit per telescope:
                nbTotalPhot[n][i] *= obsDit;
                nbPhot = nbTotalPhot[n][i];

                // number of photons in the interferometric channel (per telescope):
                nbPhotInterf[i] = nbPhot * fracFluxInInterferometry;
                // number of photons in each photometric channel (photometric flux):
                nbPhotPhoto[i] = nbPhot * fracFluxInPhotometry;
            }

            if (logger.isDebugEnabled()) {
                logger.debug("elevation                     : {}", targetPointInfos[n].getElevation());
                logger.debug("nbTotalPhot                   : {}", Arrays.toString(nbTotalPhot[n]));
                logger.debug("nbPhotonInI                   : {}", Arrays.toString(nbPhotInterf));
                logger.debug("nbPhotonInP                   : {}", Arrays.toString(nbPhotPhoto));
            }

            // Prepare numeric constants for fast error computation:
            prepareVis2Error(n);
            prepareT3PhiError(n);
        }

        if (logger.isDebugEnabled()) {
            if (nWLen < 50) {

                for (int i = 0; i < nWLen; i++) {
                    dumpVis2Error(i);
                }
            } else {
                dumpVis2Error(0);
                dumpVis2Error(iMidChannel);
                dumpVis2Error(nSpectralChannels - 1);
            }
        }
    }

    private void dumpVis2Error(final int iChannel) {
        logger.debug("channel                       : {}", iChannel);
        dumpVis2ErrorSample(iChannel, 1d);
        dumpVis2ErrorSample(iChannel, 0.75d);
        dumpVis2ErrorSample(iChannel, 0.5d);
        dumpVis2ErrorSample(iChannel, 0.25d);
        dumpVis2ErrorSample(iChannel, 0.1d);
        dumpVis2ErrorSample(iChannel, 0.01d);
        dumpVis2ErrorSample(iChannel, 0.001d);
    }

    private void dumpVis2ErrorSample(final int iChannel, final double visAmp) {
        double v2 = visAmp * visAmp;
        double errV2 = computeVis2ErrorNoBias(iChannel, visAmp);
        double snr = v2 / errV2;

        logger.debug("computeVis2Error({}) :{} SNR= {}", NumberUtils.trimTo5Digits(v2), errV2, NumberUtils.trimTo3Digits(snr));
    }

    double[] computeTargetFlux() {

        // TODO: fix transmission for all instruments (before now):
        final double[] atmTrans = AtmosphereSpectrumService.getInstance().getTransmission(this.waveLengths, this.waveBands);

        if (logger.isDebugEnabled()) {
            logger.debug("atmTrans: {}", Arrays.toString(atmTrans));
        }

        // nb of photons m^-2.s^-1.m^-1 for an object at magnitude 0:
        // note: fzero depends on the spectral band:
        final double fzero = FastMath.pow(10d, insBand.getLogFluxZero()) * (insBand.getLambda() * AsproConstants.MICRO_METER) / (H_PLANCK * C_LIGHT);

        // TODO: source flux may be spectrally dependent:
        // nb of photons m^-2.s^-1.m^-1 for the target object:
        final double fsrc = fzero * FastMath.pow(10d, -0.4d * objectMag);

        if (logger.isDebugEnabled()) {
            logger.debug("fzero                         : {}", fzero);
            logger.debug("fsrc                          : {}", fsrc);
        }

        final int nWLen = nSpectralChannels;
        // nb photons per surface and per second:
        final double[] fluxSrcPerChannel = new double[nWLen];

        /* TODO: use band range (previous / next) to interpolate flux or SED based on spectral type */
        for (int i = 0; i < nWLen; i++) {
            // nb of photons m^-2.s^-1 for the target object:
            fluxSrcPerChannel[i] = atmTrans[i] * fsrc * waveBands[i];
        }
        if (logger.isDebugEnabled()) {
            logger.debug("fluxSrcPerChannel             : {}", Arrays.toString(fluxSrcPerChannel));
        }

        return fluxSrcPerChannel;
    }

    /**
     * Return the correlated flux weight of the object (without visibility). 
     * It returns NaN if the flux can not be computed
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @return correlated flux or NaN if the flux can not be computed
     */
    public double computeCorrelatedFluxWeight(final int iPoint, final int iChannel) {
        if (DO_CHECKS) {
            // fast return NaN if invalid configuration :
            if (this.invalidParameters) {
                return Double.NaN;
            }
            if (check(iPoint, iChannel)) {
                return Double.NaN;
            }
        }

        final NoiseWParams param = this.params[iPoint];

        // correlated flux (include instrumental visibility loss) (1T):
        final double fcorrel = param.nbPhotInterf[iChannel] * vinst[iChannel];

        return fcorrel;
    }

    /**
     * Prepare numeric constants for square visiblity error
     *
     * Note: this method is statefull and NOT thread safe
     *
     * @param iPoint index of the observable point
     */
    private void prepareVis2Error(final int iPoint) {
        final int nWLen = nSpectralChannels;

        final NoiseWParams param = this.params[iPoint];

        final double[] nbPhotInterf = param.nbPhotInterf;
        final double[] nbPhotPhoto = param.nbPhotPhoto;

        final double[] sqErrVis2Phot = param.sqErrVis2Phot;
        final double[] sqCorFluxCoef = param.sqCorFluxCoef;
        final double[] varSqCorFluxCoef = param.varSqCorFluxCoef;
        final double[] varSqCorFluxConst = param.varSqCorFluxConst;

        if (usePhotometry) {
            for (int i = 0; i < nWLen; i++) {
                // variance of the photometric flux in photometric channel:
                final double varFluxPhot = nbPhotPhoto[i]
                        + ratioPhotoPerBeam * (nbPhotTherm[i] + nbPixPhoto * FastMath.pow2(ron));

                // square error contribution of 2 photometric channels on the square visiblity FiFj:
                sqErrVis2Phot[i] = 2.0 * varFluxPhot / FastMath.pow2(nbPhotPhoto[i]);
            }

            if (logger.isDebugEnabled()) {
                logger.debug("sqErrVis2Phot                 : {}", Arrays.toString(sqErrVis2Phot));
            }
        } else {
            Arrays.fill(sqErrVis2Phot, 0.0);
        }

        double nbPhot;

        for (int i = 0; i < nWLen; i++) {
            // Per telescope
            nbPhot = nbPhotInterf[i];

            // squared correlated flux (include instrumental visibility loss) for vis2 = 1.0:
            sqCorFluxCoef[i] = FastMath.pow2(nbPhot * vinst[i]);

            // total number of photons for all telescopes:
            nbPhot *= nbTel;

            // variance of the squared correlated flux = sqCorFlux * coef + constant
            varSqCorFluxCoef[i] = 2.0 * (nbPhot + nbPhotTherm[i] * nbTel)
                    + 4.0 + 2.0 * nbPixInterf * FastMath.pow2(ron);

            varSqCorFluxConst[i] = nbPhot * (1.0 + nbPhot + 2.0 * nbPixInterf * FastMath.pow2(ron))
                    + nbPixInterf * (nbPixInterf + 3.0) * FastMath.pow(ron, 4.0);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("elevation                     : {}", targetPointInfos[iPoint].getElevation());
            logger.debug("sqCorFluxCoef                 : {}", Arrays.toString(sqCorFluxCoef));
            logger.debug("varSqCorFluxCoef              : {}", Arrays.toString(varSqCorFluxCoef));
            logger.debug("varSqCorFluxConst             : {}", Arrays.toString(varSqCorFluxConst));
        }
    }

    /**
     * Compute error on square visibility
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @param visAmp visibility amplitude
     * @param useBias use instrumentalVisibilityBias and instrumentVis2CalibrationBias
     * @return square visiblity error
     */
    private double computeVis2Error(final int iPoint, final int iChannel,
                                    final double visAmp, final boolean useBias) {
        if (DO_CHECKS) {
            // fast return NaN if invalid configuration :
            if (this.invalidParameters) {
                return Double.NaN;
            }
            if (check(iPoint, iChannel)) {
                return Double.NaN;
            }
        }
        // TODO: use V2 as input parameter instead of visAmp:
        final double vis2 = FastMath.pow2(visAmp);

        final NoiseWParams param = this.params[iPoint];

        // squared correlated flux (include instrumental visibility loss):
        double sqCorFlux = param.sqCorFluxCoef[iChannel] * vis2;
        /*
         double sqCorFlux = FastMath.pow2(nbPhotonInI * vinst / nbTel) * vis2;
         */

        // variance of the squared correlated flux:
        // variance = sqCorFlux * coef + constant
        final double varSqCorFlux = sqCorFlux * param.varSqCorFluxCoef[iChannel]
                + param.varSqCorFluxConst[iChannel];
        /*
         final double varSqCorFlux = sqCorFlux * (2d * nbPhotonInI + 4d + 2d * nbPixInterf * ron * ron)
         + nbPhotonInI * (1d + nbPhotonInI + 2d * nbPixInterf * ron * ron)
         + nbPixInterf * (nbPixInterf + 3d) * FastMath.pow(ron, 4d);
         */

        // Uncertainty on square visibility per frame:
        double errVis2;
        if (usePhotometry) {
            // TODO: howto deal with beams ? ie what is the impact of 1 vs 4 beams ?
            errVis2 = vis2 * Math.sqrt((varSqCorFlux / FastMath.pow2(sqCorFlux)) + param.sqErrVis2Phot[iChannel]);
        } else {
            // no photometry...
            errVis2 = vis2 * Math.sqrt((varSqCorFlux / FastMath.pow2(sqCorFlux)));
        }
        // repeat OBS measurements to reach totalObsTime minutes:
        errVis2 *= totFrameCorrection;

        // Limit excessively large errors (very low transmission or strehl):
        errVis2 = Math.min(errVis2, MAX_ERR_V2);

        if (useBias) {
            // Note: bias are normally not a gaussian distribution (mean = 0) so should not be used to compute gaussian noise !!
            if (useVis2CalibrationBias) {
                // JBLB: bias estimation (first order for PIONIER):
                return Math.max(errVis2, instrumentVis2CalibrationBias * vis2 + instrumentalVis2Bias);
            }
            // TODO: find correct coefficients for instruments AMBER, MIDI, VEGA and use instrumentVis2CalibrationBias in estimation.
            return Math.max(errVis2, instrumentalVis2Bias);
        }
        return errVis2;
    }

    /**
     * Prepare numeric constants for closure phase error
     *
     * Note: this method is statefull and NOT thread safe
     *
     * @param iPoint index of the observable point
     * 
     * @deprecated 
     */
    private void prepareT3PhiError(final int iPoint) {
        final int nWLen = nSpectralChannels;

        final NoiseWParams param = this.params[iPoint];

        final double[] nbPhotInterf = param.nbPhotInterf;

        final double[] t3photCoef = param.t3photCoef;
        final double[] t3photCoef2 = param.t3photCoef2;
        final double[] t3photCoef3 = param.t3photCoef3;

        final double[] t3detConst = param.t3detConst;
        final double[] t3detCoef1 = param.t3detCoef1;
        final double[] t3detCoef2 = param.t3detCoef2;

        for (int i = 0; i < nWLen; i++) {
            final double invNbPhotonInIPerTel = 1.0 / nbPhotInterf[i];

            // photon noise on closure phase
            t3photCoef[i] = invNbPhotonInIPerTel;
            t3photCoef2[i] = FastMath.pow2(invNbPhotonInIPerTel);
            t3photCoef3[i] = FastMath.pow3(invNbPhotonInIPerTel);

            // detector noise on closure phase
            t3detConst[i] = FastMath.pow(invNbPhotonInIPerTel, 6d) * (FastMath.pow(nbPixInterf, 3d) * FastMath.pow(ron, 6d)
                    + 3 * FastMath.pow2(nbPixInterf) * FastMath.pow(ron, 6d));

            t3detCoef1[i] = FastMath.pow(invNbPhotonInIPerTel, 4d) * (3d * nbPixInterf * FastMath.pow(ron, 4d)
                    + FastMath.pow2(nbPixInterf) * FastMath.pow(ron, 4d));

            t3detCoef2[i] = FastMath.pow2(invNbPhotonInIPerTel) * nbPixInterf * FastMath.pow2(ron);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("elevation                     : {}", targetPointInfos[iPoint].getElevation());
            logger.debug("t3photCoef                    : {}", Arrays.toString(t3photCoef));
            logger.debug("t3photCoef2                   : {}", Arrays.toString(t3photCoef2));
            logger.debug("t3photCoef3                   : {}", Arrays.toString(t3photCoef3));
            logger.debug("t3detConst                    : {}", Arrays.toString(t3detConst));
            logger.debug("t3detCoef1                    : {}", Arrays.toString(t3detCoef1));
            logger.debug("t3detCoef2                    : {}", Arrays.toString(t3detCoef2));
        }
    }

    /**
     * Compute error on closure phase. It returns NaN if the error can not be computed
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @param visAmp12 visibility amplitude of baseline AB = 12
     * @param visAmp23 visibility amplitude of baseline BC = 23
     * @param visAmp31 visibility amplitude of baseline CA = 31
     * @return error on closure phase in radians or NaN if the error can not be computed
     * 
     * @deprecated 
     */
    public double computeT3PhiError(final int iPoint, final int iChannel,
                                    final double visAmp12, final double visAmp23, final double visAmp31) {
        if (DO_CHECKS) {
            // fast return NaN if invalid configuration :
            if (this.invalidParameters) {
                return Double.NaN;
            }
            if (check(iPoint, iChannel)) {
                return Double.NaN;
            }
        }

        final NoiseWParams param = this.params[iPoint];
        final double[] t3photCoef = param.t3photCoef;
        final double[] t3photCoef2 = param.t3photCoef2;
        final double[] t3photCoef3 = param.t3photCoef3;

        final double[] t3detConst = param.t3detConst;
        final double[] t3detCoef1 = param.t3detCoef1;
        final double[] t3detCoef2 = param.t3detCoef2;

        // include instrumental visib
        final double v1 = visAmp12 * vinst[iChannel];
        final double v2 = visAmp23 * vinst[iChannel];
        final double v3 = visAmp31 * vinst[iChannel];

        double v123 = v1 * v2 * v3;

        // protect zero divide: why 1e-3 ?
        v123 = Math.max(v123, 1e-3);

        final double v12 = v1 * v2;
        final double v13 = v1 * v3;
        final double v23 = v2 * v3;

        final double sv1 = v1 * v1;
        final double sv2 = v2 * v2;
        final double sv3 = v3 * v3;

        final double sv123 = v123 * v123;
        final double sv12 = v12 * v12;
        final double sv13 = v13 * v13;
        final double sv23 = v23 * v23;

        // photon noise on closure phase
        final double scpphot = (t3photCoef3[iChannel] * (nbTel * nbTel * nbTel - 2d * v123)
                + t3photCoef2[iChannel] * (nbTel * nbTel * (sv1 + sv2 + sv3) - (sv1 * sv1 + sv2 * sv2 + sv3 * sv3 + 2 * (sv12 + sv13 + sv23)))
                + t3photCoef[iChannel] * (nbTel * (sv12 + sv13 + sv23) - 2d * v123 * (sv1 + sv2 + sv3))) / (2d * sv123);
        /*
         final double scpphot = (Math.pow(nbTel / nbPhotonInI, 3d) * (nbTel * nbTel * nbTel - 2d * v123)
         + Math.pow(nbTel / nbPhotonInI, 2d) * (nbTel * nbTel * (sv1 + sv2 + sv3) - (sv1 * sv1 + sv2 * sv2 + sv3 * sv3 + 2 * (sv12 + sv13 + sv23)))
         + (nbTel / nbPhotonInI) * (nbTel * (sv12 + sv13 + sv23) - 2d * v123 * (sv1 + sv2 + sv3))) / (2d * sv123);
         */

        // detector noise on closure phase
        final double scpdet = (t3detConst[iChannel]
                + t3detCoef1[iChannel] * (sv1 + sv2 + sv3)
                + t3detCoef2[iChannel] * (sv12 + sv13 + sv23)) / (2d * sv123);
        /*
         double scpdet = (Math.pow(nbTel / nbPhotonInI, 6d) * (Math.pow(nbPixInterf, 3d) * Math.pow(ron, 6d) + 3 * Math.pow(nbPixInterf, 2d) * Math.pow(ron, 6d))
         + Math.pow(nbTel / nbPhotonInI, 4d) * (sv1 + sv2 + sv3) * (3d * nbPixInterf * Math.pow(ron, 4d) + Math.pow(nbPixInterf, 2d) * Math.pow(ron, 4d))
         + Math.pow(nbTel / nbPhotonInI, 2d) * nbPixInterf * Math.pow(ron, 2d) * (sv12 + sv13 + sv23) ) / (2d * sv123);
         */

        // total noise on closure phase per frame:
        double sclosph = Math.sqrt(scpphot + scpdet);

        // repeat OBS measurements to reach totalObsTime minutes
        sclosph *= totFrameCorrection;

        // t3PhiErr and t3AmpErr = t3Amp * t3PhiErr :
        // Note: bias are normally not a gaussian distribution (mean = 0) so should not be used to compute gaussian noise !!
        return Math.max(sclosph, instrumentalPhaseBias);
    }

    /**
     * Return true if all parameters are valid i.e. returned errors are valid
     * @return true if all parameters are valid
     */
    public boolean isValid() {
        return !this.invalidParameters;
    }

    /**
     * Compute error on complex visibility derived from computeVis2Error(visAmp).
     * It returns Double.NaN if the error can not be computed
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @param visAmp visibility amplitude
     * @return complex visiblity error or NaN if the error can not be computed
     */
    public double computeVisComplexErrorValue(final int iPoint, final int iChannel, final double visAmp) {
        // visibility amplitude error (gaussian distribution):
        double visAmpErr = computeVisError(iPoint, iChannel, visAmp);

        // Limit excessively large errors (very low transmission or strehl):
        visAmpErr = Math.min(visAmpErr, MAX_ERR_V);

        if (NoiseService.USE_DISTRIB_APPROACH) {
            // I disagree: it seems a bivariate distribution (2 independent variables C = re + i * im)
            // var(re)=var(im)=var(amp) for a circular normal distribution
            return visAmpErr;
        }

        // Distribute the error on RE/IM parts for an uniform error distribution :
        // see These Martin Vannier (2003) p 76
        // sigma2(visRe) = 1/2 ( sigma2(visRe) + sigma2(visIm) ) = sigma2(vis) / 2
        // complex visibility error : visErrRe = visErrIm = visAmpErr / SQRT(2) :
        return visAmpErr * VIS_AMP_TO_VIS_CPX_ERR;
    }

    /**
     * Compute error on visibility amplitude derived from computeVis2Error(visAmp)
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @param visAmp visibility amplitude
     * @return visiblity error
     */
    private double computeVisError(final int iPoint, final int iChannel, final double visAmp) {
        // vis2 error without bias :
        final double errV2 = computeVis2ErrorNoBias(iPoint, iChannel, visAmp);

        // dvis = d(vis2) / (2 * vis) :
        final double visAmpErr = errV2 / (2d * visAmp);

        // convert instrumental phase bias as an error too. Use it as a limit:
        // Note: bias are normally not a gaussian distribution (mean = 0) so should not be used to compute gaussian noise !!
        return Math.max(visAmpErr, visAmp * instrumentalPhaseBias);
    }

    /**
     * Compute error on square visibility. It returns NaN if the error can not be computed
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @param visAmp visibility amplitude
     * @return square visiblity error or NaN if the error can not be computed
     */
    public double computeVis2Error(final int iPoint, final int iChannel, final double visAmp) {
        return computeVis2Error(iPoint, iChannel, visAmp, true);
    }

    /**
     * Compute error on square visibility without bias. It returns NaN if the error can not be computed
     *
     * @param iChannel index of the channel
     * @param visAmp visibility amplitude
     * @return square visiblity error or NaN if the error can not be computed
     */
    private double computeVis2ErrorNoBias(final int iChannel, final double visAmp) {
        return computeVis2Error(iMidPoint, iChannel, visAmp, false);
    }

    /**
     * Compute error on square visibility without bias. It returns NaN if the error can not be computed
     *
     * @param iPoint index of the observable point
     * @param iChannel index of the channel
     * @param visAmp visibility amplitude
     * @return square visiblity error or NaN if the error can not be computed
     */
    private double computeVis2ErrorNoBias(final int iPoint, final int iChannel, final double visAmp) {
        return computeVis2Error(iPoint, iChannel, visAmp, false);
    }

    private boolean check(final int iPoint, final int iChannel) {
        if (iPoint < 0 || iPoint >= nPoints) {
            logger.warn("invalid point index {}, expect [0 to {}]", iPoint, nPoints);
            return true;
        }
        if (iChannel < 0 || iChannel >= nSpectralChannels) {
            logger.warn("invalid channel index {}, expect [0 to {}]", iChannel, nSpectralChannels);
            return true;
        }
        return false;
    }

    /* --- VisNoiseService implementation --- */
    /**
     * Return true if this service is enabled
     * @return true if this service is enabled
     */
    @Override
    public boolean isEnabled() {
        return isValid();
    }

    /**
     * Compute error on complex visibility given its amplitude.
     * It returns Double.NaN if the error can not be computed
     *
     * @param visAmp visibility amplitude
     * @return complex visiblity error or NaN if the error can not be computed
     */
    @Override
    public double computeVisComplexErrorValue(final double visAmp) {
        return computeVisComplexErrorValue(iMidPoint, iMidChannel, visAmp);
    }

    /* --- utility methods --- */
    /**
     * Find the band corresponding to the given wavelength
     * but always use V band instead of I & R bands
     *
     * @param waveLength wave length in microns
     * @return corresponding band
     * @throws IllegalArgumentException if no band found
     */
    public static Band findBand(final double waveLength) throws IllegalArgumentException {
        Band band = Band.findBand(waveLength);
        // TODO: fix that logic to use all possible bands within the instrument bandwidth
        switch (band) {
            case B:
            case V:
            case R:
            case I:
                // always use V for VEGA:
                return Band.V;
            default:
                return band;
        }
    }

    /**
     * Add a warning message in the OIFits file
     * @param msg message to add
     */
    private void addWarning(final String msg) {
        this.warningContainer.addWarningMessage(msg);
    }

    /**
     * Add an information message in the OIFits file
     * @param msg message to add
     */
    private void addInformation(final String msg) {
        this.warningContainer.addInformationMessage(msg);
    }

    /**
     * Format time value for warning messages
     * @param value time (s)
     * @return formatted value
     */
    private String formatTime(final double value) {
        final String unit;
        final double val;
        if (value >= 1d) {
            val = value;
            unit = " s";
        } else {
            val = value * 1000d;
            unit = " ms";
        }
        final DecimalFormat df = new DecimalFormat("##0.##");
        return df.format(val) + unit;
    }

    private static double getPercents(final double bias) {
        return 0.01d * bias; // percents
    }

    static class NoiseWParams {

        /* varying values (spectrally dependent) */
        /** (W) number of photons in interferometer channel per telescope */
        final double[] nbPhotInterf;
        /** (W) number of photons in each photometric channel (photometric flux) */
        final double[] nbPhotPhoto;
        /** (W) square error contribution of 2 photometric channels on the square visiblity FiFj */
        final double[] sqErrVis2Phot;
        /** (W) coefficient used to the squared correlated flux */
        final double[] sqCorFluxCoef;
        /** (W) coefficient used to compute variance of the squared correlated flux */
        final double[] varSqCorFluxCoef;
        /** (W) constant used to compute variance of the squared correlated flux */
        final double[] varSqCorFluxConst;
        /** (W) t3 phi error - coefficient */
        final double[] t3photCoef;
        /** (W) t3 phi error - coefficient^2 */
        final double[] t3photCoef2;
        /** (W) t3 phi error - coefficient^3 */
        final double[] t3photCoef3;
        /** (W) t3 phi detector error - constant */
        final double[] t3detConst;
        /** (W) t3 phi detector error - coefficient 1 */
        final double[] t3detCoef1;
        /** (W) t3 phi detector error - coefficient 2 */
        final double[] t3detCoef2;

        NoiseWParams(final int nWLen) {
            // TODO: align to 8 or 16 ? (simd ?)
            this.nbPhotInterf = init(nWLen);
            this.nbPhotPhoto = init(nWLen);
            this.sqErrVis2Phot = init(nWLen);
            this.sqCorFluxCoef = init(nWLen);
            this.varSqCorFluxCoef = init(nWLen);
            this.varSqCorFluxConst = init(nWLen);
            this.t3photCoef = init(nWLen);
            this.t3photCoef2 = init(nWLen);
            this.t3photCoef3 = init(nWLen);
            this.t3detConst = init(nWLen);
            this.t3detCoef1 = init(nWLen);
            this.t3detCoef2 = init(nWLen);
        }

        static double[] init(final int len) {
            final double[] array = new double[len];
            Arrays.fill(array, Double.NaN);
            return array;
        }
    }
}
