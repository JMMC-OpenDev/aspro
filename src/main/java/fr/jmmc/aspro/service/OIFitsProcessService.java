/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.UserModel;
import fr.jmmc.aspro.model.util.TargetRole;
import fr.jmmc.jmcs.util.StatUtils;
import fr.jmmc.jmal.complex.Complex;
import fr.jmmc.jmal.complex.MutableComplex;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.SpecialChars;
import fr.jmmc.jmcs.util.StatUtils.ComplexDistribution;
import fr.jmmc.oitools.model.DataModel;
import fr.jmmc.oitools.model.OIArray;
import fr.jmmc.oitools.model.OIData;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIFlux;
import fr.jmmc.oitools.model.OIT3;
import fr.jmmc.oitools.model.OIVis;
import fr.jmmc.oitools.model.OIVis2;
import fr.jmmc.oitools.model.OIWavelength;
import fr.nom.tam.fits.FitsException;
import java.io.IOException;
import java.util.Arrays;
import net.jafama.FastMath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class updates an existing OIFits structure with the given user model to compute Observables
 * 
 * @author bourgesl
 */
public class OIFitsProcessService extends AbstractOIFitsProducer {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(OIFitsProcessService.class.getName());
    /** enable the OIFits validation */
    protected final static boolean DO_VALIDATE_OIFITS = true;

    /* members */
    /** fast mode: true to ignore useless data (faster); false to have highest precision */
    private final boolean useFastMode;
    /** fast mode error in percents */
    private final double fastError;
    /** apodization flag: true to perform image apodization; false to disable */
    private final boolean doApodization;
    /** optional telescope diameter (meters) used by image apodization (m) */
    private final double diameter;
    /** temporary OIDATA table */
    private OIData oiData = null;
    /** index (subset for complex data) to original OIWaveLength indices */
    private int[] wlIndex;
    /** index of original OIWaveLength indices to be cleared */
    private int[] wlClear;

    /**
     * Public constructor
     * @param target target to process
     * @param oifitsFile oifits to process
     * @param useFastMode true to ignore useless data (faster); false to have highest precision
     * @param fastError fast mode threshold in percents
     * @param options OIFits options
     * @param doApodization true to perform image apodization; false to disable
     * @param diameter default telescope diameter (meters) used by image apodization (m)
     */
    public OIFitsProcessService(final Target target,
                                final OIFitsFile oifitsFile,
                                final boolean useFastMode, final double fastError,
                                final OIFitsProducerOptions options,
                                final boolean doApodization, final double diameter) {

        super(target, TargetRole.SCI, options, null);

        this.oiFitsFile = oifitsFile;

        // Perform analysis:
        oifitsFile.analyze();

        this.useFastMode = useFastMode;
        this.fastError = fastError;

        this.doApodization = doApodization;
        this.diameter = diameter;

        // Noise Service is null (disabled)
    }

    /**
     * Update the OIFits structure
     * @param warningContainer container for warning messages
     * @return true if sucessful; false otherwise
     */
    public boolean processOIFits(final WarningContainer warningContainer) {

        for (OIWavelength oiWaveLength : this.oiFitsFile.getOiWavelengths()) {
            final String insName = oiWaveLength.getInsName();

            logger.info("processing OIWavelength.INSNAME: {}", insName);

            // Prepare with OI_WAVELENGTH:
            // note: may adjust wavelengths:
            if (!prepare(oiWaveLength, warningContainer)) {
                return false;
            }

            final double lambdaMin = StatUtils.min(this.waveLengths);
            final double lambdaMax = StatUtils.max(this.waveLengths);

            for (OIData table : this.oiFitsFile.getOiDataList()) {
                if (table.getOiWavelength() == oiWaveLength) {
                    this.oiData = table;

                    if (this.hasModel && !target.hasAnalyticalModel()) {
                        // Check user model:
                        validateOrPrepareUserModel(oiData, target.getUserModel(),
                                useFastMode, fastError,
                                doApodization, diameter, lambdaMin, lambdaMax
                        );
                    }

                    // Compute complex visibilities:
                    if (this.computeModelVisibilities()) {
                        // use complex visibilities
                        computeOIData();

                        // fast interrupt :
                        if (Thread.currentThread().isInterrupted()) {
                            return false;
                        }
                    } else if (this.oiData instanceof OIFlux) {
                        // OI_FLUX :
                        computeOIFlux((OIFlux) oiData);
                    }
                }
            }
        }
        return true;
    }

    private static void validateOrPrepareUserModel(final OIData oiData, final UserModel userModel,
                                                   final boolean useFastMode, final double fastError,
                                                   final boolean doApodization, final double defDiameter,
                                                   final double lambdaMin, final double lambdaMax) throws IllegalArgumentException {

        // Apodization: get Telescope diameter and instrument wavelength:
        double diameter = Double.NaN;

        if (doApodization) {
            final OIArray oiArray = oiData.getOiArray();

            double diameterMin = Double.POSITIVE_INFINITY;
            double diameterMax = Double.NEGATIVE_INFINITY;

            for (short[] staIndexes : oiData.getDistinctStaConf()) {
                for (short staIndex : staIndexes) {
                    final Integer i = oiArray.getRowIndex(Short.valueOf(staIndex));
                    if (i != null) {
                        final double telDiam = oiArray.getDiameter()[i];

                        if (logger.isDebugEnabled()) {
                            logger.debug("Station: {} diameter: {}", oiArray.getStaName()[i], telDiam);
                        }
                        if (telDiam < diameterMin) {
                            diameterMin = telDiam;
                        }
                        if (telDiam > diameterMax) {
                            diameterMax = telDiam;
                        }
                    }
                }
            }
            logger.debug("diameters: {} - {}", diameterMin, diameterMax);

            // check range if diameterMin != diameterMax ?
            if (!NumberUtils.equals(diameterMin, diameterMax)) {
                logger.warn("Telescope diameters are varying among configurations in [{} - {} m] (apodization)", diameterMin, diameterMax);
            }
            if (diameterMax > 0.0) {
                diameter = diameterMax;
                logger.debug("Using diameter: {}", diameter);
            } else {
                logger.warn("Missing telescope diameter values in OI_ARRAY tables");
                if (defDiameter > 0.0) {
                    logger.warn("Using default diameter = {} m", defDiameter);
                    diameter = defDiameter;
                } else {
                    logger.warn("Apodization is disabled (use CLI argument -diameter to set default diameter value)");
                }
            }
        }

        final ApodizationParameters params = new ApodizationParameters(diameter, lambdaMin, lambdaMax);

        // IF needed reload (or process again apodization + image preparation...)
        if (!UserModelService.checkAiryRadius(userModel, params)) {
            try {
                // throws exceptions if the given fits file or image is incorrect:
                UserModelService.prepareUserModel(userModel, useFastMode, fastError, doApodization, params);
            } catch (FitsException fe) {
                throw new RuntimeException("Could not read file: " + userModel.getFile(), fe);
            } catch (IOException ioe) {
                throw new RuntimeException("Could not read file: " + userModel.getFile(), ioe);
            } catch (IllegalArgumentException iae) {
                throw iae;
            }
        }
    }

    private void computeOIData() {
        if (this.oiData instanceof OIVis) {
            // OI_VIS :
            computeOIVis((OIVis) oiData);
        } else if (this.oiData instanceof OIVis2) {
            // OI_VIS2 :
            computeOIVis2((OIVis2) oiData);
        } else if (this.oiData instanceof OIT3) {
            // OI_T3 :
            computeOIT3((OIT3) oiData);
        } else {
            logger.info("Unsupported table: {}", oiData);
        }
    }

    /**
     * Prepare OIFits keywords and the instrumental spectral configuration and may add warning messages
     * @param oiWaveLength OI_WAVELENGTH table to use
     * @param warningContainer warning container to use if needed
     * @return true if the model is valid; false otherwise
     */
    private boolean prepare(final OIWavelength oiWaveLength, final WarningContainer warningContainer) {

        this.instrumentName = oiWaveLength.getInsName();
        if (logger.isDebugEnabled()) {
            logger.debug("instrumentName: {}", instrumentName);
        }

        // TODO: handle properly spectral channels (rebinning):
        int nWaveLengths = oiWaveLength.getNWave();

        // prepare wavelengths independently of the user model Fits cube (wavelengths):
        // note: wavelength array can be unordered:
        this.waveLengths = oiWaveLength.getEffWaveAsDouble();

        double lambdaMin = StatUtils.min(this.waveLengths);
        double lambdaMax = StatUtils.max(this.waveLengths);

        if (logger.isDebugEnabled()) {
            logger.debug("lambdaMin: {}", lambdaMin);
            logger.debug("lambdaMax: {}", lambdaMax);
            logger.debug("nChannels: {}", nWaveLengths);
        }

        this.waveBands = convertArray(oiWaveLength.getEffBand());
        final double waveBand = StatUtils.mean(this.waveBands);

        // Initial Wavelength information:
        String firstChannel = Double.toString(convertWL(lambdaMin));
        String lastChannel = null;
        if (nWaveLengths > 1) {
            lastChannel = Double.toString(convertWL(lambdaMax));
        }
        warningContainer.addInformation(instrumentName + " instrument mode: "
                + nWaveLengths + " channels "
                + '[' + firstChannel + ((lastChannel != null) ? (" - " + lastChannel) : "") + " " + SpecialChars.UNIT_MICRO_METER + "] "
                + "(band: " + convertWL(waveBand) + " " + SpecialChars.UNIT_MICRO_METER + ')');

        // keep number of channels:
        final double[] waveLengthsInput = this.waveLengths;
        final int nChannels = nWaveLengths;

        // Keep only spectral channels where user model is defined:
        // note: Instrument spectral channels (waveLengths, waveBand, lambdaMin, lambdaMax) can be modified by this method:
        final boolean isModelWLValid = prepareUserModel(warningContainer);

        // refresh:
        nWaveLengths = this.waveLengths.length;
        wlIndex = new int[nWaveLengths];
        wlClear = new int[nChannels - nWaveLengths];

        // adjust used spectral channels in information and log:
        if (nWaveLengths != nChannels) {
            // Wavelength information:
            lambdaMin = StatUtils.min(this.waveLengths);
            lambdaMax = StatUtils.max(this.waveLengths);

            firstChannel = Double.toString(convertWL(lambdaMin));
            lastChannel = null;
            if (nWaveLengths > 1) {
                lastChannel = Double.toString(convertWL(lambdaMax));
            }
            warningContainer.addInformation("Restricted instrument mode: "
                    + nWaveLengths + " channels "
                    + '[' + firstChannel + ((lastChannel != null) ? (" - " + lastChannel) : "") + " " + SpecialChars.UNIT_MICRO_METER + "] ");

            // Build wavelength index:
            for (int l = 0, m; l < nWaveLengths; l++) {
                int idx = -1;
                final double wl = waveLengths[l];

                for (m = 0; m < nChannels; m++) {
                    if (waveLengthsInput[m] == wl) {
                        idx = m;
                        // exact, no rounding must have happen:
                        break;
                    }
                }
                if (idx == -1) {
                    throw new IllegalStateException("Wavelength[" + wl + "] not found !");
                }
                wlIndex[l] = idx;
            }
            if (logger.isDebugEnabled()) {
                logger.debug("wlIndex: {}", Arrays.toString(wlIndex));
            }

            // Complementary:
            int i = 0;
            for (int m = 0, l; m < nChannels; m++) {
                int idx = -1;

                for (l = 0; l < nWaveLengths; l++) {
                    if (wlIndex[l] == m) {
                        idx = m;
                        break;
                    }
                }
                if (idx == -1) {
                    wlClear[i++] = m;
                }
            }
            if (i != wlClear.length) {
                throw new IllegalStateException("wlClear index incorrect at " + i + "!");
            }
        } else {
            // 1-1 index:
            for (int l = 0; l < nWaveLengths; l++) {
                wlIndex[l] = l;
            }
            // wlClear is zero-length
        }
        return isModelWLValid;
    }

    @Override
    protected UVFreqTable computeSpatialFreqTable(final double[] sampleWaveLengths) {

        final int nUV = this.oiData.getNbRows();
        final int nRows;

        // From table columns:
        final double[] uCoords;
        final double[] vCoords;

        if (this.oiData instanceof OIVis) {
            nRows = nUV;
            // OI_VIS :
            final OIVis vis = (OIVis) oiData;
            uCoords = vis.getUCoord();
            vCoords = vis.getVCoord();
        } else if (this.oiData instanceof OIVis2) {
            nRows = nUV;
            // OI_VIS2 :
            final OIVis2 vis2 = (OIVis2) oiData;
            uCoords = vis2.getUCoord();
            vCoords = vis2.getVCoord();
        } else if (this.oiData instanceof OIT3) {
            nRows = nUV * 3;
            // Create an interlaced UV point array (U1/V1, U2/V2, U3/V3):
            uCoords = new double[nRows];
            vCoords = new double[nRows];
            // OI_T3 :
            final OIT3 t3 = (OIT3) oiData;
            final double[] u1Coords = t3.getU1Coord();
            final double[] v1Coords = t3.getV1Coord();
            final double[] u2Coords = t3.getU2Coord();
            final double[] v2Coords = t3.getV2Coord();

            for (int i = 0, k; i < nUV; i++) {
                k = 3 * i;
                // U1/V1
                uCoords[k] = u1Coords[i];
                vCoords[k] = v1Coords[i];
                // U2/V2
                uCoords[k + 1] = u2Coords[i];
                vCoords[k + 1] = v2Coords[i];
                // U3/V3 = (U1+U2) (V1+V2)
                uCoords[k + 2] = u1Coords[i] + u2Coords[i];
                vCoords[k + 2] = v1Coords[i] + v2Coords[i];
            }
        } else if (this.oiData instanceof OIFlux) {
            // ignore:
            return null;
        } else {
            logger.info("computeSpatialFreqTable: Unsupported table: {}", oiData);
            return null;
        }

        final int nWLen = sampleWaveLengths.length;

        final int nBl = 0; // TODO: guess nBL

        final UVFreqTable table = new UVFreqTable(nBl, nRows, nWLen);

        // Allocate data array for spatial frequencies:
        final double[][] ufreq = table.ufreq;
        final double[][] vfreq = table.vfreq;

        // index of the baseline:
        final int[] blIdx = table.blIdx; // set to -1

        // index of the observation point (time):
        final int[] ptIdx = table.ptIdx; // set to -1

        // new block to limit variable scope:
        // Inverse wavelength:
        final double[] invWaveLengths = new double[nWLen];
        for (int l = 0; l < nWLen; l++) {
            invWaveLengths[l] = 1d / sampleWaveLengths[l];
        }

        // Iterate on rows :
        for (int k = 0, l; k < nRows; k++) {
            // define baseline index:
            blIdx[k] = -1;

            // define observation point index:
            ptIdx[k] = -1;

            // UV coords (m) :
            // Note: for T3 = alternate between U1, U2, U3=U1+U2 (T3)
            final double u = uCoords[k];
            final double v = vCoords[k];

            // u/v freqs ...
            final double[] uRow = ufreq[k];
            final double[] vRow = vfreq[k];

            // prepare spatial frequencies :
            for (l = 0; l < nWLen; l++) {
                uRow[l] = u * invWaveLengths[l];
                vRow[l] = v * invWaveLengths[l];
            }
        }
        return table;
    }

    /**
     * Update the given OI_VIS table using internal computed visComplex data
     * @param vis OI_VIS table to update
     */
    private void computeOIVis(final OIVis vis) {
        final long start = System.nanoTime();

        final int nRows = vis.getNbRows();
        final String insName = vis.getInsName();

        // test if the instrument is AMBER to use dedicated diffVis algorithm :
        final boolean isAmber = insName.startsWith(AsproConstants.INS_AMBER);

        // Update the data model before calling new OIVis():
        DataModel.setOiVisComplexSupport(false);

        // Columns :
        final double[][] visAmp = vis.getVisAmp();
        final double[][] visAmpErr = vis.getVisAmpErr();

        final double[][] visPhi = vis.getVisPhi();
        final double[][] visPhiErr = vis.getVisPhiErr();

        final boolean[][] flags = vis.getFlag();

        final Complex[][] visComplex = dataTable.visComplex;
        final double[][] visAmpError = dataTable.visAmpError;
        final ComplexDistribution[] visRndDist = dataTable.visRndDist;

        final int nWaveLengths = this.waveLengths.length;

        // generate correlated fluxes (VISDATA):
        // TODO: refine if OIVIS has OIFITS2 (VISAMPTYPE / VISPHITYPE) keywords ?
        final boolean useVisDiff = isAmber
                || insName.startsWith(AsproConstants.INS_GRAVITY)
                || AsproConstants.MATCHER_MATISSE.match(insName);

        if (this.hasModel && !isAmber) {
            logger.info("createOIVis: {} VisAmp/Phi", useVisDiff ? "Differential" : "Absolute");
        }

        // vars:
        double vamp, vphi, vampTh, vphiTh, errAmp, errPhi;

        // try resampling:
        double re, im, diff;
        boolean doFlag;

        // Use mutable complex carefully:
        final double normFactorWL = 1.0 / (nWaveLengths - 1);

        final MutableComplex cpxVisSum = new MutableComplex();
        final MutableComplex cpxVisRef = new MutableComplex();
        final MutableComplex cpxVisDiff = new MutableComplex();

        int chi2_amp_nb = 0;
        int chi2_phi_nb = 0;
        double chi2_amp_sum = 0.0;
        double chi2_phi_sum = 0.0;

        // Iterate on rows :
        for (int k = 0, l, m; k < nRows; k++) {
            // Next block makes easier comparison with OIFitsCreatorService (ref)
            {
                // if target has a model, then complex visibility are computed :
                if (!hasModel) {
                    // Invalid => NaN value :

                    // Iterate on wave lengths :
                    for (l = 0; l < nWaveLengths; l++) {
                        m = wlIndex[l];
                        visAmp[k][m] = Double.NaN;
                        visAmpErr[k][m] = Double.NaN;

                        visPhi[k][m] = Double.NaN;
                        visPhiErr[k][m] = Double.NaN;

                        // mark this value as invalid :
                        flags[k][m] = true;
                    }

                } else {

                    if (!isAmber && useVisDiff) {
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
                        m = wlIndex[l];
                        doFlag = flags[k][m]; // real data flag

                        if (!isAmber) {
                            {
                                {
                                    if (useVisDiff) {
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
                                }
                                // original data:
                                vampTh = visAmp[k][m];
                                vphiTh = FastMath.toRadians(visPhi[k][m]);

                                if (!doFlag) {
                                    // chi2 = sum ( (x - x_th) / err ) ^2
                                    diff = (vamp - vampTh) / visAmpErr[k][m];
                                    if (!Double.isNaN(diff)) {
                                        chi2_amp_nb++;
                                        chi2_amp_sum += diff * diff;
                                    }
                                    diff = distanceAngle(vphi, vphiTh) / FastMath.toRadians(visPhiErr[k][m]);
                                    if (!Double.isNaN(diff)) {
                                        chi2_phi_nb++;
                                        chi2_phi_sum += diff * diff;
                                    }
                                }

                                // Set values:
                                visAmp[k][m] = vamp;
                                visAmpErr[k][m] = errAmp;

                                // convert errPhi in degrees :
                                visPhi[k][m] = toDegrees(vphi);
                                visPhiErr[k][m] = toDegrees(errPhi);
                            }
                        }
                        // Anyway: Disable flag (pure theoretical model):
                        flags[k][m] = false;
                    }
                }
                // Clear:
                for (l = 0; l < wlClear.length; l++) {
                    m = wlClear[l];
                    visAmp[k][m] = Double.NaN;
                    visAmpErr[k][m] = Double.NaN;

                    visPhi[k][m] = Double.NaN;
                    visPhiErr[k][m] = Double.NaN;

                    // mark this value as invalid :
                    flags[k][m] = true;
                }
            }
        }

        if (hasModel) {
            /* Compute visAmp / visPhi as amber does */
            if (isAmber && false) {
                // note: distributions are needed (disabled for now)
                OIFitsAMBERService.amdlibFakeAmberDiffVis(vis, visComplex, visAmpError, nWaveLengths, visRndDist);
                // TODO: generate noisy samples if doNoise
            } else if (!isAmber && useVisDiff) {
                double vamp_sum;
                /* Normalize differential visibilities to 1 */
                for (int k = 0, l, m; k < visAmp.length; k++) {
                    vamp_sum = 0.0;

                    for (l = 0; l < nWaveLengths; l++) {
                        m = wlIndex[l];
                        vamp_sum += visAmp[k][m];
                    }
                    vamp_sum /= nWaveLengths; // mean

                    for (l = 0; l < nWaveLengths; l++) {
                        m = wlIndex[l];
                        visAmp[k][m] /= vamp_sum;
                        visAmpErr[k][m] /= vamp_sum;
                    }
                }
            }
        }

        // Chi2:
        if (chi2_amp_nb != 0) {
            final double chi2_amp_red = chi2_amp_sum / chi2_amp_nb; // degrees of freedom = ??
            logger.info("VISAMP: chi2 = " + chi2_amp_sum + " (n = " + chi2_amp_nb + "): red_chi2: " + chi2_amp_red);
        }
        if (chi2_phi_nb != 0) {
            final double chi2_phi_red = chi2_phi_sum / chi2_phi_nb; // degrees of freedom = ??
            logger.info("VISPHI: chi2 = " + chi2_phi_sum + " (n = " + chi2_phi_nb + "): red_chi2: " + chi2_phi_red);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("computeOIVis: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        }
    }

    /**
     * Update the given OI_VIS2 table using internal computed visComplex data
     * @param vis2 OI_VIS2 table to update
     */
    private void computeOIVis2(final OIVis2 vis2) {
        final long start = System.nanoTime();

        final int nRows = vis2.getNbRows();

        // Create OI_VIS2 table :
        final double[][] vis2Data = vis2.getVis2Data();
        final double[][] vis2Err = vis2.getVis2Err();

        final boolean[][] flags = vis2.getFlag();

        final Complex[][] visComplex = dataTable.visComplex;

        // vars:
        double visRe, visIm;
        double v2Th, v2, v2Err;
        double diff;
        boolean doFlag;

        final int nWaveLengths = this.waveLengths.length;

        int chi2_nb = 0;
        double chi2_sum = 0.0;

        // Iterate on rows :
        for (int k = 0, l, m; k < nRows; k++) {
            // Next block makes easier comparison with OIFitsCreatorService (ref)
            {
                // if target has models, then complex visibility are computed :
                if (!this.hasModel) {
                    // Iterate on wave lengths :
                    for (l = 0; l < nWaveLengths; l++) {
                        m = wlIndex[l];
                        vis2Data[k][m] = Double.NaN;
                        vis2Err[k][m] = Double.NaN;

                        // mark this value as invalid :
                        flags[k][m] = true;
                    }
                } else {
                    // Iterate on wave lengths :
                    for (l = 0; l < nWaveLengths; l++) {
                        m = wlIndex[l];
                        // pure complex visibility data :
                        visRe = visComplex[k][l].getReal();
                        visIm = visComplex[k][l].getImaginary();

                        // pure square visibility :
                        v2 = visRe * visRe + visIm * visIm;
                        v2Err = Double.NaN;

                        doFlag = flags[k][m]; // real data flag

                        // original data:
                        v2Th = vis2Data[k][m];

                        if (!doFlag) {
                            // chi2 = sum ( (x - x_th) / err ) ^2
                            diff = (v2 - v2Th) / vis2Err[k][m];
                            if (!Double.isNaN(diff)) {
                                chi2_nb++;
                                chi2_sum += diff * diff;
                            }
                        }

                        // Set values:
                        vis2Data[k][m] = v2;
                        vis2Err[k][m] = v2Err;

                        // Anyway: Disable flag (pure theoretical model):
                        flags[k][m] = false;
                    }
                }
                // Clear:
                for (l = 0; l < wlClear.length; l++) {
                    m = wlClear[l];
                    vis2Data[k][m] = Double.NaN;
                    vis2Err[k][m] = Double.NaN;

                    // mark this value as invalid :
                    flags[k][m] = true;
                }
            }
        }

        // Chi2:
        if (chi2_nb != 0) {
            final double chi2_red = chi2_sum / chi2_nb; // degrees of freedom = ??
            logger.info("VIS2: chi2 = " + chi2_sum + " (n = " + chi2_nb + "): red_chi2: " + chi2_red);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("computeOIVis2: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        }
    }

    /**
     * Update the given OI_T3 table using internal computed visComplex data
     * @param t3 OI_T3 table to update
     */
    private void computeOIT3(final OIT3 t3) {
        final long start = System.nanoTime();

        final int nRows = t3.getNbRows();

        // OI_T3 Columns :
        final double[][] t3Amp = t3.getT3Amp();
        final double[][] t3AmpErr = t3.getT3AmpErr();

        final double[][] t3Phi = t3.getT3Phi();
        final double[][] t3PhiErr = t3.getT3PhiErr();

        final boolean[][] flags = t3.getFlag();

        final Complex[][] visComplex = dataTable.visComplex;

        // The following code use some hypothesis on the OI_VIS table as defined in createOIVis()
        // 1 - the number of rows per HA point corresponds to the number of baselines.
        // 2 - OI_VIS rows have the same ordering than the list of baselines per HA points.
        // vars :
        Complex cvis12, cvis23, cvis13;
        Complex[] visData12, visData23, visData13;
        double t3amp, t3phi, t3ampTh, t3phiTh, errAmp, errPhi;

        // temporary mutable complex:
        final int[] relPos = new int[]{0, 1, 2}; // fixed relative positions in the UV Freq table
        int pos;

        boolean doFlag;

        double visRe12, visIm12;
        double visRe23, visIm23;
        double visRe31, visIm31;

        double t3Re, t3Im, diff;

        final int nWaveLengths = this.waveLengths.length;

        int chi2_amp_nb = 0;
        int chi2_phi_nb = 0;
        double chi2_amp_sum = 0.0;
        double chi2_phi_sum = 0.0;

        // Iterate on rows :
        for (int k = 0, vp, l, m; k < nRows; k++) {

            // position in the row group :
            vp = 3 * k;

            // Next block makes easier comparison with OIFitsCreatorService (ref)
            {
                // Use relative positions to get the 3 complex vectors (AB, BC, AC)
                // relPos = triplet.getRelativePosition();

                // Find baseline AB = 12 :
                pos = relPos[0];

                // pure complex visibility data :
                visData12 = (this.hasModel) ? visComplex[vp + pos] : null;

                // Find baseline BC = 23 :
                pos = relPos[1];

                // pure complex visibility data :
                visData23 = (this.hasModel) ? visComplex[vp + pos] : null;

                // Find baseline AC = 13 :
                pos = relPos[2];

                // pure complex visibility data :
                visData13 = (this.hasModel) ? visComplex[vp + pos] : null;

                // if target has models, then complex visibility are computed :
                if (!this.hasModel) {

                    // Iterate on wave lengths :
                    for (l = 0; l < nWaveLengths; l++) {
                        m = wlIndex[l];
                        t3Amp[k][m] = Double.NaN;
                        t3AmpErr[k][m] = Double.NaN;

                        t3Phi[k][m] = Double.NaN;
                        t3PhiErr[k][m] = Double.NaN;

                        // mark this value as invalid :
                        flags[k][m] = true;
                    }

                } else {
                    // Iterate on wave lengths :
                    for (l = 0; l < nWaveLengths; l++) {
                        m = wlIndex[l];
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
                        t3phi = toAngle(t3Re, t3Im);

                        doFlag = flags[k][m]; // real data flag

                        errAmp = errPhi = Double.NaN;

                        // original data:
                        t3ampTh = t3Amp[k][m];
                        t3phiTh = FastMath.toRadians(t3Phi[k][m]);

                        if (!doFlag) {
                            // chi2 = sum ( (x - x_th) / err ) ^2
                            diff = (t3amp - t3ampTh) / t3AmpErr[k][m];
                            if (!Double.isNaN(diff)) {
                                chi2_amp_nb++;
                                chi2_amp_sum += diff * diff;
                            }
                            diff = distanceAngle(t3phi, t3phiTh) / FastMath.toRadians(t3PhiErr[k][m]);
                            if (!Double.isNaN(diff)) {
                                chi2_phi_nb++;
                                chi2_phi_sum += diff * diff;
                            }
                        }

                        // Set values:
                        t3Amp[k][m] = t3amp;
                        t3AmpErr[k][m] = errAmp;

                        // convert errPhi in degrees :
                        t3Phi[k][m] = toDegrees(t3phi);
                        t3PhiErr[k][m] = toDegrees(errPhi);

                        // Anyway: Disable flag (pure theoretical model):
                        flags[k][m] = false;
                    }
                }
                // Clear:
                for (l = 0; l < wlClear.length; l++) {
                    m = wlClear[l];
                    t3Amp[k][m] = Double.NaN;
                    t3AmpErr[k][m] = Double.NaN;

                    t3Phi[k][m] = Double.NaN;
                    t3PhiErr[k][m] = Double.NaN;

                    // mark this value as invalid :
                    flags[k][m] = true;
                }
            }
        }

        // Chi2:
        if (chi2_amp_nb != 0) {
            final double chi2_amp_red = chi2_amp_sum / chi2_amp_nb; // degrees of freedom = ??
            logger.info("T3AMP: chi2 = " + chi2_amp_sum + " (n = " + chi2_amp_nb + "): red_chi2: " + chi2_amp_red);
        }
        if (chi2_phi_nb != 0) {
            final double chi2_phi_red = chi2_phi_sum / chi2_phi_nb; // degrees of freedom = ??
            logger.info("T3PHI: chi2 = " + chi2_phi_sum + " (n = " + chi2_phi_nb + "): red_chi2: " + chi2_phi_red);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("computeOIT3: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        }
    }

    /**
     * Update the OI_FLUX table using internal computed visComplex data
     * @param flux OI_FLUX table to update
     */
    private void computeOIFlux(final OIFlux flux) {
        final long start = System.nanoTime();

        final int nRows = flux.getNbRows();

        final int nWaveLengths = this.waveLengths.length;

        // Columns :
        final double[][] fluxData = flux.getFluxData();
        final double[][] fluxErr = flux.getFluxErr();
        final boolean[][] flags = flux.getFlag();

        // Iterate on rows :
        for (int k = 0, l, m; k < nRows; k++) {
            // Next block makes easier comparison with OIFitsCreatorService (ref)
            {
                // Impossible to compute flux without any target:
                {
                    // Invalid => NaN value :

                    // Iterate on wave lengths :
                    for (l = 0; l < nWaveLengths; l++) {
                        m = wlIndex[l];
                        fluxData[k][m] = Double.NaN;
                        fluxErr[k][m] = Double.NaN;

                        // mark this value as invalid :
                        flags[k][m] = true;
                    }
                }
                // Clear:
                for (l = 0; l < wlClear.length; l++) {
                    m = wlClear[l];
                    fluxData[k][m] = Double.NaN;
                    fluxErr[k][m] = Double.NaN;

                    // mark this value as invalid :
                    flags[k][m] = true;
                }
            }
        }

        if (logger.isDebugEnabled()) {
            logger.debug("computeOIFlux: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        }
    }

}
