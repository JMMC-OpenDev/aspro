/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.WarningContainer;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.util.StatUtils;
import fr.jmmc.aspro.util.StatUtils.ComplexDistribution;
import static fr.jmmc.aspro.util.StatUtils.N_SAMPLES;
import static fr.jmmc.aspro.util.StatUtils.SAMPLING_FACTOR_MEAN;
import static fr.jmmc.aspro.util.StatUtils.SAMPLING_FACTOR_VARIANCE;
import fr.jmmc.jmal.complex.Complex;
import fr.jmmc.jmal.complex.ImmutableComplex;
import fr.jmmc.jmcs.util.SpecialChars;
import fr.jmmc.oitools.model.OIData;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIT3;
import fr.jmmc.oitools.model.OIVis;
import fr.jmmc.oitools.model.OIVis2;
import fr.jmmc.oitools.model.OIWavelength;
import static java.lang.Math.PI;
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
    private OIData oiData = null;

    /**
     * 
     * @param target
     * @param supersamplingOIFits
     * @param mathModeOIFits
     * @param oifitsFile
     */
    public OIFitsProcessService(final Target target,
                                final int supersamplingOIFits,
                                final UserModelService.MathMode mathModeOIFits,
                                final OIFitsFile oifitsFile) {

        super(target, supersamplingOIFits, mathModeOIFits);

        this.oiFitsFile = oifitsFile;

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

            logger.info("processing insName: {}", insName);

            // Prepare with OI_WAVELENGTH:
            // note: may adjust wavelengths:
            if (!prepare(oiWaveLength, warningContainer)) {
                return false;
            }

            for (OIData table : this.oiFitsFile.getOiDataList()) {
                if (table.getOiWavelength() == oiWaveLength) {
                    this.oiData = table;

                    // Compute complex visibilities:
                    if (this.computeModelVisibilities()) {
                        // use complex visibilities
                        computeOIData();

                        // fast interrupt :
                        if (Thread.currentThread().isInterrupted()) {
                            return false;
                        }
                    }
                }
            }
        }
        return true;
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

        final String instrumentName = oiWaveLength.getInsName();

        if (logger.isDebugEnabled()) {
            logger.debug("instrumentName: {}", instrumentName);
        }

        // Get wavelength range for the selected instrument mode :
        final double lambdaMin = AsproConstants.MICRO_METER * oiWaveLength.getEffBandMin();
        final double lambdaMax = AsproConstants.MICRO_METER * oiWaveLength.getEffBandMax();

        // TODO: handle properly spectral channels (rebinning):
        int nWaveLengths = oiWaveLength.getNWave();

        if (logger.isDebugEnabled()) {
            logger.debug("lambdaMin: {}", lambdaMin);
            logger.debug("lambdaMax: {}", lambdaMax);
            logger.debug("nChannels: {}", nWaveLengths);
        }

        // prepare wavelengths independently of the user model Fits cube (wavelengths):
        final double waveBand;

        this.waveLengths = oiWaveLength.getEffWaveAsDouble();
        this.waveBands = convertArray(oiWaveLength.getEffBand());
        waveBand = StatUtils.mean(this.waveBands);

        // Initial Wavelength information:
        String firstChannel = Double.toString(convertWL(this.waveLengths[0]));
        String lastChannel = (nWaveLengths > 1) ? Double.toString(convertWL(this.waveLengths[nWaveLengths - 1])) : null;

        addInformation(warningContainer, instrumentName + " instrument mode: "
                + nWaveLengths + " channels "
                + '[' + firstChannel + ((lastChannel != null) ? (" - " + lastChannel) : "") + " " + SpecialChars.UNIT_MICRO_METER + "] "
                + "(band: " + convertWL(waveBand) + " " + SpecialChars.UNIT_MICRO_METER + ')');

        // keep number of channels:
        final int nChannels = nWaveLengths;

        // Keep only spectral channels where user model is defined:
        // note: Instrument spectral channels (waveLengths, waveBand, lambdaMin, lambdaMax) can be modified by this method:
        final boolean isModelWLValid = prepareUserModel(warningContainer);

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

        } else {
            logger.info("computeSpatialFreqTable: Unsupported table: {}", oiData);
            return null;
        }

        final int nWLen = sampleWaveLengths.length;

        final int nBl = 0; // TODO: guess nBL

        final UVFreqTable freqTable = new UVFreqTable(nBl, nRows, nWLen);

        // Allocate data array for spatial frequencies:
        final double[][] ufreq = freqTable.ufreq;
        final double[][] vfreq = freqTable.vfreq;

        // index of the observation point (HA):
        final int[] ptIdx = freqTable.ptIdx; // TODO: FILL properly

        // new block to limit variable scope:
        // Inverse wavelength:
        final double[] invWaveLengths = new double[nWLen];
        for (int l = 0; l < nWLen; l++) {
            invWaveLengths[l] = 1d / sampleWaveLengths[l];
        }

        // Iterate on rows :
        for (int k = 0, l; k < nRows; k++) {

            // define point index:
//                ptIdx[k] = i;
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
        return freqTable;
    }

    /**
     * Update the given OI_VIS table using internal computed visComplex data
     * @param vis OI_VIS table to update
     */
    private void computeOIVis(final OIVis vis) {
        final long start = System.nanoTime();

        final int nRows = vis.getNbRows();

        // test if the instrument is AMBER to use dedicated diffVis algorithm :
        final boolean isAmber = vis.getArrName().startsWith(AsproConstants.INS_AMBER);

        // Columns :
        final float[][][] visData = vis.getVisData();
        final float[][][] visErr = vis.getVisErr();

//TODO: adjust flag for AMBER or GRAVITY ?
        final boolean useVisData = (visData != null && visErr != null);

        final double[][] visAmp = vis.getVisAmp();
        final double[][] visAmpErr = vis.getVisAmpErr();

        final double[][] visPhi = vis.getVisPhi();
        final double[][] visPhiErr = vis.getVisPhiErr();

        final boolean[][] flags = vis.getFlag();

        final int nWaveLengths = this.waveLengths.length;

        // complex visiblity with noise (sigma = visError) used by AMBER:
        final Complex[][] visComplexNoisy = new Complex[nRows][nWaveLengths];

        final NoiseService ns = this.noiseService;

        final boolean instrumentExperimental = true; // TODO: adjust

        if (this.hasModel && (ns != null) && !isAmber && instrumentExperimental) {
            logger.info("createOIVis: Experimental instrument: VisAmp/Phi errors computed using {} random complex visiblities", N_SAMPLES);
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
        double s_vamp_mean, s_vamp_err;
        double vphi_sum_cos, vphi_sum_sin;
        double vphi_sum_diff, vphi_sum_diff_square;
        double s_vphi_mean, s_vphi_err;

        final double[] vamp_samples = new double[N_SAMPLES];
        final double[] vphi_samples = new double[N_SAMPLES];
        int[] visRndIdxRow = null;
        boolean doFlag;

        // Iterate on rows :
        for (int k = 0, l, n; k < nRows; k++) {
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
//                        flags[k][l] = true;
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
                    // Get pure complex visibility:
                    visComplexNoisy[k][l] = visComplex[k][l];

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
                            visComplexNoisy[k][l] = new ImmutableComplex(
                                    visRe + (visErrCplx * distRe[visRndIdxRow[l]]),
                                    visIm + (visErrCplx * distIm[visRndIdxRow[l]])
                            ); // immutable complex for safety
                        }

                        if (useVisData) {
                            /*                            
                            // pure correlated fluxes or NaN:
                            flux = ns.computeCorrelatedFluxWeight(i, l);

                            // store pure (0..1) or noisy correlated fluxes (NaN if no flux):
                            visData[k][l][0] = (float) (flux * visComplexNoisy[k][l].getReal());
                            visData[k][l][1] = (float) (flux * visComplexNoisy[k][l].getImaginary());

                            // error on correlated fluxes :
                            visErr[k][l][0] = (float) (flux * visErrCplx);
                            visErr[k][l][1] = (float) (flux * visErrCplx);
                             */
                        }
                    }

                    if (!isAmber) {
                        if (instrumentExperimental) {

                            // For experimental instruments: VisAmp/Phi are only amplitude and phase of complex visibility:
                            if (ns == null) {
                                vamp = visComplex[k][l].abs();
                                vphi = visComplex[k][l].getArgument();

                                errAmp = errPhi = Double.NaN;

                            } else {
                                // pure complex visibility data :
                                visRe = visComplex[k][l].getReal();
                                visIm = visComplex[k][l].getImaginary();

                                // complex visibility error : visErrRe = visErrIm = visAmpErr or Complex.NaN :
                                visErrCplx = visError[k][l];

                                // pure visibility amplitude:
                                vamp = Math.sqrt(visRe * visRe + visIm * visIm); // TODO: remove bias

                                // note: if (V2 - bias) is negative then vamp must be NaN (no error)
                                // pure visibility phase:
                                vphi = (visIm != 0.0) ? FastMath.atan2(visIm, visRe) : 0.0;

                                // Sampling complex visibilities:
                                vamp_sum = vamp_sum_diff = vamp_sum_diff_square = 0.0;
                                vphi_sum_cos = vphi_sum_sin = 0.0;

                                // bivariate distribution (complex normal):
                                for (n = 0; n < N_SAMPLES; n++) {
                                    // update nth sample:
                                    re = visRe + (visErrCplx * distRe[n]);
                                    im = visIm + (visErrCplx * distIm[n]);

                                    // amplitude:
                                    sample = Math.sqrt(re * re + im * im);
                                    vamp_samples[n] = sample;

                                    // Compensated-summation variant for better numeric precision:
                                    vamp_sum += sample;
                                    diff = sample - vamp;
                                    vamp_sum_diff += diff;
                                    vamp_sum_diff_square += diff * diff;

                                    // mean angle: compute sum(cos) and sum(sin):
                                    // note: do not check div 0 (should never happen as distrib != pure 0.0):
                                    vphi_sum_cos += re / sample;
                                    vphi_sum_sin += im / sample;

                                    // phase in [-PI; PI]:
                                    sample = (im != 0.0) ? FastMath.atan2(im, re) : 0.0;
                                    vphi_samples[n] = sample;
                                }

                                // mean(vamp):
                                s_vamp_mean = SAMPLING_FACTOR_MEAN * vamp_sum;

                                // error(vamp):
                                // note: this algorithm ensures correctness (stable) even if the mean used in diff is wrong !
                                s_vamp_err = Math.sqrt(
                                        SAMPLING_FACTOR_VARIANCE * (vamp_sum_diff_square - (SAMPLING_FACTOR_MEAN * (vamp_sum_diff * vamp_sum_diff)))
                                );

                                // mean(vphi):
                                s_vphi_mean = (vphi_sum_sin != 0.0) ? FastMath.atan2(vphi_sum_sin, vphi_sum_cos) : 0.0;

                                vphi_sum_diff = vphi_sum_diff_square = 0.0;

                                // compute angle variance:
                                for (n = 0; n < N_SAMPLES; n++) {
                                    sample = vphi_samples[n];

                                    // Compensated-summation variant for better numeric precision:
                                    // check if diff is [-PI; PI]:
                                    diff = distance(sample, s_vphi_mean);
                                    vphi_sum_diff += diff;
                                    vphi_sum_diff_square += diff * diff;
                                }

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

                                if (NoiseService.USE_DISTRIB_APPROACH) {
                                    // check SNR:
                                    // PI / 8  = 22.5 deg:
                                    if ((s_vphi_err > (Math.PI * 0.125)) && (Math.abs(s_vphi_mean / s_vphi_err) < SNR_THRESHOLD)) {
                                        doFlag = true;
                                    }
                                }

                                if (this.doNoise) {
                                    // Use the corresponding sample among vis, vis2, t3 on any baselines !
                                    final int nSample = visRndIdxRow[l];
                                    vamp = vamp_samples[nSample];
                                    vphi = vphi_samples[nSample];
                                } else if (DO_USE_SAMPLED_MEAN) {
                                    vamp = s_vamp_mean;
                                    vphi = s_vphi_mean;
                                }
                                errAmp = s_vamp_err;
                                errPhi = s_vphi_err;
                            }

                            // Set values:
                            visAmp[k][l] = vamp;
                            visAmpErr[k][l] = errAmp;

                            // toShort errPhi in degrees :
                            visPhi[k][l] = FastMath.toDegrees(vphi);
                            visPhiErr[k][l] = FastMath.toDegrees(errPhi);

                        } else {
                            // Waiting for explanations on every instrument processing to compute VisAmp/Phi:
                            // following values are considered as invalid :
                            visAmp[k][l] = Double.NaN;
                            visAmpErr[k][l] = Double.NaN;

                            visPhi[k][l] = Double.NaN;
                            visPhiErr[k][l] = Double.NaN;
                        }
                    }

                    // mark this value as valid only if error is valid and SNR is OK:
//                        flags[k][l] = doFlag;
// Disable flag:
                    flags[k][l] = false;
                }
            }
        }

        /* Compute visAmp / visPhi as amber does */
        if (isAmber && this.hasModel && (ns != null)) {
            // Should perform that algorithm using sampling ?
            OIFitsAMBERService.amdlibFakeAmberDiffVis(vis, visComplexNoisy, this.visError, this.waveLengths);
        }

        if (logger.isDebugEnabled()) {
            logger.debug("computeOIVis: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        }
        logger.info("computeOIVis: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
    }

    /**
     * Update the given OI_VIS2 table using internal computed visComplex data
     * @param vis2 OI_VIS2 table to update
     */
    private void computeOIVis2(final OIVis2 vis2) {
        logger.info("computeOIVis2: {}", oiData);

        final long start = System.nanoTime();

        final int nRows = vis2.getNbRows();

        // Create OI_VIS2 table :
        final double[][] vis2Data = vis2.getVis2Data();
        final double[][] vis2Err = vis2.getVis2Err();

        final boolean[][] flags = vis2.getFlag();

        final NoiseService ns = this.noiseService;

        // vars:
        double visRe, visIm, errCVis, bias;
        double v2, v2Err;

        // distribution samples:
        double[] distRe = null, distIm = null;

        // try resampling:
        double re, im, sample, diff;
        double v2_sum, v2_sum_diff, v2_sum_diff_square;
        double s_v2_mean, s_v2_err;

        final double[] v2_samples = new double[N_SAMPLES];
        int[] visRndIdxRow = null;
        boolean doFlag = false;

        final int nWaveLengths = this.waveLengths.length;

        // Iterate on rows :
        for (int k = 0, l, n; k < nRows; k++) {
            // if target has models, then complex visibility are computed :
            if (!this.hasModel) {

                // Iterate on wave lengths :
                for (l = 0; l < nWaveLengths; l++) {
                    vis2Data[k][l] = Double.NaN;
                    vis2Err[k][l] = Double.NaN;

                    // mark this value as invalid :
//                    flags[k][l] = true;
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
                    v2Err = Double.NaN;
//                    doFlag = visSnrFlag[k][l];

                    if (ns != null) {
                        // Sampling complex visibilities:

                        // complex visibility error : visErrRe = visErrIm = visAmpErr or Complex.NaN :
                        errCVis = visError[k][l];

                        if (this.doNoise) {
                            /*                            
                            bias = ns.computeVis2Bias(i, l);
                            // Remove v2 bias:
                            v2 -= bias;
                             */
// TODO:
                            bias = 0.0;
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

                            // compute unbiased V2 = re^2 + im^2 - bias:
                            sample = re * re + im * im - bias;
                            v2_samples[n] = sample;

                            // Compensated-summation variant for better numeric precision:
                            v2_sum += sample;
                            diff = sample - v2;
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
                            /*                            
                            // square visibility error :
                            v2Err = ns.computeVis2Error(i, l, Math.sqrt(v2));
                             */
                            logger.info("Sampling[" + N_SAMPLES + "] snr=" + (s_v2_mean / s_v2_err) + " V2"
                                    + " (err(re,im)= " + errCVis + ")"
                                    + " avg= " + s_v2_mean + " V2= " + v2 + " ratio: " + (s_v2_mean / v2)
                                    + " stddev= " + s_v2_err + " err(V2)= " + v2Err + " ratio: " + (s_v2_err / v2Err)
                            );
                        }

                        if (NoiseService.USE_DISTRIB_APPROACH) {
                            // check SNR:
                            if ((s_v2_err > 0.1) && Math.abs(s_v2_mean / s_v2_err) < SNR_THRESHOLD) {
                                doFlag = true;
                            }
                        }

                        if (this.doNoise) {
                            // Use the corresponding sample:
                            v2 = v2_samples[visRndIdxRow[l]];
                        } else if (DO_USE_SAMPLED_MEAN) {
                            v2 = s_v2_mean;
                        }
                        v2Err = s_v2_err;
                    }

                    // Set values:
                    vis2Data[k][l] = v2;
                    vis2Err[k][l] = v2Err;

                    // mark this value as valid only if error is valid and SNR is OK:
//                    flags[k][l] = doFlag;
// Disable flag:
                    flags[k][l] = false;
                }
            }
        }

        if (logger.isDebugEnabled()) {
            logger.debug("computeOIVis2: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        }
        logger.info("computeOIVis2: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
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

        final NoiseService ns = this.noiseService;

        // The following code use some hypothesis on the OI_VIS table as defined in createOIVis()
        // 1 - the number of rows per HA point corresponds to the number of baselines.
        // 2 - OI_VIS rows have the same ordering than the list of baselines per HA points.
        // vars :
        Complex cvis;
        Complex[] visData12, visData23, visData13;
        double[] visErr12, visErr23, visErr13;
        ComplexDistribution dist12, dist23, dist13;
        boolean[] visSnrFlag12, visSnrFlag23, visSnrFlag13;
        double t3amp, t3phi, errAmp, errPhi;

        // temporary mutable complex:
        final int[] relPos = new int[]{0, 1, 2}; // fixed relative positions in the UV Freq table
        int pos;

        // try resampling:
        double t3amp_sum, t3amp_sum_diff, t3amp_sum_diff_square;
        double s_t3amp_mean, s_t3amp_err;
        double t3phi_sum_cos, t3phi_sum_sin;
        double t3phi_sum_diff, t3phi_sum_diff_square;
        double s_t3phi_mean, s_t3phi_err;

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

        // Iterate on rows :
        for (int k = 0, vp, l, n; k < nRows; k++) {

            // position in the row group :
            vp = 3 * k;
            /*
            // Use relative positions to get the 3 complex vectors (AB, BC, AC)
            relPos = triplet.getRelativePosition();
             */
            // Find baseline AB = 12 :
            pos = relPos[0];

            // pure complex visibility data :
            visData12 = (this.hasModel) ? this.visComplex[vp + pos] : null;
            visErr12 = (this.hasModel) ? this.visError[vp + pos] : null;
            dist12 = this.visRndDist[vp + pos];
            visSnrFlag12 = this.visSnrFlag[vp + pos];

            // Find baseline BC = 23 :
            pos = relPos[1];

            // pure complex visibility data :
            visData23 = (this.hasModel) ? this.visComplex[vp + pos] : null;
            visErr23 = (this.hasModel) ? this.visError[vp + pos] : null;
            dist23 = this.visRndDist[vp + pos];
            visSnrFlag23 = this.visSnrFlag[vp + pos];

            // Find baseline AC = 13 :
            pos = relPos[2];

            // pure complex visibility data :
            visData13 = (this.hasModel) ? this.visComplex[vp + pos] : null;
            visErr13 = (this.hasModel) ? this.visError[vp + pos] : null;
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
//                    flags[k][l] = true;
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
                    cvis = visData12[l];
                    visRe12 = cvis.getReal();
                    visIm12 = cvis.getImaginary();

                    // baseline BC = 23
                    cvis = visData23[l];
                    visRe23 = cvis.getReal();
                    visIm23 = cvis.getImaginary();

                    // baseline AC = 13 => conjugate for 31 (im = -im)
                    cvis = visData13[l];
                    visRe31 = cvis.getReal();
                    // conjugate for 31 (im = -im)
                    visIm31 = -cvis.getImaginary();

                    // Compute RE/IM bispectrum with C12*C23*~C13 :
//                        ComplexUtils.bispectrum(vis12, vis23, vis31, t3Data);
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
                        // Sampling complex visibilities:
                        // complex visibility errors : visErrRe = visErrIm = visAmpErr or Complex.NaN :
                        visErrCplx12 = visErr12[l];
                        visErrCplx23 = visErr23[l];
                        visErrCplx31 = visErr13[l];

                        t3amp_sum = t3amp_sum_diff = t3amp_sum_diff_square = 0.0;
                        t3phi_sum_cos = t3phi_sum_sin = 0.0;

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
                            //                        ComplexUtils.bispectrum(vis12, vis23, vis31, t3Data);
                            t3Re = sRe12 * sRe23 * sRe31 - sRe12 * sIm23 * sIm31 - sIm12 * sRe23 * sIm31 - sIm12 * sIm23 * sRe31;
                            t3Im = sRe12 * sRe23 * sIm31 + sRe12 * sIm23 * sRe31 + sIm12 * sRe23 * sRe31 - sIm12 * sIm23 * sIm31;

                            // amplitude :
                            sample = Math.sqrt(t3Re * t3Re + t3Im * t3Im);
                            t3amp_samples[n] = sample;

                            // Compensated-summation variant for better numeric precision:
                            t3amp_sum += sample;
                            diff = sample - t3amp;
                            t3amp_sum_diff += diff;
                            t3amp_sum_diff_square += diff * diff;

                            // mean angle: compute sum(cos) and sum(sin):
                            // note: do not check div 0 (should never happen as distrib != pure 0.0):
                            t3phi_sum_cos += t3Re / sample;
                            t3phi_sum_sin += t3Im / sample;

                            // phase in [-PI; PI]:
                            sample = (t3Im != 0.0) ? FastMath.atan2(t3Im, t3Re) : 0.0;
                            t3phi_samples[n] = sample;
                        }

                        // mean(T3amp):
                        s_t3amp_mean = SAMPLING_FACTOR_MEAN * t3amp_sum;

                        // error(t3amp):
                        // note: this algorithm ensures correctness (stable) even if the mean used in diff is wrong !
                        s_t3amp_err = Math.sqrt(
                                SAMPLING_FACTOR_VARIANCE * (t3amp_sum_diff_square - (SAMPLING_FACTOR_MEAN * (t3amp_sum_diff * t3amp_sum_diff)))
                        );

                        // mean(T3phi):
                        s_t3phi_mean = (t3phi_sum_sin != 0.0) ? FastMath.atan2(t3phi_sum_sin, t3phi_sum_cos) : 0.0;

                        t3phi_sum_diff = t3phi_sum_diff_square = 0.0;

                        // compute angle variance:
                        for (n = 0; n < N_SAMPLES; n++) {
                            sample = t3phi_samples[n];

                            // Compensated-summation variant for better numeric precision:
                            // check if diff is [-PI; PI]:
                            diff = distance(sample, s_t3phi_mean);
                            t3phi_sum_diff += diff;
                            t3phi_sum_diff_square += diff * diff;
                        }

                        // error(t3phi):
                        // note: this algorithm ensures correctness (stable) even if the mean used in diff is wrong !
                        s_t3phi_err = Math.sqrt(
                                SAMPLING_FACTOR_VARIANCE * (t3phi_sum_diff_square - (SAMPLING_FACTOR_MEAN * (t3phi_sum_diff * t3phi_sum_diff)))
                        );

                        if (DEBUG) {
                            /*                            
                            // phase closure error (rad) :
                            errPhi = ns.computeT3PhiError(i, l, cvis.abs(), cvis.abs(), cvis.abs());

                            // amplitude error t3AmpErr = t3Amp * t3PhiErr :
                            errAmp = t3amp * errPhi;

                            logger.info("Sampling[" + N_SAMPLES + "] snr=" + (s_t3amp_mean / s_t3amp_err) + " AMP "
                                    + " avg= " + s_t3amp_mean + " T3amp= " + t3amp + " ratio: " + (s_t3amp_mean / t3amp)
                                    + " stddev= " + s_t3amp_err + " errAmp= " + errAmp + " ratio: " + (s_t3amp_err / errAmp)
                            );
                            logger.info("Sampling[" + N_SAMPLES + "] snr=" + (s_t3phi_mean / s_t3phi_err) + " PHI "
                                    + " avg= " + s_t3phi_mean + " T3phi= " + t3phi + " ratio: " + ((PI + s_t3phi_mean) / (PI + t3phi))
                                    + " stddev= " + s_t3phi_err + " errPhi= " + errPhi + " ratio: " + (s_t3phi_err / errPhi)
                            );
                             */
                        }

                        if (NoiseService.USE_DISTRIB_APPROACH) {
                            // check SNR:
                            // PI / 8  = 22.5 deg:
                            if ((s_t3phi_err > (Math.PI * 0.125)) && (Math.abs(s_t3phi_mean / s_t3phi_err) < SNR_THRESHOLD)) {
                                doFlag = true;
                            }
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

                    // Set values:
                    t3Amp[k][l] = t3amp;
                    t3AmpErr[k][l] = errAmp;

                    // toShort errPhi in degrees :
                    t3Phi[k][l] = FastMath.toDegrees(t3phi);
                    t3PhiErr[k][l] = FastMath.toDegrees(errPhi);

                    // mark this value as valid only if error is valid and SNR is OK:
                    //                  flags[k][l] = doFlag;
// Disable flag:
                    flags[k][l] = false;
                }
            }
        }

        if (logger.isDebugEnabled()) {
            logger.debug("computeOIT3: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        }
        logger.info("computeOIT3: duration = {} ms.", 1e-6d * (System.nanoTime() - start));
    }

}
