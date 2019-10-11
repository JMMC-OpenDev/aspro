/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.util.StatUtils.ComplexDistribution;
import static fr.jmmc.aspro.util.StatUtils.N_SAMPLES;
import fr.jmmc.jmal.complex.Complex;
import fr.jmmc.jmal.complex.ImmutableComplex;
import fr.jmmc.jmal.complex.MutableComplex;
import fr.jmmc.oitools.model.OIVis;
import net.jafama.FastMath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is dedicated to the AMBER instrument to compute differential visibility and phase
 * @author bourgesl
 */
public final class OIFitsAMBERService {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(OIFitsAMBERService.class.getName());
    /** constant used by abacusErrPhi */
    public final static double ASYMPTOT = Math.PI / Math.sqrt(3d);
    /** polynomial coefficients used by abacusErrPhi */
    private final static double abacusCoeff[] = {
        2.7191808010909d,
        -17.1901043936273d,
        45.0654103760899d,
        -63.4441678243197d,
        52.3098941426378d,
        -25.8090699917488d,
        7.84352873962491d,
        -1.57308595820081d};

    /**
     * Forbidden constructor
     */
    private OIFitsAMBERService() {
        super();
    }

    /**
     * Compute the differential visibility (amplitude / phase) according to AMBER amdlib algorithms
     * @param vis OI_VIS table
     * @param visComplex complex visibility [row][waveLength]
     * @param visError complex visibility error [row][waveLength]
     * @param waveLengths wavelengths
     * @param visRndDist complex normal distribution to sample complex visibilities
     */
    public static void amdlibFakeAmberDiffVis(final OIVis vis,
                                              final Complex[][] visComplex,
                                              final double[][] visError,
                                              final int nbLVis,
                                              final ComplexDistribution[] visRndDist) {

        /*
         * Note on amdlib port :
         * In our case (Aspro 1) : nbBases = 1 and nbFrames = vis.getNbRows().
         * => The double loop (iFrame/iBase) is replaced by a single loop (iRow)
         */
        // number of rows in OI_VIS table :
        final int nRows = vis.getNbRows();

        /* loop on rows */
        int iRow;

        int lVis, n;

        /* We try to follow the paper's notations */
        final Complex[][][] cpxVisTable = new Complex[nRows][nbLVis][N_SAMPLES];

        final Complex[][][] cNopTable = cpxVisTable; // use same input / output
        final Complex[][][] cRefTable = new Complex[nRows][nbLVis][N_SAMPLES];
        final Complex[][][] w1 = new Complex[nRows][nbLVis][N_SAMPLES];

        final double[] cpxVisVectR = new double[N_SAMPLES];
        final double[] cpxVisVectI = new double[N_SAMPLES];
        final double[] phiVect = new double[N_SAMPLES];

        Complex phasor, w1Avg;
        double x, squareModulus, cosPhi, sinPhi;

        // Output in OI_VIS table :
        final double[][] visAmp = vis.getVisAmp();
        final double[][] visAmpErr = vis.getVisAmpErr();

        final double[][] visPhi = vis.getVisPhi();
        final double[][] visPhiErr = vis.getVisPhiErr();

        // distribution samples:
        double[] distRe = null, distIm = null;
        double visRe, visIm, visErrCplx;

        // Compute complex visibility samples:
        for (iRow = 0; iRow < nRows; iRow++) {

            // Get the complex distribution for this row:
            distRe = visRndDist[iRow].getSamples()[0];
            distIm = visRndDist[iRow].getSamples()[1];

            for (lVis = 0; lVis < nbLVis; lVis++) {
                // pure complex visibility data :
                visRe = visComplex[iRow][lVis].getReal();
                visIm = visComplex[iRow][lVis].getImaginary();

                // complex visibility error : visErrRe = visErrIm = visAmpErr or Complex.NaN :
                visErrCplx = visError[iRow][lVis];

                // bivariate distribution (complex normal):
                for (n = 0; n < N_SAMPLES; n++) {
                    cpxVisTable[iRow][lVis][n] = new ImmutableComplex(
                            visRe + (visErrCplx * distRe[n]),
                            visIm + (visErrCplx * distIm[n])
                    ); // immutable complex for safety
                }
            }
        }

        /* First, correct coherent flux from achromatic piston phase (eq 2.2)
         * Here the piston is Zero (perfect piston correction) */
        amdlibCorrect3DVisTableFromAchromaticPiston(cpxVisTable,
                cNopTable,
                nRows,
                nbLVis);

        // Use mutable complex carefully:
        final Complex cpxVis = new MutableComplex();
        final Complex sigma2_cpxVis = new MutableComplex();

        final double normFactorWL = 1d / (nbLVis - 1);

        /* Production of the reference channel: mean of all R and I parts
         * of all channels except the one considered ( eq 2.3) */
        // bivariate distribution (complex normal):
        for (n = 0; n < N_SAMPLES; n++) { // Frame

            for (iRow = 0; iRow < nRows; iRow++) {
                // reset:
                cpxVis.updateComplex(0d, 0d);
                sigma2_cpxVis.updateComplex(0d, 0d);

                /* sum all R and I */
                for (lVis = 0; lVis < nbLVis; lVis++) {
                    cpxVis.add(cNopTable[iRow][lVis][n]);
                }

                /* then construct Cref by substracting current R and I
                 * at that Wlen and make the arithmetic mean */
                for (lVis = 0; lVis < nbLVis; lVis++) {
                    cRefTable[iRow][lVis][n] = new ImmutableComplex(
                            (cpxVis.getReal() - cNopTable[iRow][lVis][n].getReal()) * normFactorWL,
                            (cpxVis.getImaginary() - cNopTable[iRow][lVis][n].getImaginary()) * normFactorWL); // immutable complex for safety
                }
            }
        }

        // variables cpxVis and sigma2_cpxVis will then store ImmutableComplex reference below:

        /* Now the interspectrum is cNopTable*~cRefTable. Store in w1. */
        for (iRow = 0; iRow < nRows; iRow++) {

            for (lVis = 0; lVis < nbLVis; lVis++) {

                // bivariate distribution (complex normal):
                for (n = 0; n < N_SAMPLES; n++) { // Frame
                    phasor = cRefTable[iRow][lVis][n].conjugate();

                    w1[iRow][lVis][n] = cNopTable[iRow][lVis][n].multiply(phasor);

                    /* Compute mean VisPhi as average of selected frames */
                    // bivariate distribution (complex normal):
                    /* The W1 vector */
                    cpxVisVectR[n] = w1[iRow][lVis][n].getReal();
                    cpxVisVectI[n] = w1[iRow][lVis][n].getImaginary();
                }
                /* The Phase Herself :
                 * average re and im */
                w1Avg = new ImmutableComplex(
                        amdlibAvgTable(cpxVisVectR),
                        amdlibAvgTable(cpxVisVectI)
                );

                /* store */
                visPhi[iRow][lVis] = /* ((double) (N_SAMPLES - 1) / (double) N_SAMPLES) */ FastMath.toDegrees(w1Avg.getArgument());

                /* WE USE THE STATISTICAL ERROR FOR BINNING */
                w1Avg = w1Avg.conjugate();

                // bivariate distribution (complex normal):
                for (n = 0; n < N_SAMPLES; n++) {
                    /* add w1*conj(w1Avg) to vector
                     * use phiVect to store phases */
                    phiVect[n] = w1[iRow][lVis][n].multiply(w1Avg).getArgument();
                }

                x = amdlibRmsTable(phiVect);

                /* Err on Phi must be corrected with an abacus*/
                // do not divide by SQRT(N_SAMPLES) as initial distribution correspond to 1 sigma
                visPhiErr[iRow][lVis] = FastMath.toDegrees(amdlibAbacusErrPhi(x));
            }
        }

        // Use latest approach (2013) for VisAmp:
        if (true) {
            /* Amplitude of Differential Visibility (VisAmp)
             * substract averaged phase from each complex vis; store in, e.g., w1 */
            for (iRow = 0; iRow < nRows; iRow++) {

                for (lVis = 0; lVis < nbLVis; lVis++) {
                    /* Buildup a phasor out of the differential phase */
                    x = FastMath.toRadians(visPhi[iRow][lVis]);
                    phasor = new ImmutableComplex(FastMath.cos(x), -FastMath.sin(x)); // immutable complex for safety

                    /* Subtract differential phase phasor from the complex coherent
                       flux to obtain a clean coherent flux w1 */
                    // bivariate distribution (complex normal):
                    for (n = 0; n < N_SAMPLES; n++) {
                        w1[iRow][lVis][n] = cNopTable[iRow][lVis][n].multiply(phasor);
                    }
                }
            }

            /* recompute differential vis on these values that are now in w1 */
            // bivariate distribution (complex normal):
            for (n = 0; n < N_SAMPLES; n++) {

                for (iRow = 0; iRow < nRows; iRow++) {
                    // reset:
                    cpxVis.updateComplex(0d, 0d);

                    /* sum all R and I */
                    for (lVis = 0; lVis < nbLVis; lVis++) {
                        cpxVis.add(w1[iRow][lVis][n]);
                    }

                    /* then construct Cref by substracting current R and I 
                     * at that Wlen and make the arithmetic mean */
                    for (lVis = 0; lVis < nbLVis; lVis++) {
                        cRefTable[iRow][lVis][n] = new ImmutableComplex(
                                (cpxVis.getReal() - w1[iRow][lVis][n].getReal()) * normFactorWL,
                                (cpxVis.getImaginary() - w1[iRow][lVis][n].getImaginary()) * normFactorWL); // immutable complex for safety
                    }
                }
            }

            /* Now the interspectrum is w1*~cRefTable. Store back in w1. */
            for (iRow = 0; iRow < nRows; iRow++) {

                for (lVis = 0; lVis < nbLVis; lVis++) {
                    squareModulus = 0.0;

                    // bivariate distribution (complex normal):
                    for (n = 0; n < N_SAMPLES; n++) {
                        phasor = cRefTable[iRow][lVis][n].conjugate();

                        w1[iRow][lVis][n] = w1[iRow][lVis][n].multiply(phasor);

                        /* Hack to get the differential visibility normalized to 1
                         * compute the squared modulus of cRefTable */
                        squareModulus += phasor.getReal();

                        /* perform mean of R and I of w1 that is now the depistoned interspectrum and compute visamp
                         * populate serie of R and I of w in cpxVisVect*/
                        cpxVisVectR[n] = w1[iRow][lVis][n].getReal();
                    }
                    squareModulus /= N_SAMPLES;

                    x = amdlibAvgTable(cpxVisVectR);

                    /* Store differential visibilities */
                    // suppose pow(p1p2,0.5) = 1 (already normalized in Aspro2)
                    visAmp[iRow][lVis] = x / squareModulus; // / pow(p1p2,0.5)

                    /* variance will be taken as the statistical variance, i use cpxVisVectR to store temporarily the values */
                    // bivariate distribution (complex normal):
                    for (n = 0; n < N_SAMPLES; n++) {
                        cpxVisVectR[n] /= squareModulus;
                    }

                    // do not divide by SQRT(N_SAMPLES) as initial distribution correspond to 1 sigma
                    visAmpErr[iRow][lVis] = amdlibRmsTable(cpxVisVectR);
                }
            }
        } else {
            /* Now for the differential vis, we use cNopTable/cRefTable. Store in w1.
             * note this is not exactly as complicated as in the computeDiff yorick
             * implementation by Millour */
            for (iRow = 0; iRow < nRows; iRow++) {

                for (lVis = 0; lVis < nbLVis; lVis++) {

                    // bivariate distribution (complex normal):
                    for (n = 0; n < N_SAMPLES; n++) {
                        phasor = cRefTable[iRow][lVis][n];

                        w1[iRow][lVis][n] = cNopTable[iRow][lVis][n].divide(phasor);
                    }
                }
            }

            /* Compute mean VisAmp as average of selected frames. */
            for (iRow = 0; iRow < nRows; iRow++) {

                for (lVis = 0; lVis < nbLVis; lVis++) {
                    /* see eq 2.8 */
                    x = FastMath.toRadians(visPhi[iRow][lVis]);
                    cosPhi = FastMath.cos(x);
                    sinPhi = FastMath.sin(x); // sinAndCos ?

                    // bivariate distribution (complex normal):
                    for (n = 0; n < N_SAMPLES; n++) {
                        /* The W1 vector */
                        cpxVisVectR[n] = cosPhi * w1[iRow][lVis][n].getReal() + sinPhi * w1[iRow][lVis][n].getImaginary();
                    }

                    visAmp[iRow][lVis] = amdlibAvgTable(cpxVisVectR);
                    visAmpErr[iRow][lVis] = amdlibRmsTable(cpxVisVectR);
                }
            }
        }

        /* Normalize differential visibilities to 1 */
        for (iRow = 0; iRow < nRows; iRow++) {
            x = 0.0;

            for (lVis = 0; lVis < nbLVis; lVis++) {
                x += visAmp[iRow][lVis];
            }
            x /= nbLVis;

            for (lVis = 0; lVis < nbLVis; lVis++) {
                visAmp[iRow][lVis] /= x;
                visAmpErr[iRow][lVis] /= x;
            }
        }
    }

    /**
     * Removes a constant phase (achromatic piston) on a complexVisibility vector.
     * @param cpxVisTable computed complex visibility [row][waveLength] (input)
     * @param cNopTable corrected complex visibility [row][waveLength] (output)
     * @param nRows number of rows in OI_VIS
     * @param nbLVis number of wavelengths
     */
    private static void amdlibCorrect3DVisTableFromAchromaticPiston(
            final Complex[][][] cpxVisTable,
            final Complex[][][] cNopTable,
            final int nRows,
            final int nbLVis) {

        int iRow, lVis, n;
        Complex phasor, cpxVis, cpxVisAvgConj;

        final MutableComplex cpxVisAvg = new MutableComplex();

        final double normFactorWL = 1d / nbLVis;

        // bivariate distribution (complex normal):
        for (n = 0; n < N_SAMPLES; n++) { // Frame

            for (iRow = 0; iRow < nRows; iRow++) {

                // Piston is 0:
                for (lVis = 0; lVis < nbLVis; lVis++) {

                    cpxVis = cpxVisTable[iRow][lVis][n];
                    phasor = Complex.ONE; // piston = 0

                    /* Store complex Product of the phasor with the coherent flux
                     in cNopTable*/
                    cNopTable[iRow][lVis][n] = cpxVis.multiply(phasor);
                }

                /* Recenter this by removing the mean, taking account of bad Vis
                 * Compute the mean over lambda */
                // reset:
                cpxVisAvg.updateComplex(0d, 0d);

                for (lVis = 0; lVis < nbLVis; lVis++) {
                    cpxVisAvg.add(cNopTable[iRow][lVis][n]);
                }
                cpxVisAvg.multiply(normFactorWL);

                // store immutable conjugate complex:
                cpxVisAvgConj = new ImmutableComplex(cpxVisAvg.conjugate());

                /* Remove constant by multiplying the two */
                for (lVis = 0; lVis < nbLVis; lVis++) {
                    /* Store conjugate complex values in tmpCpxVis as (constant) vector */
                    phasor = cpxVisAvgConj;
                    cpxVis = cNopTable[iRow][lVis][n];

                    /* Store Complex Product of the phasor with the coherent flux in cNopTable */
                    cNopTable[iRow][lVis][n] = cpxVis.multiply(phasor);
                }
                /* re-Flag cNopTable where cpxVis was Flagged : skipped flags */
            }
        }
    }

    /**
     * Estimate true phase rms from the cross-spectrum variance.
     * see Petrov, Roddier and Aime, JOSAA vol 3, N°5, may 1986 p 634.
     * and Petrov's Thesis, p. 50 ff.
     * I replace the piecewise interpolation usually used by a polynomial
     * approximation of the function:
     * if z=log10(y),
     * then z=(C[1]*X^7+C[2]*X^6+C[3]*X^5+C[4]*X^4+C[5]*X^3+C[6]*X^2+C[7]*X+C[8])
     * and y=10^z.
     * where
     * C[01]= 2.71918080109099
     * C[02]=-17.1901043936273
     * C[03]= 45.0654103760899
     * C[04]=-63.4441678243197
     * C[05]= 52.3098941426378
     * C[06]=-25.8090699917488
     * C[07]= 7.84352873962491
     * C[08]=-1.57308595820081
     * This interpolation is valid in the range x=[0.1,1.74413].
     * Error is 1% everywhere except above x=1.73 where it is 10%
     * Below x=0.1: y=x
     * Above x=M_PI/sqrt(3.0), y = blanking (impossible value)
     * Above x=1.74413, we take: y=0.691/(pi/sqrt(3.0)-x)
     * @param x parameter
     * @return phase rms from the cross-spectrum variance
     */
    private static double amdlibAbacusErrPhi(final double x) {

        if (true) {
            return x;
        }

        if (x > ASYMPTOT) {
            return Double.NaN;
        }
        if (x > 1.74413d) {
            return (0.691d / (ASYMPTOT - x));
        }
        if (x < 0.1d) {
            return (x);
        }
        final double x2 = x * x;
        final double x3 = x2 * x;
        final double x4 = x2 * x2;
        final double x5 = x3 * x2;
        final double x6 = x3 * x3;
        final double x7 = x6 * x;

        final double z = abacusCoeff[0] * x7
                + abacusCoeff[1] * x6
                + abacusCoeff[2] * x5
                + abacusCoeff[3] * x4
                + abacusCoeff[4] * x3
                + abacusCoeff[5] * x2
                + abacusCoeff[6] * x
                + abacusCoeff[7];
        return FastMath.pow(10d, z);
    }

    private static double amdlibAvgTable(final double[] table) {
        double avg = 0.0;

        for (int iPix = 0; iPix < table.length; iPix++) {
            avg += table[iPix];
        }
        avg /= table.length;
        return avg;
    }

    private static double amdlibRmsTable(final double[] table) {
        double rms = 0.0;

        final double avg = amdlibAvgTable(table);

        for (int iPix = 0; iPix < table.length; iPix++) {
            rms += FastMath.pow2(table[iPix] - avg);
        }

        rms /= table.length;
        rms = Math.sqrt(rms);

        return rms;
    }
}
