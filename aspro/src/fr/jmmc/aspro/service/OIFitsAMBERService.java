/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

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
     */
    public static void amdlibFakeAmberDiffVis(final OIVis vis,
            final Complex[][] visComplex,
            final Complex[][] visError,
            final double[] waveLengths) {

        /*
         * Note on amdlib port :
         * In our case (Aspro 1) : nbBases = 1 and nbFrames = vis.getNbRows().
         * => The double loop (iFrame/iBase) is replaced by a single loop (iRow)
         *
         * This simplify both loops and temporary arrays
         *
         * variable mapping :
         * cpxVisTable[(][)][]         =  visComplex[][]
         * wlen[]                      =  waveLengths[]
         */

        // number of rows in OI_VIS table :
        final int nRows = vis.getNbRows();

        /* loop on rows */
        int iRow;

        // number of wavelengths :
        final int nbLVis = waveLengths.length;

        int lVis = 0;

        /* We try to follow the paper's notations */
        final Complex[][] cpxVisTable = visComplex;
        final Complex[][] sigma2_cpxVisTable = new Complex[nRows][nbLVis];

        final Complex[][] cNopTable = new Complex[nRows][nbLVis];
        final Complex[][] w1 = new Complex[nRows][nbLVis];
        final Complex[][] sigma2_w1 = new Complex[nRows][nbLVis];
        final Complex[][] cRefTable = new Complex[nRows][nbLVis];
        final Complex[][] sigma2_cRefTable = new Complex[nRows][nbLVis];

        final double[] opd = new double[nRows];
        final double[] cpxVisVectR = new double[nRows];

        Complex phasor, w1Avg;
        double x;

        // Output in OI_VIS table :
        final double[][] visAmp = vis.getVisAmp();
        final double[][] visAmpErr = vis.getVisAmpErr();

        final double[][] visPhi = vis.getVisPhi();
        final double[][] visPhiErr = vis.getVisPhiErr();

        /*
         * Immediately copy input cpxVis to 3D structure, flagging as BLANK when Flag is present
         *
         * Skipped because :
         * cpxVisTable[(][)][]         =  visComplex[][]
         */

        // Compute sigma2_cpxVis = visError**2 :
        for (iRow = 0; iRow < nRows; iRow++) {

            for (lVis = 0; lVis < nbLVis; lVis++) {

                sigma2_cpxVisTable[iRow][lVis] = new ImmutableComplex(
                        FastMath.pow2(visError[iRow][lVis].getReal()),
                        FastMath.pow2(visError[iRow][lVis].getImaginary())); // immutable complex for safety
            }
        }

        // Aspro 2 : compute fake piston2T now :

        // Port amdlibPiston.c : amdlibComputePiston2T()


        /* Initialize opd: local copy where all bad pistons are Blank:
         * if opd = BLANK, cNopTable[iFrame][iBase] will be BLANK, and the avg
         * and rms done at the end will avoid these values */
        for (iRow = 0; iRow < nRows; iRow++) {
            /* force opd to 0 : was :instantOpdPistonPtr[iRow]; */
            opd[iRow] = 0d;
        }

        /* First, correct coherent flux from achromatic piston phase (eq 2.2)*/
        /* Here the piston is Zero for the moment */
        amdlibCorrect3DVisTableFromAchromaticPiston(cpxVisTable,
                cNopTable,
                nRows,
                nbLVis,
                waveLengths,
                opd);

        // Use mutable complex carefully:
        Complex cpxVis = new MutableComplex();
        Complex sigma2_cpxVis = new MutableComplex();

        final double normFactorWL = 1d / (nbLVis - 1);

        /* Production of the reference channel: mean of all R and I parts
         * of all channels except the one considered ( eq 2.3) */
        for (iRow = 0; iRow < nRows; iRow++) {

            // reset:
            cpxVis.updateComplex(0d, 0d);
            sigma2_cpxVis.updateComplex(0d, 0d);

            /* sum all R and I */
            for (lVis = 0; lVis < nbLVis; lVis++) {
                /* skipped flags
                 if (cpxVisTable[iRow][lVis].re != amdlibBLANKING_VALUE)
                 */
                cpxVis.add(cNopTable[iRow][lVis]);
                sigma2_cpxVis.add(sigma2_cpxVisTable[iRow][lVis]);
            }

            /* then construct Cref by substracting current R and I
             * at that Wlen and make the arithmetic mean */
            for (lVis = 0; lVis < nbLVis; lVis++) {
                cRefTable[iRow][lVis] = new ImmutableComplex(
                        (cpxVis.getReal() - cNopTable[iRow][lVis].getReal()) * normFactorWL,
                        (cpxVis.getImaginary() - cNopTable[iRow][lVis].getImaginary()) * normFactorWL); // immutable complex for safety

                sigma2_cRefTable[iRow][lVis] = new ImmutableComplex(
                        (sigma2_cpxVis.getReal() - sigma2_cpxVisTable[iRow][lVis].getReal()) * normFactorWL,
                        (sigma2_cpxVis.getImaginary() - sigma2_cpxVisTable[iRow][lVis].getImaginary()) * normFactorWL); // immutable complex for safety
            }
        }

        // variables cpxVis and sigma2_cpxVis will then store ImmutableComplex reference below:

        /* Now the interspectrum is cNopTable*~cRefTable. Store in w1. */
        for (iRow = 0; iRow < nRows; iRow++) {

            for (lVis = 0; lVis < nbLVis; lVis++) {
                cpxVis = cNopTable[iRow][lVis];
                phasor = cRefTable[iRow][lVis].conjugate();

                w1[iRow][lVis] = cpxVis.multiply(phasor);

                /* Please have a look to the F. Millour thesis
                 (http://tel.archives-ouvertes.fr/tel-00134268),
                 pp.91-892 (eq. 4.55 to 4.58) */

                sigma2_w1[iRow][lVis] = new ImmutableComplex(
                        sigma2_cpxVisTable[iRow][lVis].getReal()
                        * FastMath.pow2(cRefTable[iRow][lVis].getReal())
                        + sigma2_cRefTable[iRow][lVis].getReal()
                        * FastMath.pow2(cpxVisTable[iRow][lVis].getReal())
                        + sigma2_cpxVisTable[iRow][lVis].getImaginary()
                        * FastMath.pow2(cRefTable[iRow][lVis].getImaginary())
                        + sigma2_cRefTable[iRow][lVis].getImaginary()
                        * FastMath.pow2(cpxVisTable[iRow][lVis].getImaginary()),
                        sigma2_cpxVisTable[iRow][lVis].getImaginary()
                        * FastMath.pow2(cRefTable[iRow][lVis].getReal())
                        + sigma2_cRefTable[iRow][lVis].getImaginary()
                        * FastMath.pow2(cpxVisTable[iRow][lVis].getReal())
                        + sigma2_cpxVisTable[iRow][lVis].getReal()
                        * FastMath.pow2(cRefTable[iRow][lVis].getImaginary())
                        + sigma2_cRefTable[iRow][lVis].getReal()
                        * FastMath.pow2(cpxVisTable[iRow][lVis].getImaginary())); // immutable complex for safety


                /*Here there is no over-frame averaging. Compute 'averaged' values frame by frame */
                w1Avg = w1[iRow][lVis];

                visPhi[iRow][lVis] = FastMath.toDegrees(w1Avg.getArgument());

                visPhiErr[iRow][lVis] = FastMath.toDegrees(amdlibAbacusErrPhi(Math.sqrt(sigma2_w1[iRow][lVis].getReal() + sigma2_w1[iRow][lVis].getImaginary())));

            }
        }

        /* Now for the differential vis, we use cNopTable/cRefTable. Store in w1.
         * note this is not exactly as complicated as in the computeDiff yorick
         * implementation by Millour */
        for (iRow = 0; iRow < nRows; iRow++) {

            for (lVis = 0; lVis < nbLVis; lVis++) {
                cpxVis = cNopTable[iRow][lVis];
                phasor = cRefTable[iRow][lVis];

                w1[iRow][lVis] = cpxVis.divide(phasor);
            }
        }

        /* Compute VisAmp */
        for (iRow = 0; iRow < nRows; iRow++) {

            for (lVis = 0; lVis < nbLVis; lVis++) {

                /* The W1 vector */

                /* see eq 2.8 */
                x = FastMath.toRadians(visPhi[iRow][lVis]);

                cpxVis = w1[iRow][lVis];

                /*Here there is no over-frame averaging. Compute 'averaged' values frame by frame */
                cpxVisVectR[iRow] = FastMath.cos(x) * cpxVis.getReal() + FastMath.sin(x) * cpxVis.getImaginary();

                visAmp[iRow][lVis] = cpxVisVectR[iRow];
                visAmpErr[iRow][lVis] = Math.sqrt(sigma2_w1[iRow][lVis].getImaginary() + sigma2_w1[iRow][lVis].getReal());
            }
        }
    }

    /**
     * Compute piston, iterative phasor method.
     */
    private static void amdlibFakeComputePiston2T(int nbLVis,
            final double[] wlen,
            final Complex[] cpxVisTable,
            final Complex[] s2cpxVisTable,
            final double[] pistonOPD,
            final double[] sigma,
            double wlenAvg,
            double wlenDifAvg,
            double R) {
        // TODO : port amdlibPiston.c
    }

    /**
     * Removes a constant phase (achromatic piston) on a complexVisibility vector.
     * @param cpxVisTable computed complex visibility [row][waveLength] (input)
     * @param cNopTable corrected complex visibility [row][waveLength] (output)
     * @param nRows number of rows in OI_VIS
     * @param nbLVis number of wavelengths
     * @param waveLengths wave lengths
     * @param pst piston / opd array
     */
    private static void amdlibCorrect3DVisTableFromAchromaticPiston(
            final Complex[][] cpxVisTable,
            final Complex[][] cNopTable,
            int nRows,
            int nbLVis,
            final double[] waveLengths,
            final double[] pst) {

        int iRow;
        int lVis;
        double x;
        Complex phasor, cpxVis, cpxVisAvgConj;

        final MutableComplex cpxVisAvg = new MutableComplex();
        final Complex[][] tmpCpxVis = new Complex[nRows][nbLVis];

        final double normFactorWL = 1d / nbLVis;

        for (iRow = 0; iRow < nRows; iRow++) {

            /*
             * skip test as pst = 0 here :
             if( !(pst[iRow]==amdlibBLANKING_VALUE))
             */
            for (lVis = 0; lVis < nbLVis; lVis++) {
                /*Flagged Vis not important here*/

                /* Note older versions artificially removed -1*pst
                 * since the pst sign in amdlib was kept wrong because
                 * the OPD correction at ESO was in the wrong direction.
                 * To use this new version in the OS at Paranal, one must
                 * change the sign of the displacements of, e.g., the Delay
                 * lines that are used in templates.
                 * x = (2 * M_PI/wlen[lVis]) * -1*pst[iFrame][iBase]; */
                if (pst[iRow] == 0d) {
                    phasor = Complex.ONE;
                } else {
                    x = ((2d * Math.PI) / waveLengths[lVis]) * pst[iRow];

                    phasor = new ImmutableComplex(FastMath.cos(x), -FastMath.sin(x)); // immutable complex for safety
                }

                cpxVis = cpxVisTable[iRow][lVis];

                /* Store complex Product of the phasor with the coherent flux
                 in cNopTable*/
                cNopTable[iRow][lVis] = cpxVis.multiply(phasor);
            }

            /* Flag cNopTable where cpxVis was Flagged : skipped flags */

            /* Recenter this by removing the mean, taking account of bad Vis */
            /* Compute the mean over lambda */

            // reset:
            cpxVisAvg.updateComplex(0d, 0d);

            for (lVis = 0; lVis < nbLVis; lVis++) {
                /* skipped flags
                 if (cNopTable[iRow][lVis].re!=amdlibBLANKING_VALUE)
                 */
                cpxVisAvg.add(cNopTable[iRow][lVis]);
            }
            cpxVisAvg.multiply(normFactorWL);

            // store immutable conjugate complex:
            cpxVisAvgConj = new ImmutableComplex(cpxVisAvg.conjugate());

            /* Store conjugate complex values in tmpCpxVis as (constant) vector */
            for (lVis = 0; lVis < nbLVis; lVis++) {
                tmpCpxVis[iRow][lVis] = cpxVisAvgConj;
            }

            /* Remove constant by multiplying the two */
            for (lVis = 0; lVis < nbLVis; lVis++) {
                phasor = tmpCpxVis[iRow][lVis];
                cpxVis = cNopTable[iRow][lVis];

                /* Store Complex Product of the phasor with the coherent flux in cNopTable*/
                cNopTable[iRow][lVis] = cpxVis.multiply(phasor);
            }
            /* re-Flag cNopTable where cpxVis was Flagged : skipped flags */
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
}
