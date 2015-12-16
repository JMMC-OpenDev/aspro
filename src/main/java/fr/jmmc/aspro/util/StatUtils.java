/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.util;

import fr.jmmc.jmal.complex.MutableComplex;
import java.util.ArrayList;
import java.util.Random;
import net.jafama.FastMath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author bourgesl
 */
public final class StatUtils {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(StatUtils.class.getName());
    /** precision expected on mean */
    private final static double EPSILON_MEAN = 5e-4;
    /** precision expected on stddev */
    private final static double EPSILON_VARIANCE = 5e-5;
    /** number of samples */
    public final static int N_SAMPLES = 256;
    /** normalization factor = 1/N_SAMPLES */
    public final static double SAMPLING_FACTOR_MEAN = 1d / N_SAMPLES;
    /** normalization factor for variance = 1 / (N_SAMPLES - 1) (bessel correction) */
    public final static double SAMPLING_FACTOR_VARIANCE = 1d / (N_SAMPLES - 1);
    /** cache size = max number of baselines (15 for 6 telescopes) */
    private final static int CACHE_SIZE = 15;
    /** singleton */
    private final static StatUtils instance = new StatUtils();

    public static StatUtils getInstance() {
        return instance;
    }

    /* members */
    /** current index in the distribution cache */
    private int current;
    /** cached distributions */
    private final ArrayList<ComplexDistribution> cache = new ArrayList<ComplexDistribution>(3);

    private StatUtils() {
        for (int i = 0; i < CACHE_SIZE; i++) {
            cache.add(ComplexDistribution.create(new Random()));
        }
    }

    public ComplexDistribution get() {
        final int idx = current;
        final ComplexDistribution distrib = cache.get(idx);

        this.current = (idx + 1) % CACHE_SIZE;

        return distrib;
    }

    public static final class ComplexDistribution {

        private final double[][] samples = new double[2][N_SAMPLES];

        public static ComplexDistribution create(final Random random) {
            final ComplexDistribution distrib = new ComplexDistribution();

            final long start = System.nanoTime();

            do {
                distrib.generate(random);
            } while (!distrib.test());

            logger.info("done: {} ms.", 1e-6d * (System.nanoTime() - start));

            return distrib;
        }

        private ComplexDistribution() {
            super();
        }

        private void generate(final Random random) {
            final double[] distRe = samples[0];
            final double[] distIm = samples[1];

            // bivariate distribution (complex normal):
            for (int n = 0; n < N_SAMPLES; n++) {
                // generate nth sample:
                distRe[n] = random.nextGaussian();
                distIm[n] = random.nextGaussian();
            }
        }

        private boolean test() {
            final double err = 10.0;
            // SNR=100:
            final double ref_amp = (100.0 * err);

            final double ref_re = ref_amp / Math.sqrt(2);
//            System.out.println("ref_amp: " + ref_amp + " visRe: " + ref_re);

            final double[] distRe = samples[0];
            final double[] distIm = samples[1];

            double re, im, sample, diff;
            double sum = 0.0;
            double sum_diff = 0.0;
            double sum_diff_square = 0.0;

            // bivariate distribution (complex normal):
            for (int n = 0; n < N_SAMPLES; n++) {

                // update nth sample:
                re = ref_re + (err * distRe[n]);
                im = ref_re + (err * distIm[n]);
//            System.out.println("" + re + " " + im);

                // compute norm=re^2+im^2:
                sample = re * re + im * im;

                // Compensated-summation variant for better numeric precision:
                sum += sample;
                diff = sample - ref_amp;
                sum_diff += diff;
                sum_diff_square += diff * diff;
            }

            // mean(norm):
            final double mean = SAMPLING_FACTOR_MEAN * sum;

            // variance(norm):
            // note: this algorithm ensures correctness (stable) even if the mean used in diff is wrong !
            final double variance = SAMPLING_FACTOR_VARIANCE * (sum_diff_square - (SAMPLING_FACTOR_MEAN * (sum_diff * sum_diff)));

            final double norm = ref_amp * ref_amp;
            final double ratio_avg = mean / norm;

            // d(v2) = 2v * dv 
            final double errNorm = 2.0 * ref_amp * err;
            final double ratio_variance = variance / (errNorm * errNorm);

            final boolean good = (Math.abs(ratio_avg - 1.0) < EPSILON_MEAN)
                    && (Math.abs(ratio_variance - 1.0) < EPSILON_VARIANCE);

            if (good && logger.isDebugEnabled()) {
                logger.debug("Sampling[" + N_SAMPLES + "] (err(re,im)= " + err + ")"
                        + " avg= " + mean + " norm= " + norm + " ratio: " + ratio_avg
                        + " stddev= " + Math.sqrt(variance) + " err(norm)= " + errNorm + " ratio: " + ratio_variance
                        + " good = " + good);
            }

            return good;
        }

        public double[][] getSamples() {
            return samples;
        }
    }

    // TEST:
    public static void main(String[] args) {
        final int N = 10;

        for (int i = 0; i < N; i++) {
            System.out.println("get(): " + StatUtils.getInstance().get());
        }

        System.out.println("VIS AMP:");
        for (int i = 0; i < N; i++) {
            test(true);
        }

        if (true) {
            System.out.println("V2:");
            for (int i = 0; i < N; i++) {
                test(false);
            }
        }
    }

    public static void test(boolean amp) {

        /** random generator */
        final Random random = new Random();

        final double visErrRe = 13.0;
        double ref = (100.0 * visErrRe);
        final double visRe = ref / Math.sqrt(2);
        final double visIm = visRe;

        if (!amp) {
            ref *= ref;
        }

        System.out.println("ref: " + ref + " visRe: " + visRe);

        double sample, diff;
        int n;
        final MutableComplex visComplexSample = new MutableComplex();
        double vis_acc = 0.0;
        double vis_diff_acc = 0.0;
        double vis_sq_diff_acc = 0.0;

        // bivariate distribution (complex normal):
        for (n = 0; n < N_SAMPLES; n++) {
            // update nth sample:
            visComplexSample.updateComplex(
                    visRe + visErrRe * random.nextGaussian(),
                    visIm + visErrRe * random.nextGaussian()
            );
//            System.out.println("" + (visComplexSample.getReal()) + " " + (visComplexSample.getImaginary()));

            // compute amplitude:
            sample = FastMath.pow2(visComplexSample.getReal()) + FastMath.pow2(visComplexSample.getImaginary());
            if (amp) {
                // compute vis amp:
                sample = Math.sqrt(sample);
            }

            // Compensated-summation variant for better numeric precision:
            vis_acc += sample;
            diff = sample - ref;
            vis_diff_acc += diff;
            vis_sq_diff_acc += diff * diff;
        }

        // average on amplitude:
        double avg = SAMPLING_FACTOR_MEAN * vis_acc;

        // standard deviation on amplitude:
        // note: this algorithm ensures correctness (stable) even if the mean used in diff is wrong !
        double stddev = Math.sqrt(SAMPLING_FACTOR_VARIANCE * (vis_sq_diff_acc - (SAMPLING_FACTOR_MEAN * FastMath.pow2(vis_diff_acc)))
        );

        System.out.println("Sampling[" + N_SAMPLES + "] avg= " + avg + " stddev= " + stddev + " vs visErrRe= " + visErrRe + " ratio: " + (stddev / visErrRe));
    }

}
