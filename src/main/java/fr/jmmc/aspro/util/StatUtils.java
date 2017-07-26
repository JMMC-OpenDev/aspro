/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.util;

import fr.jmmc.jmal.complex.MutableComplex;
import fr.jmmc.jmcs.util.FileUtils;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
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
    public final static int N_SAMPLES = 1024;
    /** normalization factor = 1/N_SAMPLES */
    public final static double SAMPLING_FACTOR_MEAN = 1d / N_SAMPLES;
    /** normalization factor for variance = 1 / (N_SAMPLES - 1) (bessel correction) */
    public final static double SAMPLING_FACTOR_VARIANCE = 1d / (N_SAMPLES - 1);
    /** initial cache size = number of baselines (15 for 6 telescopes) */
    private final static int INITIAL_CAPACITY = 15;
    /** singleton */
    private final static StatUtils INSTANCE = new StatUtils();

    public static StatUtils getInstance() {
        return INSTANCE;
    }

    /* members */
    /** current index in the distribution cache */
    private int current = 0;
    /** cached distributions */
    private final ArrayList<ComplexDistribution> cache;

    private StatUtils() {
        this.cache = new ArrayList<ComplexDistribution>(INITIAL_CAPACITY);
        prepare(INITIAL_CAPACITY);
    }

    public synchronized void prepare(final int count) {
        final int needed = count - cache.size();
        if (needed > 0) {
            logger.info("prepare: {} needed distributions", needed);
            final long start = System.nanoTime();

            for (int i = 0; i < needed; i++) {
                /* create a new random generator to have different seed (single thread) */
                final Random random = new Random();
                cache.add(ComplexDistribution.create(random));
            }

            logger.info("prepare done: {} ms.", 1e-6d * (System.nanoTime() - start));
        }
    }

    public synchronized ComplexDistribution get() {
        final int idx = current;
        final ComplexDistribution distrib = cache.get(idx);

        this.current = (idx + 1) % cache.size();

        return distrib;
    }

    public static final class ComplexDistribution {

        private final double[][] samples = new double[2][N_SAMPLES];
        private final double[][] moments = new double[2][4];

        public static ComplexDistribution create(final Random random) {
            final ComplexDistribution distrib = new ComplexDistribution();

            final long start = System.nanoTime();

            int n = 0;
            do {
                distrib.generate(random);
                n++;
            } while (!distrib.test());

            logger.info("done: {} ms ({} iterations).", 1e-6d * (System.nanoTime() - start), n);

            distrib.computeMoments();

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
            final double snr = 100.0;

            final double ref_amp = 1.0;
            final double err = ref_amp / snr;

            final double ref_re = ref_amp / Math.sqrt(2);
//            System.out.println("ref_amp: " + ref_amp + " visRe: " + ref_re);

            final double norm = ref_amp * ref_amp;

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
                diff = sample - norm;
                sum_diff += diff;
                sum_diff_square += diff * diff;
            }

            // mean(norm):
            final double mean = SAMPLING_FACTOR_MEAN * sum;

            // variance(norm):
            // note: this algorithm ensures correctness (stable) even if the mean used in diff is wrong !
            final double variance = SAMPLING_FACTOR_VARIANCE * (sum_diff_square - (SAMPLING_FACTOR_MEAN * (sum_diff * sum_diff)));

            final double ratio_avg = mean / norm;

            // d(v2) = 2v * dv 
            final double errNorm = 2.0 * ref_amp * err;
            final double ratio_variance = variance / (errNorm * errNorm);

            final boolean good = (Math.abs(ratio_avg - 1.0) < EPSILON_MEAN)
                    && (Math.abs(ratio_variance - 1.0) < EPSILON_VARIANCE);

            if (good && logger.isDebugEnabled()) {
                logger.debug("Sampling[" + N_SAMPLES + "] snr=" + snr + " (err(re,im)= " + err + ")"
                        + " avg= " + mean + " norm= " + norm + " ratio: " + ratio_avg
                        + " stddev= " + Math.sqrt(variance) + " err(norm)= " + errNorm + " ratio: " + ratio_variance
                        + " good = " + good);
            }

            return good;
        }

        public double[][] getSamples() {
            return samples;
        }

        public double[][] getMoments() {
            return moments;
        }

        private void computeMoments() {
            moments(samples[0], moments[0]);
            moments(samples[1], moments[1]);
        }
    }

    public static double mean(final double[] array) {
        double sample, sum = 0.0;

        for (int n = 0; n < array.length; n++) {
            sample = array[n];
            // No Compensated-summation (double):
            sum += sample;
        }
        return (sum / array.length);
    }

    public static double[] moments(final double[] array) {
        final double[] moments = new double[4];
        moments(array, moments);
        return moments;
    }

    public static void moments(final double[] array, final double[] moments) {
        final double mean = mean(array);

        double sample, diff;
        double sum_diff = 0.0;
        double sum_diff2 = 0.0;

        for (int n = 0; n < array.length; n++) {
            sample = array[n];
            // Compensated-summation variant for better numeric precision:
            diff = sample - mean;
            sum_diff += diff;
            sum_diff2 += diff * diff;
        }

        // variance(norm):
        // note: this algorithm ensures correctness (stable) even if the mean used in diff is wrong !
        final double variance = (sum_diff2 - (SAMPLING_FACTOR_MEAN * (sum_diff * sum_diff))) / (array.length - 1);

        final double stddev = Math.sqrt(variance);

        // Moments ordre 3 et 4:
        double sum_diff3 = 0.0;
        double sum_diff4 = 0.0;

        for (int n = 0; n < array.length; n++) {
            sample = array[n];
            // Compensated-summation variant for better numeric precision:
            diff = (sample - mean) / stddev;
            sum_diff3 += diff * diff * diff;
            sum_diff4 += diff * diff * diff * diff;
        }

        final double asymetry = sum_diff3 / array.length;
        final double kurtosis = (sum_diff4 / array.length) - 3.0; // normalised

        // output:
        moments[0] = mean;
        moments[1] = variance;
        moments[2] = asymetry;
        moments[3] = kurtosis;
    }

    // --- TEST ---
    public static void main(String[] args) throws IOException {
        final boolean TEST_SUM = false;
        final boolean TEST_DIST = true;
        final boolean DO_DUMP = false;

        // Test kahan sum:
        if (TEST_SUM) {
            final double[] values = new double[1024 * 1024];
            testSum(values, 1.0);
            testSum(values, 100.0);
            testSum(values, 10000.0);
            testSum(values, 1000000.0);
        }

        // Precompute distributions:
        StatUtils.getInstance();

        if (DO_DUMP) {
            // dump all distributions:
            for (int n = 0; n < INITIAL_CAPACITY; n++) {
                ComplexDistribution d = StatUtils.getInstance().get();
                System.out.println("get(): " + d);

                // Get the complex distribution for this row:
                final double[] distRe = d.getSamples()[0];
                final double[] distIm = d.getSamples()[1];

                System.out.println("moments(re): " + Arrays.toString(d.getMoments()[0]));
                System.out.println("moments(im): " + Arrays.toString(d.getMoments()[1]));

                final StringBuilder sb = new StringBuilder(4096);
                sb.append("# RE\tIM\n");

                for (int i = 0; i < N_SAMPLES; i++) {
                    sb.append(distRe[i]).append("\t").append(distIm[i]).append('\n');
                }
                final File file = new File("dist_" + N_SAMPLES + "_" + n + ".txt");
                System.out.println("Writing: " + file.getAbsolutePath());
                FileUtils.writeFile(file, sb.toString());
            }
        }

        final double[][] means = new double[2][INITIAL_CAPACITY];
        final double[][] vars = new double[2][INITIAL_CAPACITY];

        for (int n = 0; n < INITIAL_CAPACITY; n++) {
            ComplexDistribution d = StatUtils.getInstance().get();
            System.out.println("get(): " + d);

            System.out.println("moments(re): " + Arrays.toString(d.getMoments()[0]));
            System.out.println("moments(im): " + Arrays.toString(d.getMoments()[1]));

            means[0][n] = d.getMoments()[0][0];
            means[1][n] = d.getMoments()[1][0];

            vars[0][n] = d.getMoments()[0][1];
            vars[1][n] = d.getMoments()[1][1];
        }

        System.out.println("moments(mean) (re): " + Arrays.toString(moments(means[0])));
        System.out.println("moments(mean) (im): " + Arrays.toString(moments(means[1])));

        System.out.println("moments(variance) (re): " + Arrays.toString(moments(vars[0])));
        System.out.println("moments(variance) (im): " + Arrays.toString(moments(vars[1])));

        for (double snr = 100.0; snr > 0.09; snr /= 10.0) {
            System.out.println("--- SNR: " + snr + " ---");

            System.out.println("VISAMP");
            for (int i = 0; i < INITIAL_CAPACITY; i++) {
                ComplexDistribution d = StatUtils.getInstance().get();
                if (TEST_DIST) {
                    test(snr, true, d.getSamples());
                } else {
                    test(snr, true);
                }
            }
            System.out.println("VIS2:");
            for (int i = 0; i < INITIAL_CAPACITY; i++) {
                ComplexDistribution d = StatUtils.getInstance().get();
                if (TEST_DIST) {
                    test(snr, false, d.getSamples());
                } else {
                    test(snr, false);
                }
            }
        }
    }

    private static void test(final double snr, final boolean amp) {
        /* create a new random generator to have different seed (single thread) */
        final Random random = new Random();

        final double[][] samples = new double[2][];
        samples[0] = new double[N_SAMPLES];
        samples[1] = new double[N_SAMPLES];

        for (int n = 0; n < N_SAMPLES; n++) {
            // update nth sample:
            samples[0][n] = random.nextGaussian();
            samples[1][n] = random.nextGaussian();
        }
        test(snr, amp, samples);
    }

    private static void test(final double snr, final boolean amp, double[][] samples) {
        final double visErr = 10.0;

        double ref = snr * visErr;
        double err = visErr;
        final double visRe = ref / Math.sqrt(2);
        final double visIm = visRe;

        if (!amp) {
            // d(v2) = 2v * dv 
            err *= 2.0 * ref;
            ref *= ref;
        }
        /*
        System.out.println("ref: " + ref + " err: " + err);
        System.out.println("visRe=visIm: " + visRe + " visErr: " + visErr);
         */
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
                    visRe + visErr * samples[0][n],
                    visIm + visErr * samples[1][n]
            );

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
        double stddev = Math.sqrt(SAMPLING_FACTOR_VARIANCE * (vis_sq_diff_acc - (SAMPLING_FACTOR_MEAN * FastMath.pow2(vis_diff_acc))));

        System.out.println("Sampling[" + N_SAMPLES + "] avg= " + avg + " vs expected ref= " + ref + " ratio: " + (avg / ref));
        System.out.println("Sampling[" + N_SAMPLES + "] stddev= " + stddev + " vs expected Err= " + err + " ratio: " + (stddev / err));
    }

    // sum tests
    private static void testSum(final double[] values, final double val) {
        Arrays.fill(values, val);
        final double naive = naiveSum(values);
        System.out.println("naiveSum[" + val + "]: " + naive);
        final double kahan = kahanSum(values);
        System.out.println("kahanSum[" + val + "]: " + kahan);
        System.out.println("delta: " + (naive - kahan));
    }

    private static double naiveSum(double[] values) {
        final double[] state = new double[1]; // sum
        state[0] = 0.0;
        for (int i = 0; i < values.length; i++) {
            state[0] += values[i];
        }
        return state[0];
    }

    private static double kahanSum(double[] values) {
        final double[] state = new double[2]; // sum | error
        state[0] = 0.0;
        state[1] = 0.0;
        for (int i = 0; i < values.length; i++) {
            final double y = values[i] - state[1];
            final double t = state[0] + y;
            state[1] = (t - state[0]) - y;
            state[0] = t;
        }
        return state[0];
    }

}
