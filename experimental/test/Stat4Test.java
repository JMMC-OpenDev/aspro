/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package test;

import fr.jmmc.jmcs.Bootstrapper;
import fr.jmmc.jmcs.util.NumberUtils;
import java.io.IOException;
import java.util.Locale;
import org.apache.commons.math4.exception.DimensionMismatchException;
import org.apache.commons.math4.random.GaussianRandomGenerator;
import org.apache.commons.math4.random.HaltonSequenceGenerator;
import org.apache.commons.math4.random.RandomVectorGenerator;
import org.apache.commons.math4.random.UncorrelatedRandomVectorGenerator;
import org.apache.commons.math4.stat.descriptive.DescriptiveStatistics;
import org.apache.commons.numbers.complex.Complex;
import org.apache.commons.rng.UniformRandomProvider;
import org.apache.commons.rng.simple.RandomSource;
import org.apache.commons.statistics.distribution.NormalDistribution;
import org.jfree.data.Range;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 *
 * @author bourgesl
 */
public class Stat4Test {

    private final static boolean TEST_C2_2_C = false;

    private final static int SUPER_SAMPLING = 16 * 4; // x1 (normal), x16 (high quality), x64 (ultra-high quality)

    private final static int N = 1024 * SUPER_SAMPLING;

    private final static boolean USE_PSEUDO_RANDOM = true;

    private final static double mu = 0.1; // 0.1

    private final static double DEF_ANGLE = 3.0;

    private final static long seed = 17399225432L; // Fixed seed means same results every time 

    private final static Range PLOT_RANGE = new Range(-2, 2);

    private static boolean SHOW_PLOT = true;

    // --- TEST ---
    public static void main(String[] args) throws IOException {

        Bootstrapper.getState();

        // Set the default locale to en-US locale (for Numerical Fields "." ",")
        Locale.setDefault(Locale.US);

        if (TEST_C2_2_C) {
            if (false) {
                SHOW_PLOT = false;
                for (double angle = 0.0; angle < 90.5; angle += 10.0) {
                    testC2toC(angle);
                }
            } else {
                testC2toC(DEF_ANGLE);
            }
        } else {
            if (false) {
                SHOW_PLOT = false;
                for (double angle = 3.0 - 90.0; angle < 95.0; angle += 10.0) {
                    testCtoC2(angle);
                }
            } else {
                testCtoC2(DEF_ANGLE);
            }
        }
    }

    /*
    testCtoC2:
SNR: 5.0 angle: 3.0 C2: ratio mean: 0.999982856509197 sigma: 1.0098452061141512
SNR: 4.0 angle: 3.0 C2: ratio mean: 0.9999794521371985 sigma: 1.015383975237218
SNR: 3.0 angle: 3.0 C2: ratio mean: 0.9999745618889425 sigma: 1.027253387259957
SNR: 2.0 angle: 3.0 C2: ratio mean: 0.9999677209151077 sigma: 1.0604478968701103
SNR: 1.8 angle: 3.0 C2: ratio mean: 0.9999663117769574 sigma: 1.0741582114593213
SNR: 1.6 angle: 3.0 C2: ratio mean: 0.999965163071902 sigma: 1.0930388909858024
SNR: 1.4 angle: 3.0 C2: ratio mean: 0.9999646867415607 sigma: 1.1200095729620607
SNR: 1.2 angle: 3.0 C2: ratio mean: 0.9999658031900239 sigma: 1.1603412550896448
SNR: 1.0 angle: 3.0 C2: ratio mean: 0.9999707307832936 sigma: 1.2243094764848377
SNR: 0.8 angle: 3.0 C2: ratio mean: 0.9999854877968007 sigma: 1.3340811525680163
SNR: 0.6 angle: 3.0 C2: ratio mean: 1.0000297523079746 sigma: 1.54485381509995
SNR: 0.399 angle: 3.0 C2: ratio mean: 1.0001938791418683 sigma: 2.0337401888813176
SNR: 0.199 angle: 3.0 C2: ratio mean: 1.001294744442735 sigma: 3.6891595293951123
SNR: 0.179 angle: 3.0 C2: ratio mean: 1.0016667094985756 sigma: 4.072524296371414
SNR: 0.159 angle: 3.0 C2: ratio mean: 1.0021986538917063 sigma: 4.555575679311024
SNR: 0.139 angle: 3.0 C2: ratio mean: 1.002994012643467 sigma: 5.181415644745049
SNR: 0.119 angle: 3.0 C2: ratio mean: 1.0042546819195843 sigma: 6.022114604056376
SNR: 0.098 angle: 3.0 C2: ratio mean: 1.0065658655194336 sigma: 7.280116664337107
SNR: 0.078 angle: 3.0 C2: ratio mean: 1.010899301501257 sigma: 9.11520853595174
SNR: 0.057 angle: 3.0 C2: ratio mean: 1.0219509570310992 sigma: 12.43852627820948
SNR: 0.037 angle: 3.0 C2: ratio mean: 1.0594745235885874 sigma: 19.12634682392065
SNR: 0.016 angle: 3.0 C2: ratio mean: 1.4961662325352802 sigma: 44.18088419268908
     */
    public static void testCtoC2(final double angle) {
        System.out.println("-----");
        System.out.println("angle: " + angle);

        final double angRad = Math.toRadians(angle);

        final double cos_angle = Math.cos(angRad);
        final double sin_angle = Math.sin(angRad);

        for (double snr = 5.0; snr > 0.01;) {
            // fix snr rounding:
            snr = NumberUtils.trimTo3Digits(snr);

            final double sigma = mu / snr;

            System.out.println("SNR = " + snr);
            System.out.println("C : mu = " + mu + " sigma = " + sigma);
            final double mu_c = mu;
            final double sigma_c = sigma;
            final double mu_c2 = (mu * mu);
            final double sigma_c2 = (2.0 * mu * sigma); // dv2 = 2v*dv
            System.out.println("C2: mu = " + mu_c2 + " sigma = " + sigma_c2);

            // add angle on C:
            final double mean_re = mu * cos_angle;
            final double mean_im = mu * sin_angle;

            final double[] mean = new double[]{mean_re, mean_im};
            final double[] stddev = new double[]{sigma / Math.sqrt(2.0), sigma / Math.sqrt(2.0)}; // (1/2 1/2) as variance sums up

            // Generate complex vector:
            final RandomVectorGenerator generator = createRandomVectorGenerator(mean, stddev);

            final DescriptiveStatistics2D dc = new DescriptiveStatistics2D(N);
            final DescriptiveStatistics2D dc2 = new DescriptiveStatistics2D(N);

            final XYSeriesCollection dataset = ChartUtils.createDataset("C");
            final XYSeries xySeries = dataset.getSeries(0);

            final XYSeriesCollection dataset2 = ChartUtils.createDataset("C^2");
            final XYSeries xySeries2 = dataset2.getSeries(0);

            // Use the generator to generate correlated vectors.
            for (int i = 0; i < N; i++) {
                final double[] randomVector = generator.nextVector();
//            System.out.println("RV[" + i + "]: (" + randomVector[0] + " , " + randomVector[1] + ")");

                final Complex c = Complex.ofCartesian(randomVector[0], randomVector[1]);
                xySeries.add(c.getReal(), c.getImaginary());
                dc.add(c.getReal(), c.getImaginary());

                final Complex c2 = c.multiply(c);
                xySeries2.add(c2.getReal(), c2.getImaginary());
                dc2.add(c2.getReal(), c2.getImaginary());
            }

            System.out.println("Stats C    : " + dc);
            System.out.println("Stats C2   : " + dc2);

            final double est_mu_c = Math.sqrt(dc.getMeanReal() * dc.getMeanReal() + dc.getMeanIm() * dc.getMeanIm());
            final double est_sigma_c = Math.sqrt(dc.getStddevReal() * dc.getStddevReal() + dc.getStddevIm() * dc.getStddevIm());
            System.out.println("Est C: mu = " + est_mu_c + " sigma = " + est_sigma_c);

            final double ratio_mu_c = est_mu_c / mu_c;
            final double ratio_sigma_c = est_sigma_c / sigma_c;
            System.out.println("SNR: " + snr + " angle: " + angle + " C: ratio mean: " + ratio_mu_c + " sigma: " + ratio_sigma_c);

            final double est_mu_c2 = Math.sqrt(dc2.getMeanReal() * dc2.getMeanReal() + dc2.getMeanIm() * dc2.getMeanIm());
            final double est_sigma_c2 = Math.sqrt(dc2.getStddevReal() * dc2.getStddevReal() + dc2.getStddevIm() * dc2.getStddevIm());
            System.out.println("Est C2 : mu = " + est_mu_c2 + " sigma = " + est_sigma_c2);

            final double ratio_mu_c2 = est_mu_c2 / mu_c2;
            final double ratio_sigma_c2 = est_sigma_c2 / sigma_c2;
            System.out.println("SNR: " + snr + " angle: " + angle + " C2: ratio mean: " + ratio_mu_c2 + " sigma: " + ratio_sigma_c2);

            if (SHOW_PLOT) {
                final String prefix = "CtoC2[SNR = " + snr + "|" + N + (USE_PSEUDO_RANDOM ? " PRNG" : " RNG") + "]";
                ChartUtils.showXYPlot(prefix + " C", "re", "im", dataset, PLOT_RANGE, PLOT_RANGE);
                ChartUtils.showXYPlot(prefix + " C2", "re", "im", dataset2, PLOT_RANGE, PLOT_RANGE);
            }

            if (snr > 25) {
                snr -= 10.0;
            } else if (snr > 2.5) {
                snr -= 1.0;
            } else if (snr > 0.25) {
                snr -= 0.2;
            } else {
                snr -= 0.02;
            }
        }
    }

    public static void testC2toC(final double angle) {
        System.out.println("-----");
        System.out.println("angle: " + angle);

        final double angRad = Math.toRadians(angle);

        final double cos_angle = Math.cos(angRad);
        final double sin_angle = Math.sin(angRad);

        for (double snr = 5.0; snr > 0.01;) {
            // fix snr rounding:
            snr = NumberUtils.trimTo3Digits(snr);

            final double sigma = mu / snr;

            System.out.println("SNR = " + snr);
            System.out.println("C2 : mu = " + mu + " sigma = " + sigma);
            final double mu_c2 = mu;
            final double sigma_c2 = sigma;
            final double mu_c = Math.sqrt(mu);
            final double sigma_c = (sigma / (2.0 * mu_c)); // dv2 = 2v*dv
            System.out.println("C: mu = " + mu_c + " sigma = " + sigma_c);

            // add angle on C2:
            final double mean_re = mu * cos_angle;
            final double mean_im = mu * sin_angle;

            final double[] mean = new double[]{mean_re, mean_im};
            final double[] stddev = new double[]{sigma / Math.sqrt(2.0), sigma / Math.sqrt(2.0)}; // (1/2 1/2) as variance sums up

            // Generate complex vector:
            final RandomVectorGenerator generator = createRandomVectorGenerator(mean, stddev);

            final DescriptiveStatistics2D dc2 = new DescriptiveStatistics2D(N);
            final DescriptiveStatistics2D dc = new DescriptiveStatistics2D(N);

            final XYSeriesCollection dataset2 = ChartUtils.createDataset("C^2");
            final XYSeries xySeries2 = dataset2.getSeries(0);

            final XYSeriesCollection dataset = ChartUtils.createDataset("C=SQRT(C^2)");
            final XYSeries xySeries = dataset.getSeries(0);

            // Use the generator to generate correlated vectors.
            for (int i = 0; i < N; i++) {
                final double[] randomVector = generator.nextVector();
//            System.out.println("RV[" + i + "]: (" + randomVector[0] + " , " + randomVector[1] + ")");

                final Complex c2 = Complex.ofCartesian(randomVector[0], randomVector[1]);
                xySeries2.add(c2.getReal(), c2.getImaginary());
                dc2.add(c2.getReal(), c2.getImaginary());

                final Complex c = c2.sqrt();
                xySeries.add(c.getReal(), c.getImaginary());
                dc.add(c.getReal(), c.getImaginary());

                if (true) {
                    // add other symetric root:
                    final Complex cn = Complex.ofCartesian(-c.getReal(), -c.getImaginary());
                    xySeries.add(cn.getReal(), cn.getImaginary());
                }
            }

            System.out.println("Stats C2   : " + dc2);
            System.out.println("Stats C    : " + dc);

            final double est_mu_c2 = Math.sqrt(dc2.getMeanReal() * dc2.getMeanReal() + dc2.getMeanIm() * dc2.getMeanIm());
            final double est_sigma_c2 = Math.sqrt(dc2.getStddevReal() * dc2.getStddevReal() + dc2.getStddevIm() * dc2.getStddevIm());
            System.out.println("Est C2 : mu = " + est_mu_c2 + " sigma = " + est_sigma_c2);

            final double ratio_mu_c2 = est_mu_c2 / mu_c2;
            final double ratio_sigma_c2 = est_sigma_c2 / sigma_c2;
            System.out.println("SNR: " + snr + " angle: " + angle + " C2: ratio mean: " + ratio_mu_c2 + " sigma: " + ratio_sigma_c2);

            final double est_mu_c = Math.sqrt(dc.getMeanReal() * dc.getMeanReal() + dc.getMeanIm() * dc.getMeanIm());
            final double est_sigma_c = Math.sqrt(dc.getStddevReal() * dc.getStddevReal() + dc.getStddevIm() * dc.getStddevIm());
            System.out.println("Est C: mu = " + est_mu_c + " sigma = " + est_sigma_c);

            final double ratio_mu_c = est_mu_c / mu_c;
            final double ratio_sigma_c = est_sigma_c / sigma_c;
            System.out.println("SNR: " + snr + " angle: " + angle + " C: ratio mean: " + ratio_mu_c + " sigma: " + ratio_sigma_c);

            if (SHOW_PLOT) {
                final String prefix = "C2toC[SNR = " + snr + "|" + N + (USE_PSEUDO_RANDOM ? " PRNG" : " RNG") + "]";
                ChartUtils.showXYPlot(prefix + " C2", "re", "im", dataset2, PLOT_RANGE, PLOT_RANGE);
                ChartUtils.showXYPlot(prefix + " C", "re", "im", dataset, PLOT_RANGE, PLOT_RANGE);
            }

            if (snr > 25) {
                snr -= 10.0;
            } else if (snr > 2.5) {
                snr -= 1.0;
            } else if (snr > 0.25) {
                snr -= 0.2;
            } else {
                snr -= 0.02;
            }
        }
    }

    private static RandomVectorGenerator createRandomVectorGenerator(final double[] mean, final double[] stddev) {
        // Generate complex vector:
        final RandomVectorGenerator generator;
        if (USE_PSEUDO_RANDOM) {
            generator = new UncorrelatedPseudoRandomVectorGenerator(mean, stddev);
        } else {
            // Create (and possibly seed) a PRNG (could use any of the CM-provided generators).
            final UniformRandomProvider rg = RandomSource.create(RandomSource.WELL_1024_A, seed);

            // Create a GaussianRandomGenerator using "rg" as its source of randomness.
            final GaussianRandomGenerator rawGenerator = new GaussianRandomGenerator(rg);

            // Create a CorrelatedRandomVectorGenerator using "rawGenerator" for the components.
            generator = new UncorrelatedRandomVectorGenerator(mean, stddev, rawGenerator);
        }
        return generator;
    }

    private final static class PseudoRandomGenerator implements RandomVectorGenerator {

        private final NormalDistribution norm = new NormalDistribution(0, 1.0);

        // Create a Sobol sequence generator for n-dimensional vectors
        private final RandomVectorGenerator seqGenerator;

        PseudoRandomGenerator(final int dimension) {
            // seqGenerator = new SobolSequenceGenerator(2);
            seqGenerator = new HaltonSequenceGenerator(2);
            seqGenerator.nextVector(); // skip 0
        }

        @Override
        public double[] nextVector() {
            final double[] nextVector = seqGenerator.nextVector();
            // System.out.println("next vect: " + Arrays.toString(nextVector));

            for (int i = 0; i < nextVector.length; ++i) {
                nextVector[i] = norm.inverseCumulativeProbability(nextVector[i]);
            }
            // System.out.println("next norm: " + Arrays.toString(nextVector));
            return nextVector;
        }
    }

    private final static class UncorrelatedPseudoRandomVectorGenerator implements RandomVectorGenerator {

        /** Underlying sobol generator. */
        private final RandomVectorGenerator generator;

        /** Mean vector. */
        private final double[] mean;

        /** Standard deviation vector. */
        private final double[] standardDeviation;

        /** Simple constructor.
         * <p>Build an uncorrelated random vector generator from
         * its mean and standard deviation vectors.</p>
         * @param mean expected mean values for each component
         * @param standardDeviation standard deviation for each component
         */
        UncorrelatedPseudoRandomVectorGenerator(double[] mean,
                                                double[] standardDeviation) {
            if (mean.length != standardDeviation.length) {
                throw new DimensionMismatchException(mean.length, standardDeviation.length);
            }
            this.mean = mean.clone();
            this.standardDeviation = standardDeviation.clone();

            this.generator = new PseudoRandomGenerator(mean.length);
        }

        /** Generate an uncorrelated random vector.
         * @return a random vector as a newly built array of double
         */
        @Override
        public double[] nextVector() {

            final double[] random = generator.nextVector();

            for (int i = 0; i < random.length; ++i) {
                random[i] = mean[i] + standardDeviation[i] * random[i];
            }

            return random;

        }

    }

    private final static class DescriptiveStatistics2D {

        private final DescriptiveStatistics[] stats;

        DescriptiveStatistics2D(final int window) {
            stats = new DescriptiveStatistics[2];
            for (int i = 0; i < 2; i++) {
                stats[i] = new DescriptiveStatistics(window);
            }
        }

        void add(double re, double im) {
            stats[0].addValue(re);
            stats[1].addValue(im);
        }

        double getMeanReal() {
            return stats[0].getMean();
        }

        double getMeanIm() {
            return stats[1].getMean();
        }

        double getStddevReal() {
            return stats[0].getStandardDeviation();
        }

        double getStddevIm() {
            return stats[1].getStandardDeviation();
        }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder(128);
            sb.append("Real ");
            toString(stats[0], sb);
            sb.append(" Imaginary ");
            toString(stats[1], sb);
            return sb.toString();
        }

        private static void toString(DescriptiveStatistics s, final StringBuilder sb) {
            sb.append("(n: ").append(s.getN());
            sb.append(" mean: ").append(s.getMean());
            sb.append(" stddev: ").append(s.getStandardDeviation());
            sb.append(" skewness: ").append(s.getSkewness());
            sb.append(" kurtosis: ").append(s.getKurtosis()).append(')');
        }
    }
}
