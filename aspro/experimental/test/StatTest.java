/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package test;

import fr.jmmc.jmcs.Bootstrapper;
import fr.jmmc.jmcs.util.NumberUtils;
import java.io.IOException;
import java.util.Locale;
import org.apache.commons.math3.complex.Complex;
import org.apache.commons.math3.random.GaussianRandomGenerator;
import org.apache.commons.math3.random.JDKRandomGenerator;
import org.apache.commons.math3.random.RandomGenerator;
import org.apache.commons.math3.random.UncorrelatedRandomVectorGenerator;
import org.apache.commons.math3.stat.descriptive.MultivariateSummaryStatistics;
import org.jfree.data.Range;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 *
 * @author bourgesl
 */
public class StatTest {

    // --- TEST ---
    public static void main(String[] args) throws IOException {

        Bootstrapper.getState();

        // Set the default locale to en-US locale (for Numerical Fields "." ",")
        Locale.setDefault(Locale.US);

        final double mu = 0.7; // 0.1
        final Range range = new Range(-2, 2);

        for (double snr = 5.0; snr > 0.01;) {
            // fix snr rounding:
            snr = NumberUtils.trimTo3Digits(snr);

            final double sigma = mu / snr;

            System.out.println("SNR = " + snr);
            System.out.println("mu = " + mu);
            System.out.println("sigma = " + sigma);

            System.out.println("sqrt(mu) = " + Math.sqrt(mu));

            final int N = 5000;

            final double[] mean = new double[]{mu, 0.0};
            final double[] stddev = new double[]{sigma, sigma};

            // Generate complex vector:
            // Create (and possibly seed) a PRNG (could use any of the CM-provided generators).
            final long seed = 17399225432L; // Fixed seed means same results every time 
            final RandomGenerator rg = new JDKRandomGenerator((int) seed);

            // Create a GaussianRandomGenerator using "rg" as its source of randomness.
            final GaussianRandomGenerator rawGenerator = new GaussianRandomGenerator(rg);

            // Create a CorrelatedRandomVectorGenerator using "rawGenerator" for the components.
            final UncorrelatedRandomVectorGenerator generator = new UncorrelatedRandomVectorGenerator(mean, stddev, rawGenerator);

            final MultivariateSummaryStatistics sc2 = new MultivariateSummaryStatistics(2, true);
            final MultivariateSummaryStatistics sc = new MultivariateSummaryStatistics(2, true);
            final MultivariateSummaryStatistics scAbs = new MultivariateSummaryStatistics(2, true);
            final MultivariateSummaryStatistics sc2b = new MultivariateSummaryStatistics(2, true);

            final XYSeriesCollection dataset2 = ChartUtils.createDataset("C2");
            final XYSeries xySeries2 = dataset2.getSeries(0);

            final XYSeriesCollection dataset = ChartUtils.createDataset("C");
            final XYSeries xySeries = dataset.getSeries(0);

            final XYSeriesCollection dataset2b = ChartUtils.createDataset("C2=C*C");
            final XYSeries xySeries2b = dataset2b.getSeries(0);

            // Use the generator to generate correlated vectors.
            final double[] res = new double[2];
            for (int i = 0; i < N; i++) {
                final double[] randomVector = generator.nextVector();

//            System.out.println("RV[" + i + "]: (" + randomVector[0] + " , " + randomVector[1] + ")");
                final Complex c2 = new Complex(randomVector[0], randomVector[1]);
                xySeries2.add(c2.getReal(), c2.getImaginary());
                add(sc2, c2, res);

                final Complex c = c2.sqrt();
                xySeries.add(c.getReal(), c.getImaginary());
                // add other symetric root:
                final Complex cn = new Complex(-c.getReal(), -c.getImaginary());
                xySeries.add(cn.getReal(), cn.getImaginary());

                final Complex ca = new Complex(Math.abs(c.getReal()), c.getImaginary()); // only abs(re)
                add(sc, ca, res);

                final Complex cabs = new Complex(c.abs(), Math.toDegrees(c.getArgument()));
                add(scAbs, cabs, res);

                if (false) {
                    // add other symetric root:
                    final Complex cabs2 = new Complex(cn.abs(), Math.toDegrees(cn.getArgument()));
                    add(scAbs, cabs2, res);
                    add(sc, cn, res);
                }

                if (false) {
                    // inverse again:
                    final Complex c2b = c.multiply(c);
                    xySeries2b.add(c2b.getReal(), c2b.getImaginary());
                    add(sc2b, c2b, res);

                    if (false) {
                        final Complex c2bn = cn.multiply(cn);
                        xySeries2b.add(c2bn.getReal(), c2bn.getImaginary());
                        add(sc2b, c2bn, res);
                    }
                }
            }

            System.out.println("Stats sc2   : " + sc2);
            if (false) {
                System.out.println("Stats sc2b  : " + sc2b);
            }
            System.out.println("Stats sc    : " + sc);
            System.out.println("Stats scAbs : " + scAbs);

            ChartUtils.showXYPlot("V2[SNR = " + snr + "]", "re", "im", dataset2, range, range);
            ChartUtils.showXYPlot("V[SNR = " + snr + "]", "re", "im", dataset, range, range);
//            ChartUtils.showXYPlot("V2b[SNR = " + snr + "]", "re", "im", dataset2b, range, range);

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

    private static void add(final MultivariateSummaryStatistics sc, final Complex c, final double[] res) {
        res[0] = c.getReal();
        res[1] = c.getImaginary();
        sc.addValue(res);
    }
}
