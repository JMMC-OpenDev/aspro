/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package test;

import fr.jmmc.jmal.Band;
import fr.jmmc.jmcs.Bootstrapper;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.oiexplorer.core.gui.chart.ChartUtils;
import java.awt.BasicStroke;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.util.EnumSet;
import javax.swing.JFrame;
import javax.swing.JPanel;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.function.Function2D;
import org.jfree.data.general.DatasetUtils;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 *
 * @author bourgesl
 */
public class StrehlChartTest {

    public static final double UT_DIAM = 8.0; // pupil size not mirror size

    private static double ZENITH_ANGLE_DEF = 15.0;
    private static double ZENITH_ANGLE = ZENITH_ANGLE_DEF;

    public static final int MAG_MIN = 6;
    public static final int MAG_MAX = 22;
    /*
    // CIAO plot mags
    public static final int MAG_MIN = 7;
    public static final int MAG_MAX = 10;
     */
    private static final double[] MAGS = new double[2 * (MAG_MAX - MAG_MIN) + 1];

    private final static double[] LAMBDA_H = new double[]{1.5e-6};
    private final static double[] LAMBDA_K = new double[]{2.2e-6};

    // NAOMI plots @ 1.1, MACAO, CIAO @ 0.85
//     private final static double[] SEEING = new double[]{1.4, 1.1, 1.0, 0.85, 0.7, 0.5};
    private final static double[] SEEING = new double[]{/*0.6, 0.7, 0.85, */0.6, 1.0, 1.15};

    private final static double[] DIT_LGS = new double[]{2.0 /*, 1.0, 4.0, 10.0 */};

    // h0 tests:
    private final static double[] H0 = new double[]{1500.0, /*3000.0, 3500.0, 4300.0, 6000.0, 8000.0, 10000.0 */};

    public static void main(String[] args) {
        // invoke App method to initialize logback now:
        Bootstrapper.getState();

        // to define themes
        ChartUtils.createScientificTickUnits();

        // Prepare mags:
        for (int i = 0; i <= 2 * (MAG_MAX - MAG_MIN); i++) {
            MAGS[i] = MAG_MIN + 0.5 * i;
        }

        SwingUtils.invokeEDT(new Runnable() {

            @Override
            public void run() {

                if (false) {
                    // strehl iso:
                    createChartFrame("Strehl ISO GPAO_VIS - Strehl vs distance",
                            createStrehlIsoChart(true));
                    createChartFrame("Strehl ISO GPAO_IR  - Strehl vs distance",
                            createStrehlIsoChart(false));
                }

                int nbSubPupils, nbActuators;
                Band aoBand;
                double td;

                if (false) {
                    aoBand = Band.V;

                    // >500Hz
                    td = 2.0;
                    final double qe = 0.70;
                    final double ron = 4 / 30.0; // 1.0 / 30;
                    nbSubPupils = 36 * 2;

                    createChartFrame("CHARA - Strehl (NEW) vs mag" + aoBand.getName(),
                            createCHARAStrehlvsMagChart(aoBand, LAMBDA_H, nbSubPupils, nbSubPupils, td, qe, ron));
                    return;
                }

                if (false) {
                    if (false) {
                        // OK
                        // UT MACAO:
                        aoBand = Band.V;
                        createChartFrame("UT (MACAO) - Strehl vs seeing",
                                createUTMacaoStrehlvsSeeingChart(aoBand, LAMBDA_K));
                    }

                    if (false) {
                        // OK
                        // UT:
                        final double qe = 0.70;
                        final double ron = 1.0;

                        Band b = Band.K;
                        aoBand = Band.V;

                        td = 0.80; // 1050 Hz
                        createChartFrame("UT (MACAO) - NEW - Strehl " + b.getName() + " vs mag" + aoBand.getName() + " td: " + td,
                                createUTStrehlvsMagChart(aoBand, LAMBDA_K, 60, 50, td, qe, ron)); // auto t0
                    }

                    if (false) {
                        ZENITH_ANGLE = 30.0;
                        // OK
                        double qe = 0.70;
                        double ron = 6.5;

                        Band b = Band.K;
                        aoBand = Band.K;
                        td = 1.75; // 500Hz
                        createChartFrame("UT (CIAO Off-axis) - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                createUTStrehlvsMagChart(aoBand, LAMBDA_K, 9 * 9, 50, td, qe, ron));

                        qe *= 0.5; // half flux
                        ron = 5.0;
                        td = 1.5; // 500Hz
                        createChartFrame("UT (CIAO ON-axis) - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                createUTStrehlvsMagChart(aoBand, LAMBDA_K, 9 * 9, 50, td, qe, ron));

                        ZENITH_ANGLE = ZENITH_ANGLE_DEF;
                    }
                    if (true) {
                        final Band b = Band.H;
                        // AT:
                        if (false) {
                            // STRAP
                            aoBand = Band.V;
                            nbSubPupils = 4;

                            final double qe = 0.8;
                            final double ron = 0.1;
                            td = 1.5;

                            createChartFrame("AT (Tip tilt) - NEW - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                    createATStrehlvsMagChart(aoBand, LAMBDA_H, nbSubPupils, 2, td, qe, ron)); // auto t0
                        }

                        /* NAOMI: seeing = 1.0" and t0 = 2.5ms */
                        aoBand = Band.R;

                        final double qe = 0.95;
                        final double ron = 64.0;
                        double effRon;

                        /* 14 modes */
                        nbSubPupils = 16;
                        nbActuators = 15;
                        // 500Hz ie max 2ms
                        td = 2.0;
                        effRon = ron / 200.0; // gain = g * KI = 100 * 0.4 = 40
                        System.out.println("effRon: " + NumberUtils.trimTo3Digits(effRon));

                        createChartFrame("AT (NAOMI_BRIGHT ns=" + nbSubPupils + ") - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                createATStrehlvsMagChart(aoBand, LAMBDA_H, nbSubPupils, nbActuators, td, qe, effRon));

                        /* transition R> 13 : 7 modes */
                        nbSubPupils = 16;
                        nbActuators = 7;
                        // 100Hz ie max 10s
                        td = 5.0;
                        effRon = ron / 90.0; // gain = g * KI = 100 * 0.9 = 90
                        System.out.println("effRon: " + NumberUtils.trimTo3Digits(effRon));

                        createChartFrame("AT (NAOMI_FAINT ns=" + nbSubPupils + ") - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                createATStrehlvsMagChart(aoBand, LAMBDA_H, nbSubPupils, nbActuators, td, qe, effRon));
                    }
                    return;
                }

                final double[] lambda = new double[1];

                for (Band b : EnumSet.of(Band.V, Band.H, Band.K, Band.L, Band.M, Band.N)) {
                    if (b == Band.K) {
                        lambda[0] = 2.2e-6;;
                    } else {
                        lambda[0] = b.getLambdaFluxZero() * 1E-6;
                    }

                    // UT:
                    if (false) {
                        // OK
                        final double qe = 0.70;
                        final double ron = 1.0;

                        aoBand = Band.V;

                        td = 0.80; // 1050 Hz
                        createChartFrame("UT (MACAO) - NEW - Strehl " + b.getName() + " vs mag" + aoBand.getName() + " td: " + td,
                                createUTStrehlvsMagChart(aoBand, lambda, 60, 50, td, qe, ron)); // auto t0
                    }
                    if (false && (b == Band.K)) {
                        // OK
                        double qe = 0.70;
                        double ron = 6.5;

                        aoBand = Band.K;
                        td = 1.75; // 500Hz
                        createChartFrame("UT (CIAO Off-axis) - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                createUTStrehlvsMagChart(aoBand, lambda, 9 * 9, 9 * 9, td, qe, ron));

                        qe *= 0.5; // half flux
                        ron = 5.0;
                        td = 1.5; // 500Hz
                        createChartFrame("UT (CIAO ON-axis) - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                createUTStrehlvsMagChart(aoBand, lambda, 9 * 9, 9 * 9, td, qe, ron));
                    }
                    // GPAO:
                    if (true && (b == Band.K)) {
                        // GPAO_NGS_VIS
                        aoBand = Band.G_RP;

                        double effRon, magOffset;
                        double strehlMax;

                        double qe = 0.25;
                        final double ron = 180.0;

                        /* 1200 modes (3/4) (40*40) */
                        nbSubPupils = 1200;
                        nbActuators = 800;
                        strehlMax = 0.73; // 0.93 by default

                        // 1kHz
                        td = 1.0; // adjusted to get high strehl ~ 0.85
                        System.out.println("GPAO_NGS_VIS: td: " + td);

                        effRon = ron / 900.0; // 0.2
                        System.out.println("GPAO_NGS_VIS: effRon: " + NumberUtils.trimTo3Digits(effRon));

                        magOffset = -1.0; // adjusted to have 0.4 / 0.5 at mag=12
                        System.out.println("magOffset: " + magOffset);

                        if (true) {
                            createChartFrame("UT (GPAO_NGS_VIS ns=" + nbSubPupils + ") - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                    createStrehlvsMagChart("GPAO_NGS_VIS", aoBand, lambda, UT_DIAM, nbSubPupils, nbActuators, td, qe, effRon, magOffset, strehlMax));
                        }

                        // FAKE GPAO_LGS_VIS
                        if (true) {
                            /* 900 modes (3/4) (30*30) */
                            nbSubPupils = 900;
                            nbActuators = 600;
                            strehlMax = 0.85; // 0.93 by default

                            for (double dit : DIT_LGS) {
                                // 500 Hz by default?
                                td = dit; // adjusted to get high strehl ~ 0.6
                                System.out.println("GPAO_LGS_VIS: real td: " + td);
                                effRon = ron / 900.0 * 2; // 0.4
                                System.out.println("GPAO_LGS_VIS: effRon: " + NumberUtils.trimTo3Digits(effRon));

                                magOffset = -(15.75 - 12.5);
                                System.out.println("magOffset: " + magOffset);

                                createChartFrame("UT (GPAO_LGS_VIS ns=" + nbSubPupils + " dit = " + dit + ") - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                        createStrehlvsMagChart("GPAO_LGS_VIS_" + dit, aoBand, lambda, UT_DIAM, nbSubPupils, nbActuators, td, qe, effRon, magOffset, strehlMax));

                            }
                        }

                        // FAKE GPAO_LGS_IR
                        if (true) {
                            aoBand = Band.K;
                            qe = 0.70;
                            /* 900 modes (3/4) (30*30) */
                            nbSubPupils = 900;
                            nbActuators = 600;
                            strehlMax = 0.85; // 0.93 by default

                            for (double dit : DIT_LGS) {
                                // 500 Hz by default?
                                td = dit; // adjusted to get high strehl ~ 0.6
                                System.out.println("GPAO_LGS_IR: real td: " + td);
                                effRon = ron / 900.0 * 2; // 0.4
                                System.out.println("GPAO_LGS_IR: effRon: " + NumberUtils.trimTo3Digits(effRon));

                                magOffset = -(10.25 - 10.75);
                                System.out.println("magOffset: " + magOffset);

                                createChartFrame("UT (GPAO_LGS_IR ns=" + nbSubPupils + " dit = " + dit + ") - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                        createStrehlvsMagChart("GPAO_LGS_IR_" + dit, aoBand, lambda, UT_DIAM, nbSubPupils, nbActuators, td, qe, effRon, magOffset, strehlMax));

                            }
                        }
                    }
                    // AT:
                    if (false) {
                        if (false) {
                            // STRAP
                            aoBand = Band.V;
                            nbSubPupils = 4;

                            final double qe = 0.8;
                            final double ron = 0.1;
                            td = 1.5;

                            createChartFrame("AT (Tip tilt) - NEW - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                    createATStrehlvsMagChart(aoBand, lambda, nbSubPupils, nbActuators, td, qe, ron));
                        }


                        /* NAOMI: seeing = 1.0" and t0 = 2.5ms */
                        aoBand = Band.R;

                        final double qe = 0.10;
                        final double ron = 64.0;
                        double effRon;

                        /* 14 modes */
                        nbSubPupils = 16;
                        nbActuators = 15;
                        // 500Hz ie max 2ms
                        td = 2.0;
                        effRon = ron / 200.0; // gain = g * KI = 100 * 0.4 = 40
                        System.out.println("effRon: " + NumberUtils.trimTo3Digits(effRon));

                        createChartFrame("AT (NAOMI ns=" + nbSubPupils + ") - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                createATStrehlvsMagChart(aoBand, lambda, nbSubPupils, nbActuators, td, qe, effRon));

                        if (false) {
                            /* transition R> 13 : 7 modes */
                            nbSubPupils = 16;
                            nbActuators = 7;
                            // 100Hz ie max 10s
                            td = 5.0;
                            effRon = ron / 90.0; // gain = g * KI = 100 * 0.9 = 90
                            System.out.println("effRon: " + NumberUtils.trimTo3Digits(effRon));

                            createChartFrame("AT (NAOMI ns=" + nbSubPupils + ") - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                    createATStrehlvsMagChart(aoBand, lambda, nbSubPupils, nbActuators, td, qe, effRon));
                        }
                    }
                }
            }
        });
    }

    private static void createChartFrame(String title, JPanel panel) {
        final JFrame frame = new JFrame(title);
        frame.setPreferredSize(new Dimension(1200, 800));
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        frame.add(panel);
        frame.pack();
        frame.setVisible(true);
    }

    static JPanel createUTStrehlvsMagChart(final Band aoBand, final double[] LAMBDA,
                                           final int nbSubPupils, final int nbActuators,
                                           final double td,
                                           final double qe, final double ron) {

        return createStrehlvsMagChart((aoBand == Band.V) ? "MACAO" : "CIAO", aoBand, LAMBDA, UT_DIAM, nbSubPupils, nbActuators, td, qe, ron);
    }

    static JPanel createATStrehlvsMagChart(final Band aoBand, final double[] LAMBDA,
                                           final int nbSubPupils, final int nbActuators,
                                           final double td,
                                           final double qe, final double ron) {
        return createStrehlvsMagChart((aoBand == Band.V) ? "STRAP" : "NAOMI", aoBand, LAMBDA, 1.8, nbSubPupils, nbActuators, td, qe, ron);
    }

    static JPanel createCHARAStrehlvsMagChart(final Band aoBand, final double[] LAMBDA,
                                              final int nbSubPupils, final int nbActuators,
                                              final double td,
                                              final double qe, final double ron) {
        return createStrehlvsMagChart("TIP-TILT", aoBand, LAMBDA, 1.0, nbSubPupils, nbActuators, td, qe, ron);
    }

    static JPanel createStrehlvsMagChart(final String aoName, final Band aoBand, final double[] LAMBDA,
                                         final double telDiameter,
                                         final int nbSubPupils, final int nbActuators,
                                         final double td,
                                         final double qe, final double ron) {
        return createStrehlvsMagChart(aoName, aoBand, LAMBDA, telDiameter, nbSubPupils, nbActuators, td, qe, ron, 0.0, 0.0);
    }

    static JPanel createStrehlvsMagChart(final String aoName, final Band aoBand, final double[] LAMBDA,
                                         final double telDiameter,
                                         final int nbSubPupils, final int nbActuators,
                                         final double td,
                                         final double qe, final double ron,
                                         final double magOffset,
                                         final double strehlMax) {

        final double lambdaObs = LAMBDA[0] * 1e6;
        System.out.println("Strehl " + aoName + " (@" + lambdaObs + ") AO vs mag(" + aoBand + ")");

        System.out.println("AO setup:"
                + " aoBand: " + aoBand
                + " telDiameter: " + telDiameter
                + " nbSubPupils: " + nbSubPupils
                + " nbActuators: " + nbActuators
                + " td: " + td
                + " quantumEfficiency: " + qe
                + " ron: " + ron
                + " strehlMax: " + strehlMax
        );

        final XYSeriesCollection xySeriesCollection = new XYSeriesCollection();

        for (double seeing : SEEING) {
            final double t0 = t0(seeing);

            System.out.printf("#####\n");
            System.out.printf("#seeing: %.2f\n", seeing);
            System.out.printf("#Mag\tStrehl\n");

            final XYSeries xySeries = new XYSeries("Seeing " + seeing, false, false);

            for (int i = 0; i < MAGS.length; i++) {
                final double mag = MAGS[i];
                final double magoff = mag + magOffset;

                final double strehl = strehl(aoBand, magoff, LAMBDA, telDiameter, seeing, nbSubPupils, nbActuators, td, t0, qe, ron, strehlMax)[0];

                System.out.printf("%.2f\t%.3f\n", mag, strehl);

                xySeries.add(mag, strehl);
            }
            xySeriesCollection.addSeries(xySeries);
        }

        JFreeChart chart = ChartFactory.createXYLineChart(
                "Strehl " + aoName + " @ " + NumberUtils.trimTo3Digits(lambdaObs) + " microns",
                "mag" + aoBand, "Strehl (%)", xySeriesCollection
        );

        org.jfree.chart.ChartUtils.applyCurrentTheme(chart);
        fixRenderer(chart);

        return ChartUtils.createChartPanel(chart, true);
    }

    static double t0(double seeing) {
        if (seeing <= 0.60) {
            return 5.2;
        }
        if (seeing <= 0.7) {
            return 4.4;
        }
        if (seeing <= 1.0) {
            return 3.2;
        }
        if (seeing <= 1.15) {
            return 2.2;
        }
        if (seeing <= 1.4) {
            return 1.6;
        }
        return 1.0;
    }

    static JPanel createUTMacaoStrehlvsSeeingChart(Band aoBand, double[] LAMBDA) {
        final double lambdaObs = LAMBDA[0] * 1e6;
        System.out.println("Strehl(@" + lambdaObs + ") UT AO vs seeing");

        final double mag = 11;
        final double telDiameter = UT_DIAM;

        // MACAO setup:
        final int nbSubPupils = 60;
        final double td = 0.80; // 1050 Hz
        final double qe = 0.90;
        final double ron = 1.0;

        final XYSeriesCollection xySeriesCollection = new XYSeriesCollection();
        final XYSeries xySeries = new XYSeries("Strehl", false, false);

        for (double seeing = 0.4; seeing < 1.5; seeing += 0.1) {
            final double strehl = strehl(aoBand, mag, LAMBDA, telDiameter, seeing, nbSubPupils, nbSubPupils, td, t0(seeing), qe, ron, 0.0)[0];

            xySeries.add(seeing, strehl);
        }
        xySeriesCollection.addSeries(xySeries);

        JFreeChart chart = ChartFactory.createXYLineChart(
                "Strehl @ " + NumberUtils.trimTo3Digits(lambdaObs),
                "seeing", "Strehl (%)", xySeriesCollection
        );

        org.jfree.chart.ChartUtils.applyCurrentTheme(chart);
        fixRenderer(chart);

        return ChartUtils.createChartPanel(chart, true);
    }

    static void fixRenderer(JFreeChart chart) {
        chart.getXYPlot().getRangeAxis().setRange(0.0, 1.0);

        XYLineAndShapeRenderer rdr = ((XYLineAndShapeRenderer) chart.getXYPlot().getRenderer());
        rdr.setDefaultShapesVisible(true);
        rdr.setAutoPopulateSeriesStroke(false);
        rdr.setAutoPopulateSeriesShape(false);
        rdr.setDefaultStroke(new BasicStroke(4f));
        rdr.setDefaultShape(new Rectangle(-7, -7, 14, 14));
    }

    static double[] strehl(final Band aoBand, final double magnitude, final double[] waveLengths,
                           final double diameter, final double seeing, final int nbSubPupils, final int nbActuators,
                           final double td, final double t0, final double quantumEfficiency, final double ron,
                           final double strehlMax) {

        return Band.strehl(aoBand, magnitude, waveLengths, diameter, seeing, nbSubPupils, nbActuators, td, t0, quantumEfficiency, ron, 90.0 - ZENITH_ANGLE, strehlMax);
    }

    static JPanel createStrehlIsoChart(final boolean visWFS) {

        final Band band = Band.G_RP;
        final double seeing = 1.0;
        final double lambdaObs = 2.2e-6;
        final double elevation = 90.0;

        final XYSeriesCollection xySeriesCollection = new XYSeriesCollection();

        for (double h0 : H0) {
            XYSeries xySeries;
            if (true) {
                xySeries = DatasetUtils.sampleFunction2DToSeries(
                        new StrehlIsoFunction(band, visWFS, lambdaObs, seeing, h0, elevation, Double.NaN),
                        0.0, 40.0, 20, "SR_iso NGS_" + (visWFS ? "VIS" : "IR") + " (h0 = " + NumberUtils.trimTo3Digits(h0) + ")"
                );
                xySeriesCollection.addSeries(xySeries);
            }
            if (true) {
                for (double distAs_NGS : new double[]{0.0, 5.0, 10.0, 15.0, 20.0, 30.0, 40.0}) {
                    xySeries = DatasetUtils.sampleFunction2DToSeries(
                            new StrehlIsoFunction(band, visWFS, lambdaObs, seeing, h0, elevation, distAs_NGS),
                            0.0, 40.0, 20, "SR_iso LGS_" + (visWFS ? "VIS" : "IR") + " (h0 = " + NumberUtils.trimTo3Digits(h0) + ", dist_NGS = " + distAs_NGS + " as)"
                    );
                    xySeriesCollection.addSeries(xySeries);
                }
            }
        }

        final JFreeChart chart = ChartFactory.createXYLineChart("Strehl ISO GPAO_" + (visWFS ? "VIS" : "IR"), "dist (as)", "Strehl (%)", xySeriesCollection);

        org.jfree.chart.ChartUtils.applyCurrentTheme(chart);
        fixRenderer(chart);

        return ChartUtils.createChartPanel(chart, true);
    }

    protected final static class StrehlIsoFunction implements Function2D {

        private final Band aoBand;
        private final boolean visWFS;
        private final double[] waveLengths;
        private final double seeing;
        private final double h0;
        private final double elevation;
        private final double distAs_NGS;

        StrehlIsoFunction(final Band band, final boolean visWFS, final double wavelength,
                          final double seeing, final double h0, final double elevation,
                          final double distAs_NGS) {

            this.aoBand = band;
            this.visWFS = visWFS;
            this.waveLengths = new double[]{wavelength};
            this.seeing = seeing;
            this.h0 = h0;
            this.elevation = elevation;
            this.distAs_NGS = distAs_NGS;
        }

        /**
         * Returns the function value.
         *
         * @param x  the x-value.
         *
         * @return f(x).
         */
        public double getValue(double x) {
            final double distAs = x;

            final double[] sr
                           = (distAs_NGS >= 0.0)
                            ? Band.strehl_iso_LGS(aoBand, visWFS, waveLengths, seeing, h0, elevation, distAs, distAs_NGS)
                            : Band.strehl_iso_NGS(aoBand, visWFS, waveLengths, seeing, h0, elevation, distAs);

            return (sr != null) ? sr[0] : Double.NaN;
        }
    }
}
