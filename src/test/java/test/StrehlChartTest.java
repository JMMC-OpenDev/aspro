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
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

/**
 *
 * @author bourgesl
 */
public class StrehlChartTest {

    private static boolean USE_STREHL_OLD = false;

    private static double ZENITH_ANGLE = 0.0;

    public static final double DEFAULT_TD_OVER_T0 = (1.0 / 5.0);
    public static final int MAG_MIN = 5;
    public static final int MAG_MAX = 20;
    /*
    // CIAO plot mags
    public static final int MAG_MIN = 7;
    public static final int MAG_MAX = 10;
     */
    private static final double[] MAGS = new double[2 * (MAG_MAX - MAG_MIN) + 1];

    private final static double[] LAMBDA_H = new double[]{1.5e-6};
    private final static double[] LAMBDA_K = new double[]{2.2e-6};

    // NAOMI plots @ 1.1, MACAO, CIAO @ 0.85
    private final static double[] SEEING = new double[]{1.1, 1.0, 0.85, 0.7, 0.4};

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
                int nbSubPupils;
                Band aoBand;
                double td;

                if (true) {
                    // OK
                    // UT MACAO:
                    aoBand = Band.V;
                    createChartFrame("UT (MACAO) - Strehl vs seeing",
                            StrehlChartTest.createUTMacaoStrehlvsSeeingChart(aoBand, LAMBDA_K));
                }

                if (true) {
                    // OK
                    // UT:
                    final double qe = 0.90;
                    final double ron = 1.0;

                    Band b = Band.K;
                    aoBand = Band.V;
                    USE_STREHL_OLD = true;
                    createChartFrame("UT (MACAO) - OLD - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                            StrehlChartTest.createUTStrehlvsMagChart(aoBand, LAMBDA_K, 60, -1, -1, 0, 0)); // td, t0 not used
                    USE_STREHL_OLD = false;

                    td = 0.80; // 1050 Hz
                    createChartFrame("UT (MACAO) - NEW - Strehl " + b.getName() + " vs mag" + aoBand.getName() + " td: " + td,
                            StrehlChartTest.createUTStrehlvsMagChart(aoBand, LAMBDA_K, 60, td, -1, qe, ron)); // auto t0
                }

                if (true) {
                    ZENITH_ANGLE = 30.0;
                    // OK
                    double qe = 0.70;
                    double ron = 6.5;

                    Band b = Band.K;
                    aoBand = Band.K;
                    td = 1.75; // 500Hz
                    createChartFrame("UT (CIAO Off-axis) - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                            StrehlChartTest.createUTStrehlvsMagChart(aoBand, LAMBDA_K, 9 * 9, td, 4.0, qe, ron));

                    qe *= 0.5; // half flux
                    ron = 5.0;
                    td = 1.5; // 500Hz
                    createChartFrame("UT (CIAO ON-axis) - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                            StrehlChartTest.createUTStrehlvsMagChart(aoBand, LAMBDA_K, 9 * 9, td, 4.0, qe, ron));

                    ZENITH_ANGLE = 0.0;
                }
                if (true) {
                    final Band b = Band.H;
                    // AT:
                    if (true) {
                        aoBand = Band.V;
                        nbSubPupils = 2;

                        final double qe = 0.8;
                        final double ron = 1.0;
                        td = 1.0; // STRAP ??

                        USE_STREHL_OLD = true;
                        createChartFrame("AT (Tip tilt) - OLD - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                StrehlChartTest.createATStrehlvsMagChart(aoBand, LAMBDA_H, nbSubPupils, -1, -1, 0, 0)); // td, t0 not used

                        USE_STREHL_OLD = false;
                        createChartFrame("AT (Tip tilt) - NEW - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                StrehlChartTest.createATStrehlvsMagChart(aoBand, LAMBDA_H, nbSubPupils, td, -1, qe, ron)); // auto t0
                    }

                    /* NAOMI: seeing = 1.0" and t0 = 2.5ms */
                    aoBand = Band.R;

                    final double qe = 0.95;
                    final double ron = 1.0;
                    double effRon, t0;

                    /* 14 modes */
                    nbSubPupils = 9 * 9; // or 6 * 6;
                    // 500Hz ie max 2ms
                    td = 2.0;
                    t0 = 2.5 * td;
                    effRon = ron / 40.0; // gain = g * KI = 100 * 0.4 = 40

                    createChartFrame("AT (NAOMI ns=" + nbSubPupils + ") - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                            StrehlChartTest.createATStrehlvsMagChart(aoBand, LAMBDA_H, nbSubPupils, td, t0, qe, effRon));

                    /* transition R> 13 : 7 modes */
                    nbSubPupils = 7 * 7; // or 5 * 5;
                    // 100Hz ie max 10s
                    td = 5.0;
                    t0 = 1.3 * td;
                    effRon = ron / 90.0; // gain = g * KI = 100 * 0.9 = 90

                    createChartFrame("AT (NAOMI ns=" + nbSubPupils + ") - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                            StrehlChartTest.createATStrehlvsMagChart(aoBand, LAMBDA_H, nbSubPupils, td, t0, qe, effRon));
                }
                if (true) {
                    return;
                }

                final double[] lambda = new double[1];

                for (Band b : EnumSet.of(Band.K /*Band.K, Band.L, Band.M */ /*, Band.N */)) {
                    lambda[0] = b.getLambda() * 1E-6;

                    if (true) {
                        // OK
                        // UT:
                        final double qe = 0.90;
                        final double ron = 1.0;

                        aoBand = Band.V;
                        td = 0.80; // 1050 Hz
                        createChartFrame("UT (MACAO) - NEW - Strehl " + b.getName() + " vs mag" + aoBand.getName() + " td: " + td,
                                StrehlChartTest.createUTStrehlvsMagChart(aoBand, LAMBDA_K, 60, td, -1, qe, ron)); // auto t0
                    }
                    if (true) {
                        // OK
                        double qe = 0.70;
                        double ron = 6.5;

                        aoBand = Band.K;
                        td = 1.75; // 500Hz
                        createChartFrame("UT (CIAO Off-axis) - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                StrehlChartTest.createUTStrehlvsMagChart(aoBand, LAMBDA_K, 9 * 9, td, 4.0, qe, ron));

                        qe *= 0.5; // half flux
                        td = 1.5; // 500Hz
                        ron = 5.0;
                        createChartFrame("UT (CIAO ON-axis) - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                StrehlChartTest.createUTStrehlvsMagChart(aoBand, LAMBDA_K, 9 * 9, td, 4.0, qe, ron));
                    }
                    if (false) {
                        // AT:
                        if (true) {
                            aoBand = Band.V;
                            nbSubPupils = 2;

                            final double qe = 0.90;
                            final double ron = 1.0;
                            td = 1.0; // STRAP ??

                            USE_STREHL_OLD = true;
                            createChartFrame("AT (Tip tilt) - OLD - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                    StrehlChartTest.createATStrehlvsMagChart(aoBand, LAMBDA_H, nbSubPupils, -1, -1, 0, 0)); // td, t0 not used

                            USE_STREHL_OLD = false;
                            createChartFrame("AT (Tip tilt) - NEW - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                    StrehlChartTest.createATStrehlvsMagChart(aoBand, LAMBDA_H, nbSubPupils, td, -1, qe, ron)); // auto t0
                        }

                        /* NAOMI: seeing = 1.1" and t0 = 2.5ms */
                        aoBand = Band.R;

                        final double qe = 0.90;
                        final double ron = 0.9;

                        /* 14 modes */ /* 6 * 6 */
                        nbSubPupils = 14 * 2;
                        // 500Hz ie max 2ms
                        td = 0.75;

                        createChartFrame("AT (NAOMI ns=" + nbSubPupils + ") - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                StrehlChartTest.createATStrehlvsMagChart(aoBand, LAMBDA_H, nbSubPupils, td, 2.5, qe, ron));

                        /* transition R> 13 : 7 modes */
                        nbSubPupils = 7 * 2;
                        // 100Hz ie max 10s
                        td = 1.5;

                        createChartFrame("AT (NAOMI ns=" + nbSubPupils + ") - Strehl " + b.getName() + " vs mag" + aoBand.getName(),
                                StrehlChartTest.createATStrehlvsMagChart(aoBand, LAMBDA_H, nbSubPupils, td, 2.5, qe, ron));
                    }
                }
            }
        });
    }

    private static void createChartFrame(String title, JPanel panel) {
        final JFrame frame = new JFrame(title);
        frame.setPreferredSize(new Dimension(1800, 1000));
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        frame.add(panel);
        frame.pack();
        frame.setVisible(true);
    }

    static JPanel createUTStrehlvsMagChart(final Band aoBand, final double[] LAMBDA,
                                           final int nbSubPupils,
                                           final double td, final double t0,
                                           final double qe, final double ron) {

        return createStrehlvsMagChart((aoBand == Band.V) ? "MACAO" : "CIAO", aoBand, LAMBDA, 8.2, nbSubPupils, td, t0, qe, ron);
    }

    static JPanel createATStrehlvsMagChart(final Band aoBand, final double[] LAMBDA,
                                           final int nbSubPupils,
                                           final double td, final double t0,
                                           final double qe, final double ron) {
        return createStrehlvsMagChart((aoBand == Band.V) ? "STRAP" : "NAOMI", aoBand, LAMBDA, 1.8, nbSubPupils, td, t0, qe, ron);
    }

    static JPanel createStrehlvsMagChart(final String aoName, final Band aoBand, final double[] LAMBDA,
                                         final double telDiameter,
                                         final int nbSubPupils,
                                         final double td, final double t0,
                                         final double qe, final double ron) {

        final double lambdaObs = LAMBDA[0] * 1e6;
        System.out.println("Strehl " + aoName + " (@" + lambdaObs + ") UT AO vs mag(" + aoBand + ")");

        final XYSeriesCollection xySeriesCollection = new XYSeriesCollection();

        for (double seeing : SEEING) {
            final XYSeries xySeries = new XYSeries("Seeing " + seeing, false, false);

            for (double mag : MAGS) {
                final double strehl = (USE_STREHL_OLD) ? strehl(aoBand, mag, LAMBDA, telDiameter, seeing, nbSubPupils, td, (t0 > 0.0) ? t0 : t0(seeing))[0]
                        : strehl(aoBand, mag, LAMBDA, telDiameter, seeing, nbSubPupils, td, (t0 > 0.0) ? t0 : t0(seeing),
                                (qe > 0.0) ? qe : Band.DEFAULT_QE, (ron > 0.0) ? ron : Band.DEFAULT_RON)[0];
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
        if (seeing <= 0.45) {
            return 10.0;
        }
        if (seeing <= 0.75) {
            return 6.0;
        }
        if (seeing <= 0.95) {
            return 4.0;
        }
        if (seeing <= 1.05) {
            return 3.0;
        }
        if (seeing <= 1.15) {
            return 2.0;
        }
        return 1.0;
    }

    static JPanel createUTMacaoStrehlvsSeeingChart(Band aoBand, double[] LAMBDA) {
        final double lambdaObs = LAMBDA[0] * 1e6;
        System.out.println("Strehl(@" + lambdaObs + ") UT AO vs seeing");

        final double mag = 11;
        final double telDiameter = 8.2;

        // MACAO setup:
        final int nbSubPupils = 60;
        final double td = 0.80; // 1050 Hz
        final double qe = 0.90;
        final double ron = 1.0;

        final XYSeriesCollection xySeriesCollection = new XYSeriesCollection();
        final XYSeries xySeries = new XYSeries("Strehl", false, false);

        for (double seeing = 0.4; seeing < 1.5; seeing += 0.1) {
            final double strehl = strehl(aoBand, mag, LAMBDA, telDiameter, seeing, nbSubPupils, td, t0(seeing), qe, ron)[0];

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
                           final double diameter, final double seeing, final int nbSubPupils,
                           final double td, final double t0) {
        if (USE_STREHL_OLD) {
            return Band.strehlOLD(magnitude, waveLengths, diameter, seeing, nbSubPupils, 90.0);
        }
        return Band.strehl(aoBand, magnitude, waveLengths, diameter, seeing, nbSubPupils, td, t0, 90.0);
    }

    static double[] strehl(final Band aoBand, final double magnitude, final double[] waveLengths,
                           final double diameter, final double seeing, final int nbSubPupils,
                           final double td, final double t0, final double quantumEfficiency, final double ron) {
        return Band.strehl(aoBand, magnitude, waveLengths, diameter, seeing, nbSubPupils, td, t0, quantumEfficiency, ron, 90.0 - ZENITH_ANGLE);
    }

}
