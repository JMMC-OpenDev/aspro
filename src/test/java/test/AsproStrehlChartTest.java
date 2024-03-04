/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package test;

import static fest.common.FestSwingCustomJUnitTestCase.getProjectFolderPath;
import fr.jmmc.aspro.Aspro2;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.oi.AdaptiveOptics;
import fr.jmmc.aspro.model.oi.AdaptiveOpticsSetup;
import fr.jmmc.aspro.model.oi.AtmosphereQuality;
import fr.jmmc.aspro.model.oi.FocalInstrument;
import fr.jmmc.aspro.model.oi.FocalInstrumentConfiguration;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.aspro.model.oi.Telescope;
import fr.jmmc.aspro.model.util.AtmosphereQualityUtils;
import fr.jmmc.aspro.model.util.SpectralBandUtils;
import fr.jmmc.jmal.Band;
import fr.jmmc.jmcs.Bootstrapper;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.StringUtils;
import fr.jmmc.jmcs.util.concurrent.ThreadExecutors;
import fr.jmmc.oiexplorer.core.gui.chart.ChartUtils;
import fr.jmmc.oiexplorer.core.gui.chart.ColorPalette;
import java.awt.BasicStroke;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.swing.JFrame;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.chart.util.ExportUtils;
import org.jfree.data.function.Function2D;
import org.jfree.data.general.DatasetUtils;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author bourgesl
 */
public class AsproStrehlChartTest {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(RawObservationTest.class.getName());

    private static final List<String> INTERFEROMETER_NAMES = Arrays.asList("VLTI", "CHARA");

    /** absolute path to test folder to save outputs */
    protected final static String TEST_FOLDER = getProjectFolderPath() + "test/";

    private static boolean TEST = false;

    private static double ZENITH_ANGLE = 0.0;

    public static final int MAG_MIN = 0;
    public static final int MAG_MAX = 20;

    private static final double[] MAGS;

    static {
        // Prepare mags:
        MAGS = new double[4 * (MAG_MAX - MAG_MIN) + 1];

        for (int i = 0; i <= 4 * (MAG_MAX - MAG_MIN); i++) {
            MAGS[i] = MAG_MIN + 0.25 * i;
        }
        // System.out.println("Mags: " + Arrays.toString(MAGS));
    }

    public static void main(String[] args) {
        // Start application without GUI:
        Bootstrapper.launchApp(new Aspro2(args), true, true, false);

        test();
    }

    private static void wl(final PrintWriter pw) {
        if (pw != null) {
            pw.println();
        }
    }

    private static void w(final PrintWriter pw, final String s) {
        if (pw != null) {
            pw.println(s);
        }
    }

    private static void test() {
        // to define themes
        ChartUtils.createScientificTickUnits();

        if (true) {
            // strehl iso:
            createChartFrame(null, "Strehl ISO - Strehl vs distance",
                    createStrehlIsoChart());
        }

        final ConfigurationManager cm = ConfigurationManager.getInstance();

        for (String interferometerName : cm.getInterferometerNames()) {
            logger.info("interferometerName: {}", interferometerName);

            if (!INTERFEROMETER_NAMES.contains(interferometerName)) {
                continue;
            }

            try (final PrintWriter pw = new PrintWriter(new FileOutputStream(TEST_FOLDER + "index_" + interferometerName + ".md"))) {

                w(pw, "[![JMMC logo](http://www.jmmc.fr/images/logo.png)](http://www.jmmc.fr)");
                wl(pw);
                wl(pw);
                w(pw, "[![ASPRO 2 Logo](images/Aspro2.png)](http://www.jmmc.fr/aspro)");
                wl(pw);
                wl(pw);

                w(pw, "# ASPRO 2 - Strehl ratios per instrument, telescope and Adaptive Optics (AO) systems");
                wl(pw);
                w(pw, "Date: " + new Date());
                wl(pw);
                // w(pw, "");

                final InterferometerDescription interferometerDescription = cm.getInterferometerDescription(interferometerName);

                final String configurationName = cm.getInterferometerConfigurationNames(interferometerName).get(0);
                logger.info("configurationName: {}", configurationName);

                w(pw, "InterferometerConfiguration: " + configurationName);
                wl(pw);

                final InterferometerConfiguration interferometerConfiguration = cm.getInterferometerConfiguration(configurationName);
                logger.info("InterferometerConfiguration: {}", interferometerConfiguration);

                final Map<Telescope, List<AdaptiveOptics>> telAOs = new LinkedHashMap<>(8);

                for (Telescope tel : interferometerDescription.getTelescopes()) {
                    if (!tel.getAdaptiveOptics().isEmpty()) {
                        telAOs.put(tel, tel.getAdaptiveOptics());
                    }
                }
                logger.info("telAOs: {}", telAOs);

                final double[] lambda = new double[1];

                // Use only instrument available in latest InterferometerConfiguration:
                for (final FocalInstrumentConfiguration insConf : interferometerConfiguration.getInstruments()) {
                    final FocalInstrument instrument = insConf.getFocalInstrument();
                    logger.info("FocalInstrument: {}", instrument);

                    // use alias or real instrument name:
                    final String instrumentName = instrument.getAliasOrName();

                    if ("GRAVITY_FT".equalsIgnoreCase(instrumentName)) {
                        continue;
                    }
                    if (TEST && !"SPICA".equalsIgnoreCase(instrumentName)) {
                        // shortcuts for focused tests
                        continue;
                    }

                    final double lambdaMid = 0.5 * (instrument.getWaveLengthMin() + instrument.getWaveLengthMax());
                    final Band insBand = findBand(lambdaMid);
                    final SpectralBand insSpecBand = SpectralBandUtils.findBand(insBand);

                    logger.info("insBand: {}", insBand);

                    w(pw, "## FocalInstrument: " + instrumentName);
                    wl(pw);
                    w(pw, "Instrument band: " + insBand);
                    wl(pw);

                    for (final Map.Entry<Telescope, List<AdaptiveOptics>> entry : telAOs.entrySet()) {
                        final Telescope telescope = entry.getKey();

                        w(pw, "### Telescope: " + telescope.getName());
                        wl(pw);
                        w(pw, "Telescope diameter (m): " + telescope.getDiameter());
                        wl(pw);

                        for (final AdaptiveOptics ao : entry.getValue()) {
                            if (ao.getInstrumentBand() != null) {
                                // check wavelength range
                                if (ao.getInstrumentBand() != insSpecBand) {
                                    logger.info("skip {} band {} <> band {}", ao.getName(), ao.getInstrumentBand(), insSpecBand);
                                    continue;
                                }
                            }

                            final Band aoBand = Band.valueOf(ao.getBand().name());

                            w(pw, "#### Adaptive Optics: " + ao.getName());
                            wl(pw);
                            w(pw, "AO band: " + aoBand);
                            wl(pw);

                            // Compatible: plot chart:
                            for (final AdaptiveOpticsSetup aoSetup : ao.getSetups()) {
                                logger.info("Processing: [{} band={}] (tel={} ao={} setup={})", instrumentName, insSpecBand, telescope, ao, aoSetup);

                                if (false && !"CIAO_ON_AXIS".equalsIgnoreCase(aoSetup.getName())) {
                                    // shortcuts for GPAO tests
                                    continue;
                                }

                                // define output wavelengths:
                                lambda[0] = insBand.getLambdaFluxZero() * 1E-6;

                                w(pw, "- AO setup: " + aoSetup.getName());
                                wl(pw);

                                createChartFrame(pw,
                                        instrumentName + " " + telescope.getName() + " (" + aoSetup.getName() + ")" + " Strehl ratio " + insBand.getName() + " vs mag" + ao.getBand(),
                                        createChartStrehlRatioVsMag(pw, aoSetup.getName(), aoBand, lambda, telescope, aoSetup)
                                );
                            }

                            wl(pw);
                        }
                    }

                    if (false) {
                        for (final FocalInstrumentMode mode : instrument.getModes()) {
                            logger.info("FocalInstrumentMode: {}", mode);
                        }
                    }
                }
            } catch (IOException ioe) {
                logger.error("IO failure: ", ioe);
            }
        }
    }

    private static void createChartFrame(final PrintWriter pw, final String title, final ChartPanel panel) {

        final File filePNG = new File(TEST_FOLDER, StringUtils.replaceNonAlphaNumericCharsByUnderscore(title) + ".png");
        logger.info("Saving {} ...", filePNG.getAbsolutePath());

        w(pw, "  ![" + title + "](" + filePNG.getName() + ")");
        wl(pw);

        // auto-save as PNG / SVG (slow then run in background):
        ThreadExecutors.getSingleExecutor("AsproStrehlChartTest").submit(new Runnable() {
            @Override
            public void run() {
                try {
                    ExportUtils.writeAsPNG(panel.getChart(), 2000, 1000, filePNG);
                } catch (IOException ioe) {
                    logger.error("IO failure: ", ioe);
                }
                // Once done: show frame:
                SwingUtils.invokeLaterEDT(new Runnable() {
                    @Override
                    public void run() {
                        final JFrame frame = new JFrame(title);
                        frame.setPreferredSize(new Dimension(2000, 1000));
                        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                        frame.add(panel);
                        frame.pack();
                        frame.setVisible(true);
                    }
                });
            }
        });
    }

    static ChartPanel createChartStrehlRatioVsMag(final PrintWriter pw, final String aoName, final Band aoBand, final double[] LAMBDA,
                                                  final Telescope telescope, final AdaptiveOpticsSetup aoSetup) {

        final double lambdaObs = LAMBDA[0] * 1e6;
        System.out.println("Strehl " + aoName + " (@" + lambdaObs + ") AO vs mag(" + aoBand + ")");

        final int nbSubPupils = aoSetup.getNumberSubPupils();
        final int nbActuators;
        if (aoSetup.getNumberActuators() != null) {
            nbActuators = aoSetup.getNumberActuators();
        } else {
            nbActuators = nbSubPupils;
        }
        final double td = aoSetup.getDit();
        final double ron = aoSetup.getRon();
        final double qe = aoSetup.getQuantumEfficiency();

        w(pw, "  - AO band: " + aoBand);
        w(pw, "  - AO nbSubPupils: " + nbSubPupils);
        w(pw, "  - AO nbActuators: " + nbActuators);
        w(pw, "  - AO td: " + td);
        w(pw, "  - AO ron: " + ron);
        w(pw, "  - AO qe: " + qe);
        wl(pw);

        final XYSeriesCollection xySeriesCollection = new XYSeriesCollection();

        for (final AtmosphereQuality atmQual : AtmosphereQualityUtils.ORDERED) {
            final double seeing = AtmosphereQualityUtils.getSeeing(atmQual);
            final double t0 = AtmosphereQualityUtils.getCoherenceTime(atmQual);
            /*
            w(pw, "    - seeing: " + seeing);
            w(pw, "    - t0: " + t0);
             */
            final XYSeries xySeries = new XYSeries("Seeing " + seeing, false, false);

            for (double aoMag : MAGS) {
                final double strehl = strehl(aoBand, aoMag, LAMBDA, telescope.getDiameter(), seeing, nbSubPupils, nbActuators, td, t0, qe, ron)[0];
                xySeries.add(aoMag, strehl);
            }

            xySeriesCollection.addSeries(xySeries);
        }

        JFreeChart chart = ChartFactory.createXYLineChart(
                "Strehl " + aoName + " @ " + NumberUtils.trimTo3Digits(lambdaObs) + " microns",
                "mag" + aoBand, "Strehl (%)", xySeriesCollection
        );

        org.jfree.chart.ChartUtils.applyCurrentTheme(chart);
        fixRenderer(chart, MAG_MIN, MAG_MAX);

        return ChartUtils.createChartPanel(chart, true);
    }

    static void fixRenderer(JFreeChart chart, double xMin, double xMax) {
        chart.getXYPlot().getRangeAxis().setRange(0.0, 1.0);
        chart.getXYPlot().getDomainAxis().setRange(xMin, xMax);

        XYLineAndShapeRenderer rdr = ((XYLineAndShapeRenderer) chart.getXYPlot().getRenderer());
        rdr.setDefaultShapesVisible(true);
        rdr.setAutoPopulateSeriesStroke(false);
        rdr.setAutoPopulateSeriesPaint(false);
        rdr.setAutoPopulateSeriesShape(false);
        rdr.setDefaultStroke(new BasicStroke(4f));
        rdr.setDefaultShape(new Rectangle(-4, -4, 8, 8));

        final ColorPalette palette = ColorPalette.getColorPalette("Armytage");

        final XYPlot plot = (XYPlot) chart.getPlot();

        for (int i = 0, len = plot.getDataset().getSeriesCount(); i < len; i++) {
            rdr.setSeriesPaint(i, palette.getColor(i));
        }
    }

    static double[] strehl(final Band aoBand, final double adaptiveOpticsMag, final double[] waveLengths,
                           final double telDiam, final double seeing, final int nbSubPupils, final int nbActuators,
                           final double td, final double t0, final double qe, final double ron) {

        if (false) {
            System.out.println("setup: seeing: " + seeing
                    + " nbSubPupils: " + nbSubPupils
                    + " nbActuators: " + nbActuators
                    + " td: " + td
                    + " t0: " + t0
                    + " qe: " + qe
                    + " ron: " + ron
            );
        }
        return Band.strehl(aoBand, adaptiveOpticsMag, waveLengths, telDiam, seeing, nbSubPupils, nbActuators,
                td, t0, qe, ron, 90.0 - ZENITH_ANGLE);
    }

    static ChartPanel createStrehlIsoChart() {

        final Band band = Band.R;
        final double lambdaObs = 2.2e-6;
        final double elevation = 90.0;

        final XYSeriesCollection xySeriesCollection = new XYSeriesCollection();

        for (final AtmosphereQuality atmQual : AtmosphereQualityUtils.ORDERED) {
            final double seeing = AtmosphereQualityUtils.getSeeing(atmQual);
            final double h0 = AtmosphereQualityUtils.getTurbulenceHeight(atmQual);

            final XYSeries xySeries = DatasetUtils.sampleFunction2DToSeries(
                    new StrehlIsoFunction(band, lambdaObs, seeing, h0, elevation),
                    0.0, 30.0, 180, "SR_iso(seeing = " + NumberUtils.trimTo3Digits(seeing) + " - h0 = " + NumberUtils.trimTo3Digits(h0) + ")"
            );
            xySeriesCollection.addSeries(xySeries);
        }

        final JFreeChart chart = ChartFactory.createXYLineChart("Strehl ISO", "dist (as)", "Strehl (%)", xySeriesCollection);

        org.jfree.chart.ChartUtils.applyCurrentTheme(chart);
        fixRenderer(chart, 0.0, 30.0);

        return ChartUtils.createChartPanel(chart, true);
    }

    private static Band findBand(final double waveLength) {
        final Band band = Band.findBand(waveLength);
        // TODO: fix that logic to use all possible bands within the instrument bandwidth
        switch (band) {
            case U:
            // avoid 'band U not supported'
            case B:
            case V:
            case R:
            case I:
                // always use V for Visible:
                return Band.V;
            case Q:
                // avoid 'band Q not supported'
                return Band.N;
            default:
                return band;
        }
    }

    protected final static class StrehlIsoFunction implements Function2D {

        private final Band aoBand;
        private final double[] waveLengths;
        private final double seeing;
        private final double h0;
        private final double elevation;

        protected StrehlIsoFunction(final Band band, final double wavelength,
                                    final double seeing, final double h0, final double elevation) {

            this.aoBand = band;
            this.waveLengths = new double[]{wavelength};
            this.seeing = seeing;
            this.h0 = h0;
            this.elevation = elevation;
        }

        /**
         * Returns the function value.
         *
         * @param x  the x-value.
         *
         * @return f(x).
         */
        @Override
        public double getValue(final double x) {
            final double distAs = x;

            final double[] sr = Band.strehl_iso(aoBand, waveLengths, seeing, h0, elevation, distAs);

            return (sr != null) ? sr[0] : Double.NaN;
        }
    }
}
