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
import fr.jmmc.oitools.model.range.Range;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import javax.swing.JFrame;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.DeviationRenderer;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.chart.util.ExportUtils;
import org.jfree.data.function.Function2D;
import org.jfree.data.general.DatasetUtils;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.data.xy.YIntervalSeries;
import org.jfree.data.xy.YIntervalSeriesCollection;
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

    private static boolean SHOW = false;

    private static boolean TEST = false;

    private static double ZENITH_ANGLE = 0.0;

    public static final int MAG_MIN = 0;
    public static final int MAG_MAX = 22;

    private static final double[] MAGS;

    static {
        // Prepare mags:
        MAGS = new double[4 * (MAG_MAX - MAG_MIN) + 1];

        for (int i = 0; i <= 4 * (MAG_MAX - MAG_MIN); i++) {
            MAGS[i] = MAG_MIN + 0.25 * i;
        }
        // System.out.println("Mags: " + Arrays.toString(MAGS));
    }

    private static ThreadExecutors executor = null;
    private static AtomicInteger jobCount = new AtomicInteger();

    public static void main(String[] args) {
        // Start application without GUI:
        Bootstrapper.launchApp(new Aspro2(args), true, true, false);

        if (executor == null) {
            executor = ThreadExecutors.getSingleExecutor("AsproStrehlChartTest");
//            executor = ThreadExecutors.getRunnerExecutor();
        }
        test();

        if (executor != null) {
            int n = 100;
            do {
                logger.info("job count: {}", jobCount.get());

                ThreadExecutors.sleep(500l);
            } while ((jobCount.get() != 0) && (--n >= 0));

            logger.info("job count: {}", jobCount.get());
            Bootstrapper.quitApp(null);
        }
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
                w(pw, "[![ASPRO 2 Logo](https://github.com/JMMC-OpenDev/aspro-doc/blob/main/images/Aspro2.png?raw=true)](http://www.jmmc.fr/aspro)");
                wl(pw);
                wl(pw);

                w(pw, "# ASPRO 2 - Strehl ratios per instrument, telescope and Adaptive Optics (AO) systems");
                wl(pw);
                w(pw, "- Date: " + new Date());
                wl(pw);
                // w(pw, "");

                final InterferometerDescription interferometerDescription = cm.getInterferometerDescription(interferometerName);

                final String configurationName = cm.getInterferometerConfigurationNames(interferometerName).get(0);
                logger.info("configurationName: {}", configurationName);

                w(pw, "- InterferometerConfiguration: '" + configurationName + "'");
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

                final double[] lambda = new double[3]; // min / center / max

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

                    w(pw, "## Instrument [" + instrumentName + "]");
                    wl(pw);

                    final HashMap<Range, Boolean> ranges = new LinkedHashMap<>(8);
                    final HashMap<Range, List<FocalInstrumentMode>> modesPerRange = new LinkedHashMap<>(8);

                    for (final FocalInstrumentMode mode : instrument.getModes()) {
                        logger.debug("FocalInstrumentMode: {}", mode);

                        final Range r = new Range(
                                trimWavelength(mode.getWaveLengthMin()),
                                trimWavelength(mode.getWaveLengthMax())
                        );
                        ranges.put(r, Boolean.FALSE);

                        List<FocalInstrumentMode> modes = modesPerRange.get(r);
                        if (modes == null) {
                            modes = new ArrayList<>(4);
                            modesPerRange.put(r, modes);
                        }
                        modes.add(mode);
                    }

                    logger.debug("FocalInstrumentMode ranges: {}", ranges);
                    logger.debug("FocalInstrumentMode modesPerRange: {}", modesPerRange);

                    for (final FocalInstrumentMode mode : instrument.getModes()) {
                        final Range r = new Range(
                                trimWavelength(mode.getWaveLengthMin()),
                                trimWavelength(mode.getWaveLengthMax())
                        );

                        if (!Boolean.TRUE.equals(ranges.get(r))) {
                            ranges.put(r, Boolean.TRUE);

                            logger.info("FocalInstrumentMode: {}", mode);

                            final StringBuilder sb = new StringBuilder(64).append("[");

                            for (final FocalInstrumentMode m : modesPerRange.get(r)) {
                                sb.append(m.getName()).append(' ');
                            }
                            sb.deleteCharAt(sb.length() - 1);
                            final String modes = sb.append("]").toString();

                            final double lambdaMin = mode.getWaveLengthMin();
                            final double lambdaMax = mode.getWaveLengthMax();
                            final double lambdaMid = mode.getEffWaveLengthRef();

                            final Band insBand = findBand(lambdaMid);
                            final SpectralBand insSpecBand = SpectralBandUtils.findBand(insBand);

                            logger.info("insBand:   {}", insBand);

                            // define output wavelengths:
                            lambda[0] = lambdaMin * 1E-6;
                            lambda[1] = lambdaMid * 1E-6;
                            lambda[2] = lambdaMax * 1E-6;

                            w(pw, "### Instrument Modes " + modes);
                            wl(pw);

                            w(pw, "- Instrument band: " + insBand);
                            w(pw, "- Instrument ref. wavelength : " + NumberUtils.trimTo2Digits(lambdaMid) + " µm");
                            w(pw, "- Instrument min. wavelength : " + NumberUtils.trimTo2Digits(lambdaMin) + " µm");
                            w(pw, "- Instrument max. wavelength : " + NumberUtils.trimTo2Digits(lambdaMax) + " µm");
                            wl(pw);

                            for (final Map.Entry<Telescope, List<AdaptiveOptics>> entry : telAOs.entrySet()) {
                                final Telescope telescope = entry.getKey();

                                w(pw, "#### Telescope [" + telescope.getName() + "]");
                                wl(pw);
                                w(pw, "- Telescope diameter (m): " + telescope.getDiameter());
                                wl(pw);

                                for (final AdaptiveOptics ao : entry.getValue()) {
                                    if (ao.getInstrumentBand() != null) {
                                        // check wavelength range
                                        if (ao.getInstrumentBand() != insSpecBand) {
                                            logger.info("skip {} band {} <> band {}", ao.getName(), ao.getInstrumentBand(), insSpecBand);
                                            continue;
                                        }
                                    }

                                    // Handle G_rp
                                    final Band aoBand = Band.valueOf(ao.getBand().name());

                                    w(pw, "##### Adaptive Optics [" + ao.getName() + "]");
                                    wl(pw);
                                    w(pw, "- AO band: " + aoBand);
                                    wl(pw);

                                    // Compatible: plot chart:
                                    for (final AdaptiveOpticsSetup aoSetup : ao.getSetups()) {
                                        logger.info("Processing: [{} band={}] (tel={} ao={} setup={})", instrumentName, insSpecBand, telescope, ao, aoSetup);

                                        if (false && !"CIAO_ON_AXIS".equalsIgnoreCase(aoSetup.getName())) {
                                            // shortcuts for GPAO tests
                                            continue;
                                        }

                                        w(pw, "- AO setup: '" + aoSetup.getName() + "'");
                                        wl(pw);

                                        createChartFrame(pw,
                                                instrumentName + " " + modes + " " + telescope.getName() + " (" + aoSetup.getName() + ")"
                                                + " Strehl ratio " + insBand.getName() + " vs " + ao.getBand() + " mag",
                                                createChartStrehlRatioVsMag(pw, aoSetup.getName(), aoBand, lambda, telescope, aoSetup,
                                                        instrumentName + " " + modes + " " + telescope.getName())
                                        );
                                    }

                                    wl(pw);
                                }
                            }
                        }
                    }
                }

                if ("VLTI".equals(interferometerName)) {
                    w(pw, "## GPAO VIS:");
                    wl(pw);

                    createChartFrame(pw, "Strehl ISO GPAO_VIS - Strehl vs distance",
                            createStrehlIsoChart(true));

                    w(pw, "## GPAO IR:");
                    wl(pw);

                    createChartFrame(pw, "Strehl ISO GPAO_IR - Strehl vs distance",
                            createStrehlIsoChart(false));
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

        jobCount.incrementAndGet();

        // auto-save as PNG / SVG (slow then run in background):
        executor.submit(new Runnable() {
            @Override
            public void run() {
                try {
                    ExportUtils.writeAsPNG(panel.getChart(), 2000, 1000, filePNG);
                } catch (IOException ioe) {
                    logger.error("IO failure: ", ioe);
                }
                if (SHOW) {
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
                jobCount.decrementAndGet();
            }
        });
    }

    static ChartPanel createChartStrehlRatioVsMag(final PrintWriter pw, final String aoName, final Band aoBand, final double[] LAMBDA,
                                                  final Telescope telescope, final AdaptiveOpticsSetup aoSetup,
                                                  final String setup) {

        final double lambdaObs = LAMBDA[1] * 1e6;
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
        final double magOffset = aoSetup.getMagOffsetOrZero();
        final double strehlMax = aoSetup.getStrehlMaxOrZero();

        w(pw, "- nbSubPupils: " + nbSubPupils);
        w(pw, "- nbActuators: " + nbActuators);
        w(pw, "- td (ms):     " + td);
        w(pw, "- ron (e-/s):  " + ron);
        w(pw, "- Q.E:         " + qe);
        w(pw, "- magOffset:   " + magOffset);
        w(pw, "- strehlMax:   " + strehlMax);
        wl(pw);

        final YIntervalSeriesCollection xySeriesCollection = new YIntervalSeriesCollection();

        for (final AtmosphereQuality atmQual : AtmosphereQualityUtils.ORDERED) {
            final double seeing = AtmosphereQualityUtils.getSeeing(atmQual);
            final double t0 = AtmosphereQualityUtils.getCoherenceTime(atmQual);

            final YIntervalSeries xySeries = new YIntervalSeries("Seeing " + seeing, false, false);

            for (double aoMag : MAGS) {
                final double[] strehls = strehl(aoBand, (aoMag + magOffset), LAMBDA, telescope.getDiameter(),
                        seeing, nbSubPupils, nbActuators, td, t0, qe, ron, strehlMax);

                xySeries.add(aoMag, strehls[1], strehls[0], strehls[2]);
            }

            xySeriesCollection.addSeries(xySeries);
        }

        final JFreeChart chart = ChartFactory.createXYLineChart(
                "Strehl " + aoName + " @ " + NumberUtils.trimTo2Digits(lambdaObs)
                + " [" + NumberUtils.trimTo2Digits(LAMBDA[0] * 1e6)
                + " - " + NumberUtils.trimTo2Digits(LAMBDA[2] * 1e6) + "] µm"
                + " - " + setup,
                aoBand + " mag", "Strehl (%)", xySeriesCollection
        );

        final XYPlot plot = (XYPlot) chart.getPlot();

        final DeviationRenderer renderer = new DeviationRenderer(true, true);
        plot.setRenderer(renderer);

        org.jfree.chart.ChartUtils.applyCurrentTheme(chart);
        fixRenderer(chart, MAG_MIN, MAG_MAX);

        return ChartUtils.createChartPanel(chart, true);
    }

    static void fixRenderer(JFreeChart chart, double xMin, double xMax) {
        chart.getXYPlot().getRangeAxis().setRange(0.0, 1.0);
        chart.getXYPlot().getDomainAxis().setRange(xMin, xMax);

        final XYLineAndShapeRenderer rdr = ((XYLineAndShapeRenderer) chart.getXYPlot().getRenderer());
        rdr.setDefaultShapesVisible(true);
        rdr.setAutoPopulateSeriesStroke(false);
        rdr.setAutoPopulateSeriesPaint(false);
        rdr.setAutoPopulateSeriesShape(false);
        rdr.setDefaultStroke(ChartUtils.VERY_LARGE_STROKE);
        rdr.setDefaultShape(new Rectangle(-4, -4, 8, 8));

        final ColorPalette palette = ColorPalette.getColorPalette("Armytage");

        final XYPlot plot = (XYPlot) chart.getPlot();

        for (int i = 0, len = plot.getDataset().getSeriesCount(); i < len; i++) {
            Color col = palette.getColor(i);
            rdr.setSeriesPaint(i, col);
            rdr.setSeriesFillPaint(i, new Color(col.getRed(), col.getGreen(), col.getBlue(), 48));
        }
    }

    static double[] strehl(final Band aoBand, final double adaptiveOpticsMag, final double[] waveLengths,
                           final double telDiam, final double seeing, final int nbSubPupils, final int nbActuators,
                           final double td, final double t0, final double qe, final double ron, final double strehlMax) {

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
                td, t0, qe, ron, 90.0 - ZENITH_ANGLE, strehlMax);
    }

    static ChartPanel createStrehlIsoChart(final boolean visWFS) {

        final Band band = Band.G_RP;
        final double lambdaObs = 2.2e-6;
        final double elevation = 90.0;

        final XYSeriesCollection xySeriesCollection = new XYSeriesCollection();

        for (final AtmosphereQuality atmQual : AtmosphereQualityUtils.ORDERED) {
            final double seeing = AtmosphereQualityUtils.getSeeing(atmQual);
            final double h0 = AtmosphereQualityUtils.getTurbulenceHeight(atmQual);

            xySeriesCollection.addSeries(DatasetUtils.sampleFunction2DToSeries(
                    new StrehlIsoFunction(band, visWFS, lambdaObs, seeing, h0, elevation, Double.NaN),
                    0.0, 40.0, 20, "SR_iso_NGS_" + (visWFS ? "VIS" : "IR") + " (seeing = " + NumberUtils.trimTo2Digits(seeing) + " - h0 = " + NumberUtils.trimTo2Digits(h0) + ")"
            ));

            xySeriesCollection.addSeries(DatasetUtils.sampleFunction2DToSeries(
                    new StrehlIsoFunction(band, visWFS, lambdaObs, seeing, h0, elevation, 0.0),
                    0.0, 40.0, 20, "SR_iso_LGS_" + (visWFS ? "VIS" : "IR") + " (seeing = " + NumberUtils.trimTo2Digits(seeing) + " - h0 = " + NumberUtils.trimTo2Digits(h0) + ")"
            ));
        }

        final JFreeChart chart = ChartFactory.createXYLineChart("Strehl ISO GPAO_" + (visWFS ? "VIS" : "IR"), "dist (as)", "Strehl (%)", xySeriesCollection);

        org.jfree.chart.ChartUtils.applyCurrentTheme(chart);
        fixRenderer(chart, 0.0, 30.0);

        return ChartUtils.createChartPanel(chart, true);
    }

    private static Band findBand(final double waveLength) {
        final Band band = Band.findBand(waveLength);
        // TODO: fix that logic to use all possible bands within the instrument bandwidth
        switch (band) {
            case U:
            // avoid 'band U not supported' => V
            case B:
            case V:
                // always use V for Visible (UBV):
                return Band.V;
            case Q:
                // avoid 'band Q not supported' => N
                return Band.N;
            default:
                return band;
        }
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

    public static double trimWavelength(final double value) {
        return ((long) Math.round(50.0 * value)) / 50.0;
    }
}
