/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.gui.action.AsproExportPDFAction;
import fr.jmmc.aspro.gui.chart.AsproChartUtils;
import fr.jmmc.aspro.gui.chart.NameLabelGenerator;
import fr.jmmc.aspro.gui.chart.XYZNameDataSet;
import fr.jmmc.aspro.model.InterferometerMapData;
import fr.jmmc.aspro.model.ObservationCollectionMapData;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.event.ObservationEvent;
import fr.jmmc.aspro.model.event.ObservationListener;
import fr.jmmc.aspro.model.oi.ObservationCollection;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.service.InterferometerMapService;
import fr.jmmc.oiexplorer.core.gui.PDFExportable;
import fr.jmmc.oiexplorer.core.gui.chart.ChartUtils;
import fr.jmmc.oiexplorer.core.gui.chart.ColorPalette;
import fr.jmmc.oiexplorer.core.gui.chart.PDFOptions;
import fr.jmmc.oiexplorer.core.gui.chart.SquareChartPanel;
import fr.jmmc.oiexplorer.core.gui.chart.SquareXYPlot;
import fr.jmmc.oiexplorer.core.util.Constants;
import java.awt.Dimension;
import java.util.ArrayList;
import java.util.List;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.event.ChartProgressEvent;
import org.jfree.chart.event.ChartProgressListener;
import org.jfree.chart.labels.ItemLabelAnchor;
import org.jfree.chart.labels.ItemLabelPosition;
import org.jfree.chart.renderer.xy.XYBubbleRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.ui.Layer;
import org.jfree.ui.TextAnchor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This panel presents the interferometer plot (station, base lines ...)
 * @author bourgesl
 */
public final class InterferometerMapPanel extends javax.swing.JPanel implements ChartProgressListener,
        ObservationListener, PDFExportable {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(InterferometerMapPanel.class.getName());

    /* members */
    /** jFreeChart instance */
    private JFreeChart chart;
    /** xy plot instance */
    private SquareXYPlot xyPlot;
    /** JMMC annotation */
    private XYTextAnnotation aJMMC = null;
    /** current configuration to track changes */
    private String configuration = null;

    /* plot data */
    /** observation collection associated with interferometer map data */
    private ObservationCollectionMapData chartData = null;

    /* swing */
    /** chart panel */
    private SquareChartPanel chartPanel;

    /**
     * Constructor
     */
    public InterferometerMapPanel() {
        initComponents();

        postInit();
    }

    /**
     * This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
  // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
  private void initComponents() {

    setLayout(new java.awt.BorderLayout());
  }// </editor-fold>//GEN-END:initComponents

    /**
     * Export the chart component as a PDF document
     */
    @Override
    public void performPDFAction() {
        AsproExportPDFAction.exportPDF(this);
    }

    /**
     * Return the PDF default file name
     * @return PDF default file name
     */
    @Override
    public String getPDFDefaultFileName() {
        if (this.getChartData() != null) {
            final StringBuilder sb = new StringBuilder(32);
            sb.append("MAP_");
            sb.append(this.getChartData().getInterferometerConfiguration(true)).append('_');
            sb.append(this.getChartData().getDisplayConfigurations("_", true)).append('.').append(PDF_EXT);

            return sb.toString();
        }
        return null;
    }

    /**
     * Prepare the chart(s) before exporting them as a PDF document:
     * Performs layout and return PDF options
     * @return PDF options
     */
    public PDFOptions preparePDFExport() {
        return PDFOptions.DEFAULT_PDF_OPTIONS;
    }

    /**
     * Return the chart to export on the given page index
     * @param pageIndex page index (1..n)
     * @return chart
     */
    @Override
    public JFreeChart prepareChart(final int pageIndex) {
        return this.chart;
    }

    /**
     * Callback indicating the chart was processed by the PDF engine
     */
    @Override
    public void postPDFExport() {
        // no-op
    }

    /**
     * This method is useful to set the models and specific features of initialized swing components :
     */
    private void postInit() {

        this.chart = ChartUtils.createSquareXYLineChart(null, null, true);
        this.xyPlot = (SquareXYPlot) this.chart.getPlot();

        final XYItemRenderer lineRenderer = this.xyPlot.getRenderer();

        // Use Bubble Renderer for the first dataset :
        this.xyPlot.setRenderer(0, new XYBubbleRenderer());

        // Use Line Renderer for the second dataset :
        this.xyPlot.setRenderer(1, lineRenderer);

        // hide axes at [0,0] :
        this.xyPlot.setDomainZeroBaselineVisible(false);
        this.xyPlot.setRangeZeroBaselineVisible(false);

        final XYItemRenderer renderer = this.xyPlot.getRenderer();

        // station labels :
        renderer.setBaseItemLabelGenerator(new NameLabelGenerator());
        renderer.setBasePositiveItemLabelPosition(new ItemLabelPosition(ItemLabelAnchor.CENTER, TextAnchor.BOTTOM_RIGHT));
        renderer.setBaseItemLabelsVisible(true);

        // create new JMMC annotation (moving position):
        this.aJMMC = AsproChartUtils.createJMMCAnnotation();
        this.xyPlot.getRenderer().addAnnotation(this.aJMMC, Layer.BACKGROUND);

        // add listener :
        this.chart.addProgressListener(this);
        this.chartPanel = ChartUtils.createSquareChartPanel(this.chart);

        // zoom options :
        this.chartPanel.setDomainZoomable(Constants.ENABLE_ZOOM);
        this.chartPanel.setRangeZoomable(Constants.ENABLE_ZOOM);

        this.chartPanel.setMinimumSize(new Dimension(650, 500));
        this.add(this.chartPanel);
    }

    /**
     * Handle the changed event to plot the interferometer map synchronously.
     * As this instance is the first observation listener, the plot is first done
     * before other plots / computations are done.
     * @param event event
     */
    @Override
    public void onProcess(final ObservationEvent event) {
        if (logger.isDebugEnabled()) {
            logger.debug("event [{}] process IN", event.getType());
        }
        switch (event.getType()) {
            case REFRESH:
                this.plot(event.getObservationCollection());
                break;
            default:
        }
        if (logger.isDebugEnabled()) {
            logger.debug("event [{}] process OUT", event.getType());
        }
    }

    /**
     * Plot the interferometer map synchronously.
     * This code must be executed by the Swing Event Dispatcher thread (EDT)
     * @param obsCollection observation collection to use
     */
    private void plot(final ObservationCollection obsCollection) {
        if (logger.isDebugEnabled()) {
            logger.debug("plot: {}", ObservationManager.toString(obsCollection));
        }

        // create a unique key (interferometer configuration|stations...)
        // to check if the map must be refreshed
        final StringBuilder sb = new StringBuilder(64);
        sb.append(obsCollection.getInterferometerConfiguration()).append('|');
        obsCollection.getAllConfigurations(sb, "|");
        final String config = sb.toString();

        if (!config.equals(this.configuration)) {
            // refresh the plot :
            this.configuration = config;

            logger.debug("plot : refresh");

            final long start = System.nanoTime();

            final List<InterferometerMapData> mapDataList = new ArrayList<InterferometerMapData>(obsCollection.size());

            for (ObservationSetting observation : obsCollection.getObservations()) {
                mapDataList.add(InterferometerMapService.compute(observation));
            }

            this.updatePlot(new ObservationCollectionMapData(obsCollection, mapDataList));

            logger.info("plot : duration = {} ms.", 1e-6d * (System.nanoTime() - start));
        }
    }

    /**
     * Return the chart data
     * @return chart data
     */
    private ObservationCollectionMapData getChartData() {
        return this.chartData;
    }

    /**
     * Define the chart data
     * @param chartData chart data
     */
    private void setChartData(final ObservationCollectionMapData chartData) {
        this.chartData = chartData;
    }

    /**
     * Refresh the plot using chart data.
     * This code is executed by the Swing Event Dispatcher thread (EDT)
     *
     * @param chartData chart data
     */
    private void updatePlot(final ObservationCollectionMapData chartData) {
        // memorize chart data (used by export PDF) :
        setChartData(chartData);

        // disable chart & plot notifications:
        this.chart.setNotify(false);
        this.xyPlot.setNotify(false);
        try {
            // title :
            ChartUtils.clearTextSubTitle(this.chart);

            final StringBuilder sb = new StringBuilder(32);
            sb.append(chartData.getInterferometerConfiguration(false)).append(" - ");
            sb.append(chartData.getDisplayConfigurations(" / "));

            ChartUtils.addSubtitle(this.chart, sb.toString());

            // computed data are valid :
            updateChart(chartData);

            // update theme at end :
            ChartUtilities.applyCurrentTheme(this.chart);

        } finally {
            // restore chart & plot notifications:
            this.xyPlot.setNotify(true);
            this.chart.setNotify(true);
        }
    }

    /**
     * Update the datasets
     * @param chartData chart data
     */
    private void updateChart(final ObservationCollectionMapData chartData) {

        // First map Data is always defined :
        final InterferometerMapData mapData1 = chartData.getFirstMapData();

        // 1 - Stations :

        // define bounds to the maximum value + 10% (before setDataset) :
        final double boxSize = mapData1.getMaxXY() * 1.10d;
        this.xyPlot.defineBounds(boxSize);

        // first plot stations :
        final XYZNameDataSet stationDataSet = new XYZNameDataSet();

        stationDataSet.addSeries("Stations", new double[][]{mapData1.getStationX(), mapData1.getStationY(), mapData1.getDiameter()}, mapData1.getStationName());

        // set the first data set :
        this.xyPlot.setDataset(stationDataSet);

        // 2 - Baselines :

        // renderer for base lines :
        final XYLineAndShapeRenderer renderer = (XYLineAndShapeRenderer) this.xyPlot.getRenderer(1);

        // reset colors :
        renderer.clearSeriesPaints(false);
        // side effect with chart theme :
        renderer.setAutoPopulateSeriesPaint(false);

        final ColorPalette palette = ColorPalette.getDefaultColorPalette();

        final XYSeriesCollection dataset = new XYSeriesCollection();

        XYSeries xySeries = null;

        String[] blName;
        double[] blX1, blY1, blX2, blY2;
        int n;

        final boolean single = chartData.isSingle();

        // Iterate over map data (multi conf) :
        for (InterferometerMapData mapData : chartData.getMapDataList()) {
            blName = mapData.getBaselineName();
            blX1 = mapData.getBaselineStationX1();
            blY1 = mapData.getBaselineStationY1();
            blX2 = mapData.getBaselineStationX2();
            blY2 = mapData.getBaselineStationY2();

            if (!single) {
                // 1 color per configuration (i.e. per XYSeries) :
                xySeries = new XYSeries(mapData.getStationNames(), false);
                xySeries.setNotify(false);

                dataset.addSeries(xySeries);
                n = dataset.getSeriesCount() - 1;
                renderer.setSeriesPaint(n, palette.getColor(n), false);
            }

            for (int i = 0, len = blName.length; i < len; i++) {

                if (single) {
                    // 1 color per base line (i.e. per XYSeries) :
                    xySeries = new XYSeries(blName[i], false);
                    xySeries.setNotify(false);

                    dataset.addSeries(xySeries);
                    n = dataset.getSeriesCount() - 1;
                    renderer.setSeriesPaint(n, palette.getColor(n), false);
                }

                // first station :
                xySeries.add(blX1[i], blY1[i], false);

                // second station :
                xySeries.add(blX2[i], blY2[i], false);

                // add an invalid point to break the line between the 2 segments :
                xySeries.add(Double.NaN, Double.NaN, false);

            } // BL
        }

        // set the second data set :
        this.xyPlot.setDataset(1, dataset);
    }
  // Variables declaration - do not modify//GEN-BEGIN:variables
  // End of variables declaration//GEN-END:variables
    /** drawing started time value */
    private long chartDrawStartTime = 0l;

    /**
     * Handle the chart progress event to log the chart rendering delay
     * @param event chart progress event
     */
    @Override
    public void chartProgress(final ChartProgressEvent event) {
        if (logger.isDebugEnabled()) {
            switch (event.getType()) {
                case ChartProgressEvent.DRAWING_STARTED:
                    this.chartDrawStartTime = System.nanoTime();
                    break;
                case ChartProgressEvent.DRAWING_FINISHED:
                    logger.debug("Drawing chart time = {} ms.", 1e-6d * (System.nanoTime() - this.chartDrawStartTime));
                    this.chartDrawStartTime = 0l;
                    break;
                default:
            }
        }

        // Perform custom operations before/after chart rendering:
        // move JMMC annotation:
        this.aJMMC.setX(this.xyPlot.getDomainAxis().getUpperBound());
        this.aJMMC.setY(this.xyPlot.getRangeAxis().getLowerBound());
    }
}
