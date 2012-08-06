/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.gui.action.ExportPDFAction;
import fr.jmmc.aspro.gui.chart.BoundedNumberAxis;
import fr.jmmc.aspro.gui.chart.ChartMouseSelectionListener;
import fr.jmmc.aspro.gui.chart.ChartUtils;
import fr.jmmc.aspro.gui.chart.CombinedCrosshairOverlay;
import fr.jmmc.aspro.gui.chart.EnhancedChartMouseListener;
import fr.jmmc.aspro.gui.chart.FastXYErrorRenderer;
import fr.jmmc.aspro.gui.chart.PDFOptions;
import fr.jmmc.aspro.gui.chart.PDFOptions.Orientation;
import fr.jmmc.aspro.gui.chart.PDFOptions.PageSize;
import fr.jmmc.aspro.gui.chart.SelectionOverlay;
import fr.jmmc.aspro.model.event.OIFitsEvent;
import fr.jmmc.aspro.model.event.ObservationEvent;
import fr.jmmc.aspro.model.event.ObservationListener;
import fr.jmmc.jmal.image.ColorModels;
import fr.jmmc.jmal.image.ImageUtils;
import fr.jmmc.oitools.model.OIArray;
import fr.jmmc.oitools.model.OIData;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIT3;
import fr.jmmc.oitools.model.OIVis2;
import java.awt.Color;
import java.awt.Point;
import java.awt.Shape;
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.IndexColorModel;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import org.jfree.chart.ChartMouseEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.axis.LogarithmicAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.event.ChartProgressEvent;
import org.jfree.chart.event.ChartProgressListener;
import org.jfree.chart.plot.CombinedDomainXYPlot;
import org.jfree.chart.plot.Crosshair;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.PlotRenderingInfo;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.AbstractRenderer;
import org.jfree.data.Range;
import org.jfree.data.xy.DefaultIntervalXYDataset;
import org.jfree.data.xy.XYDataset;
import org.jfree.ui.Layer;
import org.jfree.ui.RectangleInsets;
import org.jfree.ui.TextAnchor;

/**
 * This panel presents the interferometer plot (station, base lines ...)
 * @author bourgesl
 */
public final class Vis2Panel extends javax.swing.JPanel implements ChartProgressListener, EnhancedChartMouseListener, ChartMouseSelectionListener,
        ObservationListener, PDFExportable {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(Vis2Panel.class.getName());
  /** flag to enable log axis to display log(Vis2) */
  private final static boolean USE_LOG_SCALE = false;
  /** default color model (aspro - Rainbow) */
  private final static IndexColorModel RAINBOW_COLOR_MODEL = ColorModels.getColorModel("Rainbow");
  /** scaling factor to Mega Lambda for U,V points */
  private final static double MEGA_LAMBDA_SCALE = 1e-6;
  /** data margin in percents */
  private final static double MARGIN_PERCENTS = 5d / 100d;
  /** double formatter for wave lengths */
  private final static NumberFormat df4 = new DecimalFormat("0.000#");

  /* members */
  /** jFreeChart instance */
  private JFreeChart chart;
  /** combined xy plot sharing domain axis */
  private CombinedDomainXYPlot combinedXYPlot;
  /** mapping between xy plot and subplot index */
  private Map<XYPlot, Integer> plotMapping = new IdentityHashMap<XYPlot, Integer>();
  /** mapping between subplot index and xy plot (reverse) */
  private Map<Integer, XYPlot> plotIndexMapping = new HashMap<Integer, XYPlot>();
  /** xy plot instance for VIS2 */
  private XYPlot xyPlotVis2;
  /** xy plot instance for T3 */
  private XYPlot xyPlotT3;
  /** JMMC annotation */
  private XYTextAnnotation aJMMCVis2 = null;
  /** JMMC annotation */
  private XYTextAnnotation aJMMCT3 = null;
  /** uv coordinates scaling factor */
  private double uvPlotScalingFactor = MEGA_LAMBDA_SCALE;
  /* plot data */
  /** current oifits file to track changes */
  private OIFitsFile oiFitsFile = null;
  /* swing */
  /** chart panel */
  private ChartPanel chartPanel;
  /** crosshair overlay */
  private CombinedCrosshairOverlay crosshairOverlay = null;
  /** selection overlay */
  private SelectionOverlay selectionOverlay = null;

  /**
   * Constructor
   */
  public Vis2Panel() {
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

        jPanelCenter = new javax.swing.JPanel();
        jLabelMessage = new javax.swing.JLabel();

        setLayout(new java.awt.BorderLayout());

        jPanelCenter.setBackground(new java.awt.Color(255, 255, 255));
        jPanelCenter.setLayout(new javax.swing.BoxLayout(jPanelCenter, javax.swing.BoxLayout.Y_AXIS));

        jLabelMessage.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        jLabelMessage.setText("LABEL");
        jLabelMessage.setAlignmentX(0.5F);
        jPanelCenter.add(jLabelMessage);

        add(jPanelCenter, java.awt.BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents

  /**
   * Export the chart component as a PDF document
   */
  @Override
  public void performPDFAction() {
    // if no OIFits data, discard action:
    if (this.oiFitsFile != null) {
      ExportPDFAction.exportPDF(this);
    }
  }

  /**
   * Return the PDF default file name
   * [Vis2_<TARGET>_<INSTRUMENT>_<CONFIGURATION>_<DATE>]
   * @return PDF default file name
   */
  @Override
  public String getPDFDefaultFileName() {
    if (this.oiFitsFile != null) {
      final StringBuilder sb = new StringBuilder(32).append("Vis2_");

      final String targetName = this.oiFitsFile.getOiTarget().getTarget()[0];
      final String altName = targetName.replaceAll(AsproConstants.REGEXP_INVALID_TEXT_CHARS, "_");

      sb.append(altName).append('_');

      final OIVis2 vis2 = this.oiFitsFile.getOiVis2()[0];

      final String arrayName = vis2.getArrName();
      final String insName = vis2.getInsName();

      sb.append(insName).append('_');

      final OIArray array = this.oiFitsFile.getOiArray(arrayName);
      for (String station : array.getStaName()) {
        sb.append(station).append('-');
      }
      sb.deleteCharAt(sb.length() - 1);
      sb.append('_');

      final String dateObs = vis2.getDateObs();

      sb.append(dateObs);

      sb.append('.').append(PDF_EXT);

      return sb.toString();
    }
    return null;
  }

  /**
   * Return the PDF options
   * @return PDF options
   */
  @Override
  public PDFOptions getPDFOptions() {
    return new PDFOptions(PageSize.A3, Orientation.Landscape);
  }

  /**
   * Return the chart to export as a PDF document
   * @return chart
   */
  @Override
  public JFreeChart prepareChart() {
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

    final boolean usePlotCrossHairSupport = false;
    final boolean useSelectionSupport = false;

    this.xyPlotVis2 = createScientificScatterPlot("UV radius (M\u03BB)", "V²", usePlotCrossHairSupport);

    this.aJMMCVis2 = ChartUtils.createXYTextAnnotation(AsproConstants.JMMC_ANNOTATION, 0, 0);
    this.aJMMCVis2.setTextAnchor(TextAnchor.BOTTOM_RIGHT);
    this.aJMMCVis2.setPaint(Color.DARK_GRAY);
    this.xyPlotVis2.getRenderer().addAnnotation(this.aJMMCVis2, Layer.BACKGROUND);

    this.xyPlotT3 = createScientificScatterPlot("UV radius (M\u03BB)", "Closure phase (deg)", usePlotCrossHairSupport);
    this.xyPlotT3.setNoDataMessage("No closure phase data (OI_T3)");

    this.aJMMCT3 = ChartUtils.createXYTextAnnotation(AsproConstants.JMMC_ANNOTATION, 0, 0);
    this.aJMMCT3.setTextAnchor(TextAnchor.BOTTOM_RIGHT);
    this.aJMMCT3.setPaint(Color.DARK_GRAY);
    this.xyPlotT3.getRenderer().addAnnotation(this.aJMMCT3, Layer.BACKGROUND);

    final ValueAxis domainAxis = this.xyPlotVis2.getDomainAxis();

    // create chart and add listener :
    this.combinedXYPlot = new CombinedDomainXYPlot(domainAxis);
    this.combinedXYPlot.setGap(10.0D);
    this.combinedXYPlot.setOrientation(PlotOrientation.VERTICAL);
    this.combinedXYPlot.add(this.xyPlotVis2, 1);
    this.combinedXYPlot.add(this.xyPlotT3, 1);

    this.plotMapping.put(this.xyPlotVis2, Integer.valueOf(1));
    this.plotIndexMapping.put(Integer.valueOf(1), this.xyPlotVis2);
    this.plotMapping.put(this.xyPlotT3, Integer.valueOf(2));
    this.plotIndexMapping.put(Integer.valueOf(2), this.xyPlotT3);

    configureCrosshair(this.combinedXYPlot, usePlotCrossHairSupport);

    this.chart = ChartUtils.createChart(null, this.combinedXYPlot, false);
    this.chart.addProgressListener(this);
    this.chartPanel = ChartUtils.createChartPanel(this.chart, false);

    // zoom options :
    this.chartPanel.setDomainZoomable(AsproConstants.ENABLE_ZOOM);
    this.chartPanel.setRangeZoomable(AsproConstants.ENABLE_ZOOM);

    // enable mouse wheel:
    this.chartPanel.setMouseWheelEnabled(true);

    if (useSelectionSupport) {
      this.selectionOverlay = new SelectionOverlay(this.chartPanel, this);
      this.chartPanel.addOverlay(this.selectionOverlay);
    }

    if (!usePlotCrossHairSupport) {
      this.crosshairOverlay = new CombinedCrosshairOverlay();

      for (Integer plotIndex : this.plotMapping.values()) {
        crosshairOverlay.addDomainCrosshair(plotIndex, createCrosshair());
        crosshairOverlay.addRangeCrosshair(plotIndex, createCrosshair());
      }

      this.chartPanel.addOverlay(crosshairOverlay);
    }

    if (useSelectionSupport || !usePlotCrossHairSupport) {
      this.chartPanel.addChartMouseListener(this);
    }

    this.jPanelCenter.add(this.chartPanel);
  }

  private static Crosshair createCrosshair() {
    final Crosshair crosshair = new Crosshair(Double.NaN);
    crosshair.setPaint(Color.BLUE);
    crosshair.setLabelVisible(true);
    crosshair.setLabelFont(ChartUtils.DEFAULT_TEXT_SMALL_FONT);
    crosshair.setLabelBackgroundPaint(new Color(255, 255, 0, 200));
    return crosshair;
  }

  /**
   * Create custom scatter plot with several display options (error renderer)
   * @param xAxisLabel x axis label
   * @param yAxisLabel y axis label
   * @param usePlotCrossHairSupport flag to use internal crosshair support on plot
   * @return xy plot
   */
  private static XYPlot createScientificScatterPlot(final String xAxisLabel, final String yAxisLabel, final boolean usePlotCrossHairSupport) {

    final XYPlot plot = ChartUtils.createScatterPlot(null, xAxisLabel, yAxisLabel, null, PlotOrientation.VERTICAL, false, false, false);

    // enlarge right margin to have last displayed value:
    plot.setInsets(new RectangleInsets(2d, 10d, 2d, 20d));

    configureCrosshair(plot, usePlotCrossHairSupport);

    plot.setDomainGridlinePaint(Color.LIGHT_GRAY);
    plot.setRangeGridlinePaint(Color.LIGHT_GRAY);

    // use custom units :
    plot.getRangeAxis().setStandardTickUnits(ChartUtils.createScientificTickUnits());
    plot.getDomainAxis().setStandardTickUnits(ChartUtils.createScientificTickUnits());

    final FastXYErrorRenderer renderer = (FastXYErrorRenderer) plot.getRenderer();

    // only display Y error:
    renderer.setDrawXError(false);

    // force to use the base shape
    renderer.setAutoPopulateSeriesShape(false);

    // reset colors :
    renderer.setAutoPopulateSeriesPaint(false);
    renderer.clearSeriesPaints(false);

    // define deprecated methods to set renderer options for ALL series (performance):
    renderer.setDrawOutlines(false);
    renderer.setShapesVisible(true);
    renderer.setShapesFilled(true);
    renderer.setLinesVisible(false);
    renderer.setItemLabelsVisible(false);

    // define error bar settings:
    renderer.setErrorStroke(AbstractRenderer.DEFAULT_STROKE);
    renderer.setCapLength(0d);
    renderer.setErrorPaint(new Color(192, 192, 192, 128));

    return plot;
  }

  private static void configureCrosshair(final XYPlot plot, final boolean usePlotCrossHairSupport) {
    // configure xyplot or overlay crosshairs:
    plot.setDomainCrosshairLockedOnData(usePlotCrossHairSupport);
    plot.setDomainCrosshairVisible(usePlotCrossHairSupport);

    plot.setRangeCrosshairLockedOnData(usePlotCrossHairSupport);
    plot.setRangeCrosshairVisible(usePlotCrossHairSupport);
  }

  /* EnhancedChartMouseListener implementation */
  /**
   * Return true if this listener implements / uses this mouse event type
   * @param eventType mouse event type
   * @return true if this listener implements / uses this mouse event type
   */
  @Override
  public boolean support(final int eventType) {
    return (eventType == EnhancedChartMouseListener.EVENT_CLICKED);
  }

  /**
   * Handle click on plot
   * @param chartMouseEvent chart mouse event
   */
  @Override
  public void chartMouseClicked(final ChartMouseEvent chartMouseEvent) {
    final int i = chartMouseEvent.getTrigger().getX();
    final int j = chartMouseEvent.getTrigger().getY();

    if (this.chartPanel.getScreenDataArea().contains(i, j)) {
      final Point2D point2D = this.chartPanel.translateScreenToJava2D(new Point(i, j));

      final PlotRenderingInfo plotInfo = this.chartPanel.getChartRenderingInfo().getPlotInfo();

      final int subplotIndex = plotInfo.getSubplotIndex(point2D);
      if (subplotIndex == -1) {
        return;
      }

      // data area for sub plot:
      final Rectangle2D dataArea = plotInfo.getSubplotInfo(subplotIndex).getDataArea();

      final Integer plotIndex = Integer.valueOf(subplotIndex + 1);

      final XYPlot plot = this.plotIndexMapping.get(plotIndex);
      if (plot == null) {
        return;
      }

      final ValueAxis domainAxis = plot.getDomainAxis();
      final double domainValue = domainAxis.java2DToValue(point2D.getX(), dataArea, plot.getDomainAxisEdge());

      final ValueAxis rangeAxis = plot.getRangeAxis();
      final double rangeValue = rangeAxis.java2DToValue(point2D.getY(), dataArea, plot.getRangeAxisEdge());

      logger.warn("Mouse coordinates are (" + i + ", " + j + "), in data space = (" + domainValue + ", " + rangeValue + ")");

      // aspect ratio:
      final double xRatio = dataArea.getWidth() / Math.abs(domainAxis.getUpperBound() - domainAxis.getLowerBound());
      final double yRatio = dataArea.getHeight() / Math.abs(rangeAxis.getUpperBound() - rangeAxis.getLowerBound());

      // find matching data ie. closest data point according to its screen distance to the mouse clicked point:
      Point2D dataPoint = findDataPoint(plot, domainValue, rangeValue, xRatio, yRatio);

      List<Crosshair> xCrosshairs = this.crosshairOverlay.getDomainCrosshairs(plotIndex);
      if (xCrosshairs.size() == 1) {
        xCrosshairs.get(0).setValue(dataPoint.getX());
      }
      List<Crosshair> yCrosshairs = this.crosshairOverlay.getRangeCrosshairs(plotIndex);
      if (yCrosshairs.size() == 1) {
        yCrosshairs.get(0).setValue(dataPoint.getY());
      }

      // update other plot crosshairs:
      for (Integer index : this.plotIndexMapping.keySet()) {
        if (index != plotIndex) {
          final XYPlot otherPlot = this.plotIndexMapping.get(index);
          if (otherPlot != null) {
            xCrosshairs = this.crosshairOverlay.getDomainCrosshairs(index);
            if (xCrosshairs.size() == 1) {
              xCrosshairs.get(0).setValue(dataPoint.getX());
            }
            yCrosshairs = this.crosshairOverlay.getRangeCrosshairs(index);
            if (yCrosshairs.size() == 1) {
              yCrosshairs.get(0).setValue(Double.NaN);
            }
          }
        }
      }
    }
  }

  /**
   * Not implemented
   * @param chartMouseEvent useless
   */
  @Override
  public void chartMouseMoved(final ChartMouseEvent chartMouseEvent) {
    if (false) {
      chartMouseClicked(chartMouseEvent);
    }
  }

  /**
   * Handle rectangular selection event
   *
   * @param selection the selected region.
   */
  @Override
  public void mouseSelected(final Rectangle2D selection) {
    logger.warn("mouseSelected: rectangle {}", selection);

    // find data points:
    final List<Point2D> points = findDataPoints(selection);

    this.selectionOverlay.setRectSelArea(selection);

    // push data points to overlay for rendering:
    this.selectionOverlay.setPoints(points);
  }

  /**
   * Find data point closest in FIRST dataset to the given coordinates X / Y
   * @param plot xy plot to get its dataset
   * @param anchorX domain axis coordinate
   * @param anchorY range axis coordinate
   * @param xRatio pixels per data on domain axis
   * @param yRatio pixels per data on range axis
   * @return found Point2D (data coordinates) or Point2D(NaN, NaN)
   */
  private static Point2D findDataPoint(final XYPlot plot, final double anchorX, final double anchorY, final double xRatio, final double yRatio) {
    final XYDataset dataset = plot.getDataset();

    // TODO: move such code elsewhere : ChartUtils or XYDataSetUtils ?

    final long startTime = System.nanoTime();

    double minDistance = Double.POSITIVE_INFINITY;
    int matchSerie = -1;
    int matchItem = -1;

    double x, y, dx, dy, distance;

    // NOTE: not optimized

    // standard case - plain XYDataset
    for (int serie = 0, seriesCount = dataset.getSeriesCount(), item, itemCount; serie < seriesCount; serie++) {
      itemCount = dataset.getItemCount(serie);
      for (item = 0; item < itemCount; item++) {
        x = dataset.getXValue(serie, item);
        y = dataset.getYValue(serie, item);

        if (!Double.isNaN(x) && !Double.isNaN(y)) {
          // converted in pixels:
          dx = (x - anchorX) * xRatio;
          dy = (y - anchorY) * yRatio;

          distance = dx * dx + dy * dy;

          if (distance < minDistance) {
            minDistance = distance;
            matchSerie = serie;
            matchItem = item;
          }
        }
      }
    }

    logger.warn("findDataPoint: time = {} ms.", 1e-6d * (System.nanoTime() - startTime));

    if (matchItem != -1) {
      final double matchX = dataset.getXValue(matchSerie, matchItem);
      final double matchY = dataset.getYValue(matchSerie, matchItem);

      logger.warn("Matching item [serie = " + matchSerie + ", item = " + matchItem + "] : (" + matchX + ", " + matchY + ")");

      return new Point2D.Double(matchX, matchY);
    }

    logger.warn("No Matching item.");

    return new Point2D.Double(Double.NaN, Double.NaN);
  }

  /**
   * Find data points inside the given Shape (data coordinates)
   * @param shape shape to use
   * @return found list of Point2D (data coordinates) or empty list
   */
  private List<Point2D> findDataPoints(final Shape shape) {
    final XYDataset dataset = this.xyPlotVis2.getDataset();

    // TODO: move such code elsewhere : ChartUtils or XYDataSetUtils ?

    final long startTime = System.nanoTime();
    /*
     int matchSerie = -1;
     int matchItem = -1;
     */
    double x, y;

    final List<Point2D> points = new ArrayList<Point2D>();

    // NOTE: not optimized

    // standard case - plain XYDataset
    for (int serie = 0, seriesCount = dataset.getSeriesCount(), item, itemCount; serie < seriesCount; serie++) {
      itemCount = dataset.getItemCount(serie);
      for (item = 0; item < itemCount; item++) {
        x = dataset.getXValue(serie, item);
        y = dataset.getYValue(serie, item);

        if (!Double.isNaN(x) && !Double.isNaN(y)) {

          if (shape.contains(x, y)) {
            // TODO: keep data selection (pointer to real data)
            /*
             matchSerie = serie;
             matchItem = item;
             */
            points.add(new Point2D.Double(x, y));
          }
        }
      }
    }

    logger.warn("findDataPoints: time = {} ms.", 1e-6d * (System.nanoTime() - startTime));
    if (false) {
      logger.warn("Matching points: {}", points);
    }

    return points;
  }

  /**
   * Handle the changed event to plot the vis2 plot synchronously.
   * @param event event
   */
  @Override
  public void onProcess(final ObservationEvent event) {
    if (logger.isDebugEnabled()) {
      logger.debug("event [{}] process IN", event.getType());
    }
    switch (event.getType()) {
      case OIFITS_DONE:
        if (event instanceof OIFitsEvent) {
          this.plot(((OIFitsEvent) event).getOIFitsFile());
        }
        break;
      default:
    }
    if (logger.isDebugEnabled()) {
      logger.debug("event [{}] process OUT", event.getType());
    }
  }

  /**
   * Plot the squarred visibilities of the generated file synchronously.
   * This code must be executed by the Swing Event Dispatcher thread (EDT)
   * @param oiFitsFile OIFits file to use
   */
  private void plot(final OIFitsFile oiFitsFile) {
    logger.debug("plot : {}", oiFitsFile);

    // refresh the plot :
    this.oiFitsFile = oiFitsFile;

    logger.debug("plot : refresh");

    final long start = System.nanoTime();

    this.updatePlot();

    if (logger.isInfoEnabled()) {
      logger.info("plot : duration = {} ms.", 1e-6d * (System.nanoTime() - start));
    }
  }

  /**
   * Reset plot
   */
  private void resetPlot() {
    // disable chart & plot notifications:
    this.chart.setNotify(false);
    this.xyPlotVis2.setNotify(false);
    this.xyPlotT3.setNotify(false);
    try {
      // reset title:
      ChartUtils.clearTextSubTitle(this.chart);

      // reset dataset:
      this.xyPlotVis2.setDataset(null);
      this.xyPlotT3.setDataset(null);

      this.resetOverlays();

    } finally {
      // restore chart & plot notifications:
      this.xyPlotT3.setNotify(true);
      this.xyPlotVis2.setNotify(true);
      this.chart.setNotify(true);
    }
  }

  /**
   * Show message or plot
   * @param show flag to indicate to show label
   */
  private void showMessage(final boolean show) {
    this.jLabelMessage.setVisible(show);
    this.chartPanel.setVisible(!show);
  }

  /**
   * Refresh the plot using chart data.
   * This code is executed by the Swing Event Dispatcher thread (EDT)
   */
  private void updatePlot() {

    if (this.oiFitsFile == null) {
      resetPlot();
      this.jLabelMessage.setText("No VIS2 data available: the target is not observable or multiple configurations are selected.");
      showMessage(true);
      return;
    }

    final OIVis2 vis2 = this.oiFitsFile.getOiVis2()[0];
    final String arrayName = vis2.getArrName();
    final String insName = vis2.getInsName();
    final OIArray array = this.oiFitsFile.getOiArray(arrayName);

    final float[] effWaves = vis2.getOiWavelength().getEffWave();
    final double wlMin = 1e6d * effWaves[0];
    final double wlMax = 1e6d * effWaves[effWaves.length - 1];

    final boolean hasData;
    // disable chart & plot notifications:
    this.chart.setNotify(false);
    this.xyPlotVis2.setNotify(false);
    this.xyPlotT3.setNotify(false);

    try {
      // title :
      ChartUtils.clearTextSubTitle(this.chart);

      final StringBuilder sb = new StringBuilder(32);
      sb.append(arrayName).append(" - ");
      sb.append(insName);
      sb.append(" [").append(df4.format(wlMin)).append(" \u00B5m - ").append(df4.format(wlMax)).append(" \u00B5m] - ");

      for (String station : array.getStaName()) {
        sb.append(station).append(' ');
      }

      ChartUtils.addSubtitle(this.chart, sb.toString());

      // date - Source:
      ChartUtils.addSubtitle(this.chart, "Day: " + vis2.getDateObs()
              + " - Source: " + this.oiFitsFile.getOiTarget().getTarget()[0]);

      // change the scaling factor ?
      setUvPlotScalingFactor(MEGA_LAMBDA_SCALE);

      // computed data are valid :
      hasData = updateChart();

      if (hasData) {
        // update theme at end :
        ChartUtilities.applyCurrentTheme(this.chart);

        this.xyPlotVis2.setBackgroundPaint(Color.WHITE);
        this.xyPlotVis2.setDomainGridlinePaint(Color.LIGHT_GRAY);
        this.xyPlotVis2.setRangeGridlinePaint(Color.LIGHT_GRAY);

        this.xyPlotT3.setBackgroundPaint(Color.WHITE);
        this.xyPlotT3.setDomainGridlinePaint(Color.LIGHT_GRAY);
        this.xyPlotT3.setRangeGridlinePaint(Color.LIGHT_GRAY);
      } else {
        this.jLabelMessage.setText("No VIS2 data available: the target has no model.");
      }

      this.resetOverlays();

    } finally {
      // restore chart & plot notifications:
      this.xyPlotT3.setNotify(true);
      this.xyPlotVis2.setNotify(true);
      this.chart.setNotify(true);
    }

    showMessage(!hasData);
  }

  /**
   * reset overlays
   */
  private void resetOverlays() {
    // reset crossHairs:
    if (this.crosshairOverlay != null) {
      for (Integer plotIndex : this.plotMapping.values()) {
        for (Crosshair ch : this.crosshairOverlay.getDomainCrosshairs(plotIndex)) {
          ch.setValue(Double.NaN);
        }
        for (Crosshair ch : this.crosshairOverlay.getRangeCrosshairs(plotIndex)) {
          ch.setValue(Double.NaN);
        }
      }
    }

    // reset selection:
    if (this.selectionOverlay != null) {
      this.selectionOverlay.reset();
    }
  }

  /**
   * Update the datasets
   * @return true if vis2 has data to plot
   */
  private boolean updateChart() {
    Range xRangeVis2 = null;
    Range xRangeT3 = null;

    if (this.oiFitsFile.hasOiVis2()) {
      final OIVis2 vis2 = this.oiFitsFile.getOiVis2()[0];

      // compute derived data i.e. spatial frequencies:
      final double[][] spatialFreq = vis2.getSpatialFreq();

      xRangeVis2 = updatePlot(xyPlotVis2, vis2, vis2.getVis2Data(), vis2.getVis2Err(), spatialFreq);
    }

    if (this.oiFitsFile.hasOiT3()) {
      final OIT3 t3 = this.oiFitsFile.getOiT3()[0];

      // compute derived data i.e. spatial frequencies:
      final double[][] spatialFreq = t3.getSpatial();

      xRangeT3 = updatePlot(xyPlotT3, t3, t3.getT3Phi(), t3.getT3PhiErr(), spatialFreq);
    } else {
      // reset T3 dataset:
      this.xyPlotT3.setDataset(null);
    }

    final boolean showT3Plot = (xRangeT3 != null);
    final boolean hasT3Plot = this.combinedXYPlot.getSubplots().contains(this.xyPlotT3);

    if (showT3Plot != hasT3Plot) {
      if (showT3Plot) {
        this.combinedXYPlot.add(this.xyPlotT3, 1);
      } else {
        this.combinedXYPlot.remove(this.xyPlotT3);
      }
    }

    if (xRangeVis2 == null && xRangeT3 == null) {
      return false;
    }

    final double minX = Math.min(xRangeVis2.getLowerBound(), (xRangeT3 != null) ? xRangeT3.getLowerBound() : Double.POSITIVE_INFINITY);
    final double maxX = Math.max(xRangeVis2.getUpperBound(), (xRangeT3 != null) ? xRangeT3.getUpperBound() : Double.NEGATIVE_INFINITY);

    BoundedNumberAxis axis;

    axis = (BoundedNumberAxis) this.combinedXYPlot.getDomainAxis();
    axis.setBounds(new Range(minX, maxX));
    axis.setRange(minX, maxX);

    return true;
  }

  /**
   * TODO use column names and virtual columns (spatial ...)
   * @param plot
   * @param table
   * @param data
   * @param dataErr
   * @return
   */
  private Range updatePlot(final XYPlot plot, final OIData table,
          final double[][] data, final double[][] dataErr, final double[][] spatialFreq) {

    final int nRows = table.getNbRows();
    final int nWaves = table.getNWave();

    // Prepare palette
    final Color[] colors = new Color[nWaves];

    final IndexColorModel colorModel = RAINBOW_COLOR_MODEL;

    final int iMaxColor = colorModel.getMapSize() - 1;

    final float factor = ((float) iMaxColor) / nWaves;
    float value;

    final int alphaMask = Math.round(255 * 0.8f) << 24;

    for (int i = 0; i < nWaves; i++) {
      // invert palette to have (VIOLET - BLUE - GREEN - RED) ie color spectrum:
      value = iMaxColor - factor * i;

      colors[i] = new Color(ImageUtils.getRGB(colorModel, iMaxColor, value, alphaMask), true);
    }


    final FastXYErrorRenderer renderer = (FastXYErrorRenderer) plot.getRenderer();

    // try to fill with squared visibilities:

    // Use DefaultIntervalXYDataset for performance (arrays XY intervals)
    boolean hasData = false;
    boolean hasErr = false;

    double minX = Double.POSITIVE_INFINITY;
    double maxX = 0d;
    double minY = (USE_LOG_SCALE) ? 1d : 0d;
    double maxY = 1d;

    final DefaultIntervalXYDataset dataset = new DefaultIntervalXYDataset();

    double[] xValue, xLower, xUpper, yValue, yLower, yUpper;

    double x, y, err;

    for (int j = 0; j < nWaves; j++) {

      // 1 color per spectral channel (i.e. per Serie) :
      xValue = new double[nRows];
      xLower = new double[nRows];
      xUpper = new double[nRows];
      yValue = new double[nRows];
      yLower = new double[nRows];
      yUpper = new double[nRows];

      for (int i = 0; i < nRows; i++) {

        x = toUVPlotScale(spatialFreq[i][j]);
        y = data[i][j];
        err = dataErr[i][j];

        // TODO: use also OIVIS2 Flags to skip flagged data:

        if (Double.isNaN(y)) {
          xValue[i] = Double.NaN;
          xLower[i] = Double.NaN;
          xUpper[i] = Double.NaN;
          yValue[i] = Double.NaN;
          yLower[i] = Double.NaN;
          yUpper[i] = Double.NaN;
        } else {
          hasData = true;

          if (USE_LOG_SCALE && y < 0d) {
            // keep only positive data:
            y = -y;
          }

          if (x < minX) {
            minX = x;
          }
          if (x > maxX) {
            maxX = x;
          }
          if (y < minY) {
            minY = y;
          }
          if (y > maxY) {
            maxY = y;
          }

          // TODO: handle x error too:
          xValue[i] = x;
          xLower[i] = Double.NaN;
          xUpper[i] = Double.NaN;

          if (Double.isNaN(err)) {
            yValue[i] = y;
            yLower[i] = Double.NaN;
            yUpper[i] = Double.NaN;
          } else {
            // USE_LOG_SCALE: check if y - err < 0:
            yValue[i] = y;
            yLower[i] = (USE_LOG_SCALE && (y - err) < 0d) ? 0d : (y - err);
            yUpper[i] = y + err;

            hasErr = true;
          }
        }
      }

      dataset.addSeries("OIDATA W" + j, new double[][]{xValue, xLower, xUpper, yValue, yLower, yUpper});

      renderer.setSeriesPaint(j, colors[j], false);
    }

    plot.setDataset(dataset);

    if (!hasData) {
      return null;
    }

    // set shape depending on error (triangle or square):
    final Shape shape = getPointShape(hasErr);

    // disable error rendering (performance):
    renderer.setDrawYError(hasErr);

    // use deprecated method but defines shape once for ALL series (performance):
    renderer.setShape(shape);

    // fix minX to include zero spatial frequency:
    if (minX > 0d) {
      minX = 0d;
    }

    // margin:
    final double marginX = (maxX - minX) * MARGIN_PERCENTS;
    if (minX > 0d) {
      minX -= marginX;
    }
    maxX += marginX;

    if (!USE_LOG_SCALE) {
      final double marginY = (maxY - minY) * MARGIN_PERCENTS;
      minY -= marginY;
      maxY += marginY;
    }

    BoundedNumberAxis axis;

    if (plot.getRangeAxis() instanceof BoundedNumberAxis) {
      axis = (BoundedNumberAxis) plot.getRangeAxis();
      axis.setBounds(new Range(minY, maxY));
      axis.setRange(minY, maxY);
    }

    if (USE_LOG_SCALE) {
      // test logarithmic axis:
      final LogarithmicAxis logAxis = new LogarithmicAxis("log(V²)");
      logAxis.setExpTickLabelsFlag(true);
      //      logAxis.setAllowNegativesFlag(true);
      //      logAxis.setStrictValuesFlag(false);
      logAxis.setAutoRangeNextLogFlag(true);

      logger.debug("logAxis range: [{} - {}]", minY, maxY);

      logAxis.setRange(minY, maxY);

      plot.setRangeAxis(logAxis);
    }

    return new Range(minX, maxX);
  }
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel jLabelMessage;
    private javax.swing.JPanel jPanelCenter;
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

    // DEBUG:
    switch (event.getType()) {
      case ChartProgressEvent.DRAWING_STARTED:
        this.chartDrawStartTime = System.nanoTime();
        break;
      case ChartProgressEvent.DRAWING_FINISHED:
        logger.warn("Drawing chart time = {} ms.", 1e-6d * (System.nanoTime() - this.chartDrawStartTime));
        this.chartDrawStartTime = 0l;
        break;
      default:
    }

    // Perform custom operations before/after chart rendering:
    // move JMMC annotations:
    this.aJMMCVis2.setX(this.xyPlotVis2.getDomainAxis().getUpperBound());
    this.aJMMCVis2.setY(this.xyPlotVis2.getRangeAxis().getLowerBound());

    if (this.xyPlotT3.getDomainAxis() != null) {
      this.aJMMCT3.setX(this.xyPlotT3.getDomainAxis().getUpperBound());
      this.aJMMCT3.setY(this.xyPlotT3.getRangeAxis().getLowerBound());
    }
  }

  /**
   * Define the uv scaling factor
   * @param uvPlotScalingFactor new value
   */
  private void setUvPlotScalingFactor(final double uvPlotScalingFactor) {
    this.uvPlotScalingFactor = uvPlotScalingFactor;
  }

  /**
   * Convert the given value (u or v) to the plot scale
   * @param value u or v coordinate in rad-1
   * @return u or v coordinate in the plot unit
   */
  private double toUVPlotScale(final double value) {
    return this.uvPlotScalingFactor * value;
  }

  /**
   * Convert the given plot value (u or v) to the standard unit (rad-1)
   * @param value u or v coordinate in the plot unit
   * @return u or v coordinate in rad-1
   */
  private double fromUVPlotScale(final double value) {
    return value / this.uvPlotScalingFactor;
  }

  /**
   * Return the shape used to represent points on the plot
   * @param hasError flag indicating to return the shape associated to data with error or without
   * @return shape
   */
  private static Shape getPointShape(final boolean hasError) {

    if (hasError) {
      return new Rectangle2D.Double(-3d, -3d, 6d, 6d);
    }

    // equilateral triangle centered on its barycenter:
    final GeneralPath path = new GeneralPath();

    path.moveTo(0f, -4f);
    path.lineTo(3f, 2f);
    path.lineTo(-3f, 2f);
    path.lineTo(0f, -4f);

    return path;
  }
}
