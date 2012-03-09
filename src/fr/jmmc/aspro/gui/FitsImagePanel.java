/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.gui.action.ExportPDFAction;

import fr.jmmc.aspro.gui.chart.ChartUtils;
import fr.jmmc.aspro.gui.chart.ColorModelPaintScale;
import fr.jmmc.aspro.gui.chart.PDFOptions;
import fr.jmmc.aspro.gui.chart.PaintLogScaleLegend;
import fr.jmmc.aspro.gui.chart.SquareChartPanel;
import fr.jmmc.aspro.gui.chart.SquareXYPlot;
import fr.jmmc.aspro.gui.chart.ZoomEvent;
import fr.jmmc.aspro.gui.chart.ZoomEventListener;
import fr.jmmc.aspro.image.FitsImageUtils;
import fr.jmmc.jmcs.gui.MessagePane;
import fr.jmmc.jmcs.gui.StatusBar;
import fr.jmmc.jmcs.gui.task.TaskSwingWorker;
import fr.jmmc.jmal.image.ColorModels;
import fr.jmmc.jmal.image.ImageUtils;
import fr.jmmc.jmal.image.ColorScale;
import fr.jmmc.jmcs.gui.task.Task;
import fr.jmmc.oitools.image.FitsImage;
import java.awt.Color;
import java.awt.Image;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.IndexColorModel;
import java.util.Observable;
import java.util.Observer;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.axis.AxisLocation;
import org.jfree.chart.axis.LogarithmicAxis;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.block.BlockBorder;
import org.jfree.chart.event.ChartProgressEvent;
import org.jfree.chart.event.ChartProgressListener;
import org.jfree.chart.title.PaintScaleLegend;
import org.jfree.data.Range;
import org.jfree.ui.Layer;
import org.jfree.ui.RectangleEdge;
import org.jfree.ui.RectangleInsets;
import org.jfree.ui.TextAnchor;

/**
 * This panel represents a FitsImage plot
 * @author bourgesl
 */
public final class FitsImagePanel extends javax.swing.JPanel implements ChartProgressListener, ZoomEventListener,
        Observer, PDFExportable, Disposable {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1L;
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(FitsImagePanel.class.getName());
  /** image task prefix 'convertFitsImage-' */
  private static final String PREFIX_IMAGE_TASK = "convertFitsImage-";
  /** global thread counter */
  private final static AtomicInteger panelCounter = new AtomicInteger(1);
  /* members */
  /** image convert task */
  final Task task;
  /** fits image to plot */
  private FitsImage fitsImage = null;
  /** preference singleton */
  private final Preferences myPreferences = Preferences.getInstance();
  /** jFreeChart instance */
  private JFreeChart chart;
  /** xy plot instance */
  private SquareXYPlot xyPlot;
  /** JMMC annotation */
  private XYTextAnnotation aJMMC = null;
  /** image scale legend */
  private PaintScaleLegend mapLegend = null;

  /* plot data */
  /** last zoom event to check if the zoom area changed */
  private ZoomEvent lastZoomEvent = null;
  /** chart data */
  private ImageChartData chartData = null;
  /* swing */
  /** chart panel */
  private SquareChartPanel chartPanel;

  /**
   * Constructor
   */
  public FitsImagePanel() {
    initComponents();

    postInit();

    this.task = new Task(PREFIX_IMAGE_TASK + panelCounter.getAndIncrement());
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
   * Export the current chart as a PDF document
   * @param evt action event
   */
  /**
   * Export the chart component as a PDF document
   */
  @Override
  public void performPDFAction() {
    ExportPDFAction.exportPDF(this);
  }

  /**
   * Return the PDF default file name
   * @return PDF default file name
   */
  @Override
  public String getPDFDefaultFileName() {
    return null;
  }

  /**
   * Return the PDF options
   * @return PDF options
   */
  @Override
  public PDFOptions getPDFOptions() {
    return PDFOptions.DEFAULT_PDF_OPTIONS;
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

    this.chart = ChartUtils.createSquareXYLineChart(null, null, false);
    this.chart.setPadding(new RectangleInsets(0d, 0d, 0d, 10d));

    this.xyPlot = (SquareXYPlot) this.chart.getPlot();

    // Adjust background settings :
    this.xyPlot.setBackgroundImageAlpha(1.0f);

    // create new JMMC annotation (moving position):
    this.aJMMC = ChartUtils.createXYTextAnnotation(AsproConstants.JMMC_ANNOTATION, 0, 0);
    this.aJMMC.setTextAnchor(TextAnchor.BOTTOM_RIGHT);
    this.aJMMC.setPaint(Color.DARK_GRAY);
    this.xyPlot.getRenderer().addAnnotation(this.aJMMC, Layer.BACKGROUND);

    // add listener :
    this.chart.addProgressListener(this);
    this.chartPanel = ChartUtils.createSquareChartPanel(this.chart);

    // zoom options :
    this.chartPanel.setDomainZoomable(AsproConstants.ENABLE_ZOOM);
    this.chartPanel.setRangeZoomable(AsproConstants.ENABLE_ZOOM);

    // define zoom listener :
    this.chartPanel.setZoomEventListener(this);

    this.add(this.chartPanel);

    // register this instance as a Preference Observer :
    this.myPreferences.addObserver(this);
  }

  /**
   * Free any ressource or reference to this instance :
   * remove this instance form Preference Observers
   */
  @Override
  public void dispose() {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("dispose : " + this);
    }

    // unregister this instance as a Preference Observer :
    this.myPreferences.deleteObserver(this);
  }

  /**
   * Overriden method to give object identifier
   * @return string identifier
   */
  @Override
  public String toString() {
    return "FitsImagePanel@" + Integer.toHexString(hashCode());
  }

  /**
   * Listen to preferences changes
   * @param o Preferences
   * @param arg unused
   */
  @Override
  public void update(final Observable o, final Object arg) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("Preferences updated on : " + this);
    }

    final IndexColorModel colorModel = ColorModels.getColorModel(this.myPreferences.getPreference(Preferences.MODEL_IMAGE_LUT));
    final ColorScale colorScale = this.myPreferences.getImageColorScale();

    if (getChartData() != null
            && (getChartData().getColorModel() != colorModel || getChartData().getColorScale() != colorScale)) {
      this.plot();
    }
  }

  /**
   * Update the fits image to plot
   * @param image image to plot
   */
  public void setFitsImage(final FitsImage image) {
    this.fitsImage = image;
    this.plot();
  }

  /**
   * Plot the image using a SwingWorker to do the computation in the background.
   * This code is executed by the Swing Event Dispatcher thread (EDT)
   */
  private void plot() {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("plot : " + this.fitsImage);
    }

    // check if fits image is available :
    if (this.fitsImage != null) {

      // Use model image Preferences :
      final IndexColorModel colorModel = ColorModels.getColorModel(this.myPreferences.getPreference(Preferences.MODEL_IMAGE_LUT));
      final ColorScale colorScale = this.myPreferences.getImageColorScale();

      // update the status bar :
      StatusBar.show("computing image ...");

      // Create image convert task worker :
      // Cancel other tasks and execute this new task :
      new ConvertFitsImageSwingWorker(this, this.fitsImage, colorModel, colorScale).executeTask();
    }
  }

  /**
   * TaskSwingWorker child class to compute an image from the given fits image
   */
  private final static class ConvertFitsImageSwingWorker extends TaskSwingWorker<ImageChartData> {

    /* members */
    /** fits panel used for refreshUI callback */
    private final FitsImagePanel fitsPanel;
    /** fits image */
    private final FitsImage fitsImage;
    /** image color model */
    private final IndexColorModel colorModel;
    /** color scaling method */
    private final ColorScale colorScale;

    /**
     * Hidden constructor
     *
     * @param fitsPanel fits panel
     * @param fitsImage fits image
     * @param colorModel color model to use
     * @param colorScale color scaling method
     */
    private ConvertFitsImageSwingWorker(final FitsImagePanel fitsPanel, final FitsImage fitsImage,
            final IndexColorModel colorModel, final ColorScale colorScale) {
      // get current observation version :
      super(fitsPanel.task);
      this.fitsPanel = fitsPanel;
      this.fitsImage = fitsImage;
      this.colorModel = colorModel;
      this.colorScale = colorScale;
    }

    /**
     * Compute the image in background
     * This code is executed by a Worker thread (Not Swing EDT)
     * @return computed image data
     */
    @Override
    public ImageChartData computeInBackground() {

      // Start the computations :
      final long start = System.nanoTime();

      float dataMin = (float) this.fitsImage.getDataMin();
      float dataMax = (float) this.fitsImage.getDataMax();

      final ColorScale usedColorScale;
      if (colorScale == ColorScale.LOGARITHMIC
              && (dataMin <= 0f || dataMax <= 0f || dataMin == dataMax || Float.isInfinite(dataMin) || Float.isInfinite(dataMax))) {
        usedColorScale = ColorScale.LINEAR;
        
        // update min/max:
        FitsImageUtils.updateDataRange(fitsImage);
        dataMin = (float) this.fitsImage.getDataMin();
        dataMax = (float) this.fitsImage.getDataMax();
        
        if (dataMin == dataMax) {
          dataMax = dataMin + 1f;
        }
      } else {
        usedColorScale = colorScale;
      }

      // throws InterruptedJobException if the current thread is interrupted (cancelled):
      final BufferedImage image = ImageUtils.createImage(this.fitsImage.getNbCols(), this.fitsImage.getNbRows(),
              this.fitsImage.getData(), dataMin, dataMax,
              this.colorModel, usedColorScale);

      // fast interrupt :
      if (Thread.currentThread().isInterrupted()) {
        return null;
      }

      if (logger.isInfoEnabled()) {
        logger.info("compute : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");
      }

      return new ImageChartData(fitsImage, colorModel, usedColorScale, image);
    }

    /**
     * Refresh the plot using the computed image.
     * This code is executed by the Swing Event Dispatcher thread (EDT)
     * @param imageData computed image data
     */
    @Override
    public void refreshUI(final ImageChartData imageData) {
      // Refresh the GUI using coherent data :
      this.fitsPanel.updatePlot(imageData);
    }

    /**
     * Handle the execution exception that occured in the compute operation @see #computeInBackground()
     * This implementation resets the plot and opens a message dialog or the feedback report depending on the cause.
     *
     * @param ee execution exception
     */
    @Override
    public void handleException(final ExecutionException ee) {
      this.fitsPanel.resetPlot();
      if (ee.getCause() instanceof IllegalArgumentException) {
        MessagePane.showErrorMessage(ee.getCause().getMessage());
      } else {
        super.handleException(ee);
      }
    }
  }

  /**
   * Return the chart data
   * @return chart data
   */
  private ImageChartData getChartData() {
    return this.chartData;
  }

  /**
   * Define the chart data
   * @param chartData chart data
   */
  private void setChartData(final ImageChartData chartData) {
    this.chartData = chartData;
  }

  /**
   * Reset the plot in case of model exception
   */
  private void resetPlot() {

    ChartUtils.clearTextSubTitle(this.chart);

    this.lastZoomEvent = null;
    this.chartData = null;

    // reset bounds to [-1;1] (before setDataset) :
    this.xyPlot.defineBounds(1d);
    // reset dataset for baseline limits :
    this.xyPlot.setDataset(null);

    // update the background image :
    this.updatePlotImage(null);

    // update theme at end :
    ChartUtilities.applyCurrentTheme(this.chart);
  }

  /**
   * Refresh the plot using the given image.
   * This code is executed by the Swing Event Dispatcher thread (EDT)
   *
   * @param imageData computed image data
   */
  private void updatePlot(final ImageChartData imageData) {
    // memorize image (used by zoom handling) :
    setChartData(imageData);

    // reset zoom cache :
    this.lastZoomEvent = null;

    // title :
    ChartUtils.clearTextSubTitle(this.chart);

    if (imageData.getFitsImage().getFitsImageIdentifier() != null) {
      ChartUtils.addSubtitle(this.chart, "Id: " + imageData.getFitsImage().getFitsImageIdentifier());
    }

    // define axis boundaries:
    final Rectangle2D.Double imgRectRef = imageData.getFitsImage().getArea();

    this.xyPlot.defineBounds(new Range(imgRectRef.getX(), imgRectRef.getX() + imgRectRef.getWidth()),
            new Range(imgRectRef.getY(), imgRectRef.getY() + imgRectRef.getHeight()));

    this.xyPlot.restoreAxesBounds();

    // define axis orientation:
    if (!imageData.getFitsImage().isIncColPositive()) {
      final ValueAxis domainAxis = this.xyPlot.getDomainAxis(0);
      domainAxis.setInverted(true);
    }
    if (!imageData.getFitsImage().isIncRowPositive()) {
      final ValueAxis rangeAxis = this.xyPlot.getRangeAxis(0);
      rangeAxis.setInverted(true);
    }

    // update the background image and legend:
    updateImage(imageData);

    // update theme at end :
    ChartUtilities.applyCurrentTheme(this.chart);

    // update the status bar :
    StatusBar.show("image done.");
  }

  /**
   * Process the zoom event to refresh the image according to the new coordinates
   * @param ze zoom event
   */
  @Override
  public void chartChanged(final ZoomEvent ze) {
    // check if the zoom changed :
    if (!ze.equals(this.lastZoomEvent)) {
      this.lastZoomEvent = ze;

      if (this.getChartData() != null) {
        // Update image :

        final Rectangle2D.Double imgRect = new Rectangle2D.Double();
        imgRect.setFrameFromDiagonal(ze.getDomainLowerBound(), ze.getRangeLowerBound(),
                ze.getDomainUpperBound(), ze.getRangeUpperBound());

        // compute an approximated image from the reference image :
        computeSubImage(this.getChartData(), imgRect);
      }
    }
  }

  /**
   * Compute a sub image for the image given the new area
   * @param imageData computed image data
   * @param imgRect new image area
   * @return true if the given image rectangle is smaller than rectangle of the reference image 
   */
  private boolean computeSubImage(final ImageChartData imageData, final Rectangle2D.Double imgRect) {
    boolean doCrop = false;

    final BufferedImage image = imageData.getImage();

    final int imageWidth = image.getWidth();
    final int imageHeight = image.getHeight();

    // area reference :
    final Rectangle2D.Double imgRectRef = imageData.getFitsImage().getArea();

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("image rect     = " + imgRect);
      logger.fine("image rect REF = " + imgRectRef);
    }

    // note : floor/ceil to be sure to have at least 1x1 pixel image
    int x = (int) Math.floor(imageWidth * (imgRect.getX() - imgRectRef.getX()) / imgRectRef.getWidth());
    int y = (int) Math.floor(imageHeight * (imgRect.getY() - imgRectRef.getY()) / imgRectRef.getHeight());
    int w = (int) Math.ceil(imageWidth * imgRect.getWidth() / imgRectRef.getWidth());
    int h = (int) Math.ceil(imageHeight * imgRect.getHeight() / imgRectRef.getHeight());

    // Note : the image is produced from an array where 0,0 corresponds to the upper left corner
    // whereas it corresponds in Fits image to the lower left corner => inverse the Y axis

    if (!imageData.getFitsImage().isIncColPositive()) {
      // Inverse X axis issue :
      x = imageWidth - x - w;
    }

    if (imageData.getFitsImage().isIncRowPositive()) {
      // Inverse Y axis issue :
      y = imageHeight - y - h;
    }

    // check bounds:
    x = checkBounds(x, 0, imageWidth - 1);
    y = checkBounds(y, 0, imageHeight - 1);
    w = checkBounds(w, 1, imageWidth - x);
    h = checkBounds(h, 1, imageHeight - y);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("sub image [" + x + ", " + y + " - " + w + ", " + h + "] - doCrop = " + doCrop);
    }

    doCrop = ((x != 0) || (y != 0) || (w != imageWidth) || (h != imageHeight));

    // crop a small sub image:

    // check reset zoom to avoid computing sub image == ref image:
    final Image subImage = (doCrop) ? image.getSubimage(x, y, w, h) : image;

    // TODO: adjust axis bounds to exact viewed rectangle (i.e. avoid rounding errors) !!

    // update the background image :
    updatePlotImage(subImage);

    return doCrop;
  }

  /**
   * Return the value or the closest bound
   * @param value value to check
   * @param min minimum value
   * @param max maximum value
   * @return value or the closest bound
   */
  private static int checkBounds(final int value, final int min, final int max) {
    if (value < min) {
      return min;
    }
    if (value > max) {
      return max;
    }
    return value;
  }

  /**
   * Update the background image of the chart with the given image and its legend
   * @param imageData computed image data or null
   */
  private void updateImage(final ImageChartData imageData) {

    if (mapLegend != null) {
      this.chart.removeSubtitle(mapLegend);
    }

    if (imageData != null) {
      final FitsImage lFitsImage = imageData.getFitsImage();

      final double min = lFitsImage.getDataMin();
      final double max = lFitsImage.getDataMax();
      final IndexColorModel colorModel = imageData.getColorModel();
      final ColorScale colorScale = imageData.getColorScale();

      final NumberAxis uvMapAxis;
      if (colorScale == ColorScale.LINEAR) {
        uvMapAxis = new NumberAxis();
        mapLegend = new PaintScaleLegend(new ColorModelPaintScale(min, max, colorModel, colorScale), uvMapAxis);
      } else {
        uvMapAxis = new LogarithmicAxis(null);
        ((LogarithmicAxis) uvMapAxis).setExpTickLabelsFlag(true);
        mapLegend = new PaintLogScaleLegend(new ColorModelPaintScale(min, max, colorModel, colorScale), uvMapAxis);
      }

      uvMapAxis.setTickLabelFont(ChartUtils.DEFAULT_FONT);
      uvMapAxis.setAxisLinePaint(Color.BLACK);
      uvMapAxis.setTickMarkPaint(Color.BLACK);

      mapLegend.setPosition(RectangleEdge.LEFT);
      mapLegend.setStripWidth(15d);
      mapLegend.setStripOutlinePaint(Color.BLACK);
      mapLegend.setStripOutlineVisible(true);
      mapLegend.setSubdivisionCount(colorModel.getMapSize());
      mapLegend.setAxisLocation(AxisLocation.BOTTOM_OR_LEFT);
      mapLegend.setFrame(new BlockBorder(Color.BLACK));
      mapLegend.setMargin(1d, 1d, 1d, 1d);
      mapLegend.setPadding(10d, 10d, 10d, 10d);

      this.chart.addSubtitle(mapLegend);

      updatePlotImage(imageData.getImage());

    } else {
      updatePlotImage(null);
    }
  }

  /**
   * Update the background image of the chart with the given image
   * @param image image or null
   */
  private void updatePlotImage(final Image image) {
    if (image != null) {
      this.xyPlot.setBackgroundPaint(null);
      this.xyPlot.setBackgroundImage(image);
    } else {
      this.xyPlot.setBackgroundPaint(Color.lightGray);
      this.xyPlot.setBackgroundImage(null);
    }
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
    if (logger.isLoggable(Level.FINE)) {
      switch (event.getType()) {
        case ChartProgressEvent.DRAWING_STARTED:
          this.chartDrawStartTime = System.nanoTime();
          break;
        case ChartProgressEvent.DRAWING_FINISHED:
          logger.fine("Drawing chart time : " + 1e-6d * (System.nanoTime() - this.chartDrawStartTime) + " ms.");
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

  /**
   * This class contains image data (fits image, image, colorModel ...) for consistency
   */
  private static class ImageChartData {

    /** fits image */
    private final FitsImage fitsImage;
    /** image color model */
    private final IndexColorModel colorModel;
    /** java2D image */
    private final BufferedImage image;
    /** color scaling method */
    private final ColorScale colorScale;

    /**
     * Protected constructor
     * @param fitsImage fits image
     * @param colorModel image color model
     * @param colorScale color scaling method
     * @param image java2D image 
     */
    ImageChartData(final FitsImage fitsImage, final IndexColorModel colorModel, final ColorScale colorScale,
            final BufferedImage image) {
      this.fitsImage = fitsImage;
      this.colorModel = colorModel;
      this.colorScale = colorScale;
      this.image = image;
    }

    /**
     * Return the fits image
     * @return fits image
     */
    FitsImage getFitsImage() {
      return fitsImage;
    }

    /**
     * Return the image color model
     * @return image color model
     */
    IndexColorModel getColorModel() {
      return colorModel;
    }

    /**
     * Return the color scaling method
     * @return color scaling method
     */
    ColorScale getColorScale() {
      return colorScale;
    }

    /**
     * Return the java2D image
     * @return java2D image
     */
    BufferedImage getImage() {
      return image;
    }
  }
}