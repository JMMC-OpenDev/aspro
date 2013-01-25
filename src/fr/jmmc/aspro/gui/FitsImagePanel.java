/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.gui.action.AsproExportPDFAction;
import fr.jmmc.aspro.gui.chart.ColorModelPaintScale;
import fr.jmmc.aspro.gui.chart.PaintLogScaleLegend;
import fr.jmmc.aspro.image.FitsImageUtils;
import fr.jmmc.jmal.image.ColorModels;
import fr.jmmc.jmal.image.ColorScale;
import fr.jmmc.jmal.image.ImageUtils;
import fr.jmmc.jmcs.gui.component.Disposable;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.task.Task;
import fr.jmmc.jmcs.gui.task.TaskSwingWorker;
import fr.jmmc.jmcs.gui.task.TaskSwingWorkerExecutor;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.ObjectUtils;
import fr.jmmc.oiexplorer.core.gui.PDFExportable;
import fr.jmmc.oiexplorer.core.gui.chart.ChartUtils;
import fr.jmmc.oiexplorer.core.gui.chart.PDFOptions;
import fr.jmmc.oiexplorer.core.gui.chart.SquareChartPanel;
import fr.jmmc.oiexplorer.core.gui.chart.SquareXYPlot;
import fr.jmmc.oiexplorer.core.gui.chart.ZoomEvent;
import fr.jmmc.oiexplorer.core.gui.chart.ZoomEventListener;
import fr.jmmc.oiexplorer.core.util.Constants;
import fr.jmmc.oitools.image.FitsImage;
import java.awt.Color;
import java.awt.Image;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.IndexColorModel;
import java.text.DecimalFormat;
import java.util.Observable;
import java.util.Observer;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicInteger;
import javax.swing.DefaultComboBoxModel;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.axis.AxisLocation;
import org.jfree.chart.axis.LogarithmicAxis;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.block.BlockBorder;
import org.jfree.chart.block.BlockContainer;
import org.jfree.chart.block.ColumnArrangement;
import org.jfree.chart.event.ChartProgressEvent;
import org.jfree.chart.event.ChartProgressListener;
import org.jfree.chart.title.CompositeTitle;
import org.jfree.chart.title.PaintScaleLegend;
import org.jfree.chart.title.TextTitle;
import org.jfree.chart.title.Title;
import org.jfree.data.Range;
import org.jfree.ui.Layer;
import org.jfree.ui.RectangleEdge;
import org.jfree.ui.RectangleInsets;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This panel represents a FitsImage plot
 * @author bourgesl
 */
public final class FitsImagePanel extends javax.swing.JPanel implements ChartProgressListener, ZoomEventListener,
        Observer, PDFExportable, Disposable {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1L;
  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(FitsImagePanel.class.getName());
  /** image task prefix 'convertFitsImage-' */
  private static final String PREFIX_IMAGE_TASK = "convertFitsImage-";
  /** global thread counter */
  private final static AtomicInteger panelCounter = new AtomicInteger(1);
  /* members */
  /** show the image identifier */
  private final boolean showId;
  /** show the image options (LUT, color scale) */
  private final boolean showOptions;
  /** optional minimum range for data */
  private final float[] minDataRange;
  /** image convert task */
  final Task task;
  /** fits image to plot */
  private FitsImage fitsImage = null;
  /** preference singleton */
  private final Preferences myPreferences = Preferences.getInstance();
  /** flag to enable / disable the automatic refresh of the plot when any swing component changes */
  private boolean doAutoRefresh = true;
  /** jFreeChart instance */
  private JFreeChart chart;
  /** xy plot instance */
  private SquareXYPlot xyPlot;
  /** JMMC annotation */
  private XYTextAnnotation aJMMC = null;
  /** image scale legend */
  private PaintScaleLegend mapLegend = null;
  /** formatter for legend title / scale */
  private final DecimalFormat df = new DecimalFormat("0.0#E0");
  /** angle formatter for legend title */
  private final DecimalFormat df3 = new DecimalFormat("0.0##");
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
    this(true, false, null);
  }

  /**
   * Constructor
   * @param showId true to show the image identifier
   * @param showOptions true to show the image options (LUT, color scale)
   */
  public FitsImagePanel(final boolean showId, final boolean showOptions) {
    this(showId, showOptions, null);
  }

  /**
   * Constructor
   * @param showId true to show the image identifier
   * @param showOptions true to show the image options (LUT, color scale)
   * @param minDataRange optional minimal range for data
   */
  public FitsImagePanel(final boolean showId, final boolean showOptions, final float[] minDataRange) {
    this.showId = showId;
    this.showOptions = showOptions;
    this.minDataRange = minDataRange;
    this.task = new Task(PREFIX_IMAGE_TASK + panelCounter.getAndIncrement());

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

    jPanelOptions = new javax.swing.JPanel();
    jLabelLutTable = new javax.swing.JLabel();
    jComboBoxLUT = new javax.swing.JComboBox();
    jLabelColorScale = new javax.swing.JLabel();
    jComboBoxColorScale = new javax.swing.JComboBox();

    setLayout(new java.awt.BorderLayout());

    jPanelOptions.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 15, 2));

    jLabelLutTable.setText("LUT table");
    jPanelOptions.add(jLabelLutTable);

    jComboBoxLUT.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jComboBoxLUTActionPerformed(evt);
      }
    });
    jPanelOptions.add(jComboBoxLUT);

    jLabelColorScale.setText("Color scale");
    jPanelOptions.add(jLabelColorScale);

    jComboBoxColorScale.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        jComboBoxColorScaleActionPerformed(evt);
      }
    });
    jPanelOptions.add(jComboBoxColorScale);

    add(jPanelOptions, java.awt.BorderLayout.PAGE_END);
  }// </editor-fold>//GEN-END:initComponents

  private void jComboBoxColorScaleActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBoxColorScaleActionPerformed
    refreshPlot();
  }//GEN-LAST:event_jComboBoxColorScaleActionPerformed

  private void jComboBoxLUTActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jComboBoxLUTActionPerformed
    refreshPlot();
  }//GEN-LAST:event_jComboBoxLUTActionPerformed

  /**
   * Export the current chart as a PDF document
   * @param evt action event
   */
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
    this.aJMMC = ChartUtils.createJMMCAnnotation(AsproConstants.JMMC_ANNOTATION);
    this.xyPlot.getRenderer().addAnnotation(this.aJMMC, Layer.BACKGROUND);

    // add listener :
    this.chart.addProgressListener(this);
    this.chartPanel = ChartUtils.createSquareChartPanel(this.chart);

    // zoom options :
    this.chartPanel.setDomainZoomable(Constants.ENABLE_ZOOM);
    this.chartPanel.setRangeZoomable(Constants.ENABLE_ZOOM);

    // define zoom listener :
    this.chartPanel.setZoomEventListener(this);

    this.add(this.chartPanel);

    // register this instance as a Preference Observer :
    this.myPreferences.addObserver(this);

    // disable the automatic refresh :
    final boolean prevAutoRefresh = setAutoRefresh(false);
    try {
      // define custom models :
      this.jComboBoxLUT.setModel(new DefaultComboBoxModel(ColorModels.getColorModelNames()));
      this.jComboBoxColorScale.setModel(new DefaultComboBoxModel(ColorScale.values()));

      // update selected items:
      this.jComboBoxLUT.setSelectedItem(this.myPreferences.getPreference(Preferences.MODEL_IMAGE_LUT));
      this.jComboBoxColorScale.setSelectedItem(this.myPreferences.getImageColorScale());
    } finally {
      // restore the automatic refresh :
      setAutoRefresh(prevAutoRefresh);
    }
    // show / hide the option panel:
    this.jPanelOptions.setVisible(this.showOptions);
  }

  /**
   * Free any ressource or reference to this instance :
   * remove this instance form Preference Observers
   */
  @Override
  public void dispose() {
    if (logger.isDebugEnabled()) {
      logger.debug("dispose: {}", ObjectUtils.getObjectInfo(this));
    }

    // Cancel any running task:
    TaskSwingWorkerExecutor.cancelTask(this.task);

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
    logger.debug("Preferences updated on : {}", this);

    final String colorModelPref = this.myPreferences.getPreference(Preferences.MODEL_IMAGE_LUT);
    final ColorScale colorScale = this.myPreferences.getImageColorScale();

    // disable the automatic refresh :
    final boolean prevAutoRefresh = setAutoRefresh(false);
    try {
      // update selected items:
      this.jComboBoxLUT.setSelectedItem(colorModelPref);
      this.jComboBoxColorScale.setSelectedItem(colorScale);
    } finally {
      // restore the automatic refresh :
      setAutoRefresh(prevAutoRefresh);
    }

    final IndexColorModel colorModel = ColorModels.getColorModel(colorModelPref);

    if (getChartData() != null && (getChartData().getColorModel() != colorModel || getChartData().getColorScale() != colorScale)) {
      refreshPlot();
    }
  }

  /**
   * Update the fits image to plot
   * @param image image to plot
   */
  public void setFitsImage(final FitsImage image) {
    this.fitsImage = image;
    refreshPlot();
  }

  /**
   * Refresh the plot when an UI widget changes.
   * Check the doAutoRefresh flag to avoid unwanted refresh
   */
  private void refreshPlot() {
    if (this.doAutoRefresh) {
      logger.debug("refreshPlot");
      this.plot();
    }
  }

  /**
   * Plot the image using a SwingWorker to do the computation in the background.
   * This code is executed by the Swing Event Dispatcher thread (EDT)
   */
  private void plot() {
    logger.debug("plot : {}", this.fitsImage);

    // check if fits image is available :
    if (this.fitsImage == null) {
      resetPlot();
    } else {

      // Use model image Preferences :
      final IndexColorModel colorModel = ColorModels.getColorModel((String) this.jComboBoxLUT.getSelectedItem());
      final ColorScale colorScale = (ColorScale) this.jComboBoxColorScale.getSelectedItem();

      // Create image convert task worker :
      // Cancel other tasks and execute this new task :
      new ConvertFitsImageSwingWorker(this, this.fitsImage, this.minDataRange, colorModel, colorScale).executeTask();
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
    /** optional minimum range for data */
    private final float[] minDataRange;
    /** image color model */
    private final IndexColorModel colorModel;
    /** color scaling method */
    private final ColorScale colorScale;

    /**
     * Hidden constructor
     *
     * @param fitsPanel fits panel
     * @param fitsImage fits image
     * @param minDataRange optional minimal range for data
     * @param colorModel color model to use
     * @param colorScale color scaling method
     */
    private ConvertFitsImageSwingWorker(final FitsImagePanel fitsPanel, final FitsImage fitsImage, final float[] minDataRange,
            final IndexColorModel colorModel, final ColorScale colorScale) {
      // get current observation version :
      super(fitsPanel.task);
      this.fitsPanel = fitsPanel;
      this.fitsImage = fitsImage;
      this.minDataRange = minDataRange;
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

      float min = (float) this.fitsImage.getDataMin();
      float max = (float) this.fitsImage.getDataMax();

      if (this.minDataRange != null) {
        // check minimum data range:
        if (min > this.minDataRange[0]) {
          min = this.minDataRange[0];
        }
        if (max < this.minDataRange[1]) {
          max = this.minDataRange[1];
        }
      }

      final ColorScale usedColorScale;
      if (colorScale == ColorScale.LOGARITHMIC
              && (min <= 0f || max <= 0f || min == max || Float.isInfinite(min) || Float.isInfinite(max))) {
        usedColorScale = ColorScale.LINEAR;

        // update min/max:
        FitsImageUtils.updateDataRange(fitsImage);
        min = (float) this.fitsImage.getDataMin();
        max = (float) this.fitsImage.getDataMax();

        if (min == max) {
          max = min + 1f;
        }
      } else if (colorScale == ColorScale.LINEAR
              && (min <= 0f || max <= 0f || min == max || Float.isInfinite(min) || Float.isInfinite(max))) {
        usedColorScale = ColorScale.LINEAR;

        // update min/max:
        FitsImageUtils.updateDataRange(fitsImage);
        min = (float) this.fitsImage.getDataMin();
        max = (float) this.fitsImage.getDataMax();

        if (min == max) {
          max = min + 1f;
        }
      } else {
        usedColorScale = colorScale;
      }

      // throws InterruptedJobException if the current thread is interrupted (cancelled):
      final BufferedImage image = ImageUtils.createImage(this.fitsImage.getNbCols(), this.fitsImage.getNbRows(),
              this.fitsImage.getData(), min, max,
              this.colorModel, usedColorScale);

      // fast interrupt :
      if (Thread.currentThread().isInterrupted()) {
        return null;
      }

      _logger.info("compute[ImageChartData]: duration = {} ms.", 1e-6d * (System.nanoTime() - start));

      return new ImageChartData(fitsImage, colorModel, usedColorScale, min, max, image);
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

    final FitsImage lFitsImage = imageData.getFitsImage();

    if (this.showId && lFitsImage.getFitsImageIdentifier() != null) {
      ChartUtils.addSubtitle(this.chart, "Id: " + lFitsImage.getFitsImageIdentifier());
    }

    final Title infoTitle;

    if (!lFitsImage.isIncRowDefined() || !lFitsImage.isIncColDefined()) {
      infoTitle = null;
    } else {

      final BlockContainer infoBlock = new BlockContainer(new ColumnArrangement());

      infoBlock.add(new TextTitle("Increment (rad):", ChartUtils.DEFAULT_FONT));
      infoBlock.add(new TextTitle("RA : " + df.format(lFitsImage.getIncCol()), ChartUtils.DEFAULT_FONT));
      infoBlock.add(new TextTitle("DEC: " + df.format(lFitsImage.getIncRow()), ChartUtils.DEFAULT_FONT));
      
      infoBlock.add(new TextTitle("\nFOV:", ChartUtils.DEFAULT_FONT));
      infoBlock.add(new TextTitle(FitsImage.getAngleAsString(lFitsImage.getMaxAngle(), df3), ChartUtils.DEFAULT_FONT));

      if (lFitsImage.getWaveLength() != null) {
        infoBlock.add(new TextTitle("\nWavelength:", ChartUtils.DEFAULT_FONT));
        infoBlock.add(new TextTitle(NumberUtils.trimTo3Digits(1e6d * lFitsImage.getWaveLength()) + " µm", ChartUtils.DEFAULT_FONT));
      }

      infoTitle = new CompositeTitle(infoBlock);
      infoTitle.setFrame(new BlockBorder(Color.BLACK));
      infoTitle.setMargin(1d, 1d, 1d, 1d);
      infoTitle.setPadding(5d, 5d, 5d, 5d);
      infoTitle.setPosition(RectangleEdge.RIGHT);
    }

    // define axis boundaries:
    final Rectangle2D.Double imgRectRef = lFitsImage.getArea();

    this.xyPlot.defineBounds(new Range(imgRectRef.getX(), imgRectRef.getX() + imgRectRef.getWidth()),
            new Range(imgRectRef.getY(), imgRectRef.getY() + imgRectRef.getHeight()));

    this.xyPlot.restoreAxesBounds();

    // define axis orientation:
    if (!lFitsImage.isIncColPositive()) {
      final ValueAxis domainAxis = this.xyPlot.getDomainAxis(0);
      domainAxis.setInverted(true);
    }
    if (!lFitsImage.isIncRowPositive()) {
      final ValueAxis rangeAxis = this.xyPlot.getRangeAxis(0);
      rangeAxis.setInverted(true);
    }

    // update the background image and legend:
    updateImage(imageData);

    // update theme at end :
    ChartUtilities.applyCurrentTheme(this.chart);

    if (infoTitle != null) {
      // after theme:
      chart.addSubtitle(infoTitle);
    }

    // disable the automatic refresh :
    final boolean prevAutoRefresh = setAutoRefresh(false);
    try {
      // update color scale if changed during image computation (logarithmic to linear):
      this.jComboBoxColorScale.setSelectedItem(imageData.getColorScale());
    } finally {
      // restore the automatic refresh :
      setAutoRefresh(prevAutoRefresh);
    }
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

    if (logger.isDebugEnabled()) {
      logger.debug("image rect     = {}", imgRect);
      logger.debug("image rect REF = {}", imgRectRef);
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

    if (logger.isDebugEnabled()) {
      logger.debug("sub image [{}, {} - {}, {}] - doCrop = {}", new Object[]{x, y, w, h, doCrop});
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
      final double min = imageData.getMin();
      final double max = imageData.getMax();
      final IndexColorModel colorModel = imageData.getColorModel();
      final ColorScale colorScale = imageData.getColorScale();

      final NumberAxis uvMapAxis;
      if (colorScale == ColorScale.LINEAR) {
        uvMapAxis = new NumberAxis();
        if (max < 1e-3d) {
          uvMapAxis.setNumberFormatOverride(df);
        }
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
  private javax.swing.JComboBox jComboBoxColorScale;
  private javax.swing.JComboBox jComboBoxLUT;
  private javax.swing.JLabel jLabelColorScale;
  private javax.swing.JLabel jLabelLutTable;
  private javax.swing.JPanel jPanelOptions;
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

  /**
   * Enable / Disable the automatic refresh of the plot when any swing component changes.
   * Return its previous value.
   *
   * Typical use is as following :
   * // disable the automatic refresh :
   * final boolean prevAutoRefresh = this.setAutoRefresh(false);
   * try {
   *   // operations ...
   *
   * } finally {
   *   // restore the automatic refresh :
   *   this.setAutoRefresh(prevAutoRefresh);
   * }
   *
   * @param value new value
   * @return previous value
   */
  private boolean setAutoRefresh(final boolean value) {
    // first backup the state of the automatic update observation :
    final boolean previous = this.doAutoRefresh;

    // then change its state :
    this.doAutoRefresh = value;

    // return previous state :
    return previous;
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
    /** minimum value used by color conversion */
    private final float min;
    /** maximum value used by color conversion */
    private final float max;

    /**
     * Protected constructor
     * @param fitsImage fits image
     * @param colorModel image color model
     * @param colorScale color scaling method
     * @param min minimum value used by color conversion
     * @param max maximum value used by color conversion
     * @param image java2D image 
     */
    ImageChartData(final FitsImage fitsImage, final IndexColorModel colorModel, final ColorScale colorScale,
            final float min, final float max,
            final BufferedImage image) {
      this.fitsImage = fitsImage;
      this.colorModel = colorModel;
      this.colorScale = colorScale;
      this.min = min;
      this.max = max;
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

    /**
     * Return the minimum value used by color conversion
     * @return minimum value used by color conversion
     */
    public float getMin() {
      return min;
    }

    /**
     * Return the maximum value used by color conversion
     * @return maximum value used by color conversion
     */
    public float getMax() {
      return max;
    }
  }
}
