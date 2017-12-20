/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import java.awt.AlphaComposite;
import java.awt.Composite;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Map;
import org.jfree.chart.axis.AxisSpace;
import org.jfree.chart.axis.AxisState;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.CrosshairState;
import org.jfree.chart.plot.DatasetRenderingOrder;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.PlotRenderingInfo;
import org.jfree.chart.plot.PlotState;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.chart.ui.Layer;
import org.jfree.chart.ui.RectangleEdge;
import org.jfree.chart.ui.RectangleInsets;
import org.jfree.data.xy.XYDataset;

/**
 * This hacked XYPlot fixes rendering order problem between grid lines (BEFORE) and background markers (AFTER)
 * @author bourgesl
 */
public final class GridLineFixedXYPlot extends XYPlot {

  /** For serialization. */
  private static final long serialVersionUID = 1L;

  /**
   * Creates a new plot with the specified dataset, axes and renderer.  Any
   * of the arguments can be <code>null</code>, but in that case you should
   * take care to specify the value before using the plot (otherwise a
   * <code>NullPointerException</code> may be thrown).
   *
   * @param dataset  the dataset (<code>null</code> permitted).
   * @param domainAxis  the domain axis (<code>null</code> permitted).
   * @param rangeAxis  the range axis (<code>null</code> permitted).
   * @param renderer  the renderer (<code>null</code> permitted).
   */
  public GridLineFixedXYPlot(final XYDataset dataset,
          final ValueAxis domainAxis,
          final ValueAxis rangeAxis,
          final XYItemRenderer renderer) {
    super(dataset, domainAxis, rangeAxis, renderer);
  }

  /**
   * Draws the plot within the specified area on a graphics device.
   *
   * @param g2  the graphics device.
   * @param area  the plot area (in Java2D space).
   * @param anchor  an anchor point in Java2D space (<code>null</code>
   *                permitted).
   * @param parentState  the state from the parent plot, if there is one
   *                     (<code>null</code> permitted).
   * @param info  collects chart drawing information (<code>null</code>
   *              permitted).
   */
  @Override
  public void draw(final Graphics2D g2, final Rectangle2D area, Point2D anchor,
          final PlotState parentState, final PlotRenderingInfo info) {

    // if the plot area is too small, just return...
    boolean b1 = (area.getWidth() <= MINIMUM_WIDTH_TO_DRAW);
    boolean b2 = (area.getHeight() <= MINIMUM_HEIGHT_TO_DRAW);
    if (b1 || b2) {
      return;
    }

    // record the plot area...
    if (info != null) {
      info.setPlotArea(area);
    }

    // adjust the drawing area for the plot insets (if any)...
    RectangleInsets insets = getInsets();
    insets.trim(area);

    AxisSpace space = calculateAxisSpace(g2, area);
    Rectangle2D dataArea = space.shrink(area, null);
    this.getAxisOffset().trim(dataArea);
    createAndAddEntity((Rectangle2D) dataArea.clone(), info, null, null);
    if (info != null) {
      info.setDataArea(dataArea);
    }

    // draw the plot background and axes...
    drawBackground(g2, dataArea);
    Map axisStateMap = drawAxes(g2, area, dataArea, info);

    PlotOrientation orient = getOrientation();

    // the anchor point is typically the point where the mouse last
    // clicked - the crosshairs will be driven off this point...
    if (anchor != null && !dataArea.contains(anchor)) {
      anchor = null;
    }
    CrosshairState crosshairState = new CrosshairState();
    crosshairState.setCrosshairDistance(Double.POSITIVE_INFINITY);
    crosshairState.setAnchor(anchor);

    crosshairState.setAnchorX(Double.NaN);
    crosshairState.setAnchorY(Double.NaN);
    if (anchor != null) {
      ValueAxis domainAxis = getDomainAxis();
      if (domainAxis != null) {
        double x;
        if (orient == PlotOrientation.VERTICAL) {
          x = domainAxis.java2DToValue(anchor.getX(), dataArea, getDomainAxisEdge());
        } else {
          x = domainAxis.java2DToValue(anchor.getY(), dataArea, getDomainAxisEdge());
        }
        crosshairState.setAnchorX(x);
      }
      ValueAxis rangeAxis = getRangeAxis();
      if (rangeAxis != null) {
        double y;
        if (orient == PlotOrientation.VERTICAL) {
          y = rangeAxis.java2DToValue(anchor.getY(), dataArea, getRangeAxisEdge());
        } else {
          y = rangeAxis.java2DToValue(anchor.getX(), dataArea, getRangeAxisEdge());
        }
        crosshairState.setAnchorY(y);
      }
    }
    crosshairState.setCrosshairX(getDomainCrosshairValue());
    crosshairState.setCrosshairY(getRangeCrosshairValue());
    Shape originalClip = g2.getClip();
    Composite originalComposite = g2.getComposite();

    g2.clip(dataArea);
    g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, getForegroundAlpha()));

    final int rendererCount = this.getRendererCount();

    // Draw background markers BEFORE gridlines and baselines:
    // draw the markers that are associated with a specific renderer...
    for (int i = 0; i < rendererCount; i++) {
      drawDomainMarkers(g2, dataArea, i, Layer.BACKGROUND);
    }
    for (int i = 0; i < rendererCount; i++) {
      drawRangeMarkers(g2, dataArea, i, Layer.BACKGROUND);
    }

    AxisState domainAxisState = (AxisState) axisStateMap.get(getDomainAxis());
    if (domainAxisState == null) {
      if (parentState != null) {
        domainAxisState = (AxisState) parentState.getSharedAxisStates().get(getDomainAxis());
      }
    }

    AxisState rangeAxisState = (AxisState) axisStateMap.get(getRangeAxis());
    if (rangeAxisState == null) {
      if (parentState != null) {
        rangeAxisState = (AxisState) parentState.getSharedAxisStates().get(getRangeAxis());
      }
    }
    if (domainAxisState != null) {
      drawDomainTickBands(g2, dataArea, domainAxisState.getTicks());
    }
    if (rangeAxisState != null) {
      drawRangeTickBands(g2, dataArea, rangeAxisState.getTicks());
    }
    if (domainAxisState != null) {
      drawDomainGridlines(g2, dataArea, domainAxisState.getTicks());
      drawZeroDomainBaseline(g2, dataArea);
    }
    if (rangeAxisState != null) {
      drawRangeGridlines(g2, dataArea, rangeAxisState.getTicks());
      drawZeroRangeBaseline(g2, dataArea);
    }

    // Draw background markers WAS HERE BEFORE HACK:
/*    
    // draw the markers that are associated with a specific renderer...
    for (int i = 0; i < rendererCount; i++) {
    drawDomainMarkers(g2, dataArea, i, Layer.BACKGROUND);
    }
    for (int i = 0; i < rendererCount; i++) {
    drawRangeMarkers(g2, dataArea, i, Layer.BACKGROUND);
    }
     */
    // now draw annotations and render data items...
    boolean foundData = false;
    DatasetRenderingOrder order = getDatasetRenderingOrder();
    if (order == DatasetRenderingOrder.FORWARD) {

      // draw background annotations
      for (int i = 0; i < rendererCount; i++) {
        XYItemRenderer r = getRenderer(i);
        if (r != null) {
          ValueAxis domainAxis = getDomainAxisForDataset(i);
          ValueAxis rangeAxis = getRangeAxisForDataset(i);
          r.drawAnnotations(g2, dataArea, domainAxis, rangeAxis, Layer.BACKGROUND, info);
        }
      }

      // render data items...
      for (int i = 0; i < getDatasetCount(); i++) {
        foundData = render(g2, dataArea, i, info, crosshairState)
                || foundData;
      }

      // draw foreground annotations
      for (int i = 0; i < rendererCount; i++) {
        XYItemRenderer r = getRenderer(i);
        if (r != null) {
          ValueAxis domainAxis = getDomainAxisForDataset(i);
          ValueAxis rangeAxis = getRangeAxisForDataset(i);
          r.drawAnnotations(g2, dataArea, domainAxis, rangeAxis, Layer.FOREGROUND, info);
        }
      }

    } else if (order == DatasetRenderingOrder.REVERSE) {

      // draw background annotations
      for (int i = rendererCount - 1; i >= 0; i--) {
        XYItemRenderer r = getRenderer(i);
        if (i >= getDatasetCount()) { // we need the dataset to make
          continue;                 // a link to the axes
        }
        if (r != null) {
          ValueAxis domainAxis = getDomainAxisForDataset(i);
          ValueAxis rangeAxis = getRangeAxisForDataset(i);
          r.drawAnnotations(g2, dataArea, domainAxis, rangeAxis, Layer.BACKGROUND, info);
        }
      }

      for (int i = getDatasetCount() - 1; i >= 0; i--) {
        foundData = render(g2, dataArea, i, info, crosshairState) || foundData;
      }

      // draw foreground annotations
      for (int i = rendererCount - 1; i >= 0; i--) {
        XYItemRenderer r = getRenderer(i);
        if (i >= getDatasetCount()) { // we need the dataset to make
          continue;                 // a link to the axes
        }
        if (r != null) {
          ValueAxis domainAxis = getDomainAxisForDataset(i);
          ValueAxis rangeAxis = getRangeAxisForDataset(i);
          r.drawAnnotations(g2, dataArea, domainAxis, rangeAxis, Layer.FOREGROUND, info);
        }
      }

    }
/*
    // draw domain crosshair if required...
    if (isDomainCrosshairVisible()) {
      int xAxisIndex = crosshairState.getDomainAxisIndex();
      ValueAxis xAxis = getDomainAxis(xAxisIndex);
      RectangleEdge xAxisEdge = getDomainAxisEdge(xAxisIndex);
      if (!this.isDomainCrosshairLockedOnData() && anchor != null) {
        double xx;
        if (orient == PlotOrientation.VERTICAL) {
          xx = xAxis.java2DToValue(anchor.getX(), dataArea, xAxisEdge);
        } else {
          xx = xAxis.java2DToValue(anchor.getY(), dataArea, xAxisEdge);
        }
        crosshairState.setCrosshairX(xx);
      }
      setDomainCrosshairValue(crosshairState.getCrosshairX(), false);
      double x = getDomainCrosshairValue();
      Paint paint = getDomainCrosshairPaint();
      Stroke stroke = getDomainCrosshairStroke();
      drawDomainCrosshair(g2, dataArea, orient, x, xAxis, stroke, paint);
    }

    // draw range crosshair if required...
    if (isRangeCrosshairVisible()) {
      int yAxisIndex = crosshairState.getRangeAxisIndex();
      ValueAxis yAxis = getRangeAxis(yAxisIndex);
      RectangleEdge yAxisEdge = getRangeAxisEdge(yAxisIndex);
      if (!this.isDomainCrosshairLockedOnData() && anchor != null) {
        double yy;
        if (orient == PlotOrientation.VERTICAL) {
          yy = yAxis.java2DToValue(anchor.getY(), dataArea, yAxisEdge);
        } else {
          yy = yAxis.java2DToValue(anchor.getX(), dataArea, yAxisEdge);
        }
        crosshairState.setCrosshairY(yy);
      }
      setRangeCrosshairValue(crosshairState.getCrosshairY(), false);
      double y = getRangeCrosshairValue();
      Paint paint = getRangeCrosshairPaint();
      Stroke stroke = getRangeCrosshairStroke();
      drawRangeCrosshair(g2, dataArea, orient, y, yAxis, stroke, paint);
    }
*/
        // draw domain crosshair if required...
        int datasetIndex = crosshairState.getDatasetIndex();
        ValueAxis xAxis = getDomainAxisForDataset(datasetIndex);
        RectangleEdge xAxisEdge = getDomainAxisEdge(getDomainAxisIndex(xAxis));
        if (!this.isDomainCrosshairLockedOnData() && anchor != null) {
            double xx;
            if (orient == PlotOrientation.VERTICAL) {
                xx = xAxis.java2DToValue(anchor.getX(), dataArea, xAxisEdge);
            }
            else {
                xx = xAxis.java2DToValue(anchor.getY(), dataArea, xAxisEdge);
            }
            crosshairState.setCrosshairX(xx);
        }
        setDomainCrosshairValue(crosshairState.getCrosshairX(), false);
        if (isDomainCrosshairVisible()) {
            double x = getDomainCrosshairValue();
            Paint paint = getDomainCrosshairPaint();
            Stroke stroke = getDomainCrosshairStroke();
            drawDomainCrosshair(g2, dataArea, orient, x, xAxis, stroke, paint);
        }

        // draw range crosshair if required...
        ValueAxis yAxis = getRangeAxisForDataset(datasetIndex);
        RectangleEdge yAxisEdge = getRangeAxisEdge(getRangeAxisIndex(yAxis));
        if (!this.isRangeCrosshairLockedOnData() && anchor != null) {
            double yy;
            if (orient == PlotOrientation.VERTICAL) {
                yy = yAxis.java2DToValue(anchor.getY(), dataArea, yAxisEdge);
            } else {
                yy = yAxis.java2DToValue(anchor.getX(), dataArea, yAxisEdge);
            }
            crosshairState.setCrosshairY(yy);
        }
        setRangeCrosshairValue(crosshairState.getCrosshairY(), false);
        if (isRangeCrosshairVisible()) {
            double y = getRangeCrosshairValue();
            Paint paint = getRangeCrosshairPaint();
            Stroke stroke = getRangeCrosshairStroke();
            drawRangeCrosshair(g2, dataArea, orient, y, yAxis, stroke, paint);
        }


    if (!foundData) {
      drawNoDataMessage(g2, dataArea);
    }

    for (int i = 0; i < rendererCount; i++) {
      drawDomainMarkers(g2, dataArea, i, Layer.FOREGROUND);
    }
    for (int i = 0; i < rendererCount; i++) {
      drawRangeMarkers(g2, dataArea, i, Layer.FOREGROUND);
    }

    drawAnnotations(g2, dataArea, info);
    g2.setClip(originalClip);
    g2.setComposite(originalComposite);

    drawOutline(g2, dataArea);
  }
}
