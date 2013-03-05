/* ===========================================================
 * JFreeChart : a free chart library for the Java(tm) platform
 * ===========================================================
 *
 * (C) Copyright 2000-2004, by Passiatore Gianluigi and Contributors.
 *
 * Project Info: http://www.jfree.org/jfreechart/index.html
 *
 * This library is free software; you can redistribute it and/or modify it under the terms
 * of the GNU Lesser General Public License as published by the Free Software Foundation;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this
 * library; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * [Java is a trademark or registered trademark of Sun Microsystems, Inc.
 * in the United States and other countries.]
 *
 * ---------------------------
 * XYDiamondAnnotation.java
 * ---------------------------
 * (C) Copyright 2003, 2004, by Passiatore Gianluigi.
 *
 * Original Author: Passiatore Gianluigi;
 * Contributor(s): -;
 *
 * Changes:
 * --------
 * 06-Mar-2009 : Version 1 (DG);
 *
 *
 */
package fr.jmmc.aspro.gui.chart;

import fr.jmmc.oiexplorer.core.gui.chart.ChartUtils;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.awt.geom.Rectangle2D;
import java.io.Serializable;
import org.jfree.chart.annotations.AbstractXYAnnotation;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.Plot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.PlotRenderingInfo;
import org.jfree.chart.plot.XYPlot;
import org.jfree.ui.RectangleEdge;
import org.jfree.util.PublicCloneable;

/**
 * Custom annotation to plot a diamond mark that supports also auto scaling 
 * @author bourgesl
 */
public final class XYDiamondAnnotation extends AbstractXYAnnotation implements
        Cloneable, PublicCloneable, Serializable {

    /**
     * Una Annotazione a forma di rombo può essere aggiunta su un {@link org.jfree.chart.plot.XYPlot}.
     */
    private static final long serialVersionUID = -2184152269531019722L;
    /** outline color */
    public static final Paint DEFAULT_OUTLINE_PAINT = Color.BLACK;
    /** Colore di default. */
    public static final Paint DEFAULT_PAINT = Color.YELLOW;
    /** margin in pixels */
    private final static int MARGIN = 1;
    /* members */
    /** Coordinata x (in data space). */
    private double x;
    /** Coordinata y (in data space). */
    private double y;
    /** standard size (maximum size) in pixels */
    private double displaySize;
    /** Larghezza del Tratto . */
    private transient Stroke diamondStroke;
    /** The diamond paint. */
    private transient Paint diamondPaint;

    /**
     * Public constructor
     * @param x  the x-coordinate (in data space).
     * @param y  the y-coordinate (in data space).
     */
    public XYDiamondAnnotation(final double x, final double y) {
        this(x, y, 10d);
    }

    /**
     * Public constructor
     * @param x  the x-coordinate (in data space).
     * @param y  the y-coordinate (in data space).
     * @param size standard size (in java2D)
     */
    public XYDiamondAnnotation(final double x, final double y, final double size) {
        this.x = x;
        this.y = y;
        this.displaySize = size;
        this.diamondPaint = DEFAULT_PAINT;
        this.diamondStroke = ChartUtils.DEFAULT_STROKE;
    }

    /**
     * @return Ritorna la coordinata x.
     */
    public double getX() {
        return x;
    }

    /**
     * @param x x da settare internamente alla classe.
     */
    public void setX(final double x) {
        this.x = x;
    }

    /**
     * @return Ritorna la coordinata y.
     */
    public double getY() {
        return y;
    }

    /**
     * @param y y da settare internamente alla classe.
     */
    public void setY(final double y) {
        this.y = y;
    }

    /**
     * @return Ritorna la larghezza del diamante.
     */
    public double getDisplaySize() {
        return displaySize;
    }

    /**
     * @param displaySize Setta la larghezza del diamante.
     */
    public void setDisplaySize(final double displaySize) {
        this.displaySize = displaySize;
    }

    /**
     * @return Ritorna la larghezza del tratto di disegno.
     */
    public Stroke getStroke() {
        return diamondStroke;
    }

    /**
     * @param diamondStroke Setta la larghezza del tratto di disegno.
     */
    public void setStroke(final Stroke diamondStroke) {
        this.diamondStroke = diamondStroke;
    }

    /**
     * @return Ritorna il colore del diamante.
     */
    public Paint getPaint() {
        return diamondPaint;
    }

    /**
     * @param diamondPaint Setta il colore del diamante.
     */
    public void setPaint(final Paint diamondPaint) {
        this.diamondPaint = diamondPaint;
    }

    /**
     * @see org.jfree.chart.annotations.XYAnnotation#draw(java.awt.Graphics2D, org.jfree.chart.plot.XYPlot, java.awt.geom.Rectangle2D, org.jfree.chart.axis.ValueAxis, org.jfree.chart.axis.ValueAxis, int, org.jfree.chart.plot.PlotRenderingInfo)
     */
    @Override
    public void draw(final Graphics2D g2, final XYPlot plot, final Rectangle2D dataArea,
            final ValueAxis domainAxis, final ValueAxis rangeAxis,
            final int rendererIndex, final PlotRenderingInfo info) {

        final PlotOrientation orientation = plot.getOrientation();

        final RectangleEdge domainEdge = Plot.resolveDomainAxisLocation(plot.getDomainAxisLocation(), orientation);
        final RectangleEdge rangeEdge = Plot.resolveRangeAxisLocation(plot.getRangeAxisLocation(), orientation);

        double j2DX = domainAxis.valueToJava2D(this.x, dataArea, domainEdge);
        double j2DY = rangeAxis.valueToJava2D(this.y, dataArea, rangeEdge);

        if (orientation == PlotOrientation.HORIZONTAL) {
            final double tempAnchor = j2DX;
            j2DX = j2DY;
            j2DY = tempAnchor;
        }

        // Use Observability Plot Context to determine once for all the appropriate display size
        // that best fits the bar width; if too large, do not render the annotation

        final ObservabilityPlotContext renderContext = ObservabilityPlotContext.getInstance();

        final double drawScaleFactor;

        if (renderContext.autoFitDiamondSizeDone()) {
            drawScaleFactor = renderContext.autoFitDiamondScale();
        } else {
            // first time, perform scaling:

            // convert max width width in data units (relative to bar width) i.e. domain axis :
            final double j2Max = Math.round(domainAxis.lengthToJava2D(renderContext.getMaxDiamondWidth(), dataArea, domainEdge)) - 2d * MARGIN;

            drawScaleFactor = renderContext.autoFitDiamondSize(j2Max, this.displaySize);
        }

        // Dont render if the size do not fit in block size:
        if (drawScaleFactor == 0d) {
            return;
        }

        final double size = this.displaySize * drawScaleFactor;

        final AffineTransform savedTransform = g2.getTransform();

        final Rectangle2D drawArea = new Rectangle2D.Double(0d, 0d, size, size);

        g2.translate(j2DX - 0.5d * size, j2DY - 0.5d * size);

        drawDiamond(g2, drawArea);

        g2.setTransform(savedTransform);

        final String toolTip = getToolTipText();
        final String url = getURL();
        if (toolTip != null || url != null) {
            final Rectangle2D displayArea = new Rectangle2D.Double(j2DX - 0.5d * size, j2DY - 0.5d * size, size, size);

            addEntity(info, displayArea, rendererIndex, toolTip, url);
        }
    }

    /**
     * @param g2 Dispositivo di disegno
     * @param drawArea Area in cui disegnare il diamante.
     */
    private void drawDiamond(final Graphics2D g2, final Rectangle2D drawArea) {
        // use float (java 1.5 support):
        final float offset = (float) (0.5d * drawArea.getWidth());
        final float coordX = (float) drawArea.getCenterX();
        final float coordY = (float) drawArea.getCenterY();

        final GeneralPath diamond = new GeneralPath(GeneralPath.WIND_NON_ZERO, 6);
        diamond.moveTo(coordX, coordY - offset);
        diamond.lineTo(coordX - offset, coordY);
        diamond.lineTo(coordX - offset, coordY);
        diamond.lineTo(coordX, coordY + offset);
        diamond.lineTo(coordX + offset, coordY);
        diamond.lineTo(coordX, coordY - offset);

        g2.setPaint(getPaint());
        g2.setStroke(getStroke());
        g2.fill(diamond);

        // Outline paint :
        g2.setPaint(DEFAULT_OUTLINE_PAINT);
        g2.draw(diamond);
    }

    /**
     * Controlla se l'oggetto è uguale ad un'altro passato.
     *
     * @param object oggetto per il confronto.
     *
     * @return <code>true</code> or <code>false</code>.
     */
    @Override
    public boolean equals(final Object object) {

        if (object == null) {
            return false;
        }
        if (object == this) {
            return true;
        }
        if (object instanceof XYDiamondAnnotation) {
            final XYDiamondAnnotation diamond = (XYDiamondAnnotation) object;
            if (super.equals(object)) {
                return (this.x == diamond.getX()) && (this.y == diamond.getY());
            }
        }
        return false;
    }

    /**
     * Returna un clone della annotazione.
     *
     * @return un clone.
     *
     * @throws CloneNotSupportedException questa classe può non lanciare questa exception, ma le sottoclassi (se esistono) debbono.
     */
    @Override
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }
}
