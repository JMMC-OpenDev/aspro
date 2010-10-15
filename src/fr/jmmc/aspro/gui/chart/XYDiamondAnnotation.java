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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Polygon;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
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

public class XYDiamondAnnotation extends AbstractXYAnnotation implements
        Cloneable, PublicCloneable, Serializable {

  /**
   * Una Annotazione a forma di rombo può essere aggiunta su un {@link org.jfree.chart.plot.XYPlot}.
   */
  private static final long serialVersionUID = -2184152269531019722L;
  /** outline color */
  public static final Paint DEFAULT_OUTLINE_PAINT = Color.BLACK;
  /** Colore di default. */
  public static final Paint DEFAULT_PAINT = Color.YELLOW;
  /** Larghezza del tratto di default. */
  public static final Stroke DEFAULT_STROKE = new BasicStroke(1.0f);
  /** Fattore di Scala di default. */
  public static final double DEFAULT_SCALE = 1.0;

  /* members */
  /** Fattore di Scala. */
  private double drawScaleFactor;
  /** Coordinata x. */
  private double x;
  /** Coordinata y. */
  private double y;
  /** Larghezza. */
  private double displayWidth;
  /** Altezza. */
  private double displayHeight;
  /** Larghezza del Tratto . */
  private transient Stroke diamondStroke;
  /** The diamond paint. */
  private transient Paint diamondPaint;

  public XYDiamondAnnotation(double x, double y, double width, double height) {
    this.x = x;
    this.y = y;
    this.displayWidth = width;
    this.displayHeight = height;
    this.drawScaleFactor = DEFAULT_SCALE;
    this.diamondPaint = DEFAULT_PAINT;
    this.diamondStroke = DEFAULT_STROKE;
  }

  /**
   * @return Ritorna il Fattore di Scala.
   */
  public double getDrawScaleFactor() {
    return drawScaleFactor;
  }

  /**
   * @param drawScaleFactor Fattore di Scala da settare internamente alla classe.
   */
  public void setDrawScaleFactor(double drawScaleFactor) {
    this.drawScaleFactor = drawScaleFactor;
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
  public void setX(double x) {
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
  public void setY(double y) {
    this.y = y;
  }

  /**
   * @return Ritorna la larghezza del diamante.
   */
  public double getDisplayWidth() {
    return displayWidth;
  }

  /**
   * @param displayWidth Setta la larghezza del diamante.
   */
  public void setDisplayWidth(double displayWidth) {
    this.displayWidth = displayWidth;
  }

  /**
   * @return Ritorna l'altezza del diamante.
   */
  public double getDisplayHeight() {
    return displayHeight;
  }

  /**
   * @param displayHeight Setta l'altezza del diamante.
   */
  public void setDisplayHeight(double displayHeight) {
    this.displayHeight = displayHeight;
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
  public void setStroke(Stroke diamondStroke) {
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
  public void setPaint(Paint diamondPaint) {
    this.diamondPaint = diamondPaint;
  }

  /**
   * @see org.jfree.chart.annotations.XYAnnotation#draw(java.awt.Graphics2D, org.jfree.chart.plot.XYPlot, java.awt.geom.Rectangle2D, org.jfree.chart.axis.CategoryAxis, org.jfree.chart.axis.ValueAxis,RenderIndex,org.jfree.chart.plot.PlotRenderingInfo)
   */
  @Override
  public void draw(Graphics2D g2, XYPlot plot, Rectangle2D dataArea,
                   ValueAxis domainAxis, ValueAxis rangeAxis, int rendererIndex,
                   PlotRenderingInfo info) {
    PlotOrientation orientation = plot.getOrientation();
    RectangleEdge domainEdge = Plot.resolveDomainAxisLocation(
            plot.getDomainAxisLocation(), orientation);
    RectangleEdge rangeEdge = Plot.resolveRangeAxisLocation(
            plot.getRangeAxisLocation(), orientation);
    float j2DX = (float) domainAxis.valueToJava2D(this.x, dataArea, domainEdge);
    float j2DY = (float) rangeAxis.valueToJava2D(this.y, dataArea, rangeEdge);

    if (orientation == PlotOrientation.HORIZONTAL) {
      float tempAnchor = j2DX;
      j2DX = j2DY;
      j2DY = tempAnchor;
    }

    Rectangle2D displayArea = new Rectangle2D.Double(j2DX - this.displayWidth / 2.0, j2DY - this.displayHeight / 2.0, this.displayWidth,
            this.displayHeight);
    AffineTransform savedTransform = g2.getTransform();
    Rectangle2D drawArea = new Rectangle2D.Double(0.0, 0.0, this.displayWidth * this.drawScaleFactor, this.displayHeight * this.drawScaleFactor);

    g2.scale(1 / this.drawScaleFactor, 1 / this.drawScaleFactor);
    g2.translate((j2DX - this.displayWidth / 2.0) * this.drawScaleFactor,
            (j2DY - this.displayHeight / 2.0) * this.drawScaleFactor);
    drawDiamond(g2, drawArea);
    g2.setTransform(savedTransform);
    String toolTip = getToolTipText();
    String url = getURL();
    if (toolTip != null || url != null) {
      addEntity(info, displayArea, rendererIndex, toolTip, url);
    }

  }

  /**
   * @param g2 Dispositivo di disegno
   * @param drawArea Area in cui disegnare il diamante.
   */
  private void drawDiamond(Graphics2D g2, Rectangle2D drawArea) {
    Polygon diamond = new Polygon();
    int offset = (int) drawArea.getWidth() / 2;
    int coordX = (int) drawArea.getCenterX();
    int coordY = (int) drawArea.getCenterY();
//disegnamo il rombo partendo da un punto
    diamond.addPoint(coordX, coordY - offset);
    diamond.addPoint(coordX - offset, coordY);
    diamond.addPoint(coordX, coordY + offset);
    diamond.addPoint(coordX + offset, coordY);
//ritorniamo al punto di partenza per chiudere il poligono
    diamond.addPoint(coordX, coordY - offset);
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
  public boolean equals(Object object) {

    if (object == null) {
      return false;
    }
    if (object == this) {
      return true;
    }
    if (object instanceof XYDiamondAnnotation) {
      XYDiamondAnnotation diamond = (XYDiamondAnnotation) object;
      if (super.equals(object)) {
        boolean b0 = this.x == diamond.getX();
        boolean b1 = this.y == diamond.getY();
        boolean b2 = this.getToolTipText().equals(diamond.getToolTipText());
        boolean b3 = this.getURL().equals(diamond.getURL());
        return b0 && b1 && b2 && b3;
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
  public Object clone() throws CloneNotSupportedException {
    return super.clone();
  }
}
