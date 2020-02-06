/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import java.awt.BasicStroke;
import java.awt.Paint;
import java.awt.Shape;
import org.jfree.chart.entity.EntityCollection;
import org.jfree.chart.entity.XYAnnotationEntity;
import org.jfree.chart.plot.PlotRenderingInfo;
import org.jfree.chart.ui.Layer;

/**
 * A clickable box annotation
 *
 * @author bourgesl
 */
public final class ClickableXYBoxAnnotation extends EnhancedXYBoxAnnotation {

    /** For serialization. */
    private static final long serialVersionUID = 1L;

    /* members */
    private final transient Object reference;

    /**
     * Creates a new annotation.
     *
     * @param reference object reference (hard) giving information to handle click
     * @param x0  the lower x-coordinate of the box (in data space).
     * @param y0  the lower y-coordinate of the box (in data space).
     * @param x1  the upper x-coordinate of the box (in data space).
     * @param y1  the upper y-coordinate of the box (in data space).
     * @param stroke  the shape stroke (<code>null</code> permitted).
     * @param outlinePaint  the shape color (<code>null</code> permitted).
     * @param fillPaint  the paint used to fill the shape (<code>null</code>
     *                   permitted).
     * @param layer layer to draw this annotation = foreground or background (default)
     * @param tooltipText optional tooltip text
     */
    public ClickableXYBoxAnnotation(final Object reference, final double x0, final double y0, final double x1, final double y1,
                                    final BasicStroke stroke, final Paint outlinePaint, final Paint fillPaint,
                                    final Layer layer, final String tooltipText) {
        super(x0, y0, x1, y1, stroke, outlinePaint, fillPaint, layer, tooltipText);
        // store strong reference:
        this.reference = reference;
    }

    /**
     * A utility method for adding an {@link XYAnnotationEntity} to
     * a {@link PlotRenderingInfo} instance.
     *
     * @param info  the plot rendering info ({@code null} permitted).
     * @param hotspot  the hotspot area.
     * @param rendererIndex  the renderer index.
     * @param toolTipText  the tool tip text.
     * @param urlText  the URL text.
     */
    @Override
    protected void addEntity(final PlotRenderingInfo info,
                             final Shape hotspot, final int rendererIndex,
                             final String toolTipText, final String urlText) {
        if (info == null) {
            return;
        }
        final EntityCollection entities = info.getOwner().getEntityCollection();
        if (entities == null) {
            return;
        }
        entities.add(
                new ClickableXYAnnotationEntity(this.reference,
                        hotspot, rendererIndex, toolTipText, urlText)
        );
    }

    public Object getReference() {
        return reference;
    }

}
