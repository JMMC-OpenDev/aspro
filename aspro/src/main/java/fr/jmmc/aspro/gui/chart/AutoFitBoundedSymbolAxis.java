/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import fr.jmmc.oiexplorer.core.gui.chart.BoundedSymbolAxis;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;
import java.util.List;
import org.jfree.chart.axis.AxisState;
import org.jfree.chart.event.AxisChangeEvent;
import org.jfree.ui.RectangleEdge;
import org.jfree.ui.RectangleInsets;

/**
 *
 * @author bourgesl
 */
public final class AutoFitBoundedSymbolAxis extends BoundedSymbolAxis {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** autofit font */
    private Font autoFitFont = null;

    /**
     * Constructs a symbol axis, using default attribute values where
     * necessary.
     *
     * Changes the default tick label insets
     *
     *
     * @param label  the axis label (<code>null</code> permitted).
     * @param sv  the list of symbols to display instead of the numeric
     *            values.
     */
    public AutoFitBoundedSymbolAxis(final String label, final String[] sv) {
        super(label, sv);
    }

    /**
     * Returns a clone of the object.
     *
     * @return A clone.
     *
     * @throws CloneNotSupportedException if some component of the axis does
     *         not support cloning.
     */
    @Override
    public Object clone() throws CloneNotSupportedException {
        return (AutoFitBoundedSymbolAxis) super.clone();
    }

    /**
     * Called by:
     * - org.jfree.chart.plot.XYPlot.calculateAxisSpace()
     * - org.jfree.chart.axis.ValueAxis.drawTickMarksAndLabels()
     * @param g2d
     * @param state
     * @param dataArea
     * @param edge
     * @return 
     */
    @Override
    public List refreshTicks(final Graphics2D g2d,
            final AxisState state,
            final Rectangle2D dataArea,
            final RectangleEdge edge) {

        // Use Observability Plot Context to determine once for all the appropriate font size
        // that best fits the height = 1.0

        final ObservabilityPlotContext renderContext = ObservabilityPlotContext.getInstance();

        // Fit font height (max = 1 in data space)
        final RectangleInsets tickLabelInsets = getTickLabelInsets();
        final double margin = tickLabelInsets.getTop() + tickLabelInsets.getBottom();

        // convert max width width in data units (relative to bar width) i.e. domain axis :
        final double maxHeight = Math.round(lengthToJava2D(1d, dataArea, edge)) - margin;

        final Font bestFont = renderContext.autoFitSymbolAxisFont(g2d, maxHeight);

        setTickLabelFont(bestFont);

        return super.refreshTicks(g2d, state, dataArea, edge);
    }

    /**
     * Returns the font used for the tick labels (if showing).
     *
     * @return The font (never <code>null</code>).
     *
     * @see #setTickLabelFont(Font)
     */
    @Override
    public Font getTickLabelFont() {
        if (this.autoFitFont != null) {
            return this.autoFitFont;
        }
        return super.getTickLabelFont();
    }

    /**
     * Sets the font for the tick labels and DO NOT send an {@link AxisChangeEvent}
     * to all registered listeners.
     *
     * @param font  the font (<code>null</code> not allowed).
     *
     * @see #getTickLabelFont()
     */
    @Override
    public void setTickLabelFont(final Font font) {
        if (font != null) {
            // avoid firing change events:
            this.autoFitFont = font;
        }
    }
}
