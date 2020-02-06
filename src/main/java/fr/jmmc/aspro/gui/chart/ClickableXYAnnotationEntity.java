/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import java.awt.Shape;
import org.jfree.chart.entity.XYAnnotationEntity;

/**
 * A chart entity that represents a clickable annotation on an
 * {@link org.jfree.chart.plot.XYPlot}.
 */
public final class ClickableXYAnnotationEntity extends XYAnnotationEntity {

    /** For serialization. */
    private static final long serialVersionUID = 1L;

    /* members */
    private final transient Object reference;

    /**
     * Creates a new entity.
     *
     * @param hotspot  the area.
     * @param rendererIndex  the rendererIndex (zero-based index).
     * @param toolTipText  the tool tip text.
     * @param urlText  the URL text for HTML image maps.
     */
    ClickableXYAnnotationEntity(final Object reference, final Shape hotspot, final int rendererIndex,
                                final String toolTipText, final String urlText) {
        super(hotspot, rendererIndex, toolTipText, urlText);
        // store strong reference:
        this.reference = reference;
    }

    public Object getReference() {
        return reference;
    }

    /**
     * Tests the entity for equality with an arbitrary object.
     *
     * @param obj  the object ({@code null} permitted).
     *
     * @return A boolean.
     */
    @Override
    public boolean equals(final Object obj) {
        if (!super.equals(obj)) {
            return false;
        }
        if (!(obj instanceof ClickableXYAnnotationEntity)) {
            return false;
        }
        ClickableXYAnnotationEntity that = (ClickableXYAnnotationEntity) obj;
        if (this.reference != that.getReference()) {
            return false;
        }
        return true;
    }
}
