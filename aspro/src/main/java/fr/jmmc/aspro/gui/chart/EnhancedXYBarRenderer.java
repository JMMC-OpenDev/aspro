/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import org.jfree.chart.event.RendererChangeEvent;
import org.jfree.chart.renderer.xy.XYBarRenderer;

/**
 * Enhanced XYBarRenderer which event notifications can be enabled or disabled.
 * @author bourgesl
 */
public final class EnhancedXYBarRenderer extends XYBarRenderer {

    private static final long serialVersionUID = 1L;

    /* members */
    private boolean notify = true;

    /**
     * Public constructor
     */
    public EnhancedXYBarRenderer() {
        super();
    }

    /**
     * Sends a {@link RendererChangeEvent} to all registered listeners.
     *
     * @since 1.0.5
     */
    @Override
    protected void fireChangeEvent() {
        if (notify) {
            // the commented out code would be better, but only if
            // RendererChangeEvent is immutable, which it isn't.  See if there is
            // a way to fix this...
            //if (this.event == null) {
            //    this.event = new RendererChangeEvent(this);
            //}
            //notifyListeners(this.event);
            notifyListeners(new RendererChangeEvent(this));
        }
    }

    /**
     * Returns a flag that controls whether or not change events are sent to
     * registered listeners.
     *
     * @return A boolean.
     *
     * @see #setNotify(boolean)
     */
    public boolean isNotify() {
        return this.notify;
    }

    /**
     * Sets a flag that controls whether or not listeners receive
     * {@link RendererChangeEvent} notifications.
     *
     * @param notify  a boolean.
     *
     * @see #isNotify()
     */
    public void setNotify(final boolean notify) {
        this.notify = notify;
        // if the flag is being set to true, there may be queued up changes...
        if (notify) {
            fireChangeEvent();
        }
    }

}
