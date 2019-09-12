/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import javax.swing.JPanel;
import net.jafama.FastMath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This panel embeds a compass (Java 6 only) or a simple JSlider if the compass can not be used
 * @author bourgesl
 */
public abstract class WindWidget extends JPanel {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(WindWidget.class.getName());
    /** direction labels */
    private static final String[] directionLabels = new String[]{"South", "South West",
        "West", "North West", "North", "North East", "East", "South East"};
    /** value property */
    public static final String PROPERTY_VALUE = "value";

    /**
     * Create a new Wind widget (azimuth)
     * @return new Wind widget (azimuth)
     */
    public static WindWidget create() {
        return new WindCompassWidget();
    }

    /**
     * Return a short textual description of wind that can be used as tooltip.
     * 
     * TODO adjust so 0° is handled as NORTH (actually inverted)
     * http://climate.umn.edu/snow_fence/components/winddirectionanddegreeswithouttable3.htm
     * http://www.meteo-26.com/ecole/ecole4.htm
     * @param value compass value in degrees [0; 360]
     * @return short wind description
     */
    protected static String getWindDescription(final double value) {
        final int index = (int) (((value + 22.5d) % 360d) / 360d * 8d);
        return directionLabels[index] + " wind";
    }

    /**
     * Create a new Compass panel
     */
    private WindWidget() {
        super(new BorderLayout(), true);
    }

    /**
     * @return compass value in degrees [0; 360]
     */
    public abstract double getValue();

    /**
     * @param value compass value in degrees [0; 360]
     */
    public abstract void setValue(final double value);

    /**
     * Create Swing components
     */
    protected abstract void initComponents();

    /**
     * Compass implementation
     */
    private static final class WindCompassWidget extends WindWidget {

        /** default serial UID for Serializable interface */
        private static final long serialVersionUID = 1;
        /** store value to fix Compass.getValue() implementation */
        private double azValue = 0d;
        /** compass */
        private CustomCompass compass = null;

        /**
         * Constructor
         */
        private WindCompassWidget() {
            setName("WindWidget");
            initComponents();

            // update tooltip with init value
            this.compass.setToolTipText(getWindDescription(getValue()));
        }

        /**
         * Override setEnabled to keep panel enabled but not the compass widget
         * @param enabled
         */
        @Override
        public void setEnabled(final boolean enabled) {
            this.compass.setEnabled(enabled);
            super.setEnabled(true);
        }

        @Override
        public double getValue() {
            // note: Compass.getValue() implementation is wrong (always returns 0)
            return this.azValue;
        }

        @Override
        public void setValue(final double value) {
            final double oldValue = getValue();
            final double newValue = (int) (Math.round(value / 5d)) * 5;

            if (newValue != oldValue) {
                this.azValue = newValue;
                this.compass.setValue(newValue);

                if (logger.isDebugEnabled()) {
                    logger.debug("compass: {}", value);
                }

                this.firePropertyChange(PROPERTY_VALUE, oldValue, newValue);

                // update tooltip
                this.compass.setToolTipText(getWindDescription(value));
            }
        }

        @Override
        protected void initComponents() {
            this.compass = new CustomCompass();
            this.compass.setNiceScale(true);
            this.compass.setThreshold(5d);

            this.compass.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(final MouseEvent e) {
                    if (compass.isEnabled()) {
                        final Point point = e.getPoint();
                        final Point2D center = compass.getCenter();

                        final Rectangle2D bounds = compass.getBounds2D();

                        final double dx = (point.getX() - center.getX()) / (0.5d * bounds.getWidth());
                        final double dy = (center.getY() - point.getY()) / (0.5d * bounds.getHeight());

                        // arctan gives angle in [-180; 180]:
                        double angle = FastMath.toDegrees(FastMath.atan2(dx, dy));

                        // get azimuth in [0; 360]:
                        if (angle < 0d) {
                            angle += 360d;
                        }

                        setValue(angle);
                    }
                }
            });

            this.add(this.compass, BorderLayout.CENTER);
        }
    }
}
