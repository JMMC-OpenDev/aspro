/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.util.NumberUtils;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Hashtable;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import net.jafama.FastMath;
import org.apache.commons.lang.SystemUtils;
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
        // Compass widget requires Java 6:
        if (SystemUtils.JAVA_VERSION_FLOAT >= 1.6f) {
            try {
                return new WindCompassWidget();
            } catch (Throwable th) {
                logger.warn("Unable to create WindCompassWidget:", th);
            }
        }
        return new WindSliderWidget();
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
//        private eu.hansolo.steelseries.extras.Compass compass = null;        
        private fr.jmmc.aspro.gui.util.CustomCompass compass = null;

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
//            this.compass = new eu.hansolo.steelseries.extras.Compass();
            this.compass = new fr.jmmc.aspro.gui.util.CustomCompass();
            this.compass.setNiceScale(true);
            this.compass.setThreshold(5d);
            this.compass.setMinimumSize(new Dimension(16, 16));

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

    /**
     * Slider implementation
     */
    private static final class WindSliderWidget extends WindWidget implements ChangeListener {

        /** default serial UID for Serializable interface */
        private static final long serialVersionUID = 1;
        /** compass */
        private JSlider slider = null;

        /**
         * Constructor
         */
        private WindSliderWidget() {
            setName("WindWidget");
            initComponents();

            // update tooltip with init value
            this.slider.setToolTipText(getWindDescription(getValue()));
        }

        /**
         * Override setEnabled to keep panel enabled but not the compass widget
         * @param enabled
         */
        @Override
        public void setEnabled(final boolean enabled) {
            this.slider.setEnabled(enabled);
            super.setEnabled(true);
        }

        @Override
        public double getValue() {
            return this.slider.getValue();
        }

        @Override
        public void setValue(final double value) {
            final int oldValue = (int) getValue();
            final int newValue = (int) (Math.round(value / 5d)) * 5;

            if (newValue != oldValue) {
                this.slider.setValue(newValue);

                if (logger.isDebugEnabled()) {
                    logger.debug("slider: {}", value);
                }

                this.firePropertyChange(PROPERTY_VALUE, oldValue, newValue);
            }

            // update tooltip
            this.slider.setToolTipText(getWindDescription(value));
        }

        @Override
        protected void initComponents() {
            this.slider = new JSlider(0, 359, 0);
            this.slider.setMajorTickSpacing(45);
            this.slider.setPaintTicks(true);
            // reduce widget size:
            this.slider.setPreferredSize(new Dimension(40, 40));

            // Create the label table:
            final Hashtable<Integer, JLabel> labelTable = new Hashtable<Integer, JLabel>(4);
            labelTable.put(NumberUtils.valueOf(0), new JLabel("N"));
            labelTable.put(NumberUtils.valueOf(90), new JLabel("E"));
            labelTable.put(NumberUtils.valueOf(180), new JLabel("S"));
            labelTable.put(NumberUtils.valueOf(270), new JLabel("W"));
            this.slider.setLabelTable(labelTable);

            this.slider.setPaintLabels(true);
            this.slider.addChangeListener(this);

            this.add(this.slider, BorderLayout.CENTER);
        }

        /**
         * Handle the stateChanged event from the slider.
         * @param ce slider change event
         */
        @Override
        public void stateChanged(final ChangeEvent ce) {
            if (!this.slider.getValueIsAdjusting()) {

                SwingUtils.invokeLaterEDT(new Runnable() {
                    @Override
                    public void run() {
                        setValue(slider.getValue());
                    }
                });
            }
        }
    }
}
