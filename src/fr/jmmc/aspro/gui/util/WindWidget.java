/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.jmcs.gui.util.SwingUtils;
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
    private eu.hansolo.steelseries.extras.Compass compass = null;

    /**
     * Constructor
     */
    private WindCompassWidget() {
      setName("WindWidget");
      initComponents();
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
      }
    }

    @Override
    protected void initComponents() {
      this.compass = new eu.hansolo.steelseries.extras.Compass();
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
            double angle = Math.toDegrees(Math.atan2(dx, dy));

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
      labelTable.put(Integer.valueOf(0), new JLabel("N"));
      labelTable.put(Integer.valueOf(90), new JLabel("E"));
      labelTable.put(Integer.valueOf(180), new JLabel("S"));
      labelTable.put(Integer.valueOf(270), new JLabel("W"));
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
