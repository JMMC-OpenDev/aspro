/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: FieldSliderAdapter.java,v 1.2 2010-02-08 17:00:17 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/01/21 16:38:42  bourgesl
 * Custom Numeric text field binding with a JSlider
 *
 */
package fr.jmmc.aspro.gui.util;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.ParseException;
import java.util.logging.Level;
import javax.swing.event.EventListenerList;
import javax.swing.JFormattedTextField;
import javax.swing.JSlider;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * This class manages the interaction between a JFormattedTextField (numeric) and a JSlider
 * @author bourgesl
 */
public class FieldSliderAdapter implements ChangeListener, PropertyChangeListener {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.util.FieldSliderAdapter";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /* members */
  /** minimum limit */
  private double minLimit;
  /** minimum value >= minimum Limit */
  private double minValue;
  /** maximum value <= maximum Limit */
  private double maxValue;
  /** default value */
  private double defValue;
  /** slider ratio i.e. double value vs slider int value */
  private double sliderRatio;
  /** numeric text field */
  private JFormattedTextField field;
  /** slider */
  private JSlider slider;
  /* event handling */
  /** flag to indicate that an event handling is in progress */
  private boolean isEventHandling = false;
  /** A list of event listeners for this component. */
  protected EventListenerList listenerList = new EventListenerList();
  /**
   * Only one <code>ChangeEvent</code> is needed per instance since the
   * event's only (read-only) state is the source property.  The source
   * of events generated here is always "this". The event is lazily
   * created the first time that an event notification is fired.
   *
   * @see #fireStateChanged
   */
  protected transient ChangeEvent changeEvent = null;

  /**
   * Constructor
   * @param slider JSlider
   * @param field JFormattedTextField
   * @param min minimum value
   * @param max maximum value
   * @param def default value
   */
  public FieldSliderAdapter(final JSlider slider, final JFormattedTextField field, final double min, final double max, final double def) {
    this.slider = slider;
    this.field = field;

    reset(min, max, def);

    init();
  }

  /**
   * Redefine the extrema and set the default value
   * @param min minimum value
   * @param max maximum value
   * @param def default value
   */
  public void reset(final double min, final double max, final double def) {

    this.minLimit = min;
    this.minValue = min;
    
    this.maxValue = max;

    this.defValue = def;
    
    this.sliderRatio = (max - min) / (this.slider.getModel().getMaximum() - this.slider.getModel().getMinimum());

    this.field.setValue(Double.valueOf(this.defValue));
  }

  /**
   * Declare this class as a change listener for both slider and text field
   */
  private void init() {
    this.slider.addChangeListener(this);

    this.field.addPropertyChangeListener("value", this);
  }

  /** 
   * Handle the stateChanged event from the slider.
   */
  public void stateChanged(final ChangeEvent ce) {
    if (!this.isEventHandling) {
      final Double value = Double.valueOf(getSliderValue());

      if (!this.slider.getValueIsAdjusting()) {

        if (value < this.minValue || value > this.maxValue) {
          // invalid value :
          SwingUtilities.invokeLater(new Runnable() {

            public void run() {
              setSliderValue(defValue);
            }
          });
          return;
        }

        if (logger.isLoggable(Level.FINEST)) {
          logger.finest("slider changed : " + value);
        }

        // done adjusting
        try {
          this.isEventHandling = true;

          this.field.setValue(value);
        } finally {
          this.isEventHandling = false;
        }

        this.fireStateChanged();

      } else {
        try {
          //value is adjusting; just set the text
          this.field.setText(this.field.getFormatter().valueToString(value));
        } catch (ParseException pe) {
          logger.log(Level.SEVERE, "parsing exception", pe);
        }
      }
    }
  }

  /** 
   * Handle the propertyChange event from the formatted text field
   */
  public void propertyChange(final PropertyChangeEvent evt) {
    if (!this.isEventHandling) {
      final double value = ((Number) this.field.getValue()).doubleValue();

      if (value < this.minValue || value > this.maxValue) {
        // invalid value :
        this.field.setValue(this.defValue);
        return;
      }

      if (logger.isLoggable(Level.FINEST)) {
        logger.finest("field changed : " + value);
      }
      try {
        this.isEventHandling = true;

        setSliderValue(value);
      } finally {
        this.isEventHandling = false;
      }

      this.fireStateChanged();
    }
  }

  /**
   * Adds a ChangeListener to this instance.
   *
   * @param l the ChangeListener to add
   * @see #fireStateChanged
   * @see #removeChangeListener
   */
  public void addChangeListener(ChangeListener l) {
    this.listenerList.add(ChangeListener.class, l);
  }

  /**
   * Removes a ChangeListener from this instance.
   *
   * @param l the ChangeListener to remove
   * @see #fireStateChanged
   * @see #addChangeListener

   */
  public void removeChangeListener(ChangeListener l) {
    this.listenerList.remove(ChangeListener.class, l);
  }

  /**
   * Send a {@code ChangeEvent}, whose source is this {@code JSlider}, to
   * all {@code ChangeListener}s that have registered interest in
   * {@code ChangeEvent}s.
   * This method is called each time a {@code ChangeEvent} is received from
   * the model.
   * <p>
   * The event instance is created if necessary, and stored in
   * {@code changeEvent}.
   *
   * @see #addChangeListener
   * @see EventListenerList
   */
  protected void fireStateChanged() {
    final Object[] listeners = this.listenerList.getListenerList();
    for (int i = listeners.length - 2; i >= 0; i -= 2) {
      if (listeners[i] == ChangeListener.class) {
        if (changeEvent == null) {
          changeEvent = new ChangeEvent(this);
        }
        ((ChangeListener) listeners[i + 1]).stateChanged(changeEvent);
      }
    }
  }

  /**
   * Return the public double value
   * @return double value
   */
  public double getValue() {
    return ((Number) this.field.getValue()).doubleValue();
  }

  /**
   * Set the public double value
   * @param double value
   */
  public void setValue(final double value) {
    this.field.setValue(value);
  }

  public double getMaxValue() {
    return maxValue;
  }

  public void setMaxValue(double maxValue) {
    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("maxValue changed : " + maxValue);
    }
    this.maxValue = maxValue;
  }

  public double getMinValue() {
    return minValue;
  }

  public void setMinValue(double minValue) {
    if (logger.isLoggable(Level.FINEST)) {
      logger.finest("minValue changed : " + minValue);
    }
    this.minValue = minValue;
  }

  public double getDefValue() {
    return defValue;
  }

  public void setDefValue(double defValue) {
    this.defValue = defValue;
  }

  /**
   * Return the slider value as a double value
   * @return double value
   */
  private double getSliderValue() {
    return this.minLimit + this.sliderRatio * this.slider.getValue();
  }

  /**
   * Change the slider value according to the given double value
   * @param value value to set
   */
  private void setSliderValue(final double value) {
    final int val = (int) Math.round((value - this.minLimit) / this.sliderRatio);
    this.slider.setValue(val);
  }
}
