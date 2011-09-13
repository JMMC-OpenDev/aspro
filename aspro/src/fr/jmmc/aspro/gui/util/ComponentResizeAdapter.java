/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;


/**
 * Component adapter to force a resizable component to have a minimal dimension
 *
 * @author bourgesl
 */
public class ComponentResizeAdapter extends ComponentAdapter {

  /** minimal dimension to respect */
  private final Dimension dim;

  /**
   * Constructor with a given minimal dimension
   * @param dim minimal dimension
   */
  public ComponentResizeAdapter(final Dimension dim) {
    this.dim = dim;
  }

  /**
   * Invoked when the component's size changes.
   * This overriden method checks that the new size is greater than the minimal dimension
   * @param e event to process
   */

  @Override
  public void componentResized(final ComponentEvent e) {

    final Component c = e.getComponent();
    final Dimension d = c.getSize();
    int w = d.width;
    if (w < dim.width) {
      w = dim.width;
    }
    int h = d.height;
    if (h < dim.height) {
      h = dim.height;
    }

    c.setSize(w, h);
  }
}
