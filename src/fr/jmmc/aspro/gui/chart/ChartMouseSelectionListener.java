/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import java.awt.geom.Rectangle2D;
import org.jfree.chart.ChartMouseListener;

/**
 * This class defines an mouse selection event listener
 * @author bourgesl
 */
public interface ChartMouseSelectionListener {

  /**
   * Handle rectangular selection event
   *
   * @param selection the selected region.
   */
  public void mouseSelected(final Rectangle2D selection);
}
