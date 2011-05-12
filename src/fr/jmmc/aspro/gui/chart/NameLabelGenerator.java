/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import java.io.Serializable;
import org.jfree.chart.labels.XYItemLabelGenerator;
import org.jfree.data.xy.XYDataset;

/**
 *
 * @author bourgesl
 */
public class NameLabelGenerator implements Cloneable, Serializable, XYItemLabelGenerator {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;

  /**
   * Public constructor
   */
  public NameLabelGenerator() {
    super();
  }

  /**
   * Generates a label for the specified item. The label is typically a
   * formatted version of the data value, but any text can be used.
   *
   * @param dataset  the dataset (<code>null</code> not permitted).
   * @param series  the series index (zero-based).
   * @param item  the item index (zero-based).
   *
   * @return The label (possibly <code>null</code>).
   */
  public String generateLabel(final XYDataset dataset, final int series, final int item) {
    if (dataset instanceof XYZNameDataSet) {
      final XYZNameDataSet nameDataSet = (XYZNameDataSet) dataset;
      return nameDataSet.getName(series, item);
    }
    return "";
  }

  /**
   * Tests this object for equality with an arbitrary object.
   *
   * @param obj  the other object (<code>null</code> permitted).
   *
   * @return A boolean.
   */
  @Override
  public boolean equals(Object obj) {
    if (obj == this) {
      return true;
    }
    if (!(obj instanceof NameLabelGenerator)) {
      return false;
    }
    return true;
  }

  /**
   * Returns a hash code for this instance.
   *
   * @return A hash code.
   */
  @Override
  public int hashCode() {
    return 127;
  }

  /**
   * Returns an independent copy of the generator.
   *
   * @return A clone.
   *
   * @throws CloneNotSupportedException if cloning is not supported.
   */
  @Override
  public Object clone() throws CloneNotSupportedException {
    return (NameLabelGenerator) super.clone();
  }
}
