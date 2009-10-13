/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.jmmc.aspro.gui.chart;

import java.text.DecimalFormat;
import org.jfree.chart.axis.NumberTickUnit;
import org.jfree.chart.axis.TickUnitSource;
import org.jfree.chart.axis.TickUnits;

/**
 *
 * @author bourgesl
 */
public class ChartUtils {

  /**
   * Returns a collection of tick units for integer values.
   *
   * @return A collection of tick units for integer values.
   *
   * @see #setStandardTickUnits(TickUnitSource)
   * @see #createStandardTickUnits()
   */
  public static TickUnitSource createScientificTickUnits() {
    TickUnits units = new TickUnits();
    DecimalFormat df0 = new DecimalFormat("0");
    DecimalFormat df1 = new DecimalFormat("0.00E00");

    units.add(new NumberTickUnit(1, df0, 2));
    units.add(new NumberTickUnit(5, df0, 5));
    units.add(new NumberTickUnit(10, df0, 2));
    units.add(new NumberTickUnit(50, df0, 5));
    units.add(new NumberTickUnit(100, df0, 2));
    units.add(new NumberTickUnit(500, df0, 5));

    units.add(new NumberTickUnit(1000, df1, 2));
    units.add(new NumberTickUnit(5000, df1, 5));
    units.add(new NumberTickUnit(10000, df1, 2));
    units.add(new NumberTickUnit(50000, df1, 5));
    units.add(new NumberTickUnit(100000, df1, 2));
    units.add(new NumberTickUnit(500000, df1, 5));
    units.add(new NumberTickUnit(1000000, df1, 2));
    units.add(new NumberTickUnit(5000000, df1, 5));
    units.add(new NumberTickUnit(10000000, df1, 2));
    units.add(new NumberTickUnit(50000000, df1, 5));
    units.add(new NumberTickUnit(100000000, df1, 2));
    units.add(new NumberTickUnit(500000000, df1, 5));
    units.add(new NumberTickUnit(1000000000, df1, 2));
    units.add(new NumberTickUnit(5000000000.0, df1, 5));
    units.add(new NumberTickUnit(10000000000.0, df1, 2));
    return units;
  }
}
