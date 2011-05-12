/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: XYZNameDataSet.java,v 1.2 2011-01-25 12:29:36 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/05/11 12:06:40  bourgesl
 * Overriden XYZDataSet to include a name property used by NameLabelGenerator
 *
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import java.util.List;
import org.jfree.data.xy.DefaultXYZDataset;

/**
 *
 * @author bourgesl
 */
public class XYZNameDataSet extends DefaultXYZDataset {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;

  /* members */
  /**
   * Storage for the name fo the series in the dataset.  We use a list because the
   * order of the series is significant.  This list must be kept in sync
   * with the seriesKeys list.
   */
  private List<String[]> seriesNamesList;

  /**
   * Creates a new <code>DefaultXYZDataset</code> instance, initially
   * containing no data.
   */
  public XYZNameDataSet() {
    super();
    this.seriesNamesList = new java.util.ArrayList<String[]>();
  }

  /**
   * Returns the name value for an item within a series.
   *
   * @param series  the series index (in the range <code>0</code> to
   *     <code>getSeriesCount() - 1</code>).
   * @param item  the item index (in the range <code>0</code> to
   *     <code>getItemCount(series)</code>).
   *
   * @return The name value.
   *
   * @throws ArrayIndexOutOfBoundsException if <code>series</code> is not
   *     within the specified range.
   */
  public String getName(final int series, final int item) {
    final String[] data = this.seriesNamesList.get(series);
    if (data != null && item < data.length) {
      return data[item];
    }
    return "AAAhhhh";
  }

  /**
   * Adds a series or if a series with the same key already exists replaces
   * the data for that series, then sends a {@link org.jfree.data.general.DatasetChangeEvent} to
   * all registered listeners.
   *
   * @param seriesKey  the series key (<code>null</code> not permitted).
   * @param data  the data (must be an array with length 3, containing three
   *     arrays of equal length, the first containing the x-values, the
   *     second containing the y-values and the third containing the
   *     z-values).
   * @param names the names array
   */
  public void addSeries(final Comparable<?> seriesKey, final double[][] data, final String[] names) {
    final int seriesIndex = indexOf(seriesKey);
    if (seriesIndex == -1) {
      // add a new series
      this.seriesNamesList.add(names);
    } else {
      // replace an existing series
      this.seriesNamesList.add(seriesIndex, names);
    }
    super.addSeries(seriesKey, data);
  }
}
