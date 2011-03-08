/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: CalibratorInfoTableModel.java,v 1.1 2011-03-08 17:27:50 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 *
 */
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.oi.BaseValue;
import fr.jmmc.aspro.model.oi.CalibratorInformations;
import fr.jmmc.aspro.model.oi.NumberValue;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import javax.swing.table.AbstractTableModel;

/**
 * This class is a specific table model (JTable) to display only calibrator information
 * @author bourgesl
 */
public final class CalibratorInfoTableModel extends AbstractTableModel {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          CalibratorInfoTableModel.class.getName());
  /** double formatter for number values */
  protected final static NumberFormat df3 = new DecimalFormat("0.0##");

  /** Column definition enum */
  public enum ColumnDef {

    /** parameter or field name */
    NAME("Name", String.class, false),
    /** parameter or field value */
    VALUE("Value", String.class, true),
    /** optional unit */
    UNIT("Unit", String.class, false);

    /**
     * Custom constructor
     * @param name name of the column
     * @param type class type of the column value
     * @param editable flag to indicate if column values are editable
     */
    private ColumnDef(final String name, final Class<?> type, final boolean editable) {
      this.name = name;
      this.type = type;
      this.editable = editable;
    }
    /** column name */
    private final String name;
    /** class type */
    private final Class<?> type;
    /** editable flag */
    private final boolean editable;

    /**
     * Return the name of the column
     * @return name of the column
     */
    public String getName() {
      return name;
    }

    /**
     * Return the class type of the column value
     * @return class type of the column value
     */
    public Class<?> getType() {
      return type;
    }

    /**
     * Return the flag to indicate if column values are editable
     * @return flag to indicate if column values are editable
     */
    public boolean isEditable() {
      return editable;
    }

    /**
     * Return the name of the column
     * @return name of the column
     */
    @Override
    public String toString() {
      return name;
    }
  }
  /** Table Columns (all) */
  private static final ColumnDef[] COLUMNS = ColumnDef.values();

  /* members */
  /** column count */
  private final int columnCount;
  /** column definitions */
  private final ColumnDef[] columnDefs;
  /** list of values (row) present in the table */
  private final List<BaseValue> rowList = new ArrayList<BaseValue>();

  /**
   * Public constructor
   */
  public CalibratorInfoTableModel() {
    super();
    this.columnDefs = COLUMNS;
    this.columnCount = this.columnDefs.length;
  }

  /**
   * Define the data to use in this table model
   * @param calInfos calibrator informations
   */
  public void setData(final CalibratorInformations calInfos) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("setData[" + calInfos);
    }
    resetData();
    processData(calInfos);

    // fire the table data changed event :
    fireTableDataChanged();
  }

  /**
   * Reset the table data members
   */
  private void resetData() {
    this.rowList.clear();
  }

  /**
   * Fill the table data members using the given calibrator informations
   * @param calInfos calibrator informations
   */
  private void processData(final CalibratorInformations calInfos) {
    if (calInfos != null) {
      // First add parameters :
      addParameterValue(calInfos, CalibratorInformations.PARAMETER_SCL_GUI_VERSION);
      addParameterValue(calInfos, CalibratorInformations.PARAMETER_SCL_BASELINE_MAX);
      addParameterValue(calInfos, CalibratorInformations.PARAMETER_SCL_WAVE_LENGTH);
      addParameterValue(calInfos, CalibratorInformations.PARAMETER_SCL_BRIGHT_MODE);
      /*
      for (BaseValue value : calInfos.getParameters()) {
      this.rowList.add(value);
      }
       */

      // Secondly add fields :
      addFieldValue(calInfos, CalibratorInformations.FIELD_DISTANCE);
      addFieldValue(calInfos, CalibratorInformations.FIELD_UD_V);
      addFieldValue(calInfos, CalibratorInformations.FIELD_UD_R);
      addFieldValue(calInfos, CalibratorInformations.FIELD_UD_I);
      addFieldValue(calInfos, CalibratorInformations.FIELD_UD_J);
      addFieldValue(calInfos, CalibratorInformations.FIELD_UD_H);
      addFieldValue(calInfos, CalibratorInformations.FIELD_UD_K);
      addFieldValue(calInfos, CalibratorInformations.FIELD_UD_N);
      addFieldValue(calInfos, CalibratorInformations.FIELD_UD);
      addFieldValue(calInfos, CalibratorInformations.FIELD_LD);
      addFieldValue(calInfos, CalibratorInformations.FIELD_UDDK);
      addFieldValue(calInfos, CalibratorInformations.FIELD_DIA12);
      /*
      for (BaseValue value : calInfos.getFields()) {
      this.rowList.add(value);
      }
       */
    }
  }

  /**
   * Add the parameter value of the given name to table rows if not null
   * @param calInfos calibrator informations
   * @param name parameter name
   */
  private void addParameterValue(final CalibratorInformations calInfos, final String name) {
    final BaseValue value = calInfos.getParameter(name);
    if (value != null) {
      this.rowList.add(value);
    }
  }

  /**
   * Add the field value of the given name to table rows if not null
   * @param calInfos calibrator informations
   * @param name field name
   */
  private void addFieldValue(final CalibratorInformations calInfos, final String name) {
    final BaseValue value = calInfos.getField(name);
    if (value != null) {
      this.rowList.add(value);
    }
  }

  /**
   * Add a value to table rows if not null
   * @param value value to add
   */
  private void addValue(final BaseValue value) {
    if (value != null) {
      this.rowList.add(value);
    }
  }

  /* TableModel interface implementation */
  /**
   * Returns the number of columns in the model. A
   * <code>JTable</code> uses this method to determine how many columns it
   * should create and display by default.
   *
   * @return the number of columns in the model
   * @see #getRowCount
   */
  public int getColumnCount() {
    return this.columnCount;
  }

  /**
   * Returns the name of the column at <code>columnIndex</code>.  This is used
   * to initialize the table's column header name.  Note: this name does
   * not need to be unique; two columns in a table can have the same name.
   *
   * @param	columnIndex	the index of the column
   * @return  the name of the column
   */
  @Override
  public String getColumnName(final int columnIndex) {
    return this.columnDefs[columnIndex].getName();
  }

  /**
   * Returns the most specific superclass for all the cell values
   * in the column.  This is used by the <code>JTable</code> to set up a
   * default renderer and editor for the column.
   *
   * @param columnIndex  the index of the column
   * @return the common ancestor class of the object values in the model.
   */
  @Override
  public Class<?> getColumnClass(final int columnIndex) {
    return this.columnDefs[columnIndex].getType();
  }

  /**
   * Returns false
   *
   * @param	rowIndex	the row whose value to be queried
   * @param	columnIndex	the column whose value to be queried
   * @return	true if the cell is editable
   * @see #setValueAt
   */
  @Override
  public boolean isCellEditable(final int rowIndex, final int columnIndex) {
    return false;
  }

  /**
   * Returns the number of rows in the model. A
   * <code>JTable</code> uses this method to determine how many rows it
   * should display.  This method should be quick, as it
   * is called frequently during rendering.
   *
   * @return the number of rows in the model
   * @see #getColumnCount
   */
  public int getRowCount() {
    return this.rowList.size();
  }

  /**
   * Returns the value for the cell at <code>columnIndex</code> and
   * <code>rowIndex</code>.
   *
   * @param	rowIndex	the row whose value is to be queried
   * @param	columnIndex 	the column whose value is to be queried
   * @return	the value Object at the specified cell
   */
  public Object getValueAt(final int rowIndex, final int columnIndex) {
    final BaseValue value = this.rowList.get(rowIndex);

    if (value != null) {
      switch (this.columnDefs[columnIndex]) {
        case NAME:
          return value.getName();
        case VALUE:
          if (value instanceof NumberValue) {
            return df3.format(value.getNumber());
          }
          return value.getValue();
        case UNIT:
          return value.getUnit();
        default:
      }
    }

    return null;
  }

  /**
   * Sets the value in the cell at <code>columnIndex</code> and
   * <code>rowIndex</code> to <code>aValue</code>.
   *
   * @param	aValue		 the new value
   * @param	rowIndex	 the row whose value is to be changed
   * @param	columnIndex 	 the column whose value is to be changed
   * @see #getValueAt
   * @see #isCellEditable
   */
  @Override
  public void setValueAt(final Object aValue, final int rowIndex, final int columnIndex) {
  }
}
