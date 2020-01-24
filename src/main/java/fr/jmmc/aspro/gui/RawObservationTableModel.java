/** *****************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ***************************************************************************** */
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.model.rawobs.RawObservation;
import java.util.List;
import javax.swing.table.AbstractTableModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is a specific table model (JTable) to display raw observations
 *
 * @author Laurent BOURGES
 */
public final class RawObservationTableModel extends AbstractTableModel {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(RawObservationTableModel.class.getName());

    /** Column definition enum */
    public enum ColumnDef {
        ID("Id", String.class),
        TYPE("Type", String.class),
        PARENT_ID("Parent Id", String.class),
        PROGRAM_ID("Program", String.class),
        INTERF_NAME("Array", String.class),
        INTERF_VERSION("Period", String.class),
        STATIONS("Stations", String.class),
        POPS("PoPs", String.class),
        INS_NAME("Ins. Name", String.class),
        INS_MODE("Ins. Mode", String.class),
        INS_SUB_MODE("Ins. SubMode", String.class),
        TARGET_NAME("Target", String.class),
        TARGET_RA("RA", Double.class),
        TARGET_DEC("DEC", Double.class),
        MJD_OBS("MJD OBS", Double.class),
        EXP_TIME("Exp. time", Double.class);

        /**
         * Custom constructor
         *
         * @param name name of the column
         * @param type class type of the column value
         * @param editable flag to indicate if column values are editable
         */
        private ColumnDef(final String name, final Class<?> type) {
            this.name = name;
            this.type = type;
        }
        /** column name */
        private final String name;
        /** class type */
        private final Class<?> type;

        /**
         * Return the name of the column
         *
         * @return name of the column
         */
        public String getName() {
            return name;
        }

        /**
         * Return the class type of the column value
         *
         * @return class type of the column value
         */
        public Class<?> getType() {
            return type;
        }

        /**
         * Return the name of the column
         *
         * @return name of the column
         */
        @Override
        public String toString() {
            return name;
        }
    }
    /** LITpro Columns (all) */
    private static final ColumnDef[] COLUMNS = ColumnDef.values();

    /* members */
    /** list of parameters (row) present in the table */
    private List<RawObservation> observations = null;

    /**
     * Public constructor
     */
    public RawObservationTableModel() {
        super();
    }

    /**
     * Define the data to use in this table model for a single target model
     *
     * @param observations
     */
    public void setData(final List<RawObservation> observations) {
        if (logger.isDebugEnabled()) {
            logger.debug("setData[{}]: {}", observations);
        }
        this.observations = observations;

        // fire the table data changed event :
        fireTableDataChanged();
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
    @Override
    public int getColumnCount() {
        return COLUMNS.length;
    }

    /**
     * Returns the name of the column at
     * <code>columnIndex</code>. This is used
     * to initialize the table's column header name. Note: this name does
     * not need to be unique; two columns in a table can have the same name.
     *
     * @param	columnIndex	the index of the column
     * @return the name of the column
     */
    @Override
    public String getColumnName(final int columnIndex) {
        return COLUMNS[columnIndex].getName();
    }

    /**
     * Returns the most specific superclass for all the cell values
     * in the column. This is used by the
     * <code>JTable</code> to set up a
     * default renderer and editor for the column.
     *
     * @param columnIndex the index of the column
     * @return the common ancestor class of the object values in the model.
     */
    @Override
    public Class<?> getColumnClass(final int columnIndex) {
        return COLUMNS[columnIndex].getType();
    }

    /**
     * Returns true if the cell at
     * <code>rowIndex</code> and
     * <code>columnIndex</code>
     * is editable. Otherwise,
     * <code>setValueAt</code> on the cell will not
     * change the value of that cell.
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
     * should display. This method should be quick, as it
     * is called frequently during rendering.
     *
     * @return the number of rows in the model
     * @see #getColumnCount
     */
    @Override
    public int getRowCount() {
        return this.observations.size();
    }

    /**
     * Returns the value for the cell at
     * <code>columnIndex</code> and
     * <code>rowIndex</code>.
     *
     * @param	rowIndex	the row whose value is to be queried
     * @param	columnIndex the column whose value is to be queried
     * @return	the value Object at the specified cell
     */
    @Override
    public Object getValueAt(final int rowIndex, final int columnIndex) {
        final RawObservation obs = getObsAt(rowIndex);

        if (obs != null) {
            switch (COLUMNS[columnIndex]) {
                case ID:
                    return obs.getObsId();
                case TYPE:
                    return obs.getType();
                case PARENT_ID:
                    return obs.getParentId();
                case PROGRAM_ID:
                    return obs.getProgramId();
                case INTERF_NAME:
                    return obs.getInterferometerName();
                case INTERF_VERSION:
                    return obs.getInterferometerVersion();
                case STATIONS:
                    return obs.getStations();
                case POPS:
                    return obs.getPops();
                case INS_NAME:
                    return obs.getInstrumentName();
                case INS_MODE:
                    return obs.getInstrumentMode();
                case INS_SUB_MODE:
                    return obs.getInstrumentSubMode();
                case TARGET_NAME:
                    return obs.getTargetName();
                case TARGET_RA:
                    return obs.getTargetRa();
                case TARGET_DEC:
                    return obs.getTargetDec();
                case MJD_OBS:
                    return obs.getMjdStart();
                case EXP_TIME:
                    return obs.getExpTime();
                default:
            }
        }

        return null;
    }

    /**
     * Sets the value in the cell at
     * <code>columnIndex</code> and
     * <code>rowIndex</code> to
     * <code>aValue</code>.
     *
     * @param	aValue	the new value
     * @param	rowIndex	the row whose value is to be changed
     * @param	columnIndex the column whose value is to be changed
     * @see #getValueAt
     * @see #isCellEditable
     */
    @Override
    public void setValueAt(final Object aValue, final int rowIndex, final int columnIndex) {
        // no-op
    }

    /* custom */
    /**
     * Return the model corresponding to the row at
     * <code>rowIndex</code>
     *
     * @param	rowIndex	the row whose value is to be queried
     * @return model
     */
    public RawObservation getObsAt(final int rowIndex) {
        return this.observations.get(rowIndex);
    }

}
