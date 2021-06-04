/** *****************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ***************************************************************************** */
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.model.rawobs.ObservationType;
import fr.jmmc.aspro.model.util.AtmosphereQualityUtils;
import java.util.Collections;
import java.util.List;
import javax.swing.table.AbstractTableModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is a specific table model (JTable) to display observations
 *
 * @author Laurent BOURGES
 */
public final class ObservationTableModel extends AbstractTableModel {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(ObservationTableModel.class.getName());

    /** Column definition enum */
    /*
    observation:
    // protected List<ObservationVariant> variants;    
    protected String name;
    
    protected InterferometerConfigurationChoice interferometerConfiguration {
        "name",
        "minElevation"
    }
    
    protected FocalInstrumentConfigurationChoice instrumentConfiguration {
        "name",
        "stations",
        "pops",
        "instrumentMode",
        "samplingPeriod",
        "acquisitionTime"
    }
    protected List<Target> targets;
    protected TargetUserInformations targetUserInfos {
        "calibrators",
        "groups",
        "groupMembers",
        "targetInfos"
    }
    
    protected WhenSetting when {
        "date",
        "nightRestriction",
        "atmosphereQuality",
        "windAzimuth"
    }
     */
    private enum ColumnDef {
        ID("Id", String.class),
        TYPE("Type", String.class),
        PROGRAM_ID("Program", String.class),
        // Interferometer:
        INTERF_NAME("Array", String.class),
        INTERF_VERSION("Period", String.class),
        STATIONS("Stations", String.class),
        POPS("PoPs", String.class),
        // Instrument mode:
        INS_NAME("Ins. Name", String.class),
        INS_MODE("Ins. Mode", String.class),
        // Target:
        TARGET_NAME("Target", String.class),
        // coords (deg)
        TARGET_RA("RA", Double.class),
        TARGET_DEC("DEC", Double.class),
        // coords (hms / dms)
        TARGET_RA_HMS("RA (HMS)", String.class),
        TARGET_DEC_DMS("DEC (DMS)", String.class),
        // when :
        MJD_START("MJD OBS", Double.class),
        // Atmosphere quality ?
        TAU0("Tau0 (ms)", Double.class),
        SEEING("Seeing (as)", Double.class),
        EXP_TIME("Exp. time", Double.class),
        // timestamps:
        DATE("Date (UTC)", String.class),
        TIME("Time (UTC)", String.class),
        LST_START("LST Start", Double.class);

        /**
         * Custom constructor
         *
         * @param name name of the column
         * @param type class type of the column value
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
    /** Definition of Columns (all) */
    private static final ColumnDef[] COLUMNS = ColumnDef.values();
    /** empty collection */
    private static final List<TargetObservation> EMPTY = Collections.emptyList();

    /* members */
    /** list of observations (row) present in the table */
    private List<TargetObservation> obsTargetList = EMPTY;

    /**
     * Public constructor
     */
    public ObservationTableModel() {
        super();
    }

    /**
     * Define the data to use in this table model
     *
     * @param obsTargetList
     */
    public void setData(final List<TargetObservation> obsTargetList) {
        if (logger.isDebugEnabled()) {
            logger.debug("setData[{}]: {}", obsTargetList);
        }
        this.obsTargetList = (obsTargetList != null) ? obsTargetList : EMPTY;

        // fire the table data changed event :
        fireTableDataChanged();
    }

    public boolean hasURL(final int column) {
        return false;
    }

    public String getURL(final int column, final int row) {
        final String id = (String) getValueAt(row, column);
        if (id != null) {
            // TODO: build url
        }
        return null;
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
        return hasURL(columnIndex);
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
        return this.obsTargetList.size();
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
        final TargetObservation to = getObsAt(rowIndex);

        if (to != null) {
            final ObservationSetting obs = to.observation;
            final Target target = to.target;
            final TargetUserInformations targetUserInfos = obs.getTargetUserInfos();

            switch (COLUMNS[columnIndex]) {
                case ID:
                    return obs.getName();
                case TYPE:
                    return (targetUserInfos != null && targetUserInfos.isCalibrator(target)) ? ObservationType.CALIBRATOR : ObservationType.SCIENCE;
                case PROGRAM_ID:
                    return obs.getDescription(); // TODO
                // Interferometer:
                case INTERF_NAME:
                    return obs.getInterferometerConfiguration().getInterferometerConfiguration().getInterferometer().getName();
                case INTERF_VERSION:
                    return obs.getInterferometerConfiguration().getInterferometerConfiguration().getVersion();
                case STATIONS:
                    return obs.getInstrumentConfiguration().getStations();
                case POPS:
                    return obs.getInstrumentConfiguration().getPops();
                // Instrument mode:
                case INS_NAME:
                    return obs.getInstrumentConfiguration().getName();
                case INS_MODE:
                    return obs.getInstrumentConfiguration().getInstrumentMode();
                // Target:
                case TARGET_NAME:
                    return target.getName();
                // coords (deg)
                case TARGET_RA:
                    return target.getRADeg();
                case TARGET_DEC:
                    return target.getDECDeg();
                // coords (hms / dms)
                case TARGET_RA_HMS:
                    return target.getRA();
                case TARGET_DEC_DMS:
                    return target.getDEC();
                // when :
                case MJD_START:
                    return null; // TODO to.getMjdStart();
                // Atmosphere quality ?
                case TAU0:
                    return AtmosphereQualityUtils.getCoherenceTime(obs.getWhen().getAtmosphereQuality()) * 0.001; // s
                case SEEING:
                    return AtmosphereQualityUtils.getSeeing(obs.getWhen().getAtmosphereQuality());
                case EXP_TIME:
                    return obs.getInstrumentConfiguration().getAcquisitionTime(); // s
                case DATE:
                    return obs.getWhen().getDate().toString();
                case TIME:
                    return null;
                case LST_START:
                    return null;
                default:
            }
        }
        return null;
    }

    /* custom */
    /**
     * Return the model corresponding to the row at
     * <code>rowIndex</code>
     *
     * @param	rowIndex	the row whose value is to be queried
     * @return model
     */
    public TargetObservation getObsAt(final int rowIndex) {
        return this.obsTargetList.get(rowIndex);
    }

    final static class TargetObservation {

        private final ObservationSetting observation;
        private final Target target;

        TargetObservation(final ObservationSetting observation, final Target target) {
            this.observation = observation;
            this.target = target;
        }
    }
}
