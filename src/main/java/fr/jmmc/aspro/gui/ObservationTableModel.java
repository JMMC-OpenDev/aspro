/** *****************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ***************************************************************************** */
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.model.rawobs.ObservationType;
import fr.jmmc.aspro.model.util.AtmosphereQualityUtils;
import fr.jmmc.aspro.model.util.TargetObservation;
import fr.jmmc.aspro.model.util.TargetUtils;
import fr.jmmc.jmcs.model.ColumnDesc;
import fr.jmmc.jmcs.model.ColumnDescURLTableModel;
import java.util.Collections;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is a specific table model (JTable) to display observations
 *
 * @author Laurent BOURGES
 */
public final class ObservationTableModel extends ColumnDescURLTableModel {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(ObservationTableModel.class.getName());

    /** Column definition enum */
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

        private final ColumnDesc columnDesc;

        private ColumnDef(String label, Class<?> dataClass) {
            this.columnDesc = new ColumnDesc(name(), dataClass, ColumnDesc.SOURCE_UNDEFINED, label);
        }

        public ColumnDesc getColumnDesc() {
            return columnDesc;
        }
    }
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
        // define fixed columns:
        for (ColumnDef c : ColumnDef.values()) {
            listColumnDesc.add(c.getColumnDesc());
        }
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

    @Override
    public boolean hasURL(final int column) {
        if (column == ColumnDef.TARGET_NAME.ordinal()) {
            return true;
        }
        return false;
    }

    @Override
    public String getURL(final int column, final int row) {
        final String id = (String) getValueAt(row, column);
        if (id != null) {
            if (column == ColumnDef.TARGET_NAME.ordinal()) {
                return TargetUtils.getSimbadURL(id);
            }
        }
        return null;
    }

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

    /* TableModel interface implementation */
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

        final ObservationSetting obs = to.getObservation();
        final Target target = to.getTarget();
        final TargetUserInformations targetUserInfos = obs.getTargetUserInfos();

        final ColumnDesc columnDesc = getColumnDesc(columnIndex);

        switch (ColumnDef.valueOf(columnDesc.getName())) {
            case ID:
                return obs.getName();
            case TYPE:
                return (targetUserInfos != null && targetUserInfos.isCalibrator(target)) ? ObservationType.CALIBRATOR : ObservationType.SCIENCE;
            case PROGRAM_ID:
                return null; // TODO
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
        return null;
    }

}
