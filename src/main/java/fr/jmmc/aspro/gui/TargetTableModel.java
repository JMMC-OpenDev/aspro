/** *****************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ***************************************************************************** */
package fr.jmmc.aspro.gui;

import static fr.jmmc.aspro.model.OIBase.isEmpty;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetGroup;
import fr.jmmc.aspro.model.oi.TargetGroupMembers;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.model.rawobs.ObservationType;
import fr.jmmc.aspro.model.util.TargetUtils;
import fr.jmmc.jmal.model.targetmodel.Model;
import fr.jmmc.jmcs.model.ColumnDesc;
import fr.jmmc.jmcs.model.ColumnDescURLTableModel;
import fr.jmmc.jmcs.util.ColorEncoder;
import java.io.StringWriter;
import java.util.Collections;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is a specific table model (JTable) to display targets
 *
 * @author Laurent BOURGES
 */
public final class TargetTableModel extends ColumnDescURLTableModel {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(TargetTableModel.class.getName());

    /** Column definition enum */
    private enum ColumnDef {
        ID("Id", String.class),
        TYPE("Type", String.class),
        NAME("Target", String.class),
        // TargetUserInformations / TargetInformation:
        TARGET_GROUPS("Groups", String.class),
        // TargetUserInformations / TargetInformation:
        TARGET_NOTES("Notes", String.class),
        // coords (hms / dms)
        RA_HMS("RA (HMS)", String.class),
        DEC_DMS("DEC (DMS)", String.class),
        EQUINOX("Equinox", Double.class),
        // coords (deg):
        RA("RA", Double.class),
        DEC("DEC", Double.class),
        // velocity:
        SYSVEL("Sys. Vel.", Double.class),
        VELTYP("Vel. type", String.class),
        // proper motion:
        PMRA("PmRA", Double.class),
        PMDEC("PmDEC", Double.class),
        // parallax:
        PARALLAX("Parallax", Double.class),
        PARA_ERR("e_parallax", Double.class),
        // SIMBAD IDs:
        IDS("Ids", String.class),
        // SIMBAD OBJTYPE:
        OBJTYP("ObjType", String.class),
        // SIMBAD SPTYPE:
        SPECTYP("SpType", String.class),
        // Fluxes:
        FLUX_B("B", Double.class),
        FLUX_V("V", Double.class),
        FLUX_G("G", Double.class),
        FLUX_R("R", Double.class),
        FLUX_I("I", Double.class),
        FLUX_J("J", Double.class),
        FLUX_H("H", Double.class),
        FLUX_K("K", Double.class),
        FLUX_L("L", Double.class),
        FLUX_M("M", Double.class),
        FLUX_N("N", Double.class),
        // Models:
        MODEL_ANALYTIC("Analytic", Boolean.class),
        MODEL_CONTENT("Model", String.class),
        USER_MODEL_NAME("User Model", String.class),
        USER_MODEL_READY("User Model Ready", Boolean.class),
        // TargetConfiguration:
        CONF_HA_MIN("HA Min", Double.class),
        CONF_HA_MAX("HA Max", Double.class), /* related to obs setup (out of scope of this table):
            AO_SETUP("AO. Setup", String.class),
            FT_MODE("FT. Mode", String.class),
            WL_REF("WL_REF", Double.class)
        
            TargetUserInformations / TargetInformation (related calibrators)
         */ /* CalibratorInformations calibratorInfos */;
        /* members */
        private final ColumnDesc columnDesc;

        private ColumnDef(String label, Class<?> dataClass) {
            this.columnDesc = new ColumnDesc(name(), dataClass, ColumnDesc.SOURCE_UNDEFINED, label);
        }

        public ColumnDesc getColumnDesc() {
            return columnDesc;
        }
    }
    /** empty collection */
    private static final List<Target> EMPTY = Collections.emptyList();

    /* members */
    /** list of target (row) present in the table */
    private List<Target> _targetList = EMPTY;
    /** associated target user information */
    private TargetUserInformations _targetUserInfos = null;

    /**
     * Public constructor
     */
    public TargetTableModel() {
        super();
        // define fixed columns:
        for (ColumnDef c : ColumnDef.values()) {
            listColumnDesc.add(c.getColumnDesc());
        }
    }

    /**
     * Define the data to use in this table model
     *
     * @param targets
     */
    public void setData(final List<Target> targets, final TargetUserInformations targetUserInfos) {
        if (logger.isDebugEnabled()) {
            logger.debug("setData: {}", targets);
        }
        this._targetList = (targets != null) ? targets : EMPTY;
        this._targetUserInfos = targetUserInfos;

        // fire the table data changed event :
        fireTableDataChanged();
    }

    @Override
    public boolean hasURL(final int column) {
        if (column == ColumnDef.NAME.ordinal()) {
            return true;
        }
        return false;
    }

    @Override
    public String getURL(final int column, final int row) {
        final String id = (String) getValueAt(row, column);
        if (id != null) {
            if (column == ColumnDef.NAME.ordinal()) {
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
    public Target getTargetAt(final int rowIndex) {
        return this._targetList.get(rowIndex);
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
        return this._targetList.size();
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
        final Target target = getTargetAt(rowIndex);
        final TargetUserInformations targetUserInfos = this._targetUserInfos;

        final ColumnDesc columnDesc = getColumnDesc(columnIndex);

        switch (ColumnDef.valueOf(columnDesc.getName())) {
            case ID:
                return target.getIdentifier();
            case TYPE:
                return ((targetUserInfos != null) && targetUserInfos.isCalibrator(target)) ? ObservationType.CALIBRATOR : ObservationType.SCIENCE;
            case NAME:
                return target.getName();
            // coords (hms / dms)
            case RA_HMS:
                return target.getRA();
            case DEC_DMS:
                return target.getDEC();
            // coords (deg)
            case RA:
                return target.getRADeg();
            case DEC:
                return target.getDECDeg();
            // velocity:
            case SYSVEL:
                return target.getSYSVEL();
            case VELTYP:
                return target.getVELTYP();
            // proper motion:
            case PMRA:
                return target.getPMRA();
            case PMDEC:
                return target.getPMDEC();
            // parallax:
            case PARALLAX:
                return target.getPARALLAX();
            case PARA_ERR:
                return target.getPARAERR();
            // SIMBAD IDs:
            case IDS:
                return target.getIDS();
            // SIMBAD OBJTYPE:
            case OBJTYP:
                return target.getOBJTYP();
            // SIMBAD SPTYPE:
            case SPECTYP:
                return target.getSPECTYP();
            // Fluxes:
            case FLUX_B:
                return target.getFLUXB();
            case FLUX_V:
                return target.getFLUXV();
            case FLUX_G:
                return target.getFLUXG();
            case FLUX_R:
                return target.getFLUXR();
            case FLUX_I:
                return target.getFLUXI();
            case FLUX_J:
                return target.getFLUXJ();
            case FLUX_H:
                return target.getFLUXH();
            case FLUX_K:
                return target.getFLUXK();
            case FLUX_L:
                return target.getFLUXL();
            case FLUX_M:
                return target.getFLUXM();
            case FLUX_N:
                return target.getFLUXN();
            // Models:
            case MODEL_ANALYTIC:
                return target.hasAnalyticalModel() && !isEmpty(target.getModels());
            case MODEL_CONTENT:
                return (target.hasAnalyticalModel()) ? getModelContent(target.getModels()) : null;
            case USER_MODEL_NAME:
                return (target.getUserModel() != null) ? target.getUserModel().getName() : null;
            case USER_MODEL_READY:
                return (target.getUserModel() != null) ? target.getUserModel().isModelDataReady() : null;
            // TargetConfiguration:
            case CONF_HA_MIN:
                return (target.getConfiguration() != null) ? target.getConfiguration().getHAMin() : null;
            case CONF_HA_MAX:
                return (target.getConfiguration() != null) ? target.getConfiguration().getHAMax() : null;
            /* related to obs setup (out of scope of this table):
            case AO_SETUP:
                return (target.getConfiguration() != null) ? target.getConfiguration().getAoSetup(): null;
            case FT_MODE:
                return (target.getConfiguration() != null) ? target.getConfiguration().getFringeTrackerMode(): null;
            case WL_REF:
                return (target.getConfiguration() != null) ? target.getConfiguration().getInstrumentWaveLengthRef(): null;
             */
            // TargetUserInformations / TargetInformation:
            case TARGET_GROUPS:
                return (targetUserInfos != null) ? getGroups(targetUserInfos, target) : null;
            case TARGET_NOTES:
                return (targetUserInfos != null) ? targetUserInfos.getDescription(target) : null;
            default:
        }
        return null;
    }

    /** Create a 4K buffer for models */
    private final static StringWriter SW = new StringWriter(4096); // 4K buffer

    private static String getModelContent(final List<Model> models) {
        if (!isEmpty(models)) {
            // reset:
            SW.getBuffer().setLength(0);

            final Model targetModel = new Model();
            targetModel.setNameAndType("Container");

            for (Model model : models) {
                targetModel.getModels().add(model);
            }

            // serialize models to xml :
            ObservationManager.getInstance().saveObject(SW, targetModel);

            return SW.toString();
        }
        return null;
    }

    private static String getGroups(final TargetUserInformations targetUserInfos, final Target target) {
        // reset:
        final StringBuffer sb = SW.getBuffer();
        sb.setLength(0);

        boolean startGroups = true;

        for (TargetGroup group : targetUserInfos.getDisplayGroups()) {
            TargetGroupMembers gm = targetUserInfos.getGroupMembers(group);

            if (gm != null && gm.hasTarget(target)) {
                if (startGroups) {
                    startGroups = false;
                    sb.append("<html>");
                }
                sb.append("&nbsp;<span style=\"color:")
                        .append(ColorEncoder.encode(group.getOverDecodedColor()))
                        .append(";background:")
                        .append(ColorEncoder.encode(group.getDecodedColor()))
                        .append("\">&nbsp;")
                        .append(group.getName())
                        .append("&nbsp;</span>");
            }
        }
        return (startGroups) ? null : sb.append("</html>").toString();
    }
}
