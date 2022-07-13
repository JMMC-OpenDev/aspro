/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.model.OIFitsData;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.jmcs.gui.component.FileChooser;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.data.MimeType;
import fr.jmmc.jmcs.util.StringUtils;
import fr.jmmc.oiexplorer.core.gui.action.WaitingTaskAction;
import fr.jmmc.oitools.model.OIDataListHelper;
import fr.jmmc.oitools.model.OIData;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIFitsWriter;
import fr.jmmc.oitools.processing.Merger;
import fr.nom.tam.fits.FitsException;
import java.io.File;
import java.io.IOException;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This registered action represents a File Menu entry to export an OIFits file
 * containing the visibilities of the selected target.
 * @author bourgesl
 */
public final class ExportOIFitsAction extends WaitingTaskAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    private final static String className = ExportOIFitsAction.class.getName();
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "exportOIFits";
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(className);
    /** OIFits MimeType */
    private final static MimeType mimeType = MimeType.OIFITS;
    /** fits extension including '.' (dot) character ie '.fits' */
    public final static String OIFITS_EXTENSION = "." + mimeType.getExtension();

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public ExportOIFitsAction() {
        super(className, actionName);
    }

    /**
     * Handle the action event
     */
    @Override
    public void actionPerformed() {
        logger.debug("actionPerformed");

        final OIFitsFile oiFitsFile = checkAndGetMergedOIFits();
        if (oiFitsFile == null) {
            return; // no data
        }

        final File file = FileChooser.showSaveFileChooser("Export this observation as an OIFits file", null, mimeType,
                getDefaultFileName(oiFitsFile));

        logger.debug("Selected file: {}", file);

        // If a file was defined (No cancel in the dialog)
        if (file != null) {
            try {
                OIFitsWriter.writeOIFits(file.getAbsolutePath(), oiFitsFile);

                StatusBar.show(file.getName() + " created.");
            } catch (FitsException fe) {
                MessagePane.showErrorMessage("Could not export to file : " + file.getAbsolutePath(), fe);
            } catch (IOException ioe) {
                MessagePane.showErrorMessage("Could not export to file : " + file.getAbsolutePath(), ioe);
            }
        }
    }

    /**
     * Return 1 OIFits file (merged if needed)
     * @return merged OIFits if multiple or original one
     */
    public static OIFitsFile checkAndGetMergedOIFits() {
        List<OIFitsFile> oiFitsFiles = ObservationManager.getInstance().checkAndGetOIFitsList();

        if (oiFitsFiles == null) {
            MessagePane.showMessage("There is currently no OIFits data (your target is not observable)");
            return null;
        } else if (oiFitsFiles == OIFitsData.IGNORE_OIFITS_LIST) {
            return null;
        }

        final OIFitsFile oiFitsFile;

        // Merge OIFITS files:
        if (oiFitsFiles.size() > 1) {
            // Use Merger to obtain 1 OIFits:
            oiFitsFile = Merger.process(oiFitsFiles.toArray(new OIFitsFile[oiFitsFiles.size()]));
            oiFitsFile.analyze();

            logger.debug("Merged OIFits: {}", oiFitsFile);
        } else {
            oiFitsFile = oiFitsFiles.get(0);
        }
        return oiFitsFile;
    }

    /**
     * Generate a default name for the given OIFits structure
     * @param oiFitsFile OIFits structure
     * @return default name [Aspro2_<TARGET>_<INSTRUMENT>_<CONFIGURATION>_<DATE>]
     */
    public static String getDefaultFileName(final OIFitsFile oiFitsFile) {
        return getDefaultFileName(oiFitsFile, true);
    }

    /**
     * Generate a default name for the given OIFits structure
     * @param oiFitsFile OIFits structure
     * @param addExtension true to add oifits extension into the returned file name; false otherwise
     * @return default name [Aspro2_<TARGET>_<INSTRUMENT>_<CONFIGURATION>_<DATE>]
     */
    public static String getDefaultFileName(final OIFitsFile oiFitsFile, final boolean addExtension) {

        // TODO: merge with PlotChartPanel.getDefaultFileName() and OIXP ExportOIFitsAction ...
        final Set<String> distinct = new LinkedHashSet<String>();

        final StringBuilder sb = new StringBuilder(128).append("Aspro2_");

        // Add target name:
        final String altName = StringUtils.replaceNonAlphaNumericCharsByUnderscore(oiFitsFile.getOiTarget().getTarget()[0]);
        sb.append(altName).append('_');

        final List<OIData> oidataList = oiFitsFile.getOiDataList();

        // Add distinct arrNames:
        distinct.clear();
        OIDataListHelper.getDistinct(oidataList, distinct, OIDataListHelper.GET_ARR_NAME);
        if (!distinct.isEmpty()) {
            OIDataListHelper.toString(distinct, sb, "_", "_", 3, "MULTI_ARRNAME");
        }
        sb.append('_');

        // Add unique insNames:
        distinct.clear();
        // fix values to strip suffix '_nn':
        OIDataListHelper.getDistinctNoSuffix(oidataList, distinct, OIDataListHelper.GET_INS_NAME);
        if (!distinct.isEmpty()) {
            OIDataListHelper.toString(distinct, sb, "_", "_", 3, "MULTI_INSNAME");
        }
        sb.append('_');

        // Add unique configurations:
        distinct.clear();
        distinct.addAll(OIDataListHelper.getDistinctStaConfs(oidataList)); // requires analysis
        if (!distinct.isEmpty()) {
            OIDataListHelper.toString(distinct, sb, "-", "_", 3, "MULTI_CONF");
        }
        sb.append('_');

        // Add unique dateObs:
        distinct.clear();
        OIDataListHelper.getDistinct(oidataList, distinct, OIDataListHelper.GET_DATE_OBS);
        if (!distinct.isEmpty()) {
            OIDataListHelper.toString(distinct, sb, "_", "_", 3, "MULTI_DATE");
        }

        if (addExtension) {
            sb.append(OIFITS_EXTENSION);
        }

        return sb.toString();
    }
}
