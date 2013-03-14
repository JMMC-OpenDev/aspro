/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.jmcs.gui.component.FileChooser;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.data.MimeType;
import fr.jmmc.jmcs.util.StringUtils;
import fr.jmmc.oiexplorer.core.gui.action.WaitingTaskAction;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIFitsWriter;
import fr.jmmc.oitools.model.OIVis2;
import fr.nom.tam.fits.FitsException;
import java.io.File;
import java.io.IOException;
import java.util.List;
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

        final List<OIFitsFile> oiFitsFiles = ObservationManager.getInstance().getOIFitsList();

        if (oiFitsFiles != null) {

            final boolean exportAll = oiFitsFiles.size() > 1;

            File file;

            if (exportAll) {
                file = FileChooser.showDirectoryChooser("Export observations as OIFits files", null, mimeType);
            } else {
                final OIFitsFile oiFitsFile = oiFitsFiles.get(0);

                file = FileChooser.showSaveFileChooser("Export this observation as an OIFits file", null, mimeType, getDefaultFileName(oiFitsFile));
            }

            logger.debug("Selected file: {}", file);

            // If a file was defined (No cancel in the dialog)
            if (file != null) {
                final String directory = (exportAll) ? file.getPath() : file.getParent();

                try {

                    if (exportAll) {

                        for (OIFitsFile oiFitsFile : oiFitsFiles) {

                            file = new File(directory, getDefaultFileName(oiFitsFile));

                            OIFitsWriter.writeOIFits(file.getAbsolutePath(), oiFitsFile);

                            StatusBar.show(file.getName() + " created.");
                        }

                        StatusBar.show("OIFits files saved in " + directory + ".");

                    } else {
                        final OIFitsFile oiFitsFile = oiFitsFiles.get(0);

                        OIFitsWriter.writeOIFits(file.getAbsolutePath(), oiFitsFile);

                        StatusBar.show(file.getName() + " created.");
                    }

                } catch (FitsException fe) {
                    MessagePane.showErrorMessage("Could not export to file : " + file.getAbsolutePath(), fe);
                } catch (IOException ioe) {
                    MessagePane.showErrorMessage("Could not export to file : " + file.getAbsolutePath(), ioe);
                }
            }
        } else {
            MessagePane.showMessage("There is currently no OIFits data (your target is not observable)");
        }
    }

    /**
     * Generate a default name for the given OIFits structure
     * @param oiFitsFile OIFits structure
     * @return default name [Aspro2_<TARGET>_<INSTRUMENT>_<CONFIGURATION>_<DATE>]
     */
    public static String getDefaultFileName(final OIFitsFile oiFitsFile) {

        final StringBuilder sb = new StringBuilder(32).append("Aspro2_");

        final String altName = StringUtils.replaceNonAlphaNumericCharsByUnderscore(oiFitsFile.getOiTarget().getTarget()[0]);

        sb.append(altName).append('_');

        final OIVis2 vis2 = oiFitsFile.getOiVis2()[0];

        final String insName = StringUtils.replaceNonAlphaNumericCharsByUnderscore(vis2.getInsName());

        sb.append(insName).append('_');

        // requires analysis:
        final short[] staConf = vis2.getDistinctStaConf().iterator().next();
        final String staConfName = vis2.getStaNames(staConf);

        sb.append(StringUtils.replaceWhiteSpaces(staConfName, "-"));

        sb.append('_');

        final String dateObs = vis2.getDateObs();

        sb.append(dateObs);

        sb.append('.').append(mimeType.getExtension());

        return sb.toString();
    }
}
