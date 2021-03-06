/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.Aspro2;
import fr.jmmc.jmcs.data.MimeType;
import fr.jmmc.oiexplorer.core.export.DocumentExportable;
import fr.jmmc.oiexplorer.core.gui.action.ExportDocumentAction;

/**
 * This registered action represents a File Menu entry to export the selected view as a document.
 *
 * @author bourgesl
 */
public final class AsproExportAction extends ExportDocumentAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     * @param mimeType exported document's MimeType
     */
    public AsproExportAction(final MimeType mimeType) {
        super(mimeType);
    }

    /**
     * Return the currently selected GUI panel (JPanel ...) to determine if it is possible to export it (PDFExportable)
     * @return selected GUI panel
     */
    @Override
    protected DocumentExportable getSelectedComponent() {
        return Aspro2.getInstance().getSettingPanel().getExportableSelectedComponent();
    }
}
