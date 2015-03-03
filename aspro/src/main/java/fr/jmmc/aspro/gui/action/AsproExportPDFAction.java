/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.Aspro2;
import fr.jmmc.oiexplorer.core.gui.action.ExportPDFAction;
import java.awt.Component;

/**
 * This registered action represents a File Menu entry to export any chart as a PDF document.
 *
 * @author bourgesl
 */
public final class AsproExportPDFAction extends ExportPDFAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;

  /**
   * Public constructor that automatically register the action in RegisteredAction.
   */
  public AsproExportPDFAction() {
    super();
  }

  /**
   * Return the currently selected GUI panel (JPanel ...) to determine if it is possible to export it (PDFExportable)
   * @return selected GUI panel
   */
  protected Component getSelectedComponent() {
    return Aspro2.getInstance().getSettingPanel().getTabSelectedComponent();
  }
}
