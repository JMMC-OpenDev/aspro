/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ExportPDFAction.java,v 1.16 2010-10-15 16:59:43 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.15  2010/10/05 15:06:00  bourgesl
 * use AsproGui:getInstance()
 *
 * Revision 1.14  2010/10/01 15:33:39  bourgesl
 * use MessagePane.showConfirmFileOverwrite
 *
 * Revision 1.13  2010/09/26 11:59:11  bourgesl
 * replaced RuntimeException by IllegalStateException to avoid catching runtime exceptions
 *
 * Revision 1.12  2010/09/24 15:54:25  bourgesl
 * better exception handling + use MessagePane
 *
 * Revision 1.11  2010/09/01 16:24:30  bourgesl
 * removed exception
 *
 * Revision 1.10  2010/09/01 12:57:14  bourgesl
 * added runtime exception message to user message dialog
 *
 * Revision 1.9  2010/06/17 10:02:50  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.8  2010/06/10 13:43:42  bourgesl
 * fixed file description with extension
 *
 * Revision 1.7  2010/06/09 12:57:15  bourgesl
 * now implements RegisteredAction (menu File) to export the chart of the current tab settingPanel.getTabSelectedComponent()
 * use the PDFExportable interface to let the panel call back this action with the chart instance
 * javadoc
 *
 * Revision 1.6  2010/05/26 15:29:14  bourgesl
 * light refactoring and javadoc
 *
 * Revision 1.5  2010/05/11 12:03:17  bourgesl
 * fix : check the file extension before the existence of the file to display the confirm dialog
 *
 * Revision 1.4  2010/04/13 15:54:48  bourgesl
 * javadoc
 *
 * Revision 1.3  2010/04/02 10:07:11  bourgesl
 * refactoring
 *
 * Revision 1.2  2010/01/14 17:02:38  bourgesl
 * FileFilter fix
 *
 * Revision 1.1  2010/01/13 16:11:43  bourgesl
 * pdf related classes
 *
 */
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.AsproGui;
import fr.jmmc.aspro.gui.PDFExportable;
import fr.jmmc.aspro.gui.chart.PDFUtils;
import fr.jmmc.aspro.util.FileUtils;
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.util.ActionRegistrar;
import fr.jmmc.mcs.util.FileFilterRepository;
import fr.jmmc.mcs.util.RegisteredAction;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;
import org.jfree.chart.JFreeChart;

/**
 * This registered action represents a File Menu entry to export any chart as a PDF document.
 *
 * @author bourgesl
 */
public final class ExportPDFAction extends RegisteredAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  private final static String className = "fr.jmmc.aspro.gui.action.ExportPDFAction";
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "exportPDF";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);
  /** PDF mime type */
  public static final String PDF_MIME_TYPE = "application/pdf";
  /** PDF extension */
  public static final String PDF_EXT = "pdf";

  /**
   * Return the singleton ExportPDFAction instance
   * @return ExportPDFAction instance
   */
  private static ExportPDFAction getInstance() {
    return (ExportPDFAction) ActionRegistrar.getInstance().get(className, actionName);
  }

  /**
   * Export the given exportable chart as a PDF document
   *
   * TODO : add rendering options (output format, orientation ?)
   *
   * @param exportable component
   */
  public static void exportPDF(final PDFExportable exportable) {
    getInstance().process(exportable);
  }

  /* members */
  /** last directory used to save a file; by default = user home */
  private String lastDir = System.getProperty("user.home");

  /**
   * Public constructor that automatically register the action in RegisteredAction.
   */
  public ExportPDFAction() {
    super(className, actionName);

    FileFilterRepository.getInstance().put(PDF_MIME_TYPE, PDF_EXT, "Portable Document Format (" + PDF_EXT + ")");
  }

  /**
   * Handle the action event
   * @param evt action event
   */
  public void actionPerformed(final ActionEvent evt) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("actionPerformed");
    }

    final Component selectedPanel = AsproGui.getInstance().getSettingPanel().getTabSelectedComponent();

    // be sure the selected panel implements PDFExportable (not null of course) :
    if (selectedPanel instanceof PDFExportable) {
      ((PDFExportable) selectedPanel).performPDFAction();
    }
  }

  /**
   * Export the given chart as a PDF document
   * @param exportable exportable component
   */
  public void process(final PDFExportable exportable) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("process");
    }

    File file = null;

    final JFileChooser fileChooser = new JFileChooser();
    fileChooser.setFileFilter(getFileFilter());

    if (this.getLastDir() != null) {
      fileChooser.setCurrentDirectory(new File(this.getLastDir()));
    }

    fileChooser.setDialogTitle("Export the plot to PDF");

    final int returnVal = fileChooser.showSaveDialog(null);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      file = checkFileExtension(fileChooser.getSelectedFile());

      if (file.exists()) {
        if (!MessagePane.showConfirmFileOverwrite(file.getName())) {
          file = null;
        }
      }
    } else {
      file = null;
    }

    // If a file was defined (No cancel in the dialog)
    if (file != null) {
      this.setLastDir(file.getParent());

      // prepare Chart :
      final JFreeChart chart = exportable.prepareChart();
      try {
        PDFUtils.saveChartAsPDF(file, chart, exportable.getPDFOptions());

        StatusBar.show(file.getName() + " created.");

      } catch (IOException ioe) {
        MessagePane.showErrorMessage(
                "Could not write to file : " + file.getName(), ioe);
      } finally {
        // restore Chart state if modified :
        exportable.postPDFExport();
      }
    }
  }

  /**
   * Return the file filter
   * @return file filter
   */
  protected FileFilter getFileFilter() {
    return FileFilterRepository.getInstance().get(PDF_MIME_TYPE);
  }

  /**
   * Check if the given file has the correct extension. If not, return a new file with it
   * @param file file to check
   * @return given file or new file with the correct extension
   */
  protected File checkFileExtension(final File file) {
    final String ext = FileUtils.getExtension(file);

    if (!PDF_EXT.equals(ext)) {
      return new File(file.getParentFile(), file.getName() + "." + PDF_EXT);
    }
    return file;
  }

  /**
   * Return the last directory used
   * @return last directory used
   */
  protected String getLastDir() {
    return this.lastDir;
  }

  /**
   * Define the last directory used
   * @param lastDir new value
   */
  protected void setLastDir(String lastDir) {
    this.lastDir = lastDir;
  }
}
