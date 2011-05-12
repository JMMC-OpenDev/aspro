/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.AsproGui;
import fr.jmmc.aspro.gui.PDFExportable;
import fr.jmmc.aspro.gui.chart.PDFUtils;
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.util.ActionRegistrar;
import fr.jmmc.mcs.util.FileFilterRepository;
import fr.jmmc.mcs.util.FileUtils;
import java.awt.Component;
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
public final class ExportPDFAction extends WaitingTaskAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  private final static String className = "fr.jmmc.aspro.gui.action.ExportPDFAction";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "exportPDF";
  /** PDF mime type */
  public static final String PDF_MIME_TYPE = "application/pdf";
  /** flag to avoid use StatusBar (JUnit) */
  private static boolean avoidUseStatusBar = false;

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
   * @param exportable component
   */
  public static void exportPDF(final PDFExportable exportable) {
    getInstance().process(exportable);
  }

  /**
   * Define the flag to avoid use StatusBar (JUnit)
   * @param flag true to avoid use StatusBar
   */
  public final static void setAvoidUseStatusBar(final boolean flag) {
    avoidUseStatusBar = flag;
  }
  /* members */
  /** last directory used to save a file; by default = user home */
  private String lastDir = System.getProperty("user.home");

  /**
   * Public constructor that automatically register the action in RegisteredAction.
   */
  public ExportPDFAction() {
    super(className, actionName);

    FileFilterRepository.getInstance().put(PDF_MIME_TYPE, PDFExportable.PDF_EXT, "Portable Document Format (" + PDFExportable.PDF_EXT + ")");
  }

  /**
   * Handle the action event
   */
  public final void actionPerformed() {
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

    // default PDF file name :
    final String fileName = exportable.getPDFDefaultFileName();
    if (fileName != null) {
      fileChooser.setSelectedFile(new File(fileChooser.getCurrentDirectory(), fileName));
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

        if (!avoidUseStatusBar) {
          StatusBar.show(file.getName() + " created.");
        }

      } catch (IOException ioe) {
        MessagePane.showErrorMessage(
                "Could not write to file : " + file.getAbsolutePath(), ioe);
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
    return FileFilterRepository.get(PDF_MIME_TYPE);
  }

  /**
   * Check if the given file has the correct extension. If not, return a new file with it
   * @param file file to check
   * @return given file or new file with the correct extension
   */
  protected File checkFileExtension(final File file) {
    final String ext = FileUtils.getExtension(file);

    if (!PDFExportable.PDF_EXT.equals(ext)) {
      return new File(file.getParentFile(), file.getName() + "." + PDFExportable.PDF_EXT);
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
