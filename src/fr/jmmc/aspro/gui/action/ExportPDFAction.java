/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ExportPDFAction.java,v 1.3 2010-04-02 10:07:11 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2010/01/14 17:02:38  bourgesl
 * FileFilter fix
 *
 * Revision 1.1  2010/01/13 16:11:43  bourgesl
 * pdf related classes
 *
 */
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.gui.chart.PDFUtils;
import fr.jmmc.aspro.util.FileUtils;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.util.FileFilterRepository;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.logging.Level;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileFilter;
import org.jfree.chart.JFreeChart;

/**
 * This class implement the export to PDF action.
 *
 * TODO : use the standard MCS action to add it in the menu.
 *
 * @author bourgesl
 */
public class ExportPDFAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  public final static String className = "fr.jmmc.aspro.gui.action.ExportPDFAction";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);
  /** PDF mime type */
  public static final String PDF_MIME_TYPE = "application/pdf";
  /** PDF extension */
  public static final String PDF_EXT = "pdf";
  /** action singleton */
  private static ExportPDFAction instance = new ExportPDFAction();

  /**
   * Return the singleton ExportPDFAction instance
   * @return ExportPDFAction instance
   */
  public static ExportPDFAction getInstance() {
    return instance;
  }

  /* members */
  /** last directory used to save a file; by default = user home */
  private String lastDir = System.getProperty("user.home");

  /**
   * Forbidden Constructor
   */
  private ExportPDFAction() {
    super();

    FileFilterRepository.getInstance().put(PDF_MIME_TYPE, PDF_EXT, "Portable Document Format (PDF)");
  }

  /**
   * Execute the action.
   *
   * Note : the action event's source must be the chart to export
   *
   * @param e action event
   */
  public void actionPerformed(final ActionEvent event) {
    if (!(event.getSource() instanceof JFreeChart)) {
      return;
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("actionPerformed");
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
      file = fileChooser.getSelectedFile();
      if (file.exists()) {
        final int answer = JOptionPane.showConfirmDialog(null, "File \'" + file.getName() + "\' already exists\nDo you want to overwrite this file ?");
        if (answer != JOptionPane.YES_OPTION) {
          file = null;
        }
      }
    } else {
      file = null;
    }

    // If a file was defined (No cancel in the dialog)
    if (file != null) {
      this.setLastDir(file.getParent());

      try {
        file = checkFileExtension(file);

        PDFUtils.saveChartAsPDF(file, (JFreeChart) event.getSource());

        StatusBar.show(file.getName() + " created.");

      } catch (RuntimeException re) {
        logger.log(Level.SEVERE, "runtime failure : ", re);

        JOptionPane.showMessageDialog(null,
                "Could not export to file " + file.getName(),
                "Error", JOptionPane.ERROR_MESSAGE);
      }
    }
  }

  protected FileFilter getFileFilter() {
    return FileFilterRepository.getInstance().get(PDF_MIME_TYPE);
  }

  protected File checkFileExtension(final File file) {
    final String ext = FileUtils.getExtension(file);

    if (!PDF_EXT.equals(ext)) {
      return new File(file.getParentFile(), file.getName() + "." + PDF_EXT);
    }
    return file;
  }

  public String getLastDir() {
    return lastDir;
  }

  public void setLastDir(String lastDir) {
    this.lastDir = lastDir;
  }
}
