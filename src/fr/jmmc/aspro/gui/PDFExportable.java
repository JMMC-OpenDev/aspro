/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: PDFExportable.java,v 1.3 2010-12-07 17:34:21 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2010/10/15 16:59:43  bourgesl
 * new PDF options (page size and orientation)
 * PDFExportable refactoring to include prepareChart, postPDF and getPDFOptions methods
 *
 * Revision 1.1  2010/06/09 12:51:09  bourgesl
 * new interface PDFExportable to define a standard method performPDFAction() that use ExportPDFAction to export the chart to PDF
 *
 */
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.gui.chart.PDFOptions;
import org.jfree.chart.JFreeChart;

/**
 * This interface defines a simple method to export a chart as a PDF document
 * @author bourgesl
 */
public interface PDFExportable {

  /**
   * Export the chart component as a PDF document.
   */
  public void performPDFAction();

  /**
   * Return the PDF options
   * @return PDF options
   */
  public PDFOptions getPDFOptions();

  /**
   * Return the chart to export as a PDF document
   * @return chart
   */
  public JFreeChart prepareChart();

  /**
   * Callback indicating the chart was processed by the PDF engine
   */
  public void postPDFExport();

}
