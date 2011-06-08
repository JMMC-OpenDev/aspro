/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.gui.chart.PDFOptions;
import fr.jmmc.mcs.util.MimeType;
import org.jfree.chart.JFreeChart;

/**
 * This interface defines a simple method to export a chart as a PDF document
 * @author bourgesl
 */
public interface PDFExportable {
  /** PDF extension */
  public static final String PDF_EXT = MimeType.PDF.getExtension();

  /**
   * Export the chart component as a PDF document.
   */
  public void performPDFAction();

  /**
   * Return the PDF default file name
   * @return PDF default file name
   */
  public String getPDFDefaultFileName();

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
