/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: PDFUtils.java,v 1.1 2010-01-13 16:11:43 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.chart;


import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.PageSize;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.DefaultFontMapper;
import com.lowagie.text.pdf.FontMapper;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfTemplate;
import com.lowagie.text.pdf.PdfWriter;

import fr.jmmc.mcs.gui.App;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.logging.Level;
import org.jfree.chart.JFreeChart;

/**
 * This class is dedicated to export charts as PDF documents
 * @author bourgesl
 */
public class PDFUtils {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.chart.PDFUtils";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /**
   * Save the given chart as a PDF document in the given file
   * @param file PDF file to create
   * @param chart chart to export
   */
  public static void saveChartAsPDF(final File file, final JFreeChart chart) {

    BufferedOutputStream localBufferedOutputStream = null;
    try {
      localBufferedOutputStream = new BufferedOutputStream(new FileOutputStream(file));

      writeChartAsPDF(localBufferedOutputStream, chart, new DefaultFontMapper());

    } catch (IOException ioe) {
      logger.log(Level.SEVERE, "IO exception : ", ioe);
    } finally {
      if (localBufferedOutputStream != null) {
        try {
          localBufferedOutputStream.close();
        } catch (IOException ioe) {
          logger.log(Level.SEVERE, "IO exception : ", ioe);
        }
      }
    }
  }

  public static void writeChartAsPDF(final OutputStream outputStream, final JFreeChart chart, final FontMapper fontMapper)
          throws IOException {

    Graphics2D g2 = null;

    final Document document = new Document(PageSize.A4.rotate());

    final Rectangle pdfRectangle = document.getPageSize();

    final int width = (int) pdfRectangle.getWidth();
    final int height = (int) pdfRectangle.getHeight();

    try {
      final PdfWriter localPdfWriter = PdfWriter.getInstance(document, outputStream);

      document.open();

      definePDFProperties(document);

      final PdfContentByte pdfContentByte = localPdfWriter.getDirectContent();
      final PdfTemplate pdfTemplate = pdfContentByte.createTemplate(width, height);

      g2 = pdfTemplate.createGraphics(width, height, fontMapper);
      final Rectangle2D.Double localDouble = new Rectangle2D.Double(0.0D, 0.0D, width, height);

      chart.draw(g2, localDouble);

      pdfContentByte.addTemplate(pdfTemplate, 0.0F, 0.0F);
    } catch (DocumentException de) {
      logger.log(Level.SEVERE, "document exception : ", de);
    } finally {
      if (g2 != null) {
        g2.dispose();
      }
      document.close();
    }
  }

  /**
   * Define PDF properties (margins, author, creator ...)
   * @param document pdf document
   */
  private static void definePDFProperties(final Document document) {
    document.setMargins(50.0F, 50.0F, 50.0F, 50.0F);

    document.addCreator(App.getSharedApplicationDataModel().getProgramName() + " v" + App.getSharedApplicationDataModel().getProgramVersion());
  }
}
