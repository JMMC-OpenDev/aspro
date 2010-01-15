/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: PDFUtils.java,v 1.2 2010-01-15 16:13:27 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/01/13 16:11:43  bourgesl
 * pdf related classes
 *
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

    final float width = (int) pdfRectangle.getWidth();
    final float height = (int) pdfRectangle.getHeight();

    /*
    Measurements
    When creating a rectangle or choosing a margin, you might wonder what measurement unit is used:
    centimeters, inches or pixels.
    In fact, the default measurement system roughly corresponds to the various definitions of the typographic
    unit of measurement known as the point. There are 72 points in 1 inch (2.54 cm).
     */

    final float margin = 72f / 2.54f;

    final float innerWidth = width - 2 * margin;
    final float innerHeight = height - 2 * margin;

    try {
      final PdfWriter localPdfWriter = PdfWriter.getInstance(document, outputStream);

      document.open();

      definePDFProperties(document);

      final PdfContentByte pdfContentByte = localPdfWriter.getDirectContent();

      final PdfTemplate pdfTemplate = pdfContentByte.createTemplate(width, height);

      g2 = pdfTemplate.createGraphics(innerWidth, innerHeight, fontMapper);
      final Rectangle2D.Float drawArea = new Rectangle2D.Float(0F, 0F, innerWidth, innerHeight);

      chart.draw(g2, drawArea);

      pdfContentByte.addTemplate(pdfTemplate, margin, margin);
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
    document.addCreator(App.getSharedApplicationDataModel().getProgramName() + " v" + App.getSharedApplicationDataModel().getProgramVersion());
  }
}
