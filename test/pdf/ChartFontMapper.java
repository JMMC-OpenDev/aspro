/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package pdf;

import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.BaseFont;
import com.lowagie.text.pdf.DefaultFontMapper;
import java.awt.Font;
import java.io.IOException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This font mapper replaces SansSerif virtual fonts by DejaVu fonts (GPL) to support unicode characters and
 * embed it in generated PDF files
 * 
 * Useful debugging Java2D fonts: System.setProperty("sun.java2d.debugfonts", "true");
 * 
 * @author bourgesl
 */
public final class ChartFontMapper extends DefaultFontMapper {

  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(ChartFontMapper.class.getName());

  @Override
  public BaseFont awtToPdf(final Font font) {
    try {
      BaseFont bf = super.awtToPdf(font);

      if ("SansSerif".equals(font.getName())) {
        // customize mapping to embed custom font instead of Helvetica standard font (not supporting unicode characters)

        try {
          logger.warn("awtToPdf: substitute SansSerif font= '" + font + "'");

          final String fontFile;
          switch (font.getStyle()) {
            case Font.BOLD:
              fontFile = "/usr/share/fonts/dejavu/DejaVuSans-Bold.ttf";
              break;
            case Font.PLAIN:
            default:
              fontFile = "/usr/share/fonts/dejavu/DejaVuSans.ttf";
          }

          bf = BaseFont.createFont(fontFile, BaseFont.IDENTITY_H, BaseFont.EMBEDDED);
          bf.setSubset(true);

        } catch (DocumentException de) {
          logger.error("awtToPdf failed:", de);
        } catch (IOException ioe) {
          logger.error("awtToPdf failed:", ioe);
        }
      }

      logger.warn("awtToPdf: font= '" + font + "' returns " + bf + "{encoding= '" + bf.getEncoding() + "'}");

      return bf;

    } catch (RuntimeException re) {
      logger.error("awtToPdf failed:", re);
      throw re;
    }
  }

  @Override
  public Font pdfToAwt(final BaseFont font, final int size) {
    final Font f = super.pdfToAwt(font, size);

    logger.warn("pdfToAwt: font= '" + font + "' size= " + size + " returns " + f);

    return f;
  }
}