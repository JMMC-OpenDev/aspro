/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ExportOBPionier.java,v 1.1 2010-12-15 13:32:00 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 *
 */
package fr.jmmc.aspro.ob;

import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.util.FileUtils;
import java.io.File;
import java.io.IOException;
import java.util.logging.Level;

/**
 * This class generates an observing block for the VLTI PIONIER instrument
 * @author bourgesl
 */
public final class ExportOBPionier extends ExportOBVLTI {

  /** template name */
  private final static String TEMPLATE_FILE = "fr/jmmc/aspro/ob/PIONIER_template.obx";

  /**
   * Forbidden constructor
   */
  private ExportOBPionier() {
    super();
  }

  /**
   * Generate the OB file for the given target
   * @param file file to save
   * @param observation observation settings
   * @param target target to process
   *
   * @throws IllegalStateException if the template file is not found or can not be read
   * @throws IOException if an I/O exception occured while writing the observing block
   */
  public static void generate(final File file, final ObservationSetting observation, final Target target)
                        throws IllegalStateException, IOException {

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("generate file : " + file);
    }

    // get OB template :
    final String template = FileUtils.readFile(TEMPLATE_FILE);

    // process common VLTI part :
    String document = processCommon(template, file.getName(), observation, target);

    // Magnitudes for H :
    document = document.replaceFirst(KEY_HMAG, df3.format(getMagnitude(target.getFLUXH())));

    // Coude Guided Star = Science (= mag V) :
    document = document.replaceFirst(KEY_COUDE_GS_MAG, df3.format(getMagnitude(target.getFLUXV())));

    // what else ?

    // Finally, write the file :
    FileUtils.writeFile(file, document);
  }
}
