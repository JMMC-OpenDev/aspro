/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.ob;

import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.mcs.util.FileUtils;

import java.io.File;
import java.io.IOException;
import java.util.logging.Level;

/**
 * This class generates an observing block for the VLTI AMBER instrument
 * @author bourgesl
 */
public final class ExportOBAmber extends ExportOBVLTI {

  /** template name */
  private final static String TEMPLATE_FILE = "fr/jmmc/aspro/ob/AMBER_template.obx";
  /* keywords */
  /** keyword - SEQ.SOURCE.KMAG */
  public final static String KEY_KMAG = "<KMAG>";
  /** keyword - TEL.DEL.FTSENSOR */
  public final static String KEY_FT_SENSOR = "<FT-SENSOR>";
  /** keyword - OCS.OBS.SPECCONF */
  public final static String KEY_SPEC_CONF = "<SPEC-CONF>";

  /**
   * Forbidden constructor
   */
  private ExportOBAmber() {
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

    // Magnitudes for H, K :
    document = document.replaceFirst(KEY_HMAG, df3.format(getMagnitude(target.getFLUXH())));
    document = document.replaceFirst(KEY_KMAG, df3.format(getMagnitude(target.getFLUXK())));

    // Coude Guided Star = Science (= mag V) :
    document = document.replaceFirst(KEY_COUDE_GS_MAG, df3.format(getMagnitude(target.getFLUXV())));

    // Check if a fringe tracker is used :
    final TargetConfiguration targetConf = target.getConfiguration();

    document = document.replaceFirst(KEY_FT_SENSOR,
            (targetConf != null && targetConf.getFringeTrackerMode() != null) ? "FINITO" : "NONE");

    // spectral configuration = instrument mode :
    document = document.replaceFirst(KEY_SPEC_CONF, observation.getInstrumentConfiguration().getInstrumentMode());

    // Finally, write the file :
    FileUtils.writeFile(file, document);
  }
}
