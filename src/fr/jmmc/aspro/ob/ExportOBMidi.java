/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.ob;

import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.service.ObservabilityService;
import fr.jmmc.mcs.util.FileUtils;
import java.io.File;
import java.io.IOException;
import java.util.logging.Level;

/**
 * This class generates an observing block for the VLTI MIDI instrument
 * @author bourgesl
 */
public final class ExportOBMidi extends ExportOBVLTI {

  /** template name */
  private final static String TEMPLATE_FILE = "fr/jmmc/aspro/ob/MIDI_template.obx";
  /* keywords */
  /** keyword - GRIS-NAME */
  public final static String KEY_GRIS_NAME = "<GRIS-NAME>";
  /** keyword - OPT-NAME */
  public final static String KEY_OPT_NAME = "<OPT-NAME>";

  /**
   * Forbidden constructor
   */
  private ExportOBMidi() {
    super();
  }

  /**
   * Generate the OB file for the given target
   * @param file file to save
   * @param observation observation settings
   * @param os observability service with computed data
   * @param target target to process
   *
   * @throws IllegalStateException if the template file is not found or can not be read
   * @throws IOException if an I/O exception occured while writing the observing block
   */
  public static void generate(final File file, final ObservationSetting observation, 
                                               final ObservabilityService os, final Target target)
                        throws IllegalStateException, IOException {
    
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("generate file : " + file);
    }

    // get OB template :
    final String template = FileUtils.readFile(TEMPLATE_FILE);

    // process common VLTI part :
    String document = processCommon(template, file.getName(), observation, os, target);

    /*
     * TODO : compute fluxes from mag N in Jansky Jy unit :
     *  
     * SEQ.CORR.IRFLUX   "15"
     * SEQ.UNCORR.IRFLUX "20"
     */

    // Magnitudes for H :
    document = document.replaceFirst(KEY_HMAG, df3.format(getMagnitude(target.getFLUXH())));

    // Coude Guided Star = Science (= mag V) :
    document = document.replaceFirst(KEY_COUDE_GS_MAG, df3.format(getMagnitude(target.getFLUXV())));

    final String instrumentMode = observation.getInstrumentConfiguration().getInstrumentMode();

    /*
     * Instrument modes are defined with the format : 'HIGH_SENS+PRISM' i.e. 'SENS'+'GRIS'
     */
    final String[] conf = instrumentMode.split("\\+");

    // Beam combiner (HIGH_SENS or SCI_PHOT) :
    document = document.replaceFirst(KEY_OPT_NAME, conf[0]);

    // Dispersive element (GRIS or PRISM) :
    document = document.replaceFirst(KEY_GRIS_NAME, conf[1]);

    // Finally, write the file :
    FileUtils.writeFile(file, document);
  }
}
