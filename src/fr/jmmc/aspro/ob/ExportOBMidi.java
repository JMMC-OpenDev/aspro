/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ExportOBMidi.java,v 1.1 2010-04-14 13:09:59 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.7  2010/04/13 15:55:14  bourgesl
 * extracted common part for VLTI
 *
 * Revision 1.6  2010/04/13 14:06:04  bourgesl
 * fixed Name keyword without file name extension and 32 character maximum length
 *
 * Revision 1.5  2010/04/12 14:33:18  bourgesl
 * restored RA/DEC conversion to have proper P2PP format
 * Proper motion converted to arcsec/year
 *
 * Revision 1.4  2010/04/09 10:22:26  bourgesl
 * use RA/DEC in HMS/DMS instead of angle conversion (deg)
 *
 * Revision 1.3  2010/04/08 14:08:23  bourgesl
 * changed missing value for fluxes to -99
 *
 * Revision 1.2  2010/04/06 08:31:44  bourgesl
 * fixed classloader issue with JNLP
 *
 * Revision 1.1  2010/04/02 10:07:35  bourgesl
 * simple OB generation for AMBER
 *
 */
package fr.jmmc.aspro.ob;

import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.util.FileUtils;
import java.io.File;
import java.util.logging.Level;

/**
 * This class generates an observing block for the VLTI MIDI instrument
 * @author bourgesl
 */
public class ExportOBMidi extends ExportOBVLTI {

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
   * @param target target to process
   */
  public static void generate(final File file, final ObservationSetting observation, final Target target) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("generate file : " + file);
    }

    // get OB template :
    final String template = FileUtils.readFile(TEMPLATE_FILE);

    // process common VLTI part :
    String document = processCommon(template, file.getName(), observation, target);

    /*
     * TODO : compute fluxes from mag N in Jansky Jy unit :
     *  
     * SEQ.CORR.IRFLUX   "15"
     * SEQ.UNCORR.IRFLUX "20"
     */

    // Magnitudes for H :
    document = document.replaceFirst(KEY_HMAG,
            df3.format((target.getFLUXH() != null) ? target.getFLUXH().doubleValue() : -99d));

    // Coude Guided Star = Science (= mag V) :
    document = document.replaceFirst(KEY_COUDE_GS_MAG,
            df3.format((target.getFLUXV() != null) ? target.getFLUXV().doubleValue() : -99d));

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
