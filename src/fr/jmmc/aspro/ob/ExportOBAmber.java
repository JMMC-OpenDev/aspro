/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ExportOBAmber.java,v 1.7 2010-04-13 15:55:14 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.util.FileUtils;
import java.io.File;
import java.util.logging.Level;

/**
 * This class generates an observing block for the VLTI AMBER instrument
 * @author bourgesl
 */
public class ExportOBAmber extends ExportOBVLTI {

  /** template name */
  private final static String TEMPLATE_FILE = "fr/jmmc/aspro/ob/AMBER_template.obx";
  /* keywords */
  /** keyword - HMAG */
  public final static String KEY_HMAG = "<HMAG>";
  /** keyword - HVIS */
  public final static String KEY_HVIS = "<HVIS>";
  /** keyword - KMAG */
  public final static String KEY_KMAG = "<KMAG>";
  /** keyword - KVIS */
  public final static String KEY_KVIS = "<KVIS>";
  /** keyword - COUDE_GS_MAG */
  public final static String KEY_COUDE_GS_MAG = "<COUDE-GS-MAG>";
  /** keyword - FT_SENSOR */
  public final static String KEY_FT_SENSOR = "<FT-SENSOR>";
  /** keyword - SPEC_CONF */
  public final static String KEY_SPEC_CONF = "<SPEC-CONF>";

  /**
   * Forbidden constructor
   */
  private ExportOBAmber() {
    super();
  }

  /**
   * Generate the OB file for the given target
   * @param targetName target to process
   * @param file file to save
   */
  public static void process(final String targetName, final File file) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("process " + targetName + " to " + file);
    }

    // get OB template :
    String template = FileUtils.readFile(TEMPLATE_FILE);

    // get observation and target :
    final ObservationSetting observation = ObservationManager.getInstance().getObservation();
    final Target target = ObservationManager.getTarget(observation, targetName);

    // process common VLTI part :
    String document = processCommon(template, targetName, observation, target);

    // Magnitudes for H, K :
    document = document.replaceFirst(KEY_HMAG,
            df3.format((target.getFLUXH() != null) ? target.getFLUXH().doubleValue() : -99d));
    document = document.replaceFirst(KEY_KMAG,
            df3.format((target.getFLUXK() != null) ? target.getFLUXK().doubleValue() : -99d));

    // Coude Guided Star = Science (= mag V) :
    document = document.replaceFirst(KEY_COUDE_GS_MAG,
            df3.format((target.getFLUXV() != null) ? target.getFLUXV().doubleValue() : -99d));

    // Later : ft sensor
    document = document.replaceFirst(KEY_FT_SENSOR, "NONE");

    // spectral configuration = instrument mode :
    document = document.replaceFirst(KEY_SPEC_CONF, observation.getInstrumentConfiguration().getInstrumentMode());

    // Finally, write the file :
    FileUtils.writeFile(file, document);
  }
}
