/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ExportOBAmber.java,v 1.13 2010-09-24 15:52:03 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.12  2010/09/15 14:09:33  bourgesl
 * set FT_SENSOR according to user choice (Finito)
 *
 * Revision 1.11  2010/05/26 15:29:13  bourgesl
 * light refactoring and javadoc
 *
 * Revision 1.10  2010/05/06 15:42:18  bourgesl
 * use HA Min/Max + FT Mode for the target in the observation settings
 *
 * Revision 1.9  2010/05/05 14:29:51  bourgesl
 * added ha Min / Max to generate OB with correct LST intervals
 *
 * Revision 1.8  2010/04/14 13:09:59  bourgesl
 * first minimal OB for MIDI
 *
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
import fr.jmmc.aspro.model.oi.TargetConfiguration;
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
   * @throws IllegalStateException if the template file is not found
   */
  public static void generate(final File file, final ObservationSetting observation, final Target target)
                        throws IllegalStateException {

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
