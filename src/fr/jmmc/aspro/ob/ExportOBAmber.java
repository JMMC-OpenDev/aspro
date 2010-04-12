/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ExportOBAmber.java,v 1.5 2010-04-12 14:33:18 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.util.FileUtils;
import java.io.File;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.logging.Level;

/**
 * This class generates an observing block for the VLTI AMBER instrument
 * @author bourgesl
 */
public class ExportOBAmber {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.ob.ExportOBAmber";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** template name */
  private final static String TEMPLATE_FILE = "fr/jmmc/aspro/ob/AMBER_template.obx";
  /** double formatter for magnitudes */
  private final static NumberFormat df3 = new DecimalFormat("0.000");
  /** double formatter for PM */
  private final static NumberFormat df6 = new DecimalFormat("0.000000");

  /* keywords */
  /** keyword - name */
  public final static String KEY_FILE_NAME = "<FILE-NAME>";
  /** keyword - comments */
  public final static String KEY_COMMENTS = "<COMMENTS>";
  /** keyword - ra */
  public final static String KEY_RA = "<RA>";
  /** keyword - dec */
  public final static String KEY_DEC = "<DEC>";
  /** keyword - propRA */
  public final static String KEY_PM_RA = "<PMRA>";
  /** keyword - propDEC */
  public final static String KEY_PM_DEC = "<PMDEC>";
  /** keyword - TARGET.NAME */
  public final static String KEY_TARGET_NAME = "<TARGET-NAME>";
  /** keyword - Baseline */
  public final static String KEY_BASE_LINE = "<BASE-LINE>";
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
    // no-op
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
    String document = readTemplate();

    // Set file name :
    document = document.replaceFirst(KEY_FILE_NAME, file.getName());

    // get observation and target :
    final ObservationSetting observation = ObservationManager.getInstance().getObservation();
    final Target target = ObservationManager.getTarget(observation, targetName);

    // --- Target information ---

    // comments = spectral type :
    document = document.replaceFirst(KEY_COMMENTS, target.getSPECTYP());

    // convert RA/DEC with mas up to 3 digits :
    final String[] raDec = AstroSkyCalc.toString(target.getRADeg(), target.getDECDeg());

    document = document.replaceFirst(KEY_RA, raDec[0]);
    document = document.replaceFirst(KEY_DEC, raDec[1]);

    // PMRA / PMDEC (optional) converted to arcsec/year :
    document = document.replaceFirst(KEY_PM_RA,
            df6.format((target.getPMRA() != null) ? target.getPMRA().doubleValue() / 1000d : 0d));
    document = document.replaceFirst(KEY_PM_DEC,
            df6.format((target.getPMDEC() != null) ? target.getPMDEC().doubleValue() / 1000d : 0d));

    // replace invalid characters (i.e. not alpha numeric) :
    final String altName = targetName.replaceAll("[^a-zA-Z_0-9]", "_");
    document = document.replaceFirst(KEY_TARGET_NAME, altName);

    // Later : atmosphere / seeing

    // Base line :
    document = document.replaceFirst(KEY_BASE_LINE, getBaseLine(observation));

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

  public static String readTemplate() {
    return FileUtils.readFile(TEMPLATE_FILE);
  }

  private static String getBaseLine(final ObservationSetting observation) {

    final StringBuilder sb = new StringBuilder();

    for (Station s : observation.getInstrumentConfiguration().getStationList()) {
      sb.append(s.getName()).append("-");
    }
    sb.deleteCharAt(sb.length() - 1);

    return sb.toString();
  }
}
