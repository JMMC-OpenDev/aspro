/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ExportOBVLTI.java,v 1.2 2010-04-14 13:09:59 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/04/13 15:55:14  bourgesl
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

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import java.io.File;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.logging.Level;

/**
 * This class generates the common part of observing blocks for the VLTI
 * @author bourgesl
 */
public class ExportOBVLTI {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.ob.ExportOBVLTI";
  /** Class logger */
  protected static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** double formatter for magnitudes */
  protected final static NumberFormat df3 = new DecimalFormat("0.000");
  /** double formatter for PM */
  protected final static NumberFormat df6 = new DecimalFormat("0.000000");

  /* keywords */
  /** keyword - name */
  public final static String KEY_NAME = "<NAME>";
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

  /* keywords common to AMBER and MIDI */
  /** keyword - xxx.HMAG */
  public final static String KEY_HMAG = "<HMAG>";
  /** keyword - TEL.COU.MAG or COU.GS.MAG */
  public final static String KEY_COUDE_GS_MAG = "<COUDE-GS-MAG>";

  /**
   * Forbidden constructor
   */
  protected ExportOBVLTI() {
    // no-op
  }

  /**
   * Generate the OB file for the given target.
   * According to the instrument defined in the observation, it uses ExportOBAmber or ExportOBMidi.
   * @param targetName target to process
   * @param file file to save
   */
  public static void process(final String targetName, final File file) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("process " + targetName + " to " + file);
    }

    // get observation and target :
    final ObservationSetting observation = ObservationManager.getInstance().getObservation();
    final Target target = ObservationManager.getTarget(observation, targetName);

    // Dispatch to AMBER or MIDI classes :
    final String instrumentName = observation.getInstrumentConfiguration().getName();

    if (AsproConstants.INS_AMBER.equals(instrumentName)) {
      ExportOBAmber.generate(file, observation, target);
    } else if (AsproConstants.INS_MIDI.equals(instrumentName)) {
      ExportOBMidi.generate(file, observation, target);
    } else {
      throw new IllegalArgumentException("The application can not generate an Observing Block for this instrument [" + instrumentName + "] !");
    }
  }

  /**
   * Process the common part of the given template for the given target
   * @param template OB template
   * @param fileName OB file name
   * @param observation observation settings
   * @param target target to process
   * @return processed template
   */
  protected static String processCommon(final String template, final String fileName, final ObservationSetting observation, final Target target) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("processCommon " + target.getName());
    }

    // get OB template :
    String document = template;

    // Set name :
    String name = fileName;
    // remove obx extension :
    int pos = name.lastIndexOf('.');
    if (pos != -1) {
      name = name.substring(0, pos);
    }
    // maximum length :
    if (name.length() > 32) {
      name = name.substring(0, 31);
    }

    document = document.replaceFirst(KEY_NAME, name);

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
    final String altName = target.getName().replaceAll("[^a-zA-Z_0-9]", "_");
    document = document.replaceFirst(KEY_TARGET_NAME, altName);

    // Later : atmosphere / seeing

    // Base line :
    document = document.replaceFirst(KEY_BASE_LINE, getBaseLine(observation));

    return document;
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
